open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid


(* This module produces a reference to either a nominal type, or a type alias,
of the form 

  BTYP_inst or 
  BTYP_vinst or 
  BTYP_typevar or 
  BTYP_finst


The referenced type need not yet be in the bound symbol table.

The generated term BTYP_inst(`Nominal, index, ts, kind) is not replaceable.
The ts and kind expressions might be reducible, but the term itself cannot
be replaced. It might be elided by an optimisation. In the back end,
it generates a reference to a C++ type, either a primitive like int,
a generated C++ class, or is a binding to an extant C++ class, possibly
a template.

On the other hand `Alias references are transient and must be removed by
expansion of typedefs and replaced by their bound representation. However
this may not be available at the time this routine runs.

Removal of aliases is basically substitution with one special case,
recursion. A recursive expansion must be replaced by a fixpoint term.

There are THREE cases for recursive expansion:

1. A simple BTYP_inst (`Alias, ..) term is replaced by the bound
definition it refers to unless the term is in the trail, in which
case a fixpoint term is used instead.

2. A recursive function call on the function's parameter is replaced
by a fixpoint term.

3. Polymorphic recursion is expanded up to a hard limit. This is not idea
and should be fixed. Recursions can converge to a fixpoint, converge
to a cycle, or diverge. A cyclic convergence should expand to a fixpoint
so there is only divergence left to handle. 

IN THEORY I believe any bounded recursions must terminate in a single
fixpoint, that is, because of the bound, a limited number of expansions
can take place until some term is repeated, generating a fixpoint.
Therefore a rigorous proof of divergencee should lead to an error,
otherwise expand up to the limit.

NOTE: these notes partly belong in another module.

*)

let debug = false 

let rec bind_type_index 
  bind_type'
  inner_bind_type
  state (bsym_table:Flx_bsym_table.t) (rs:recstop) sr index ts mkenv
=
  (* fixup the fixpoints *)
  let ts = adjust_ts state.sym_table bsym_table sr index ts in
  let bt t =
    (* get hold of the type variable parameters *)
    let vs,_ = find_vs state.sym_table bsym_table index in
    (* check we have the right number of input arguments *)
    if List.length vs <> List.length ts then 
      begin
        print_endline ("vs=" ^
          catmap "," (fun (s,i,_)-> s ^ "<" ^ string_of_bid i ^ ">") vs);
        print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
        failwith "len vs != len ts"
      end
    ;
    let env:env_t = mkenv index in
    let params = [] in
    let t =
      bind_type' state bsym_table env
      { rs with type_alias_fixlist = (index,rs.depth):: rs.type_alias_fixlist }
      sr t params mkenv
    in
    (* replace type variable parameters with arguments *)
    let t = tsubst sr (List.map (fun (s,i,mt) -> s,i,Flx_btype.bmt "Flx_bind_type_index" mt) vs) ts t in 
    t
  in

  (* check for recursion *)
  if List.mem_assoc index rs.type_alias_fixlist
  then begin
    let mt = Flx_guess_meta_type.guess_meta_type state bsym_table bt index in
    print_endline ("Flx_bind_type_index: fixpoint, meta type calculated by guess_meta_type!"); 
    let fixated = btyp_fix ((List.assoc index rs.type_alias_fixlist)-rs.depth) mt in
    fixated
  end

  (* no recursion *)
  else begin
  let parent = Flx_sym_table.find_parent state.sym_table index in
  match get_data state.sym_table index with
  | { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
    match entry with
    | SYMDEF_kindvar _ -> assert false
    | SYMDEF_typevar mt ->
      let mt = bmt "bind_type_index" mt in
      btyp_type_var (index,mt)

    (* type alias RECURSE *)
    | SYMDEF_instance_type t ->
      if debug then
      print_endline ("Bind type index: Unravelling virtual type instance " ^ id ^ " index=" ^ si index);
      let t = bt t in
      t


    | SYMDEF_type_function (iks,t) ->
(*
print_endline ("Bind type index, trying to bind type function " ^id ^ "<" ^string_of_int index ^ "> = " ^ string_of_typecode t);
*)
    begin try
      let t = bt t in
      let k = Flx_btype_kind.metatype sr t in
        let dom,cod = match k with 
        | KIND_function (dom,cod) -> dom, cod
        | _ -> assert false
        in
        let t = btyp_finst (index,[],dom,cod) in
        t
    with Not_found -> 
      print_endline ("Failure binding type function body " ^ string_of_typecode t);
      assert false
    end


    | SYMDEF_type_alias t ->
(*
print_endline ("Bind type index, trying to bind " ^id ^ "<" ^string_of_int index ^ "> = " ^ string_of_typecode t);
*)
    begin try
      let bsym = Flx_bsym_table.find bsym_table index in
      let bbdcl = Flx_bsym.bbdcl bsym in
      begin match bbdcl with
      | BBDCL_structural_type_alias (bvs, alias) ->
        let salias = Flx_btype_subst.tsubst sr bvs ts alias in
(*
        print_endline ("Bind type index: STRUCTURAL Unravelling type alias " ^ id ^ " index=" ^ si index ^ " to " ^
          Flx_btype.st salias);
*)
        salias

      (* This case would not normally happen, HOWEVER it MAY be the case a nominal
         type record is generated for a type alias sometimes. We should
         check this CANNOT occur. At the moment it definitely DOES occur and that
         needs to be fixed.
      *)
      | BBDCL_nominal_type_alias (bvs, alias) ->
(*
        print_endline ("ERROR! [Bind type index]: NOMINAL symbol table entry found for type alias " ^ id ^ " index=" ^ si index ^ " to " ^
          Flx_btype.st alias);
*)
        let k = Flx_btype_kind.metatype sr alias in
        let t = btyp_inst (`Alias, index,ts,k) in
        t

      | _ -> failwith ("Flx_bind_type expected type alias in bound symbol table " ^ id);
      end
    with Not_found ->
      let k = Flx_guess_meta_type.guess_metatype sr t in
(*
print_endline ("Flx_bind_type_index: btyp_inst, meta type calculated by guess_metatype!"); 
*)
      let t = btyp_inst (`Alias, index,ts,k) in 
(*
      print_endline ("Bind type index: INITIAL nominalising type alias " ^ id ^ 
       " index=" ^ si index ^ " to " ^ Flx_btype.st t ^ ", kind=" ^ Flx_kind.sk k);
*)
      t
    end

    | SYMDEF_abs _ ->
      btyp_inst (`Nominal, index,ts,Flx_kind.KIND_type)

    | SYMDEF_virtual_type  ->
      btyp_vinst (index,ts,Flx_kind.KIND_type)

    | SYMDEF_newtype _
    | SYMDEF_union _
    | SYMDEF_struct _
    | SYMDEF_cstruct _
      ->
(*
print_endline ("bind type index, struct thing " ^ si index ^ " ts=" ^ catmap "," (sbt bsym_table) ts);
*)
      btyp_inst (`Nominal,index,ts,Flx_kind.KIND_type)


    (* allow binding to type constructors now too .. *)
    | SYMDEF_const_ctor (uidx,ut,idx,vs') ->
      btyp_inst (`Nominal, index,ts,Flx_kind.KIND_type)

    | SYMDEF_nonconst_ctor (uidx,ut,idx,vs',argt) ->
      btyp_inst (`Nominal, index,ts,Flx_kind.KIND_type)

    | SYMDEF_typeclass 
    | SYMDEF_module 
    | SYMDEF_library
    | SYMDEF_label _
    | SYMDEF_parameter _
    | SYMDEF_axiom _
    | SYMDEF_lemma _
    | SYMDEF_reduce _
    | SYMDEF_function _
    | SYMDEF_root _
    | SYMDEF_const _
    | SYMDEF_var _
    | SYMDEF_once _
    | SYMDEF_val _
    | SYMDEF_ref _
    | SYMDEF_lazy _
    | SYMDEF_fun _
    | SYMDEF_callback _
    | SYMDEF_insert _
    | SYMDEF_inherit _
    | SYMDEF_inherit_fun _
    | SYMDEF_instance _
      ->
      clierrx "[flx_bind/flx_bind_type_index.ml:316: E103] " sr
      (
        "[bind_type_index] Type " ^ id ^ "<" ^ string_of_bid index ^ ">" ^
        " must be a type [alias, abstract, union, struct, virtual type], got:\n" ^
        string_of_symdef entry id vs
      )
  end


