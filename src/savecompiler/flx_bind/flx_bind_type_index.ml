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

let debug = false 

let rec bind_type_index 
  bind_type'
  inner_bind_type
  state (bsym_table:Flx_bsym_table.t) (rs:recstop) sr index ts mkenv
=
(*
if index = 37335 then 
  print_endline "**** Bind_type_index, special 37335";
if index = 37335 then 
  print_endline (" **** RAW ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
*)
  let ts = adjust_ts state.sym_table bsym_table sr index ts in
(*
if index = 37335 then 
  print_endline (" **** Adjusted ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
*)
  (*
  print_endline ("Adjusted ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
  *)
  let bt t =
      (*
      print_endline "Making params .. ";
      *)
      let vs,_ = find_vs state.sym_table bsym_table index in
(*
if index = 37335 then begin
        print_endline (" **** preparing to bind type " ^ string_of_typecode t);
        print_endline (" **** making params for call to bind type");
        print_endline (" **** vs=" ^
          catmap "," (fun (s,i,_)-> s ^ "<" ^ string_of_bid i ^ ">") vs);
        print_endline (" **** ts=" ^ catmap "," (sbt bsym_table) ts);
end;
*)
      if List.length vs <> List.length ts then 
      begin
        print_endline ("vs=" ^
          catmap "," (fun (s,i,_)-> s ^ "<" ^ string_of_bid i ^ ">") vs);
        print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
        failwith "len vs != len ts"
      end
      else
      (* I think this is the wrong idea: params is for type function parameters! *)
      (* let params = List.map2 (fun (s,i,_) t -> s,t) vs ts in *)
      (* so lets try with out them *)
      let params = [] in
(*
if index = 37335 then begin
  print_endline (" **** params = " ^ catmap "," (fun (s,t) -> s ^ " --> " ^ sbt bsym_table t) params);
end;
*)
      (*
      let params = make_params state.sym_table sr index ts in
      *)
      (*
      print_endline ("params made");
      *)
      let env:env_t = mkenv index in
      let t =
        bind_type' state bsym_table env
        { rs with type_alias_fixlist = (index,rs.depth):: rs.type_alias_fixlist }
        sr t params mkenv
      in
(*
if index = 37335 then begin
  print_endline (" **** Bound type is " ^ sbt bsym_table t);
  print_endline (" **** SHOULD HAVE VARIABLES REPLACED!");
end;
*)
(* DO A HACK NOW, cause params doesn't propagate *)
     let t = tsubst sr (List.map (fun (s,i,mt) -> s,i,Flx_btype.bmt "Flx_bind_type_index" mt) vs) ts t in 
(*
if index = 37335 then begin
  print_endline (" **** AFTER TSUBST Bound type is " ^ sbt bsym_table t);
  print_endline (" **** SHOULD HAVE VARIABLES REPLACED!");
end;
*)
(*
        print_endline ("Unravelled and bound is " ^ sbt bsym_table t);
        *)
        (*
        let t = beta_reduce state.counter sr t in
        *)
        (*
        print_endline ("Beta reduced: " ^ sbt bsym_table t);
        *)
        t
  in
(*
  print_endline
  (
    "BINDING INDEX " ^ string_of_int index ^
    " with ["^ catmap ", " (sbt bsym_table) ts^ "]"
  );
*)
  (*
  print_endline ("type alias fixlist is " ^ catmap ","
    (fun (i,j) -> si i ^ "(depth "^si j^")") type_alias_fixlist
  );
  *)
  if List.mem_assoc index rs.type_alias_fixlist
  then begin
(*
    print_endline (
      "Making fixpoint for Recursive type alias " ^
      (
        match get_data state.sym_table index with { Flx_sym.id=id;sr=sr}->
          id ^ " defined at " ^
          Flx_srcref.short_string_of_src sr
      )
    );
*)
    let mt = Flx_guess_meta_type.guess_meta_type state bsym_table bt index in
    let fixated = btyp_fix ((List.assoc index rs.type_alias_fixlist)-rs.depth) mt in
(*
print_endline ("flx_lookup: bind-type-index returning fixated " ^ sbt bsym_table fixated);
*)
    fixated
  end
  else begin
  (*
  print_endline "bind_type_index";
  *)
  let parent = Flx_sym_table.find_parent state.sym_table index in
  match get_data state.sym_table index with
  | { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
    (*
    if List.length vs <> List.length ts
    then
      clierrx "[flx_bind/flx_lookup.ml:1496: E102] " sr
      (
        "[bind_type_index] Wrong number of type arguments for " ^ id ^
        ", expected " ^
        si (List.length vs) ^ " got " ^ si (List.length ts)
      );
    *)
    match entry with
    | SYMDEF_typevar mt ->
      (* HACK! We will assume metatype are entirely algebraic,
        that is, they cannot be named and referenced, we also
        assume they cannot be subscripted .. the bt routine
        that works for type aliases doesn't seem to work for
        metatypes .. we get vs != ts .. ts don't make sense
        for type variables, only for named things ..
      *)
      (* WELL the above is PROBABLY because we're calling
      this routine using sye function to strip the view,
      so the supplied ts are wrong ..
      *)
      (*
      print_endline ("CALCULATING TYPE VARIABLE METATYPE " ^ si index ^ " unbound=" ^ string_of_typecode mt);
      *)
      (* weird .. a type variables parent function has an env containing
      the type variable .. so we need ITS parent for resolving the
      meta type ..??

      No? We STILL get an infinite recursion???????
      *)
      (*
      print_endline ("type variable index " ^ si index);
      *)
(*
      let env = match parent with
        | Some parent ->
          (*
          print_endline ("It's parent is " ^ si parent);
          *)
          (*
          let {parent=parent} = hfind "lookup" state.sym_table parent in
          begin match parent with
          | Some parent ->
             print_endline ("and IT's parent is " ^ si parent);
          *)
            let mkenv i = Flx_name_lookup.mk_bare_env state.sym_table i in
            mkenv parent
          (*
          | None -> []
          end
          *)
        | None -> []
      in
*)
      let mt = bmt "bind_type_index" mt in
      (*
      print_endline ("Bound metatype is " ^ sbt bsym_table mt);
      let mt = cal_assoc_type state sr mt in
      print_endline ("Assoc type is " ^ sbt bsym_table mt);
      *)
(*
if index = 7141 then
print_endline ("Flx_bind_type_index.Binding type variable " ^ si index ^ ", kind=" ^ Flx_kind.sk mt);
*)
      btyp_type_var (index,mt)

    (* type alias RECURSE *)
    | SYMDEF_instance_type t ->
      if debug then
      print_endline ("Bind type index: Unravelling virtual type instance " ^ id ^ " index=" ^ si index);
      let t = bt t in
      t


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
      | BBDCL_nominal_type_alias (bvs, alias) ->
(*
        print_endline ("Bind type index: NOMINAL bind type alias " ^ id ^ " index=" ^ si index ^ " to " ^
          Flx_btype.st alias);
*)
        let k = Flx_btype_kind.metatype sr alias in
        let t = btyp_inst (index,ts,k) in
        t

      | _ -> failwith ("Flx_bind_type expected type alias in bound symbol table " ^ id);
      end
    with Not_found ->
      let k = Flx_guess_meta_type.guess_metatype sr t in
      let t = btyp_inst (index,ts,k) in 
(*
      print_endline ("Bind type index: INITIAL nominalising type alias " ^ id ^ 
       " index=" ^ si index ^ " to " ^ Flx_btype.st t ^ ", kind=" ^ Flx_kind.sk k);
*)
      t
    end

    | SYMDEF_abs _ ->
      btyp_inst (index,ts,Flx_kind.KIND_type)

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
      btyp_inst (index,ts,Flx_kind.KIND_type)


    (* allow binding to type constructors now too .. *)
    | SYMDEF_const_ctor (uidx,ut,idx,vs') ->
      btyp_inst (index,ts,Flx_kind.KIND_type)

    | SYMDEF_nonconst_ctor (uidx,ut,idx,vs',argt) ->
      btyp_inst (index,ts,Flx_kind.KIND_type)

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
      clierrx "[flx_bind/flx_lookup.ml:1591: E103] " sr
      (
        "[bind_type_index] Type " ^ id ^ "<" ^ string_of_bid index ^ ">" ^
        " must be a type [alias, abstract, union, struct, virtual type], got:\n" ^
        string_of_symdef entry id vs
      )
  end


