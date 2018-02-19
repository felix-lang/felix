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

let rec inner_type_of_index' 
  build_env
  bind_type'
  btype_of_bsym
  cal_ret_type
  state bsym_table sr rs index =
(*
  if index = 37461 then
*)
if debug then
     print_endline ("Inner type of index " ^ si index ^ "=pve");

  (* Check if we've already cached this index. *)
  try Hashtbl.find state.ticache index with Not_found ->

  (* Check index recursion. If so, return a fix type. *)
(*  HACK: metatype guess *)
  if List.mem index rs.idx_fixlist then begin
if debug then
print_endline "inner_typeof+index returning fixpoint";
    btyp_fix (-rs.depth) (btyp_type 0) 
  end else

  let mkenv i = build_env state bsym_table (Some i) in
  let env = mkenv index in
(*
if index = 37461 then print_env env;
*)
  (* Helper function that binds and beta reduces a type. *)
  let bt sr t =
    let t = bind_type' state bsym_table env rs sr t [] mkenv in
    beta_reduce "flx_lookup: inner_type_of_index" state.counter bsym_table sr t
  in

  (* First check if we've already bound this index. If so, return the type of
   * the symbol. Otherwise, look up the type in the environment. *)
  match
    try Some (Flx_bsym_table.find bsym_table index) with Not_found -> None
  with
  | Some bsym -> btype_of_bsym state bsym_table sr bt index bsym
  | None ->

  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_label s -> btyp_label ()

  | SYMDEF_callback _ ->
      print_endline "Inner type of index finds callback";
      assert false
  | SYMDEF_inherit qn ->
      failwith ("Woops inner_type_of_index found inherit " ^
        string_of_bid index)
  | SYMDEF_inherit_fun qn ->
      failwith ("Woops inner_type_of_index found inherit fun!! " ^
        string_of_bid index)
  | SYMDEF_instance_type t
  | SYMDEF_type_alias t ->
(*
print_endline ("bind_type_index finds: Type alias name " ^ sym.Flx_sym.id);
  NOTE: this routine is finding the type of a type, that is, a metatype,
  its a bit of a hack!
*)
print_endline ("Inner type of index trying to calculate meta type of type alias");
assert false
(*
      let t = bt sym.Flx_sym.sr t in
      Flx_metatype.metatype state.sym_table bsym_table rs sym.Flx_sym.sr t
*)

  | SYMDEF_function ((ps,_),rt,effects,props,_) ->
(*
print_endline ("** BEGIN ** Calculating Function type for function " ^ sym.Flx_sym.id ^ " index "^si index);
*)
      let ptyp = typeof_paramspec_t ps in

      (* Calculate the return type. *)
      let rt =
        (* First, check if we've already calculated it. *)
        try Hashtbl.find state.varmap index with Not_found ->
          (* Nope! So let's and calculate it. Add ourselves to the fix list and
           * recurse. *)
          let rs = { rs with idx_fixlist = index::rs.idx_fixlist } in
          try
            let result = cal_ret_type state bsym_table rs index [] in
(*
print_endline ("** END **** Normal Exit Calculating Function type for function " ^ sym.Flx_sym.id );
*)
            result 
          with exn ->
(*
print_endline ("** END **** Abnormal Exit Function type for function " ^ sym.Flx_sym.id );
*)
          raise exn

      in
      (* this really isn't right .. need a better way to handle indeterminate
       * result .. hmm .. *)
      if var_i_occurs index rt then begin
        raise (Unresolved_return (sym.Flx_sym.sr,
          (
            "[type_of_index'] " ^
            "function " ^ sym.Flx_sym.id ^ "<" ^ string_of_bid index ^
            ">: Can't resolve return type, got : " ^
            sbt bsym_table rt ^
            "\nPossibly each returned expression depends on the return type" ^
            "\nTry adding an explicit return type annotation"
          )))
      end;

      let d = bt sym.Flx_sym.sr ptyp in
      let e = bt sym.Flx_sym.sr effects in
      let ft = 
        if List.mem `Cfun props
        then btyp_cfunction (d, rt)
        else btyp_effector (d, e, rt)
      in
(*
print_endline ("** FINISH **** Calculating Function type for function " ^ sym.Flx_sym.id ^ ":" ^ sbt bsym_table ft);
*)
      ft

  | SYMDEF_const (_,t,_,_)
  | SYMDEF_once t
  | SYMDEF_val t
  | SYMDEF_var t -> 
    let t' = bt sym.Flx_sym.sr t in
    t'

  | SYMDEF_ref t -> btyp_pointer (bt sym.Flx_sym.sr t)

  | SYMDEF_lazy (t,x) -> bt sym.Flx_sym.sr t

  | SYMDEF_parameter (`POnce,t)
  | SYMDEF_parameter (`PVal,t)
  | SYMDEF_parameter (`PVar,t) -> bt sym.Flx_sym.sr t

  | SYMDEF_const_ctor (_,t,_,_) -> bt sym.Flx_sym.sr t
  | SYMDEF_nonconst_ctor (_,ut,_,_,argt) ->
      bt sym.Flx_sym.sr (TYP_function (argt,ut))

  | SYMDEF_fun (_,pts,rt,_,_,_) ->
      bt sym.Flx_sym.sr (TYP_function (type_of_list pts,rt))

  | SYMDEF_union _ ->
      clierrx "[flx_bind/flx_lookup.ml:2048: E108] " sym.Flx_sym.sr ("Union " ^ sym.Flx_sym.id ^ " doesn't have a type")

  (* struct as function *)
  | SYMDEF_cstruct (ls,_)
  | SYMDEF_struct ls ->
      let _,vs,_  = find_split_vs state.sym_table bsym_table index in
      let ts = List.map
        (fun (s,i,_) -> TYP_name (sym.Flx_sym.sr,s,[]))
        vs
        (* (fst sym.Flx_sym.vs) *)
      in
      let ts = List.map (bt sym.Flx_sym.sr) ts in

(* print_endline "inner type of index .. struct .. about to adjust ts"; *)
      let ts = adjust_ts state.sym_table bsym_table sym.Flx_sym.sr index ts in
(* print_endline "inner type of index .. struct .. adjust ts done"; *)
      let t = type_of_list (List.map snd ls) in
      let mt = Flx_kind.KIND_type in
      let result = btyp_function (bt sym.Flx_sym.sr t, btyp_inst (index, ts, mt)) in
(*
print_endline ("struct as function [inner_type_of_index] " ^ sbt bsym_table result);
*)
      result

  | SYMDEF_abs _ ->
      clierrx "[flx_bind/flx_lookup.ml:2068: E109] " sym.Flx_sym.sr
      (
        "[type_of_index] Expected declaration of typed entity for index " ^
        string_of_bid index ^ "\ngot abstract type " ^ sym.Flx_sym.id  ^
        " instead.\n" ^
        "Perhaps a constructor named " ^ "_ctor_" ^ sym.Flx_sym.id ^
        " is missing or out of scope?"
      )

  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2078: E110] " sym.Flx_sym.sr
      (
        "[type_of_index] Expected declaration of typed entity for index "^
        string_of_bid index ^ ", got " ^ sym.Flx_sym.id
      )


