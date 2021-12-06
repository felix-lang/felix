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
open Flx_kind

let debug = false

(* FIXME: one problem of this routine is that the guess can be overly general.
One reason (at least) for that is that the unbound TYP_* terms are not necessarily
fully abstract. The bound terms they generate are abstract. For example
you can have `TYP_sum [`TYP_unitsum 1; `TYP_unitsum 1] or write the same type
as `TYP_unitsum 2, or as `TYP_rptsumn (2,`TYP_unitsum).

When bound, all types have a canonical (unique) representation which ensures
the most specialised kind can be generated.

This has to be fixed. Unfortunately. The way forward is that purely structural
types should be the SAME terms bound and unbound, and in particular the
abstraction invariants applied when they're constructed.

TODO!
*)

(* THIS IS USED in Flx_bind_type_index to bind a btyp_inst during the
replacement of a typedef with a type expression which is NOT mapped to a structural or
nominal type alias BBDCL entry by a common index, in other words, a simple typedef
*) 
let rec guess_metatype sr t : kind =
  match t with
  | `TYP_uniq _ -> kind_linear
  | `TYP_borrowed _ -> kind_borrowed
  | `TYP_bool _ -> kind_bool 
  | `TYP_typeop (sr,op,t,k) -> bmt "Flx_guess_meta_type" k 
  | `TYP_unitsum _ -> kind_unitsum
  | `TYP_compactarray _ 
  | `TYP_compacttuple _ -> kind_compactlinear

  | `TYP_tuple_cons (sr,t1,t2) -> assert false
  | `TYP_tuple_snoc (sr,t1,t2) -> assert false
  | `TYP_type_tuple _ -> print_endline "A type tuple"; assert false
  | `TYP_typefun (d,c,body) -> 
(*
    print_endline ("A type fun: " ^ 
    Flx_util.catmap "," (fun (n,t) -> str_of_kindcode t) d ^ " -> " ^ str_of_kindcode c);
*)
    let atyps = List.map (fun (_,t) -> t) d in
    let atyp = match atyps with
    | [x]->x
    | _ -> KND_tuple atyps
    in
    let k = KND_function (atyp, c) in
(*
print_endline (" ** mata type is " ^ Flx_print.str_of_kindcode k);
*)
    let bmt = Flx_btype.bmt "Flx_guess_meta_type.guess_metatype" k in
(*
print_endline (" ** BOUND mata type is " ^ Flx_kind.sk bmt);
*)
    bmt

  | `TYP_subtype_match (_,bs) 
  | `TYP_type_match (_,bs) ->
    kind_max (List.map (fun (_,t) -> guess_metatype sr t) bs)

  (* name like, its a big guess! *)
  | `TYP_label
  | `TYP_suffix _
  | `TYP_index _
  | `TYP_lookup _ 
  | `TYP_name _ -> (* print_endline "A type name?"; *) kind_type
  | `TYP_as _ -> print_endline "A type as (recursion)?"; assert false

  (* usually actual types! *)
  | `TYP_rptsum _
  | `TYP_compactrptsum _
  | `TYP_pclt _
  | `TYP_rpclt _
  | `TYP_wpclt _
  | `TYP_void _ 
  | `TYP_case_tag _ 
  | `TYP_callback _
  | `TYP_patvar _ 
  | `TYP_intersect _
  | `TYP_tuple _
  | `TYP_sum _
  | `TYP_compactsum _
  | `TYP_record _
  | `TYP_polyrecord _
  | `TYP_variant _
  | `TYP_cfunction _
  | `TYP_pointer _
  | `TYP_rref _
  | `TYP_wref _
  | `TYP_type_extension _
  | `TYP_array _ -> kind_type

  (* note this one COULD be a type function type *)
  | `TYP_function _ -> kind_type
  | `TYP_effector _ -> kind_type
  | `TYP_linearfunction _ -> kind_type
  | `TYP_lineareffector _ -> kind_type

  | `TYP_dual t -> guess_metatype sr t

  | `TYP_typeof _
  | `TYP_var _
  | `TYP_none 
  | `TYP_ellipsis   
  | `TYP_isin _ 

  | `TYP_typeset _
  | `TYP_setunion _
  | `TYP_setintersection _


  | `TYP_apply _ (* FIXME! *)

  | `TYP_patany _
    -> print_endline ("Woops, dunno meta type of " ^ string_of_typecode t); kind_type

let guess_meta_type state bsym_table bt index : kind = 
  let data = get_data state.sym_table index in
  match data with { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
    match entry with
    | SYMDEF_instance_type t
    | SYMDEF_type_alias t  -> 
      guess_metatype sr t
    | _ -> print_endline ("Dunno, assume a type " ^ string_of_symdef entry id vs); assert false

