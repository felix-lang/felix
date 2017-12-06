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

let rec guess_metatype sr t : kind =
  match t with
  | TYP_defer _ -> print_endline "Guess metatype: defered type found"; assert false
  | TYP_tuple_cons (sr,t1,t2) -> assert false
  | TYP_tuple_snoc (sr,t1,t2) -> assert false
  | TYP_type_tuple _ -> print_endline "A type tuple"; assert false
  | TYP_typefun (d,c,body) -> 
(*
    print_endline ("A type fun: " ^ 
    catmap "," (fun (n,t) -> string_of_typecode t) d ^ " -> " ^ string_of_typecode c);
*)
    let atyps = List.map (fun (_,t) -> t) d in
    let atyp = match atyps with
    | [x]->x
    | _ -> KND_tuple atyps
    in
    let t = KND_function (atyp, c) in
    Flx_btype.bmt "Flx_guess_meta_type" t

  (* name like, its a big guess! *)
  | TYP_label
  | TYP_suffix _
  | TYP_index _
  | TYP_lookup _ 
  | TYP_name _ -> (* print_endline "A type name?"; *) kind_type
  | TYP_as _ -> print_endline "A type as (recursion)?"; assert false

  (* usually actual types! *)
  | TYP_rptsum _
  | TYP_pclt _
  | TYP_uniq _
  | TYP_void _ 
  | TYP_case_tag _ 
  | TYP_typed_case _
  | TYP_callback _
  | TYP_patvar _ 
  | TYP_tuple _
  | TYP_unitsum _
  | TYP_sum _
  | TYP_intersect _
  | TYP_union _
  | TYP_record _
  | TYP_polyrecord _
  | TYP_variant _
  | TYP_cfunction _
  | TYP_pointer _
  | TYP_rref _
  | TYP_wref _
  | TYP_type_extension _
  | TYP_array _ -> kind_type

  (* note this one COULD be a type function type *)
  | TYP_function _ -> kind_type
  | TYP_effector _ -> kind_type

  | TYP_dual t -> guess_metatype sr t

  | TYP_typeof _
  | TYP_var _
  | TYP_none 
  | TYP_ellipsis   
  | TYP_isin _ 

  | TYP_typeset _
  | TYP_setunion _
  | TYP_setintersection _


  | TYP_apply _

  | TYP_subtype_match _
  | TYP_type_match _ (* FIXME: calculate by examining branches ! *)
  | TYP_patany _
    -> print_endline ("Woops, dunno meta type of " ^ string_of_typecode t); kind_type

let guess_meta_type state bsym_table bt index : kind = 
  let data = get_data state.sym_table index in
  match data with { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
    match entry with
    | SYMDEF_instance_type t
    | SYMDEF_type_alias t  -> 
      guess_metatype sr t
    | _ -> print_endline ("Dunno, assume a type " ^ string_of_symdef entry id vs); assert false

