open Flx_types
open Flx_bexpr
open Flx_bbdcl
open Flx_bid

let rec eassoc x l = match l with
  | [] -> raise Not_found
  | (a,b) ::t ->
    if Flx_bexpr.cmp x a then b else eassoc x t

(* Unravel an expression into 'three address code',
  at the same time eliminating common sub-expressions.
  Note primitive applications are regarded as unary operators.
*)
let unravel syms bsym_table e =
  let sube = ref [] in
  let get e =
    try eassoc e !sube
    with Not_found ->
      let n = fresh_bid syms.Flx_mtypes2.counter in
      let name = "_tmp" ^ Flx_print.string_of_bid n in
      sube := (e, name) :: !sube;
      name
  in
  let refer ((_, t) as e) = bexpr_expr (Flx_code_spec.Str (get e), t, bexpr_tuple (Flx_btype.btyp_unit ()) []) in
  let e' =
    let rec aux e =
      match e with
      | BEXPR_apply ((BEXPR_varname _,_) as f, b), t ->
        refer (bexpr_apply t (f, aux b))

      (*
      (* no unravelling of primitives *)
      | BEXPR_apply_prim (i,ts,b),t  when n > 0 ->
        BEXPR_apply_prim (i, ts, aux n b),t
      *)
(* ????
      | BEXPR_cond (c,tr,fa), t -> bexpr_cond (aux c) tr fa 
*)
      | BEXPR_apply_direct (i, ts, b), t
      | BEXPR_apply ((BEXPR_closure (i, ts), _), b), t ->

        begin match Flx_bsym_table.find_bbdcl bsym_table i with
        | BBDCL_fun _ -> refer (bexpr_apply_direct t (i, ts, aux b))
        | BBDCL_struct _
        | BBDCL_external_fun _ -> bexpr_apply_direct t (i, ts, aux b)
        | _ -> assert false
        end
      | BEXPR_apply ((BEXPR_prj _,_) as f,b),t -> bexpr_apply t (f, aux b)
      | BEXPR_apply ((BEXPR_inj _,_) as f,b),t -> bexpr_apply t (f, aux b)
      | BEXPR_apply ((BEXPR_aprj _,_) as f,b),t -> bexpr_apply t (f, aux b)
      | BEXPR_apply (f, b), t -> refer (bexpr_apply t (aux f, aux b))
      | BEXPR_tuple ls, t -> (bexpr_tuple t (List.map aux ls))
      | (BEXPR_varname _, t) as x -> x
      | (BEXPR_literal  _, t) as x -> x
      | x -> refer x
    in
      aux e
  in
  let sube = List.rev !sube in
(*
  print_endline
  (
    "Unravelled " ^ Flx_print.sbe bsym_table e ^ "\n-->\n" ^
    Flx_print.sbe bsym_table e' ^ "\nwhere:\n" ^
    Flx_util.catmap ""
    (fun (x,s) ->
      "  " ^ s ^ " = " ^ Flx_print.sbe bsym_table x ^ ";\n"
    )
    sube
  );
*)
  sube, e'

