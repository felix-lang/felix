open Flx_types

let rec eassoc x l = match l with
  | [] -> raise Not_found
  | (a,b) ::t ->
    if Flx_typing.cmp_tbexpr x a then b else eassoc x t

(* Unravel an expression into 'three address code',
  at the same time eliminating common sub-expressions.
  Note primitive applications are regarded as unary operators.
*)
let unravel syms bsym_table e =
  let sube = ref [] in
  let get e =
    try eassoc e !sube
    with Not_found ->
      let n = !(syms.Flx_mtypes2.counter) in incr (syms.Flx_mtypes2.counter);
      let name = "_tmp" ^ Flx_print.string_of_bid n in
      sube := (e, name) :: !sube;
      name
  in
  let refer ((_, t) as e) = BEXPR_expr (get e, t), t in
  let e' =
    let rec aux e =
      match e with
      | BEXPR_apply ((BEXPR_name _,_) as f, b), t ->
        refer (BEXPR_apply (f, aux b), t)

      (*
      (* no unravelling of primitives *)
      | BEXPR_apply_prim (i,ts,b),t  when n > 0 ->
        BEXPR_apply_prim (i, ts, aux n b),t
      *)

      | BEXPR_apply_direct (i, ts, b), t
      | BEXPR_apply ((BEXPR_closure (i, ts), _), b), t ->

        let id,parent,sr,entry = Hashtbl.find bsym_table i in
        begin match entry with
        | BBDCL_struct _
        | BBDCL_fun _ -> BEXPR_apply_direct (i, ts, aux b),t
        | BBDCL_function _ -> refer (BEXPR_apply_direct (i, ts, aux b),t)

        | _ -> assert false
        end

      | BEXPR_apply (f, b), t -> refer (BEXPR_apply (aux f, aux b), t)
      | BEXPR_tuple ls, t -> (BEXPR_tuple (List.map aux ls), t)
      | (BEXPR_name _, t) as x -> x
      | (BEXPR_literal (Flx_ast.AST_int _ )), t as x -> x
      | (BEXPR_literal (Flx_ast.AST_float _ )), t as x -> x
      | x -> refer x
    in
      aux e
  in
  let sube = List.rev !sube in
  (*
  print_endline
  (
    "Unravelled " ^ Flx_print.sbe syms.Flx_mtypes2.sym_table bsym_table e ^ "-->" ^
    Flx_print.sbe syms.Flx_mtypes2.sym_table bsym_table e' ^ " where:\n" ^
    Flx_util.catmap ""
    (fun (x,s) ->
      s ^ " = " ^ Flx_print.sbe syms.Flx_mtypes2.sym_table bsym_table x ^ ";\n"
    )
    sube
  );
  *)
  sube, e'
