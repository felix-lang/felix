open Flx_types
open Flx_mtypes2
open List
open Flx_util

let rec eassoc x l = match l with
  | [] -> raise Not_found
  | (a,b) ::t ->
    if Flx_typing.cmp_tbexpr x a then b else eassoc x t

(* Unravel an expression into 'three address code',
  at the same time eliminating common sub-expressions.
  Note primitive applications are regarded as unary operators.
*)
let unravel syms bbdfns e =
  let urn = 4 in
  let sube = ref [] in
  let get e =
    try eassoc e !sube
    with Not_found ->
      let n = !(syms.counter) in incr (syms.counter);
      let name = "_tmp" ^ si n in
      sube := (e,name) :: !sube;
      name

  in
  let refer ((_,t) as e) =
    BEXPR_expr (get e,t),t
  in
  let idt t = t in
  let e' =
    let rec aux n e =
      let n = n - 1 in
      match e with
      | BEXPR_apply ((BEXPR_name _,_) as f, b),t ->
        refer (BEXPR_apply (f, aux urn b),t)

      (*
      (* no unravelling of primitives *)
      | BEXPR_apply_prim (i,ts,b),t  when n > 0 ->
        BEXPR_apply_prim (i, ts, aux n b),t
      *)

      | BEXPR_apply_direct (i,ts,b),t
      | BEXPR_apply ((BEXPR_closure (i,ts),_), b),t ->

        let id,parent,sr,entry = Hashtbl.find bbdfns i in
        begin match entry with
        | BBDCL_struct _
        | BBDCL_fun _ -> BEXPR_apply_direct (i, ts, aux n b),t
        | BBDCL_function _ -> refer (BEXPR_apply_direct (i,ts, aux urn b),t)

        | _ -> assert false
        end

      | BEXPR_apply (f,b),t -> refer (BEXPR_apply(aux urn f, aux urn b),t)
      | BEXPR_tuple ls,t -> (BEXPR_tuple (map (aux n) ls),t)
      | (BEXPR_name _,t) as x -> x
      | (BEXPR_literal (Flx_ast.AST_int _ )),t as x -> x
      | (BEXPR_literal (Flx_ast.AST_float _ )),t as x -> x
      | x -> refer x
    in
      aux urn e
  in
  let sube = rev !sube in
  (*
  print_endline
  (
    "Unravelled " ^ sbe syms.dfns e ^ "-->" ^ sbe syms.dfns e' ^
    " where:\n" ^
    catmap ""
    (fun (x,s) ->
      s ^ " = "^sbe syms.dfns x ^";\n"
    )
    sube
  );
  *)
  sube,e'
