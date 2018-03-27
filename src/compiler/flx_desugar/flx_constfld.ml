open Flx_ast
open Flx_print
open Flx_exceptions
open List
open Flx_typing
open Flx_set
open Flx_maps

open Flx_literal

let truth sr r =
  let r = if r then 1 else 0 in
  EXPR_typed_case (sr,r,flx_bool)

(*
let sbi x = string_of_big_int x
let bis x = big_int_of_string x

let minus x = sbi (minus_big_int (bis x))
let abs x = sbi (abs_big_int (bis x))
let add x y = sbi (add_big_int (bis x) (bis y))
let sub x y = sbi (sub_big_int (bis x) (bis y))
let mult x y = sbi (mult_big_int (bis x) (bis y))
let div x y = sbi (div_big_int (bis x) (bis y))
let modu x y = sbi (mod_big_int (bis x) (bis y))
let pow x y = sbi (power_big_int_positive_big_int (bis x) (bis y))

let lt x y = (lt_big_int (bis x) (bis y))
let le x y = (le_big_int (bis x) (bis y))
let eq x y = (eq_big_int (bis x) (bis y))
let ge x y = (ge_big_int (bis x) (bis y))
let gt x y = (gt_big_int (bis x) (bis y))

let mkint sr x = EXPR_literal (sr, {felix_type="int"; internal_value=x; c_value=x}) 
*)

let mkstring sr x = EXPR_literal (sr, {felix_type="string"; internal_value=x; 
  c_value=Flx_string.c_quote_of_string x}) 

let const_fold' e sr name arg =
(*
  let mkint x = mkint sr x in
*)
  let mkstring x = mkstring sr x in
  match name, arg with
(*
  (* integers *)
  (* -x *)
  | "neg", EXPR_literal (_,{felix_type="int"; internal_value=x})
    -> mkint (minus x)

  (* +x *)
  | "pos", EXPR_literal (_,{felix_type="int"; internal_value=x})
    -> mkint x

  (* abs x *)
  | "abs", EXPR_literal (_,{felix_type="int"; internal_value=x})
    -> mkint (abs x)

  (* x+y *)
  | "+", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    -> mkint (add x y)

  (* x-y *)
  | "-", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
     mkint (sub x y)

  (* x*y *)
  | "*", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    mkint (mult x y)

  (* x/y *)
  | "/", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    let r =
      try div x y
      with Division_by_zero ->
        clierrx "[flx_desugar/flx_constfld.ml:87: E315] " sr "[constfld] Division by zero"
    in
    mkint r


  (* x mod y *)
  | "mod", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    let r =
      try modu x y
      with Division_by_zero ->
        clierrx "[flx_desugar/flx_constfld.ml:101: E316] " sr "[constfld] Division by zero"
    in
    mkint r

  (* x ** y *)
  | "pow", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    mkint (pow x y)

  (* x < y *)
  | "<", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (lt x y)

  (* x > y *)
  | ">", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (gt x y)

  (* x <= y *)
  | "<=", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (le x y)

  (* x >= y *)
  | ">=", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (ge x y)

  (* x == y *)
  | "==", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (eq x y)

  (* x != y *)
  | "!=", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="int"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    truth sr (not (eq x y))
*)

  (* strings *)
  (* x+y *)
  | "+", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="string"; internal_value=x});
           EXPR_literal (_,{felix_type="string"; internal_value=y})
          ])
    ->
    let r = String.concat "" [x; y] in mkstring r

  (* x*y *)
  | "*", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="string"; internal_value=x});
           EXPR_literal (_,{felix_type="int"; internal_value=y})
          ])
    ->
    let y =
      try
        int_of_string y
      with _ -> clierrx "[flx_desugar/flx_constfld.ml:179: E317] " sr "String repeat count too large"
    in
    if String.length x = 1 then
      let r = String.make y x.[0] in
      mkstring r
    else
    let s = Buffer.create (String.length x * y) in
    for i = 1 to y do
      Buffer.add_string s x
    done;
    let r = Buffer.contents s in
    mkstring r

  (* x == y *)
  | "==", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="string"; internal_value=x});
           EXPR_literal (_,{felix_type="string"; internal_value=y})
          ])
    ->
    truth sr (x = y)

  (* x != y *)
  | "!=", EXPR_tuple ( _, [
           EXPR_literal (_,{felix_type="string"; internal_value=x});
           EXPR_literal (_,{felix_type="string"; internal_value=y})
          ])
    ->
    truth sr (x <> y)


  (* bool *)
  (* not x *)
  | "lnot", EXPR_typed_case (_,x,TYP_unitsum 2)
    ->
    truth sr (x=0)

  (* x or y *)
  | "lor", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=1 || y=1)

  (* x and y *)
  | "land", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=1 && y=1)

  (* x eq y *)
  | "==", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=y)

  (* x ne y *)
  | "!=", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x<>y)

  | _ -> e

let rec const_fold e =
  let e' = map_expr const_fold e in
  match e' with
  | EXPR_not (sr, EXPR_not (sr2, e)) -> e (* could do more here .. *)

  | EXPR_apply (sr, (EXPR_name (_,name,[]),arg)) ->
    const_fold' e sr name arg

  | EXPR_apply ( sr, ((EXPR_literal (_,{felix_type="string"; internal_value=_}) as x), y)) ->
    const_fold' e sr "add" (EXPR_tuple (sr,[x;y]))

  | _ -> e'


