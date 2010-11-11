open Flx_ast
open Flx_print
open Flx_exceptions
open List
open Flx_typing
open Big_int
open Flx_set
open Flx_maps

let truth sr r =
  let r = if r then 1 else 0 in
  EXPR_typed_case (sr,r,flx_bool)

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

let const_fold' e sr name arg =
  match name, arg with
  (* integers *)
  (* -x *)
  | "neg", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", (minus x)))

  (* +x *)
  | "pos", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", x))

  (* abs x *)
  | "abs", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", (abs x)))

  (* x+y *)
  | "add", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(add x y)))

  (* x-y *)
  | "sub", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(sub x y)))

  (* x*y *)
  | "mul", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(mult x y)))

  (* x/y *)
  | "div", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    let r =
      try div x y
      with Division_by_zero ->
        clierr sr "[constfld] Division by zero"
    in
    EXPR_literal (sr,AST_int ("int",r))


  (* x mod y *)
  | "mod", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    let r =
      try modu x y
      with Division_by_zero ->
        clierr sr "[constfld] Division by zero"
    in
    EXPR_literal (sr,AST_int ("int",r))

  (* x ** y *)
  | "pow", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(pow x y)))

  (* x < y *)
  | "lt", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (lt x y)

  (* x > y *)
  | "gt", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (gt x y)

  (* x <= y *)
  | "le", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (le x y)

  (* x >= y *)
  | "ge", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (ge x y)

  (* x == y *)
  | "eq", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (eq x y)

  (* x != y *)
  | "ne", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (not (eq x y))

  (* strings *)
  (* x+y *)
  | "add", EXPR_tuple ( _, [
           EXPR_literal (_,AST_string x);
           EXPR_literal (_,AST_string y)
          ])
    ->
    EXPR_literal (sr,AST_string (String.concat "" [x; y]))

  (* x*y *)
  | "mul", EXPR_tuple ( _, [
           EXPR_literal (_,AST_string x);
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    let y =
      try
        int_of_string y
      with _ -> clierr sr "String repeat count too large"
    in
    if String.length x = 1 then
      EXPR_literal (sr,AST_string (String.make y x.[0]))
    else
    let s = Buffer.create (String.length x * y) in
    for i = 1 to y do
      Buffer.add_string s x
    done;
    EXPR_literal (sr,AST_string (Buffer.contents s))

  (* x == y *)
  | "eq", EXPR_tuple ( _, [
           EXPR_literal (_,AST_string x);
           EXPR_literal (_,AST_string y)
          ])
    ->
    truth sr (x = y)

  (* x != y *)
  | "ne", EXPR_tuple ( _, [
           EXPR_literal (_,AST_string x);
           EXPR_literal (_,AST_string y)
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
    -> truth sr (x=1 or y=1)

  (* x and y *)
  | "land", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=1 && y=1)

  (* x eq y *)
  | "eq", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=y)

  (* x ne y *)
  | "ne", EXPR_tuple ( _, [
           EXPR_typed_case (_,x,TYP_unitsum 2);
           EXPR_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x<>y)

  | _ -> e

let rec const_fold e =
  let e' = map_expr const_fold e in
  match e' with
  | EXPR_apply (sr, (EXPR_name (_,name,[]),arg)) ->
    const_fold' e sr name arg

  | EXPR_apply ( sr, ((EXPR_literal (_,AST_string _) as x), y)) ->
    const_fold' e sr "add" (EXPR_tuple (sr,[x;y]))

  | _ -> e'
