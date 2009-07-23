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

let const_fold' e sr name arg =
  match name, arg with
  (* integers *)
  (* -x *)
  | "neg", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", (minus_big_int x)))

  (* +x *)
  | "pos", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", x))

  (* abs x *)
  | "abs", EXPR_literal (_,AST_int ("int",x))
    ->
    EXPR_literal (sr,AST_int ("int", (abs_big_int x)))

  (* x+y *)
  | "add", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(add_big_int x y)))

  (* x-y *)
  | "sub", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(sub_big_int x y)))

  (* x*y *)
  | "mul", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    EXPR_literal (sr,AST_int ("int",(mult_big_int x y)))

  (* x/y *)
  | "div", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    let r =
      try div_big_int x y
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
      try mod_big_int x y
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
    EXPR_literal (sr,AST_int ("int",(power_big_int_positive_big_int x y)))

  (* x < y *)
  | "lt", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (lt_big_int x y)

  (* x > y *)
  | "gt", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (gt_big_int x y)

  (* x <= y *)
  | "le", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (le_big_int x y)

  (* x >= y *)
  | "ge", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (ge_big_int x y)

  (* x == y *)
  | "eq", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (eq_big_int x y)

  (* x != y *)
  | "ne", EXPR_tuple ( _, [
           EXPR_literal (_,AST_int ("int",x));
           EXPR_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (not (eq_big_int x y))

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
        int_of_big_int y
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
