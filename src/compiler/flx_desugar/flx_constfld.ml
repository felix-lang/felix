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
  `AST_typed_case (sr,r,flx_bool)

let const_fold' e sr name arg =
  match name, arg with
  (* integers *)
  (* -x *)
  | "neg", `AST_literal (_,AST_int ("int",x))
    ->
    `AST_literal (sr,AST_int ("int", (minus_big_int x)))

  (* +x *)
  | "pos", `AST_literal (_,AST_int ("int",x))
    ->
    `AST_literal (sr,AST_int ("int", x))

  (* abs x *)
  | "abs", `AST_literal (_,AST_int ("int",x))
    ->
    `AST_literal (sr,AST_int ("int", (abs_big_int x)))

  (* x+y *)
  | "add", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    `AST_literal (sr,AST_int ("int",(add_big_int x y)))

  (* x-y *)
  | "sub", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    `AST_literal (sr,AST_int ("int",(sub_big_int x y)))

  (* x*y *)
  | "mul", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    `AST_literal (sr,AST_int ("int",(mult_big_int x y)))

  (* x/y *)
  | "div", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    let r =
      try div_big_int x y
      with Division_by_zero ->
        clierr sr "[constfld] Division by zero"
    in
    `AST_literal (sr,AST_int ("int",r))


  (* x mod y *)
  | "mod", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    let r =
      try mod_big_int x y
      with Division_by_zero ->
        clierr sr "[constfld] Division by zero"
    in
    `AST_literal (sr,AST_int ("int",r))

  (* x ** y *)
  | "pow", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    `AST_literal (sr,AST_int ("int",(power_big_int_positive_big_int x y)))

  (* x < y *)
  | "lt", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (lt_big_int x y)

  (* x > y *)
  | "gt", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (gt_big_int x y)

  (* x <= y *)
  | "le", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (le_big_int x y)

  (* x >= y *)
  | "ge", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (ge_big_int x y)

  (* x == y *)
  | "eq", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (eq_big_int x y)

  (* x != y *)
  | "ne", `AST_tuple ( _, [
           `AST_literal (_,AST_int ("int",x));
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    truth sr (not (eq_big_int x y))

  (* strings *)
  (* x+y *)
  | "add", `AST_tuple ( _, [
           `AST_literal (_,AST_string x);
           `AST_literal (_,AST_string y)
          ])
    ->
    `AST_literal (sr,AST_string (String.concat "" [x; y]))

  (* x*y *)
  | "mul", `AST_tuple ( _, [
           `AST_literal (_,AST_string x);
           `AST_literal (_,AST_int ("int",y))
          ])
    ->
    let y =
      try
        int_of_big_int y
      with _ -> clierr sr "String repeat count too large"
    in
    if String.length x = 1 then
      `AST_literal (sr,AST_string (String.make y x.[0]))
    else
    let s = Buffer.create (String.length x * y) in
    for i = 1 to y do
      Buffer.add_string s x
    done;
    `AST_literal (sr,AST_string (Buffer.contents s))

  (* x == y *)
  | "eq", `AST_tuple ( _, [
           `AST_literal (_,AST_string x);
           `AST_literal (_,AST_string y)
          ])
    ->
    truth sr (x = y)

  (* x != y *)
  | "ne", `AST_tuple ( _, [
           `AST_literal (_,AST_string x);
           `AST_literal (_,AST_string y)
          ])
    ->
    truth sr (x <> y)


  (* bool *)
  (* not x *)
  | "lnot", `AST_typed_case (_,x,TYP_unitsum 2)
    ->
    truth sr (x=0)

  (* x or y *)
  | "lor", `AST_tuple ( _, [
           `AST_typed_case (_,x,TYP_unitsum 2);
           `AST_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=1 or y=1)

  (* x and y *)
  | "land", `AST_tuple ( _, [
           `AST_typed_case (_,x,TYP_unitsum 2);
           `AST_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=1 && y=1)

  (* x eq y *)
  | "eq", `AST_tuple ( _, [
           `AST_typed_case (_,x,TYP_unitsum 2);
           `AST_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x=y)

  (* x ne y *)
  | "ne", `AST_tuple ( _, [
           `AST_typed_case (_,x,TYP_unitsum 2);
           `AST_typed_case (_,y,TYP_unitsum 2)
          ])
    -> truth sr (x<>y)

  | _ -> e

let rec const_fold e =
  let e' = map_expr const_fold e in
  match e' with
  | `AST_apply (sr, (`AST_name (_,name,[]),arg)) ->
    const_fold' e sr name arg

  | `AST_apply ( sr, (( `AST_literal (_,AST_string _) as x), y)) ->
    const_fold' e sr "add" (`AST_tuple (sr,[x;y]))

  | _ -> e'
