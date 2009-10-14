open Flx_ast
open List
open Flx_mtypes2
open Flx_maps

let dummy_sr = Flx_srcref.make_dummy "[flx_tpat] generated"

(*
let type_of_tpattern syms p :
  typecode_t *
  (int * string) list *     (* variables for '?' terms *)
  int list *                (* variables for 'any' terms *)
  (int * string) list *     (* variables for 'as' terms *)
  (int * typecode_t) list   (* assignments for as terms *)
=
  let explicit_vars = ref [] in
  let any_vars = ref [] in
  let as_vars = ref [] in
  let eqns = ref [] in
  let rec tp p =
    match p with
    | TPAT_function (a,b) -> TYP_function (tp a, tp b)
    | TPAT_tuple ps -> TYP_tuple (map tp ps)
    | TPAT_sum ps -> TYP_sum (map tp ps)
    | TPAT_pointer p -> TYP_pointer (tp p)
    | TPAT_name (n,ps) -> `AST_name (dummy_sr,n,map tp ps)
    | TPAT_void -> `AST_void dummy_sr

    | TPAT_var n ->
      let j = !(syms.counter) in
      incr (syms.counter);
      explicit_vars := (j,n) :: !explicit_vars;
      TYP_var j

    | TPAT_any ->
      let j = !(syms.counter) in
      incr (syms.counter);
      any_vars := j :: !any_vars;
      TYP_var j

    | TPAT_as (t,n) ->
      let t = tp t in
      let j = !(syms.counter) in
      incr (syms.counter);
      as_vars := (j,n) :: !as_vars;
      eqns := (j,t) :: !eqns;
      t

    | TPAT_unitsum j -> TYP_unitsum j
    | TPAT_type_tuple ts -> TYP_type_tuple (map tp ts)
  in
    let t = tp p in
    t,!explicit_vars, !any_vars, !as_vars, !eqns

*)
let type_of_tpattern syms p =
  let explicit_vars = ref [] in
  let any_vars = ref [] in
  let as_vars = ref [] in
  let eqns = ref [] in

  let rec tp p = match map_type tp p with
    | TYP_patvar (dummy_sr, n) ->
      let j = fresh_bid syms.counter in
      explicit_vars := (j,n) :: !explicit_vars;
      TYP_var j

    | TYP_patany _ ->
      let j = fresh_bid syms.counter in
      any_vars := j :: !any_vars;
      TYP_var j

    (* NOTE CONFUSION! Is this a pattern assignment,
       or is it fixpoint binder? Or is this the
       same thing ..?

       Treated here as pattern assignment.

       1 + int * list as list => list
    *)
    | TYP_as (t,n) ->
      let t = tp t in
      let j = fresh_bid syms.counter in
      as_vars := (j,n) :: !as_vars;
      eqns := (j,t) :: !eqns;
      t

    | x -> x
  in
    let t = tp p in
    t,!explicit_vars, !any_vars, !as_vars, !eqns
