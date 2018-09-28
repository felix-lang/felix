open Flx_ast
open List
open Flx_mtypes2
open Flx_maps
open Flx_types
open Flx_btype
open Flx_unify
open Flx_print
open Flx_btype_subst
open Flx_bid

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
let type_of_tpattern counter p =
  let explicit_vars = ref [] in
  let any_vars = ref [] in
  let as_vars = ref [] in
  let eqns = ref [] in

  let rec tp p = match map_type tp p with
    | TYP_patvar (dummy_sr, n) ->
      let j = fresh_bid counter in
      explicit_vars := (j,n) :: !explicit_vars;
      TYP_var j

    | TYP_patany _ ->
      let j = fresh_bid counter in
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
      let j = fresh_bid counter in
      as_vars := (j,n) :: !as_vars;
      eqns := (j,t) :: !eqns;
      t

    | x -> x
  in
    let t = tp p in
    t,!explicit_vars, !any_vars, !as_vars, !eqns


let bind_type_match bsym_table counter bt btp params sr t ps ubt =
  let pts = ref [] in
  let finished = ref false in
  List.iter begin fun (p',t') ->
    let p',explicit_vars,any_vars, as_vars, eqns =
      type_of_tpattern counter p'
    in
    let p' = bt p' in
    let eqns = List.map (fun (j,t) -> j, bt t) eqns in
    let varset =
      let x = List.fold_left
        (fun s (i,_) -> BidSet.add i s)
        BidSet.empty explicit_vars
      in
      List.fold_left (fun s i -> BidSet.add i s)
      x any_vars
    in

    (* HACK! GACK! we have to assume a variable in a pattern is is a TYPE
     * variable .. type patterns don't include coercion terms at the moment,
     * so there isn't any way to even specify the metatype In some contexts
     * the kinding can be infered, for example:
     *
     * int * ?x
     *
     * clearly x has to be a type .. but a lone type variable would require
     * the argument typing to be known ... no notation for that yet either
     * *)
    let args = List.map (fun (i,s) ->
    s, btyp_type_var (i,btyp_type 0)) (explicit_vars @ as_vars)
    in
    let t' = btp t' (params@args) in
    let t' = list_subst counter eqns t' in
    pts := ({pattern=p'; pattern_vars=varset; assignments=eqns},t') :: !pts;
    let u = maybe_unification bsym_table counter [p', t] in
    match u with
    | None ->  ()
        (* CRAP! The below argument is correct BUT ..  our unification
         * algorithm isn't strong enough ...  so just let this thru and hope
         * it is reduced later on instantiation
         *)
        (* If the initially bound, context free pattern can never unify with
         * the argument, we have a choice: chuck an error, or just eliminate
         * the match case -- I'm going to chuck an error for now, because I
         * don't see why one would ever code such a case, except as a
         * mistake. *)
        (*
        clierrx "[flx_bind/flx_tpat.ml:145: E262] " sr
          ("[bind_type'] type match argument\n" ^
          sbt bsym_table t ^
          "\nwill never unify with pattern\n" ^
          sbt bsym_table p'
          )
        *)
    | Some mgu ->
        if !finished then begin
(*
          print_endline "[bind_type] Warning: useless match case in typematch ignored:";
          print_endline (Flx_srcref.short_string_of_src sr);
          print_endline (string_of_typecode ubt);
*)
        end
        else
          let mguvars = List.fold_left
            (fun s (i,_) -> BidSet.add i s)
            BidSet.empty mgu
          in
          if varset = mguvars then finished := true
  end ps;
  let pts = List.rev !pts in

  btyp_type_match (t,pts)

let bind_subtype_match bsym_table counter bt btp params sr t ps ubt =
  let pts = ref [] in
  let finished = ref false in
  List.iter begin fun (p',t') ->
    let p',explicit_vars,any_vars, as_vars, eqns =
      type_of_tpattern counter p'
    in
    let p' = bt p' in
    let eqns = List.map (fun (j,t) -> j, bt t) eqns in
    let varset =
      let x = List.fold_left
        (fun s (i,_) -> BidSet.add i s)
        BidSet.empty explicit_vars
      in
      List.fold_left (fun s i -> BidSet.add i s)
      x any_vars
    in

    (* HACK! GACK! we have to assume a variable in a pattern is is a TYPE
     * variable .. type patterns don't include coercion terms at the moment,
     * so there isn't any way to even specify the metatype In some contexts
     * the kinding can be infered, for example:
     *
     * int * ?x
     *
     * clearly x has to be a type .. but a lone type variable would require
     * the argument typing to be known ... no notation for that yet either
     * *)
    let args = List.map (fun (i,s) ->
    s, btyp_type_var (i,btyp_type 0)) (explicit_vars @ as_vars)
    in
    let t' = btp t' (params@args) in
    let t' = list_subst counter eqns t' in
    pts := ({pattern=p'; pattern_vars=varset; assignments=eqns},t') :: !pts;
    let u = maybe_specialisation bsym_table counter [p', t] in
    match u with
    | None ->  ()
        (* CRAP! The below argument is correct BUT ..  our unification
         * algorithm isn't strong enough ...  so just let this thru and hope
         * it is reduced later on instantiation
         *)
        (* If the initially bound, context free pattern can never unify with
         * the argument, we have a choice: chuck an error, or just eliminate
         * the match case -- I'm going to chuck an error for now, because I
         * don't see why one would ever code such a case, except as a
         * mistake. *)
        (*
        clierrx "[flx_bind/flx_tpat.ml:145: E262] " sr
          ("[bind_type'] type match argument\n" ^
          sbt bsym_table t ^
          "\nwill never unify with pattern\n" ^
          sbt bsym_table p'
          )
        *)
    | Some mgu ->
        if !finished then begin
(*
          print_endline "[bind_type] Warning: useless match case in typematch ignored:";
          print_endline (Flx_srcref.short_string_of_src sr);
          print_endline (string_of_typecode ubt);
*)
        end
        else
          let mguvars = List.fold_left
            (fun s (i,_) -> BidSet.add i s)
            BidSet.empty mgu
          in
          if varset = mguvars then finished := true
  end ps;
  let pts = List.rev !pts in

  btyp_subtype_match (t,pts)


