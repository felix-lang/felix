
open Flx_types
open Flx_btype
open Flx_util
open Flx_list
open Flx_exceptions
open Flx_bid
open Flx_type_aux

(* returns the most general unifier (mgu)
  of a set of type equations as a list
  of variable assignments i -> t
  or raises Not_found if there is no solution

  HOW IT WORKS:

  We start with some set of type equations
  t1 = t2
  t3 = t4  (1)
  ...

  in which the LHS and RHS are general terms that
  may contain type variables.

  We want to say whether the equations are consistent,
  and if so, to return a solution of the form
  of a set of equations:

  v1 = u1
  v2 = u2   (2)

  where v1 .. vn are type variable
  which do not occur in any of the
  terms u1 .. un

  Such a set is a solution if by replacing v1 with u1,
  v2 with u2 .. vn with un,
  everywhere they occur in t1, t2 .... tn,
  the original equations are reduced to
  equations terms which are structurally equal

  The technique is to pick one equation,
  and match up the outermost structure,
  making new equations out of the pieces in the middle,
  or failing if the outer structure does not match.

  We discard the original equation,
  add the new equations to the set,
  and then for any variable assignments of form (2)
  found, we eliminate that variable in the
  all the other equations by substitution.


  At the end we are guarrateed to either have found
  the equations have no solution, or computed one,
  although it may be that the terms u1 .. u2 ..
  contain some type variables.

  There is a caveat though: we may obtain
  an equation

    v = t

  where v occurs in t, that is, a recursive equation.
  If that happens, we eliminate the occurences
  of v in t before replacement in other equations:
  we do this by replacing the RHS occurences of
  v with a fixpoint operator.

*)

let var_i_occurs i t =
  let rec f_btype t =
    match t with
    | BTYP_type_var (j,_) when i = j -> raise Not_found
    | _ -> Flx_btype.flat_iter ~f_btype t
 in
   try
     f_btype t;
     false
   with Not_found -> true

let rec vars_in t =
  let vs = ref BidSet.empty in
  let add_var i = vs := BidSet.add i !vs in
  let rec f_btype t =
    match t with
    | BTYP_type_var (i,_) -> add_var i
    | _ -> Flx_btype.flat_iter ~f_btype t
  in
  f_btype t;
  !vs

let var_list_occurs ls t =
  let yes = ref false in
  List.iter (fun i -> yes := !yes || var_i_occurs i t) ls;
  !yes

(* NOTE: this algorithm unifies EQUATIONS
  not inequations, therefore it doesn't
  handle any subtyping
*)

(* NOTE: at the moment,
  unification doesn't care about type variable
  meta types: we should probably require them
  to be the same for an assignment or fail
  the unification .. however that requires
  comparing the metatypes for equality, and to that
  that right requires unification .. :)
*)

(* NOTE: there is a serious problem with this algorithm.

   If one type is unfolded it will fail. To fix this we simply
   insist that the arguments are minimised, then, modulo
   type variables, they argument have to be equal and finite.

   However there is a problem: the algorithm terminates because
   it only analyses types. But it can analyse down inside
   an implied fixpoint binder. To fix this, we need to unfold
   the types before analysis to the pieces don't contain 
   free fixpoints.

   The problem is that then the algorithm may not terminate.
   We can just keep unfolding forever.

   To fix this I'm trying a HACK. The idea is to keep a history
   of every equation ever processed in the form it is originally
   submitted. When an equation is submitted by analysis it is first
   checked against the history. If it has already been submitted it
   is not added to the list to be processed, on the basis that
   adding it cannot add any new information.

   The termination proof is roughly that for the loop to
   run forever, without re-submitting an equation, it would
   have to generate something such as a new type variable
   that didn't exist before. Perhaps that can happen!
   
   Unfolding also clearly generates something.
   However the argument is that this cannot get out of hand,
   because eventually we must get back to analysing the same
   pair of terms.

  So here's the situation: the only way this can actually
  fail is if we synthesise new variables. And we can do that,
  we're explicitly passing a counter in to allow it!

  It is used in two places: first for type_apply and type_match.
  Ouch. This fairly clearly does require alpha conversion.

  Second it is used at the end of the loop, to simplify the
  set of equations by removing type variables once we have
  assignments for them. Alpha conversion should not be required
  here EXCEPT for type match and apply, for the same reason as
  above.

  The test would be more robust if the comparison for equality
  were replaced by type_eq, which takes alternate names of function
  parameters into account. But it would also be dead slow!

*)

let rec memq trail (a,b) = match trail with
  | [] -> false
  | (i,j)::t -> i == a && j == b || memq t (a,b)

let rec type_eq' sbt counter ltrail ldepth rtrail rdepth trail t1 t2 =
  (* print_endline (sbt sym_table t1 ^ " =? " ^ sbt sym_table t2); *)
  if memq trail (t1,t2) then true
  else let te a b = type_eq' sbt counter
    ((ldepth,t1)::ltrail) (ldepth+1)
    ((rdepth,t2)::rtrail) (rdepth+1)
    ((t1,t2)::trail)
    a b
  in
  (*
  let assoc i ls =
    try List.assoc i ls
    with Not_found -> raise Not_found
      (*
      print_endline (
      "trail failure, index=" ^ si i ^ ", trail="
      ^ catmap "," (fun (k,t) -> si k ^ "->" ^ sbt sym_table t) ls
      );
      failwith "Trail failure"
      *)
  in
  *)
  match t1,t2 with
  | BTYP_hole,_ (* for zippers *)
  | _,BTYP_hole -> true

  | BTYP_label,BTYP_label -> true
  | BTYP_type i, BTYP_type j -> i = j
  | BTYP_inst (i1,ts1),BTYP_inst (i2,ts2) ->
    i1 = i2 &&
    List.length ts1 = List.length ts2 &&
    List.fold_left2
    (fun tr a b -> tr && te a b)
    true ts1 ts2

  | BTYP_unitsum i,BTYP_unitsum j -> i = j

  | BTYP_sum ts1, BTYP_sum ts2
  | BTYP_type_tuple ts1,BTYP_type_tuple ts2
  | BTYP_tuple ts1,BTYP_tuple ts2 ->
    let result =
    if List.length ts1 = List.length ts2
    then
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true ts1 ts2
    else false
    in
    (*
    print_endline ("Tuple/sum compared " ^ (if result then "TRUE" else "FALSE"));
    if List.length ts1 = List.length ts2 then
    print_endline ("Args = " ^ catmap "\n  " (fun (t1,t2) ->
      "lhs=" ^sbt sym_table t1 ^" vs rhs=" ^ sbt sym_table t2)
     (combine ts1 ts2))
    else print_endline ("unequal lengths");
    *)
    result

  | BTYP_record ([]),BTYP_tuple []
  | BTYP_tuple [],BTYP_record ([]) -> true

  | BTYP_polyrecord (t1,v1), BTYP_polyrecord (t2,v2) ->
   te (btyp_record t1) (btyp_record t2) && te v1 v2 

  | BTYP_record (t1),BTYP_record (t2) ->
    if List.length t1 = List.length t2
    then begin
      List.map fst t1 = List.map fst t2 &&
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true (List.map snd t1) (List.map snd t2)
    end else false

  | BTYP_variant [],BTYP_tuple []
  | BTYP_tuple [],BTYP_variant [] -> true

  | BTYP_variant t1,BTYP_variant t2 ->
    if List.length t1 = List.length t2
    then begin
      (* should not be needed but variants aren't implemented yet *)
      let rcmp (s1,_) (s2,_) = compare s1 s2 in
      let t1 = List.sort rcmp t1 in
      let t2 = List.sort rcmp t2 in
      List.map fst t1 = List.map fst t2 &&
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true (List.map snd t1) (List.map snd t2)
    end else false


  | BTYP_array (s1,d1),BTYP_array (s2,d2)
  | BTYP_function (s1,d1),BTYP_function (s2,d2)
  | BTYP_cfunction (s1,d1),BTYP_cfunction (s2,d2)
  | BTYP_type_apply(s1,d1),BTYP_type_apply(s2,d2)
  | BTYP_type_map(s1,d1),BTYP_type_map(s2,d2)
  | BTYP_tuple_cons (s1,d1),BTYP_tuple_cons (s2,d2)
    -> te s1 s2 && te d1 d2

  | BTYP_tuple_snoc (s1,d1),BTYP_tuple_snoc (s2,d2)
    -> te s1 s2 && te d1 d2


  | BTYP_effector (s1,e1,d1),BTYP_effector (s2,e2,d2)
    -> te s1 s2 && te d1 d2 && te e1 e2

  (* order is important for lvalues .. *)
  | BTYP_array (ta,BTYP_unitsum n),BTYP_tuple ts
    when List.length ts = n ->
    List.fold_left (fun tr t -> tr && te ta t) true ts


  | BTYP_tuple ts,BTYP_array (ta,BTYP_unitsum n)
    when List.length ts = n ->
    List.fold_left (fun tr t -> tr && te t ta) true ts

  | BTYP_uniq p1,BTYP_uniq p2
  | BTYP_rref p1,BTYP_rref p2
  | BTYP_pointer p1,BTYP_pointer p2
    -> te p1 p2

  | BTYP_void,BTYP_void
    -> true

  | BTYP_type_var (i,_), BTYP_type_var (j,_) ->
    let result = i = j in
    (*
    print_endline ("Type variables compared " ^ (if result then "TRUE" else "FALSE"));
    *)
    result


(*
  | BTYP_fix (0,BTYP_type 0),_ 
  | _,BTYP_fix (0,BTYP_type 0) -> true
*)

  | BTYP_fix (i,t1),BTYP_fix (j,t2) ->
    (*
    print_endline ("Check fixpoint " ^ si i ^ " vs " ^ si j);
    *)
    if i = j then begin 
      if t1 <> t2 then print_endline "[type_eq] Fix points at same level have different metatypes";
(*
      true
*)
      (* should be correct .. but something breaks , it seems to clobber the trail.
         but we're going down a level from types to meta-types, so this shouldn't
         happen
      *)
      te t1 t2 
    end else (* hack ..? *)
    begin
      (*
      print_endline "Matching fixpoints";
      *)
      try
      let a = List.assoc (ldepth+i) ltrail in
      let b = List.assoc (rdepth+j) rtrail in
      type_eq' sbt counter ltrail ldepth rtrail rdepth trail a b
      with Not_found -> false
    end

  | BTYP_fix (i,mt1),t ->
    (*
    print_endline "LHS fixpoint";
    *)
    begin try
    let a = List.assoc (ldepth+i) ltrail in
    type_eq' sbt counter ltrail ldepth rtrail rdepth trail a t
    with Not_found -> false
    end

  | t,BTYP_fix (j,mt2) ->
    (*
    print_endline "RHS fixpoint";
    *)
    begin try
    let b = List.assoc (rdepth+j) rtrail in
    type_eq' sbt counter ltrail ldepth rtrail rdepth trail t b
    with Not_found -> false
    end

  | BTYP_type_function (p1,r1,b1), BTYP_type_function (p2,r2,b2) ->
    List.length p1 = List.length p2 &&
    let vs = List.map2 (fun (i1,_) (i2,t) -> i1,btyp_type_var (i2,t))  p1 p2 in
(*
    print_endline "Comparing type functions";
    print_endline ("b1 =          " ^ sbt b1);
*)
    let b1 = list_subst counter vs b1 in
(*
    print_endline ("Adjusted b1 = " ^ sbt b1);
    print_endline ("b2 =          " ^ sbt b2);
*)
    let result = te b1 b2 in
(*
    print_endline ("Compared = " ^ (if result then "TRUE" else "FALSE"));
*)
    result

  | l,r ->
(*
    print_endline ("WOOOPS .. dunno.." ^ sbt l ^" vs " ^ sbt r);
*)
    false

let type_eq sbt counter t1 t2 = (* print_endline "TYPE EQ";  *)
  type_eq' sbt counter [] 0 [] 0 [] t1 t2

let type_match sbt counter t1 t2 = (* print_endline "TYPE MATCH"; *)
  type_eq' sbt counter [] 0 [] 0 [] t1 t2


