open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_maps
open Flx_util
open Flx_list
open Flx_exceptions

let unit_t = btyp_tuple []

(* NOTE: this routine doesn't adjust fixpoints! Probably should! *)
let normalise_tuple_cons bsym_table t = 
  let rec nt t = 
    match Flx_btype.map ~f_btype:nt t with
    | BTYP_tuple_cons (t1, BTYP_tuple ls) ->
      let r = BTYP_tuple (t1 :: ls) in
      r

    | BTYP_tuple_cons (t1, BTYP_array (t2, BTYP_unitsum n)) when t1 = t2 ->
      let r = BTYP_array (t1, BTYP_unitsum (n+1)) in
      r

    | BTYP_tuple_cons (t1, BTYP_array (t2, BTYP_unitsum n)) ->
      assert (n < 50);
      let rec arr n ts = match n with 0 -> ts | _ -> arr (n-1) (t2::ts) in
      let ts = arr n [] in
      let r = BTYP_tuple (t1 :: ts) in
      r

(*
    | BTYP_tuple_cons (t1, (BTYP_type_var _ as v)) ->
      BTYP_tuple_cons (nt t1, v)
*)
    | t -> t 
  in 
  let t' = nt t in
(*
  if t' <> t then
    print_endline ("Normalise " ^ sbt bsym_table t ^ " --> " ^ sbt bsym_table t');
*)
  t'

let rec dual t =
  match Flx_btype.map ~f_btype:dual t with
  | BTYP_none -> assert false

  | BTYP_sum ls ->
    begin match ls with
    | [t] -> t
    | ls -> btyp_tuple ls
    end

  | BTYP_tuple ls ->
    begin match ls with
    | [] -> btyp_void ()
    | [t] -> t
    | ls -> btyp_sum ls
    end

  | BTYP_function (a,b) -> btyp_function (b,a)
  | BTYP_effector (a,e,b) -> btyp_effector (b,e,a)
  | BTYP_cfunction (a,b) -> btyp_cfunction (b,a)
  | BTYP_array (a,b) -> btyp_array (b,a)

  | BTYP_pointer t -> btyp_pointer (dual t)
  | BTYP_void -> unit_t
  | BTYP_unitsum k ->
    let rec aux ds k = if k = 0 then ds else aux (unit_t::ds) (k-1) in
    btyp_tuple (aux [] k)

  | BTYP_type_set ts -> btyp_intersect (List.map dual ts)
  | BTYP_intersect ts -> btyp_type_set (List.map dual ts)
  | BTYP_record (ts) -> btyp_variant ts
  | t -> t

(* top down check for fix point not under sum or pointer *)
let rec check_recursion t = match t with
   | BTYP_pointer _
   | BTYP_sum _
   | BTYP_function _
   | BTYP_effector _
   | BTYP_cfunction _
     -> ()

   | BTYP_fix (i,_)
     -> raise Bad_recursion

   | x -> Flx_btype.flat_iter ~f_btype:check_recursion x

let is_recursive_type t = 
  let rec ir j t = 
    match t with
    | BTYP_fix (i,_) when i = j -> raise Not_found (* means yes *)
    | _ ->
      let f_btype t = ir (j-1) t in
      Flx_btype.flat_iter ~f_btype t
  in try ir 0 t; false with Not_found -> true
 
let var_subst t (i, j) =
  let rec f_btype t =
    match t with
    | BTYP_type_var (k,t) when i = k -> btyp_type_var (j,t)
    | t -> Flx_btype.map ~f_btype t
  in
  f_btype t

let vars_subst ls t = List.fold_left var_subst t ls

let rec alpha counter t =
  match t with
  | BTYP_type_function (ps,r,b) ->
      let remap_list = List.map (fun (i,_) -> i, fresh_bid counter) ps in
      let remap i = List.assoc i remap_list in
      let cvt t = alpha counter (vars_subst remap_list t) in
      let ps = List.map (fun (i,t) -> remap i,t) ps in
      btyp_type_function (ps, cvt r, cvt b)
  | t -> Flx_btype.map ~f_btype:(alpha counter) t

let term_subst counter t1 i t2 =
  let rec f_btype t =
    match t with
    | BTYP_type_var (k,_) when k = i -> t2

    | BTYP_type_match (tt, pts) ->
        let tt = f_btype tt in
        let pts =
          List.map begin fun ((bpat, x) as case) ->
            if BidSet.mem i bpat.pattern_vars then case else
            let asgs = List.map (fun (i,t) -> i,f_btype t) bpat.assignments in
            { bpat with
              pattern=f_btype bpat.pattern;
              assignments=asgs }, f_btype x
          end pts
        in
        btyp_type_match (tt,pts)

    | t -> Flx_btype.map ~f_btype t
  in
  f_btype t1

let list_subst counter x t =
  let t = alpha counter t in
  List.fold_left (fun t1 (i,t2) ->
    term_subst counter t1 i (alpha counter t2))
  t
  x

let varmap0_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,_) as x ->
        if Hashtbl.mem varmap i
        then Hashtbl.find varmap i
        else x
    | x -> x
  in
  f_btype t

let varmap_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,_) as x ->
        if Hashtbl.mem varmap i
        then Hashtbl.find varmap i
        else x
    | BTYP_type_function (p,r,b) ->
        let
          p = List.map (fun (name,kind) -> (name,f_btype kind)) p and
          r = f_btype r and
          b = f_btype b
        in
        btyp_type_function (p,r,b)
    | x -> x
  in
  f_btype t

(* the type arguments are matched up with the type
  variables in order so that
  vs_i -> ts_i
  where vs_t might be (fred,var j)
*)
let mk_varmap sr vs ts =
  if List.length ts <> List.length vs
  then
    clierrx "[flx_core/flx_unify.ml:188: E280] " sr 
    (
      "[mk_varmap] wrong number of type args, expected vs=" ^
      si (List.length vs) ^
      ", got ts=" ^
      si (List.length ts) ^
      "\nvs= " ^ catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs
    )
  ;
  let varmap = Hashtbl.create 97 in
  List.iter2
  (fun (_, varidx) typ -> Hashtbl.add varmap varidx typ)
  vs ts
  ;
  varmap

let tsubst sr vs ts t =
  varmap_subst (mk_varmap sr vs ts) t

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

let fix i t =
  let rec aux n t =
    let aux t = aux (n - 1) t in
    match t with
    | BTYP_hole -> assert false
    | BTYP_tuple_cons _ -> assert false
    | BTYP_none -> assert false
    | BTYP_type_var (k,mt) -> if k = i then btyp_fix n mt else t
    | BTYP_inst (k,ts) -> btyp_inst (k, List.map aux ts)
    | BTYP_tuple ts -> btyp_tuple (List.map aux ts)
    | BTYP_sum ts -> btyp_sum (List.map aux ts)
    | BTYP_intersect ts -> btyp_intersect (List.map aux ts)
    | BTYP_type_set ts -> btyp_type_set (List.map aux ts)
    | BTYP_function (a,b) -> btyp_function (aux a, aux b)
    | BTYP_effector (a,e,b) -> btyp_effector (aux a, aux e, aux b)
    | BTYP_cfunction (a,b) -> btyp_cfunction (aux a, aux b)
    | BTYP_pointer a -> btyp_pointer (aux a)
    | BTYP_array (a,b) -> btyp_array (aux a, aux b)

    | BTYP_record (ts) ->
       btyp_record (List.map (fun (s,t) -> s, aux t) ts)

    | BTYP_polyrecord (ts,v) ->
       btyp_polyrecord (List.map (fun (s,t) -> s, aux t) ts) (aux v)

    | BTYP_variant ts ->
       btyp_variant (List.map (fun (s,t) -> s, aux t) ts)

    | BTYP_int 
    | BTYP_label 
    | BTYP_unitsum _
    | BTYP_void
    | BTYP_fix _
    | BTYP_type_apply _
    | BTYP_type_function _
    | BTYP_type _
    | BTYP_type_tuple _
    | BTYP_type_match _
    | BTYP_type_set_union _ -> t
    | BTYP_type_set_intersection _ -> t
  in
    aux 0 t

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


let rec unification bsym_table counter eqns dvars =
  (*
  print_endline ( "Dvars = { " ^ catmap ", " si (BidSet.elements dvars) ^ "}");
  *)
  let history = ref eqns in
  let eqns = ref eqns in
  let mgu = ref [] in
  let add_eqn eqn =
    if List.mem eqn (!history) then ()
    else begin
       eqns := eqn :: (!eqns);
       history := eqn :: (!history)
    end
  in
  let rec loop () : unit =
    match !eqns with
    | [] -> ()
    | h :: t ->
      eqns := t;
      let s = ref None in
      let lhs,rhs = h in 
      let lhs = unfold "unification" lhs in
      let rhs = unfold "unification" rhs in
      begin match lhs,rhs with
      | (BTYP_type_var (i,mi) as ti), (BTYP_type_var (j,mj) as tj)->
        (*
        print_endline ("Equated variables " ^ si i ^ " <-> " ^ si j);
        *)

        (* meta type have to agree *)
        if mi <> mj then raise Not_found;

        if i <> j then
          if BidSet.mem i dvars then
            s := Some (i,tj)
          else if BidSet.mem j dvars then
            s := Some (j,ti)
          else raise Not_found
      | BTYP_type_var (i,_), t
      | t,BTYP_type_var (i,_) ->
        (*
        print_endline ("variable assignment " ^ si i ^ " -> " ^ sbt sym_table t);
        *)

        (* WE SHOULD CHECK THAT t has the right meta type .. but
        the metatype routine isn't defined yet ..
        *)
        if not (BidSet.mem i dvars) then raise Not_found;
        if var_i_occurs i t
        then begin
          (*
          print_endline
          (
            "recursion in unification, terms: " ^
            match h with (a,b) ->
            sbt sym_table a ^ " = " ^ sbt sym_table b
          );
          *)
          s := Some (i, fix i t)
        end else begin
          (*
          print_endline "Adding substitution";
          *)
          s := Some (i,t)
        end

      | BTYP_intersect ts,t
      | t,BTYP_intersect ts ->
        List.iter (function t' -> add_eqn (t,t')) ts

      | BTYP_pointer t1, BTYP_pointer t2 ->
        add_eqn (t1,t2)

      | BTYP_unitsum i, BTYP_unitsum j when i = j -> ()

      | BTYP_unitsum k, BTYP_sum ls
      | BTYP_sum ls, BTYP_unitsum k when List.length ls = k ->
        List.iter
        (function
          | BTYP_type_var _ as v ->
             add_eqn (v,unit_t)
          | _ -> raise Not_found
        )
        ls

      | BTYP_array (t11, t12), BTYP_array (t21, t22)
      | BTYP_function (t11, t12), BTYP_function (t21, t22)
      | BTYP_cfunction (t11, t12), BTYP_cfunction (t21, t22) ->
        add_eqn (t11,t21); add_eqn (t12,t22)

      (* FIXME *)
      | BTYP_effector (t11, t12, t13), BTYP_effector (t21, t22, t23) ->
        add_eqn (t11,t21); add_eqn (t12,t22); add_eqn (t13, t23)

      | BTYP_effector (t11, t12, t13), BTYP_function (t21, t23)
      | BTYP_function (t21, t23), BTYP_effector (t11, t12, t13) ->
        let t22 = btyp_tuple [] in
        add_eqn (t11,t21); add_eqn (t12,t22); add_eqn (t13, t23)



      | BTYP_record ([]),BTYP_tuple []
      | BTYP_tuple [],BTYP_record ([]) -> ()

      | BTYP_polyrecord (t1,v1),BTYP_polyrecord (t2,v2) ->
(*
print_endline ("Polyrecord/polyrecord unification " ^ sbt bsym_table lhs ^ " = " ^ sbt bsym_table rhs);
*)
        let extra1 = ref [] in
        let extra2 = ref [] in
        List.iter (fun (s,t) -> 
          if List.mem_assoc s t2 
          then add_eqn (t, List.assoc s t2)
          else extra1 := (s,t) :: !extra1
        )
        t1;
        List.iter (fun (s,t) -> 
          if List.mem_assoc s t1 
          then ()
          else extra2 := (s,t) :: !extra2
        )
        t2;
        begin match !extra1, !extra2 with
        | [],[] -> 
(*
          print_endline "  *** matching fields";
*)
          add_eqn (v1,v2)
        | x,[] -> 
(*
          print_endline "  *** more fields on left";
          print_endline ("  *** add eqn " ^ sbt bsym_table (btyp_polyrecord x v1) ^ " = " ^ sbt bsym_table v2);
*)
          add_eqn (btyp_polyrecord x v1, v2)
        | [],x -> 
(*
          print_endline "  *** more fields on right";
          print_endline ("  *** add eqn " ^ sbt bsym_table v1 ^ " = " ^ sbt bsym_table (btyp_polyrecord x v2));
*)
          add_eqn (v1, btyp_polyrecord x v2)
        | _ -> 
(*
          print_endline "  *** FAILED"; 
*)
          raise Not_found (* cant unify *)
        end
  

      | BTYP_polyrecord (t1,v),BTYP_record (t2)
      | BTYP_record (t2),BTYP_polyrecord (t1,v) -> 
(*
print_endline ("Polyrecord/record unification " ^ sbt bsym_table lhs ^ " = " ^ sbt bsym_table rhs);
*)
        let extra = ref [] in
        List.iter (fun (s,t) -> 
          if List.mem_assoc s t2 
          then add_eqn (t, List.assoc s t2)
          else raise Not_found 
        )
        t1;
        List.iter (fun (s,t) -> 
          if List.mem_assoc s t1 
          then ()
          else extra := (s,t) :: !extra
        )
        t2;

        add_eqn (v,btyp_record (!extra))

      | BTYP_record (t1),BTYP_record (t2) ->
        if List.length t1 = List.length t2
        then begin
          if (List.map fst t1) <> (List.map fst t2) then raise Not_found;
            List.iter2 (fun a b -> add_eqn (snd a, snd b)) t1 t2;
            s := None
        end
        else raise Not_found

      | BTYP_variant [],BTYP_void
      | BTYP_void,BTYP_variant [] -> ()

      | BTYP_variant t1,BTYP_variant t2 ->
        if List.length t1 = List.length t2
        then begin
          (* FIXME: should not be needed but variants aren't implemented yet *)
          let rcmp (s1,_) (s2,_) = compare s1 s2 in
          let t1 = List.stable_sort rcmp t1 in
          let t2 = List.stable_sort rcmp t2 in
          if (List.map fst t1) <> (List.map fst t2) then raise Not_found;
            List.iter2 (fun a b -> add_eqn (snd a,snd b)) t1 t2;
            s := None
        end
        else raise Not_found

      | BTYP_label , BTYP_label -> ()
      | BTYP_type i,BTYP_type j when i = j -> ()
      | BTYP_void,BTYP_void -> ()

      | BTYP_inst (i1,ts1),BTYP_inst (i2,ts2) ->
(*
print_endline "Trying to unify instances (1)";
*)
        if i1 <> i2 then raise Not_found
        else if List.length ts1 <> List.length ts2 then raise Not_found
        else
        begin
(*
print_endline "Trying to unify instances (2)";
*)
            List.iter2 (fun a b -> add_eqn (a,b)) ts1 ts2;
            s := None
        end

(*
      | BTYP_fix (0,_),_
      | _,BTYP_fix (0,_) -> ()
*)
      | BTYP_fix (i,t1),BTYP_fix (j,t2) ->
        if i <> j then raise Not_found;
        if t1 <> t2 then print_endline "unification: fix points at same level with unequal metatypes!";
        add_eqn (t1,t2)

      | BTYP_tuple ls, BTYP_array (ta,BTYP_unitsum n)
      | BTYP_array (ta,BTYP_unitsum n), BTYP_tuple ls
        when n = List.length ls ->
        List.iter (fun t -> add_eqn (t,ta)) ls

      | BTYP_tuple_cons (t0,ts), BTYP_tuple_cons (t0',ts') ->
        add_eqn (t0,t0'); add_eqn (ts,ts')

      | BTYP_tuple (t0::ts1::ts2::ts), BTYP_tuple_cons (t0',ts')
      | BTYP_tuple_cons (t0',ts'), BTYP_tuple (t0::ts1::ts2::ts) ->
        add_eqn (t0,t0'); add_eqn (BTYP_tuple (ts1::ts2::ts), ts')

(*
      (* T ^ N = T by setting N = 1 *)
      | BTYP_array (t11, (BTYP_type_var (i,_) as tv)), t21 
      | t21, BTYP_array (t11, (BTYP_type_var (i,_) as tv))
        when BidSet.mem i dvars 
       ->
let lhs,rhs = h in
print_endline ("Weird array thing " ^ Flx_print.sbt bsym_table lhs ^ " <--> " ^ Flx_print.sbt bsym_table rhs);
        eqns := (t11,t21) :: (tv, btyp_tuple []) :: !eqns
*)

      (* type tuple is handled same as a tuple type .. not
        really sure this is right. Certainly, the corresponding
        terms in both must unify, however possibly we should
        return distinct MGU for each case for the type tuple,
        possibly with distinct bindings for the same variable..
      *)

      | (BTYP_type_tuple ls1, BTYP_type_tuple ls2)
      | (BTYP_tuple ls1, BTYP_tuple ls2)
      | (BTYP_sum ls1, BTYP_sum ls2)
        when List.length ls1 = List.length ls2 ->
        begin
            List.iter2 (fun a b -> add_eqn (a,b)) ls1 ls2;
            s := None
        end

      (* structural, not functional, equality of lambdas by alpha equivalence *)
      | BTYP_type_function (p1,r1,b1), BTYP_type_function (p2,r2,b2)
        when List.length p1 = List.length p2 ->
(*
print_endline "Trying to unify type functions";
print_endline (sbt bsym_table lhs);
print_endline (sbt bsym_table rhs);
*)
        (* This is overly ambitious! Maybe should just do a plain type equality test *)
        let meta_type_equations = List.map2 (fun (_,t1) (_,t2) -> (t1,t2)) p1 p2 in
        let meta_type_equations = (r1,r2) :: meta_type_equations in

        let vs = List.map2 (fun (i1,_) (i2,t) -> i1,btyp_type_var (i2,t))  p1 p2 in
(*
print_endline ("vs=" ^ catmap "," (fun (i,t) -> string_of_int i^":"^sbt bsym_table t) vs);
*)
        let b1 = list_subst counter vs b1 in
(*
print_endline ("Converted LHS body=" ^ sbt bsym_table b1);
*)
        add_eqn (b1, b2);
        List.iter add_eqn meta_type_equations;
        s := None

      | BTYP_type_apply (f1,a1), BTYP_type_apply (f2,a2)  ->
print_endline "Trying to unify type application";
        add_eqn (f1,f2); add_eqn (a1,a2)

      | x,y ->
(*
        print_endline ("Terms do not match: " ^ sbt bsym_table x ^ " <-> " ^ sbt bsym_table y);
*)
        raise Not_found
      end
      ;
      begin match !s with
      | None -> ()
      | Some (i,t) ->
        (*
        print_endline ("Substituting " ^ si i ^ " -> " ^ sbt sym_table t);
        *)
        eqns :=
          List.map
          (fun (a,b) ->
            term_subst counter a i t,
            term_subst counter b i t
          )
          !eqns
        ;
        assert(not (List.mem_assoc i !mgu));
        mgu :=
          (i,t) ::
          (List.map
            (fun (j,t') -> j,term_subst counter t' i t)
            !mgu
          )
      end
      ;
      loop ()
    in
      loop ();
      !mgu

let find_vars_eqns eqns =
  let lhs_vars = ref BidSet.empty in
  let rhs_vars = ref BidSet.empty in
  List.iter (fun (l,r) ->
    lhs_vars := BidSet.union !lhs_vars (vars_in l);
    rhs_vars := BidSet.union !rhs_vars (vars_in r)
  )
  eqns
  ;
  !lhs_vars,!rhs_vars

let maybe_unification bsym_table counter eqns =
  let l,r = find_vars_eqns eqns in
  let dvars = BidSet.union l r in
  try Some (unification bsym_table counter eqns dvars)
  with Not_found -> None

let maybe_matches bsym_table counter eqns =
  let l,r = find_vars_eqns eqns in
  let dvars = BidSet.union l r in
  try Some (unification bsym_table counter eqns dvars)
  with Not_found -> None

let maybe_specialisation bsym_table counter eqns =
  let l,_ = find_vars_eqns eqns in
  try Some (unification bsym_table counter eqns l)
  with Not_found -> None

let unifies bsym_table counter t1 t2 =
  let eqns = [t1,t2] in
  match maybe_unification bsym_table counter eqns with
  | None -> false
  | Some _ -> true

let ge bsym_table counter a b =
(*
  print_endline ("Compare terms " ^ sbt bsym_table a ^ " >? " ^ sbt bsym_table b);
*)
  match maybe_specialisation bsym_table counter [a,b] with
  | None -> (* print_endline "    ** false"; *) false
  | Some mgu ->
(*
    print_endline ("MGU from specialisation = ");
    List.iter (fun (i, t) -> print_endline (si i ^ " --> " ^ sbt bsym_table t)) mgu;
    print_endline "";
*)
    true

let str_of_cmp = function
| `Equal -> " = "
| `Incomparable -> " <> "
| `Less-> " < "
| `Greater-> " > "

let compare_sigs bsym_table counter a b =
  let result = match ge bsym_table counter a b, ge bsym_table counter b a with
  | true, true -> `Equal
  | false, false -> `Incomparable
  | true, false -> `Greater
  | false, true -> `Less
  in
(*
  print_endline ("compare_sigs " ^ sbt bsym_table a ^ str_of_cmp result ^ sbt bsym_table b);
*)
  result

let rec memq trail (a,b) = match trail with
  | [] -> false
  | (i,j)::t -> i == a && j == b || memq t (a,b)

let rec type_eq' bsym_table counter ltrail ldepth rtrail rdepth trail t1 t2 =
  (* print_endline (sbt sym_table t1 ^ " =? " ^ sbt sym_table t2); *)
  if memq trail (t1,t2) then true
  else let te a b = type_eq' bsym_table counter
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
  | BTYP_tuple_cons (s1,d1),BTYP_tuple_cons (s2,d2)
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
      type_eq' bsym_table counter ltrail ldepth rtrail rdepth trail a b
      with Not_found -> false
    end

  | BTYP_fix (i,mt1),t ->
    (*
    print_endline "LHS fixpoint";
    *)
    begin try
    let a = List.assoc (ldepth+i) ltrail in
    type_eq' bsym_table counter ltrail ldepth rtrail rdepth trail a t
    with Not_found -> false
    end

  | t,BTYP_fix (j,mt2) ->
    (*
    print_endline "RHS fixpoint";
    *)
    begin try
    let b = List.assoc (rdepth+j) rtrail in
    type_eq' bsym_table counter ltrail ldepth rtrail rdepth trail t b
    with Not_found -> false
    end

  | BTYP_type_function (p1,r1,b1), BTYP_type_function (p2,r2,b2) ->
    List.length p1 = List.length p2 &&
    let vs = List.map2 (fun (i1,_) (i2,t) -> i1,btyp_type_var (i2,t))  p1 p2 in
(*
    print_endline "Comparing type functions";
    print_endline ("b1 =          " ^ sbt bsym_table b1);
*)
    let b1 = list_subst counter vs b1 in
(*
    print_endline ("Adjusted b1 = " ^ sbt bsym_table b1);
    print_endline ("b2 =          " ^ sbt bsym_table b2);
*)
    let result = te b1 b2 in
(*
    print_endline ("Compared = " ^ (if result then "TRUE" else "FALSE"));
*)
    result

  | l,r ->
(*
    print_endline ("WOOOPS .. dunno.." ^ sbt bsym_table l ^" vs " ^ sbt bsym_table r);
*)
    false

let type_eq bsym_table counter t1 t2 = (* print_endline "TYPE EQ";  *)
  type_eq' bsym_table counter [] 0 [] 0 [] t1 t2

let type_match bsym_table counter t1 t2 = (* print_endline "TYPE MATCH"; *)
  type_eq' bsym_table counter [] 0 [] 0 [] t1 t2

exception Found of Flx_btype.t

let fold bsym_table counter t =
  let rec aux trail depth t' =
    let ax t = aux ((depth,t')::trail) (depth+1) t in
    match t' with
    | BTYP_intersect ls
    | BTYP_sum ls
    | BTYP_inst (_,ls)
    | BTYP_tuple ls -> List.iter ax ls
    | BTYP_record (ls) -> List.iter (fun (s,t) -> ax t) ls
    | BTYP_polyrecord (ls,v) -> List.iter (fun (s,t) -> ax t) ls; ax v
    | BTYP_variant ls -> List.iter (fun (s,t) -> ax t) ls

    | BTYP_array (a,b)
    | BTYP_function (a,b) -> ax a; ax b
    | BTYP_effector (a,e, b) -> ax a; ax e; ax b
    | BTYP_cfunction (a,b) -> ax a; ax b

    | BTYP_pointer a -> ax a
    | BTYP_tuple_cons (a,b) -> ax a; ax b

    | BTYP_hole
    | BTYP_label 
    | BTYP_none
    | BTYP_int
    | BTYP_void
    | BTYP_unitsum _
    | BTYP_type_var _
    | BTYP_fix (0,_) -> ()

    | BTYP_fix (i,_) ->
      let k = depth + i in
      begin try
        let t'' = List.assoc k trail in
        if type_eq bsym_table counter t'' t then raise (Found t'')
      with Not_found -> ()
      end

    | BTYP_type_apply (a,b) -> ax a; ax b

    | BTYP_type_set_intersection _
    | BTYP_type_set_union _
    | BTYP_type_set _
    | BTYP_type_function _
    | BTYP_type _
    | BTYP_type_tuple _
    | BTYP_type_match _ -> () (* assume fixpoint can't span these boundaries *)
      (* failwith ("[fold] unexpected metatype " ^ sbt sym_table t') *)
  in
    try aux [] 0 t; t
    with Found t -> t

exception Discard of int * int 

let wrap bsym_table counter t =
  let rec aux trail depth t' =
    let rec scan ctor left right =
      match right with
      | pivot::tail ->
        let trail = (depth,ctor (left @ [btyp_hole] @ tail))::trail in
        let r = aux trail (depth+1) pivot in
        scan ctor (left @ [r]) tail
      | _ -> ctor left
    in 

    let pair ctor a b =
      let trail = (depth, (ctor (btyp_hole,b))):: trail in
      let a' = aux trail (depth+1) a in
      let trail = (depth, (ctor (a,btyp_hole))):: trail in
      let b' = aux trail (depth+1) b in
      ctor (a,b)
    in
    try
      match t' with
      | BTYP_intersect ls -> scan btyp_intersect [] ls
      | BTYP_sum ls -> scan btyp_sum [] ls
      | BTYP_inst (k,ls) ->
        let ctor ls = btyp_inst (k,ls) in
        scan ctor [] ls

      | BTYP_tuple ls -> scan btyp_tuple [] ls
      | BTYP_record (ls) ->
        let rec scan left right =
          match right with
          | (label,pivot)::tail ->
            let trail = (depth,btyp_record (left @ [label,btyp_hole] @ tail))::trail in
            let r = aux trail (depth+1) pivot in
            scan (left @ [label,r]) tail
          | _ -> btyp_record left
        in 
        scan [] ls
 
      | BTYP_polyrecord (ls,v) ->
        let rec scan left right =
          match right with
          | (label,pivot)::tail ->
            let trail = (depth,btyp_polyrecord (left @ [label,btyp_hole] @ tail) v)::trail in
            let r = aux trail (depth+1) pivot in
            scan (left @ [label,r]) tail
          | _ -> left 
        in 
        let ls = scan [] ls in
        let trail = (depth, btyp_polyrecord ls btyp_hole):: trail in
        let v = aux trail (depth+1) v in
        btyp_polyrecord ls v
 
      | BTYP_variant ls ->
        let rec scan left right =
          match right with
          | (label,pivot)::tail ->
            let trail = (depth,btyp_variant(left @ [label,btyp_hole] @ tail))::trail in
            let r = aux trail (depth+1) pivot in
            scan (left @ [label,r]) tail
          | _ -> btyp_variant left
        in 
        scan [] ls

      | BTYP_effector _ -> assert false
 
      | BTYP_array (a,b) -> pair btyp_array a b
      | BTYP_function (a,b) -> pair btyp_function a b
      | BTYP_cfunction (a,b) -> pair btyp_cfunction a b
      | BTYP_pointer a ->
        let trail = (depth, (btyp_pointer btyp_hole)):: trail in
        let a' = aux trail (depth+1) a in
        btyp_pointer a'

      | BTYP_tuple_cons (a,b) -> 
        let ctor (a,b) = btyp_tuple_cons a b 
        in pair ctor a b 

      | BTYP_label 
      | BTYP_none
      | BTYP_int
      | BTYP_void
      | BTYP_unitsum _
      | BTYP_type_var _
      | BTYP_fix (0,_) -> t' 
      | BTYP_fix (i,_) when depth + i = 0 -> t' (* original term is recursive *)

      | BTYP_fix (i,_) ->
       let rec slide j = 
          (* off the top! *)
          if depth - j + i < 0 then raise (Discard (j-1,i)) else

          (* grab the unzipped term above the binderl *)
          let binder_zipper_m1 = List.assoc (depth - j + i) trail in 

          (* grab the unzipped term above fixpoint *)
          let fix_zipper_m1 = List.assoc (depth - j) trail in

          (* check if the two terms are equal with holes *)
          if type_eq bsym_table counter binder_zipper_m1 fix_zipper_m1 
          then begin
            slide (j+1) (* if equal, slide up again *)
          end else begin
            raise (Discard (j-1,i))
          end
        in 
        slide 1


      | BTYP_type_apply (a,b) -> pair btyp_type_apply a b
   
      | BTYP_type_set_intersection _
      | BTYP_type_set_union _
      | BTYP_type_set _
      | BTYP_type_function _
      | BTYP_type _
      | BTYP_type_tuple _
      | BTYP_type_match _ ->  t' (* a bit hacky *)

      | BTYP_hole -> assert false
    with Discard (n,i) -> if n=0 then btyp_fix i (btyp_type 0) else raise (Discard (n-1,i))

    in
    aux [] 0 t


(* produces a unique minimal representation of a type
by folding at every node *)

let minimise bsym_table counter t =
  fold bsym_table counter (Flx_btype.map ~f_btype:(fold bsym_table counter) t)

(*
let minimise bsym_table counter t = 
  let t' =  wrap bsym_table counter t in
  if t <> t' then 
    print_endline ("Minimised " ^ sbt bsym_table t ^ "\n  to " ^ sbt bsym_table t')
  ;
  t'
*)

let var_occurs bsym_table t =
  let rec aux' excl t = let aux t = aux' excl t in
    match t with
    | BTYP_intersect ls
    | BTYP_type_set ls
    | BTYP_type_set_intersection ls
    | BTYP_type_set_union ls
    | BTYP_sum ls
    | BTYP_inst (_,ls)
    | BTYP_tuple ls -> List.iter aux ls
    | BTYP_record (ls) -> List.iter (fun (s,t) -> aux t) ls
    | BTYP_polyrecord (ls,v) -> List.iter (fun (s,t) -> aux t) ls; aux v
    | BTYP_variant ls -> List.iter (fun (s,t) -> aux t) ls

    | BTYP_array (a,b)
    | BTYP_function (a,b) -> aux a; aux b
    | BTYP_effector (a,e,b) -> aux a; aux e; aux b
    | BTYP_cfunction (a,b) -> aux a; aux b

    | BTYP_pointer a  -> aux a

    | BTYP_label
    | BTYP_unitsum _
    | BTYP_void
    | BTYP_fix _ -> ()

    | BTYP_type_var (k,_) -> if not (List.mem k excl) then raise Not_found
    | BTYP_type_function (p,r,b) ->
      aux' (List.map fst p @ excl) b
    
    | BTYP_type_apply (a,b) -> aux a; aux b
    | BTYP_tuple_cons (a,b) -> aux a; aux b
    | _ -> 
      print_endline ("[var_occurs] unexpected metatype " ^ sbt bsym_table t);
      failwith ("[var_occurs] unexpected metatype " ^ sbt bsym_table t)

 in try aux' [] t; false with Not_found -> true

let ident x = x

(* not really right! Need to map the types as well,
  since we're instantiating a polymorphic term with
  a more specialised one

  Also won't substitute into LHS of things like direct_apply.
*)
let expr_term_subst e1 i e2 =
  let rec f_bexpr e =
    match Flx_bexpr.map ~f_bexpr e with
    | BEXPR_varname (j,_),_ when i = j -> e2
    | e -> e
  in
  f_bexpr e1

let rec expr_unification bsym_table counter
  eqns
  tdvars
  edvars
=
  (*
  print_endline ( "Tdvars = { " ^ catmap ", " si (BidSet.elements tdvars) ^ "}");
  print_endline ( "Edvars = { " ^ catmap ", " si (BidSet.elements edvars) ^ "}");
  *)
  let teqns = ref [] in
  let eqns = ref eqns in
  let mgu = ref [] in
  let rec loop () : unit =
    match !eqns with
    | [] -> ()
    | h :: t ->
      eqns := t;
      let s = ref None in
      let (lhse,lhst),(rhse,rhst) = h in
      teqns := (lhst,rhst) :: !teqns;

      (* WE COULD UNIFY TYPES HERE -- but there is no need!
         if the terms unify, the types MUST
         We DO need to unify the types -- but only after
         we've found matching terms.

         Note: the types in the ts lists DO have to be
         unified! It's only the types OF terms that
         don't require processing .. since they're just
         convenience caches of the term type, which can
         be computed directly from the term.
      *)
      begin match (lhse,rhse) with
      | (BEXPR_varname (i,[]) as ei), (BEXPR_varname (j,[]) as ej) ->
        (*
        print_endline ("Equated variables " ^ si i ^ " <-> " ^ si j);
        *)

        if i <> j then
          if BidSet.mem i edvars then
            s := Some (i,(ej,rhst))
          else if BidSet.mem j edvars then
            s := Some (j,(ei,lhst))
          else raise Not_found

      | BEXPR_varname (i,_),x ->
        if not (BidSet.mem i edvars) then raise Not_found;
        s := Some (i,(x,rhst))

      | x, BEXPR_varname (i,_) ->
        if not (BidSet.mem i edvars) then raise Not_found;
        s := Some (i,(x,lhst))

      | BEXPR_apply (f1,e1), BEXPR_apply (f2,e2) ->
        (*
        print_endline "matched applications";
        *)
        eqns := (f1,f2) :: (e1,e2) :: !eqns

      | BEXPR_closure (i,ts1), BEXPR_closure (j,ts2) when i = j -> ()

(*
      | BEXPR_apply_prim _, _
      | BEXPR_apply_direct _, _
      | BEXPR_apply_stack _, _
      | _, BEXPR_apply_prim _
      | _, BEXPR_apply_direct _
      | _, BEXPR_apply_stack _
         -> assert false
*)
      | BEXPR_apply_prim (i,ts1,e1),BEXPR_apply_prim(j,ts2,e2)
      | BEXPR_apply_direct (i,ts1,e1),BEXPR_apply_direct(j,ts2,e2)
      | BEXPR_apply_stack(i,ts1,e1),BEXPR_apply_stack(j,ts2,e2)
        when i = j
        ->
        assert (List.length ts1 = List.length ts2);
        teqns := List.combine ts1 ts2 @ !teqns;
        eqns := (e1,e2) :: !eqns

      | BEXPR_coerce (e,t), BEXPR_coerce (e',t') ->
        teqns := (t,t') :: !teqns;
        eqns := (e,e') :: !eqns

      | BEXPR_aprj (ix,d,c), BEXPR_aprj (ix',d',c') ->
        teqns := (d,d') :: (c,c') :: !teqns;
        eqns := (ix,ix') :: !eqns

      | BEXPR_prj (n,d,c), BEXPR_prj (n',d',c')
      | BEXPR_inj (n,d,c), BEXPR_inj (n',d',c') ->
        teqns := (d,d') :: (c,c') :: !teqns;
        if n <> n' then raise Not_found
 
      | BEXPR_deref e1, BEXPR_deref e2  ->
        eqns := (e1,e2) :: !eqns

      (* CHEAT HERE .. ignore ts .. fix later *)
      | BEXPR_ref (i1,ts1), BEXPR_ref (i2,ts2) when i1 = i2 -> ()

      | (BEXPR_tuple ls1, BEXPR_tuple ls2)
        when List.length ls1 = List.length ls2 ->
        begin
            List.iter2 (fun a b -> eqns := (a,b) :: !eqns) ls1 ls2;
            s := None
        end

      | x,y ->
        (* the BTYP_void is a hack .. *)
        (*
        print_endline ("Terms do not match: " ^ sbe sym_table (x,BTYP_void) ^ " <-> " ^ sbe sym_table (y,BTYP_void));
        *)
        raise Not_found
      end
      ;
      begin match !s with
      | None -> ()
      | Some (i,t) ->
        (*
        print_endline ("Substituting " ^ si i ^ " -> " ^ sbt sym_table t);
        *)
        eqns :=
          List.map
          (fun (a,b) ->
            expr_term_subst a i t,
            expr_term_subst b i t
          )
          !eqns
        ;
        assert(not (List.mem_assoc i !mgu));
        mgu :=
          (i,t) ::
          (List.map
            (fun (j,t') -> j,expr_term_subst t' i t)
            !mgu
          )
      end
      ;
      loop ()
    in
      loop ();
      let tmgu = unification bsym_table counter !teqns tdvars in
      tmgu,
      !mgu

let setoflist ls = List.fold_left (fun s i -> BidSet.add i s) BidSet.empty ls

let expr_maybe_matches bsym_table counter tvars evars le re =
  let tvars = setoflist tvars in
  let evars = setoflist evars in
  let eqns = [le,re] in
  (*
  print_endline ("Expr unify: le = " ^ sbe sym_table le ^  "\nre = " ^ sbe sym_table re);
  *)
  try Some (expr_unification bsym_table counter eqns tvars evars)
  with Not_found -> None



