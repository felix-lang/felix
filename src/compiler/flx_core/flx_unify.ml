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
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let unit_t = btyp_tuple []

let check_recursion bsym_table t =
  try Flx_btype_rec.check_rec t
  with 
  | Bad_recursion ->
    print_endline ("Flx-unify: check_recursion: Bad_recursion " ^ 
      str_of_btype t ^ " = " ^ 
      sbt bsym_table t);
    raise Bad_recursion 

type relmode_t = [`Eq | `Ge]
type tpair_t = Flx_btype.t * Flx_btype.t
type rel_t = relmode_t * tpair_t
type rels_t = rel_t list
type vassign_t = int * Flx_btype.t
type mgu_t = vassign_t list
type maybe_vassign_t = vassign_t option
type reladd_t = tpair_t -> unit
type dvars_t = BidSet.t

(* LHS ge RHS, parameter supertype of argument *)
let rec solve_subtypes bsym_table counter lhs rhs dvars (s:vassign_t option ref) (add_eq:reladd_t) (add_ge:reladd_t) =
  match lhs, rhs with
  | BTYP_inst (l,[],_),BTYP_inst(r,[],_) ->
    (* meta types have to agree if types do? *)
    if l <> r && not (Flx_bsym_table.is_supertype bsym_table l r)
    then raise Not_found

  (* arrays and tuples, must be the same length, covariant by element *)
  | BTYP_tuple ls, BTYP_tuple rs ->
    if List.length ls <> List.length rs then raise Not_found;
    List.iter2 (fun l r -> add_ge(l,r)) ls rs

  | BTYP_tuple ls, BTYP_array (r,BTYP_unitsum n) ->
    if List.length ls <> n then raise Not_found;
    List.iter (fun l -> add_ge(l,r)) ls
    
  | BTYP_array (l, BTYP_unitsum n), BTYP_tuple rs ->
    if List.length rs <> n then raise Not_found;
    List.iter (fun r -> add_ge(l,r)) rs

  | BTYP_array (l, BTYP_unitsum n), BTYP_array (r, BTYP_unitsum m) ->
    if m <> n then raise Not_found;
    add_ge (l,r)

  (* pointer is a subtype of read only pointer and write only pointer.
     no variance at the moment
  *)
  | BTYP_rref l, BTYP_pointer r ->
    add_eq (l,r)

  | BTYP_wref l, BTYP_pointer r ->
    add_eq (l,r)

  | BTYP_function (dl,cl), BTYP_function (dr,cr) ->
    add_ge (dr, dl); (* contravariant *)
    add_ge (cl, cr) (* covariant *)

  (* This rule isn't variant but should be!
     The rule basically says, the parameter type is a super type
     of the argument type if the argument type is one of the types
     in the union. This will only work if the union is monomorphic.

     The correct rule is the same as overloading and is very complex.
     The rule should be to first calculate ALL of the union components
     that the argument type is a subtype of, keeping track of the 
     resultant MGUs. Then, of all the candidates we have to find
     the most specialised one, so we have to recursively apply
     the unification engine and matching stuff as in overloading.

     An ambiguous result is a problem. It means, we have to unify,
     but the problem now is that we have to inject the MGU into
     the solutions obtained so far, but now we have a choice
     of MGUs.
  *)
  | BTYP_union lhs, t ->
    if List.mem t lhs then () else raise Not_found

  | BTYP_record lhs, BTYP_record rhs ->
    let counts = Hashtbl.create 97 in
    let get_rel_seq name = 
      let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
      Hashtbl.replace counts name n;
      n
    in
    List.iter (fun (name,ltyp) ->
      let rel_seq = get_rel_seq name in
(*
print_endline ("Unify: Param Field " ^ name ^ ", rel_seq=" ^ string_of_int rel_seq ^ ",type=" ^ str_of_btype ltyp);
*)
      let maybe = find_seq name rel_seq rhs in
      match maybe with
      | None -> 
(*
print_endline ("Unify: not found in Argument!");
*)
        raise Not_found 
      | Some (_,rtyp) -> 
(*
print_endline ("Found in argument, type=" ^ str_of_btype rtyp);
*)
        add_ge (ltyp, rtyp) (* covariant *)
    )
    lhs

  (* width subtyping is reversed from records but arguments still have to be covariant *)
  | BTYP_variant lhs, BTYP_variant rhs ->
    let counts = Hashtbl.create 97 in
    let get_rel_seq name = 
      let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
      Hashtbl.replace counts name n;
      n
    in
    List.iter (fun (name,rtyp) ->
      let rel_seq = get_rel_seq name in
(*
print_endline ("Unify: Arg Ctor " ^ name ^ ", rel_seq=" ^ string_of_int rel_seq ^ ",type=" ^ str_of_btype rtyp);
*)
      let maybe = find_seq name rel_seq lhs in
      match maybe with
      | None -> 
(*
print_endline ("Unify: not found in Param!");
*)
        raise Not_found 
      | Some (_,ltyp) -> 
(*
print_endline ("Found in param, type=" ^ str_of_btype ltyp);
*)
        add_ge (ltyp, rtyp) (* covariant *)
    )
    rhs


  | _ ->  
    solve_subsumption bsym_table counter lhs rhs dvars s add_eq

and solve_subsumption bsym_table counter lhs rhs  dvars (s:vassign_t option ref) (add_eqn:reladd_t) =
      begin match lhs,rhs with
      | BTYP_rev t1, BTYP_rev t2 ->
        add_eqn (t1,t2)


      | (BTYP_type_var (i,mi) as ti), (BTYP_type_var (j,mj) as tj)->
        (*
        print_endline ("Equated variables " ^ si i ^ " <-> " ^ si j);
        *)

        (* meta type have to agree *)
        if mi <> mj then begin
          print_endline ("Unify: metatype mismatch T<"^string_of_int i^">" ^Flx_kind.sk mi ^ 
          " != T<"^string_of_int j^">" ^ Flx_kind.sk mj);
(* META TYPE ARE BUGGED *)
(*
          raise Not_found;
*)
        end;

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
          s := Some (i, Flx_btype_rec.fix i t)
        end else begin
          (*
          print_endline "Adding substitution";
          *)
          s := Some (i,t)
        end

      (* Note: the t here cannot be a BTYP_rev term, nor a type variable!
       * the first case is rev t = rev u previously eliminated,
       * the second case rev t = u also previously eliminated
       * therefore this operation cannot cause an infinite loop
       * note the laws rev(rev x) = x and rev x = y implies x = rev y
       *)
      | BTYP_rev (BTYP_type_var (i,m) as tvar),t 
      | t,BTYP_rev (BTYP_type_var (i,m) as tvar) ->
        add_eqn (tvar,btyp_rev t)
 

      | BTYP_intersect ts,t
      | t,BTYP_intersect ts ->
        List.iter (function t' -> add_eqn (t,t')) ts

      | BTYP_union ts,t
      | t,BTYP_union ts ->
        print_endline ("Unify union type not implemented");
        assert false

      | BTYP_pointer t1, BTYP_pointer t2 ->
        add_eqn (t1,t2)

      | BTYP_wref t1, BTYP_wref t2
      | BTYP_rref t1, BTYP_rref t2 ->
        add_eqn (t1,t2)

      (* Compact linear type pointers, invariant on both params
         for now
      *)
      | BTYP_cltpointer (d1,c1), BTYP_cltpointer (d2,c2)
      | BTYP_cltrref(d1,c1), BTYP_cltrref (d2,c2)
      | BTYP_cltwref(d1,c1), BTYP_cltwref(d2,c2) ->
        add_eqn (d1,d2);
        add_eqn (c1,c2);

      | BTYP_uniq t1, BTYP_uniq t2 ->
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
      | BTYP_void,BTYP_void -> ()

      | BTYP_vinst (i1,ts1,mt1),BTYP_vinst (i2,ts2,mt2) 
      | BTYP_inst (i1,ts1,mt1),BTYP_inst (i2,ts2,mt2) ->
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
        (* kind equality *)
        if not (Flx_kind.kind_eq t1 t2) then raise Not_found

      | BTYP_tuple ls, BTYP_array (ta,BTYP_unitsum n)
      | BTYP_array (ta,BTYP_unitsum n), BTYP_tuple ls
        when n = List.length ls ->
        List.iter (fun t -> add_eqn (t,ta)) ls

      | BTYP_tuple_cons (t0,ts), BTYP_tuple_cons (t0',ts') ->
        add_eqn (t0,t0'); add_eqn (ts,ts')
      | BTYP_tuple_snoc (ts,t0), BTYP_tuple_snoc (ts',t0') ->
        add_eqn (t0,t0'); add_eqn (ts,ts')

      | BTYP_tuple (t0::ts1::ts2::ts), BTYP_tuple_cons (t0',ts')
      | BTYP_tuple_cons (t0',ts'), BTYP_tuple (t0::ts1::ts2::ts) ->
        add_eqn (t0,t0'); add_eqn (btyp_tuple (ts1::ts2::ts), ts')

      | BTYP_tuple (ts), BTYP_tuple_snoc (ts',t0')
      | BTYP_tuple_snoc (ts',t0'), BTYP_tuple (ts) ->
        begin match List.rev ts with
        | t0::ts1::ts2::rts ->
          add_eqn (t0,t0'); 
          let ts = List.rev (ts1::ts2::rts) in 
          add_eqn (btyp_tuple (ts), ts')
        | _ -> ()
        end
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

      (* repeated sums *)
      | BTYP_rptsum (n1,t1), BTYP_rptsum (n2,t2) ->
        add_eqn (n1,n2);
        add_eqn(t1,t2)

      (* linearly repeated sum and sum *)
      | BTYP_sum (ts), BTYP_rptsum (BTYP_unitsum n,t) 
      | BTYP_rptsum (BTYP_unitsum n,t), BTYP_sum (ts) 
        when List.length ts = n ->
        List.iter  (fun k -> add_eqn (k,t)) ts

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
(* META TYPES ARE BUGGED: IGNORE *)
(*
        List.iter add_eqn meta_type_equations;
*)
        s := None

      | BTYP_type_apply (f1,a1), BTYP_type_apply (f2,a2)  ->
(*
print_endline ("Trying to unify type application " ^ Flx_btype.st lhs ^ " and " ^ Flx_btype.st rhs);
*)
        add_eqn (f1,f2); add_eqn (a1,a2)

      | BTYP_type_map (f1,a1), BTYP_type_map (f2,a2)  ->
print_endline "Trying to unify type map";
        add_eqn (f1,f2); add_eqn (a1,a2)


      | x,y ->
(*
        print_endline ("Terms do not match: " ^ sbt bsym_table x ^ " <-> " ^ sbt bsym_table y);
*)
        raise Not_found
      end

let unif bsym_table counter (inrels: rels_t) (dvars:dvars_t) =
  (*
  print_endline ( "Dvars = { " ^ catmap ", " si (BidSet.elements dvars) ^ "}");
  *)
  let history : rels_t ref = ref inrels in
  let rels : rels_t ref = ref inrels in
  let mgu : mgu_t ref = ref [] in
  let add_rel (rel:rel_t) =
    if List.mem rel (!history) then ()
    else begin
       rels := rel :: (!rels);
       history := rel :: (!history)
    end
  in
  let add_eq x = add_rel (`Eq, x) in
  let add_ge x = add_rel (`Ge, x) in
  let rec loop () : unit =
    match !rels with
    | [] -> ()
    | h :: t ->
      rels := t;
      let s: vassign_t option ref = ref None in
      let (mode,(lhs,rhs)): rel_t = h in 
      let lhs = unfold "unification" lhs in
      let rhs = unfold "unification" rhs in
      begin match mode with
      | `Eq -> solve_subsumption bsym_table counter lhs rhs dvars s add_eq 
      | `Ge -> solve_subtypes bsym_table counter lhs rhs dvars s add_eq add_ge
      end
      ;
      begin match !s with
      | None -> ()
      | Some (i,t) ->
        (*
        print_endline ("Substituting " ^ si i ^ " -> " ^ sbt sym_table t);
        *)
        rels :=
          List.map
          (fun (mode,(a,b)) ->
            mode,
            (term_subst counter a i t,
            term_subst counter b i t)
          )
          !rels
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

let unification bsym_table counter eqns dvars =
  let eqns = List.map (fun x -> `Eq, x) eqns in
  unif bsym_table counter eqns dvars

let maybe_unification bsym_table counter eqns =
  let l,r = find_vars_eqns eqns in
  let dvars = BidSet.union l r in
  let eqns = List.map (fun x -> `Eq, x) eqns in
  try Some (unif bsym_table counter eqns dvars)
  with Not_found -> None

(* same as unifies so why is this here? *)
let maybe_matches bsym_table counter eqns =
  let l,r = find_vars_eqns eqns in
  let dvars = BidSet.union l r in
  let eqns = List.map (fun x -> `Eq, x) eqns in
  try Some (unif bsym_table counter eqns dvars)
  with Not_found -> None

(* LHS is parameter, RHS is argument, we require LHS >= RHS *)
let maybe_specialisation_with_dvars bsym_table counter eqns dvars =
  let eqns = List.map (fun x -> `Ge, x) eqns in
  try Some (unif bsym_table counter eqns dvars)
  with Not_found -> None

let maybe_specialisation bsym_table counter eqns =
  let l,_ = find_vars_eqns eqns in
  maybe_specialisation_with_dvars bsym_table counter eqns l

let unifies bsym_table counter t1 t2 =
  let eqns = [t1,t2] in
  match maybe_unification bsym_table counter eqns with
  | None -> false
  | Some _ -> true

let ge bsym_table counter a b =
(*
  print_endline ("Compare terms " ^ sbt bsym_table a ^ " >? " ^ sbt bsym_table b);
*)
  let eqns = [a,b] in
  let l,_ = find_vars_eqns eqns in
  match maybe_specialisation bsym_table counter eqns with
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

let type_eq bsym_table counter t1 t2 = 
  Flx_typeeq.type_eq (sbt bsym_table) counter t1 t2

let type_match bsym_table counter t1 t2 = 
  Flx_typeeq.type_match (sbt bsym_table) counter t1 t2

