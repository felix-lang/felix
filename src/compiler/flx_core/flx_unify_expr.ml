open Flx_btype
open Flx_bexpr
open Flx_bid

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
      let tmgu = Flx_unify.unification bsym_table counter !teqns tdvars in
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



