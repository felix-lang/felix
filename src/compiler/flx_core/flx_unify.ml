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

let mode_supertype m1 m2 = match m1,m2 with
  | `R, `RW
  | `W, `RW
  | `N, _ -> ()
  | x,y when x = y -> ()
  | _ -> raise Not_found

let unit_t = btyp_tuple []

let check_recursion bsym_table t =
  try Flx_btype_rec.check_rec t
  with 
  | Bad_recursion ->
    print_endline ("Flx-unify: check_recursion: Bad_recursion " ^ 
      str_of_btype t ^ " = " ^ 
      sbt bsym_table t);
    raise Bad_recursion 

(* LHS ge RHS, parameter supertype of argument *)
let rec solve_subtypes bsym_table counter lhs rhs dvars (s:vassign_t option ref) (add_eq:reladd_t) (add_ge:reladd_t) =
(*
print_endline ("Solve subtypes " ^ Flx_btype.str_of_btype lhs ^ " >=? " ^ Flx_btype.str_of_btype rhs); 
*)
  match lhs, rhs with
(*
  | BTYP_inst (`Nominal _,l,[],_),BTYP_inst(`Nominal _,r,[],_) -> (* distinct monomorphic nominal types *)
    if l <> r && not (Flx_bsym_table.is_indirect_supertype bsym_table l r)
    then raise Not_found
*)
  | BTYP_inst (`Nominal variance,l,lts,_),BTYP_inst(`Nominal _,r,rts,_) when l = r -> (* same polymorphic nominal type *)
    assert (List.length lts = List.length rts);
    let t2 = List.combine lts rts in
    assert(List.length variance <= List.length lts);
    let variance = (Flx_list.repeat `invariant (List.length lts - List.length variance)) @ variance in
    List.iter2 (fun (l, r) variance -> 
      match variance with
      | `covariant -> add_ge (l, r)
      | `invariant ->  add_eq  (l, r)
      | `contravariant -> add_ge (r, l)
    ) t2 variance

  (* recall the supertype is the BASE TYPE and subtype is DERIVED we want
     a coercion from DERIVED TO BASE

     So coercion from RHS to LHS
     which means the argument is the RHS and the domain of the coercion.
  *)
  | BTYP_inst (`Nominal variance,l,lts,knd),BTYP_inst(`Nominal _,r,rts,_)  -> (* distinct polymorphic nominal type *)
    let chains = Flx_bsym_table.find_coercion_chains bsym_table l r in
    let n = List.length chains in
(*
    if n > 0 then
      print_endline ("Unify Found " ^ string_of_int n ^ " chains");
*)
    begin match chains with
    | [] -> raise Not_found (* not a subtype *)
    | chain :: _ ->
(*
      print_endline ("Unify using chain length " ^ string_of_int (List.length chain));
      print_endline ("Chain= " ^ Flx_util.catmap "," string_of_int chain);
*)
      let ts = List.fold_left 
        (fun ats f -> 
(*
print_endline ("Input argument ats = " ^ Flx_util.catmap "," Flx_btype.st ats);
*)
           let bsym = Flx_bsym_table.find bsym_table f in
           let dom,cod,bvs = match bsym.bbdcl with
             | BBDCL_external_fun (_,bvs,params,ret,_,_,_ ) -> btyp_tuple params, ret, bvs
             | BBDCL_fun (_,bvs,bparams,ret,_,_) -> Flx_bparams.get_btype bparams, ret, bvs
             | _ -> assert false
           in 
(*
print_endline ("Coercion " ^ Flx_btype.st (btyp_function (dom,cod)));
*)
           match dom,cod with
             (* Dom=Derived[vs]->Cod=Base[ts(vs)] *)
             | BTYP_inst (`Nominal _, d,dts,_), BTYP_inst (`Nominal _, c, cts,_) ->
               (* the pts MUST be the sequence of type variables in bvs, the subtype *)
(*
print_endline ("[derived] ats = " ^ Flx_util.catmap "," Flx_btype.st ats);
*)
               let vmap = Flx_btype_subst.mk_varmap bsym.sr bvs ats in
               let mapped_cts = List.map (Flx_btype_subst.varmap_subst vmap) cts in
(*
print_endline ("[base] cts = " ^ Flx_util.catmap "," Flx_btype.st mapped_cts);
*)
               mapped_cts
         
             | _ -> assert false
        ) 
        rts (List.rev chain)
      in
      let x = btyp_inst (`Nominal variance, l, ts, knd) in
(*
print_endline ("Adding inequality " ^ Flx_btype.st lhs ^ " > " ^ Flx_btype.st x);
*)
      add_ge (lhs, x)
    end    



  (* a non-uniq parameter accepts a uniq one, uniq T is a subtype of T,
     also, covariant ???????
  *)
  | BTYP_uniq t1, BTYP_uniq t2 ->
    add_ge (t1,t2)

  | BTYP_borrowed t1, BTYP_borrowed t2 ->
    add_ge (t1,t2)


  (* here we throw away uniq part of argument type passing a value *)
  | BTYP_borrowed t1, BTYP_uniq t2 -> add_ge (t1,t2)

  (* NOTE: should we check if the type var is dependent or not?? *)
  | BTYP_type_var _, BTYP_uniq _ -> add_eq (lhs,rhs) (* hack to assign type variable *)
  | t1, BTYP_uniq t2 -> add_ge (t1,t2)

  | BTYP_borrowed _, BTYP_type_var _  -> add_eq (lhs, rhs) (* hack to assign type variable *)
  | BTYP_borrowed t1, t2 -> add_ge (t1,t2)

  (* argument type t must be a subtype of each type of the intersection parameter *)
  | BTYP_intersect ts, t ->
    (* print_endline ("Flx_unify: Argument as subtype of intersection parameter, must be subtype of each intersectand"); *)
    List.iter (fun p -> add_ge (p, t)) ts

  (* this is not right, we can certainly judge this, but we cannot construct
     an MGU because it involves alternatives, unless there is exactly one match.
     This is known, principal typing is lost. 

     The correct judgement should be made in the routine "ge" which is not 
     looking for an MGU
  *)
  | t,BTYP_intersect ts -> 
    (* print_endline ("intersection as subtype of some type "^Flx_btype.st t^ " not implemented yet"); *)
    raise Not_found

  (* arrays and tuples, must be the same length, covariant by element *)
  | BTYP_tuple ls, BTYP_tuple rs ->
    (* special hack: parameter with trailing ellipsis matchs argument if the 
       components match, upto the ellipsis, anything after that matches anything
    *)
    begin match List.rev ls with
    | BTYP_ellipsis :: tail ->
      let prefix = List.rev tail in
      let n = List.length prefix in
      (* the number of components in the argument must be greater or equal to the parameter up to the ellipsis *)
      if List.length rs >= n then 
        let argprefix = Flx_list.list_prefix ls n in
        List.iter2 (fun l r -> add_ge(l,r)) prefix argprefix 
      else raise Not_found

    | _ -> 
      if List.length ls <> List.length rs then raise Not_found;
      List.iter2 (fun l r -> add_ge(l,r)) ls rs
    end

  (* arrays and tuples, must be the same length, covariant by element *)
  | BTYP_compacttuple ls, BTYP_compacttuple rs ->
    (* special hack: parameter with trailing ellipsis matchs argument if the 
       components match, upto the ellipsis, anything after that matches anything
    *)
      if List.length ls <> List.length rs then raise Not_found;
      List.iter2 (fun l r -> add_ge(l,r)) ls rs


  | BTYP_tuple ls, BTYP_array (r,BTYP_unitsum n) ->
    begin match List.rev ls with
    | BTYP_ellipsis :: tail ->
      let prefix = List.rev tail in
      let n = List.length prefix in
      let argprefix = Flx_list.list_prefix ls n in
      List.iter (fun l -> add_ge(l,r)) prefix

    | _ -> 
      if List.length ls <> n then raise Not_found;
      List.iter (fun l -> add_ge(l,r)) ls
    end

  | BTYP_compacttuple ls, BTYP_compactarray (r,BTYP_unitsum n) ->
    if List.length ls <> n then raise Not_found;
    List.iter (fun l -> add_ge(l,r)) ls
    
    
  | BTYP_array (l, BTYP_unitsum n), BTYP_tuple rs ->
    if List.length rs <> n then raise Not_found;
    List.iter (fun r -> add_ge(l,r)) rs
    
  | BTYP_compactarray (l, BTYP_unitsum n), BTYP_compacttuple rs ->
    if List.length rs <> n then raise Not_found;
    List.iter (fun r -> add_ge(l,r)) rs


  | BTYP_array (l, BTYP_unitsum n), BTYP_array (r, BTYP_unitsum m) ->
    if m <> n then raise Not_found;
    add_ge (l,r)

  | BTYP_compactarray (l, BTYP_unitsum n), BTYP_compactarray (r, BTYP_unitsum m) ->
    if m <> n then raise Not_found;
    add_ge (l,r)


  (* FIXME: these are the ordinary pointer and clt pointer cases
     we had before. There's an obvious generalisation to just require
     the projection traces to unify, but I think that's WRONG.

     In fact I think these two discrete cases are wrong too because
     a CLT pointer argument should be able to accept a non-clt pointer
     parameter.

     In fact, I don't believe it should be possible to specify
     a clt pointer as a parameter. No algorithm can actually use that
     fact. The parameter is just a plain pointer, for which there are
     only three operations anyhow: read, write, and project, if it
     points at a product.

     Only hassle is comparison of pointers. Probably should ban that!
  *)
  | BTYP_ptr (ml,l,[]), BTYP_ptr (mr,r,[]) ->
    mode_supertype ml mr;
    add_eq (l,r)

  | BTYP_ptr (ml,l,[machl]), BTYP_ptr (mr,r,[machr]) ->
    mode_supertype ml mr;
    add_eq (l,r);
    add_eq (machl, machr);


  | BTYP_function (dl,cl), BTYP_linearfunction (dr,cr)
  | BTYP_linearfunction (dl,cl), BTYP_linearfunction (dr,cr)
  | BTYP_function (dl,cl), BTYP_function (dr,cr) ->
    add_ge (dr, dl); (* contravariant *)
    add_ge (cl, cr) (* covariant *)

  | BTYP_record lhs, BTYP_record rhs ->
    let counts = Hashtbl.create 97 in
    let get_rel_seq name = 
      let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
      Hashtbl.replace counts name n;
      n
    in
    List.iter (fun (name,ltyp) ->
      let rel_seq = get_rel_seq name in
      let maybe = find_seq name rel_seq rhs in
      match maybe with
      | None -> 
        raise Not_found 
      | Some (_,rtyp) -> 
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
      let maybe = find_seq name rel_seq lhs in
      match maybe with
      | None -> 
        raise Not_found 
      | Some (_,ltyp) -> 
        add_ge (ltyp, rtyp) (* covariant *)
    )
    rhs


  | _ ->  
    solve_subsumption bsym_table counter lhs rhs dvars s add_eq

and solve_subsumption bsym_table counter lhs rhs  dvars (s:vassign_t option ref) (add_eqn:reladd_t) =
      begin match lhs,rhs with
      | BTYP_instancetype _, BTYP_instancetype _ -> () (* weirdo but we have to do it *)
      | BTYP_rev t1, BTYP_rev t2 ->
        add_eqn (t1,t2)


(* NOTE: This case allows SUBKINDING! This should NOT be allowed in
   the subsumption routine .. it should only be allowed in the
   subtyping routine .. but it isn't in there so we need to preserve
   this code for the moment

   In fact we need to extra the kind level MGU and return that
   too ......
*)
      | (BTYP_type_var (i,mi) as ti), (BTYP_type_var (j,mj) as tj)->
        (* meta type have to agree *)
        if i <> j then
          if BidSet.mem i dvars then
          begin
            if not (Flx_kind.kind_ge [mi, mj]) then
            begin
              raise Not_found;
            end;
            s := Some (i,tj)
          end 
          else if BidSet.mem j dvars then
          begin
            if not (Flx_kind.kind_ge [mj, mi]) then
            begin
              raise Not_found;
            end;
            s := Some (j,ti)
          end
          else raise Not_found (* distinct variables, neither is dependent so its a fail *)
        else () (* same variable .. we should check kinds agree .. *)

      (* TO DO: calculate the smallest metatype of the type and do a kinding check *)
      | BTYP_type_var (i,mt), t
      | t,BTYP_type_var (i,mt) ->
        if not (BidSet.mem i dvars) then raise Not_found;
        if var_i_occurs i t
        then begin
          print_endline
          (
            "recursion in unification, terms: " ^
            str_of_btype lhs ^ " = " ^ str_of_btype rhs
          );
          s := Some (i, Flx_btype_rec.fix i t)
        end else begin
          let mt2 = Flx_btype_kind.metatype Flx_srcref.dummy_sr t in
          if not (Flx_kind.kind_ge [mt, mt2]) then
          begin
            raise Not_found;
          end;
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
 
      | BTYP_uniq t1, BTYP_uniq t2 -> add_eqn (t1,t2)

      (* FIXME: this is the non-subtyping version of the
         code in the inequality unification, it needs
         to be generalised as for that case.
      *)
      | BTYP_ptr (ml,l,[]), BTYP_ptr (mr,r,[]) ->
        if ml =  mr then add_eqn (l,r) else raise Not_found

      | BTYP_ptr (ml,l,[machl]), BTYP_ptr (mr,r,[machr]) ->
        if ml =  mr then begin 
          add_eqn (l,r);
          add_eqn (machl, machr);
        end
        else raise Not_found

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

      | BTYP_compactarray (t11, t12), BTYP_compactarray (t21, t22)
      | BTYP_array (t11, t12), BTYP_array (t21, t22)
      | BTYP_function (t11, t12), BTYP_function (t21, t22)
      | BTYP_linearfunction (t11, t12), BTYP_linearfunction (t21, t22)
      | BTYP_cfunction (t11, t12), BTYP_cfunction (t21, t22) ->
        add_eqn (t11,t21); add_eqn (t12,t22)

      (* FIXME *)
      | BTYP_lineareffector (t11, t12, t13), BTYP_lineareffector (t21, t22, t23)
      | BTYP_effector (t11, t12, t13), BTYP_effector (t21, t22, t23) ->
        add_eqn (t11,t21); add_eqn (t12,t22); add_eqn (t13, t23)

      | BTYP_effector (t11, t12, t13), BTYP_function (t21, t23)
      | BTYP_function (t21, t23), BTYP_effector (t11, t12, t13) ->
        let t22 = btyp_tuple [] in
        add_eqn (t11,t21); add_eqn (t12,t22); add_eqn (t13, t23)



      | BTYP_record ([]),BTYP_tuple []
      | BTYP_tuple [],BTYP_record ([]) -> ()

      | BTYP_polyrecord (t1,_,v1),BTYP_polyrecord (t2,_,v2) ->
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
          add_eqn (btyp_polyrecord x "" v1, v2)
        | [],x -> 
(*
          print_endline "  *** more fields on right";
          print_endline ("  *** add eqn " ^ sbt bsym_table v1 ^ " = " ^ sbt bsym_table (btyp_polyrecord x v2));
*)
          add_eqn (v1, btyp_polyrecord x "" v2)
        | _ -> 
(*
          print_endline "  *** FAILED"; 
*)
          raise Not_found (* cant unify *)
        end
  

      | BTYP_polyrecord (t1,_,v),BTYP_record (t2)
      | BTYP_record (t2),BTYP_polyrecord (t1,_,v) -> 
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

      | BTYP_finst (i1,ks1,dom1,cod1), BTYP_finst (i2,ks2,dom2,cod2) ->
       (* FIXME: later we should unify to get kind variables .. and handle subtyping, for now
          equality will have to do
       *)
        if i1 <> i2 then raise Not_found 
        else if List.length ks1 <> List.length ks2 then raise Not_found
        else
        begin
         List.iter2 (fun k1 k2 -> if (Flx_kind.kind_eq k1 k2) then () else raise Not_found) ks1 ks2; 
         s := None
        end
 

      | BTYP_vinst (i1,ts1,mt1),BTYP_vinst (i2,ts2,mt2) 
      | BTYP_inst (_,i1,ts1,mt1),BTYP_inst (_,i2,ts2,mt2) ->
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

      | (BTYP_type_tuple ls1, BTYP_type_tuple ls2)
      | (BTYP_compacttuple ls1, BTYP_compacttuple ls2)
      | (BTYP_tuple ls1, BTYP_tuple ls2)
      | (BTYP_compactsum ls1, BTYP_compactsum ls2)
      | (BTYP_sum ls1, BTYP_sum ls2)
        when List.length ls1 = List.length ls2 ->
        begin
            List.iter2 (fun a b -> add_eqn (a,b)) ls1 ls2;
            s := None
        end

      (* repeated sums *)
      | BTYP_compactrptsum (n1,t1), BTYP_compactrptsum (n2,t2)
      | BTYP_rptsum (n1,t1), BTYP_rptsum (n2,t2) ->
        add_eqn (n1,n2);
        (* FIXME: in Felix, sum types don't support width subtyping,
           but they DO support depth subtyping, so, this should be an
           inequation not an equation. Which means this check has to
           be moved up to the subtyping part of the engine.
        *)
        add_eqn(t1,t2)

      (* linearly repeated sum and sum *)
      (* NOTE: this can happen because one of the sum components might be
        a type variable. In this case the rptsum will force the variable
        to be assigned to the repeated base type. I have to remember
        that canonical forms change when type variables are replaced!
      *)
      | BTYP_sum (ts), BTYP_rptsum (BTYP_unitsum n,t) 
      | BTYP_rptsum (BTYP_unitsum n,t), BTYP_sum (ts) 
        when List.length ts = n ->
        List.iter  (fun k -> add_eqn (k,t)) ts

      (* structural, not functional, equality of lambdas by alpha equivalence *)
      | BTYP_type_function (p1,r1,b1), BTYP_type_function (p2,r2,b2)
        when List.length p1 = List.length p2 ->

        (* This is overly ambitious! Maybe should just do a plain type equality test *)
        let meta_type_equations = List.map2 (fun (_,t1) (_,t2) -> (t1,t2)) p1 p2 in
        let meta_type_equations = (r1,r2) :: meta_type_equations in

        let vs = List.map2 (fun (i1,_) (i2,t) -> i1,btyp_type_var (i2,t))  p1 p2 in
        let b1 = list_subst counter vs b1 in
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

      | BTYP_typeop (lop,lt,lk), BTYP_typeop (rop, rt, rk) ->
        if lop <> rop then raise Not_found;
        if not (Flx_kind.kind_eq lk rk) then raise Not_found;
        add_eqn (lt,rt);

      | x,y ->
(*
        print_endline ("Terms do not match: " ^ sbt bsym_table x ^ " <-> " ^ sbt bsym_table y);
*)
        raise Not_found
      end

let unif bsym_table counter (inrels: rels_t) (dvars:dvars_t) =
(*
print_endline ("Unif:");
  print_endline ( "Dvars = { " ^ catmap ", " si (BidSet.elements dvars) ^ "}");
  print_endline ("Eqns = \n");
  List.iter  (fun (rel, (param, arg)) -> 
    print_endline (sbt bsym_table param ^ " " ^string_of_relmode_t rel  ^ " " ^ sbt bsym_table arg)
  ) inrels;
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
(*
print_endline ("Trying " ^ sbt bsym_table lhs ^ " " ^ string_of_relmode_t mode ^ " " ^ sbt bsym_table rhs);
*)
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
  with Not_found -> 
    None

(* DERIVED *)
let maybe_specialisation bsym_table counter eqns =
  let l,_ = find_vars_eqns eqns in
  maybe_specialisation_with_dvars bsym_table counter eqns l

let unifies bsym_table counter t1 t2 =
  let eqns = [t1,t2] in
  match maybe_unification bsym_table counter eqns with
  | None -> false
  | Some _ -> true

let ge' bsym_table counter a b : bool =
  let eqns = [a,b] in
  let l,_ = find_vars_eqns eqns in
  match maybe_specialisation bsym_table counter eqns with
  | None -> false
  | Some mgu -> true

(* This adds a judgment that unification can't do because of loss of principal typing:

  if A < X then A & B < X

  In other words, if A is a subtype of X, then adding an extra constraint doesn't hurt.

  For an intersection argument, we try this for each component, in other words
  if any one of the components is a subtype, the whole intersection is.
*)

let try_intersection bsym_table counter a b =
  match b with
  | BTYP_intersect bs ->
    List.fold_left (fun acc b -> acc || ge' bsym_table counter a b) false bs
  | _ -> false
 
let ge bsym_table counter a b : bool =
  let eqns = [a,b] in
  let l,_ = find_vars_eqns eqns in
  match maybe_specialisation bsym_table counter eqns with
  | None -> try_intersection bsym_table counter a b 
  | Some mgu ->
    true

let str_of_cmp = function
| `Equal -> " = "
| `Incomparable -> " <> "
| `Less-> " < "
| `Greater-> " > "

let compare_sigs bsym_table counter a b =
  let b = Flx_alpha.alpha_convert counter b in (* alpha convert one of the terms *)
  let ab = ge bsym_table counter a b in
  let ba =  ge bsym_table counter b a in
  let result = match ab,ba with
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

