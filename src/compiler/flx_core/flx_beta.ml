(** Beta reduction *)

open Flx_util
open Flx_types
open Flx_btype
open Flx_mtypes2
open Flx_bid
open Flx_print
open Flx_typing
open Flx_unify
open Flx_maps
open Flx_btype_subst
open Flx_kind

(* This routine generates a typematch given an argument and a pattern list, any match
   returns TRUE otherwise it returns FALSE. The patterns must be fully reduced.
*) 

let generate_typematch_predicate bsym_table counter sr elt ls =
  let e = BidSet.empty in
  let un = bbool true in
  let lss = List.rev_map (fun t -> {pattern=t; pattern_vars=e; assignments=[]},un) ls in
  let fresh = Flx_bid.fresh_bid counter in
  let dflt =
    {
      pattern = btyp_type_var (fresh,Flx_kind.kind_type);
      pattern_vars = BidSet.singleton fresh;
      assignments=[]
    },
    bbool false
  in
  let lss = List.rev (dflt :: lss) in
  Flx_btype.btyp_type_match (elt, lss)


(* Reduce a typeset expression to a list of types *)
let rec reduce_typeset bsym_table counter sr t = 
  let r t = reduce_typeset bsym_table counter sr t in
  match t with
  (* each term must be a TYPE *)
  | BTYP_type_set ts ->
     List.map (beta_reduce' "Flx_beta.reduce_typeset" counter bsym_table sr 0 []) ts

  (* each term must be a TYPESET *)
  | BTYP_type_set_union tsets ->
     Flx_list.uniq_list (List.concat (List.map r tsets))

  | BTYP_inst (`Alias,index,ts,mt) ->
    begin try 
       let bsym = Flx_bsym_table.find bsym_table index in
       let bbdcl = Flx_bsym.bbdcl bsym in
       begin match bbdcl with
       | Flx_bbdcl.BBDCL_type_alias (bvs, alias) ->
         let alias = Flx_btype_subst.tsubst sr bvs ts alias in
         let alias = beta_reduce' "Flx_beta.reduce_typeset" counter bsym_table sr 0 [] alias in
         r alias
       | _ -> assert false
       end
     with Not_found -> 
       print_endline ("Flx_beta.expand_typeset: Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
       assert false
    end
  | t -> [t]


(* fixpoint reduction: reduce
   Fix f. Lam x. e ==> Lam x. Fix z. e [f x -> z]
   to replace a recursive function
   with a recursive data structure.

   Example: consider:

   list t = t * list t

   which is

   list = fix f. lam t. t * f t

   We can apply list to int:

   list int = (fix f. lam t. t * f t) int

   unfolding:

   list int = (lam t. t * (fix f. lam t. t * f t)) int
            = int * (fix f. lam t. t * f t) int
            = int * list int

   which is just

   list int = fix z. int * z

  Note: this is a recursive type NOT a recursive type function!
  This is the point: the functional recursion is eliminated.
  That is the kind of the recursion is changed.

  The rule ONLY works when a recursive function f
  is applied in its own definition to its own parameter.

  The rule traps in infinite expansion of a data type,
  and creates instead an recursive data type, eliminating
  the function.

  The normal beta reduction rule is

  (lam t. b) a => b [t->a]

  For a recursive function:

  (fix f. lam t. b) a => b[f-> fix f. lam t. b; t-> a]

  and the result must be reduced again.

  SO: rules for beta-reduce:

  Normally a free fixpoint is OK in a type expression being reduced.
  It is just returned "as is" except possibly for an adjustment of
  the level counter (to make sure it binds to the same term, in case
  some terms git reduced away).

  However if we have a type function application,
  and the function is a fixpoint, the whole function must be on the trail.

  If the application is to the functions parameter, the whole application
  is replaced by a type fixpoint (eliminating  the function fixpoint),
  i.e. we have a recursive type.

  If the application is to any other type, the application is replaced
  by ann application of the whole function and reduced. This unrolling
  continues until it either terminates (via a typematch reduction),
  or we get a recursive application. It is possible it may not
  terminate too, in which case its a BUG in the function.

  BOTTOM LINE: in an application the functional term can only
  be a fixpoint if the trail contains the function. Fix points are
  OK everywhere else and do not require a trail.

  Normally, only beta-reduce itself can introduce a trail,
  which means only beta-reduce is allowed to unravell a type
  function application.
*)

and fixup counter ps body =
 let param = match ps with
   | [] -> assert false
   | [i,mt] -> btyp_type_var (i,mt)
   | x -> btyp_type_tuple (List.map (fun (i,mt) -> btyp_type_var (i,mt)) x)
 in
 (*
 print_endline ("Body  = " ^ sbt bsym_table body);
 print_endline ("Param = " ^ sbt bsym_table param);
 *)
 let rec aux term depth =
   let fx t = aux t (depth+1) in
   match Flx_btype.map ~f_btype:fx term with
   | BTYP_type_apply (BTYP_fix (i,mt), arg)
     when arg = param
     && i + depth +1  = 0 (* looking inside application, one more level *)
     -> print_endline "SPECIAL REDUCTION";
(* HACK: meta type of fixpoint guessed *)
print_endline ("Flx_beta:fixup:aux: hacking meta type of fixpoint!");
     btyp_fix (i+2) (kind_type) (* elide application AND skip under lambda abstraction *)

   | BTYP_type_function (a,b,c) ->
      (* NOTE we have to add 2 to depth here, an extra
      level for the lambda binder.
      NOTE also: this is NOT a recusive call to fixup!
      It doesn't fixup this function.
      *)

      (*
      print_endline "OOPS >> no alpha conversion?";
      *)

      btyp_type_function (a, b, aux c (depth + 2))
   | x -> x
 in
   (* note depth 1: we seek a fix to an abstraction
   of which we're given only the body, that's an
   extra level in the term structure
   *)
   aux body 1

(* generic fixpoint adjuster: add amt to the fixpoint
   to make it span less deep term, to compensate
   for removing the top combinator of the term as a result
   of a one level adjustment eg: reduce a type match

   Note: do NOT call this to fix this case, for example:

   (1 -> int * F) as F

   when decoding it to  find the return type. If we just split
   the return type term out we get

   int * F

   as the return type, where F is now a free fixpoint. If we do
   an adjust as a result of an *analysis* we get

   (int * F) as F

   which is completely wrong. Analysis (breaking down terms) is NOT
   the same as a reduction: breaking the top level -> term up here
   is NOT the same as retaining the whole term in reduced form.

   The correct way to do an analysis is to do an unfold first.
   That would produce

   1 -> int * ((1 -> int * F) as F)

   which does not require an adjustment because there is no
   free fixpoint.


*)

and adjust bsym_table t =
(*
print_endline ("Fixpoint adjust " ^ sbt bsym_table t);
*)
  Flx_btype_rec.adjust_fixpoint t

(*
and mk_prim_type_inst i args =
  print_endline "MK_PRIM_TYPE";
  btyp_inst (i,args, kind_type)
*)

and beta_reduce calltag counter bsym_table sr t1 =
(*
  print_endline ("---------- " ^ calltag^ " Beta reduce " ^ sbt bsym_table t1 ^ "=" ^ Flx_btype.st t1);
*)
  let t2 =
  try
  beta_reduce' calltag counter bsym_table sr 0 [] t1
  with exn -> t1
(*
    | Not_found ->
        failwith ("Beta reduce called from " ^ calltag ^ " f ailed with Not_found in " ^
          sbt bsym_table t1)
    | Failure s ->
print_endline ("Beta reduce failed with Failure");
        failwith ("beta-reduce called from " ^ calltag ^ " failed in " ^ sbt bsym_table t1 ^
          "\nmsg: " ^ s ^ "\nsr= " ^ Flx_srcref.short_string_of_src sr)
    | exn -> print_endline ("Beta reduce failed with exn = " ^ Printexc.to_string exn);
      raise exn
  
  in
*)
  in 
(*
  print_endline ("============" ^ calltag^ "   reduced= " ^ sbt bsym_table t2 ^ "=" ^ Flx_btype.st t2);
*)
  let t2 = Flx_fold.minimise bsym_table counter t2 in
  t2

and beta_reduce' calltag counter bsym_table sr depth (termlist: (Flx_btype.t * int) list) t =
let spc = "  *** " in
(*
  print_endline ("BETA REDUCE' " ^ sbt bsym_table t ^ " trail length = " ^
    si (List.length termlist));
*)
  (*
  List.iter (fun t -> print_endline ("Trail term " ^ sbt bsym_table t))
  termlist
  ;
  begin match t with
  | BTYP_fix (i,mt) ->
    print_endline ("Fix point " ^ si i ^ " meta type " ^ sbt bsym_table mt);
  | _ -> ()
  end
  ;
*)
  if List.length termlist > 200
  then begin
    print_endline ("Trail=" ^ catmap "\n" (fun (t,depth) -> string_of_int depth ^ ": " ^sbt bsym_table t) termlist);
    failwith  ("Trail overflow, infinite expansion: BETA REDUCE " ^
    sbt bsym_table t ^ "\ntrail length = " ^ si (List.length termlist))
  end;
  let tli = 
    try 
      Flx_type_list_index.type_list_index counter bsym_table termlist t 
    with exc -> 
      print_endline ("type list index function failed  " ^ Printexc.to_string exc);
      assert false
  in
  match tli with
  | Some j ->
(* HACK: meta type of fixpoint guessed *)
    let mt = Flx_btype_kind.metatype sr t in
(*
print_endline ("Flx_beta: beta-reduce: hacking metatype of fixpoint, type is " ^ sbt bsym_table t);
print_endline ("Flx_beta: kind is " ^ sk mt);
*)
    let fp = btyp_fix (j-depth) mt  in
(*
print_endline ("Beta-reduce: type list index found term in trail, returning fixpoint " ^ sbt bsym_table fp);
*)
    fp

  | None ->
(*
print_endline "Type list index returned None";
*)
  let br t' = beta_reduce' calltag counter bsym_table sr (depth+1) termlist t' in
  let st t = sbt bsym_table t in
  match t with
  (* STAND ALONE TYPEDEF *)
  | BTYP_inst (`Alias, index,ts,mt) -> 
    (* let ts = List.map br ts in *)
    begin try 
      let bsym = Flx_bsym_table.find bsym_table index in
(*
      if bsym.Flx_bsym.id = "any" then print_endline ("Beta reduce any!");
*)
      let bbdcl = Flx_bsym.bbdcl bsym in
      begin match bbdcl with
      | Flx_bbdcl.BBDCL_type_alias (bvs, alias) ->
       let alias = Flx_btype_subst.tsubst sr bvs ts alias in
       let alias = beta_reduce' calltag counter bsym_table sr  depth ((t,depth)::termlist) alias in
       alias 
      | _ ->  
(*
print_endline ("Found non-typedef" ^ Flx_bsym.id bsym ^ "<" ^ string_of_int k ^ ">");
            let ts = List.map (aux trail level) ts in
            btyp_inst (k,ts) 
*)
        assert false
      end
    with Not_found -> 
print_endline ("Flx_expand_typedef: Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
      assert false
(*
print_endline ("Found unknown <" ^ string_of_int k ^ ">");
print_endline ("raw ts = " ^ catmap "," Flx_btype.st ts); 
            let ts = List.map (aux ((t, level+1)::trail) (level+1)) ts in
print_endline ("processed ts = " ^ catmap "," Flx_btype.st ts); 
            btyp_inst (k,ts) 
*)
    end

    (* STAND ALONE TYPE FUNCTION *)
    | BTYP_finst (index,ks,dom,cod) -> 
      begin try
        let bsym = Flx_bsym_table.find bsym_table index in
        let bbdcl = Flx_bsym.bbdcl bsym in
        begin match bbdcl with
        | Flx_bbdcl.BBDCL_type_function (bks, alias)  ->
          let ksubst (knd:Flx_kind.kind):Flx_kind.kind = Flx_kind.ksubst sr bks ks knd in
          let rec f_btype t = Flx_btype.map ~f_btype ~f_kind:ksubst t in
          let alias = f_btype alias in
          begin match alias with
          | BTYP_type_function _ -> alias
          | _ -> assert false
          end
        | _ -> 
          print_endline ("ERROR: finst doesn't refer to type function");
          assert false
        end
      with Not_found -> assert false
      end

  | BTYP_inst (`Nominal, i,ts,mt) -> btyp_inst (`Nominal,i, List.map br ts,mt)
  | BTYP_typeop (op,t,k) -> 
(*
print_endline ("Beta-reducing typeop " ^ op ^ ", type=" ^ sbt bsym_table t);
*)
    reduce_typeop op (br t) k
  | BTYP_typeof _ -> t
  | BTYP_instancetype sr -> btyp_instancetype sr
  | BTYP_ellipsis -> btyp_ellipsis (* not a value type! *)
  | BTYP_none -> assert false
  | BTYP_fix _ -> (* print_endline "Returning fixpoint"; *)  t
  | BTYP_type_var (i,_) -> t
  | BBOOL _ -> t

  | BTYP_type_function (p,r,b) -> t

  | BTYP_tuple_cons (t1,t2) -> btyp_tuple_cons (br t1) (br t2)
  | BTYP_tuple_snoc (t1,t2) -> btyp_tuple_snoc (br t1) (br t2)
  | BTYP_vinst (i,ts,mt) -> btyp_vinst (i, List.map br ts,mt)
  | BTYP_rev t -> btyp_rev (br t)
  | BTYP_uniq t -> btyp_uniq (br t)
  | BTYP_borrowed t -> btyp_borrowed (br t)

  | BTYP_compacttuple ls -> btyp_compacttuple (List.map br ls)
  | BTYP_compactarray (i,t) -> btyp_compactarray (br i, br t)
  | BTYP_compactrptsum (i,t) -> btyp_compactrptsum (br i, br t)
  | BTYP_compactsum ls -> btyp_compactsum (List.map br ls)


  | BTYP_tuple ls -> btyp_tuple (List.map br ls)
  | BTYP_intersect ls -> btyp_intersect (List.map br ls)
  | BTYP_array (i,t) -> btyp_array (br i, br t)
  | BTYP_rptsum (i,t) -> btyp_rptsum (br i, br t)
  | BTYP_sum ls -> btyp_sum (List.map br ls)

  | BTYP_record (ts) ->
     let ss,ls = List.split ts in
     btyp_record (List.combine ss (List.map br ls))

  | BTYP_polyrecord (ts,s,v) ->
     let ss,ls = List.split ts in
     btyp_polyrecord (List.combine ss (List.map br ls)) s (br v)


  | BTYP_variant ts ->
     let ss,ls = List.split ts in
     btyp_variant (List.combine ss (List.map br ls))

  | BTYP_polyvariant ts ->
    btyp_polyvariant (List.map (fun k -> match k with
      | `Ctor (s,t) -> `Ctor (s, br t)
      | `Base t -> `Base (br t)
    ) ts)

  | BTYP_type_set ls -> btyp_type_set (List.map br ls)

  | BTYP_in (elt, tset) ->
    let tset = beta_reduce' calltag counter bsym_table sr depth termlist tset in
    let tsetelts = reduce_typeset bsym_table counter sr tset in 
    let t = generate_typematch_predicate bsym_table counter sr elt tsetelts in
    beta_reduce' calltag counter bsym_table sr depth termlist t
 
  | BTYP_type_set_union ls ->
    let ls = List.rev_map br ls in
    (* split into explicit typesets and other terms
      at the moment, there shouldn't be any 'other'
      terms (since there are no typeset variables ..
    *)
    let rec aux ts ot ls  = match ls with
    | [] ->
      begin match ot with
      | [] -> btyp_type_set ts
      | _ ->
        (*
        print_endline "WARNING UNREDUCED TYPESET UNION";
        *)
        btyp_type_set_union (btyp_type_set ts :: ot)
      end

    | BTYP_type_set xs :: t -> aux (xs @ ts) ot t
    | h :: t -> aux ts (h :: ot) t
    in aux [] [] ls

  (* NOTE: sets have no unique unit *)
  (* WARNING: this representation is dangerous:
     we can only calculate the real intersection
     of discrete types *without type variables*

     If there are pattern variables, we may be able
     to apply unification as a reduction. However
     we have to be very careful doing that: we can't
     unify variables bound by universal or lambda quantifiers
     or the environment: technically I think we can only
     unify existentials. For example the intersection

     'a * int & long & 'b

     may seem to be long * int, but only if 'a and 'b are
     pattern variables, i.e. dependent variables we're allowed
     to assign. If they're actually function parameters, or
     just names for types in the environment, we have to stop
     the unification algorithm from assigning them (since they're
     actually particular constants at that point).

     but the beta-reduction can be applied anywhere .. so I'm
     not at all confident of the right reduction rule yet.

     Bottom line: the rule below is a hack.
  *)
  | BTYP_type_set_intersection ls ->
    let ls = List.map br ls in
    if List.mem (btyp_type_set []) ls then btyp_type_set []
    else begin match ls with
    | [t] -> t
    | ls -> btyp_type_set_intersection ls
    end


  | BTYP_type_tuple ls -> btyp_type_tuple (List.map br ls)

  | BTYP_function (a,b) -> btyp_function (br a, br b)
  | BTYP_effector (a,e,b) -> btyp_effector (br a, br e, br b)
  | BTYP_linearfunction (a,b) -> btyp_linearfunction (br a, br b)
  | BTYP_lineareffector (a,e,b) -> btyp_lineareffector (br a, br e, br b)
  | BTYP_cfunction (a,b) -> btyp_cfunction (br a, br b)

  | BTYP_ptr (m,t,ts)  -> btyp_ptr m (br t) (List.map br ts)

  | BTYP_label -> t
  | BTYP_void -> t
  | BTYP_unitsum _ -> t

  (* beta reduce map of function on explicit tuple as tuple of
     beta-reduced applications of function to tuple components
  *)
  | BTYP_type_map (t1,BTYP_tuple ls) ->
    let rs = 
      List.map (fun argt ->
        br (btyp_type_apply (t1,argt))
      )
      ls 
    in 
    btyp_tuple rs

  (* a bit hacky !!! Doesn't apply function to index type*)
  | BTYP_type_map (t1,BTYP_array (b,n)) ->
    let rb = br (btyp_type_apply (t1,b)) in
    btyp_array (rb,n)

  (* can't reduce *)
  | BTYP_type_map (t1,t2) -> btyp_type_map (br t1, br t2)

  | BTYP_type_apply (t1,t2) -> 
    (* 
      (* NOTE: suspect because trail comparison would be screwed up! *)
    let t1 = Flx_alpha.alpha_convert counter t1 in 
    *)
    Flx_type_fun.type_apply beta_reduce' calltag counter bsym_table sr depth termlist t1 t2

  | BTYP_type_match (tt,pts) ->
  begin
(*
    print_endline ("Typematch [before reduction] " ^ sbt bsym_table t ^ "=" ^ Flx_btype.st t);
*)
    let tt = br tt in
    let new_matches = ref [] in
    List.iter (fun ({pattern=p; pattern_vars=dvars; assignments=eqns}, t') ->
      (* print_endline (spc ^"Tring to unify argument with " ^ sbt bsym_table p); *)
      let p =  br p in
      let x =
        {
          pattern=p;
          assignments=List.map (fun (j,t) -> j, br t) eqns;
          pattern_vars=dvars;
        }, t'
      in
      match maybe_unification bsym_table counter [p,tt] with
      | Some _ -> 
        (* print_endline (spc ^"Argument unifies with  " ^ sbt bsym_table p); *)
        new_matches := x :: !new_matches
      | None ->
        (* print_endline (spc ^"Discarding pattern " ^ sbt bsym_table p); *)
        ()
    )
    pts
    ;
    let pts = List.rev !new_matches in
(* print_endline (spc ^ "Found " ^ string_of_int (List.length pts) ^ " unifies"); *)
    match pts with
    | [] ->
      print_endline ("[beta-reduce] typematch failure " ^ sbt bsym_table t);
      t 

    | ({pattern=p';pattern_vars=dvars;assignments=eqns},t') :: _ ->
      try
        (* print_endline (spc ^ "Redo unification now setting dependent vars"); *)
        (* print_endline ( spc ^ "Dvars = { " ^ catmap ", " si (Flx_bid.BidSet.elements dvars) ^ "}"); *)
        let mgu = unification bsym_table counter [p', tt] dvars in
        (* print_endline (spc ^ "Typematch success"); *)
        let t' = list_subst counter (mgu @ eqns) t' in
        beta_reduce' calltag counter bsym_table sr depth termlist t'
      with 
       | Not_found ->
         (* print_endline ("Match failed to reduce redex, return original term"); *)
         btyp_type_match (tt,pts)

  end

  | BTYP_subtype_match (tt,pts) ->
  begin
(*
    print_endline ("Typematch [before reduction] " ^ sbt bsym_table t ^ "=" ^ Flx_btype.st t);
*)
    let tt = br tt in
    let new_matches = ref [] in
    List.iter (fun ({pattern=p; pattern_vars=dvars; assignments=eqns}, t') ->
      (* print_endline (spc ^"Tring to unify argument with " ^ sbt bsym_table p'); *)
      let p =  br p in
      let x =
        {
          pattern=p;
          assignments=List.map (fun (j,t) -> j, br t) eqns;
          pattern_vars=dvars;
        }, t'
      in
      match maybe_specialisation bsym_table counter [p,tt] with
      | Some _ -> new_matches := x :: !new_matches
      | None ->
        (*
        print_endline (spc ^"Discarding pattern " ^ sbt bsym_table p');
        *)
        ()
    )
    pts
    ;
    let pts = List.rev !new_matches in
    match pts with
    | [] ->
      (*
      print_endline ("[beta-reduce] typematch failure " ^ sbt bsym_table t);
      *)
      t 

    | ({pattern=p';pattern_vars=dvars;assignments=eqns},t') :: _ ->
      let maybe_mgu = maybe_specialisation_with_dvars bsym_table counter [p', tt] dvars in
      begin match maybe_mgu with
      | Some mgu ->
        (*
        print_endline "Typematch success";
        *)
        let t' = list_subst counter (mgu @ eqns) t' in
        beta_reduce' calltag counter bsym_table sr depth termlist t'
      | None -> btyp_subtype_match (tt,pts)
      end
  end

