open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_beta

let str_of_bidset x = 
  let elts = ref [] in
  BidSet.iter (fun i -> elts := i::!elts) x;
  catmap "," si (!elts)

module CS = Flx_code_spec

let vs2ts vs = map (fun (s,i) -> btyp_type_var (i, btyp_type 0)) vs

(* drop first n elements of list l *)
let rec drop l n =
  if n = 0 then l else drop (tl l) (n-1)

let check_instance
  (syms:Flx_mtypes2.sym_state_t)
  bsym_table
  inst
  inst_id
  inst_vs
  inst_constraint
  inst_sr
  inst_props
  tc
  inst_ts
=
  (*
  print_endline ("Check instance, inst_constraint=" ^ sbt bsym_table inst_constraint);
  *)
  let tc_bsym = Flx_bsym_table.find bsym_table tc in
  match Flx_bsym.bbdcl tc_bsym with
  | BBDCL_typeclass (tc_props, tc_bvs) ->
    (*
    print_endline ("Found " ^ inst_id ^ "<"^si inst ^ ">" ^
    "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
    " to be instance of typeclass " ^ tc_id ^ "<"^si tc^">")
    ;
    print_endline ("Typeclass vs = " ^
      catmap "," (fun (s,j) -> s^"<"^si j^">") tc_bvs
    );
    *)
    if length tc_bvs <> length inst_ts then
      clierr2 inst_sr (Flx_bsym.sr tc_bsym)
      (
        "Instance [" ^
        catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") inst_vs
        ^ "] " ^
        inst_id ^"<"^ string_of_bid inst ^ ">" ^
        "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
        "\nsupplies wrong number of type arguments for typeclass parameters\n" ^
        inst_id ^ "[" ^
        catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") tc_bvs ^ "]"
      )
    ;

    let tc_kids =
      try Flx_bsym_table.find_children bsym_table tc
      with Not_found -> Flx_types.BidSet.empty
    in
    let inst_kids =
      try Flx_bsym_table.find_children bsym_table inst
      with Not_found -> Flx_types.BidSet.empty
    in
(*
if Flx_bsym.id tc_bsym = "Cart" then begin 
    print_endline ("Typeclass kids " ^ str_of_bidset tc_kids);
    print_endline ("Instance kids " ^ str_of_bidset inst_kids);
end;
*)
    let inst_map =
      Flx_types.BidSet.fold begin fun i acc ->
        let bsym = Flx_bsym_table.find bsym_table i in
        match Flx_bsym.bbdcl bsym with
        | BBDCL_external_fun (_,bvs,params,ret,_,_,_) ->
            let argt = btyp_tuple params in
            let qt = bvs, btyp_function (argt,ret) in
            (Flx_bsym.id bsym,(i,qt)) :: acc

        | BBDCL_fun (_,bvs,bps,ret,_) ->
            let argt = btyp_tuple (Flx_bparams.get_btypes bps) in
            let qt = bvs, btyp_function (argt,ret) in
            (Flx_bsym.id bsym,(i,qt)) :: acc

        | BBDCL_external_const (_,bvs,ret,_,_) ->
            let qt = bvs,ret in
            (Flx_bsym.id bsym,(i,qt)) :: acc

        | BBDCL_val (bvs,ret,`Val) ->
            let qt = bvs,ret in
            (Flx_bsym.id bsym,(i,qt)) :: acc
        | _ -> acc
      end inst_kids []
    in
    let check_binding force tck sr id tck_bvs tctype =
      let sigmatch i inst_funbvs t =
        (* typeclass X[t1,t2] { virtual fun f[t3] .. }
           Instance[i1, i2, i3] X[..,..] { fun f[i4] 

           typeclass fun poly vars = all fun vars - typeclass vars = 3 - 1 = 1
           inst fun poly vars = all fun vars - inst vars = 4 - 3 = 1
        *)

        let tc_ptv = length tck_bvs - length tc_bvs in
        let inst_ptv = length inst_funbvs - length inst_vs in
        if inst_ptv <> tc_ptv then (
          (*print_endline ("Wrong no args: inst_ptv="^ si inst_ptv^"<>"^si tc_ptv); *)
          false
        )
        else
        let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (length inst_vs)) in
        assert (length tck_bvs = length inst_funts);
        let tct = beta_reduce "flx_typeclass: check_instance"
          syms.Flx_mtypes2.counter
          bsym_table
          sr
          (tsubst tck_bvs inst_funts tctype)
        in
        let matches =  tct = t in
(*
if Flx_bsym.id tc_bsym = "Cart" then begin 
        print_endline ("Matches " ^ 
          sbt bsym_table tct ^ " = " ^ sbt bsym_table t ^ " is " ^ 
          (match matches with true->"true" | false -> "false")
        );
end;
*)
        matches
      in
      let entries = filter (fun (name,(i,(inst_funbvs,t))) -> name = id && sigmatch i inst_funbvs t) inst_map in
      match entries with
      | [] ->
          if force then
          clierr2 sr inst_sr ("Cannot find typeclass " ^ inst_id ^ " virtual " ^
            id ^ " in instance [" ^ catmap "," (sbt bsym_table) inst_ts ^
            "]")
(*
          else
          print_endline ("WARNING: Cannot find typeclass " ^ inst_id ^ " virtual " ^
            id ^ " in instance [" ^ catmap "," (sbt bsym_table) inst_ts ^
            "]")
*)

      | [_,(i,(inst_funbvs,t))] ->
        (*
        print_endline ("Typeclass " ^ tc_id ^ "<" ^ si tc ^">" ^ print_bvs tc_bvs);
        print_endline ("Typeclass function " ^ id ^ "<" ^ si tck ^ ">" ^
          print_bvs tck_bvs ^ ":" ^ sbt bsym_table tctype
        );

        print_endline ("Instance vs = " ^ print_bvs inst_vs);
        print_endline ("Instance ts = " ^ catmap "," (sbt bsym_table) inst_ts);
        print_endline ("Instance function " ^ id ^ "<"^si i^">" ^ print_bvs inst_funbvs ^
        ":" ^ sbt bsym_table t);
        *)

        let tc_ptv = length tck_bvs - length tc_bvs in
        (*
        print_endline ("Typeclass fun has " ^ si tc_ptv ^ " private type variables");
        *)

        let inst_ptv = length inst_funbvs - length inst_vs in
        (*
        print_endline ("Instance fun has " ^ si inst_ptv ^ " private type variables");
        *)

        if inst_ptv <> tc_ptv then
        clierr sr ("Wrong number of type parameters in instance fun!\n" ^
          "Expected " ^ si tc_ptv ^ "\n" ^
          "Got " ^ si inst_ptv
        );

        let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (length inst_vs)) in

        assert (length tck_bvs = length inst_funts);

        let tct = beta_reduce "flx_typeclass: check instance (2)"
          syms.Flx_mtypes2.counter
          bsym_table
          sr
          (tsubst tck_bvs inst_funts tctype)
        in
        (*
        print_endline ("Typeclass function (instantiated) " ^ id ^ "<" ^ si tck ^ ">" ^
          ":" ^ sbt bsym_table tct
        );
        *)

        (*
        (* THIS CHECK IS CRAP!!
           We need to actually use the unification engine here,
           not trivial equality.
        *)
        let matches =  tct = t in
        if matches then
          (*
          print_endline "Matches!";
          *)
          ()
        else begin
          print_endline "Warning: Sole instance doesn't match virtual";
          print_endline ("Typeclass " ^ tc_id ^ "<" ^ si tc ^">" ^ print_bvs tc_bvs);
          print_endline ("Typeclass function " ^ id ^ "<" ^ si tck ^ ">" ^
            print_bvs tck_bvs ^ ":" ^ sbt bsym_table tctype
          );

          print_endline ("Instance vs = " ^ print_bvs inst_vs);
          print_endline ("Instance ts = " ^ catmap "," (sbt bsym_table) inst_ts);
          print_endline ("Instance function " ^ id ^ "<"^si i^">" ^ print_bvs inst_funbvs ^
          ":" ^ sbt bsym_table t);
          print_endline ("Typeclass function (instantiated) " ^ id ^ "<" ^ si tck ^ ">" ^
            ":" ^ sbt bsym_table tct
          );
        end
        ;
        *)
        let old =
          try Hashtbl.find syms.virtual_to_instances tck
          with Not_found -> []
        in
        let entry = inst_vs, inst_constraint, inst_ts, i in
        if mem entry old then
          clierr sr "Instance already registered??"
        else
          Hashtbl.replace syms.virtual_to_instances tck (entry :: old);

        (*
        print_endline ("Register mapping " ^ si tck ^ " vs=" ^
          print_bvs inst_vs ^
          " constraint=(" ^ sbt bsym_table inst_constraint ^
          ") ts=[" ^ catmap "," (sbt bsym_table) inst_ts ^ "] -----> " ^ si i
        );
        *)

      | _ ->
        clierr sr ("Felix can't handle overloads in typeclass instances yet, " ^ id ^ " is overloaded")
    in

    Flx_types.BidSet.iter begin fun tck ->
      let tck_bsym = Flx_bsym_table.find bsym_table tck in
      match Flx_bsym.bbdcl tck_bsym with
      | BBDCL_external_fun (_,bvs,params,ret,_,_,`Code Flx_code_spec.Virtual) ->
        let ft = btyp_function (btyp_tuple params,ret) in
        (* check_binding true tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft *)
        check_binding false tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_fun (props,bvs,bps,ret,_) when mem `Virtual props ->
        let argt = btyp_tuple (Flx_bparams.get_btypes bps) in
        let ft = btyp_function (argt,ret) in
        check_binding false tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_external_const (props,bvs,ret,_,_) when mem `Virtual props ->
        check_binding false tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ret

      | BBDCL_external_code _ -> ()
      | BBDCL_axiom -> ()
      | BBDCL_lemma -> ()
      | BBDCL_reduce -> ()
      | _ ->
        (*
        clierr tcksr "Typeclass entry must be virtual function or procedure"
        *)
        (*
        print_endline ("Warning: typeclass " ^ Flx_bsym.id tc_bsym ^ " entry " ^
          Flx_bsym.id tck_bsym ^ " is not virtual");
        *)
        ()
    end tc_kids

  | _ ->
    clierr2 inst_sr (Flx_bsym.sr tc_bsym) ("Expected " ^ inst_id ^ "<" ^
      string_of_bid inst ^ ">[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
      " to be typeclass instance, but" ^ Flx_bsym.id tc_bsym ^ "<" ^
      string_of_bid tc ^ ">, is not a typeclass"
    )

let build_typeclass_to_instance_table syms bsym_table : unit =
  Flx_bsym_table.iter begin fun i _ bsym ->
  match Flx_bsym.bbdcl bsym with
  | BBDCL_instance (props, vs, cons, tc, ts) ->
(*
if Flx_bsym.id bsym = "Cart" then begin
print_endline ("processing instance <"^si i^">["^catmap "," (fun (v,i)->v^"<"^si i^">") vs^
  "] of class " ^ Flx_bsym.id bsym ^ 
  "<" ^ si tc ^">["^catmap "," (sbt bsym_table) ts^"]");
end;
*)
      let iss =
        try Hashtbl.find syms.instances_of_typeclass tc
        with Not_found -> []
      in
      let entry = i, (vs, cons, ts) in
      Hashtbl.replace syms.instances_of_typeclass tc (entry::iss)
      ;
      check_instance
        syms
        bsym_table
        i
        (Flx_bsym.id bsym)
        vs
        cons
        (Flx_bsym.sr bsym)
        props
        tc
        ts
  | _ -> ()
  end
  bsym_table

(* Notes.

  ts is the virtual function call ts, and generally doesn't
  include any inst_vs (unless the call is INSIDE the instance!)

  inst_vs are the type variables of the instance type schema
  They need to be eliminated, since they're arbitrary.

  inst_ts are the ts needed to replace the typeclass vs
  to obtain the candidate instance function signature from the
  virtual signature, these will contain variables of the
  instance type schema.

  We match up the call ts with the inst_ts first,
  to find values for the instance schema types, so we
  can eliminate them.

  But there is a special case: if the call is actually
  inside an instance, the ts may contain schema variables.
  In this context they're fixed variables, not to be
  eliminated. So any such variables have to be alpha converted
  away before the solution for inst_vs is found,
  the put back afterwards: when replacing the inst_vs,
  some of inst_vs type variable will then remain,
  which is correct.

  For technical reasons we do this backwards. We alpha convert
  the inst_vs in the inst_ts away, solve for the new set
  of variables, and then modify the solution back to
  the old set .. this is easier because the dependent
  variables are just integers, so the remapping
  doens't penetrate any type terms.
*)


let tcinst_chk syms bsym_table allow_fail id i ts sr (inst_vs, inst_constraint, inst_ts, j)  =
(*
if id = "make_cart" then begin
     print_endline
     ("virtual " ^ id^ "<"^si i ^ ">[" ^ catmap "," (sbt bsym_table) ts ^ "]");
end;
*)
     if length inst_ts > length ts then
       failwith (
         "Not enough ts given, expected at least " ^
         si (length inst_ts) ^ ", got " ^ si (length ts)
       )
     ;
     (* solve for vs' *)
     let vis = List.map (fun _ -> fresh_bid syms.counter) inst_vs in
     let nuvs = map (fun i -> btyp_type_var (i, btyp_type 0)) vis in
     let inst_ts' = map (tsubst inst_vs nuvs) inst_ts in
     let vset = fold_left (fun acc i -> BidSet.add i acc) BidSet.empty vis in

     (*
     let vset = fold_left (fun acc (_,i) -> IntSet.add i acc) IntSet.empty inst_vs' in
     *)
     let eqns = combine (list_prefix ts (length inst_ts)) inst_ts' in
(*
if id = "make_cart" then begin
     print_endline ("Solving equations\n " ^
       catmap "\n" (fun (a,b) -> sbt bsym_table a ^ " = " ^ sbt bsym_table b ) eqns
     );
end;
*)
     (* Well, this is a hack, but it may help solve equations containing
      lambdas and type matches which unification can't handle
     *)
     let assigns, rest = fold_left 
        (fun (assigns,rest) (l,r) -> 
         match l,r with
         | BTYP_type_var (x,BTYP_type 0), other
         | other, BTYP_type_var (x,BTYP_type 0) -> (x,other) :: assigns, rest

         | _ -> assigns, (l,r) :: rest
        )
        ([],[]) 
        eqns 
     in
(*
     print_endline (id ^ " Now Solving equations\n " ^
       catmap "\n" (fun (a,b) -> sbt bsym_table a ^ " = " ^ sbt bsym_table b ) rest 
     );
     print_endline (id ^ " With assignments \n " ^
       catmap "\n" (fun (a,b) -> si a ^ " = " ^ sbt bsym_table b ) assigns
     );
*)
     let eqns = map (fun (l,r) -> Flx_unify.list_subst syms.counter assigns l, Flx_unify.list_subst syms.counter assigns r) rest in
(*
     print_endline (id ^ " After quick subst: Solving equations\n " ^
       catmap "\n" (fun (a,b) -> sbt bsym_table a ^ " = " ^ sbt bsym_table b ) eqns
     );
*)
     let eqns = map 
       (fun (l,r) -> 
         beta_reduce "flx_typeclass: tcinst_check: lhs" syms.counter bsym_table sr l,
         beta_reduce "flx_typeclass: tcinst_check: rhs" syms.counter bsym_table sr r
       ) 
       eqns
     in
(*
if id = "make_cart" then begin
     print_endline (id ^ " After beta reduction: Solving equations\n " ^
       catmap "\n" (fun (a,b) -> sbt bsym_table a ^ " = " ^ sbt bsym_table b ) eqns
     );
end;
*)
     let assignments = map (fun (i,t) -> btyp_type_var (i,btyp_type 0),t) assigns in
     let mgu =
       try Some (unification bsym_table syms.counter (assignments @ eqns) vset)
       with Not_found -> None
     in
     begin match mgu with
     | None -> 
(*
if id = "make_cart" then begin
print_endline (id ^ " Match failed, try unification");
end;
*)
       begin try 
         let mgu = (maybe_unification bsym_table syms.counter (assignments @ eqns)) in
         match mgu with
         | Some mgu -> 
(*
print_endline (id ^ " Unified");
*)
(*let mgu =
             let goback = combine vis (map (fun (_,i)->i) inst_vs) in
             map (fun (i,t) -> 
               begin try assoc i goback
               with Not_found -> print_endline (id ^ " Cannot find variable " ^ si i); raise Not_found
               end
               , t) 
             mgu
           in
*)
           let con = try list_subst syms.counter mgu inst_constraint with Not_found -> assert false in
           let con = try Flx_beta.beta_reduce "flx_typeclass: handle mgu" syms.Flx_mtypes2.counter bsym_table sr con with Not_found -> assert false in
           begin match con with
           | BTYP_tuple [] -> (* print_endline (id ^ " MATCHES LATER MAYBE"); *) `MaybeMatchesLater,0,[]
           | BTYP_void ->  (* print_endline (id ^ " cannot match - void"); *) `CannotMatch,0,[]
           | _ -> (* print_endline (id ^ " cannot match constraint fail"); *) `CannotMatch,0,[]
           end
         | None -> 
(*
           if id = "make_cart" then begin 
             print_endline (id ^ " cannot match no mgu"); 
           end;
*)
           `CannotMatch,0,[]
       with
         Not_found -> (* print_endline (id ^ " cannot match exception Not_found thrown"); *) `CannotMatch,0,[]
       end
     | Some mgu ->
       let mgu =
         let goback = combine vis (map (fun (_,i)->i) inst_vs) in
         map (fun (i,t) -> assoc i goback, t) mgu
       in
       let tsv =
         map
         (fun (s,i) ->
           if not (mem_assoc i mgu) then
             failwith ("Didn't solve for instance type variable " ^ s)
           else
           (
             (*
             print_endline ("Solved " ^ s ^"<"^si i^">" ^ "-> " ^ sbt bsym_table (assoc i mgu));
             *)
             assoc i mgu
           )
         )
         inst_vs
       in
       let con = list_subst syms.counter mgu inst_constraint in
       let con = Flx_beta.beta_reduce "flx_typeclass: constraint" syms.Flx_mtypes2.counter bsym_table sr con in
       match con with
       | BTYP_tuple [] ->
         let tail = drop ts (length inst_ts) in
         let ts = tsv @ tail in
         (*
         print_endline ("Remap to " ^ si j ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]");
         *)
         (`MatchesNow,j,ts)
       | BTYP_void -> (* print_endline "constraint reduce failure"; *) `CannotMatch,0,[]
       | _ ->
         if not allow_fail then
         failwith ("Unable to reduce type constraint: " ^ sbt bsym_table con)
         else
         (
           (*
           print_endline ("Unable to reduce type constraint! " ^ sbt bsym_table con);
           *)
           try 
             ignore (maybe_unification bsym_table syms.counter (assignments @ eqns));
             `MaybeMatchesLater,0,[]
           with
             Not_found -> `CannotMatch,0,[]
         )
     end


let fixup_typeclass_instance' syms bsym_table allow_fail i ts =
  let id = Flx_bsym.id (Flx_bsym_table.find bsym_table i) in
  let entries =
    try Hashtbl.find syms.virtual_to_instances i
    with Not_found -> (* print_endline ("Symbol " ^ si i ^ " Not instantiated?"); *) []
  in
(*
  if List.length entries > 0 then begin 
if id = "make_cart" then begin
    print_endline ("Attempt to fixup virtual function " ^ id ^ "<" ^ si i ^ ">" ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]");
    print_endline ("Found " ^ si (List.length entries) ^ " candidates:");
    List.iter
      (fun (bvs, ret, args, ix) -> print_endline (
        Flx_print.string_of_bvs bvs ^ 
        (catmap "*" (sbt bsym_table) args) ^ "->" ^ sbt bsym_table ret) 
      )
    entries
end

  end
  ;
*)
  let sr = try Flx_bsym_table.find_sr bsym_table i with Not_found ->
    failwith ("fixup_typeclass_instance': Can't find <" ^ string_of_bid i ^ ">")
  in
  let entries = fold_left 
     (fun acc x -> 
     match tcinst_chk syms bsym_table allow_fail id i ts sr x with
     | `CannotMatch,_,_ -> acc
     | jts -> (jts,x)::acc
     ) 
     [] entries
  in
(*
print_endline ("Number of matches left is " ^ string_of_int (List.length entries));
*)
(* THIS ALGORITHM IS WRONG. IT SHOULD WORK LIKE TYPEMATCH BINDING:

  In step 1 we must do this:

   (a) case cannot ever match: throw it out
   (b) case might match later: keep it
   (c) case matches now: keep it

  (a) or (b or c) is determined by unification with all variables dependent
  if this unification fails we can't ever get a match.
  if it passes, we retry setting dependent variables to those in the LHS (virtual)
  if that passes it matches now, otherwise it doesn't but still might later

  Then, with all the cases (a) (b) we try for the most specialised one.
  If we find exactly one most specialised one AND it matches NOW
  then we can replace the virtual with it.

  Otherwise we have to wait, even if there is a match NOW on a less
  specialised instance, in case later the more specialised instances
  matches after further replacement of type variables.
*)

  match entries with
  | [] -> `CannotMatch,i,ts
  | [(`MaybeMatchLater,_,_),_] -> 
(*
     print_endline ("defer virtual(0) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "]");
*)
    `MaybeMatchesLater,i,ts


  | [(`MatchesNow,j,ts'),_] ->
(*
     print_endline ("map virtual(1) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "] to instance " ^ 
       si j ^ "[" ^ catmap "," (sbt bsym_table) ts' ^ "]");
*)
     `MatchesNow,j,ts'

  | candidates ->
(*
     print_endline ("multiple candidates(2) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "]");
*)
    if not (Flx_bsym_table.mem bsym_table i) then
      failwith ("Woops can't find virtual function index "  ^ string_of_bid i);

    (*
    print_endline
    ("Multiple matching instances for typeclass virtual instance\n"
     ^id^"<"^ si i^">["^ catmap "," (sbt bsym_table) ts ^"]"
    )
    ;
    *)
    (*
    iter
    (fun ((j,ts),(inst_vs,con,inst_ts,k)) ->
       let id,parent,sr,entry =
         try Flx_bsym_table.find bsym_table j
         with Not_found -> failwith ("Woops can't find instance function index "  ^ si j)
       in
       let parent = match parent with Some k -> k | None -> assert false in
       print_endline ("Function " ^ si j ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]");
       print_endline (" instance parent " ^ si parent ^ "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]");
       print_endline (" instance vs= " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") inst_vs );
    )
    candidates
    ;
    *)
    let candidates = fold_left
    (fun oc (((matchcat,j,ts),(inst_vs,con,inst_ts,k)) as r) ->
       let c = btyp_type_tuple inst_ts in
       (*
       print_endline ("Considering candidate sig " ^ sbt bsym_table c);
       *)
       let rec aux lhs rhs =
         match rhs with
         | [] ->
           (*
           print_endline "return elements plus candidate";
           *)
           r::lhs (* return all non-greater elements plus candidate *)
         | (((matchcat,j,ts),(inst_vs,con,inst_ts,k)) as x)::tail ->
           let c' = btyp_type_tuple inst_ts in
           (*
           print_endline (" .. comparing with " ^ sbt bsym_table c');
           *)
           begin match compare_sigs bsym_table syms.counter c' c with
           | `Less ->
             (*
             print_endline "Candidate is more general, discard it, retain whole list";
             *)
             lhs @ rhs (* keep whole list, discard c *)
           | `Equal ->
             aux (x::lhs) tail (* keep element *)

           | `Greater ->
             (*
             print_endline "Candidate is less general: discard this element";
             *)
             aux lhs tail (* discard greater element *)
           | `Incomparable ->
             (*
             print_endline "Candidate is comparable, retail element";
             *)
             aux (x::lhs) tail (* keep element *)
         end
       in aux [] oc
    )
    []
    candidates
    in
    match candidates with
    | [] -> `CannotMatch,i,ts
    | [(`MatchesNow,j,ts'),(inst_vs,con,inst_ts,k)] ->
(*
      print_endline ("map virtual(3) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "] to instance " ^ 
      si j ^ "[" ^ catmap "," (sbt bsym_table) ts' ^ "]");
*)
      `MatchesNow,j,ts'

    | candidates -> 
(*
      print_endline ("map virtual(3) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "] DEFERED");
*)
      `MaybeMatchesLater,i,ts 
      (*
      List.iter begin fun ((matchcat,j,ts),(inst_vs,con,inst_ts,k)) ->
        let parent, bsym =
          try Flx_bsym_table.find_with_parent bsym_table j
          with Not_found ->
            failwith ("Woops can't find instance function index " ^
              string_of_bid j)
        in
        let parent =
          match parent with
          | Some k -> k
          | None -> assert false
        in
        print_endline ("Function " ^ string_of_bid j ^ "[" ^
          catmap "," (sbt bsym_table) ts ^ "]");
        print_endline (" instance parent " ^ string_of_bid parent ^ "[" ^
          catmap "," (sbt bsym_table) inst_ts ^ "]");
        print_endline (" instance vs= " ^
          catmap "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") inst_vs);
      end candidates;

      clierr sr "No most specialised instance!"
      *)

let id x = x

let tcsubst syms bsym_table flag i ts =
  match fixup_typeclass_instance' syms bsym_table flag i ts with _,i,ts->i,ts

let fixup_expr syms bsym_table e =
  (*
  print_endline ("Check expr " ^ sbe sym_table e);
  *)
  let rec f_bexpr e =
    match Flx_bexpr.map ~f_bexpr e with
    | BEXPR_apply_direct (i,ts,a),t ->
        let a = f_bexpr a in
        let j,ts = (* print_endline ("Check apply direct " ^ si i);  *)
          tcsubst syms bsym_table true i ts
        in
        (*
        if j <> i then print_endline ("[direct] instantiate virtual as " ^ si j);
        *)
        bexpr_apply_direct t (j,ts,a)

    | BEXPR_apply_prim (i,ts,a),t ->
        let a = f_bexpr a in
        let j,ts = (* print_endline ("Check apply prim " ^ si i^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"); *)
          tcsubst syms bsym_table true i ts
        in
        (*
        if j <> i then
        print_endline ("[prim] instantiate virtual as " ^
          si j ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
        )
        ;
        *)
        bexpr_apply_direct t (j,ts,a)

    | BEXPR_varname (i,ts),t ->
        let j,ts = (* print_endline ("Check apply prim " ^ si i^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"); *)
        tcsubst syms bsym_table true i ts in
        bexpr_varname t (j,ts)

    | x -> x
  in
  f_bexpr e

let fixup_exe syms bsym_table exe =
  match exe with
  | BEXE_call_direct (sr,i,ts,a) ->
    let j,ts = tcsubst syms bsym_table true i ts in
    (*
    if j <> i then print_endline "instantiate virtual ..";
    *)
    let a  = fixup_expr syms bsym_table a in
    bexe_call_direct (sr,j,ts,a)
  | x ->
    Flx_bexe.map ~f_bexpr:(fixup_expr syms bsym_table) x

let fixup_exes syms bsym_table exes = map (fixup_exe syms bsym_table) exes

let fixup_typeclass_instances syms bsym_table =
  Flx_bsym_table.update_bexes (fixup_exes syms bsym_table) bsym_table

(* this routine doesn't allow constraint reduction failure
  and should only be run at instantiation time
*)
let fixup_typeclass_instance syms bsym_table i ts =
  tcsubst syms bsym_table false i ts

(* this routine allows failure, only use for early
  instantiation for optimisation
*)
let maybe_fixup_typeclass_instance syms bsym_table i ts =
  tcsubst syms bsym_table true i ts


