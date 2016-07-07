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
if inst_id = "X" then
  print_endline ("Check instance, inst_constraint=" ^ sbt bsym_table inst_constraint);
*)
(* STEP 1: find the typeclass *)
  let tc_bsym = Flx_bsym_table.find bsym_table tc in
  let tc_id = Flx_bsym.id tc_bsym in
  match Flx_bsym.bbdcl tc_bsym with
  | BBDCL_typeclass (tc_props, tc_bvs) ->
(*
if inst_id = "X" then
begin
    print_endline ("Found " ^ inst_id ^ "<"^si inst ^ ">" ^
    "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
    " to be instance of typeclass " ^ tc_id ^ "<"^si tc^">")
    ;
    print_endline ("Typeclass vs = " ^
      catmap "," (fun (s,j) -> s^"<"^si j^">") tc_bvs
    );
end;
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
(* list all the kids of the type class *)
    let tc_kids =
      try Flx_bsym_table.find_children bsym_table tc
      with Not_found -> Flx_types.BidSet.empty
    in
(*
if inst_id = "X" then 
begin
  print_string ("Typeclass has children " );
  BidSet.iter (fun i-> print_string (si i ^ ",")) tc_kids;
  print_endline "";
end;
*)
(* list all the kids of the instance *)
    let inst_kids =
      try Flx_bsym_table.find_children bsym_table inst
      with Not_found -> Flx_types.BidSet.empty
    in
(*
if inst_id = "X" then
begin
  print_string ("Instance has children ");
  BidSet.iter (fun i-> print_string (si i ^ ",")) inst_kids;
  print_endline "";
end;
*)
(* transform the instance kid list into an associatiion list
  mapping the function name to the index and function type
*)
    let inst_map =
      Flx_types.BidSet.fold begin fun i acc ->
        let bsym = Flx_bsym_table.find bsym_table i in
        match Flx_bsym.bbdcl bsym with
        | BBDCL_external_fun (_,bvs,params,ret,_,_,_) ->
            let argt = btyp_tuple params in
            let qt = bvs, btyp_function (argt,ret) in
            (Flx_bsym.id bsym,(i,qt)) :: acc

        | BBDCL_fun (_,bvs,bps,ret,effects,_) ->
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
(*
if inst_id = "X" then
begin
  List.iter (fun (name,(index,(bvs,typ))) ->
    print_endline (name ^"<" ^ si index ^ ">: " ^ sbt bsym_table typ);
  )
  inst_map
end;
*)

(* check the polymorphic binding of the virtual function id[tck,tck_bvs] type tctype
   matches the given instance 
*)
    let check_binding tck sr id tck_bvs tctype =
      let sigmatch i inst_funbvs t =
        (* typeclass X[t1,t2] { virtual fun f[t3] .. }
           Instance[i1, i2, i3] X[..,..] { fun f[i4] 

           typeclass fun poly vars = all fun vars - typeclass vars = 3 - 1 = 1
           inst fun poly vars = all fun vars - inst vars = 4 - 3 = 1
        *)

        let tc_ptv = length tck_bvs - length tc_bvs in
        let inst_ptv = length inst_funbvs - length inst_vs in
(*
if id = "g" then
begin
  print_endline ("Type class has " ^ si (List.length tc_bvs) ^ " type variables");
  print_endline ("Virtual has " ^ si (List.length tck_bvs) ^ " type variables (total)");
  print_endline ("Virtual has " ^ si tc_ptv ^ " type variables (local)");
  print_endline ("Instance has " ^ si (List.length inst_vs) ^ " type variables (total)");
  print_endline ("Virtual instance function has " ^ si (List.length inst_funbvs) ^ " type variables (total)");
  print_endline ("Virtual instance function has " ^ si inst_ptv ^ " type variables (local)");
end;
*)
        if inst_ptv <> tc_ptv then (
(*
if id = "g" then
          print_endline ("Wrong no args: inst_ptv="^ si inst_ptv^"<>"^si tc_ptv);
*)
          false
        )
        else
        let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (length inst_vs)) in
        assert (length tck_bvs = length inst_funts);
        let tct = beta_reduce "flx_typeclass: check_instance"
          syms.Flx_mtypes2.counter
          bsym_table
          sr
          (tsubst inst_sr tck_bvs inst_funts tctype)
        in
(*
if id = "g" then
  print_endline ("Virtual type: " ^ sbt bsym_table tct ^ ", instance type: " ^ sbt bsym_table t);
*)
        let matches =  tct = t in
        matches
      in

(* strip out all the instance kids with the wrong name, or for which the signatures don't agree *)
      let entries = filter (fun (name,(i,(inst_funbvs,t))) -> name = id && sigmatch i inst_funbvs t) inst_map in
(*
if inst_id = "X" then
  print_endline ("We have " ^ si (List.length entries) ^ " functions left");
*)
(* see what we have left *)
      match entries with

(* if there's nothing left that's fine! The virtual might never be called. Alternatively a default
in the typeclass might be used instead. This routine only handles actual instances!
*)
      | [] -> ()

      | [_,(i,(inst_funbvs,t))] ->
        let tc_ptv = length tck_bvs - length tc_bvs in
        let inst_ptv = length inst_funbvs - length inst_vs in
        if inst_ptv <> tc_ptv then
        clierrx "[flx_frontend/flx_typeclass.ml:208: E361] " sr ("Wrong number of type parameters in instance fun!\n" ^
          "Expected " ^ si tc_ptv ^ "\n" ^
          "Got " ^ si inst_ptv
        );

        let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (length inst_vs)) in

        assert (length tck_bvs = length inst_funts);

        let tct = beta_reduce "flx_typeclass: check instance (2)"
          syms.Flx_mtypes2.counter
          bsym_table
          sr
          (tsubst inst_sr tck_bvs inst_funts tctype)
        in
        let old =
          try Hashtbl.find syms.virtual_to_instances tck
          with Not_found -> []
        in
        let entry = inst_vs, inst_constraint, inst_ts, i in
        if mem entry old then
          clierrx "[flx_frontend/flx_typeclass.ml:229: E362] " sr "Instance already registered??"
        else begin
(* finally, add the instance to the virtual to instance mapping table for subsequent lookups *)
          Hashtbl.replace syms.virtual_to_instances tck (entry :: old);
        end

      | _ ->
        clierrx "[flx_frontend/flx_typeclass.ml:236: E363] " sr ("Felix can't handle overloads in typeclass instances yet, " ^ id ^ " is overloaded")
    in

    Flx_types.BidSet.iter begin fun tck ->
      let tck_bsym = Flx_bsym_table.find bsym_table tck in
      match Flx_bsym.bbdcl tck_bsym with
      | BBDCL_external_fun (_,bvs,params,ret,_,_,`Code Flx_code_spec.Virtual) ->
        let ft = btyp_function (btyp_tuple params,ret) in
        check_binding tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_fun (props,bvs,bps,ret,effects,_) when mem `Virtual props ->
        let argt = btyp_tuple (Flx_bparams.get_btypes bps) in
        (* ignore effects for now! *)
        let ft = btyp_function (argt,ret) in
        check_binding tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_external_const (props,bvs,ret,_,_) when mem `Virtual props ->
        check_binding tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ret

      | BBDCL_external_code _ -> ()
      | BBDCL_axiom -> ()
      | BBDCL_lemma -> ()
      | BBDCL_reduce -> ()
      | _ ->
        (*
        clierrx "[flx_frontend/flx_typeclass.ml:261: E364] " tcksr "Typeclass entry must be virtual function or procedure"
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


(* NEW SEMANTICS 20/12/2015: every virtual function will be in this
table. If it is a virtual with a default, that shall be included
as an instance. If it has no default, and no separate instances,
it must still have an empty entry. Lookup on the table determines
if a function is virtual. If a lookup succeeds when instantiating
virtuals, the compiler must bug out if no instances matches
a call. After monomorphisation, only monomorphised clones 
of virtuals with defaults will be present, except for the special
case of monomorphising a monomorphic virtual which retains
its original identity.
*)

let build_typeclass_to_instance_table syms bsym_table : unit =
  let get_insts tc =
    try Hashtbl.find syms.instances_of_typeclass tc
    with Not_found -> 
    Hashtbl.add syms.instances_of_typeclass tc [];
    []
  in
  Flx_bsym_table.iter begin fun i parent bsym ->
  let id = Flx_bsym.id bsym in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_instance (props, vs, cons, tc, ts) ->
    let iss = get_insts tc in
    let entry = i, (vs, cons, ts) in
    Hashtbl.replace syms.instances_of_typeclass tc (entry::iss);
  
    let inst_id = Flx_bsym.id bsym in
(*
    if inst_id = "X" then
    print_endline ("Typeclass: " ^ Flx_bsym.id bsym ^"<"^ si tc ^ "> instance " ^ si i );
*)
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

  (* virtual with default *)
  | BBDCL_fun (props,bvs,params,rettype,effects, exes) ->
    if List.mem `Virtual  props then
    let tc = match parent with
    | None -> assert false
    | Some p -> p
    in
    let v2i = 
      try Hashtbl.find syms.virtual_to_instances i 
      with Not_found -> Hashtbl.add syms.virtual_to_instances i []; []  
    in
    let cons = Flx_btype.btyp_unit () in
    let ts = List.map (fun (s,j) -> Flx_btype.btyp_type_var (j,Flx_btype.btyp_type 0)) bvs in
    let entry = bvs, cons, ts,i in (* self reference *)
    Hashtbl.replace syms.virtual_to_instances i (entry::v2i)

  (* virtual with default *)
  | BBDCL_external_fun (props,bvs,paramtypes,rettype,reqs,prec,kind) ->
    begin match kind with
    | `Code Flx_code_spec.Virtual -> 
(*
       print_endline ("Pure virtual primitive " ^ Flx_bsym.id bsym);
*)
       assert (List.mem `Virtual props)
    | _ -> ()
(*
      if List.mem `Virtual props then
        print_endline ("Instantiated Virtual primitive " ^ Flx_bsym.id bsym);
*)
    end;
    if List.mem `Virtual  props then
    begin match kind with
    | `Code (Flx_code_spec.Str_template s)
    | `Code (Flx_code_spec.Str s) ->
      let v2i = 
        try Hashtbl.find syms.virtual_to_instances i 
        with Not_found -> Hashtbl.add syms.virtual_to_instances i []; []  
      in
      if s <> "" then
        let cons = Flx_btype.btyp_unit () in
        let ts = List.map (fun (s,j) -> Flx_btype.btyp_type_var (j,Flx_btype.btyp_type 0)) bvs in
        let entry = bvs, cons, ts,i in (* self reference *)
        Hashtbl.replace syms.virtual_to_instances i (entry::v2i)

    | _ -> ()
    end
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


let tcinst_chk syms bsym_table id sr i ts (inst_vs, inst_constraint, inst_ts, j)  =
     if length inst_ts > length ts then
       clierrx "[flx_frontend/flx_typeclass.ml:409: E365] " sr (
         "Not enough ts given, expected at least " ^
         si (length inst_ts) ^ ", got " ^ si (length ts)
       )
     ;
     (* solve for vs' *)
     let vis = List.map (fun _ -> fresh_bid syms.counter) inst_vs in
     let nuvs = map (fun i -> btyp_type_var (i, btyp_type 0)) vis in
     let inst_ts' = map (tsubst sr inst_vs nuvs) inst_ts in
     let vset = fold_left (fun acc i -> BidSet.add i acc) BidSet.empty vis in

     (*
     let vset = fold_left (fun acc (_,i) -> IntSet.add i acc) IntSet.empty inst_vs' in
     *)
     let eqns = combine (list_prefix ts (length inst_ts)) inst_ts' in
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
     let assignments = map (fun (i,t) -> btyp_type_var (i,btyp_type 0),t) assigns in
     let mgu =
       try Some (unification bsym_table syms.counter (assignments @ eqns) vset)
       with Not_found -> None
     in
     begin match mgu with
     | None -> 
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
         `CannotMatch,0,[]
     end


let fixup_typeclass_instance' syms bsym_table sr i ts =
  let id = Flx_bsym.id (Flx_bsym_table.find bsym_table i) in
(*
if id="g" then
print_endline ("Trying to instantiate virtual " ^ id ^ "<" ^ si i ^ ">[" ^ 
catmap "," (sbt bsym_table) ts ^ "]");
*)
  let entries =
    try Some (Hashtbl.find syms.virtual_to_instances i)
    with Not_found -> None
  in
  match entries with
  | None -> 
(*
if id="g" then
    print_endline ("Virtual not registered,is it virtual?");
*)
    `NonVirtual,i,ts (* not virtual *)
  | Some [] -> 
(*
if id="g" then
    print_endline ("Virtual registered with 0 instances");
*)
    let sr2 = try Flx_bsym_table.find_sr bsym_table i with Not_found ->
      failwith ("fixup_typeclass_instance': Can't find <" ^ string_of_bid i ^ ">")
    in
    clierr2 sr sr2 "No instance for virtual"

  | Some entries ->
(*
if id="g" then
print_endline ("Found " ^ si (List.length entries) ^ " functions for virtual " ^ id ^"<"^si i^">");
*)
  let parent,bsym = try Flx_bsym_table.find_with_parent bsym_table i with Not_found -> assert false in
  let parent = match parent with | Some parent -> parent | None -> assert false in
  let tcbsym = 
    try 
      Flx_bsym_table.find bsym_table parent 
     with Not_found -> 
       print_endline ("Cannot find virtual "^id^"<"^si i^"> parent " ^ si parent);
       assert false 
  in
  let ntcbvs = List.length (Flx_bsym.get_bvs tcbsym) in 
   

  let entries = fold_left 
     (fun acc x -> 
     match tcinst_chk syms bsym_table id sr i ts x with
     | `CannotMatch,_,_ -> acc
     | jts -> (jts,x)::acc
     ) 
     [] entries
  in
(*
if id="g" then
print_endline ("Number of matches left is " ^ string_of_int (List.length entries));
*)
  match entries with
  | [] -> `CannotMatch,i,ts
  | [(`MaybeMatchesLater,_,_),_] -> 
(*
     print_endline ("defer virtual(0) " ^ id ^ " = " ^ si i ^ "["^ catmap "," (sbt bsym_table) ts ^ "]");
*)
    `MaybeMatchesLater,i,ts


  | [(`MatchesNow,j,ts'),_] ->
(*
if id="h" then
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
if id="g" then
begin
    print_endline
    ("Multiple matching instances for typeclass virtual instance\n"
     ^id^"<"^ si i^">["^ catmap "," (sbt bsym_table) ts ^"]"
    )
    ;
    iter
    (fun ((matchkind,j,ts),(inst_vs,con,inst_ts,k)) ->
       let id,parent,sr,entry =
         try 
           let parent,sym = Flx_bsym_table.find_with_parent bsym_table j in
           sym.Flx_bsym.id, parent, sym.Flx_bsym.sr, sym.Flx_bsym.bbdcl
         with Not_found -> failwith ("Woops can't find instance function index "  ^ si j)
       in
       let parent = match parent with Some k -> k | None -> assert false in
       print_endline (match matchkind with | `CannotMatch -> "cannot match" | `MatchesNow -> "matches now" | `MaybeMatchesLater -> "maybe match later");
       print_endline ("Function " ^ si j ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]");
       print_endline (" instance parent " ^ si parent ^ "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]");
       print_endline (" instance vs= " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") inst_vs );
    )
    candidates
end;
*)
    let candidates = fold_left
    (fun oc (((matchcat,j,ts),(inst_vs,r_con,inst_ts,k)) as r) ->
       let c = btyp_type_tuple (Flx_list.list_prefix inst_ts ntcbvs) in
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
         | (((matchcat,j,ts),(inst_vs,x_con,inst_ts,k)) as x)::tail ->
           let c' = btyp_type_tuple (Flx_list.list_prefix inst_ts ntcbvs) in
(*
           print_endline (" .. compare "^sbt bsym_table c^ " with " ^ sbt bsym_table c');
*)
           begin match compare_sigs bsym_table syms.counter c' c with
           | `Less ->
(*
             print_endline "Candidate is more general, discard it, retain whole list";
*)
             lhs @ rhs (* keep whole list, discard c *)
           | `Equal ->
(*
print_endline ("Equal elements? constraints x,r: " ^ sbt bsym_table x_con ^ " =? " ^ sbt bsym_table r_con);
*)
             begin match x_con, r_con with
             | BTYP_tuple [], BTYP_tuple [] ->
(*
print_endline "Keep both";
*)
               aux (x::lhs) tail (* keep element *)

             | BTYP_tuple [], _ ->
(*
print_endline "Discard x";
*)
               aux lhs tail (* discard greater element *)

             | _,BTYP_tuple [] ->
(*
print_endline "Discard r";
*)
               lhs @ rhs (* keep whole list, discard c *)

             | _,_ ->
               aux (x::lhs) tail (* keep element *)
             end

           | `Greater ->
(*
             print_endline "Candidate is less general: discard this element";
*)
             aux lhs tail (* discard greater element *)
           | `Incomparable ->
(*
             print_endline "Candidate is incomparable, retain element";
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

      print_endline ("virtual " ^ id ^ "<" ^ si i ^ ">["^ catmap "," (sbt bsym_table) ts ^ "] Too many matches:");
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
       print_endline ("  defined: " ^ Flx_srcref.long_string_of_src (Flx_bsym.sr bsym))
      end candidates;

      clierrx "[flx_frontend/flx_typeclass.ml:741: E366] " sr "No most specialised instance!"

let id x = x

let tcsubst syms bsym_table sr i ts =
  match fixup_typeclass_instance' syms bsym_table sr i ts with
  | `NonVirtual,i,ts->i,ts
  | `MatchesNow,i,ts->i,ts
  | `CannotMatch, i,ts ->
     let bsym = Flx_bsym_table.find bsym_table i in
     let id = Flx_bsym.id bsym in
     clierrx "[flx_frontend/flx_typeclass.ml:752: E367] " sr ("[Cannotmatch] Cannot instantiate virtual " ^ id ^ "<" ^ si i ^ ">[" ^catmap "," (sbt bsym_table) ts^ "]")
  | `MaybeMatchesLater, i,ts ->
     let bsym = Flx_bsym_table.find bsym_table i in
     let id = Flx_bsym.id bsym in
     clierrx "[flx_frontend/flx_typeclass.ml:756: E368] " sr ("[MaybeMatchesLater] Cannot instantiate virtual " ^ id ^ "<" ^ si i ^ ">[" ^catmap "," (sbt bsym_table) ts^ "]")


let fixup_typeclass_instance syms bsym_table sr i ts =
  tcsubst syms bsym_table sr i ts


