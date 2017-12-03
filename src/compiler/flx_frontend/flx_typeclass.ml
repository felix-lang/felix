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
open Flx_bid
open Flx_btype_subst
open Flx_kind

module CS = Flx_code_spec
let drop l n = Flx_list.list_tail l n

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


let tcinst_chk syms bsym_table id sr i ts 
  ((inst_vs:Flx_kind.bvs_t), inst_constraint, inst_ts, j) 
 =
     if length inst_ts > length ts then
       clierrx "[flx_frontend/flx_typeclass.ml:409: E365] " sr (
         "Not enough ts given, expected at least " ^
         si (length inst_ts) ^ ", got " ^ si (length ts)
       )
     ;
     (* solve for vs' *)
     let vis = List.map (fun _ -> fresh_bid syms.counter) inst_vs in
     let nuvs = map (fun i -> 
(*
if i = 7141 then
print_endline ("Flx_typeclass: bind_type var, HACKED KIND_type");
*)
       btyp_type_var (i, Flx_kind.KIND_type)) vis 
     in
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
         | BTYP_type_var (x,KIND_type), other
         | other, BTYP_type_var (x,KIND_type) -> (x,other) :: assigns, rest

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
     let eqns = map (fun (l,r) -> list_subst syms.counter assigns l, list_subst syms.counter assigns r) rest in
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
     let assignments = map (fun (i,t) -> 
if i = 7141 then
print_endline ("Flx_typeclass: HACKED assignments ");
        btyp_type_var (i,Flx_kind.KIND_type),t) assigns 
     in
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
         let goback = combine vis (map (fun (_,i,_)->i) inst_vs) in
         map (fun (i,t) -> assoc i goback, t) mgu
       in
       let tsv =
         map
         (fun (s,i,_) ->
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
          catmap "," (fun (s,i,mt) -> s ^ "<" ^ string_of_bid i ^ ">:" ^ Flx_kind.sk mt) inst_vs);
       print_endline ("  defined: " ^ Flx_srcref.long_string_of_src (Flx_bsym.sr bsym))
      end candidates;

      clierrx "[flx_frontend/flx_typeclass.ml:741: E366] " sr "No most specialised instance!"

let id x = x

let tcsubst syms bsym_table sr i ts =
  let ts = List.map (Flx_remap_vtypes.remap_virtual_types syms bsym_table) ts in
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


