open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open Flx_srcref
open List
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_beta

let dummy_sr = "[typeclass] generated",0,0,0,0

let vs2ts vs = map (fun (s,i) -> `BTYP_var (i,`BTYP_type 0)) vs

(* drop first n elements of list l *)
let rec drop l n =
  if n = 0 then l else drop (tl l) (n-1)

let check_instance syms (bbdfns:fully_bound_symbol_table_t) (child_map:child_map_t)
  (inst:int) (inst_id: string) inst_vs inst_constraint inst_sr inst_props tc inst_ts
=
  let tc_id, _, tc_sr, tc_entry = Hashtbl.find bbdfns tc in
  match tc_entry with
  | `BBDCL_typeclass (tc_props, tc_bvs) ->
    (*
    print_endline ("Found " ^ inst_id ^ "<"^si inst ^ ">" ^
    "[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]" ^
    " to be instance of typeclass " ^ tc_id ^ "<"^si tc^">")
    ;
    print_endline ("Typeclass vs = " ^
      catmap "," (fun (s,j) -> s^"<"^si j^">") tc_bvs
    );
    *)
    if length tc_bvs <> length inst_ts then
      clierr2 inst_sr tc_sr
      (
        "Instance " ^
        "["^catmap "," (fun (s,j) -> s^"<"^si j^">") inst_vs^"] " ^
        inst_id ^"<"^ si inst^">"^
        "[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]" ^
        "\nsupplies wrong number of type arguments for typeclass parameters\n" ^
        inst_id^"["^catmap "," (fun (s,j) -> s^"<"^si j^">") tc_bvs^"]"
      )
    ;

    let tc_kids = try Hashtbl.find child_map tc with Not_found -> [] in
    let inst_kids = try Hashtbl.find child_map inst with Not_found -> [] in
    (*
    print_endline ("Typeclass kids " ^ catmap "," si tc_kids);
    print_endline ("Instance kids " ^ catmap "," si inst_kids);
    *)
    let inst_map = fold_left (fun acc i->
      let id,_,_,entry = Hashtbl.find bbdfns i in
      match entry with
      | `BBDCL_fun (_,bvs,params,ret,_,_,_) ->
        let argt  : btypecode_t= typeoflist params in
        let qt = bvs,`BTYP_function (argt,ret) in
        (id,(i,qt)) :: acc

      | `BBDCL_proc (_,bvs,params,_,_) ->
        let argt  : btypecode_t= typeoflist params in
        let qt = bvs,`BTYP_function (argt,`BTYP_void) in
        (id,(i,qt)) :: acc

      | `BBDCL_procedure (_,bvs,bps,_) ->
        let argt : btypecode_t = typeoflist (typeofbps_traint bps) in
        let qt = bvs,`BTYP_function (argt,`BTYP_void) in
        (id,(i,qt)) :: acc

      | `BBDCL_function (_,bvs,bps,ret,_) ->
        let argt : btypecode_t = typeoflist (typeofbps_traint bps) in
        let qt = bvs,`BTYP_function (argt,ret) in
        (id,(i,qt)) :: acc

      | `BBDCL_const (_,bvs,ret,_,_) ->
        let qt = bvs,ret in
        (id,(i,qt)) :: acc

      | `BBDCL_val (bvs,ret) ->
        let qt = bvs,ret in
        (id,(i,qt)) :: acc


      | _ -> acc
      ) [] inst_kids
    in
    let check_binding force tck sr id tck_bvs tctype =
      let sigmatch i inst_funbvs t =
        let t = reduce_type t in
        let tc_ptv = length tck_bvs - length tc_bvs in
        let inst_ptv = length inst_funbvs - length inst_vs in
        if inst_ptv <> tc_ptv then false else
        let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (length inst_vs)) in
        assert (length tck_bvs = length inst_funts);
        let tct = reduce_type (beta_reduce syms sr (tsubst tck_bvs inst_funts tctype)) in
        let matches =  tct = t in
        matches
      in
      let entries = filter (fun (name,(i,(inst_funbvs,t))) -> name = id && sigmatch i inst_funbvs t) inst_map in
      match entries with
      | [] ->
         if force then
         clierr2 sr inst_sr ("Cannot find typeclass "^inst_id^" virtual " ^ id ^ " in instance ["^catmap "," (sbt syms.dfns) inst_ts ^"]")

      | [_,(i,(inst_funbvs,t))] ->
        let t = reduce_type t in
        (*
        print_endline ("Typeclass " ^ tc_id ^ "<" ^ si tc ^">" ^ print_bvs tc_bvs);
        print_endline ("Typeclass function " ^ id ^ "<" ^ si tck ^ ">" ^
          print_bvs tck_bvs ^ ":" ^ sbt syms.dfns tctype
        );

        print_endline ("Instance vs = " ^ print_bvs inst_vs);
        print_endline ("Instance ts = " ^ catmap "," (sbt syms.dfns) inst_ts);
        print_endline ("Instance function " ^ id ^ "<"^si i^">" ^ print_bvs inst_funbvs ^
        ":" ^ sbt syms.dfns t);
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

        let tct = reduce_type (beta_reduce syms sr (tsubst tck_bvs inst_funts tctype)) in
        (*
        print_endline ("Typeclass function (instantiated) " ^ id ^ "<" ^ si tck ^ ">" ^
          ":" ^ sbt syms.dfns tct
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
            print_bvs tck_bvs ^ ":" ^ sbt syms.dfns tctype
          );

          print_endline ("Instance vs = " ^ print_bvs inst_vs);
          print_endline ("Instance ts = " ^ catmap "," (sbt syms.dfns) inst_ts);
          print_endline ("Instance function " ^ id ^ "<"^si i^">" ^ print_bvs inst_funbvs ^
          ":" ^ sbt syms.dfns t);
          print_endline ("Typeclass function (instantiated) " ^ id ^ "<" ^ si tck ^ ">" ^
            ":" ^ sbt syms.dfns tct
          );
        end
        ;
        *)
        let old =
          try Hashtbl.find syms.typeclass_to_instance tck
          with Not_found -> []
        in
        let entry = inst_vs , inst_constraint, inst_ts , i in
        if mem entry old then
          clierr sr "Instance already registered??"
        else
          Hashtbl.replace syms.typeclass_to_instance tck (entry :: old);

        (*
        print_endline ("Register mapping " ^ si tck ^ " vs=" ^
          print_bvs inst_vs ^
          " constraint=(" ^ sbt syms.dfns inst_constraint ^
          ") ts=[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "] -----> " ^ si i
        );
        *)

      | _ ->
        clierr sr ("Felix can't handle overloads in typeclass instances yet, " ^ id ^ " is overloaded")
    in
    iter
    (fun tck ->
      let tckid,tckparent,tcksr,tckentry = Hashtbl.find bbdfns tck in
      match tckentry with
      | `BBDCL_fun (props,bvs,params,ret,ct,breq,prec) ->
        if ct == `Virtual then
          let ft = `BTYP_function (typeoflist params,ret) in
          check_binding true tck tcksr tckid bvs ft
        (*
        clierr tcksr "Typeclass requires virtual function";
        *)

      | `BBDCL_proc (props,bvs,params,ct,breq) ->
        if ct == `Virtual then
          let ft = `BTYP_function (typeoflist params,`BTYP_void) in
          check_binding true tck tcksr tckid bvs ft
        (*
        clierr tcksr "Typeclass requires virtual procedure";
        *)

      | `BBDCL_function (props,bvs,bps,ret,_) when mem `Virtual props ->
        let argt : btypecode_t = typeoflist (typeofbps_traint bps) in
        let ft = `BTYP_function (argt,ret) in
        check_binding false tck tcksr tckid bvs ft

      | `BBDCL_procedure (props, bvs, bps,_) when mem `Virtual props ->
        let argt : btypecode_t = typeoflist (typeofbps_traint bps) in
        let ft = `BTYP_function (argt,`BTYP_void) in
        check_binding false tck tcksr tckid bvs ft

      | `BBDCL_const (props,bvs,ret,_,_) when mem `Virtual props ->
        check_binding false tck tcksr tckid bvs ret


      | `BBDCL_insert _ -> ()
      | _ ->
        (*
        clierr tcksr "Typeclass entry must be virtual function or procedure"
        *)
        print_endline ("Warning: typeclass "^tc_id ^" entry " ^ tckid ^ " is not virtual");
    )
    tc_kids


  | _ ->
    clierr2 inst_sr tc_sr ("Expected " ^ inst_id ^ "<"^si inst ^ ">" ^
    "[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]" ^
    " to be typeclass instance, but" ^ tc_id ^ "<"^si tc^">, " ^
    "is not a typeclass"
    )

let typeclass_instance_check syms bbdfns child_map =
Hashtbl.iter
(fun i (id,_,sr,entry) -> match entry with
  | `BBDCL_instance (props, vs, cons, tc, ts) ->
     let iss =
       try Hashtbl.find syms.instances_of_typeclass tc
       with Not_found -> []
     in
     let entry = i,(vs,cons,ts) in
     Hashtbl.replace syms.instances_of_typeclass tc (entry::iss);
     check_instance syms bbdfns child_map i id vs cons sr props tc ts

  | _ -> ()
)
bbdfns

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


let tcinst_chk syms allow_fail i ts sr (inst_vs, inst_constraint, inst_ts, j)  =
     (*
     print_endline
     ("virtual " ^ si i ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
     if length inst_ts > length ts then
       failwith (
         "Not enough ts given, expected at least " ^
         si (length inst_ts) ^ ", got " ^ si (length ts)
       )
     ;
     *)
     (* solve for vs' *)
     let v0 = !(syms.counter) in
     let n = length inst_vs in
     let vis =  (* list of ints from v0 to v0+n-1 *)
       let rec aux i o = match i with
       | 0 -> o
       | _ -> aux (i-1) ((v0+i-1)::o)
       in aux n []
     in
     let nuvs = map (fun i -> `BTYP_var (i,`BTYP_type 0)) vis in
     let inst_ts' = map (tsubst inst_vs nuvs) inst_ts in
     let vset = fold_left (fun acc i -> IntSet.add i acc) IntSet.empty vis in

     (*
     let vset = fold_left (fun acc (_,i) -> IntSet.add i acc) IntSet.empty inst_vs' in
     *)
     let eqns = combine (list_prefix ts (length inst_ts)) inst_ts' in
     (*
     print_endline ("Solving equations\n " ^
       catmap "\n" (fun (a,b) -> sbt syms.dfns a ^ " = " ^ sbt syms.dfns b ) eqns
     );
     *)
     let mgu =
       try Some (unification false syms.counter syms.dfns eqns vset)
       with Not_found -> None
     in
     begin match mgu with
     | None -> None
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
             print_endline ("Solved " ^ s ^"<"^si i^">" ^ "-> " ^ sbt syms.dfns (assoc i mgu));
             *)
             assoc i mgu
           )
         )
         inst_vs
       in
       (*
       print_endline ("instance constraint: " ^ sbt syms.dfns inst_constraint);
       *)
       let con = list_subst syms.counter mgu inst_constraint in
       let con = reduce_type (Flx_beta.beta_reduce syms sr con) in
       match con with
       | `BTYP_tuple [] ->
         let tail = drop ts (length inst_ts) in
         let ts = tsv @ tail in
         (*
         print_endline ("Remap to " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
         *)
         Some (j,ts)
       | `BTYP_void -> (* print_endline "constraint reduce failure"; *) None
       | _ ->
         if not allow_fail then
         failwith ("Unable to reduce type constraint: " ^ sbt syms.dfns con)
         else
         (
           (*
           print_endline ("Unable to reduce type constraint! " ^ sbt syms.dfns con);
           *)
           None
         )
     end


let fixup_typeclass_instance' syms bbdcls allow_fail i ts =
  let entries =
    try Hashtbl.find syms.typeclass_to_instance i
    with Not_found -> (* print_endline ("Symbol " ^ si i ^ " Not instantiated?"); *) []
  in
  let sr =
     try match Hashtbl.find syms.dfns i with {sr=sr} -> sr
     with Not_found -> dummy_sr
  in
  let entries = fold_left (fun acc x -> match tcinst_chk syms allow_fail i ts sr x with
     | None -> acc
     | Some jts -> (jts,x)::acc
     ) [] entries
  in
  match entries with
  | [] -> i,ts
  | [(j,ts),_] ->
     (*
     print_endline ("Found instance " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
     *)
     j,ts

  | candidates ->
    let id,parent,sr,entry =
       try Hashtbl.find bbdcls i
       with Not_found -> failwith ("Woops can't find virtual function index "  ^ si i)
    in
    (*
    print_endline
    ("Unimplemented: Multiple matching instances for typeclass virtual instance\n"
     ^id^"<"^ si i^">["^ catmap "," (sbt syms.dfns) ts ^"]"
    )
    ;
    iter
    (fun ((j,ts),(inst_vs,con,inst_ts,k)) ->
       let id,parent,sr,entry =
         try Hashtbl.find bbdcls j
         with Not_found -> failwith ("Woops can't find instance function index "  ^ si j)
       in
       let parent = match parent with Some k -> k | None -> assert false in
       print_endline ("Function " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
       print_endline (" instance parent " ^ si parent ^ "[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]");
       print_endline (" instance vs= " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") inst_vs );
    )
    candidates
    ;
    *)
    let candidates = fold_left
    (fun oc (((j,ts),(inst_vs,con,inst_ts,k)) as r) ->
       let c = `BTYP_type_tuple inst_ts in
       (*
       print_endline ("Considering candidate sig " ^ sbt syms.dfns c);
       *)
       let rec aux lhs rhs =
         match rhs with
         | [] ->
           (*
           print_endline "return elements plus candidate";
           *)
           r::lhs (* return all non-greater elements plus candidate *)
         | (((j,ts),(inst_vs,con,inst_ts,k)) as x)::tail ->
           let c' = `BTYP_type_tuple inst_ts in
           (*
           print_endline (" .. comparing with " ^ sbt syms.dfns c');
           *)
           begin match compare_sigs syms.counter syms.dfns c' c with
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
    | [] -> i,ts
    | [(j,ts),(inst_vs,con,inst_ts,k)] ->
       (*
       print_endline ("Found most specialised instance " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
       print_endline (" instance [" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]");
       *)
       j,ts

    | candidates ->
      iter
      (fun ((j,ts),(inst_vs,con,inst_ts,k)) ->
         let id,parent,sr,entry =
           try Hashtbl.find bbdcls j
           with Not_found -> failwith ("Woops can't find instance function index "  ^ si j)
         in
         let parent = match parent with Some k -> k | None -> assert false in
         print_endline ("Function " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]");
         print_endline (" instance parent " ^ si parent ^ "[" ^ catmap "," (sbt syms.dfns) inst_ts ^ "]");
         print_endline (" instance vs= " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") inst_vs );
      )
      candidates
      ;
      clierr sr "No most specialised instance!"

let id x = x

let fixup_expr syms bbdfns e =
  (*
  print_endline ("Check expr " ^ sbe syms.dfns e);
  *)
  let rec aux e =  match map_tbexpr id aux id e with
  | `BEXPR_apply_direct (i,ts,a),t ->
    let a = aux a in
    let j,ts = (* print_endline ("Check apply direct " ^ si i);  *)
      fixup_typeclass_instance' syms bbdfns true i ts in
    (*
    if j <> i then print_endline ("[direct] instantiate virtual as " ^ si j);
    *)
    `BEXPR_apply_direct (j,ts,a),t

  | `BEXPR_apply_prim (i,ts,a),t ->
    let a = aux a in
    let j,ts = (* print_endline ("Check apply prim " ^ si i^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]"); *)
      fixup_typeclass_instance' syms bbdfns true i ts in
    (*
    if j <> i then
      print_endline ("[prim] instantiate virtual as " ^
        si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]"
      )
    ;
    *)
    `BEXPR_apply_direct (j,ts,a),t

  | `BEXPR_name (i,ts),t ->
    let j,ts = (* print_endline ("Check apply prim " ^ si i^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]"); *)
      fixup_typeclass_instance' syms bbdfns true i ts in
    `BEXPR_name (j,ts),t

  | x -> x
  in aux e

let fixup_exe syms bbdfns exe = match exe with
  | `BEXE_call_direct (sr,i,ts,a) ->
    let j,ts = fixup_typeclass_instance' syms bbdfns true i ts in
    (*
    if j <> i then print_endline "instantiate virtual ..";
    *)
    let a  = fixup_expr syms bbdfns a in
    `BEXE_call_direct (sr,j,ts,a)
  | x ->
    map_bexe id (fixup_expr syms bbdfns) id id id x

let fixup_exes syms bbdfns exes = map (fixup_exe syms bbdfns) exes

let fixup_typeclass_instances syms bbdfns =
  Hashtbl.iter (fun i (id,parent,sr,entry) -> match entry with
  | `BBDCL_function (props,bvs,bps,ret,exes) ->
    let exes = fixup_exes syms bbdfns exes in
    let entry = `BBDCL_function (props, bvs, bps, ret, exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)
  | `BBDCL_procedure (props, bvs, bps,exes)  ->
    let exes = fixup_exes syms bbdfns exes in
    let entry = `BBDCL_procedure (props, bvs, bps,exes) in
    Hashtbl.replace bbdfns i (id,parent,sr,entry)
  | _ -> ()
  )
  bbdfns

(* this routine doesn't allow constraint reduction failure
  and should only be run at instantiation time
*)
let fixup_typeclass_instance syms bbdcls i ts =
  fixup_typeclass_instance' syms bbdcls false i ts

(* this routine allows failure, only use for early
  instantiation for optimisation
*)
let maybe_fixup_typeclass_instance syms bbdcls i ts =
  fixup_typeclass_instance' syms bbdcls true i ts
