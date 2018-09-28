open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bbdcl
open Flx_bid
open Flx_bparameter

module CS = Flx_code_spec
let catmap x = Flx_util.catmap x
let sbt x = Flx_print.sbt x

(* Top level monomorphisation routine for symbol definitions.
Produces a completely new defintion for a polymorphic symbol 
specialised for the binding of the type variables in
the ts.

The output is fully monomorphic and contains references ONLY
to the new symbol table.
*)

let mono_fun syms bsym_table virtualinst polyinst  mt bsym sr ts 
  (props,vs,(ps,traint),ret,effects,exes) 
=
begin try
  let props = List.filter (fun p -> p <> `Virtual) props in
  if List.length vs <> List.length ts then begin try
    print_endline ("[mono] vs/ts mismatch in " ^ Flx_bsym.id bsym ^ " vs=[" ^ 
      catmap "," (fun (s,i,_) -> s) vs ^ "]");
    print_endline ("ts=[" ^ catmap "," (sbt bsym_table) ts ^ "]");
    assert false
    with Not_found -> print_endline "Not_found printint ts?"; assert false
  end;
  let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
  let ret = 
    try mt vars ret 
    with Not_found -> print_endline "Not_found fixing up return type"; 
      print_endline ("Ret=" ^ sbt bsym_table ret); 
      assert false 
  in
  let effects = 
    try mt vars effects
    with Not_found -> print_endline "Not_found fixing up effects type"; 
      print_endline ("Effects=" ^ sbt bsym_table effects); 
      assert false 
  in
  let ps = try
    (*
    print_endline ("+++processing parameters of " ^ Flx_bsym.id bsym);
    *)
    let ps = Flx_bparams.xpmap (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
    {pkind=pk;pid=s;pindex=fst (polyinst sr i ts);ptyp=mt vars t}) ps in
    (*
    print_endline ("+++parameters processed: " ^ Flx_bsym.id bsym);
    *)
    ps
    with Not_found -> print_endline ("Not Found FIXING parameters"); assert false
  in
  (* fudge unit parameters *)
  let ps = Flx_bparams.xpmap (fun {pkind=pk; pid=s; pindex=i; ptyp=t} ->
    {pkind=pk;pid=s;pindex=(match t with BTYP_tuple [] -> 0 | _ -> i);ptyp=t}) ps 
  in
  let traint =
    match traint with
    | None -> None
    | Some x -> Some (Flx_monofixup_base.fixup_expr syms bsym_table (mt vars) virtualinst polyinst sr x)
  in
  let exes = Flx_monostrip.strip_empty_calls bsym_table exes in
  let exes = 
    try Flx_monofixup_base.fixup_exes syms bsym_table vars virtualinst polyinst ts exes 
    with Not_found -> assert false
  in
  let exes = Flx_monostrip.strip_unit_assigns exes in
  let exes = List.map (fun exe -> Flx_bexe.map ~f_bexpr:Flx_bexpr.reduce exe) exes in
  let props = List.filter (fun p -> p <> `Virtual) props in
  Some (bbdcl_fun (props,[],(ps,traint),ret,effects,exes))
with Not_found ->
  assert false
end

let mono_union syms bsym_table virtualinst polyinst  mt bsym sr i ts (vs,cps) =
(*
    if List.length vs <> List.length ts then begin
      print_endline ("Monomorphise union " ^ sbt bsym_table (btyp_inst (i,ts)) ^ 
      " with ts/vs mismatch, expected " ^ string_of_int (List.length vs) ^
      " type variables to match " ^ string_of_int (List.length ts) ^ " arguments");
      print_endline "Probably a GADT?";
    end;
*)
    List.iter (fun t -> if not (Flx_btype.complete_type  t) then 
    print_endline ("type variable substitution type is not complete " ^ 
      sbt bsym_table t)) ts;
    let gadt = List.fold_left (fun acc (name,index,evs,d,c,gadt) -> 
       gadt || acc) false cps
    in
    let ut = btyp_inst (i,ts,Flx_kind.KIND_type) in
if gadt then
begin
(*
print_endline ("Monomorphising union " ^ sbt bsym_table ut);
print_endline ("  Polymorphic union index = " ^ string_of_int i);
print_endline ("  Union universal type variable instances = " ^ catmap "," (fun t -> sbt bsym_table t) ts);
print_endline ("  Target monomorphic index = " ^ string_of_int j);
*)
    let try_cal_ctor (name,index,evs,d,c,gadt) =
(*
print_endline ("    Examining constructor " ^ name ^ "<" ^ string_of_int index ^">[" ^
  catmap "," (fun (s,k) -> s^"<"^string_of_int k^">") evs ^ "] of " ^ 
  sbt bsym_table d ^ " => " ^ sbt bsym_table c);
*)

      let dvars = ref BidSet.empty in
      List.iter (fun (_,i,_) -> dvars := BidSet.add i (!dvars)) vs;
      List.iter (fun (_,i,_) -> dvars := BidSet.add i (!dvars)) evs;
      let maybe_mgu = 
        let eqns = [c,ut] in
(*
print_endline ("Attempting unification: " ^ sbt bsym_table c ^ " =? " ^ sbt bsym_table ut);
print_endline ("Dependent variables:"); 
  BidSet.iter (fun i -> print_endline ("DVAR=" ^ string_of_int i)) (!dvars);
*)
        try Some (Flx_unify.unification bsym_table syms.Flx_mtypes2.counter eqns !dvars)
        with 
          | Free_fixpoint _ -> print_endline ("Free fixpoint"); None 
          | Not_found -> None
      in
      match maybe_mgu with
      | None -> 
(*
        print_endline ("      Unification FAILED"); 
*)
        raise Flx_exceptions.GadtUnificationFailure
      | Some mgu ->
(*
        print_endline ("      Unified with MGU=" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) mgu);
*)
        let mgu = List.map (fun (i,t) -> i, Flx_fold.minimise bsym_table syms.Flx_mtypes2.counter t) mgu in
(*
        print_endline ("      Miniused MGU   =" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) mgu);
*)
(* NOTE: for non GADT, this should agree with the argument var -> ts binding!! *)
(*
    if (List.length vs = List.length ts) then begin
      let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
        print_endline ("      ORIGINAL ASSIGN=" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) vars);
    end else begin
       print_endline  ("      ORIGINAL ASSIGN WOULD FAIL TO MONOMORPHISE (ts/vs mismatch due to GADT)");    
    end;
*)
(*
        let varmap = Hashtbl.create 3 in
        List.iter (fun (j,t) -> Hashtbl.add varmap j t) mgu;
        let d = Flx_unify.varmap_subst varmap d in
        print_endline ("      ctor domain (poly) = " ^ sbt bsym_table d);
        let c = Flx_unify.varmap_subst varmap c in
*)
(*
        let d = unfold "union monomorphisation" d in
        print_endline ("      ctor domain (unfolded) = " ^ sbt bsym_table d);
*)
(*
        let d = Flx_unify.list_subst syms.counter mgu d in
*)
        let d = mt mgu d in
(*
        print_endline ("      ctor domain (mono) = " ^ sbt bsym_table d);
*)
        name,index,[],d,(btyp_void ()),gadt
    in
    let cal_ctor x = try Some (try_cal_ctor x) 
      with Flx_exceptions.GadtUnificationFailure -> None 
    in
    let cps = List.rev_map cal_ctor cps in
    let cps = List.fold_left (fun acc x -> 
      match x with Some x -> x::acc | None -> acc) [] cps
    in
(*
print_endline ("Finished union by GADT **");
*)
    Some (bbdcl_union ([], cps))
end else begin
  if (List.length vs <> List.length ts) then
    print_endline ("Union "^sbt bsym_table ut ^ " vs length " ^ string_of_int (List.length vs) ^ 
    " doesn't agree with ts length " ^ string_of_int (List.length ts));
  assert (List.length vs = List.length ts);
(*
       print_endline ("******* Union "^sbt bsym_table ut);
*)
(*
    if Flx_unify.is_recursive_type ut then
       print_endline ("Union "^sbt bsym_table ut ^" is recursive");
    List.iter (fun t -> if not (Flx_btype.complete_type  t) then 
    print_endline ("non-gadt: Union: "^sbt bsym_table (btyp_inst (i,ts))^ ",type variable substitution type is not complete " ^ 
      sbt bsym_table t)) ts;
*)
(* THIS IS A HACK, it only works with fix-1, i.e. an argument
which is precisely a pointer to the union type. We need to fix this
so if the recursion is more deeply embedded it also works
*)
(*
  let ut' = 
    try 
      unfold "unfold recursive union in numono" ut 
    with 
    | _ -> print_endline "Unfold union failed!"; assert false
  in
  let ts' = match ut' with
    | BTYP_inst (_, ts') -> ts'
    | _ -> assert false
  in
*)
(*
    if Flx_unify.is_recursive_type ut then
    print_endline ("Unfolded union = " ^ sbt bsym_table ut');
*)
(*
    let ts = List.map (fun t -> match t with 
    | BTYP_fix (-1,_) -> ut
    | t -> t) ts
    in
*)
(*
  let vars = List.map2 (fun (s,i) t -> i,t) vs ts' in
*)
  let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in


(*
if Flx_unify.is_recursive_type ut then
begin
  print_endline ("Recursive Union type " ^ sbt bsym_table ut ^
  " should be replaced by assigned monomorphic type " ^ sbt bsym_table (mt vars ut))
end;
*)
  let cps = List.map (fun (name,index,ivs,argt, resultt,gadt) -> 
(*
if Flx_unify.is_recursive_type ut then begin
  print_endline ("Recursive Union type " ^ sbt bsym_table ut ^
  " constructor " ^ name ^ ": argument type " ^ sbt bsym_table argt ^ 
  " will be replaced by monomorphic type " ^ sbt bsym_table (mt vars argt))
end;
*)
    name,index, [],mt vars argt, btyp_none (),gadt 
    ) cps 
  in
(*
print_endline ("Finished union by non GADT **");
*)
  Some (bbdcl_union ([], cps))
end


(* the j is for diagnostics only, it's the output symbol index *)
let mono_bbdcl syms bsym_table processed to_process nubids virtualinst polyinst ts bsym i j =
(*
if List.length ts > 0 then begin
  print_endline ("[mono_bbdcl] " ^ Flx_bsym.id bsym);
  print_endline ("ts=[" ^ catmap "," (sbt bsym_table) ts ^ "]");
end;
*)
  List.iter (fun t -> if not (complete_type t) then 
    print_endline ("Argument not complete!!!!!!!!!!!!!!!!!!!!!!!")
  )
  ts;
  let sr = Flx_srcref.make_dummy ("[mono_bbdcl] " ^ Flx_bsym.id bsym) in 
  begin try List.iter (Flx_monocheck.check_mono bsym_table sr) ts with _ -> assert false end;

(*
  let original_instance_type = BTYP_inst (i,ts) in
  let unfolded_instance_type = 
    try 
      unfold "unfold instance in numono" original_instance_type
    with 
    | _ -> 
      print_endline ("Unfold instance failed! " ^ sbt bsym_table original_instance_type); 
      assert false
  in
  let ts' = match unfolded_instance_type with
    | BTYP_inst (_, ts') -> ts'
    | _ -> assert false
  in
*)

  let mt vars t = Flx_monofixup_base.fixup_type syms bsym_table vars bsym virtualinst polyinst sr t in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_nominal_type_alias _ -> assert false
  | BBDCL_structural_type_alias _ -> assert false
  | BBDCL_label s -> Some (bbdcl_label s)

  | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) -> 
    mono_fun syms bsym_table virtualinst polyinst mt bsym sr ts (props,vs,(ps,traint),ret,effects,exes) 

  | BBDCL_val (vs,t,kind) ->
(*
print_endline ("Monomorphising variable "^Flx_bsym.id bsym ^" polytype " ^ sbt bsym_table t);
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let t = mt vars t in
    (* eliminate unit variables *)
    begin match t with
    | BTYP_void -> print_endline ("Void variable?"); assert false
    | BTYP_tuple [] -> 
      (* print_endline ("Elim unit var " ^ Flx_bsym.id bsym ^ " old index " ^ si i ^ " new index would be " ^ si j); i*)
      None
    | _ -> Some (bbdcl_val ([],t,kind))
    end

  (* we have tp replace types in interfaces like Vector[int]
    with monomorphic versions if any .. even if we don't
    monomorphise the bbdcl itself.

    This is weak .. it's redone for each instance, relies
    on mt being idempotent..
  *)
  | BBDCL_external_fun (props,vs,argtypes,ret,reqs,prec, fkind) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let argtypes = List.map (mt vars) argtypes in
    let ret = mt vars ret in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    let props = List.filter (fun p -> p <> `Virtual) props in
    Some (bbdcl_external_fun (props,vs,argtypes,ret,reqs,prec,fkind))

  | BBDCL_external_const (props, vs, t, CS.Str "#this", reqs) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let _ = mt vars t in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_const (props, [], t, CS.Str "#this", reqs))

  | BBDCL_external_const (props, vs, t,cs, reqs) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let t = mt vars t in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_const (props,vs, t, cs, reqs))
 
  | BBDCL_external_type (vs,quals,cs,reqs)  -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    let quals = List.map (Flx_monofixup_base.fixup_qual vars mt) quals in
    Some (bbdcl_external_type (vs,quals,cs, reqs))

  | BBDCL_external_code (vs,cs,ikind,reqs)   -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_code (vs,cs,ikind,reqs))

  | BBDCL_union (vs,cps) ->
    mono_union syms bsym_table virtualinst polyinst  mt bsym sr i ts (vs,cps)  

  | BBDCL_cstruct (vs,cps, reqs) -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let cps = List.map (fun (name,argt) -> name,mt vars argt) cps in
    let reqs = Flx_monofixup_base.fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_cstruct ([], cps, reqs))

  | BBDCL_struct (vs,cps)  -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let cps = List.map (fun (name,argt) -> name,mt vars argt) cps in
    Some (bbdcl_struct ([], cps))


  | BBDCL_const_ctor (vs,uidx,ut,ctor_idx,evs,etraint) ->
(*
print_endline "Monomorphising constant constructor?";
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let ut = mt vars ut in
    let uidx = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids uidx ts in
    Some (bbdcl_const_ctor ([],uidx,ut,ctor_idx,evs,etraint)) (* ignore GADT stuff *)
 
  | BBDCL_nonconst_ctor (vs,uidx,ut,ctor_idx,ctor_argt,evs,etraint) ->
    assert (List.length vs = List.length ts);
(*
print_endline ("Monomorphising nonconst ctor argt=: " ^ sbt bsym_table ctor_argt ^ " => " ^
   sbt bsym_table ut);
*)
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let ut = mt vars ut in
    let uidx = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids uidx ts in
    let ctor_argt = mt vars ctor_argt in
(*
print_endline ("Monomorphised nonconst ctor argt=: " ^ sbt bsym_table ctor_argt ^ " => " ^
   sbt bsym_table ut);
*)
    Some (bbdcl_nonconst_ctor ([],uidx, ut,ctor_idx,ctor_argt,evs,etraint)) (* ignore GADT stuff *)
 
  | BBDCL_newtype (vs,t) ->  
(*
print_endline ("ADJUSTING NEWTYPE " ^Flx_bsym.id bsym );
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i,_) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_newtype ([],t))

  | BBDCL_virtual_type _ 
  | BBDCL_typeclass _ 
  | BBDCL_instance _ 
  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce 
  | BBDCL_invalid 
  | BBDCL_instance_type _ 
  | BBDCL_module 
    -> assert false



