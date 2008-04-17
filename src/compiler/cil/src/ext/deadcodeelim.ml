(* Eliminate assignment instructions whose results are not
   used *)

open Cil
open Pretty

module E = Errormsg
module RD = Reachingdefs
module UD = Usedef
module IH = Inthash
module S = Stats

module IS = Set.Make(
  struct
    type t = int
    let compare = compare
  end)

let debug = RD.debug

let doTime = ref false

let time s f a =
  if !doTime then
    S.time s f a
  else f a

(* This function should be set by the client if it
 * knows of functions returning a result that have
 * no side effects. If the result is not used, then
 * the call will be eliminated. *)
let callHasNoSideEffects : (instr -> bool) ref = 
  ref (fun _ -> false)


(* the set of used definition ids *)
let usedDefsSet = ref IS.empty

(* a mapping d -> {u_1,...,u_n} where d is a
 * definition id, and the u's are definition
 * ids corresponding to definitions in which
 * d was used *)
let defUseSetHash = IH.create 100

(* a mapping d -> {sid_1,...,sid_n} where d is
 * a definition id and the sids are statement ids
 * corresponding to non-Instr statements where d
 * was used *)
let sidUseSetHash = IH.create 100

(* put used def ids into usedDefsSet *)
(* assumes reaching definitions have already been computed *)
class usedDefsCollectorClass = object(self)
    inherit RD.rdVisitorClass as super

  method add_defids iosh e u =
    UD.VS.iter (fun vi ->
      if IH.mem iosh vi.vid then 
	let ios = IH.find iosh vi.vid in
	if !debug then ignore(E.log "DCE: IOS size for vname=%s at stmt=%d: %d\n" 
				vi.vname sid (RD.IOS.cardinal ios));
	RD.IOS.iter (function
	    Some(i) -> 
	      if !debug then ignore(E.log "DCE: def %d used: %a\n" i d_plainexp e);
	      usedDefsSet := IS.add i (!usedDefsSet)
	  | None -> ()) ios
      else if !debug then ignore(E.log "DCE: vid %d:%s not in stm:%d iosh at %a\n"
				   vi.vid vi.vname sid d_plainexp e)) u

  method vexpr e =
    let u = UD.computeUseExp e in
    match self#get_cur_iosh() with
      Some(iosh) -> self#add_defids iosh e u; DoChildren
    | None ->
	if !debug then ignore(E.log "DCE: use but no rd data: %a\n" d_plainexp e);
	DoChildren

  method vstmt s =
    ignore(super#vstmt s);
    match s.skind with
    | Instr _ -> DoChildren
    | _ -> begin
	let u,d = UD.computeUseDefStmtKind s.skind in
	match self#get_cur_iosh() with
	| Some iosh ->
	    UD.VS.iter (fun vi ->
	      if IH.mem iosh vi.vid then
		let ios = IH.find iosh vi.vid in
		RD.IOS.iter (function
		  | Some i -> begin (* add s.sid to set for i *)
		      try
			let set = IH.find sidUseSetHash i in
			IH.replace sidUseSetHash i (IS.add s.sid set)
		      with Not_found ->
			IH.add sidUseSetHash i (IS.singleton s.sid)
		  end
		  | None -> ()) ios) u;
	    DoChildren
	| None -> DoChildren
    end

  method vinst i =
    let handle_inst iosh i = match i with
    | Asm(_,_,slvl,_,_,_) -> List.iter (fun (_,s,lv) ->
	match lv with (Var v, off) ->
	  if s.[0] = '+' then
	    self#add_defids iosh (Lval(Var v, off)) (UD.VS.singleton v)
	| _ -> ()) slvl
    | Call(_,ce,el,_) when not (!callHasNoSideEffects i) ->
	List.iter (fun e ->
	  let u = UD.computeUseExp e in
	  UD.VS.iter (fun vi ->
	    if IH.mem iosh vi.vid then
	      let ios = IH.find iosh vi.vid in
	      RD.IOS.iter (function
		| Some i -> begin (* add sid to set for i *)
		    try
		      let set = IH.find sidUseSetHash i in
		      IH.replace sidUseSetHash i (IS.add sid set)
		    with Not_found ->
		      IH.add sidUseSetHash i (IS.singleton sid)  
		end
		| None -> ()) ios) u) (ce::el)
    | Set((Mem _,_) as lh, rhs,l) ->
	List.iter (fun e ->
	  let u = UD.computeUseExp e in
	  UD.VS.iter (fun vi ->
	    if IH.mem iosh vi.vid then
	      let ios = IH.find iosh vi.vid in
	      RD.IOS.iter (function
		| Some i -> begin (* add sid to set for i *)
		    try
		      let set = IH.find sidUseSetHash i in
		      IH.replace sidUseSetHash i (IS.add sid set)
		    with Not_found ->
		      IH.add sidUseSetHash i (IS.singleton sid)  
		end
		| None -> ()) ios) u) ([Lval(lh);rhs])
    | _ -> ()
    in
    ignore(super#vinst i);
    match cur_rd_dat with
    | None -> begin 
	if !debug then ignore(E.log "DCE: instr with no cur_rd_dat\n");
	(* handle_inst *)
	DoChildren
    end
    | Some(_,s,iosh) -> begin
	let u,d = UD.computeUseDefInstr i in
	(* add things in d to the U sets for things in u *)
	let rec loop n =
	  if n < 0 then () else begin
	    UD.VS.iter (fun vi ->
	      if IH.mem iosh vi.vid then
		let ios = IH.find iosh vi.vid in
		RD.IOS.iter (function
		  | Some i -> begin (* add n + s to set for i *)
		      try 
			let set = IH.find defUseSetHash i in
			IH.replace defUseSetHash i (IS.add (n+s) set)
		      with Not_found ->
			IH.add defUseSetHash i (IS.singleton (n+s))
		  end
		  | None -> ()) ios
	      else ()) u;
	    loop (n-1)
	  end
	in
	loop (UD.VS.cardinal d - 1);
	handle_inst iosh i;
	DoChildren
    end

end

(***************************************************
 * Also need to find reads from volatiles 
 * uses two functions I've put in ciltools which 
 * are basically what Zach wrote, except one is for 
 * types and one is for vars. Another difference is
 * they filter out pointers to volatiles. This 
 * handles DMA 
 ***************************************************)
class hasVolatile flag = object (self)
  inherit nopCilVisitor   
  method vlval l = 
    let tp = typeOfLval l in
    if (Ciltools.is_volatile_tp tp) then flag := true;
    DoChildren
  method vexpr e =
    DoChildren
end

let exp_has_volatile e = 
  let flag = ref false in
  ignore (visitCilExpr (new hasVolatile flag) e);
  !flag

let el_has_volatile =
  List.fold_left (fun b e ->
    b || (exp_has_volatile e)) false
 (***************************************************)

let rec compareExp (e1: exp) (e2: exp) : bool =
(*   log "CompareExp %a and %a.\n" d_plainexp e1 d_plainexp e2; *)
  e1 == e2 ||
  match e1, e2 with
  | Lval lv1, Lval lv2
  | StartOf lv1, StartOf lv2
  | AddrOf lv1, AddrOf lv2 -> compareLval lv1 lv2
  | BinOp(bop1, l1, r1, _), BinOp(bop2, l2, r2, _) -> 
      bop1 = bop2 && compareExp l1 l2 && compareExp r1 r2
  | _ -> begin
      match isInteger (constFold true e1), isInteger (constFold true e2) with
        Some i1, Some i2 -> i1 = i2
      | _ -> false
    end

and compareLval (lv1: lval) (lv2: lval) : bool =
  let rec compareOffset (off1: offset) (off2: offset) : bool =
    match off1, off2 with
    | Field (fld1, off1'), Field (fld2, off2') ->
        fld1 == fld2 && compareOffset off1' off2'
    | Index (e1, off1'), Index (e2, off2') ->
        compareExp e1 e2 && compareOffset off1' off2'
    | NoOffset, NoOffset -> true
    | _ -> false
  in
  lv1 == lv2 ||
  match lv1, lv2 with
  | (Var vi1, off1), (Var vi2, off2) ->
      vi1 == vi2 && compareOffset off1 off2
  | (Mem e1, off1), (Mem e2, off2) ->
      compareExp e1 e2 && compareOffset off1 off2
  | _ -> false

let rec stripNopCasts (e:exp): exp =
  match e with
    CastE(t, e') -> begin
      match unrollType (typeOf e'), unrollType t  with
        TPtr _, TPtr _ -> (* okay to strip *)
          stripNopCasts e'
      (* strip casts from pointers to unsigned int/long*)
      | (TPtr _ as t1), (TInt(ik,_) as t2) 
          when bitsSizeOf t1 = bitsSizeOf t2 
            && not (isSigned ik) ->
          stripNopCasts e'
      | (TInt _ as t1), (TInt _ as t2) 
          when bitsSizeOf t1 = bitsSizeOf t2 -> (* Okay to strip.*)
          stripNopCasts e'
      |  _ -> e
    end
  | _ -> e
      
let compareExpStripCasts (e1: exp) (e2: exp) : bool =
  compareExp (stripNopCasts e1) (stripNopCasts e2)

let removedCount = ref 0
(* Filter out instructions whose definition ids are not
   in usedDefsSet *)
class uselessInstrElim : cilVisitor = object(self)
  inherit nopCilVisitor

  method vstmt stm =

    (* give a set of varinfos and an iosh and get
     * the set of definition ids definining the vars *)
    let viSetToDefIdSet iosh vis =
      UD.VS.fold (fun vi s ->
	if IH.mem iosh vi.vid then
	  let ios = IH.find iosh vi.vid in
	  RD.IOS.fold (fun io s ->
	    match io with None -> s
	    | Some i -> IS.add i s) ios s
	else s) vis IS.empty
    in

    (* false when U(defid)\subeq instruses and SU(d) = empty *)
    let check_defid i instruses iosh defid =
      IS.mem defid (!usedDefsSet) &&
      try
	let defuses = IH.find defUseSetHash defid in
	(*let siduses = IH.find sidUseSetHash defid in*)
	if IH.mem sidUseSetHash defid then begin
	  if !debug then ignore(E.log "siduses not empty: %a\n" d_instr i);
	  true
	end else begin
	  (* true if there is something in defuses not in instruses or when
	   * something from defuses is in instruses and is also used somewhere else *)
	  let instruses = viSetToDefIdSet iosh instruses in
	  IS.fold (fun i' b -> 
	    if not(IS.mem i' instruses) then begin
	      if !debug then ignore(E.log "i not in instruses: %a\n" d_instr i);
	      true
	    end else
	      (* can only use the definition i' at the definition defid *)
	      let i'_uses = IH.find defUseSetHash i' in
	      IH.mem sidUseSetHash i' ||
	      if not(IS.equal i'_uses (IS.singleton defid)) then begin
		IS.iter (fun iu -> match RD.getSimpRhs iu with
		| Some(RD.RDExp e) -> 
		    if !debug then ignore(E.log "i' had other than one use: %d: %a\n" 
			     (IS.cardinal i'_uses) d_exp e)
		| Some(RD.RDCall i) ->
		    if !debug then ignore(E.log "i' had other than one use: %d: %a\n" 
			     (IS.cardinal i'_uses) d_instr i)
		| None -> ()) i'_uses;
		true
	      end else b) defuses false
	end
      with Not_found -> true
    in

    let test (i,(_,s,iosh)) =
      match i with 
      | Call(Some(Var vi,NoOffset),Lval(Var vf,NoOffset),el,l) ->
	  if not(!callHasNoSideEffects i) then begin
	    if !debug then ignore(E.log "found call w/ side effects: %a\n" d_instr i);
	    true
	  end else begin
	    if !debug then ignore(E.log "found call w/o side effects: %a\n" d_instr i);
	    (vi.vglob || (Ciltools.is_volatile_vi vi) || (el_has_volatile el) ||
	    let uses, defd = UD.computeUseDefInstr i in
	    let rec loop n =
	      n >= 0 &&
	      (check_defid i uses iosh (n+s) || loop (n-1))
	    in
	    loop (UD.VS.cardinal defd - 1) || (incr removedCount; false))
	  end
      |	Call _ -> true
      | Set(lh,e,_) when compareExpStripCasts (Lval lh) e -> false (* filter x = x *)
      | Set((Var vi,NoOffset),e,_) ->
	  vi.vglob || (Ciltools.is_volatile_vi vi) || (exp_has_volatile e) ||
	  let uses, defd = UD.computeUseDefInstr i in
	  let rec loop n =
	    n >= 0 &&
	    (check_defid i uses iosh (n+s) || loop (n-1))
	  in
	  loop (UD.VS.cardinal defd - 1) || (incr removedCount; false)
      | _ -> true
    in

    let filter il stmdat =
      let rd_dat_lst = RD.instrRDs il stm.sid stmdat false in
      let ildatlst = List.combine il rd_dat_lst in
      let ildatlst' = List.filter test ildatlst in
      let (newil,_) = List.split ildatlst' in
      newil
    in

    match RD.getRDs stm.sid with
      None -> DoChildren
    | Some(_,s,iosh) ->
	match stm.skind with
	  Instr il ->
	    stm.skind <- Instr(filter il ((),s,iosh));
	    SkipChildren
	| _ -> DoChildren
	    
end

(* until fixed point is reached *)
let elim_dead_code_fp (fd : fundec) :  fundec =
  (* fundec -> fundec *)
  let rec loop fd =
    usedDefsSet := IS.empty;
    IH.clear defUseSetHash;
    IH.clear sidUseSetHash;
    removedCount := 0;
    time "reaching definitions" RD.computeRDs fd;
    ignore(time "ud-collector"
	     (visitCilFunction (new usedDefsCollectorClass :> cilVisitor)) fd);
    let fd' = time "useless-elim" (visitCilFunction (new uselessInstrElim)) fd in
    if !removedCount = 0 then fd' else loop fd'
  in
  loop fd

(* just once *)
let elim_dead_code (fd : fundec) :  fundec =
  (* fundec -> fundec *)
  usedDefsSet := IS.empty;
  IH.clear defUseSetHash;
  IH.clear sidUseSetHash;
  removedCount := 0;
  time "reaching definitions" RD.computeRDs fd;
  if !debug then ignore(E.log "DCE: collecting used definitions\n");
  ignore(time "ud-collector" 
	   (visitCilFunction (new usedDefsCollectorClass :> cilVisitor)) fd);
  if !debug then ignore(E.log "DCE: eliminating useless instructions\n");
  let fd' = time "useless-elim" (visitCilFunction (new uselessInstrElim)) fd in
  fd'

class deadCodeElimClass : cilVisitor = object(self)
    inherit nopCilVisitor

  method vfunc fd =
    let fd' = elim_dead_code(*_fp*) fd in
    ChangeTo(fd')

end

let dce f =
  if !debug then ignore(E.log "DCE: starting dead code elimination\n");
  visitCilFile (new deadCodeElimClass) f
