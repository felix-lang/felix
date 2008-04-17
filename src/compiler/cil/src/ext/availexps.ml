(* compute available expressions, although in a somewhat
   non-traditional way. the abstract state is a mapping from
   variable ids to expressions as opposed to a set of
   expressions *)

open Cil
open Pretty
open Expcompare

module E = Errormsg
module DF = Dataflow
module UD = Usedef
module IH = Inthash
module U = Util
module S = Stats

let debug = ref false
let doTime = ref false


let time s f a = 
  if !doTime then
    S.time s f a
  else f a

(*
 * When ignore_inst returns true, then
 * the instruction in question has no
 * effects on the abstract state.
 * When ignore_call returns true, then
 * the instruction only has side-effects
 * from the assignment if there is one.
 *)
let ignore_inst = ref (fun i -> false)
let ignore_call = ref (fun i -> false)

let registerIgnoreInst (f : instr -> bool) : unit =
  let f' = !ignore_inst in
  ignore_inst := (fun i -> (f i) || (f' i))

let registerIgnoreCall (f : instr -> bool) : unit =
  let f' = !ignore_call in
  ignore_call := (fun i -> (f i) || (f' i))


(* exp IH.t -> exp IH.t -> bool *)
let eh_equals eh1 eh2 =
  if not(IH.length eh1 = IH.length eh2)
  then false
  else IH.fold (fun vid e b ->
    if not b then b else
    try let e2 = IH.find eh2 vid in
    if not(compareExpStripCasts e e2)
    then false
    else true
    with Not_found -> false)
      eh1 true

let eh_pretty () eh = line ++ seq line (fun (vid,e) ->
  text "AE:vid:" ++ num vid ++ text ": " ++
    (d_exp () e)) (IH.tolist eh)

(* the result must be the intersection of eh1 and eh2 *)
(* exp IH.t -> exp IH.t -> exp IH.t *)
let eh_combine eh1 eh2 =
  if !debug then ignore(E.log "eh_combine: combining %a\n and\n %a\n"
			  eh_pretty eh1 eh_pretty eh2);
  let eh' = IH.copy eh1 in (* eh' gets all of eh1 *)
  IH.iter (fun vid e1 ->
    try let e2l = IH.find_all eh2 vid in
    if not(List.exists (fun e2 -> compareExpStripCasts e1 e2) e2l)
    (* remove things from eh' that eh2 doesn't have *)
    then let e1l = IH.find_all eh' vid in
    let e1l' = List.filter (fun e -> not(compareExpStripCasts e e1)) e1l in
    IH.remove_all eh' vid;
    List.iter (fun e -> IH.add eh' vid e) e1l'
    with Not_found ->
      IH.remove_all eh' vid) eh1;
  if !debug then ignore(E.log "with result %a\n"
			  eh_pretty eh');
  eh'


(* On a memory write, kill expressions containing memory reads
   variables whose address has been taken, and globals. *)
class memReadOrAddrOfFinderClass br = object(self)
  inherit nopCilVisitor

  method vexpr e = match e with
  | Lval(Mem _, _) -> begin
      br := true;
      SkipChildren
  end
  | AddrOf(Var vi, NoOffset) ->
      (* Writing to memory won't change the address of something *)
      SkipChildren
  | _ -> DoChildren

  method vvrbl vi =
    if vi.vaddrof || vi.vglob then
      (br := true;
       SkipChildren)
    else DoChildren

end

(* exp -> bool *)
let exp_has_mem_read e =
  let br = ref false in
  let vis = new memReadOrAddrOfFinderClass br in
  ignore(visitCilExpr vis e);
  !br

   
let eh_kill_mem eh =
  IH.iter (fun vid e ->
    if exp_has_mem_read e
    then IH.remove eh vid)
    eh

(* need to kill exps containing a particular vi sometimes *)
class viFinderClass vi br = object(self)
  inherit nopCilVisitor
      
  method vvrbl vi' = 
    if vi.vid = vi'.vid
    then (br := true; SkipChildren)
    else DoChildren

end

let exp_has_vi vi e =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilExpr vis e);
  !br

let eh_kill_vi eh vi =
  IH.iter (fun vid e ->
    if exp_has_vi vi e
    then IH.remove eh vid)
    eh

(* need to kill exps containing a particular lval sometimes *)
class lvalFinderClass lv br = object(self)
  inherit nopCilVisitor

  method vlval l =
    if compareLval l lv
    then (br := true; SkipChildren)
    else DoChildren

end

let exp_has_lval lv e =
  let br = ref false in
  let vis = new lvalFinderClass lv br in
  ignore(visitCilExpr vis e);
  !br

let eh_kill_lval eh lv =
  IH.iter (fun vid e ->
    if exp_has_lval lv e
    then IH.remove eh vid)
    eh


class volatileFinderClass br = object(self)
  inherit nopCilVisitor

  method vexpr e =
    if (hasAttribute "volatile" (typeAttrs (typeOf e))) 
    then (br := true; SkipChildren)
    else DoChildren
end

let exp_is_volatile e : bool =
  let br = ref false in
  let vis = new volatileFinderClass br in
  ignore(visitCilExpr vis e);
  !br

let varHash = IH.create 32

let eh_kill_addrof_or_global eh =
  if !debug then ignore(E.log "eh_kill: in eh_kill\n");
  IH.iter (fun vid e ->
    try let vi = IH.find varHash vid in
    if vi.vaddrof
    then begin
      if !debug then ignore(E.log "eh_kill: %s has its address taken\n"
			      vi.vname);
      IH.remove eh vid
    end
    else if vi.vglob
    then begin
      if !debug then ignore(E.log "eh_kill: %s is global\n"
			   vi.vname);
      IH.remove eh vid
    end
    with Not_found -> ()) eh

let eh_handle_inst i eh = 
  if (!ignore_inst) i then eh else
  match i with
    (* if a pointer write, kill things with read in them.
       also kill mappings from vars that have had their address taken,
       and globals.
       otherwise kill things with lv in them and add e *)
    Set(lv,e,_) -> (match lv with
      (Mem _, _) -> 
	(eh_kill_mem eh; 
	 eh_kill_addrof_or_global eh;
	 eh)
    | (Var vi, NoOffset) when not (exp_is_volatile e) -> 
	(match e with
	  Lval(Var vi', NoOffset) -> (* ignore x = x *)
	    if vi'.vid = vi.vid then eh else
	    (IH.replace eh vi.vid e;
	     eh_kill_vi eh vi;
	     eh)
	| _ ->
	    (IH.replace eh vi.vid e;
	     eh_kill_vi eh vi;
	     eh))
    | (Var vi, _ ) -> begin
	(* must remove mapping for vi *)
	IH.remove eh vi.vid;
	eh_kill_lval eh lv;
	eh
    end)
  | Call(Some(Var vi,NoOffset),_,_,_) ->
      (IH.remove eh vi.vid;
       eh_kill_vi eh vi;
       if not((!ignore_call) i) then begin
	 eh_kill_mem eh;
	 eh_kill_addrof_or_global eh
       end;
       eh)
  | Call(_,_,_,_) ->
      (eh_kill_mem eh;
       eh_kill_addrof_or_global eh;
       eh)
  | Asm(_,_,_,_,_,_) ->
      let _,d = UD.computeUseDefInstr i in
      (UD.VS.iter (fun vi ->
	eh_kill_vi eh vi) d;
       eh)

module AvailableExps =
  struct

    let name = "Available Expressions"

    let debug = debug

    (* mapping from var id to expression *)
    type t = exp IH.t

    let copy = IH.copy

    let stmtStartData = IH.create 64

    let pretty = eh_pretty

    let computeFirstPredecessor stm eh = eh

    let combinePredecessors (stm:stmt) ~(old:t) (eh:t) =
      if time "eh_equals" (eh_equals old) eh then None else
      Some(time "eh_combine" (eh_combine old) eh)

    let doInstr i eh = 
      let action = eh_handle_inst i in
      DF.Post(action)

    let doStmt stm astate = DF.SDefault

    let doGuard c astate = DF.GDefault

    let filterStmt stm = true

  end

module AE = DF.ForwardsDataFlow(AvailableExps)

(* make an exp IH.t with everything in it,
 * also, fill in varHash while we're here.
 *)
class varHashMakerClass = object(self)
  inherit nopCilVisitor

  method vvrbl vi =
    (if not(IH.mem varHash vi.vid)
    then 
      (if !debug && vi.vglob then ignore(E.log "%s is global\n" vi.vname);
       if !debug && not(vi.vglob) then ignore(E.log "%s is not global\n" vi.vname);
       IH.add varHash vi.vid vi));
    DoChildren

end

let varHashMaker = new varHashMakerClass

let make_var_hash fd =
  IH.clear varHash;
  ignore(visitCilFunction varHashMaker fd)

(*
 * Computes AEs for function fd.
 *
 *
 *)
let computeAEs fd =
  try let slst = fd.sbody.bstmts in
  let first_stm = List.hd slst in
  time "make_var_hash" make_var_hash fd;
  IH.clear AvailableExps.stmtStartData;
  IH.add AvailableExps.stmtStartData first_stm.sid (IH.create 4);
  time "compute" AE.compute [first_stm]
  with Failure "hd" -> if !debug then ignore(E.log "fn w/ no stmts?\n")
  | Not_found -> if !debug then ignore(E.log "no data for first_stm?\n")


(* get the AE data for a statement *)
let getAEs sid =
  try Some(IH.find AvailableExps.stmtStartData sid)
  with Not_found -> None

(* get the AE data for an instruction list *)
let instrAEs il sid eh out =
  let proc_one hil i =
    match hil with
      [] -> let eh' = IH.copy eh in
      let eh'' = eh_handle_inst i eh' in
       eh''::hil
    | eh'::ehrst as l ->
	let eh' = IH.copy eh' in
	let eh'' = eh_handle_inst i eh' in
	eh''::l
  in
  let folded = List.fold_left proc_one [eh] il in
  let foldednotout = List.rev (List.tl folded) in
  foldednotout

class aeVisitorClass = object(self)
  inherit nopCilVisitor

  val mutable sid = -1

  val mutable ae_dat_lst = []

  val mutable cur_ae_dat = None

  method vstmt stm =
    sid <- stm.sid;
    match getAEs sid with
      None ->
	if !debug then ignore(E.log "aeVis: stm %d has no data\n" sid);
	cur_ae_dat <- None;
	DoChildren
    | Some eh ->
	match stm.skind with
	  Instr il ->
	    if !debug then ignore(E.log "aeVist: visit il\n");
	    ae_dat_lst <- time "instrAEs" (instrAEs il stm.sid eh) false;
	    DoChildren
	| _ ->
	    if !debug then ignore(E.log "aeVisit: visit non-il\n");
	    cur_ae_dat <- None;
	    DoChildren

  method vinst i =
    if !debug then ignore(E.log "aeVist: before %a, ae_dat_lst is %d long\n"
			    d_instr i (List.length ae_dat_lst));
    try
      let data = List.hd ae_dat_lst in
      cur_ae_dat <- Some(data);
      ae_dat_lst <- List.tl ae_dat_lst;
      if !debug then ignore(E.log "aeVisit: data is %a\n" eh_pretty data);
      DoChildren
    with Failure "hd" ->
      if !debug then ignore(E.log "aeVis: il ae_dat_lst mismatch\n");
      DoChildren

  method get_cur_eh () =
    match cur_ae_dat with
      None -> getAEs sid
    | Some eh -> Some eh

end
