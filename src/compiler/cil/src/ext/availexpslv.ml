(* compute available expressions, although in a somewhat
   non-traditional way. the abstract state is a mapping from
   lvalues to expressions as opposed to a set of
   expressions *)

open Cil
open Pretty
open Expcompare

module E = Errormsg
module DF = Dataflow
module UD = Usedef
module IH = Inthash
module H = Hashtbl
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


module LvExpHash = 
  H.Make(struct
    type t = lval
    let equal lv1 lv2 = compareLval lv1 lv2
    let hash = H.hash
  end)

(* exp LvExpHash.t -> exp LvExpHash.t -> bool *)
let lvh_equals lvh1 lvh2 =
  if not(LvExpHash.length lvh1 = LvExpHash.length lvh2)
  then false
  else LvExpHash.fold (fun lv e b ->
    if not b then b else
    try let e2 = LvExpHash.find lvh2 lv in
    if not(compareExpStripCasts e e2)
    then false
    else true
    with Not_found -> false)
      lvh1 true

let lvh_pretty () lvh = LvExpHash.fold (fun lv e d ->
  d ++ line ++ (d_lval () lv) ++ text " -> " ++ (d_exp () e))
    lvh nil


(* the result must be the intersection of eh1 and eh2 *)
(* exp IH.t -> exp IH.t -> exp IH.t *)
let lvh_combine lvh1 lvh2 =
  if !debug then ignore(E.log "lvh_combine: combining %a\n and\n %a\n"
			  lvh_pretty lvh1 lvh_pretty lvh2);
  let lvh' = LvExpHash.copy lvh1 in (* eh' gets all of eh1 *)
  LvExpHash.iter (fun lv e1 ->
    try let e2l = LvExpHash.find_all lvh2 lv in
    if not(List.exists (fun e2 -> compareExpStripCasts e1 e2) e2l)
    (* remove things from eh' that eh2 doesn't have *)
    then let e1l = LvExpHash.find_all lvh' lv in
    let e1l' = List.filter (fun e -> not(compareExpStripCasts e e1)) e1l in
    LvExpHash.remove lvh' lv;
    List.iter (fun e -> LvExpHash.add lvh' lv e) e1l'
    with Not_found ->
      LvExpHash.remove lvh' lv) lvh1;
  if !debug then ignore(E.log "with result %a\n"
			  lvh_pretty lvh');
  lvh'


(* On a memory write, kill expressions containing memory reads
   variables whose address has been taken, and globals. *)
class memReadOrAddrOfFinderClass br = object(self)
  inherit nopCilVisitor

  method vexpr e = match e with
  | AddrOf(Mem _, _)
  | StartOf(Mem _, _)
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

let lval_has_mem_read lv =
  let br = ref false in
  let vis = new memReadOrAddrOfFinderClass br in
  ignore(visitCilLval vis lv);
  !br
   
let lvh_kill_mem lvh =
  LvExpHash.iter (fun lv e ->
    match lv with
    | (Mem _, _) -> LvExpHash.remove lvh lv
    | _ ->
        if exp_has_mem_read e || lval_has_mem_read lv
        then LvExpHash.remove lvh lv)
    lvh

(* need to kill exps containing a particular vi sometimes *)
class viFinderClass vi br = object(self)
  inherit nopCilVisitor
      
  method vvrbl vi' = 
    if vi.vid = vi'.vid
    then (br := true; SkipChildren)
    else DoChildren

end

let instr_has_vi vi i =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilInstr vis i);
  !br

let exp_has_vi vi e =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilExpr vis e);
  !br

let lval_has_vi vi lv =
  let br = ref false in
  let vis = new viFinderClass vi br in
  ignore(visitCilLval vis lv);
  !br

let lvh_kill_vi lvh vi =
  LvExpHash.iter (fun lv e ->
    if exp_has_vi vi e || lval_has_vi vi lv
    then LvExpHash.remove lvh lv)
    lvh

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

let lval_has_lval lv (host,hostoff) =
  let br = ref false in
  let vis = new lvalFinderClass lv br in
  (match host with
  | Mem e -> ignore(visitCilExpr vis e)
  | _ -> ());
  ignore(visitCilOffset vis hostoff);
  !br

let lvh_kill_lval lvh lv =
  LvExpHash.iter (fun lv' e ->
    if exp_has_lval lv e || lval_has_lval lv lv'
    then LvExpHash.remove lvh lv')
    lvh


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

(* let varHash = IH.create 32 *)

class addrOfOrGlobalFinderClass br = object(self)
  inherit nopCilVisitor

  method vvrbl vi =
    if vi.vaddrof || vi.vglob
    then (br := true; SkipChildren)
    else DoChildren

end

let lval_has_addrof_or_global lv =
  let br = ref false in
  let vis = new addrOfOrGlobalFinderClass br in
  ignore(visitCilLval vis lv);
  !br

let lvh_kill_addrof_or_global lvh =
  LvExpHash.iter (fun lv e ->
    if lval_has_addrof_or_global lv
    then LvExpHash.remove lvh lv)
    lvh


let lvh_handle_inst i lvh = 
  if (!ignore_inst) i then lvh else
  match i with
    Set(lv,e,_) -> begin 
      match lv with
      | (Mem _, _) -> begin
	  LvExpHash.replace lvh lv e;
	  lvh_kill_mem lvh;
	  lvh_kill_addrof_or_global lvh;
	  lvh
      end
      | _ when not (exp_is_volatile e) -> begin
	  (* ignore x = x *)
	  if compareExpStripCasts (Lval lv) e then lvh 
	  else begin
	    LvExpHash.replace lvh lv e;
	    lvh_kill_lval lvh lv;
	    lvh
	  end
      end
      | _ -> begin (* e is volatile *)
        (* must remove mapping for lv *)
	    if !debug then ignore(E.log "lvh_handle_inst: %a is volatile. killing %a\n"
		    d_exp e d_lval lv);
	    LvExpHash.remove lvh lv;
	    lvh_kill_lval lvh lv;
	    lvh
      end
    end
  | Call(Some lv,_,_,_) -> begin
      LvExpHash.remove lvh lv;
      lvh_kill_lval lvh lv;
      if not((!ignore_call) i) then begin
        lvh_kill_mem lvh;
	    lvh_kill_addrof_or_global lvh
      end;
      lvh
  end
  | Call(_,_,_,_) -> begin
      if not((!ignore_call) i) then begin
	lvh_kill_mem lvh;
	lvh_kill_addrof_or_global lvh;
      end;
      lvh
  end
  | Asm(_,_,_,_,_,_) -> begin
      let _,d = UD.computeUseDefInstr i in
      UD.VS.iter (fun vi ->
	lvh_kill_vi lvh vi) d;
      lvh
  end

module AvailableExps =
  struct

    let name = "Available Expressions"

    let debug = debug

    (* mapping from var id to expression *)
    type t = exp LvExpHash.t

    let copy = LvExpHash.copy

    let stmtStartData = IH.create 64

    let pretty = lvh_pretty

    let computeFirstPredecessor stm lvh = lvh

    let combinePredecessors (stm:stmt) ~(old:t) (lvh:t) =
      if time "lvh_equals" (lvh_equals old) lvh then None else
      Some(time "lvh_combine" (lvh_combine old) lvh)

    let doInstr i lvh = 
      let action = lvh_handle_inst i in
      DF.Post(action)

    let doStmt stm astate = DF.SDefault

    let doGuard c astate = DF.GDefault

    let filterStmt stm = true

  end

module AE = DF.ForwardsDataFlow(AvailableExps)


(*
 * Computes AEs for function fd.
 *
 *
 *)
let computeAEs fd =
  try let slst = fd.sbody.bstmts in
  let first_stm = List.hd slst in
  (*time "make_var_hash" make_var_hash fd;*)
  IH.clear AvailableExps.stmtStartData;
  IH.add AvailableExps.stmtStartData first_stm.sid (LvExpHash.create 4);
  time "compute" AE.compute [first_stm]
  with Failure "hd" -> if !debug then ignore(E.log "fn w/ no stmts?\n")
  | Not_found -> if !debug then ignore(E.log "no data for first_stm?\n")


(* get the AE data for a statement *)
let getAEs sid =
  try Some(IH.find AvailableExps.stmtStartData sid)
  with Not_found -> None

(* get the AE data for an instruction list *)
let instrAEs il sid lvh out =
  if !debug then ignore(E.log "instrAEs\n");
  let proc_one hil i =
    match hil with
      [] -> let lvh' = LvExpHash.copy lvh in
      let lvh'' = lvh_handle_inst i lvh' in
       lvh''::hil
    | lvh'::ehrst as l ->
	let lvh' = LvExpHash.copy lvh' in
	let lvh'' = lvh_handle_inst i lvh' in
	lvh''::l
  in
  let folded = List.fold_left proc_one [lvh] il in
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
      if !debug then ignore(E.log "aeVisit: data is %a\n" lvh_pretty data);
      DoChildren
    with Failure "hd" ->
      if !debug then ignore(E.log "aeVis: il ae_dat_lst mismatch\n");
      DoChildren

  method get_cur_eh () =
    match cur_ae_dat with
      None -> getAEs sid
    | Some eh -> Some eh

end
