(*
 * predabst.ml
 *
 * This is a CIL module for computing the
 * predicate abstraction of a C program.
 *
 * It assumes that expressions contain at
 * most one pointer dereference. I.e.
 *
 * Simplify.simplify has been run on the file.
 *
 * Also assumes that there has been a pointer analysis after
 * the simplemem transformation, the results for which
 * can be accessed through the Ptranal module.
 * 
 * Further assumes that each function has only one return
 * of the form "return r" where r is a variable.
 *)

open Cil
open Pretty
open Expcompare

module E = Errormsg
module P = Ptranal
module DF = Dataflow
module IH = Inthash
module H = Hashtbl
module U = Util
module S = Stats

(* Should debugging info be dumped? *)
let debug = ref false

(* This function should be defined by a client to
 * specify the predicates in a function that
 * the abstraction should track *)
let collectPredicates = ref (fun (fd:fundec) -> [])

(* This function should be set by a client to
 * specify any instructions that should be ignored
 * by the analysis *)
let ignoreInstruction = ref (fun (i:instr) -> false)

(* This function should be set by a client to
 * indicate that a function has no side-effects *)
let instrHasNoSideEffects = ref (fun (i:instr) -> false)

(* This function should be set by a client to
 * obtain asserted predicates from an instruction. i.e.
 * an assert or other runtime check. *)
(* (instr -> exp list) ref *)
let getPredsFromInstr = ref (fun (i:instr) -> [])


module ExpIntHash = 
  H.Make(struct
    type t = exp
    let equal e1 e2 = compareExpStripCasts e1 e2
    let hash = H.hash
  end)
  

module type TRANSLATOR =
  sig
    type exp
    type unop = exp -> exp
    type binop = exp -> exp -> exp
    val mkTrue   : unit -> exp
    val mkFalse  : unit -> exp

    val mkAnd    : binop
    val mkOr     : binop
    val mkNot    : unop
    val mkIte    : exp -> exp -> exp -> exp
    val mkImp    : binop

    val mkEq     : binop
    val mkNe     : binop
    val mkLt     : binop
    val mkLe     : binop
    val mkGt     : binop
    val mkGe     : binop

    val mkPlus   : binop
    val mkTimes  : binop
    val mkMinus  : binop
    val mkDiv    : binop
    val mkMod    : binop
    val mkLShift : binop
    val mkRShift : binop
    val mkBAnd   : binop
    val mkBXor   : binop
    val mkBOr    : binop

    val mkNeg    : unop
    val mkCompl  : unop

    val mkVar    : string -> exp
    val mkConst  : int -> exp

    val isValid  : exp -> bool

  end

module NullTranslator : TRANSLATOR = 
  struct
    type exp = int
    type unop = int -> int
    type binop = int -> int -> int

    let binop x y = 0
    let unop x = 0

    let mkTrue () = 0
    let mkFalse () = 0

    let mkAnd = binop
    let mkOr  = binop
    let mkNot = unop
    let mkIte i t e = 0
    let mkImp = binop

    let mkEq = binop
    let mkNe = binop
    let mkLt = binop
    let mkLe = binop
    let mkGt = binop
    let mkGe = binop

    let mkPlus = binop
    let mkTimes = binop
    let mkMinus = binop
    let mkDiv = binop
    let mkMod = binop
    let mkLShift = binop
    let mkRShift = binop
    let mkBAnd = binop
    let mkBXor = binop
    let mkBOr = binop

    let mkNeg = unop
    let mkCompl = unop

    let mkVar s = 0
    let mkConst i = 0

    let isValid i = false

  end

module type SOLVER =
  sig
    type exp

    val transExp : Cil.exp -> exp

    (* does the first exp imply the second *)
    val isValid  : exp -> exp -> bool
  end

(* Takes a translator and produces a solver *)
module Solver = functor(T:TRANSLATOR) ->
  struct
    type exp = T.exp

    exception NYI

    let transUnOp op e =
      match op with
      | Neg -> T.mkNeg e
      | BNot -> T.mkCompl e
      | _ -> raise NYI

    let transBinOp op e1 e2 =
      match op with
      | PlusA | PlusPI | IndexPI -> T.mkPlus e1 e2
      | MinusA | MinusPI | MinusPP -> T.mkMinus e1 e2
      | Mult -> T.mkTimes e1 e2
      | Div -> T.mkDiv e1 e2
      | Mod -> T.mkMod e1 e2
      | Shiftlt -> T.mkLShift e1 e2
      | Shiftrt -> T.mkRShift e1 e2
      | Lt -> T.mkLt e1 e2
      | Gt -> T.mkGt e1 e2
      | Le -> T.mkLe e1 e2
      | Ge -> T.mkGe e1 e2
      | Eq -> T.mkEq e1 e2
      | Ne -> T.mkNe e1 e2
      | BAnd -> T.mkBAnd e1 e2
      | BXor -> T.mkBXor e1 e2
      | BOr -> T.mkBOr e1 e2
      | LAnd -> T.mkAnd e1 e2
      | LOr -> T.mkOr e1 e2

    let rec transExp e =
      match e with
      | Const(CInt64(v,k,so)) -> T.mkConst (Int64.to_int v)
      | Const _ -> raise NYI
      | Lval(Var vi,NoOffset) when vi.vname = "_ZERO_" ->
	  T.mkConst 0
      | Lval l -> T.mkVar (sprint 80 (d_lval () l))
      | UnOp(op,e,_) -> 
	  let e = transExp e in
	  transUnOp op e
      | BinOp(op,e1,e2,_) ->
	  let e1 = transExp e1 in
	  let e2 = transExp e2 in
	  transBinOp op e1 e2
      | SizeOf typ -> T.mkConst ((bitsSizeOf typ)/8)
      | SizeOfE e -> transExp (SizeOf(typeOf e))
      | SizeOfStr s -> T.mkConst (1 + String.length s)
      | AlignOf typ -> T.mkConst (alignOf_int typ)
      | AlignOfE e -> transExp (AlignOf(typeOf e))
      | CastE(typ,e) -> transExp  e
      (* Cast should check if signed type, and if so, make an ite term *)
      | AddrOf lv -> T.mkVar (sprint 80 (d_exp () e))
      | StartOf lv -> T.mkVar (sprint 80 (d_exp () e))

    let isValid e1 e2 =
      let e_imp = T.mkImp e1 e2 in
      T.isValid e_imp

  end

module NullSolver = Solver(NullTranslator)

module PredAbst = functor(S:SOLVER) ->
  struct
    type boolLat = True | False | Top | Bottom

    let combineBoolLat b1 b2 = match b1,b2 with
    | Top, _
    | _, Top
    | True, False
    | False, True -> Top
    | b, Bottom
    | Bottom, b -> b
    | _, _ when b1 = b2 -> b1
    | _, _ -> Bottom

    let d_bl () bl = match bl with
    | True -> text "True"
    | False -> text "False"
    | Top -> text "Top"
    | Bottom -> text "Bottom"

    type funcSig =
	{
	 (* The set of formal parameters of the procedure *)
	 mutable fsFormals  : varinfo list;

	 (* The return variable of the procedure *)
	 mutable fsReturn   : varinfo option;

	 (* All of the predicates relevant to a function *)
	 mutable fsAllPreds : exp list;

	 (* Predicates of the procedure that refer only
	  * to the formals *)
	 mutable fsFPPreds  : exp list;

	 (* Predicates of the procedure that refer only
	  * to the return variable and formals and
	  * which refer to globals or dereferences of formals *)
	 mutable fsRetPreds : exp list;
       }

    (* A list of mappings from predicate id to an element of the
     * boolean lattice for a list of instructions, or just one
     * such mapping for any other statement. *)
    type stmtState = 
      | ILState of (boolLat IH.t) list
      | StmState of boolLat IH.t

    type absState = stmtState IH.t

    type context =
	{
	 mutable cFuncSigs   : funcSig IH.t;
	 mutable cPredicates : exp IH.t;
	 mutable cRPredMap   : int ExpIntHash.t;
	 mutable cNextPred   : int;
       }

    let emptyContext () =
      {
       cFuncSigs = IH.create 100;
       cPredicates = IH.create 100;
       cRPredMap = ExpIntHash.create 100;
       cNextPred = 0;
     }

    (**********************************
     *
     * Functions and classes for making the function signatures.
     *
     **********************************)

    class returnFinderClass vor = object(self)
      inherit nopCilVisitor
	  
      method vstmt s = match s.skind with
      | Return(Some(Lval(Var vi,NoOffset)),_) -> begin
	  vor := Some(vi);
	  SkipChildren
      end
      | _ -> DoChildren

    end

    let findReturn (fd:fundec) =
      let vor = ref None in
      ignore(visitCilFunction (new returnFinderClass vor) fd);
      !vor

    class viFinderClass vi br = object(self)
      inherit nopCilVisitor
      
      method vvrbl vi' = 
	if vi.vid = vi'.vid
	then (br := true; SkipChildren)
	else DoChildren

    end

    let expContainsVi e vi =
      let br = ref false in
      let vis = new viFinderClass vi br in
      ignore(visitCilExpr vis e);
      !br

    class derefFinderClass vi br = object(self)
      inherit nopCilVisitor

      method vlval lv = match lv with
      | (Mem(Lval(Var vi,_)),_) -> begin
	  br := true;
	  SkipChildren
      end
      | _ -> DoChildren

    end

    let expContainsDeref e vi =
      let br = ref false in
      let vis = new derefFinderClass vi br in
      ignore(visitCilExpr vis e);
      !br

    class globalFinderClass br = object(self)
      inherit nopCilVisitor
      method vvrbl vi =
	if vi.vglob then br := true;
	DoChildren
    end

    let expContainsGlobal e =
      let br = ref false in
      let vis = new globalFinderClass br in
      ignore(visitCilExpr vis e);
      !br

    class aliasFinderClass ae br = object(self)
      inherit nopCilVisitor
      method vexpr e =
	if Ptranal.may_alias e ae then br := true;
	DoChildren
    end

    (* Does he contain an alias to ae? *)
    let expHasAlias e ae =
      let br = ref false in
      let vis = new aliasFinderClass ae br in
      ignore(visitCilExpr vis e);
      !br

    (* filter out predicates from preds that refer to locals *)
    let makeFormalPreds (locals : varinfo list)
	                (preds  : exp list)
      =
      List.filter
	(fun e -> not(List.exists (expContainsVi e) locals))
	preds

    (* filter out predicates from pred that contain locals
     * and that don't refer to globals or dereferences of
     * formals *)
    let makeReturnPreds (ret     : varinfo option)
	                (locals  : varinfo list)
	                (formals : varinfo list)
	                (preds   : exp list)
	                (fpreds  : exp list)
	=
      let localsNoRet = 
	match ret with
	| Some ret ->
	    List.filter 
	      (fun vi -> not(vi.vid = ret.vid)) 
	      locals 
	| None -> locals
      in
      let retPreds = makeFormalPreds localsNoRet preds in
      let retPreds = 
	match ret with
	| Some ret ->
	    List.filter 
	      (fun e -> not(expContainsVi e ret)) 
	      retPreds 
	| None -> retPreds
      in
      let retPreds' = List.filter 
	  (fun e -> (expContainsGlobal e) ||
	  (List.exists (expContainsDeref e) formals))
	  fpreds in
      retPreds@retPreds'

    let funSigHash = IH.create 100
    class funcSigMakerClass = object(self)
      inherit nopCilVisitor

      method vfunc fd =
	if IH.mem funSigHash fd.svar.vid then SkipChildren else
	let formals = fd.sformals in
	let locals = fd.slocals in
	let ret = findReturn fd in
	let preds = !collectPredicates fd in
	let formalPreds = makeFormalPreds locals preds in
	let returnPreds = makeReturnPreds ret locals formals 
	    preds formalPreds in
	let fs = { fsFormals  = formals;
		   fsReturn   = ret;
		   fsAllPreds = preds;
		   fsFPPreds  = formalPreds;
		   fsRetPreds = returnPreds;} in
	IH.add funSigHash fd.svar.vid fs;
	SkipChildren

    end

    let makeFunctionSigs (f:file) =
      IH.clear funSigHash;
      visitCilFileSameGlobals (new funcSigMakerClass) f;
      funSigHash

    let h_equals h1 h2 =
      (* must be the same length *)
      IH.fold (fun pid bl b ->
	b &&
	try let bl2 = IH.find h2 pid in
	bl = bl2
	with Not_found -> false)
	h1 true

    let hl_equals hl1 hl2 =
      List.fold_left2
	(fun b h1 h2 -> b && (h_equals h1 h2))
	true hl1 hl2

    let h_combine h1 h2 =
      let h' = IH.copy h1 in
      IH.iter (fun pid bl1 ->
	try let bl2 = IH.find h2 pid in
	IH.replace h' pid (combineBoolLat bl1 bl2)
	with Not_found -> ()) h1;
      h'

    let hl_combine hl1 hl2 =
      List.map 
	(fun (h1, h2) -> h_combine h1 h2)
	(List.combine hl1 hl2)

    let substitute (rhse : exp) (* for *)
                   (lv   : lval) (* in *)
                   (e    : exp)
	=
      (* return a list of a list of conjuncts that are the aliasing
       * constraints, and the expression we get when those
       * constraints are satisfied *)
      let rec helper e =
	match e with
	| Const _ -> [(one,e)]
	| AddrOf(Var v, NoOffset) -> [(one, e)]
	| StartOf(Var v, NoOffset) -> [(one, e)]
	| Lval(Mem me, NoOffset) ->
	    if Ptranal.may_alias me (AddrOf lv) then
	      [(BinOp(Eq,me,AddrOf lv,intType),
		rhse);
	       (UnOp(LNot,BinOp(Eq,me,AddrOf lv,intType),intType),
		e)]
	    else [(one,e)]
	| Lval(Var v, off) ->
	    if compareLval (Var v, off) lv then
	      [(one,rhse)]
	    else
	      [(one,e)]
	| BinOp(op, e1, e2,t) ->
	    let pl1 = helper e1 in
	    let pl2 = helper e2 in
	    (* for every pair of things from pl1 and pl2 *)
	    List.fold_left (fun l (c1,e1) ->
	      l @ (List.map (fun (c2,e2) ->
		(BinOp(LAnd,c1,c2,intType),BinOp(op,e1,e2,t))) pl2))
	      [] pl1
	| UnOp(op, e, t) ->
	    let pl = helper e in
	    List.map (fun (c,e) ->
	      (c,UnOp(op,e,t)))
	      pl
	| CastE(t, e) ->
	    let pl = helper e in
	    List.map (fun (c,e) ->
	      (c,CastE(t,e)))
	      pl
	| _ -> raise (E.s "Simplify has not been run\n")
      in
      let makeDisjunction pl =
	List.fold_left (fun d (c,e) ->
	  BinOp(LOr,d,BinOp(LAnd,c,e,intType),intType))
	  zero pl
      in
      let rec cleanUpExp e = match e with
      | BinOp(LAnd,e1,e2,_) when compareExp (cleanUpExp e1) one -> 
	  cleanUpExp e2
      | BinOp(LAnd,e1,e2,_) when compareExp (cleanUpExp e2) one -> 
	  cleanUpExp e1
      | BinOp(LOr,e1,e2,_) when compareExp (cleanUpExp e1) zero -> 
	  cleanUpExp e2
      | BinOp(LOr,e1,e2,_) when compareExp (cleanUpExp e2) zero -> 
	  cleanUpExp e1
      | UnOp(LNot,e,_) when compareExp (cleanUpExp e) one -> 
	  zero
      | UnOp(LNot,e,_) when compareExp (cleanUpExp e) zero -> 
	  one
      | _ -> e
      in
      cleanUpExp (makeDisjunction (helper e))


    (* computes WP(i,e) as Some(wp) *)
    let weakestPrecondition (i : instr) 
	                    (e : exp)
	=
      match i with
      | Set((Var vi, off) as lh, rhse, l) ->
	  Some(substitute rhse (* for *) lh (* in *) e)
      | Set((Mem me, NoOffset) as lh, rhse, l) ->
	  Some(substitute rhse (* for *) lh (* in *) e)
      | Set(_,_,_) -> raise (E.s "Simplify has not been run\n")
      | _ -> None (* Call and Asm are handled elsewhere *)


    let getPred (ctxt : context)
	        (pid  : int)
	=
      (* if pid isn't mapped, then there is a bug, so don't
	 try to handle Not_found *)
      IH.find ctxt.cPredicates pid


    (* Use the state and any extra conjuncts to build a precondition.
     * Add to the new state any preds that are implied, and combine
     * with the old state. *)
    let buildPreAndTest (ctxt  : context)      (* The context *)
	                (inss  : boolLat IH.t) (* The new in-state *)
	                (oldss : boolLat IH.t) (* The old out-state *)
	                (extra : exp list)     (* extra conjuncts *)
	                (dowp  : bool)         (* whether wp should be calc'd *)
	                (io    : instr option) (* instruction if wp is calc'd *)
	=
      let inss' = IH.copy inss in
      let pre =
	IH.fold (fun pid bl pre ->
	  (* XXX: do optimizations here. *)
	  match bl with
	  | Top | Bottom -> pre
	  | True -> BinOp(LAnd,pre,getPred ctxt pid,intType)
	  | False -> BinOp(LAnd,pre,UnOp(LNot,getPred ctxt pid,intType),intType))
	  inss one
      in
      let pre = 
	List.fold_left (fun ce e ->
	  BinOp(LAnd,ce,e,intType))
	  pre extra
      in
      IH.iter (fun pid bl ->
	let e = getPred ctxt pid in
	let wp = 
	  if dowp then 
	    match io with
	    | Some i -> begin 
		match weakestPrecondition i e with
		| Some wp -> wp
		| None -> raise (E.s "given instr had no wp\n")
	    end
	    | None -> raise (E.s "No instruction for wp calc.\n")
	  else 
	    e 
	in
	let oldbl = try IH.find oldss pid with Not_found -> Bottom in
	if S.isValid (S.transExp pre) (S.transExp wp) then
	  IH.replace inss' pid (combineBoolLat True oldbl)
	else if S.isValid (S.transExp pre) (S.transExp (UnOp(LNot,e,intType))) 
	then
	  IH.replace inss' pid (combineBoolLat False oldbl)
	else
	  IH.replace inss' pid Top)
	inss;
      inss'


    (* determine if inss => wp(i,e).
       If so, merge assertion of e with oldss *)
    let handleSetInstr (ctxt     : context)
	               (asserted : exp list)
                       (inss     : boolLat IH.t)
                       (i        : instr)
                       (oldss    : boolLat IH.t)
	=
      buildPreAndTest ctxt inss oldss asserted true (Some i)


    let handleCallInstr (ctxt     : context)
	                (asserted : exp list)
                        (inss     : boolLat IH.t)
                        (i        : instr)
                        (oldss    : boolLat IH.t)
	=
      match i with
      | Call(lvo, Lval(Var vi, NoOffset), el, _) -> begin
	  (* This function wasn't extern, so it has to be defined here *)
	  let fsig = IH.find ctxt.cFuncSigs vi.vid in
	  (* replace the formals in rpreds with the 
	     expressions given as arguments *)
	  let rpreds = 
	    List.map (fun e ->
	      List.fold_left2 (fun e vi ae ->
		substitute ae (* for *) (Var vi, NoOffset) (* in *) e)
		e fsig.fsFormals el)
	      fsig.fsRetPreds
	  in
	  let rpreds =
	    match lvo, fsig.fsReturn with
	    | None, None -> rpreds
	    | Some lv, Some rvi -> 
		(* replace fsig.fsReturn in rpreds with Lval(lv) *)
		List.map (fun e ->
		  substitute (Lval lv) (* for *) (Var rvi,NoOffset) (* in *) e)
		  rpreds
	    | _, _ -> raise (E.s "fsReturn is wrong in handleCallInstr\n")
	  in
	  buildPreAndTest ctxt inss oldss (rpreds@asserted) false None
      end
      | _ -> raise (E.s "Bad instr in handleCallInstr\n")


    (* move predicates containing globals or dereferences of exps
     * that alias exps in args or the return value to Top *)
    let fixForExternCall (ctxt : context)
	                 (inss : boolLat IH.t)
	                 (reto : lval option)
	                 (args : exp list)
	=
      let inss' = IH.copy inss in
      let args =
	match reto with
	| Some lv -> (Lval lv)::args
	| None -> args
      in
      IH.iter (fun pid bl ->
	let e = getPred ctxt pid in
	if List.exists (expHasAlias e) args ||
	   expContainsGlobal e
	then IH.replace inss' pid Top)
	inss;
      inss'


    let handleIl (ctxt : context)
                 (il   : instr list)
                 (ss   : stmtState)
	= 
      match ss with
      | StmState _ -> raise (E.s "StmState for instruction list?\n")
      | ILState hl -> begin
	  let newhl = 
	    List.fold_left (fun inss (i,oldss) ->
	      (* if i is a Set:
		 =>determine if inss => wp(i,e), if so, merge assertion of e
		 with oldss.
		 similarly for not(e).
		 if i is a Call:
		 =>assert things implied by predicates in E_r.
		 combine with oldss.
		 if i is inline assembly.
		 =>give up! (but fix later)
	       *)
	      if !ignoreInstruction i then inss else
	      let asserted = !getPredsFromInstr i in
	      match i with
	      | Set(_,_,_) ->
		  (handleSetInstr ctxt asserted (List.hd inss) i oldss)::inss
	      | Call(lvo,Lval(Var vi,NoOffset),el,l)
		when not(vi.vstorage = Extern) ->
		  (handleCallInstr ctxt asserted (List.hd inss) i oldss)::inss
	      | Call(lvo,_,el,_) -> begin
		  (* There are 2 cases:
		     1. no side-effects: assert consequences of things in asserted
		     2. side-effects: move predicates with globals or
		        which contain dereferences of 
		        aliases of pointer arguments to Top, assert
		        consequences of things in asserted *)
		  if !instrHasNoSideEffects i then
		    let inhd = List.hd inss in
		    (buildPreAndTest ctxt inhd oldss asserted false None)::inss
		  else
		    let inhd = fixForExternCall ctxt (List.hd inss) lvo el in
		    (buildPreAndTest ctxt inhd oldss asserted false None)::inss
	      end
	      | Asm(_,_,_,_,_,_) -> begin
		  (* all go to top for now *)
		  let inss' = IH.copy (List.hd inss) in
		  IH.iter (fun pid _ -> IH.replace inss' pid Top) inss';
		  inss'::inss
	      end) [(List.hd hl)] (List.combine il (List.tl hl))
	  in
	  ILState(List.rev newhl)
      end

    let handleStmt (ctxt : context) 
                   (stm  : stmt)
                   (ss   : stmtState)
	= 
      match stm.skind with
      | Instr il -> handleIl ctxt il ss
      | _ -> ss

    (* add the set of things implied by e to ss *)
    let handleBranch (ctxt : context)
                     (e    : exp)
	             (ss   : stmtState)
	=
      (* go through each of the predicates and assert the
       * ones that are implied by the condition *)
      let inss = 
	match ss with
	| ILState hl -> List.hd hl
	| StmState h -> h 
      in
      StmState(buildPreAndTest ctxt inss (IH.create 16) [e] false None)


    let rec listInit n x =
      if n = 0 then [] else x::(listInit (n-1) x)

    let currentContext = emptyContext()
    module PredFlow =
      struct
	let name = "Predicate Flow"

	let debug = debug

	type t = stmtState

	let copy ss = match ss with
	| ILState hl -> begin
	    ILState(List.map (fun h -> IH.copy h) hl)
	end
	| StmState h -> StmState(IH.copy h)

	let stmtStartData = IH.create 100

	let pretty () ss = match ss with
	| ILState hl -> begin
	    line ++ seq line (fun h ->
	      seq line (fun (pid,bl) ->
		text "PF: pid: " ++ num pid ++ text ": " ++
		  (d_bl () bl)) (IH.tolist h)) hl
	end
	| StmState h -> begin
	    line ++ seq line (fun (pid,bl) ->
	      text "PF: pid: " ++ num pid ++ text ": " ++
		(d_bl () bl)) (IH.tolist h)
	end

	let computeFirstPredecessor stm ss =
	  let h = 
	    match ss with
	    | ILState hl -> List.hd (List.rev hl)
	    | StmState h -> h
	  in
	  match stm.skind with
	  | Instr il -> 
	      (* +1 so that we have the state *into* the first instruction
		 at the head of the list *)
	      ILState(listInit ((List.length il) + 1) h)
	  | _ -> StmState h

	let combinePredecessors (stm:stmt) ~(old:t) (ss:t) =
	  match old,ss with
	  | ILState hlold, ILState hlnew -> begin
	      if hl_equals hlold hlnew then None else
	      Some(ILState(hl_combine hlold hlnew))
	  end
	  | StmState hold, StmState hnew -> begin
	      if h_equals hold hnew then None else
	      Some(StmState(h_combine hold hnew))
	  end
	  | _, _ -> raise (E.s "PredFlow: old and new states different type\n")

	(* Take care of everything in doStmt and doGuard *)
	let doInstr i ss = DF.Default

	let doStmt stm ss = DF.SUse(handleStmt currentContext stm ss)

	let doGuard e ss = DF.GUse(handleBranch currentContext e ss)

	let filterStmt stm = true

      end

    module PA = DF.ForwardsDataFlow(PredFlow)

    let registerFile (f : file) : unit =
      (* add the function signatures to currentContext *)
      currentContext.cFuncSigs <- makeFunctionSigs f

    let makePreds (el : exp list) : unit =
      IH.clear currentContext.cPredicates;
      ExpIntHash.clear currentContext.cRPredMap;
      List.iter (fun e ->
	if not(ExpIntHash.mem currentContext.cRPredMap e) then begin
	  let pid = currentContext.cNextPred in
	  currentContext.cNextPred <- pid + 1;
	  IH.add currentContext.cPredicates pid e;
	  ExpIntHash.add currentContext.cRPredMap e pid
	end)
	el

    let makeAllBottom (eh : exp IH.t) : boolLat IH.t =
      let blh = IH.create 100 in
      IH.iter (fun pid _ ->
	IH.add blh pid Bottom)
	eh;
      blh

    let analyze (fd : fundec) : unit =
      (* take the AllPreds out of the function signature and
	 add them to the context *) 
	let fs = 
	  try IH.find currentContext.cFuncSigs fd.svar.vid
	  with Not_found -> raise (E.s "run registerFile on file first\n")
	in
	makePreds fs.fsAllPreds;
	try
	  let slst = fd.sbody.bstmts in
	  let first_stm = List.hd slst in
	  IH.clear PredFlow.stmtStartData;
	  let firstData = makeAllBottom currentContext.cPredicates in
	  IH.add PredFlow.stmtStartData first_stm.sid (StmState firstData);
	  PA.compute [first_stm]
	with Failure "hd" -> if !debug then ignore(E.log "fn w. no stmts?\n")
	| Not_found -> if !debug then ignore(E.log "no data for first_stm?\n")

    let getPAs sid =
      try Some(IH.find PredFlow.stmtStartData sid)
      with Not_found -> None

    class paVisitorClass = object(self)
      inherit nopCilVisitor

      val mutable sid = -1

      val mutable pa_dat_lst = []

      val mutable cur_pa_dat = None

      method vstmt stm =
	sid <- stm.sid;
	match getPAs sid with
	| None -> begin
	    if !debug then
	      ignore(E.log "paVis: stm %d has no data\n" sid);
	    cur_pa_dat <- None;
	    DoChildren
	end
	| Some ss -> begin
	    match ss with
	    | StmState eh -> begin
		cur_pa_dat <- Some eh;
		DoChildren
	    end
	    | ILState ehl -> begin
		pa_dat_lst <- ehl;
		DoChildren
	    end
	end

      method vinst i =
	try
	  let data = List.hd pa_dat_lst in
	  cur_pa_dat <- Some data;
	  pa_dat_lst <- List.tl pa_dat_lst;
	  DoChildren
	with Failure "hd" -> DoChildren

      method get_cur_dat () = cur_pa_dat

    end

    let query (blh : boolLat IH.t) (e : exp) : boolLat =
      try
	let pid = ExpIntHash.find currentContext.cRPredMap e in
	IH.find blh pid
      with Not_found -> Bottom

  end
