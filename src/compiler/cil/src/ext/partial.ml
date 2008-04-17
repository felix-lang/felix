(* See copyright notice at the end of the file *)
(*****************************************************************************
 * Partial Evaluation & Constant Folding
 *
 * Soundness Assumptions:
 * (1) Whole program analysis. You may call functions that are not defined
 *     (e.g., library functions) but they may not call back.
 * (2) An undefined function may not return the address of a function whose
 *     address is not already taken in the code I can see.
 * (3) A function pointer call may only call a function that has its
 *     address visibly taken in the code I can see.
 *
 * (More assumptions in the comments below)
 *****************************************************************************)
open Cil
open Pretty

(*****************************************************************************
 * A generic signature for Alias Analysis information. Used to compute the
 * call graph and do symbolic execution.
 ****************************************************************************)
module type AliasInfo =
  sig
    val setup : Cil.file -> unit
    val can_have_the_same_value : Cil.exp -> Cil.exp -> bool
    val resolve_function_pointer : Cil.exp -> Cil.fundec list
  end

(*****************************************************************************
 * A generic signature for Symbolic Execution execution algorithms. Such
 * algorithms are used below to perform constant folding and dead-code
 * elimination. You write a "basic-block" symex algorithm, we'll make it
 * a whole-program CFG-pruner.
 ****************************************************************************)
module type Symex =
  sig
    type t (* the type of a symex algorithm state object *)
    val empty : t                           (* all values unknown *)
    val equal : t -> t -> bool              (* are these the same? *)
    val assign : t -> Cil.lval -> Cil.exp  -> (Cil.exp * t)
      (* incorporate an assignment, return the RHS *)
    val unassign : t -> Cil.lval -> t
      (* lose all information about the given lvalue: assume an
       * unknown external value has been assigned to it *)
    val assembly : t -> Cil.instr -> t      (* handle ASM *)
    val assume : t -> Cil.exp -> t          (* incorporate an assumption *)
    val evaluate : t -> Cil.exp -> Cil.exp  (* symbolic evaluation *)
    val join : (t list) -> t                (* join a bunch of states *)
    val call : t -> Cil.fundec -> (Cil.exp list) -> (Cil.exp list * t)
      (* we are calling the given function with the given actuals *)
    val return : t -> Cil.fundec -> t
      (* we are returning from the given function *)
    val call_to_unknown_function : t -> t
      (* throw away information that may have been changed *)
    val debug : t -> unit
  end

(*****************************************************************************
 * A generic signature for whole-progam call graphs.
 ****************************************************************************)
type callGraphNode = {
          fd       : Cil.fundec;
  mutable calledBy : Cil.fundec list;
  mutable calls    : Cil.fundec list
}

type callNodeHash = (Cil.varinfo, callGraphNode) Hashtbl.t

module type CallGraph =
  sig
    val compute : Cil.file -> callNodeHash
    val can_call : callNodeHash -> Cil.fundec -> Cil.fundec list
    val can_be_called_by : callNodeHash -> Cil.fundec -> Cil.fundec list
    val fundec_of_varinfo : callNodeHash -> Cil.varinfo -> Cil.fundec
  end

module type CallGraph' =
  sig
    type t (* the type of a call graph *)
    val compute : Cil.file -> t (* file for which we compute the graph *)
    val can_call : t -> Cil.fundec -> Cil.fundec list
    val can_be_called_by : t -> Cil.fundec -> Cil.fundec list
    val fundec_of_varinfo : t -> Cil.varinfo -> Cil.fundec
  end

(*****************************************************************************
 * My cheap-o Alias Analysis. Assume all expressions can have the same
 * value and any function with its address taken can be the target of
 * any function pointer.
 *
 * Soundness Assumptions:
 * (1) Someone must call "find_all_functions_with_address_taken" before the
 *     results are valid. This is already done in the code below.
 ****************************************************************************)
module EasyAlias : AliasInfo =
struct
  let all_functions_with_address_taken = ref []

  let find_all_functions_with_address_taken (f : Cil.file) =
    iterGlobals
      f
      (function
           GFun (fd, _) ->
             if fd.svar.vaddrof then
               all_functions_with_address_taken :=
                 fd :: !all_functions_with_address_taken
         | _ -> ())

  let setup f = find_all_functions_with_address_taken f

  let can_have_the_same_value e1 e2 = true

  let resolve_function_pointer e1 = !all_functions_with_address_taken
end

(*****************************************************************************
 * Alias analysis using CIL's Ptranal feature.
 ****************************************************************************)
module PtranalAlias : AliasInfo =
  struct
    let setup f = EasyAlias.setup f

    let can_have_the_same_value e1 e2 =
      try Ptranal.may_alias e1 e2
      with Not_found -> true

    let resolve_function_pointer e1 =
      try Ptranal.resolve_funptr e1
      with Not_found -> EasyAlias.resolve_function_pointer e1
  end

(*****************************************************************************
 * My particular method for computing the Call Graph.
 ****************************************************************************)
module EasyCallGraph = functor (A : AliasInfo) ->
struct
  let cgCreateNode cg fundec =
    let newnode = {
      fd = fundec;
      calledBy = [];
      calls = []
    } in
      Hashtbl.add cg fundec.svar newnode

  let cgFindNode cg svar = Hashtbl.find cg svar

  let cgAddEdge cg caller callee =
    try
      let n1 = cgFindNode cg caller in
      let n2 = cgFindNode cg callee in
        n1.calls <- n2.fd :: n1.calls;
        n1.calledBy <- n1.fd :: n1.calledBy
    with _ -> ()

  class callGraphVisitor cg =
  object
    inherit nopCilVisitor

    val the_fun = ref None

    method vinst i =
      begin
        match i with
            Call (_, Lval (Var callee, NoOffset), _, _) ->
              begin
                (* known function call *)
                match !the_fun with
                    None -> failwith "callGraphVisitor: call outside of any function"
                  | Some enclosing -> cgAddEdge cg enclosing callee
              end
          | Call (_, e, _, _) ->
              begin
                (* unknown function call *)
                match !the_fun with
                    None -> failwith "callGraphVisitor: call outside of any function"
                  | Some enclosing ->
                      List.iter
                        (fun possible_target_fd ->
                           cgAddEdge cg enclosing possible_target_fd.svar)
                        (A.resolve_function_pointer e)
              end
          | _ -> ()
      end;
      SkipChildren

    method vfunc f =
      the_fun := Some f.svar;
      DoChildren
  end

  let compute (f : Cil.file) =
    let cg = Hashtbl.create 511 in
      iterGlobals
        f
        (function GFun (fd, _) -> cgCreateNode cg fd
           | _ -> ());
      visitCilFileSameGlobals (new callGraphVisitor cg) f;
      cg

  let can_call cg fd =
    let n = cgFindNode cg fd.svar in n.calls

  let can_be_called_by cg fd =
    let n = cgFindNode cg fd.svar in n.calledBy

  let fundec_of_varinfo cg vi =
    let n = cgFindNode cg vi in n.fd
end (* END OF: module EasyCallGraph *)

(*****************************************************************************
 * Necula's Constant Folding Strategem (re-written to be applicative)
 *
 * Soundness Assumptions:
 * (1) Inline assembly does not affect constant folding.
 ****************************************************************************)
module NeculaFolding = functor (A : AliasInfo) ->
struct
  module IntMap = Map.Make (struct
                              type t = int
                              let compare x y = x - y
                            end)

  (* Register file. Maps identifiers of local variables to expressions.
   * We also remember if the expression depends on memory or depends on
   * variables that depend on memory *)
  type reg = {
    rvi : varinfo;
    rval : exp;
    rmem : bool
  }

  type t = reg IntMap.t

  let empty = IntMap.empty

  let equal t1 t2 = (compare t1 t2 = 0) (* use OCAML here *)

  let dependsOnMem = ref false

  (* Rewrite an expression based on the current register file *)
  class rewriteExpClass (regFile : t) =
  object
    inherit nopCilVisitor
    method vexpr = function
        Lval (Var v, NoOffset) ->
          begin
            try
              let defined = IntMap.find v.vid regFile in
                if defined.rmem then dependsOnMem := true;
                match defined.rval with
                    Const x -> ChangeTo defined.rval
                  | _ -> DoChildren
            with Not_found -> DoChildren
          end
      | Lval (Mem _, _) ->
          dependsOnMem := true;
          DoChildren
      | _ -> DoChildren
  end

  (* Rewrite an expression and return the new expression along with an
   * indication of whether it depends on memory *)
  let rewriteExp r (e : exp) : exp * bool =
    dependsOnMem := false;
    let e' = constFold true (visitCilExpr (new rewriteExpClass r) e) in
      e', !dependsOnMem

  let eval r e =
    let new_e, _depends = rewriteExp r e in
      new_e

  let setMemory regFile =
    (* Get a list of all mappings that depend on memory *)
    let depids = ref [] in
      IntMap.iter (fun id v -> if v.rmem then depids := id :: !depids) regFile;
      (* And remove them from the register file *)
      List.fold_left (fun acc id -> IntMap.remove id acc) regFile !depids

  let setRegister regFile (v : varinfo) ((e, b) : exp * bool) =
    IntMap.add v.vid {rvi = v; rval = e; rmem = b} regFile

  let resetRegister regFile (id : int) =
    IntMap.remove id regFile

  class findLval lv contains =
  object
    inherit nopCilVisitor
    method vlval l =
      if Util.equals l lv then
        begin
          contains := true;
          SkipChildren
        end
      else
        DoChildren
  end

  let removeMappingsThatDependOn regFile l =
    (* Get a list of all mappings that depend on l *)
    let depids = ref [] in
      IntMap.iter
        (fun id reg ->
           let found = ref false in
             ignore (visitCilExpr (new findLval l found) reg.rval);
             if !found then depids := id :: !depids)
        regFile;
      (* And remove them from the register file *)
      List.fold_left (fun acc id -> IntMap.remove id acc) regFile !depids

  let assign r l e =
    let newe, b = rewriteExp r e in
    let r' =
      match l with
          Var v, NoOffset ->
            let r'' = setRegister r v (newe, b) in
              removeMappingsThatDependOn r'' l
        | Mem _, _ -> setMemory r
        | _ -> r
    in newe, r'

  let unassign r l =
    let r' =
      match l with
          Var v, NoOffset ->
            let r'' = resetRegister r v.vid in
              removeMappingsThatDependOn r'' l
        | Mem _, _ -> setMemory r
        | _ -> r
    in r'

  let assembly r i = r (* no-op in Necula-world *)

  let assume r e = r (* no-op in Necula-world *)

  let evaluate r e =
    let newe, _ = rewriteExp r e in
      newe

  (* Join two symex states *)
  let join2 (r1 : t) (r2 : t) =
    let keep = ref [] in
      IntMap.iter
        (fun id reg ->
           try
             let reg' = IntMap.find id r2 in
               if Util.equals reg'.rval reg.rval && reg'.rmem = reg.rmem then
                 keep := (id, reg) :: !keep
           with _ -> ())
        r1;
      List.fold_left
        (fun acc (id, v) -> IntMap.add id v acc)
        IntMap.empty
        !keep

  let join (lst : t list) =
    match lst with
        [] -> failwith "empty list"
      | r :: tl ->
          List.fold_left (fun (acc : t) (elt : t) -> join2 acc elt) r tl

  let call r fd el =
    let new_arg_list = ref [] in
    let final_r =
      List.fold_left2
        (fun r vi e ->
           let newe, r' = assign r (Var vi, NoOffset) e in
             new_arg_list := newe :: !new_arg_list;
             r')
        r
        fd.sformals el
    in
      (List.rev !new_arg_list), final_r

  let return r fd =
    let filter_out a_predicate a_map =
      IntMap.fold
        (fun k v a -> if a_predicate k v then a else IntMap.add k v a)
        IntMap.empty
        a_map
    and formals_and_locals = fd.sformals @ fd.slocals
    in
      filter_out
        (fun k v -> List.mem v.rvi formals_and_locals)
        r

  let call_to_unknown_function r =
    setMemory r

  let debug r =
    IntMap.iter
      (fun key reg ->
         ignore (Pretty.printf "%s <- %a (%b)@!"
                   reg.rvi.vname d_exp reg.rval reg.rmem))
      r
end (* END OF: NeculaFolding *)

(*****************************************************************************
 * A transformation to make every function call end its statement. So
 * { x=1; Foo(); y=1; }
 * becomes at least:
 * { { x=1; Foo(); }
 *   { y=1; } }
 * But probably more like:
 * { { x=1; } { Foo(); } { y=1; } }
 ****************************************************************************)
let rec contains_call il =
  match il with
      [] -> false
    | Call _ :: tl -> true
    | _ :: tl -> contains_call tl

class callBBVisitor =
object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
        Instr il when contains_call il ->
          begin
            let list_of_stmts =
              List.map (fun one_inst -> mkStmtOneInstr one_inst) il in
            let block = mkBlock list_of_stmts in
              ChangeDoChildrenPost
                (s, (fun _ -> s.skind <- Block block; s))
          end
      | _ -> DoChildren

  method vvdec _ = SkipChildren
  method vexpr _ = SkipChildren
  method vlval _ = SkipChildren
  method vtype _ = SkipChildren
end

let calls_end_basic_blocks f =
  let thisVisitor = new callBBVisitor in
    visitCilFileSameGlobals thisVisitor f

(*****************************************************************************
 * A transformation that gives each variable a unique identifier.
 ****************************************************************************)
class vidVisitor = object
  inherit nopCilVisitor
  val count = ref 0

  method vvdec vi =
    vi.vid <- !count;
    incr count;
    SkipChildren
end

let globally_unique_vids f =
  let thisVisitor = new vidVisitor in
    visitCilFileSameGlobals thisVisitor f

(*****************************************************************************
 * The Weimeric Partial Evaluation Data-Flow Engine
 *
 * This functor performs flow-sensitive, context-insensitive whole-program
 * data-flow analysis with an eye toward partial evaluation and constant
 * folding.
 *
 * Toposort the whole-program inter-procedural CFG to compute
 *  (1) the number of actual predecessors for each statement
 *  (2) the global toposort ordering
 *
 * Perform standard data-flow analysis (joins, etc) on the ICFG until you
 * hit a fixed point. If this changed the structure of the ICFG (by
 * removing an IF-branch or an empty function call), redo the whole thing.
 *
 * Soundness Assumptions:
 * (1) A "call instruction" is the last thing in its statement.
 *       Use "calls_end_basic_blocks" to get this. cil/src/main.ml does
 *       this when you pass --makeCFG.
 * (2) All variables have globally unique identifiers.
 *       Use "globally_unique_vids" to get this. cil/src/main.ml does
 *       this when you pass --makeCFG.
 * (3) This may not be a strict soundness requirement, but I wrote this
 *       assuming that the input file has all switch/break/continue
 *       statements removed.
 ****************************************************************************)
module MakePartial =
  functor (S : Symex) ->
    functor (C : CallGraph) ->
      functor (A : AliasInfo) ->
struct
  let debug = false

  (* Sets of {c goto}-targets *)
  module LabelSet =
    Set.Make (struct
                type t = label
                let compare x y =
                  match x, y with
                      Label (name1, _, _), Label (name2, _, _) ->
                        String.compare name1 name2
                    | _, _ -> 0
              end)

  (* We keep this information about every statement. Ideally this should
   * be put in the stmt itself, but CIL doesn't give us space. *)
  type sinfo = { (* statement info *)
    incoming_state : (int, S.t) Hashtbl.t;
    (* mapping from stmt.sid to Symex.state *)
    reachable_preds : (int, bool) Hashtbl.t;
    (* basically a set of all of the stmt.sids that can really
     * reach this statement *)
    mutable last_used_state : S.t option;
    (* When we last did the Post () of this statement, what
     * incoming state did we use? If our new incoming state is
     * the same, we don't have to do it again. *)
    mutable priority : int;
    (* Whole-program toposort priority. High means "do me first".
     * The first stmt in "main()" will have the highest priority.
     *)
  }

  let sinfo_ht = Hashtbl.create 511
  let clear_sinfo () = Hashtbl.clear sinfo_ht

  (* We construct sinfo nodes lazily: if you ask for one that isn't
   * there, we build it. *)
  let get_sinfo stmt =
    try
      Hashtbl.find sinfo_ht stmt.sid
    with _ ->
      let new_sinfo = {incoming_state = Hashtbl.create 3;
                       reachable_preds = Hashtbl.create 3;
                       last_used_state = None;
                       priority = (-1)} in
        Hashtbl.add sinfo_ht stmt.sid new_sinfo;
        new_sinfo

  (* Topological Sort is a DFS in which you assign a priority right as
   * you finished visiting the children. While we're there we compute
   * the actual number of unique predecessors for each statement. The CIL
   * information may be out of date because we keep changing the CFG by
   * removing IFs and whatnot. *)
  let toposort_counter = ref 1
  let add_edge s1 s2 =
    let si2 = get_sinfo s2 in
      Hashtbl.replace si2.reachable_preds s1.sid true

  let rec toposort c stmt =
    let si = get_sinfo stmt in
      if si.priority >= 0 then () (* already visited! *)
      else
        begin
          si.priority <- 0; (* currently visiting *)
          (* handle function calls in this basic block *)
          begin
            match stmt.skind with
                Instr il ->
                  List.iter
                    (fun i ->
                       let fd_list =
                         match i with
                             Call (_, Lval (Var vi, NoOffset), _, _) ->
                               begin
                                 try
                                   let fd = C.fundec_of_varinfo c vi in
                                     [fd]
                                 with e -> [] (* calling external function *)
                               end
                           | Call (_, e, _, _) ->
                               A.resolve_function_pointer e
                           | _ -> []
                       in
                         List.iter
                           (fun fd ->
                              if List.length fd.sbody.bstmts > 0 then
                                let fun_stmt = List.hd fd.sbody.bstmts in
                                  add_edge stmt fun_stmt;
                                  toposort c fun_stmt)
                           fd_list)
                    il
              | _ -> ()
          end;
          List.iter
            (fun succ -> add_edge stmt succ; toposort c succ)
            stmt.succs;
          si.priority <- !toposort_counter;
          incr toposort_counter
        end

    (* we set this to true whenever we eliminate an IF or otherwise
     * change the CFG *)
    let changed_cfg = ref false

    (* Partially evaluate / constant fold a statement. Basically this
     * just asks the Symex algorithm to evaluate the RHS in the current
     * state and then compute a new state that incorporates the
     * assignment.
     *
     * However, we have special handling for ifs and calls. If we can
     * evaluate an if predicate to a constant, we remove the if.
     *
     * If we are going to make a call to a function with an empty body,
     * we remove the function call. *)
    let partial_stmt c state stmt handle_funcall =
      let result =
        match stmt.skind with
            Instr il ->
              let state = ref state in
              let new_il =
                List.map
                  (fun i ->
                     if debug then
                       ignore (Pretty.printf "Instr %a@!" d_instr i);
                     match i with
                         Set (l, e, loc) ->
                           let e', state' = S.assign !state l e in
                             state := state';
                             [Set (l, e', loc)]
                       | Call (lo, Lval (Var vi, NoOffset), al, loc) ->
                           let result, know_retval =
                             try
                               let fd = C.fundec_of_varinfo c vi in
                                 match fd.sbody.bstmts with
                                     [] -> [], false (* no point in making this call *)
                                   | hd :: _tl ->
                                       if match hd.skind with
                                           Return (None, _loc) -> true
                                         | _ -> false then
                                             [], false (* no point in making this call *)
                                       else if match hd.skind with
                                           Return (Some ret_exp, _loc) ->
                                             isConstant (S.evaluate !state ret_exp)
                                         | _ -> false then
                                             match lo, hd.skind with
                                                 Some lv, Return (Some ret_exp, _loc) ->
                                                   let ret_exp', state' = S.assign !state lv ret_exp in
                                                     state := state';
                                                     [Set (lv, ret_exp', loc)], true (* replace call with constant *)
                                               | None, Return (Some _ret_exp, _loc) ->
                                                   failwith "partial_stmt: internal error"
                                               | _, _ -> [], false (* never reached *)
                                       else
                                         let al', state' = S.call !state fd al in
                                           handle_funcall stmt hd state';
                                           let state'' = S.return state' fd in
                                             state := state'';
                                             [Call (lo, Lval (Var vi, NoOffset), al', loc)], false
                             with e ->
                               let state'' = S.call_to_unknown_function !state in
                               let al' = List.map (S.evaluate !state) al in
                                 state := state'';
                                 [Call (lo, Lval (Var vi, NoOffset), al', loc)], false
                           in
                             (* handle return value *)
                             begin
                               match lo, know_retval with
                                   Some lv, false -> state := S.unassign !state lv
                                 | Some lv, true -> ()
                                 | None, _ -> ()
                             end;
                             result
                       | Call (lo, f, al, loc) ->
                           let al' = List.map (S.evaluate !state) al in
                             state := S.call_to_unknown_function !state;
                             begin
                               match lo with
                                   Some lv -> state := S.unassign !state lv
                                 | None -> ()
                             end;
                             [Call (lo, f, al', loc)]
                       | Asm _ ->
                           state := S.assembly !state i;
                           [i])
                  il in
                stmt.skind <- Instr (List.flatten new_il);
                if debug then
                  ignore (Pretty.printf "New Stmt is %a@!" d_stmt stmt);
                !state

          | If (e, b1, b2, loc) ->
              (* Answer whether block [b] contains labels that are
                 alive.  "Live" labels are actually targets of
                 [goto]-instructions {b outside} of [b]. *)
              let has_live_labels b =
                let gather_labels acc stmt =
                  List.fold_left (fun a x -> LabelSet.add x a) acc stmt.labels in
                let rec visit_block stmt_fun acc blk =
                  List.fold_left
                    (fun a x ->
                      let y = stmt_fun a x in
                        match x.skind with
                            Instr _
                          | Return _ | Goto _ | Break _ | Continue _ -> y
                          | If (_expr, then_block, else_block, _loc) ->
                              visit_block
                                stmt_fun
                                (visit_block stmt_fun y then_block)
                                else_block
                          | Switch (_expr, block, _stmt_list, _loc) ->
                              visit_block stmt_fun y block
                          | Loop (block, _loc, _opt_stmt1, _opt_stmt2) ->
                              visit_block stmt_fun y block
                          | Block block ->
                              visit_block stmt_fun y block
                          | TryFinally (block1, block2, _loc)
                          | TryExcept (block1, _, block2, _loc) ->
                              visit_block
                                stmt_fun
                                (visit_block stmt_fun y block1)
                                block2)
                    acc
                    blk.bstmts
                and gather_gotos acc stmt =
                  match stmt.skind with
                      Goto (stmt_ref, _loc) -> gather_labels acc !stmt_ref
                    | _ -> acc
                and transitive_closure ini_stmt =
                  let rec iter trace acc stmt =
                    List.fold_left
                      (fun (a_trace, a_stmt) s ->
                        if List.mem s.sid a_trace then (a_trace, a_stmt)
                        else iter (s.sid :: a_trace) (s :: a_stmt) s)
                      (trace, acc) (stmt.preds @ stmt.succs) in
                    List.sort (* sorting is unnecessary, but nice *)
                      (fun a b -> a.sid - b.sid)
                      (snd (iter [] [] ini_stmt)) in
                let block_labels = visit_block gather_labels LabelSet.empty b
                and block_gotos = visit_block gather_gotos LabelSet.empty b
                and all_gotos =
                  List.fold_left
                    (fun a x ->
                      match x.skind with
                          Goto (stmt_ref, _loc) -> gather_labels a !stmt_ref
                        | Block block -> visit_block gather_gotos a block
                        | _ -> a)
                    LabelSet.empty
                    (if b.bstmts = [] then []
                      else transitive_closure (List.hd b.bstmts))
                in
                  not (LabelSet.is_empty
                          (LabelSet.inter
                              (LabelSet.diff all_gotos block_gotos)
                              block_labels)) in
              (* helper function to remove "if"-branch [b] *)
              let remove stmt b =
                changed_cfg := true;
                match b.bstmts with
                    [] -> ()
                  | hd :: _tl ->
                      stmt.succs <- List.filter
                                      (fun succ -> succ.sid <> hd.sid)
                                      stmt.succs
              (* helper function to make a simplified "if"-statement block *)
              and mk_if_block b =
                let stmt = mkStmt (Block b) in
                  stmt.sid <- new_sid ();
                  Block {bstmts = [stmt]; battrs = []}
              (* logical falseness in C expressed in cilly's terms *)
              and is_false e = isZero e
              (* logical truth in C expressed in cilly's terms *)
              and is_true e =
                match isInteger e with
                    Some x -> x <> Int64.zero
                  | None -> false in
              (* evaluate expression and eliminate branches *)
              let e' = S.evaluate state e in
                if debug then
                  ignore (Pretty.printf "%a evals to %a\n" d_exp e d_exp e');
                if is_true e' then
                  begin
                    if has_live_labels b2 then
                      begin
                        () (* leave block alone *)
                      end
                    else
                      begin
                        if b2.bstmts = [] && b2.battrs = [] then
                          begin
                            stmt.skind <- Block b1;
                            match b1.bstmts with
                                [] -> ()
                              | hd :: _tl -> stmt.succs <- [hd]
                          end
                        else stmt.skind <- mk_if_block b1;
                        remove stmt b2
                      end
                  end
                else if is_false e' then
                  begin
                    if has_live_labels b1 then
                      begin
                        () (* leave block alone *)
                      end
                    else
                      begin
                        if b1.bstmts = [] && b1.battrs = [] then
                          begin
                            stmt.skind <- Block b2;
                            match b2.bstmts with
                                [] -> ()
                              | hd :: _tl -> stmt.succs <- [hd]
                          end
                        else stmt.skind <- mk_if_block b2;
                        remove stmt b1
                      end
                  end
                else stmt.skind <- If (e', b1, b2, loc);
                state

          | Return (Some e, loc) ->
              let e' = S.evaluate state e in
                stmt.skind <- Return (Some e', loc);
                state

          | Block b ->
              if debug && List.length stmt.succs > 1 then
                ignore (Pretty.printf "(%a) has successors [%a]@!"
                          d_stmt stmt
                          (docList ~sep:(chr '@') (d_stmt ()))
                          stmt.succs);
              state

          | _ -> state
      in result

    (* This is the main conceptual entry-point for the partial
     * evaluation data-flow functor.  *)
    let dataflow (file : Cil.file)           (* whole program *)
                 (c : callNodeHash)          (* control-flow graph *)
                 (initial_state : S.t)       (* any assumptions? *)
                 (initial_stmt : Cil.stmt) = (* entry point *)
      begin
        (* count the total number of statements in the program *)
        let num_stmts = ref 1 in
          iterGlobals
            file
            (function
                 GFun (fd, _) ->
                   begin
                     match fd.smaxstmtid with
                         Some i -> if i > !num_stmts then num_stmts := i
                       | None -> ()
                   end
               | _ -> ());
          if debug then
            Printf.printf "Dataflow: at most %d statements in program\n" !num_stmts;

          (* create a priority queue in which to store statements *)
          let worklist = Heap.create !num_stmts in

          let finished = ref false in
          let passes = ref 0 in

          (* add something to the work queue *)
          let enqueue caller callee state =
            let si = get_sinfo callee in
              Hashtbl.replace si.incoming_state caller.sid state;
              Heap.insert worklist si.priority callee
          in
            (* we will be finished when we complete a round of
             * data-flow that does not change the ICFG *)
            while not !finished do
              clear_sinfo ();
              incr passes;

              (* we must recompute the ordering and the predecessor
               * information because we may have changed it by removing
               * IFs *)
              if debug then
                Printf.printf "Dataflow: Topological Sorting & Reachability\n";
              toposort c initial_stmt;

              let initial_si = get_sinfo initial_stmt in
                Heap.insert worklist initial_si.priority initial_stmt;

                while not (Heap.is_empty worklist) do
                  let p, s = Heap.extract_max worklist in
                    if debug then
                      begin
                        ignore (Pretty.printf "Working on stmt %d (%a) %a@!"
                                  s.sid
                                  (docList ~sep:(chr ',' ++ break) (fun s -> dprintf "%d" s.sid))
                                  s.succs
                                  d_stmt s);
                        flush stdout;
                      end;
                    let si = get_sinfo s in

                    (* Even though this stmt is on the worklist, we
                     * may not have to do anything with it if the join
                     * of all of the incoming states is the same as the
                     * last state we used here. *)
                    let must_recompute, incoming_state =
                      begin
                        let list_of_incoming_states = ref [] in
                          Hashtbl.iter
                            (fun true_pred_sid b ->
                               let this_pred_state =
                                 try
                                   Hashtbl.find si.incoming_state true_pred_sid
                                 with _ ->
                                   (* this occurs when we're evaluating a statement and we
                                    * have not yet evaluated all of its predecessors (the
                                    * first time we look at a loop head, say). We must be
                                    * conservative. We'll come back later with better
                                    * information (as we work toward the fix-point). *)
                                   S.empty
                               in
                                 if debug then
                                   begin
                                     Printf.printf " Incoming State from %d\n" true_pred_sid;
                                     S.debug this_pred_state;
                                     flush stdout
                                   end;
                                 list_of_incoming_states :=
                                   this_pred_state :: !list_of_incoming_states)
                            si.reachable_preds;
                          let merged_incoming_state =
                            if !list_of_incoming_states = [] then
                              (* this occurs when we're looking at the
                               * first statement in "main" -- it has no
                               * preds *)
                              initial_state
                            else S.join !list_of_incoming_states
                          in
                            if debug then
                              begin
                                Printf.printf " Merged State:\n";
                                S.debug merged_incoming_state;
                                flush stdout
                              end;
                            let must_recompute =
                              match si.last_used_state with
                                  None -> true
                                | Some last -> not (S.equal merged_incoming_state last)
                            in must_recompute, merged_incoming_state
                      end
                    in
                      if must_recompute then
                        begin
                          si.last_used_state <- Some incoming_state;
                          let outgoing_state =
                            (* partially evaluate and optimize the
                             * statement *)
                            partial_stmt c incoming_state s enqueue in
                          let fresh_succs = s.succs in
                            (* touch every successor so that we will
                             * reconsider it *)
                            List.iter
                              (fun succ ->
                                 enqueue s succ outgoing_state)
                              fresh_succs;
                        end
                      else
                        begin
                          if debug then Printf.printf "No need to recompute.\n"
                        end
                done;
                if debug then
                  Printf.printf "Dataflow: Pass %d Complete\n" !passes;
                if !changed_cfg then
                  begin
                    if debug then
                      Printf.printf "Dataflow: Restarting (CFG Changed)\n";
                    changed_cfg := false
                  end
                else
                  finished := true
            done;
            if debug then
              Printf.printf "Dataflow: Completed (%d passes)\n" !passes
      end

    let simplify file c fd (assumptions : (Cil.lval * Cil.exp) list) =
      let starting_state =
        List.fold_left
          (fun s (l, e) -> let _e', s' = S.assign s l e in s')
          S.empty
          assumptions
      in
        dataflow file c starting_state (List.hd fd.sbody.bstmts)
end


module PartialAlgorithm :
sig
  val use_ptranal_alias : bool ref
  val setup_alias_analysis : Cil.file -> unit
  val compute_callgraph : Cil.file -> callNodeHash
  val simplify :
    Cil.file -> callNodeHash -> Cil.fundec -> (Cil.lval * Cil.exp) list -> unit
end
  =
struct
  (* Currently our partial-eval optimizer is built out of basically
   * nothing.  The (easy-)alias analysis is fake, the call graph is
   * cheap, and we're using George's old basic-block symex.  Still, it
   * works. *)

  (* Don't you love Functor application? *)
  module BasicCallGraph : CallGraph = EasyCallGraph (EasyAlias)
  module BasicSymex = NeculaFolding (EasyAlias)
  module BasicPartial =
    MakePartial (BasicSymex) (BasicCallGraph) (EasyAlias)

  module PtranalBasicCallGraph : CallGraph = EasyCallGraph (PtranalAlias)
  module PtranalBasicSymex = NeculaFolding (PtranalAlias)
  module PtranalBasicPartial =
    MakePartial (BasicSymex) (PtranalBasicCallGraph) (PtranalAlias)

  (* Select easy alias analysis or the fully-fledged one in module
   * Ptranal. *)
  let use_ptranal_alias = ref false

  let setup_alias_analysis f =
    if !use_ptranal_alias then PtranalAlias.setup f
    else EasyAlias.setup f

  let compute_callgraph f =
    if !use_ptranal_alias then PtranalBasicCallGraph.compute f
    else BasicCallGraph.compute f

  let simplify f c fd a =
    if !use_ptranal_alias then PtranalBasicPartial.simplify f c fd a
    else BasicPartial.simplify f c fd a
end

(* A very easy entry-point to partial evaluation/symbolic execution.
 * You pass the Cil file and a list of assumptions (lvalue, exp pairs
 * that should be treated as assignments that occur before the program
 * starts).
 *
 * We partially evaluate and optimize starting from root (usually
 * "main").  The Cil.file is modified in place. *)
let partial (f : Cil.file) (root : string) (assumptions : (Cil.lval * Cil.exp) list) =
  try
    PartialAlgorithm.setup_alias_analysis f;
    let c = PartialAlgorithm.compute_callgraph f in
      try
        if not (foldGlobals f (fun a x ->
                                 a ||
                                   match x with
                                       GFun (fd, _loc) ->
                                         if fd.svar.vname = root then
                                           begin
                                             PartialAlgorithm.simplify
                                               f c fd assumptions;
                                             true
                                           end
                                         else false
                                     | _ -> false)
                  false) then
          Printf.printf "Warning: root function \"%s\" not found\n" root
      with e ->
        begin
          Printf.printf "Error in DataFlow: %s\n" (Printexc.to_string e);
          raise e
        end
  with e ->
    begin
      Printf.printf "Error in Partial: %s\n" (Printexc.to_string e);
      raise e
    end

class globalConstVisitor =
object
  inherit nopCilVisitor

  val mutable init_const : (lval * exp) list = []

  method vglob g =
    let is_const vi = hasAttribute "const" (typeAttrs vi.vtype) in
      match g with
          GVar (vi, ii, loc) ->
            if is_const vi then
              match ii.init with
                  Some init ->
                    begin
                      match init with
                          SingleInit exp ->
                            begin
                              init_const <- (var vi, exp) :: init_const;
                              ChangeTo [GVar (vi,
                                              {init = Some (SingleInit (constFold true exp))},
                                              loc)]
                            end
                        | CompoundInit (_typ, _ini_list) -> SkipChildren
                    end
                | None -> SkipChildren (* uninitialized constant *)
            else SkipChildren
        | _ -> SkipChildren

  method get_initialized_constants = init_const
end

(* Assume global constants are initialized and feed this information
 * into the partial evaluator or treat constants as labels with unknown
 * values.  I am aware that we ought to distinguish between plain
 * constants and "volatile" constants. - cls *)
let initialized_constants = ref false

(* Name of function where we start to simplify *)
let root_fun = ref "main"

let do_feature_partial f =
  if not !Cilutil.makeCFG then
    Errormsg.s (Errormsg.error
                  "--dopartial: you must also specify --domakeCFG\n");
  if not !(Ptranal.feature.fd_enabled) &&
    !PartialAlgorithm.use_ptranal_alias then
    Errormsg.s (Errormsg.error
                  "--dopartial: you must also specify --doptranal\n");
  partial
    f
    !root_fun
    (if !initialized_constants then
       begin
         let gcv = new globalConstVisitor in
           visitCilFile (gcv :> Cil.cilVisitor) f;
           gcv#get_initialized_constants
       end
     else [])

let feature : featureDescr = {
  fd_name = "partial";
  fd_enabled = Cilutil.doPartial;
  fd_description = "interprocedural partial evaluation and constant folding";
  fd_extraopt = [
    ("--partial_global_const",
     Arg.Set initialized_constants,
     " treat global constants as initialized");
    ("--partial_no_global_const",
     Arg.Clear initialized_constants,
     " treat global constants as unknown values");
    ("--partial_root_function",
     Arg.String (fun name -> root_fun := name),
     (" where to start simplification"));
    ("--partial_use_easy_alias",
     Arg.Clear PartialAlgorithm.use_ptranal_alias,
     " to analyze pointers");
    ("--partial_use_ptranal_alias",
     Arg.Set PartialAlgorithm.use_ptranal_alias,
     " to analyze pointers (also see options of ptranal feature)")
  ];
  fd_doit = do_feature_partial;
  fd_post_check = false
}

(*
 *
 * Copyright (c) 2001-2002,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 *  Christoph L. Spiel  <Christoph.Spiel@partner.bmw.de>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
