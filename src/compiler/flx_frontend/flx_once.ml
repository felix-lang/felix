open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_fairy
open Flx_getset

let debug = Flx_getset.debug

exception CompleteFlow
exception IncompletePriorFlow
 

(* simulate labels at the start and end of a routine *)
let entry_label = -1
let exit_label = -2

let def_of_label bsym_table lidx = 
  match lidx with
  | _ when lidx = entry_label -> "Routine Entry"
  | _ when lidx = exit_label -> "Routine Exit"
  | _ -> 
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table lidx in
    Flx_bsym.id bsym ^
    "\nDefined at\n" ^
    Flx_srcref.long_string_of_src (Flx_bsym.sr bsym)

let str_of_label bsym_table lidx  = 
  match lidx with
  | _ when lidx = entry_label -> "Routine Entry"
  | _ when lidx = exit_label -> "Routine Exit"
  | _ -> 
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table lidx in
    Flx_bsym.id bsym

(* symmetric difference A \cup B - A \cap B *)
let symdiff a b = BidSet.diff (BidSet.union a b) (BidSet.inter a b)

type label_t = bid_t * BidSet.t
type stack_frame_t = {index: bid_t; code: augexe_t list; mutable visited: label_t list}

(* current position in a control path *)
type continuation_t = {current: augexe_t list; frame: stack_frame_t}
type stack_t = continuation_t list


(* FLOW SIMULATION.  How it works.

   The flow algorithm processes a single master function which has a set of local variables
   of interest by simulating execution starting at the function start.

   Initially, the parameters of the function are considered live, and all other local
   variables considered dead.

   Each function, including the master, has an associated stack frame with three components.
   * The index field is the identifier of the function in the symbol table. It is immutable.
   * The code field is the augmented list of instructions. It is immutable.

   * The visited field is a mutable field that contains a list of visited labels and the 
     liveness state when that label was first visited.

     When a label is first visited, it is added to the visited set along with the set
     of variables live at the time of the first visit.
  
     On subsequent visits, if the current liveness state equals the state at
     the time of the first visit, continuing to follow the same control path
     would have no impact since it is purely a function of the liveness
     state an current program position, so flow ceases for that path.
     
     If the current and prior liveness are not equal, the whole algorithm
     terminates with an inconsistent state error. 

     Consequently the algorithm only visits any instruction once, and therefore
     is linear in the number of instructions being processed.

     In order to track completion, a dummy label is inserted at the logical end of
     a function. All return instructions are assumed to jump to that dummy position
     prior to returning. The single exit points allows checking all control returns
     exit the function with a consistent state.

     In order to track recursion, a dummy label is also inserted at the start
     of the function. A recursive call is then considered like a jump.

     Flow in a single function is then tracked using a pair consisting of
     the stack frame, and a list of instructions following the current 
     position. This is the current continuation.

     Finally, the complete machine state consists of two separate variables:

     * A stack of continuations
     * the current live set

     The flow routine is a procedure which checks all flow from the starting
     machine state. Each instruction requires a modification to the live set,
     and an adjustment to the stack of continuations, follow by a recursive
     call. The recursion terminates on error or when the stack is successfully
     emptied.
 
     Calls to subroutines may occur in several places, with different
     liveness states. This means child instructions may be scanned
     more than once, however any routine only has a finite number of
     such calls so the algorithm remains linear. Indeed, all such calls
     could be eliminated by inlining.

     Control flow through parents and siblings of the master cannot influence the
     current live set because they do not have access to the masters local variables.
     Therefore, such calls are simply ignored, even if they might recursively
     call the master, because such calls logically create a whole new simulation,
     with the same initial conditions as the current analysis (all parameters live,
     all locals dead).

     However all calls to a descendant from the master or a descendant
     cannot be ignored.
*)





let rec rec_label stack index label =
  match stack with
  | [] -> None
  | {frame={index=bid; visited=visited}} :: _ when bid = index ->
    if List.mem_assoc label visited then Some (List.assoc label visited)
    else None
  | _ :: tail -> rec_label tail index label

let rec_entry stack index = rec_label stack index entry_label
let rec_exit stack index = rec_label stack index exit_label


let make_stack index exes =
  [{current=exes; frame= {index=index; code=exes; visited=[]}}]


let rec find_label augexes lab = 
  match augexes with
  | [] -> None
  | (BEXE_label (_,cand), data) :: tail when lab = cand -> Some tail
  | _ :: tail -> find_label tail lab

(* Fixup diagnostics later *)
let live bsym_table ix2chain sr bexe live {sets=set; gets=get} =
(*
print_endline ("Calculate liveness: old= " ^ Flx_bid.str_of_bidset old ^ ", gets=" ^
  Flx_bid.str_of_bidset get ^ " set=" ^ str_of_bidset set);
*)

  (* Order matters, check gets on old live values first *)
  let getdead = BidSet.diff get live in
  if not (BidSet.is_empty getdead) then begin
    print_endline ("Once error: Using uninitialised or already used once variable");
    print_endline (def_of_vars bsym_table ix2chain getdead);
    print_endline ("In instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
    print_endline ("Detected at" ^ Flx_srcref.long_string_of_src sr);
    assert false;
  end;

  (* kill used variables *)
  let live =BidSet.diff live get in 

  (* check we're not setting an already live variable *)
  let reset = BidSet.inter live set in
  if not (BidSet.is_empty reset) then begin
    print_endline ("Once error: Resetting live variable");
    print_endline (def_of_vars bsym_table ix2chain reset);
    print_endline ("In instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
    print_endline ("Detected at" ^ Flx_srcref.long_string_of_src sr);
    assert false;
  end;

  (* Add newly set variables *)
  let live = BidSet.union live set  in
(*
print_endline ("Newlive= " ^ Flx_bid.str_of_bidset live);
*)
  live

let check_liveness bsym_table ix2chain liveset : stack_t =
  if not (BidSet.is_empty liveset) then begin
    print_endline ("Once error: Once variables unused!");
    print_endline (def_of_vars bsym_table ix2chain liveset);
    assert false
  end;
  [] (* end of path *)

let check_reentry bsym_table ix2chain previous current label : stack_t=
  if previous <> current then begin
    print_endline ("Once error: Inconsistent liveness re-entering path at label " ^ 
      string_of_int label ^ " "^ str_of_label bsym_table label 
    );
    let diffset = BidSet.diff (BidSet.union previous current) (BidSet.inter previous current) in
    print_endline (def_of_vars bsym_table ix2chain diffset);
    assert false
  end;
  [] (* end of path *)

(* handle gotos, including nonlocal gotos *)
let rec goto bsym_table ix2chain (stack:stack_t) (liveness:BidSet.t) (label:bid_t) : stack_t =
  match stack with
  | [] -> check_liveness bsym_table ix2chain liveness  (* exit *)
  | head :: tail -> 
    match find_label head.frame.code label with
    | Some bexes -> 
      let visited = head.frame.visited in
      if List.mem_assoc label visited then
        let old_liveness = List.assoc label visited in
        check_reentry bsym_table ix2chain old_liveness liveness label (* path merge *)
      else
        let _ = head.frame.visited <- (label,liveness)::visited in (* continue recording liveness at label *)
        { head with current=bexes } :: tail
    | None -> goto bsym_table ix2chain tail liveness label (* look for label in parent *)

(* handle return instruction *)
let return bsym_table ix2chain (stack:stack_t) (liveness:BidSet.t) : stack_t =
  match stack with 
  | [] -> assert false
  | callee :: [] -> check_liveness bsym_table ix2chain liveness
  | callee :: caller :: chain ->
    let visited = callee.frame.visited in
    if List.mem_assoc exit_label visited then
      let old_liveness = List.assoc exit_label visited in
      check_reentry bsym_table ix2chain old_liveness liveness exit_label (* path merge *)
    else
      let _ = callee.frame.visited <- (exit_label,liveness)::visited in
      caller :: chain

(*********************************************************************) 
(* flow analysis *)
let rec flow seq counter make_augexes bsym_table chain2ix ix2chain master liveness stack : unit =
  let flow liveness stack = flow (seq + 1)  counter make_augexes bsym_table chain2ix ix2chain master liveness stack in
(*
print_endline ("Flow, stack depth = " ^ string_of_int (List.length stack));
*)
  match stack with 
  | [] -> 
if debug then print_endline ("flow: analysis done ***************"); 
    () (* finished *)

  | cc :: caller ->

  match cc.current with
  (* exists without usage *)
  | [] ->
if debug then print_endline ("flow: end of procedure or return");
    flow liveness (return bsym_table ix2chain stack liveness)

  | (bexe,deltalife) :: tail ->
  let sr = get_srcref bexe in
  let liveness = live bsym_table ix2chain sr bexe liveness deltalife in
  let next () =  flow liveness ({cc with current=tail} :: caller) in
  let final () = flow liveness (check_liveness bsym_table ix2chain liveness) in
  let abort () = flow liveness [] in

  let lno = Flx_srcref.first_line_no sr in
if debug then print_endline (string_of_int seq ^"@"^string_of_int lno^ " " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
if debug then print_endline (string_of_int seq ^"@"^string_of_int lno^ " " ^ string_of_vars bsym_table ix2chain liveness);
  match bexe with
  | BEXE_proc_return _ ->
if debug then print_endline ("flow: end of procedure or return");
    flow liveness (return bsym_table ix2chain stack liveness)

  | BEXE_fun_return _ ->
if debug then print_endline ("flow: function return");
    flow liveness (return bsym_table ix2chain stack liveness)

  (* exists with possible usage *)
  (* FIXME: could be a call to a child! *)
  | BEXE_jump (_,(BEXPR_closure (pidx,_),_),_)
  | BEXE_jump_direct (_,pidx,_,_) -> 
    if Flx_bsym_table.is_ancestor bsym_table pidx master then begin
      let bsym = Flx_bsym_table.find bsym_table pidx in
if debug then print_endline ("flow: descendant procedure call " ^ string_of_int pidx);
      begin match rec_entry stack pidx with
      | Some old_liveness -> 
if debug then print_endline ("flow: recursive procedure re-entry " ^ string_of_int pidx);
        (* dummy path termination *)
        flow liveness (check_reentry bsym_table ix2chain old_liveness liveness entry_label);

        (* continue past recursive call now *)
        begin match rec_exit stack pidx with
        | Some liveness ->
if debug then print_endline ("flow: recursive procedure returned " ^ string_of_int pidx);
          flow liveness ({cc with current=[]} :: caller) (* simulate return *)
        | None ->
if debug then print_endline ("flow: recursive procedure incomplete flow " ^ string_of_int pidx);
          raise IncompletePriorFlow
        end

      | None ->
        let bbdcl = Flx_bsym.bbdcl bsym in
        begin match bbdcl with
        | BBDCL_fun (prop, bvs, ps, res, effects, bexes) ->
          let continuation = {cc with current=[]} in (* simulate return *)
          let augexes = make_augexes bexes in 
          let new_frame = {index=pidx; code=augexes; visited=[entry_label,liveness] } in
          let entry = {current=augexes; frame=new_frame} in
          let stack = entry :: continuation :: caller in
if debug then print_endline ("flow: procedure initial entry " ^ string_of_int pidx);
          flow liveness stack 

        (* must  be external function *)
        | _ -> 
if debug then print_endline ("flow: external procedure " ^ string_of_int pidx);
          flow liveness (return bsym_table ix2chain stack liveness)
        end
      end
    end else begin
if debug then print_endline ("flow: non-descendant procedure call " ^ string_of_int pidx);
      flow liveness (return bsym_table ix2chain stack liveness)
    end


  | BEXE_call_with_trap (_,(BEXPR_closure(pidx,_),_),_)
  | BEXE_call (_,(BEXPR_closure(pidx,_),_),_)
  | BEXE_call_stack (_,pidx,_,_) 
  | BEXE_call_direct (_,pidx,_,_) ->
    if Flx_bsym_table.is_ancestor bsym_table pidx master then begin
      let bsym = Flx_bsym_table.find bsym_table pidx in
if debug then print_endline ("flow: descendant procedure call " ^ string_of_int pidx);
      begin match rec_entry stack pidx with
      | Some old_liveness -> 
if debug then print_endline ("flow: recursive procedure re-entry " ^ string_of_int pidx);
        (* dummy path termination *)
        flow liveness (check_reentry bsym_table ix2chain old_liveness liveness entry_label);

        (* continue past recursive call now *)
        begin match rec_exit stack pidx with
        | Some liveness ->
if debug then print_endline ("flow: recursive procedure returned " ^ string_of_int pidx);
          flow liveness ({cc with current=tail} :: caller)
        | None ->
if debug then print_endline ("flow: recursive procedure incomplete flow " ^ string_of_int pidx);
          raise IncompletePriorFlow
        end

      | None ->
        let bbdcl = Flx_bsym.bbdcl bsym in
        begin match bbdcl with
        | BBDCL_fun (prop, bvs, ps, res, effects, bexes) ->
          let continuation = {cc with current=tail} in
          let augexes = make_augexes bexes in 
          let new_frame = {index=pidx; code=augexes; visited=[entry_label,liveness] } in
          let entry = {current=augexes; frame=new_frame} in
          let stack = entry :: continuation :: caller in
if debug then print_endline ("flow: procedure initial entry " ^ string_of_int pidx);
          flow liveness stack 

        (* must  be external function *)
        | _ -> 
if debug then print_endline ("flow: external procedure " ^ string_of_int pidx);
          next()
        end
      end
    end else begin
if debug then print_endline ("flow: non-descendant procedure call " ^ string_of_int pidx);
      next()
    end


  | BEXE_goto (_,lidx) ->
if debug then print_endline ("flow: unconditional goto " ^ str_of_label bsym_table lidx);
    flow liveness (goto bsym_table ix2chain stack liveness lidx)

  | BEXE_ifgoto (_,c,lidx) ->
if debug then print_endline ("flow: conditional goto " ^ str_of_label bsym_table lidx);
    begin try
      (* branch not taken case *)
      next();
if debug then print_endline ("flow: branch not taken done");
      raise CompleteFlow
    with 
    | IncompletePriorFlow ->
      begin
if debug then print_endline ("flow: branch not taken failed, trying branch taken");
        (* branch taken case *)
        flow liveness (goto bsym_table ix2chain stack liveness lidx);
if debug then print_endline ("flow: branch taken ok");
        (* branch not taken case *)
if debug then print_endline ("flow: branch not taken failed, branch taken ok, retry branch not taken");
        next ();
if debug then print_endline ("flow: branch not taken retry done");
      end
    | CompleteFlow ->
if debug then print_endline ("flow: trying branch taken");
      (* branch taken case *)
      flow liveness (goto bsym_table ix2chain stack liveness lidx);
if debug then print_endline ("flow: branch taken ok");
    end;
if debug then print_endline ("flow: conditional branch ok")

      
  | BEXE_label (_,lidx) ->
if debug then print_endline ("flow: label " ^ string_of_int lidx);
    let visited = cc.frame.visited in
    if List.mem_assoc lidx visited then begin
if debug then print_endline ("flow: merge at label  " ^ str_of_label bsym_table lidx);
      let old_liveness = List.assoc lidx visited in
      flow liveness (check_reentry bsym_table ix2chain old_liveness liveness lidx) (* path merge *)
    end else begin
if debug then print_endline ("flow: first entry at label  " ^ str_of_label bsym_table lidx);
      let _ = cc.frame.visited <- (lidx,liveness)::visited in (* continue recording liveness at label *)
      next();
    end

  | BEXE_jump (_,_,_)  ->
    flow liveness (return bsym_table ix2chain stack liveness)

  | BEXE_nonreturn_code (_,_,_) ->
    abort ()

  (* FIXME: temporary hack for concept testing, handle all the
    other cases now!

    These warnings should be issued only if there is actually
    something uniq around for it to matter!

    QUESTION is how did we get here if there isn't??
  *)
  | BEXE_yield _ 
    -> 
    if debug then print_endline ("WARNING: Once analysis can't handle yield yet"); 
    (* Hack so stuff doesn't actually crash! *)
    flow liveness (return bsym_table ix2chain stack liveness)

  | BEXE_halt _
    -> 
    final ()

  (* NOTE: we should only need one of these patterns, FIXME unravel *)
  | BEXE_assign (sr, v,(BEXPR_apply ((BEXPR_closure (pidx,_),_),arg),_)) 
  | BEXE_assign (sr, v,(BEXPR_apply_direct (pidx,_,arg),_)) 
  | BEXE_assign (sr, v,(BEXPR_apply_stack (pidx,_,arg),_)) 
    ->
    (* We treat this as a procedure call on f for the moment! *)
    (* print_endline ("Found assign v = f a!"); *)

    (* NOTE: recursion not handled yet *)
    if Flx_bsym_table.is_ancestor bsym_table pidx master then begin
      let bsym = Flx_bsym_table.find bsym_table pidx in
      let bbdcl = Flx_bsym.bbdcl bsym in
      begin match bbdcl with
      | BBDCL_fun (prop, bvs, ps, res, effects, bexes) ->
        let continuation = {cc with current=tail} in
        let augexes = make_augexes bexes in 
        let new_frame = {index=pidx; code=augexes; visited=[entry_label,liveness] } in
        let entry = {current=augexes; frame=new_frame} in
        let stack = entry :: continuation :: caller in
if debug then print_endline ("flow: SPECIAL function apply initial entry " ^ string_of_int pidx);
        flow liveness stack 
      | _ -> 
if debug then print_endline ("flow: external function " ^ string_of_int pidx);
        next();
      end
    end 
    else
      next()

(* We can't handle this so pretend its final *)
  | BEXE_cgoto _
  | BEXE_ifcgoto _
    -> 
    if debug then print_endline ("WARNING: Once analysis can't handle computed gotos"); 
    flow liveness (return bsym_table ix2chain stack liveness)
(*
    print_endline ("Once analysis can't handle computed gotos"); 
    assert false
*)

  (* handled by diffset *)
  | BEXE_storeat _
  | BEXE_assign _
  | BEXE_init _
  | BEXE_call_prim _
  | BEXE_call_with_trap _
  | BEXE_call _

  (* no expressions to monitor *)
  | BEXE_comment _
  | BEXE_trace _
  | BEXE_svc _
  | BEXE_nop _
  | BEXE_code _
  | BEXE_begin
  | BEXE_end
  | BEXE_assert _
  | BEXE_assert2 _
  | BEXE_axiom_check _
  | BEXE_axiom_check2 _

  | BEXE_try _
  | BEXE_endtry _
  | BEXE_catch _
    ->
if debug then print_endline ("flow: normal= processing for " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
    next()

(* Perform a once check on a single function *)
let once_check bsym_table counter ix2chain chain2ix bid name bexes = 
  if debug then
    print_endline ("Once_check: Detected "^string_of_int (List.length ix2chain)  ^
      " once variables in function " ^ name ^ "<" ^ string_of_int bid^ ">");

  (* map the exes of the function into augmented exes *)
  let make_augexes bexes = make_augexes bsym_table counter chain2ix ix2chain once_get_sets once_get_gets bexes in
  let bexes = make_augexes bexes in
  if debug then
    print_endline ("Calculated once use per instruction of " ^ name ^ ":" ^ string_of_int bid);

  (* Calculate initially live fairies: those associated with a parameter are
     assumed live initially. For each index of a function parameter
  *)
  let bparams = Flx_bsym_table.find_bparams bsym_table bid in 
  let bids = Flx_bparams.get_bids bparams in
  let entries = List.fold_left (fun acc bid -> acc @ find_entries bsym_table chain2ix bid) [] bids in
  let ixs = List.map snd entries in
  let once_params = BidSet.of_list ixs in
(*
  if not (BidSet.is_empty once_params) then 
    print_endline ("Once parameter starts initialised")
*)
  let stack = make_stack bid bexes in
  try
    flow 0 counter make_augexes bsym_table chain2ix ix2chain bid once_params stack
  with IncompletePriorFlow -> 
    print_endline ("Recursive routine requires non-recursive branch for once analysis");
    assert false

let once_bsym bsym_table counter bid parent bsym =
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with

  (* process Felix function *)
  | BBDCL_fun(props, bvs, ps, res, effects, exes) ->
    let kids = Flx_bsym_table.find_children bsym_table bid in
    let linear = List.mem `LinearFunction props in
if debug then
print_endline ("Once check examining " ^ (if linear then "linear" else "nonlinear") ^ " function "^ Flx_bsym.id bsym);

    (* calculate fairy variables. For each variable which is a child of the function,
       calculate a chain path from the variable to a uniquely typed component,
       being the index of the variable and the tuple component index sequence
       to reach the component. 

       For each such chain, synthesise a new index, the fairy variable,
       and return two lists, one mapping the chain to the fairy variable       
       and the inverse, mapping the fairy to the chain.
    *)
    let chain2ix, ix2chain = 
       List.fold_left (fun (acca, accb) vidx ->
         let bsym = Flx_bsym_table.find bsym_table vidx in
         let bbdcl = Flx_bsym.bbdcl bsym in
         match bbdcl with
         | BBDCL_val (_,typ,_) ->
           let a,b = build_once_maps bsym_table counter vidx typ in
           acca @ a, accb @ b
         | _ -> acca, accb
      )
      ([],[])
      (BidSet.elements kids)
    in
if debug then begin
print_endline ("  ** Fun " ^ bsym.id ^ "<" ^ string_of_int bid ^"> Calculated fairy variables, ix2chain index=");
print_ix2chain bsym_table ix2chain;
end;

    (* if the function has no fairy variables, there's nothing to do, otherwise
       perform a once check.
    *)
    if (List.length chain2ix <> 0) then
      begin try once_check bsym_table counter ix2chain chain2ix bid (Flx_bsym.id bsym) exes 
      with exn ->
        print_endline ("Uniqueness Error in function " ^ Flx_bsym.id bsym);
        raise exn
      end

  (* Ignore non-function *)
  | _ -> ()

let once_bsym_table phase bsym_table counter = 
  try
    Flx_bsym_table.iter (once_bsym bsym_table counter) bsym_table
  with exn ->
    print_endline ("ERROR in uniqueness verification phase " ^ phase);
    raise exn


