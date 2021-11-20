(* this module checks that share variables are initialised before 
they are used. It is based on a simple control flow analysis using
the same style and flx_once (which checks instead that uniquely typed
variables are live on use and dead on assignment).

The algorithm is similar to the uniqueness checker. However the rules of combination
are different. 

We require a variable be live when read as does the uniqueness checker.
However it remains live after reading (unlike the uniqueness checker
which makes it dead).

Assignment is allowed to either live or dead variables, they are
live after the assignment.

The other core difference is the way we handle a jump to an already
passed label. There are two alternatives:

1. if the variable is live on either entry, we stop and it's all OK.
This method won't report false failures, but will miss some failures too.

2. If the variable is dead on either entry, we stop and it's dead.
This will not miss cases, but will report errors when, dynamically
there aren't any.

Note, with address taking, we ignore it!

*)

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

exception UseBeforeInitError

(* Fixup diagnostics later *)
let live bsym_table ix2chain sr bexe live {sets=set; gets=get} =
(*
print_endline ("Calculate liveness: old= " ^ Flx_bid.str_of_bidset old ^ ", gets=" ^
  Flx_bid.str_of_bidset get ^ " set=" ^ str_of_bidset set);
*)

  (* Order matters, check gets on old live values first *)
  let getdead = BidSet.diff get live in
  if not (BidSet.is_empty getdead) then begin
    print_endline ("Share init error: Using uninitialised shared variable");
    print_endline (def_of_vars bsym_table ix2chain getdead);
    print_endline ("In instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
    print_endline ("Detected at" ^ Flx_srcref.long_string_of_src sr);
    raise UseBeforeInitError 
  end;
(* THIS DOES'T APPLY TO SHARED VARIABLES
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
*)
  (* Add newly set variables *)
  let live = BidSet.union live set  in
(*
print_endline ("Newlive= " ^ Flx_bid.str_of_bidset live);
*)
  live

(* call at end of routine, to see if there are unused variables,
warn if there are
*)
(* We need to pass "all" variable set here but don't have it at the moment *)
(*
let check_unused bsym_table ix2chain all liveset : unit =
  let unused = BidSet.minus all liveset in
  if not (BidSet.is_empty unused) then begin
    print_endline ("Warning: unused variables");
    print_endline (def_of_vars bsym_table ix2chain unused);
  end
*)

(* handle gotos, including nonlocal gotos *)
let rec goto bsym_table ix2chain (stack:stack_t) (liveness:BidSet.t) (label:bid_t) : stack_t =
(* print_endline ( "goto " ^ string_of_int label);  *)
  match stack with
  | [] -> [] (* exit *)
  | head :: tail -> 
    match find_label head.frame.code label with
    | Some bexes -> 
      let visited = head.frame.visited in
      if List.mem_assoc label visited then
(*        let _ = print_endline ("Already visited") in *) 
        let old_liveness = List.assoc label visited in
        let new_liveness = BidSet.inter old_liveness liveness in
(*
        let _ = print_endline ("  OLD live=" ^ Flx_bid.str_of_bidset old_liveness) in
        let _ = print_endline ("  NEW live=" ^ Flx_bid.str_of_bidset new_liveness) in
        let _ = print_endline ("  " ^ if new_liveness = old_liveness then " EQUAL" else "NOT EQUAL") in
        let _ = print_endline ("  " ^ if BidSet.equal old_liveness new_liveness then " EQUAL" else "NOT EQUAL") in
*)
        if BidSet.equal new_liveness old_liveness then 
(*          let _ = print_endline ("Reached fixpoint, terminating") in  *)
          tail (* reached fixpoint, finish *)
        else (* continue with current continuation at code after label *)
(*            let _ = print_endline ("Updating visit data") in *)
            (* remove old state at label *)
            let stripped = List.filter (fun (lab,_) -> label <> lab) head.frame.visited in
            (* re-insert label with new state *)
(*
            let _ = print_endline ("  OLD live=" ^ Flx_bid.str_of_bidset old_liveness) in
            let _ = print_endline ("  NEW live=" ^ Flx_bid.str_of_bidset new_liveness) in
*)
            let _ = head.frame.visited <- (label,new_liveness)::stripped in (* continue recording liveness at label *)
            { head with current=bexes } :: tail
      else 
(*        let _ = print_endline ("first visit") in *) 
        let _ = head.frame.visited <- (label,liveness) :: head.frame.visited in (* continue recording liveness at label *)
        { head with current=bexes } :: tail

    | None -> goto bsym_table ix2chain tail liveness label (* look for label in parent *)

(* handle return instruction *)
let return bsym_table ix2chain (stack:stack_t) (liveness:BidSet.t) : stack_t =
  match stack with 
  | [] -> assert false
  | callee :: [] -> [] (* check_unused bsym_table ix2chain liveness *)
  | callee :: caller :: chain ->
    let visited = callee.frame.visited in
    let liveness = 
      if List.mem_assoc exit_label visited then BidSet.inter (List.assoc exit_label visited) liveness
      else liveness
    in
    (* check_unused bsym_table ix2chain liveness exit_label *) 
    (* exit subroutine *)
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
  let final () = flow liveness [] in
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
if debug then print_endline ("flow: ignoring recursive procedure re-entry " ^ string_of_int pidx);
        (* re-entry can, at worse, liven something dead. Since we already processed this entry
           point there's no point doing so again
        *)
        (* flow liveness (check_reentry bsym_table ix2chain old_liveness liveness entry_label); *)

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
          (* ignore the call because it cannot change any variables
         [other than via a pointer which we cannot handle at the moment]
         *)
if debug then print_endline ("flow: external procedure " ^ string_of_int pidx);
          flow liveness (return bsym_table ix2chain stack liveness)
        end
      end
    end else begin
      (* ignore non-descendant procedure calls, because they cannot change any variables
         [other than via a pointer which we cannot handle at the moment]
      *)
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
if debug then print_endline ("flow: ignoring recursive procedure re-entry " ^ string_of_int pidx);
        (* dummy path termination *)
(*
        flow liveness (check_reentry bsym_table ix2chain old_liveness liveness entry_label);
*)
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
(* dropping into a label is the same as doing a goto to the label *)
  flow liveness (goto bsym_table ix2chain stack liveness lidx)

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
  | BEXE_assign (sr, (BEXPR_varname (v,_),_),(BEXPR_apply ((BEXPR_closure (pidx,_),_),arg),_)) 
  | BEXE_assign (sr, (BEXPR_varname (v,_),_),(BEXPR_apply_direct (pidx,_,arg),_)) 
  | BEXE_assign (sr, (BEXPR_varname (v,_),_),(BEXPR_apply_stack (pidx,_,arg),_)) 
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
let shareinit_check bsym_table counter ix2chain chain2ix bid name bexes = 
  if debug then
    print_endline ("shareinit_check: Detected "^string_of_int (List.length ix2chain)  ^
      " shared variables in function " ^ name ^ "<" ^ string_of_int bid^ ">");

  (* map the exes of the function into augmented exes *)
  let make_augexes bexes = make_augexes bsym_table counter chain2ix ix2chain share_get_sets share_get_gets bexes in
  let bexes = make_augexes bexes in
  if debug then
    print_endline ("Calculated shared use per instruction of " ^ name ^ ":" ^ string_of_int bid);

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
    print_endline ("Recursive routine requires non-recursive branch for share init analysis");
    assert false

let shareinit_bsym bsym_table counter bid parent bsym =
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with

  (* process Felix function *)
  | BBDCL_fun(props, bvs, ps, res, effects, exes) ->
    let kids = Flx_bsym_table.find_children bsym_table bid in
    let linear = List.mem `LinearFunction props in
if debug then
print_endline ("Shareinit check examining " ^ (if linear then "linear" else "nonlinear") ^ " function "^ Flx_bsym.id bsym);

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
           let a,b = build_shared_maps bsym_table counter vidx typ in
           acca @ a, accb @ b
         | _ -> acca, accb
      )
      ([],[])
      (BidSet.elements kids)
    in
if debug then begin
if not (List.length ix2chain = 0) then begin
print_endline ("  ** Shared variable init: Fun " ^ bsym.id ^ "<" ^ string_of_int bid ^"> Calculated fairy variables, ix2chain index=");
print_ix2chain bsym_table ix2chain;
end
end;

    (* if the function has no fairy variables, there's nothing to do, otherwise
       perform a once check.
    *)
    if (List.length chain2ix <> 0) then
      begin try shareinit_check bsym_table counter ix2chain chain2ix bid (Flx_bsym.id bsym) exes 
      with UseBeforeInitError ->
        print_endline ("In function with code ");
        begin if List.length exes <= 20 then
          List.iter (fun bexe -> print_endline (Flx_print.string_of_bexe bsym_table 2 bexe)) exes
        else 
          print_endline (string_of_int (List.length exes) ^ " instructions, too many to list (limit 20)")
        end;
        () (* kills remainder of analysis *)
      | exn ->
        print_endline ("Shared Variable used before set Error in function " ^ Flx_bsym.id bsym);
        raise exn
      end

  (* Ignore non-function *)
  | _ -> ()

let shareinit_bsym_table phase bsym_table counter = 
  try
(*
print_endline "Doing share init";
*)
    Flx_bsym_table.iter (shareinit_bsym bsym_table counter) bsym_table
  with 
  | UseBeforeInitError ->
    failwith ("ERROR in init share variable before use verification phase " ^ phase);
    
  | exn ->
    print_endline ("ERROR in init share variable before use verification phase " ^ phase);
    raise exn

