open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr

exception CompleteFlow
exception IncompletePriorFlow

let str_of_vars bsym_table bidset =
  BidSet.fold (fun idx acc -> 
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table idx in
    acc ^
    "Variable " ^ Flx_bsym.id bsym ^ 
      " defined at\n" ^ Flx_srcref.long_string_of_src (Flx_bsym.sr bsym)
    ^ "\n"
  ) 
  bidset
  ""

(* simulate labels at the start and end of a routine *)
let entry_label = -1
let exit_label = -2

let str_of_label bsym_table lidx = 
  match lidx with
  | _ when lidx = entry_label -> "Routine Entry"
  | _ when lidx = exit_label -> "Routine Exit"
  | _ -> 
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table lidx in
    Flx_bsym.id bsym ^
    "\nDefined at\n" ^
    Flx_srcref.long_string_of_src (Flx_bsym.sr bsym)


(* symmetric difference A \cup B - A \cap B *)
let symdiff a b = BidSet.diff (BidSet.union a b) (BidSet.inter a b)

type once_data_t = {gets: BidSet.t; sets: BidSet.t}
type augexe_t = Flx_bexe.t * once_data_t 

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


let make_augexes bsym_table once_kids get_sets get_gets bexes : augexe_t list=
  List.map 
  (
    fun bexe -> 
      bexe, 
      {
        sets=get_sets bsym_table once_kids bexe; 
        gets=get_gets bsym_table once_kids bexe
      }
  ) 
  bexes 

let make_stack index exes =
  [{current=exes; frame= {index=index; code=exes; visited=[]}}]


let rec find_label augexes lab = 
  match augexes with
  | [] -> None
  | (BEXE_label (_,cand), data) :: tail when lab = cand -> Some tail
  | _ :: tail -> find_label tail lab

(* Fixup diagnostics later *)
let live bsym_table old {sets=set; gets=get} =
(*
print_endline ("Calculate liveness: old= " ^ Flx_bid.str_of_bidset old ^ ", gets=" ^
  Flx_bid.str_of_bidset get ^ " set=" ^ str_of_bidset set);
*)

  (* Order matters, check gets on old live values first *)
  if not (BidSet.subset get old) then begin
    print_endline ("Once error: Using uninitialised or already used once variable");
    let getdead = BidSet.diff get old in
    print_endline (str_of_vars bsym_table getdead);
    assert false;
  end;

  (* kill used variables *)
  let newlive =BidSet.diff old get in 

  (* check we're not setting an already live variable *)
  if not (BidSet.is_empty (BidSet.inter newlive set)) then begin
    print_endline ("Once error: Resetting live variable");
    let reset = BidSet.diff set newlive in
    print_endline (str_of_vars bsym_table reset);
    assert false;
  end;

  (* Add newly set variables *)
  let live = BidSet.union newlive set  in
(*
print_endline ("Newlive= " ^ Flx_bid.str_of_bidset live);
*)
  live

let check_liveness bsym_table liveset : stack_t =
  if not (BidSet.is_empty liveset) then begin
    print_endline ("Once error: Once variables unused!");
    print_endline (str_of_vars bsym_table liveset);
    assert false
  end;
  [] (* end of path *)

let check_reentry bsym_table previous current label : stack_t=
  if previous <> current then begin
    print_endline ("Once error: Inconsistent liveness re-entering path at label " ^ 
      string_of_int label ^ " "^ str_of_label bsym_table label 
    );
    let diffset = BidSet.diff (BidSet.union previous current) (BidSet.inter previous current) in
    print_endline (str_of_vars bsym_table diffset);
    assert false
  end;
  [] (* end of path *)

(* handle gotos, including nonlocal gotos *)
let rec goto bsym_table (stack:stack_t) (liveness:BidSet.t) (label:bid_t) : stack_t =
  match stack with
  | [] -> check_liveness bsym_table liveness  (* exit *)
  | head :: tail -> 
    match find_label head.frame.code label with
    | Some bexes -> 
      let visited = head.frame.visited in
      if List.mem_assoc label visited then
        let old_liveness = List.assoc label visited in
        check_reentry bsym_table old_liveness liveness label (* path merge *)
      else
        let _ = head.frame.visited <- (label,liveness)::visited in (* continue recording liveness at label *)
        { head with current=bexes } :: tail
    | None -> goto bsym_table tail liveness label (* look for label in parent *)

(* handle return instruction *)
let return bsym_table (stack:stack_t) (liveness:BidSet.t) : stack_t =
  match stack with 
  | [] -> assert false
  | callee :: [] -> check_liveness bsym_table liveness
  | callee :: caller :: chain ->
    let visited = callee.frame.visited in
    if List.mem_assoc exit_label visited then
      let old_liveness = List.assoc exit_label visited in
      check_reentry bsym_table old_liveness liveness exit_label (* path merge *)
    else
      let _ = callee.frame.visited <- (exit_label,liveness)::visited in
      caller :: chain
 
  
(*********************************************************************) 
(* flow analysis *)
let rec flow make_augexes bsym_table master liveness stack : unit =
  let flow liveness stack = flow make_augexes bsym_table master liveness stack in
(*
print_endline ("Flow, stack depth = " ^ string_of_int (List.length stack));
*)
  match stack with 
  | [] -> () (* finished *)
  | cc :: caller ->
  match cc.current with
  (* exists without usage *)
  | [] 
  | (BEXE_proc_return _,_)::_ ->
    flow liveness (return bsym_table stack liveness)

  (* exists with possible usage *)
  (* FIXME: could be a call to a child! *)
  | (BEXE_jump_direct (_,_,_,_),deltalife)::_ 
  | (BEXE_jump (_,_,_),deltalife)::_ 
  | (BEXE_nonreturn_code (_,_,_),deltalife)::_ 
  | (BEXE_fun_return (_,_),deltalife)::_ ->
(*
print_endline ("DETECTED JUMP or noret code or fun ret");
*)
    let liveness = live bsym_table liveness deltalife in
    flow liveness (check_liveness bsym_table liveness)

  | (BEXE_goto (_,lidx),_)::_ ->
    flow liveness (goto bsym_table stack liveness lidx)

  | (BEXE_ifgoto (_,c,lidx),deltalife)::tail ->
    let liveness = live bsym_table liveness deltalife in
    begin try
      (* branch not taken case *)
      flow liveness ({cc with current=tail} :: caller);
      raise CompleteFlow
    with 
    | IncompletePriorFlow ->
      begin
        (* branch taken case *)
        flow liveness (goto bsym_table stack liveness lidx);
        (* branch not taken case *)
        flow liveness ({cc with current=tail} :: caller)
      end
    | CompleteFlow ->
      (* branch taken case *)
      flow liveness (goto bsym_table stack liveness lidx)
    end

      
  | (BEXE_label (_,lidx),_)::tail ->
    let visited = cc.frame.visited in
    if List.mem_assoc lidx visited then
      let old_liveness = List.assoc lidx visited in
      flow liveness (check_reentry bsym_table old_liveness liveness lidx) (* path merge *)
    else
      let _ = cc.frame.visited <- (lidx,liveness)::visited in (* continue recording liveness at label *)
      flow liveness ({cc with current=tail} :: caller)

  | (BEXE_call (_,(BEXPR_closure(pidx,_),_),_),deltalife)::tail
  | (BEXE_call_stack (_,pidx,_,_),deltalife)::tail 
  | (BEXE_call_direct (_,pidx,_,_),deltalife)::tail ->
    let liveness = live bsym_table liveness deltalife in
(*
    let parent,bsym = Flx_bsym_table.find_with_parent bsym_table pidx in
    print_endline ("DETECTED PROCEDURE CALL " ^ Flx_bsym.id bsym);
*)
    if Flx_bsym_table.is_ancestor bsym_table pidx master then begin
      let bsym = Flx_bsym_table.find bsym_table pidx in
(*
      print_endline ("DETECTED PROCEDURE CALL to descendant of master: " ^ Flx_bsym.id bsym);
*)
      begin match rec_entry stack pidx with
      | Some old_liveness -> 
(*
print_endline "Recursion detected";
*)
        (* dummy path termination *)
        flow liveness (check_reentry bsym_table old_liveness liveness entry_label);

        (* continue past recursive call now *)
        begin match rec_exit stack pidx with
        | Some liveness ->
          flow liveness ({cc with current=tail} :: caller)
        | None ->
(*
          print_endline ("Recursive path didn't terminate: put non-recursive branch first!");
*)
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
          flow liveness stack 

        (* must  be external function *)
        | _ -> flow liveness ({cc with current=tail} :: caller)
        end
      end
    end else begin
(*
      print_endline "Not descendant";
*)
      flow liveness ({cc with current=tail} :: caller)
    end

  (* FIXME: temporary hack for concept testing, handle all the
    other cases now!
  *)
  | (bexe,deltalife)::tail -> 
(*
print_endline ("Normall processing for " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
*)
    let liveness = live bsym_table liveness deltalife in
    flow liveness ({cc with current=tail} :: caller)


let is_once bsym_table bid = 
  match Flx_bsym_table.find_bbdcl bsym_table bid with
  | BBDCL_val (_,_,`Once) -> true
  | _ -> false


let get_sets bsym_table once_kids bexe =
  let bidset = ref BidSet.empty in
  let add i = 
(*
    print_endline ("Adding once set " ^ string_of_int i); 
*)
    if BidSet.mem i !bidset then begin
      print_endline ("Once variable set twice in instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
      print_endline (str_of_vars bsym_table (BidSet.singleton i));
      assert false
    end;
    bidset := BidSet.add i !bidset 
  in
  begin match bexe with 
  | BEXE_assign (_,(BEXPR_varname (i,_),_),_) 
  | BEXE_init (_,i,_) when BidSet.mem i once_kids -> add i
  | _ -> ()
  end;
  !bidset

let get_gets bsym_table once_kids bexe = 
  let bidset = ref BidSet.empty in
  let add i = 
(*
    print_endline ("Adding once get " ^ string_of_int i); 
*)
    if BidSet.mem i !bidset then begin
      print_endline ("Once variable used twice in instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
      print_endline (str_of_vars bsym_table (BidSet.singleton i));
      assert false
    end;
    bidset := BidSet.add i !bidset 
  in
  let add_once i = if BidSet.mem i once_kids then add i in
  let f_bexpr e = Flx_bexpr.iter ~f_bid:add_once e in
  begin match bexe with 
  | BEXE_assign (_,(BEXPR_varname _,_),e) 
  | BEXE_init (_,_,e) -> f_bexpr e
  | _ -> Flx_bexe.iter ~f_bexpr bexe
  end;
  !bidset

let once_check bsym_table bid name once_kids bexes = 
(*
  print_endline ("Detected once variable of function " ^ name);
*)
  let is_once bid = is_once bsym_table bid in
  let make_augexes bexes = make_augexes bsym_table once_kids get_sets get_gets bexes in
  let bexes = make_augexes bexes in
(*
  print_endline ("Calculated once use per instruction of " ^ name ^ ":" ^ string_of_int bid);
*)
  let bparams = Flx_bsym_table.find_bparams bsym_table bid in 
  let ps =  List.filter is_once (Flx_bparameter.get_bids (fst bparams)) in
  let once_params = BidSet.of_list ps in
(*
  if not (BidSet.is_empty once_params) then 
    print_endline ("Once parameter starts initialised")
*)
  let stack = make_stack bid bexes in
  try
    flow  make_augexes bsym_table bid once_params stack
  with IncompletePriorFlow -> 
    print_endline ("Recursive routine requires non-recursive branch for once analysis");
    assert false

let once_bsym bsym_table bid parent bsym =
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_fun(prop, bvs, ps, res, effects, exes) ->
    let kids = Flx_bsym_table.find_children bsym_table bid in
    let once_kids = BidSet.filter (is_once bsym_table) kids in
(*
print_endline ("Once check examining " ^ Flx_bsym.id bsym);
*)
    if not (BidSet.is_empty once_kids) then
      once_check bsym_table bid (Flx_bsym.id bsym) once_kids exes 

  | _ -> ()

let once_bsym_table bsym_table = 
  Flx_bsym_table.iter (once_bsym bsym_table) bsym_table


