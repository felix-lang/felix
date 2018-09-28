open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr
open Flx_btype

exception CompleteFlow
exception IncompletePriorFlow
 
let debug = 
  try let _ = Sys.getenv "FLX_COMPILER_DEBUG_UNIQ" in true  
  with Not_found -> false 

let show_getset_only = 
  try let _ = Sys.getenv "FLX_COMPILER_DEBUG_UNIQ_GETSET" in true  
  with Not_found -> false 


let show_getset = debug || show_getset_only
 

type item_t = [`Uniq | `Tup of int]
type path_t = item_t list 
type paths_t = path_t list 

type chain2ix_t = ((bid_t * path_t) * bid_t) list
type ix2chain_t = (bid_t * (bid_t * path_t)) list

let prj_of_item item = match item with
  | `Uniq -> ":U"
  | `Tup i -> "." ^ string_of_int i

let chain_of_path (p:path_t) =
  List.fold_left (fun acc item -> acc ^ prj_of_item item) "" p

let find_chain ix2chain i = List.assoc i ix2chain 

let find_var ix2chain i =
  let v,prjs = find_chain ix2chain i in
  v

let str_of_vars bsym_table ix2chain bidset =
  "("^ String.concat "," (
  BidSet.fold (fun yidx acc -> 
    let idx,prjs = find_chain ix2chain yidx in
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table idx in
    let chain = match prjs with | [] -> "" | _ -> chain_of_path prjs in
    (string_of_int yidx ^ ":->" ^Flx_bsym.id bsym ^ chain) :: acc
  ) 
  bidset
  []) ^ ")"

let def_of_vars bsym_table ix2chain bidset =
  let chains = str_of_vars bsym_table ix2chain bidset in
  let vars = 
    BidSet.fold (fun yidx acc ->
      let idx = find_var ix2chain yidx in
      BidSet.add idx acc
    ) BidSet.empty bidset
  in
  let varlocs = 
    BidSet.fold (fun yidx acc -> 
      let idx = find_var ix2chain yidx in
      let parent, bsym = Flx_bsym_table.find_with_parent bsym_table idx in
      acc ^
      "Variable " ^ Flx_bsym.id bsym ^ 
        " defined at\n" ^ Flx_srcref.long_string_of_src (Flx_bsym.sr bsym)
      ^ "\n"
    ) 
    vars 
    ""
  in chains ^ "\n" ^ varlocs

let id_of_index bsym_table index = 
  Flx_bsym.id (Flx_bsym_table.find bsym_table index)

let str_of_ix2chain_entry bsym_table (fairy, (variable, prjs)) =
  string_of_int fairy ^ " -> " ^ id_of_index bsym_table variable ^
  "<" ^ string_of_int variable ^ ">" ^ chain_of_path prjs

let print_ix2chain bsym_table ix2chain =
  List.iter 
    (fun entry -> print_endline ("  " ^ str_of_ix2chain_entry bsym_table entry)) 
  ix2chain

(* this function analyses a type and returns a list of 
symbolic projection chains which reach a uniq component:
the lists are reversed from the order in which they
must be provided. It delves into uniqs which are products
too even though such projections are not allowed at the moment.
We might actually consider the coerion from uniq T -> T to be
a special kind of projection and allow it later.
*)

let rec uniq_anal_aux (path:path_t) typ (paths:paths_t): paths_t = 
  match typ with
  | BTYP_uniq t -> (* no index to continue *)
    let path = `Uniq :: path in
    uniq_anal_aux path  t (path::paths)

  | BTYP_tuple ts ->
    List.fold_left2 (fun acc t n -> 
      let path = `Tup n :: path in
      uniq_anal_aux path t acc
    )
    paths 
    ts (Flx_list.nlist (List.length ts))

  | _ -> paths

(* this routine reverses the paths so they're in the correct order.
It also adds every exteniosn of the reversed paths to the set, uniquely.
The U is lost.  For example if we have paths

  .1

then we might get back

  .1.2.4
  .1.2
  .1

Any projection chain from any variable of the given type,
including the empty chain, which matches any of these
chains, is either targeting a uniq component, or a component
containing one, directly or indirectly.
*)
let uniq_anal typ : paths_t =
  let rps = uniq_anal_aux [] typ [] in
  List.map (fun lst -> List.rev (List.tl lst)) rps

let find_entries bsym_table (chain2ix:chain2ix_t) bid : chain2ix_t = 
  let paths = List.filter (fun ((bid2,path),ix) -> bid2 = bid) chain2ix in
  paths

  (* the chain2ix is required when analysing expressions to determine
which variables or projections of variables are unique, we match
the left term of the current projection chain against the list
of all chains, if there's a match the associated synthesised index
is what we track.

The ix2chain is a way to get back to the original code in case
we have an error we have to report to the user.

Now, when we analyse an term there are two cases of interest:

(a) its a plain variable
(b) its the application of a projection to an argument

The third case, neither of the above, just maps our analyser over
that term with a fresh (empty) path.

In case b, we find the last projection in a chain first,
and the final variable last, because application is forward
polish notation, but chains are reverse polish. So in order
to successively pick match our projection path, the index has
to store the path in reverse order, with the variable last,
and the innermost projection first.

*)

let build_once_maps bsym_table counter vidx typ : chain2ix_t * ix2chain_t =
  let paths = uniq_anal typ in
  let uxs = List.map (fun _ -> Flx_bid.fresh_bid counter) paths in
  let chain2ix = List.map2 (fun path idx -> (vidx,path),idx) paths uxs in
  let ix2chain = List.map2 (fun path idx -> idx,(vidx,path)) paths uxs in
  chain2ix,ix2chain


(* This routine finds all the indexes of uniq expressions .. well
at the moment this is just uniq variables and constant projections
of variables to uniq components
*)

let rec find_once bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) e : unit =
(*
print_endline ("Find once for expresssion " ^ Flx_print.sbe bsym_table e);
*)
  match e with
  | BEXPR_varname (i,[]),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          b := BidSet.add ix !b;
      )
    chain2ix

  | BEXPR_apply ( (BEXPR_prj (n,_,_),_), base ),_ ->
    let path = `Tup n :: path in
    find_once bsym_table chain2ix path b base 

  | x -> Flx_bexpr.flat_iter ~f_bexpr:(find_once bsym_table chain2ix path b) x

let rec find_ponce bsym_table (chain2ix:chain2ix_t) path (b:BidSet.t ref) e : unit =
(*
print_endline ("Find pointers to once for expresssion " ^ Flx_print.sbe bsym_table e);
*)
  match e with
  | BEXPR_wref (i,[]),_  
  | BEXPR_ref (i,[]),_ -> 
    let prefix = List.rev path in
    List.iter  (fun ((j,path),ix) ->
      if j = i then
        if Flx_list.has_prefix prefix path then 
          b := BidSet.add ix !b;
      )
    chain2ix

  | BEXPR_apply ( (BEXPR_prj (n,_,_),_), base ),_ ->
    let path = `Tup n :: path in
    find_ponce bsym_table chain2ix path b base 

  | x -> Flx_bexpr.flat_iter ~f_bexpr:(find_ponce bsym_table chain2ix path b) x



(* Get and Set detectors for instructions *)

let get_sets bsym_table chain2ix ix2chain bexe =
  let bidset = ref BidSet.empty in
  let f_bexpr e = find_once bsym_table chain2ix [] bidset e in
  begin match bexe with 
  | BEXE_assign (_,l,_) -> f_bexpr l
  | BEXE_init (_,i,(_,vt as e)) -> f_bexpr (bexpr_varname vt (i,[]))
  | BEXE_storeat (_,l,r) -> 
    find_ponce bsym_table chain2ix [] bidset l
  | _ ->  () 
  end;

  if show_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  SETS: " ^ str_of_vars bsym_table ix2chain (!bidset));
  !bidset

let get_gets bsym_table chain2ix ix2chain bexe = 
  let bidset = ref BidSet.empty in
  let f_bexpr e = 
     match e with
     | BEXPR_ref (i,_),_ -> ()
     | _ -> find_once bsym_table chain2ix [] bidset e 
  in
  begin match bexe with 
  (* storing at a pointer is still a get on the pointer! *)
  | BEXE_storeat (_,l,e)  -> f_bexpr l; f_bexpr e

  (* if the target of an assignment is a variable, is not a get *)
  | BEXE_assign (_,(BEXPR_varname _,_),e) 
  | BEXE_assign (_,(BEXPR_deref (BEXPR_varname _,_),_),e) 

  (* nor is the target of an initialisation *)
  | BEXE_init (_,_,e) -> f_bexpr e
  | _ -> Flx_bexe.iter ~f_bexpr bexe
  end;

  if show_getset && not (BidSet.is_empty (!bidset)) then
    print_endline ("  GETS: " ^ str_of_vars bsym_table ix2chain (!bidset));
  !bidset


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


let make_augexes bsym_table chain2ix ix2chain get_sets get_gets bexes : augexe_t list=
  List.map 
  (
    fun bexe -> 
      if show_getset then
        print_endline ("instruction " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
      bexe, 
      {
        sets=get_sets bsym_table chain2ix ix2chain bexe; 
        gets=get_gets bsym_table chain2ix ix2chain bexe
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
let rec flow seq make_augexes bsym_table chain2ix ix2chain master liveness stack : unit =
  let flow liveness stack = flow (seq + 1)  make_augexes bsym_table chain2ix ix2chain master liveness stack in
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
if debug then print_endline (string_of_int seq ^"@"^string_of_int lno^ " " ^ str_of_vars bsym_table ix2chain liveness);
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
  *)
  | BEXE_yield _ 
    -> 
    print_endline ("Once analysis can't handle yield yet"); 
    assert false

  | BEXE_halt _
    -> 
    final ()

  | BEXE_cgoto _
  | BEXE_ifcgoto _
    -> 
    print_endline ("Once analysis can't handle computed gotos"); 
    assert false

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

let once_check bsym_table ix2chain chain2ix  bid name  bexes = 
  if debug then
    print_endline ("Once_check: Detected once variable of function " ^ name);
  let make_augexes bexes = make_augexes bsym_table chain2ix ix2chain get_sets get_gets bexes in
  let bexes = make_augexes bexes in
  if debug then
    print_endline ("Calculated once use per instruction of " ^ name ^ ":" ^ string_of_int bid);
  (* Calculate initially live indexes: those associated with a parameter are
     assumed live initially
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
    flow 0 make_augexes bsym_table chain2ix ix2chain bid once_params stack
  with IncompletePriorFlow -> 
    print_endline ("Recursive routine requires non-recursive branch for once analysis");
    assert false

let once_bsym bsym_table counter bid parent bsym =
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_fun(prop, bvs, ps, res, effects, exes) ->
    let kids = Flx_bsym_table.find_children bsym_table bid in
if debug then
print_endline ("Once check examining " ^ Flx_bsym.id bsym);
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
print_endline ("Calculated fairy variables, ix2chain index=");
print_ix2chain bsym_table ix2chain;
end;
    if (List.length chain2ix <> 0) then
      once_check bsym_table ix2chain chain2ix bid (Flx_bsym.id bsym) exes 

  | _ -> ()

let once_bsym_table bsym_table counter = 
  Flx_bsym_table.iter (once_bsym bsym_table counter) bsym_table


