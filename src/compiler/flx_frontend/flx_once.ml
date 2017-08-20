open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr

type once_data_t = {gets: BidSet.t; sets: BidSet.t}
type augexe_t = Flx_bexe.t * once_data_t 

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



module BidSetSet = Flx_set.Make (
  struct 
    type t = BidSet.t
    let compare = compare
  end
)

(* return the instructions following a label
  if the label isn't found, well fine, there are
  no instructions after it
*)
let rec find_label augexes lab = 
  match augexes with
  | [] -> [] 
  | (BEXE_label (_,cand), data) :: tail when lab = cand -> tail
  | _ :: tail -> find_label tail lab

(* Fixup diagnostics later *)
let live old {sets=set; gets=get} =
(*
print_endline ("Calculate liveness: old= " ^ Flx_bid.str_of_bidset old ^ ", gets=" ^
  Flx_bid.str_of_bidset get ^ " set=" ^ str_of_bidset set);
*)

  (* Order matters, check gets on old live values first *)
  if not (BidSet.subset get old) then begin
    print_endline ("Once error: Using uninitialised or already used once variable");
    assert false;
  end;

  (* kill used variables *)
  let newlive =BidSet.diff old get in 

  (* check we're not setting an already live variable *)
  if not (BidSet.is_empty (BidSet.inter newlive set)) then begin
    print_endline ("Once error: Resetting live variable");
    assert false;
  end;

  (* Add newly set variables *)
  let live = BidSet.union newlive set  in
(*
print_endline ("Newlive= " ^ Flx_bid.str_of_bidset live);
*)
  live

let check_liveness liveset =
  if not (BidSet.is_empty liveset) then begin
    print_endline ("Once error: Once variables unused!");
    assert false
  end

let check_reentry previous current label =
  let d = BidSet.diff previous current in
  if not (BidSet.is_empty d) then begin
    print_endline ("Once error: Inconsistent liveness re-entering path at label " ^ string_of_int label);
    assert false
  end

(* this is basically a copy of the mainline flow routine,
however we have to handle a few things differently.

First note, child_flow is interested ONLY in the parent's
once variables, not those defined in the child itself!

When we hit a return we just return the liveness set,
we don't check it because it need not be empty.

In fact, there could be multiple possible liveness sets,
so we have to actually return a set of liveness sets,
the caller then processes each as a distinct control
flow path.

*)

let add_liveness liveset final_livesets = 
  BidSetSet.add liveset final_livesets 

let merge_liveness livesets1 livesets2 = 
  BidSetSet.union livesets1 livesets2 

let rec child_flow bsym_table visited liveset final_livesets augexes : BidSetSet.t =
(*
print_endline ("CHILD FLOW: " ^ (match augexes with | []-> "END" 
  | (bexe,{sets=sets; gets=gets}) ::_ -> Flx_print.string_of_bexe bsym_table 0 bexe ^ 
  "\nInput liveness= " ^ Flx_bid.str_of_bidset liveset^
  "\nSets= " ^ Flx_bid.str_of_bidset sets ^ ", gets= " ^ Flx_bid.str_of_bidset gets));
*)
  let child_flow visited liveset final_livesets augexes = child_flow bsym_table visited liveset final_livesets augexes in
  match augexes with
  (* exists without usage *)
  | [] 
  | (BEXE_proc_return _,_)::_ ->
    add_liveness liveset final_livesets

  (* exists with possible usage *)
  | (BEXE_jump_direct (_,_,_,_),deltalife)::_ 
  | (BEXE_jump (_,_,_),deltalife)::_ 
  | (BEXE_nonreturn_code (_,_,_),deltalife)::_ 
  | (BEXE_fun_return (_,_),deltalife)::_ ->
    let liveset = live liveset deltalife in
    add_liveness liveset final_livesets

  | (BEXE_goto (_,lidx),_)::_ ->
    if List.mem_assoc lidx visited then begin
      check_reentry (List.assoc lidx visited) liveset lidx;
      add_liveness liveset final_livesets
    end else 
      child_flow ((lidx,liveset)::visited) liveset final_livesets (find_label augexes lidx)

  | (BEXE_ifgoto (_,c,lidx),deltalife)::tail ->
    let liveset = live liveset deltalife in

    let skipped = child_flow visited liveset final_livesets tail in
    if List.mem_assoc lidx visited then begin
      check_reentry (List.assoc lidx visited) liveset lidx;
      add_liveness liveset final_livesets
    end else
      let taken = 
        child_flow ((lidx,liveset)::visited) liveset final_livesets (find_label augexes lidx)
      in 
      merge_liveness skipped taken
      
  | (BEXE_label (_,lidx),_)::tail ->
    child_flow ((lidx,liveset)::visited) liveset final_livesets tail

  | (_,deltalife)::tail -> 
    let liveset = live liveset deltalife in
    child_flow visited liveset final_livesets tail


(* flow analysis *)
let rec flow make_augexes bsym_table idx visited liveset augexes =
  let flow visited liveset augexes = flow make_augexes bsym_table idx visited liveset augexes in

  match augexes with
  (* exists without usage *)
  | [] 
  | (BEXE_proc_return _,_)::_ ->
    check_liveness liveset

  (* exists with possible usage *)
  | (BEXE_jump_direct (_,_,_,_),deltalife)::_ 
  | (BEXE_jump (_,_,_),deltalife)::_ 
  | (BEXE_nonreturn_code (_,_,_),deltalife)::_ 
  | (BEXE_fun_return (_,_),deltalife)::_ ->
(*
print_endline ("DETECTED JUMP or noret code or fun ret");
*)
    let liveset = live liveset deltalife in
    check_liveness liveset

  | (BEXE_goto (_,lidx),_)::_ ->
    if List.mem_assoc lidx visited then
      check_reentry (List.assoc lidx visited) liveset lidx
    else 
     flow ((lidx,liveset)::visited) liveset (find_label augexes lidx)

  | (BEXE_ifgoto (_,c,lidx),deltalife)::tail ->
    let liveset = live liveset deltalife in
    flow visited liveset tail;
    if List.mem_assoc lidx visited then
      check_reentry (List.assoc lidx visited) liveset lidx
    else
      flow ((lidx,liveset)::visited) liveset (find_label augexes lidx)
      
  | (BEXE_label (_,lidx),_)::tail ->
    flow ((lidx,liveset)::visited) liveset tail

  | (BEXE_call (_,(BEXPR_closure(pidx,_),_),_),deltalife)::tail
  | (BEXE_call_stack (_,pidx,_,_),deltalife)::tail 
  | (BEXE_call_direct (_,pidx,_,_),deltalife)::tail ->
(*
print_endline ("DETECTED PROCEDURE CALL");
*)
    let liveset = live liveset deltalife in
    let parent,bsym = Flx_bsym_table.find_with_parent bsym_table pidx in
    begin match parent with
    | Some idx2 when idx = idx2 ->
      let bbdcl = Flx_bsym.bbdcl bsym in
      begin match bbdcl with
      | BBDCL_fun (prop, bvs, ps, res, effects, bexes) ->
        let augexes = make_augexes bexes in 
        let final_livesets = 
          child_flow bsym_table visited liveset BidSetSet.empty augexes     
        in 
        let f liveset = flow visited liveset tail in
        BidSetSet.iter f final_livesets

      (* must  be external function *)
      | _ -> flow visited liveset tail
      end
    | _ -> flow visited liveset tail
    end

  (* FIXME: temporary hack for concept testing, handle all the
    other cases now!
  *)
  | (bexe,deltalife)::tail -> 
(*
print_endline ("Normall processing for " ^ Flx_print.string_of_bexe bsym_table 0 bexe);
*)
    let liveset = live liveset deltalife in
    flow visited liveset tail

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
  if not (BidSet.is_empty once_params) then 
(*
    print_endline ("Once parameter starts initialised");
*)
  flow make_augexes bsym_table bid [] once_params bexes
(*
  print_endline ("Flow of " ^ name ^ ":" ^ string_of_int bid ^ " checked")
*)

let once_bsym bsym_table bid parent bsym =
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_fun(prop, bvs, ps, res, effects, exes) ->
    let kids = Flx_bsym_table.find_children bsym_table bid in
    let once_kids = BidSet.filter (is_once bsym_table) kids in
    if not (BidSet.is_empty once_kids) then
      once_check bsym_table bid (Flx_bsym.id bsym) once_kids exes 

  | _ -> ()

let once_bsym_table bsym_table = 
  Flx_bsym_table.iter (once_bsym bsym_table) bsym_table


