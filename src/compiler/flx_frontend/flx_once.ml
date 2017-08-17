open Flx_bsym
open Flx_bsym_table
open Flx_bbdcl
open Flx_bid
open Flx_bexe
open Flx_bexpr

type once_data_t = {gets: BidSet.t; sets: BidSet.t}


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
  if not (BidSet.is_empty (BidSet.inter old set)) then begin
    print_endline ("Once error: Resetting live variable");
    assert false;
  end;

  let newlive = BidSet.union old set in
  if not (BidSet.subset get newlive) then begin
    print_endline ("Once error: Using uninitialised or already used once variable");
    assert false;
  end;

  BidSet.diff newlive get

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

(* flow analysis *)
let rec flow visited liveset augexes =
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

  (* FIXME: temporary hack for concept testing, handle all the
    other cases now!
  *)
  | (_,deltalife)::tail -> 
    let liveset = live liveset deltalife in
    flow visited liveset tail

let is_once bsym_table bid = 
  match Flx_bsym_table.find_bbdcl bsym_table bid with
  | BBDCL_val (_,_,`Once) -> true
  | _ -> false


let get_sets bsym_table once_kids bexe =
  let bidset = ref BidSet.empty in
  let add i = 
    print_endline ("Adding once set " ^ string_of_int i); 
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
    print_endline ("Adding once get " ^ string_of_int i); 
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
  print_endline ("Detected once variable of function " ^ name);
  let is_once bid = is_once bsym_table bid in
  let bexes = 
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
   in
   print_endline ("Calculated once use per instruction of " ^ name ^ ":" ^ string_of_int bid);
   let bparams = Flx_bsym_table.find_bparams bsym_table bid in 
   let ps =  List.filter is_once (Flx_bparameter.get_bids (fst bparams)) in
   let once_params = BidSet.of_list ps in
   if not (BidSet.is_empty once_params) then 
     print_endline ("Once parameter starts initialised");

   flow [] once_params bexes;
   print_endline ("Flow of " ^ name ^ ":" ^ string_of_int bid ^ " checked")


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


