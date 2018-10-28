open List

open Flx_ast
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_print
open Flx_set
open Flx_types
open Flx_unify
open Flx_util
open Flx_btype_subst
open Flx_bid

let debug = match Sys.getenv_opt "Flx_reduce" with Some _ -> true | _ -> false ;;

let id x = x
(*
let remove_useless_reductions counter bsym_table reductions =
  List.filter
  (fun (id,bvs,bps,e1,_) ->
    let psi = Flx_bparameter.get_bids bps in
    let ui i =
      let used = List.mem i psi || Flx_bsym_table.mem bsym_table i in
      if not used then begin
        if counter.compiler_options.print_flag then
        print_endline ("ELIDING USELESS REDUCTION " ^ id ^ " because "
          ^ string_of_bid i ^ " isn't found");
        raise Not_found
      end
    in
    begin
      try
        Flx_bexpr.iter ~f_bid:ui e1;
        if counter.compiler_options.print_flag then
        print_endline ("Keep " ^ id (* ^ " matching " ^ sbe bsym_table e1 *));

        true
      with
      | Not_found ->
        if counter.compiler_options.print_flag then
        print_endline ("Discard " ^ id (* ^ " matching " ^ sbe bsym_table e1 *));
        false
    end
  )
  reductions
*)

(* match one reduction case against a fixed expression *)
let ematch nreds counter bsym_table name tvars evars e1 e2 e : bool * Flx_bexpr.t =
(*
  print_endline ("Matching " ^ sbe bsym_table e ^ " with " ^ sbe bsym_table e1); 
*)
  match Flx_unify_expr.expr_maybe_matches bsym_table counter tvars evars e1 e with
  | Some (tmgu,emgu) ->
    incr nreds;
if debug then begin
      print_endline (name^" REDUCTION: FOUND A MATCH, candidate " ^ sbe bsym_table e);
      print_endline ("with LHS " ^ sbe bsym_table e1);
      print_endline ("EMGU=" ^catmap ", " (fun (i,e')-> si i ^ " --> " ^ sbe bsym_table e') emgu);
      print_endline ("TMGU=" ^catmap ", " (fun (i,t')-> si i ^ " --> " ^ sbt bsym_table t') tmgu);
      print_endline ("with RHS=" ^ sbe bsym_table e2);
end;
    let e = fold_left (fun e (i,e') -> Flx_unify_expr.expr_term_subst e i e') e2 emgu in
if debug then
    print_endline ("RHS after expr subs=" ^ sbe bsym_table e);
    let rec s e = Flx_bexpr.map ~f_btype:(list_subst counter tmgu) ~f_bexpr:s e in
    let e' = s e in
if debug then
    print_endline ("RESULT OF SUBSTITUTION into RHS: " ^ sbe bsym_table e2 ^ " is " ^ sbe bsym_table e');
    if debug then
      print_endline ("//Reduction " ^ name^ ": " ^ sbe bsym_table e ^ " => " ^ sbe bsym_table e');
    true,e'

  | None -> false, e

(* Performs AT MOST ONE reduction *)
let rec ematchs nreds counter bsym_table name reds e =
  match reds with
  | [] -> false,e
  | (bvs,bps,e1,e2) :: tail ->
    let tvars = map (fun (tvid, tvidx,_) -> tvidx) bvs in
    let evars = List.map (fun {Flx_bparameter.pindex=ix}->ix) bps in 
    let changed,e = ematch nreds counter bsym_table name tvars evars e1 e2 e in
    if changed then changed,e else
    ematchs nreds counter bsym_table name tail e

let rec reduce_exe nreds counter bsym_table (reductions: Flx_mtypes2.reduction_t list) count exe =
(*
print_endline ("Examining " ^ Flx_print.string_of_bexe bsym_table 0 exe);
*)
  if count = 0 then exe else
  let changed = ref false in
  let ems name reds e = 
    let chd,e = ematchs nreds counter bsym_table name reds e in
    if chd then changed := true;
    e
  in
  let exe2 = fold_left
    (fun exe (name,reds) ->
(*
      print_endline ("Check reduction rule " ^ name ^ " on " ^ string_of_bexe bsym_table 0 exe);
*)
      let em e = ems name reds e in
      (* apply reduction top down AND bottom up *)
      let rec em' e = 
        (* let e = em e in (* top down application *) *)
        em (Flx_bexpr.map ~f_bexpr:em' e)  (* bottom up application *)
      in
      Flx_bexe.map ~f_bexpr:em' exe
    )
    exe
    reductions
  in
  if !changed then begin 
(*
    print_endline ("REDUCTION PERFORMED: Result= " ^ Flx_print.string_of_bexe bsym_table 0 exe2);
*)
    reduce_exe nreds counter bsym_table reductions (count - 1) exe2   
  end
  else exe

let reduce_exes nreds counter bsym_table reductions exes =
  map (reduce_exe nreds counter bsym_table reductions 10) exes


let reduce_all counter bsym_table = 
  let nreds = ref 0 in
  let reductions = Flx_bsym_table.get_reductions bsym_table in
  if debug then
  print_endline ("reduce all .. " ^ string_of_int (List.length reductions) ^ " reductions");

  Flx_bsym_table.iter
  (fun bid _ ({Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} as bsym) -> 
     (* print_endline ("BSYM " ^ id); *)
     match bbdcl with
     | Flx_bbdcl.BBDCL_fun (prop,bvs,ps,res,effects,exes) ->
       let exes2 = reduce_exes nreds counter bsym_table reductions exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res, effects, exes2) in 
       Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
     | _ -> ()
  )
  bsym_table;
  if !nreds > 0 then
    if debug then 
    print_endline (" ... performed " ^ string_of_int !nreds ^ " reductions")

let filter_viable_reductions nutab reductions =
  let uses x =
    let bids = ref BidSet.empty in
    let add_bid i = bids := BidSet.add i !bids in 
    let f_btype t = Flx_btype.iter ~f_bid:add_bid t in
    Flx_bexpr.iter ~f_btype ~f_bid:add_bid x;
    !bids
  in

  let subtract a b =
    BidSet.fold (fun elt result -> BidSet.remove elt result) b a
  in
  let reds = 
    List.fold_left (fun reds (id, cases) ->
      let cases = List.fold_left (fun cases (bvs,bps,x1,x2 as case) ->
        (* calculate external bids required for this reduction to work *)
        let vs = List.map (fun (_,i,_) -> i) bvs in
        let ps = List.map (fun {Flx_bparameter.pindex=i} -> i) bps in
        (*
        print_endline ("Reduce " ^ id ^ "[" ^ catmap "," string_of_int vs ^ "](" ^ catmap "," string_of_int ps ^ ": " ^
        Flx_print.sbe bsym_table x1 ^ " => " ^ Flx_print.sbe bsym_table x1);
        *)
        let vsbids = bidset_of_list ps in
        let psbids = bidset_of_list vs in
        let sr = Flx_srcref.dummy_sr in
        let x1bids = uses x1 in
        let x2bids = uses x2 in
        let extbids = subtract (BidSet.union x1bids x2bids) (BidSet.union vsbids psbids) in
        let bidavail = BidSet.fold (fun bid avail -> avail && Flx_bsym_table.mem nutab bid) extbids true in
        if bidavail then begin
(*
          print_endline ("KEEPING    reduction .. " ^ id ^ ":" ^ Flx_print.sbe nutab x1 ^ " => " ^ Flx_print.sbe nutab x2);
*)
          (bvs,bps,x1,x2) :: cases 
        end else cases
        ) 
        []
        cases
      in 
      if List.length cases > 0 then
        (id, List.rev cases) :: reds
      else
        reds
    )
    []
    reductions
  in 
  List.rev reds



