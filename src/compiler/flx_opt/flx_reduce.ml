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
(*
let id x = x
(*
let remove_useless_reductions syms bsym_table reductions =
  List.filter
  (fun (id,bvs,bps,e1,_) ->
    let psi = Flx_bparameter.get_bids bps in
    let ui i =
      let used = List.mem i psi || Flx_bsym_table.mem bsym_table i in
      if not used then begin
        if syms.compiler_options.print_flag then
        print_endline ("ELIDING USELESS REDUCTION " ^ id ^ " because "
          ^ string_of_bid i ^ " isn't found");
        raise Not_found
      end
    in
    begin
      try
        Flx_bexpr.iter ~f_bid:ui e1;
        if syms.compiler_options.print_flag then
        print_endline ("Keep " ^ id (* ^ " matching " ^ sbe bsym_table e1 *));

        true
      with
      | Not_found ->
        if syms.compiler_options.print_flag then
        print_endline ("Discard " ^ id (* ^ " matching " ^ sbe bsym_table e1 *));
        false
    end
  )
  reductions
*)

(* match one reduction case against a fixed expression *)
let ematch syms bsym_table name tvars evars e1 e2 e : bool * Flx_bexpr.t =
  (*
  print_endline ("Matching " ^ sbe bsym_table e ^ " with " ^ sbe bsym_table e1);
  *)
  match Flx_unify_expr.expr_maybe_matches bsym_table syms.counter tvars evars e1 e with
  | Some (tmgu,emgu) ->
(*
      print_endline (name^" REDUCTION: FOUND A MATCH, candidate " ^ sbe bsym_table e^" with reduced LHS " ^ sbe bsym_table e1);
      print_endline ("EMGU=" ^catmap ", " (fun (i,e')-> si i ^ " --> " ^ sbe bsym_table e') emgu);
      print_endline ("TMGU=" ^catmap ", " (fun (i,t')-> si i ^ " --> " ^ sbt bsym_table t') tmgu);
*)
    let e = fold_left (fun e (i,e') -> Flx_unify_expr.expr_term_subst e i e') e2 emgu in
    let rec s e = Flx_bexpr.map ~f_btype:(list_subst syms.counter tmgu) ~f_bexpr:s e in
    let e' = s e in
(*
    print_endline ("RESULT OF SUBSTITUTION into RHS: " ^ sbe bsym_table e2 ^ " is " ^ sbe bsym_table e);
    if syms.compiler_options.print_flag then
*)
(*
      print_endline ("//Reduction " ^ name^ ": " ^ sbe bsym_table e ^ " => " ^ sbe bsym_table e');
*)
    true,e'

  | None -> false, e

let rec ematchs syms bsym_table name reds e =
  match reds with
  | [] -> false,e
  | (bvs,bps,e1,e2) :: tail ->
    let tvars = map (fun (tvid, tvidx,_) -> tvidx) bvs in
    let evars = Flx_bparams.get_bids bps in
    let changed,e = ematch syms bsym_table name tvars evars e1 e2 e in
    if changed then changed,e else
    ematchs syms bsym_table name tail e

let rec reduce_exe syms bsym_table reductions count exe =
  if count = 0 then exe else
  let changed = ref false in
  let ems name reds e = 
    let chd,e = ematchs syms bsym_table name reds e in
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
    reduce_exe syms bsym_table reductions (count - 1) exe2   
  end
  else exe

let reduce_exes syms bsym_table reductions exes =
  map (reduce_exe syms bsym_table reductions 10) exes

let reduce_all syms bsym_table = 
  let reductions = !(syms.reductions) in
  (* print_endline ("reduce all .. " ^ string_of_int (List.length reductions) ^ " reductions"); *)
  Flx_bsym_table.iter
  (fun bid _ ({Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} as bsym) -> 
     (* print_endline ("BSYM " ^ id); *)
     match bbdcl with
     | Flx_bbdcl.BBDCL_fun (prop,bvs,ps,res,effects,exes) ->
       let exes2 = reduce_exes syms bsym_table reductions exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res, effects, exes2) in 
       Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
     | _ -> ()
  )
  bsym_table
*)

(* temporary hack *)
let reduce_exes syms bsym_table reductions exes = exes
let reduce_all syms bsym_table = ()

