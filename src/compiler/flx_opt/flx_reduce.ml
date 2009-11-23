open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_util
open List
open Flx_unify
open Flx_maps

let id x = x

let remove_useless_reductions syms sym_table bsym_table reductions =
  List.filter
  (fun (id,bvs,bps,e1,_) ->
    let psi = map (fun {pindex=i} -> i) bps in
    let ui i =
      let used = List.mem i psi or Flx_bsym_table.mem bsym_table i in
      if not used then begin
        if syms.compiler_options.print_flag then
        print_endline ("ELIDING USELESS REDUCTION " ^ id ^ " because "
          ^ string_of_bid i ^ " isn't found");
        raise Not_found
      end
    in
    begin
      try
        Flx_maps.iter_tbexpr ui ignore ignore e1;
        if syms.compiler_options.print_flag then
        print_endline ("Keep " ^ id (* ^ " matching " ^ sbe sym_table bsym_table e1 *));

        true
      with
      | Not_found ->
        if syms.compiler_options.print_flag then
        print_endline ("Discard " ^ id (* ^ " matching " ^ sbe sym_table bsym_table e1 *));
        false
    end
  )
  reductions

let ematch syms sym_table bsym_table changed (name,bvs,bps,e1,e2) tvars evars e =
  (*
  print_endline ("Matching " ^ sbe sym_table bsym_table e ^ " with " ^ sbe sym_table bsym_table e1);
  *)
  match Flx_unify.expr_maybe_matches syms.counter sym_table tvars evars e1 e with
  | Some (tmgu,emgu) ->
    changed := true;
      (*
      print_endline ("REDUCTION: FOUND A MATCH, candidate " ^ sbe sym_table bsym_table e^" with reduced LHS " ^ sbe sym_table bsym_table e1);
      print_endline ("EMGU=" ^catmap ", " (fun (i,e')-> si i ^ " --> " ^ sbe sym_table bsym_table e') emgu);
      print_endline ("TMGU=" ^catmap ", " (fun (i,t')-> si i ^ " --> " ^ sbt sym_table t') tmgu);
      *)
    let e = fold_left (fun e (i,e') -> Flx_unify.expr_term_subst e i e') e2 emgu in
    let rec s e = map_tbexpr id s (list_subst syms.counter tmgu) e in
    let e' = s e in
    (*
    print_endline ("RESULT OF SUBSTITUTION into RHS: " ^ sbe sym_table bsym_table e2 ^ " is " ^ sbe sym_table bsym_table e);
    *)
    if syms.compiler_options.print_flag then
      print_endline ("//Reduction " ^ sbe sym_table bsym_table e ^ " => " ^ sbe sym_table bsym_table e');
    e'

  | None -> e

let rec reduce_exe syms sym_table bsym_table reductions count exe =
  if count = 0 then exe else
  let changed = ref false in
  let exe = fold_left
    (fun exe (name,bvs,bps,e1,e2 as red,tvars,evars) ->
      (*
      print_endline ("Check reduction rule " ^ name ^ " on " ^ string_of_bexe sym_table bsym_table 0 exe);
      *)
      let em e = ematch syms sym_table bsym_table changed red tvars evars e in
      (* apply reduction top down AND bottom up *)
      let rec em' e = let e = em e in em (map_tbexpr id em' id e) in
      map_bexe id em' id id id exe
    )
    exe
    reductions
  in
  if !changed then reduce_exe syms sym_table bsym_table reductions (count - 1) exe
  else exe

let reduce_exes syms sym_table bsym_table reductions exes =
  let xreds = map
  (fun ((name,bvs,bps,e1,e2) as red) ->
    let tvars = map (fun (tvid, tvidx) -> tvidx) bvs in
    let evars = map (fun {pindex=eidx} -> eidx) bps in
    red,tvars,evars
  )
  reductions
  in

  map (reduce_exe syms sym_table bsym_table xreds 10) exes
