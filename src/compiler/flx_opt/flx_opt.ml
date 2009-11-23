(* Convenience function for printing debug statements. *)
let print_debug syms sym_table msg =
  if syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag
  then print_endline msg


(* Convert curried functions to uncurried functions so they can ba applied
 * directly instead of requiring closures. *)
let uncurry_functions syms sym_table bsym_table clean_bsym_table =
  let child_map = Flx_child.cal_children bsym_table in
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in
  let counter = ref 0 in

  while Flx_uncurry.uncurry_gen syms sym_table !bsym_table !child_map > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";

    (* Remove unused symbols. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used syms !bsym_table
      else !bsym_table;

    child_map := Flx_child.cal_children !bsym_table;
  done;

  !bsym_table


(* Recursively inline functions. *)
let inline_functions syms sym_table bsym_table root_proc clean_bsym_table =
  if syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "INPUT TO OPTIMISATION PASS";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols sym_table bsym_table
  end;

  (* Remove unused reductions. *)
  syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    syms
    sym_table
    bsym_table
    syms.Flx_mtypes2.reductions;

  Flx_typeclass.fixup_typeclass_instances syms sym_table bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  (* Perform the inlining. *)
  let child_map = Flx_child.cal_children bsym_table in
  Flx_inline.heavy_inlining syms sym_table bsym_table child_map;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  print_debug syms sym_table "PHASE 1 INLINING COMPLETE";
  if syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST PHASE 1 FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols sym_table bsym_table
  end;

  (* Clean up the inlining symbol properties. *)
  Flx_bsym_table.iter begin fun i _ ->
    Flx_prop.rem_prop bsym_table `Inlining_started i;
    Flx_prop.rem_prop bsym_table `Inlining_complete i;
  end bsym_table;

  let child_map = Flx_child.cal_children bsym_table in
  Flx_intpoly.cal_polyvars syms bsym_table child_map;

  Flx_inst.instantiate
    syms
    sym_table
    bsym_table
    true
    root_proc
    syms.Flx_mtypes2.bifaces;

  (* EXPERIMENTAL!
    Adds monomorphic versions of all symbols.
    This will do nothing, because they're not
    actually instantiated!
  *)
  print_debug syms sym_table "//MONOMORPHISING";
  Flx_mono.monomorphise syms sym_table bsym_table;
  print_debug syms sym_table "//MONOMORPHISING DONE";

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in

  if syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST MONOMORPHISATION FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols sym_table bsym_table
  end;

  (* Remove any newly unused reductions. *)
  print_debug syms sym_table "//Removing useless reductions";
  syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    syms
    sym_table
    bsym_table
    syms.Flx_mtypes2.reductions;

  (* Do another inlining pass. *)
  print_debug syms sym_table "//INLINING";
  Flx_typeclass.fixup_typeclass_instances syms sym_table bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in
  Flx_inline.heavy_inlining syms sym_table bsym_table child_map;

  (*
  print_endline "INLINING DONE: RESULT:";
  print_symbols sym_table bsym_table;
  *)

  (* Remove unused symbols. *)
  bsym_table, child_map


let mkproc syms sym_table bsym_table clean_bsym_table =
  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in

  (* XXX: What does mkproc do? *)
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in
  let counter = ref 0 in
  while Flx_mkproc.mkproc_gen syms sym_table !bsym_table !child_map > 0 do
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";

    (* Clean up the symbol table. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used syms !bsym_table
      else !bsym_table;

    child_map := Flx_child.cal_children !bsym_table;
  done;

  !bsym_table


(* Convert functions into stack calls. *)
let stack_calls syms sym_table bsym_table =
  (* Convert functions into stack calls. *)
  Flx_typeclass.fixup_typeclass_instances syms sym_table bsym_table;
  print_debug syms sym_table "//Calculating stackable calls";

  let label_map = Flx_label.create_label_map
    bsym_table
    syms.Flx_mtypes2.counter
  in
  let label_usage = Flx_label.create_label_usage
    syms
    sym_table
    bsym_table
    label_map
  in

  let child_map = Flx_child.cal_children bsym_table in
  Flx_stack_calls.make_stack_calls
    syms
    bsym_table
    child_map
    label_map
    label_usage;

  bsym_table, child_map


(* Do some platform independent optimizations of the code. *)
let optimize_bsym_table' syms sym_table bsym_table root_proc clean_bsym_table =
  print_debug syms sym_table "//OPTIMISING";

  (* Find the root and exported functions and types. *)
  Flx_use.find_roots syms bsym_table root_proc syms.Flx_mtypes2.bifaces;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  (* Uncurry curried functions. *)
  let bsym_table = uncurry_functions syms sym_table bsym_table clean_bsym_table in

  (* Inline functions. *)
  let bsym_table, _ =
    let compiler_options = syms.Flx_mtypes2.compiler_options in

    (* Exit early if we don't want to do any inlining. *)
    if compiler_options.Flx_mtypes2.max_inline_length <= 0
    then bsym_table, (Flx_child.make ())
    else inline_functions syms sym_table bsym_table root_proc clean_bsym_table
  in

  (* XXX: Not sure what this does. *)
  let bsym_table = mkproc syms sym_table bsym_table clean_bsym_table in

  (* Eliminate dead code. *)
  let elim_state = Flx_elim.make_elim_state syms bsym_table in
  Flx_elim.eliminate_unused elim_state;

  (* Convert functions into stack calls. *)
  let bsym_table, child_map = stack_calls syms sym_table bsym_table in

  bsym_table, child_map


let optimize_bsym_table syms sym_table bsym_table root_proc =
  optimize_bsym_table' syms sym_table bsym_table root_proc true


let optimize syms sym_table bsym_table child_map root_proc bids bexes =
  (* Add the symbols to the child map. *)
  List.iter begin fun bid ->
    let (_,parent,_,_) = Flx_bsym_table.find bsym_table bid in
    match parent with
    | Some parent -> Flx_child.add_child child_map parent bid
    | None -> ()
  end bids;

  (* Check the typeclasses. *)
  let bids = Flx_typeclass.typeclass_instance_check_symbols
    syms
    sym_table
    bsym_table
    child_map
    bids
  in

  (* Optimize the symbols. *)
  let bsym_table, child_map = optimize_bsym_table'
    syms
    sym_table
    bsym_table
    root_proc
    false
  in

  bsym_table, child_map, bids, bexes
