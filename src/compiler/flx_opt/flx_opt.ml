(* Convenience function for printing debug statements. *)
let print_debug syms msg =
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
  then print_endline msg


(* Convert curried functions to uncurried functions so they can ba applied
 * directly instead of requiring closures. *)
let uncurry_functions syms bsym_table clean_bsym_table =
  let bsym_table = ref bsym_table in
  let counter = ref 0 in

  while Flx_uncurry.uncurry_gen syms !bsym_table > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";

    (* Remove unused symbols. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used syms !bsym_table
      else !bsym_table
  done;

  !bsym_table


(* Recursively inline functions. *)
let inline_functions syms bsym_table root_proc clean_bsym_table =
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "INPUT TO OPTIMISATION PASS";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols bsym_table
  end;

  (* Remove unused reductions. *)
  syms.Flx_mtypes2.reductions := Flx_reduce.remove_useless_reductions
    syms
    bsym_table
    !(syms.Flx_mtypes2.reductions);

  Flx_typeclass.fixup_typeclass_instances syms bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  (* lets see if this works, make closure before inlining *)
  Flx_mkcls.premake_closures syms bsym_table;

  (* Perform the inlining. *)
  Flx_inline.heavy_inlining syms bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  print_debug syms "PHASE 1 INLINING COMPLETE";
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST PHASE 1 FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols bsym_table
  end;

  (* Clean up the inlining symbol properties. *)
  Flx_bsym_table.iter begin fun i _ _ ->
    Flx_prop.rem_prop bsym_table `Inlining_started i;
    Flx_prop.rem_prop bsym_table `Inlining_complete i
  end bsym_table;

  Flx_intpoly.cal_polyvars syms bsym_table;

  Flx_inst.instantiate
    syms
    bsym_table
    true
    root_proc
    syms.Flx_mtypes2.bifaces;

  (* EXPERIMENTAL!
    Adds monomorphic versions of all symbols.
    This will do nothing, because they're not
    actually instantiated!
  *)
  print_debug syms "//MONOMORPHISING";
  Flx_mono.monomorphise syms bsym_table;
  print_debug syms "//MONOMORPHISING DONE";

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST MONOMORPHISATION FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols bsym_table
  end;

  (* Remove any newly unused reductions. *)
  print_debug syms "//Removing useless reductions";
  syms.Flx_mtypes2.reductions := Flx_reduce.remove_useless_reductions
    syms
    bsym_table
    !(syms.Flx_mtypes2.reductions);

  (* Do another inlining pass. *)
  print_debug syms "//INLINING";
  Flx_typeclass.fixup_typeclass_instances syms bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  Flx_inline.heavy_inlining syms bsym_table;

  (*
  print_endline "INLINING DONE: RESULT:";
  print_symbols bsym_table;
  *)

  (* Remove unused symbols. *)
  bsym_table


let mkproc syms bsym_table clean_bsym_table =
  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  (* XXX: What does mkproc do? *)
  (* see below, it turns functions into procedures, by assigning
   * the return value to where an extra argument points
   *)
  let bsym_table = ref bsym_table in
  let counter = ref 0 in
  while Flx_mkproc.mkproc_gen syms !bsym_table > 0 do
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";

    (* Clean up the symbol table. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used syms !bsym_table
      else !bsym_table
  done;

  !bsym_table


(* Convert functions into stack calls. *)
let stack_calls syms bsym_table =
  (* Convert functions into stack calls. *)
  Flx_typeclass.fixup_typeclass_instances syms bsym_table;
  print_debug syms "//Calculating stackable calls";

  let label_map = Flx_label.create_label_map
    bsym_table
    syms.Flx_mtypes2.counter
  in
  let label_usage = Flx_label.create_label_usage bsym_table label_map in
  Flx_stack_calls.make_stack_calls
    syms
    bsym_table
    label_map
    label_usage;

  print_debug syms "//stackable calls done";

  bsym_table


(* Do some platform independent optimizations of the code. *)
let optimize_bsym_table' syms bsym_table root_proc clean_bsym_table =
  print_debug syms "//OPTIMISING";

  (* Find the root and exported functions and types. *)
  Flx_use.find_roots syms bsym_table root_proc syms.Flx_mtypes2.bifaces;

  (* comment out for production until it works *)
  (* let bsym_table = Flx_numono.monomorphise2 true syms bsym_table in *)


  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used syms bsym_table
    else bsym_table
  in

  (* Uncurry curried functions. *)
  let bsym_table = uncurry_functions syms bsym_table clean_bsym_table in

  (* Inline functions. *)
  let bsym_table =
    let compiler_options = syms.Flx_mtypes2.compiler_options in

    (* Exit early if we don't want to do any inlining. *)
    if compiler_options.Flx_options.max_inline_length <= 0
    then bsym_table
    else inline_functions syms bsym_table root_proc clean_bsym_table
  in

  (* XXX: Not sure what this does. *)
  (* JS: it turns some functions into procedures, eg if
   * you have y = f (x); then you could do instead f' (&y,x);
   * As procedures .. you can spawn fthreads etc and expect
   * it to work, you can't do that with functions.
   * *)
  let bsym_table = mkproc syms bsym_table clean_bsym_table in

  (* Eliminate dead code. *)
  let elim_state = Flx_elim.make_elim_state syms bsym_table in
  Flx_elim.eliminate_unused elim_state;

  (* Convert functions into stack calls. *)
  let bsym_table = stack_calls syms bsym_table in

  bsym_table


let optimize_bsym_table syms bsym_table root_proc =
  optimize_bsym_table' syms bsym_table root_proc true

