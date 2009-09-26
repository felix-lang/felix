type frontend_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  closure_state: Flx_mkcls.closure_state_t;
  use: Flx_call.usage_table_t;
}


let make_frontend_state syms = {
  syms=syms;
  closure_state=Flx_mkcls.make_closure_state syms;
  use=Hashtbl.create 97;
}


(* Convenience function for printing debug statements. *)
let print_debug state msg =
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag
  then print_endline msg


(* Convert curried functions to uncurried functions so they can ba applied
 * directly instead of requiring closures. *)
let uncurry_functions state bsym_table clean_bsym_table =
  let child_map = Flx_child.cal_children bsym_table in
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in
  let counter = ref 0 in

  while Flx_uncurry.uncurry_gen state.syms !bsym_table !child_map > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";

    (* Remove unused symbols. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used state.syms !bsym_table
      else !bsym_table;

    child_map := Flx_child.cal_children !bsym_table;
  done;

  !bsym_table


let inline_functions state bsym_table root_proc clean_bsym_table =
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "INPUT TO OPTIMISATION PASS";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.sym_table bsym_table
  end;

  (* Remove unused reductions. *)
  state.syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    state.syms
    bsym_table
    state.syms.Flx_mtypes2.reductions;

  Flx_typeclass.fixup_typeclass_instances state.syms bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  (* Perform the inlining. *)
  let child_map = Flx_child.cal_children bsym_table in
  Flx_inline.heavy_inlining state.syms bsym_table child_map;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  print_debug state "PHASE 1 INLINING COMPLETE";
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST PHASE 1 FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.sym_table bsym_table
  end;

  (* Clean up the inlining symbol properties. *)
  Hashtbl.iter begin fun i _ ->
    Flx_prop.rem_prop bsym_table `Inlining_started i;
    Flx_prop.rem_prop bsym_table `Inlining_complete i;
  end bsym_table;

  let child_map = Flx_child.cal_children bsym_table in
  Flx_intpoly.cal_polyvars state.syms bsym_table child_map;

  Flx_inst.instantiate
    state.syms
    bsym_table
    true
    root_proc
    state.syms.Flx_mtypes2.bifaces;

  (* EXPERIMENTAL!
    Adds monomorphic versions of all symbols.
    This will do nothing, because they're not
    actually instantiated!
  *)
  print_debug state "//MONOMORPHISING";
  Flx_mono.monomorphise state.syms bsym_table;
  print_debug state "//MONOMORPHISING DONE";

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in

  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST MONOMORPHISATION FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.sym_table bsym_table
  end;

  (* Remove any newly unused reductions. *)
  print_debug state "//Removing useless reductions";
  state.syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    state.syms
    bsym_table
    state.syms.Flx_mtypes2.reductions;

  (* Do another inlining pass. *)
  print_debug state "//INLINING";
  Flx_typeclass.fixup_typeclass_instances state.syms bsym_table;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in
  Flx_inline.heavy_inlining state.syms bsym_table child_map;

  (*
  print_endline "INLINING DONE: RESULT:";
  print_symbols state.syms.sym_table bsym_table;
  *)

  (* Remove unused symbols. *)
  bsym_table, child_map


let mkproc state bsym_table clean_bsym_table =
  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  let child_map = Flx_child.cal_children bsym_table in

  (* XXX: What does mkproc do? *)
  let bsym_table = ref bsym_table in
  let child_map = ref child_map in
  let counter = ref 0 in
  while Flx_mkproc.mkproc_gen state.syms !bsym_table !child_map > 0 do
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";

    (* Clean up the symbol table. *)
    bsym_table :=
      if clean_bsym_table
      then Flx_use.copy_used state.syms !bsym_table
      else !bsym_table;

    child_map := Flx_child.cal_children !bsym_table;
  done;

  !bsym_table


let stack_calls state bsym_table =
  (* Convert functions into stack calls. *)
  Flx_typeclass.fixup_typeclass_instances state.syms bsym_table;
  print_debug state "//Calculating stackable calls";

  let label_map = Flx_label.create_label_map
    bsym_table
    state.syms.Flx_mtypes2.counter
  in
  let label_usage = Flx_label.create_label_usage
    state.syms
    bsym_table
    label_map
  in

  let child_map = Flx_child.cal_children bsym_table in
  Flx_stack_calls.make_stack_calls
    state.syms
    bsym_table
    child_map
    label_map
    label_usage;

  bsym_table, child_map


(* Do some platform independent optimizations of the code. *)
let optimize state bsym_table root_proc clean_bsym_table =
  print_debug state "//OPTIMISING";

  (* Find the root and exported functions and types. *)
  Flx_use.find_roots state.syms bsym_table root_proc state.syms.Flx_mtypes2.bifaces;

  (* Clean up the symbol table. *)
  let bsym_table =
    if clean_bsym_table
    then Flx_use.copy_used state.syms bsym_table
    else bsym_table
  in

  (* Uncurry curried functions. *)
  let bsym_table = uncurry_functions state bsym_table  clean_bsym_table in

  (* Inline functions. *)
  let bsym_table, _ =
    let compiler_options = state.syms.Flx_mtypes2.compiler_options in

    (* Exit early if we don't want to do any inlining. *)
    if compiler_options.Flx_mtypes2.max_inline_length <= 0
    then bsym_table, (Flx_child.make ())
    else inline_functions state bsym_table root_proc clean_bsym_table
  in

  (* XXX: Not sure what this does. *)
  let bsym_table = mkproc state bsym_table clean_bsym_table in

  (* Eliminate dead code. *)
  let elim_state = Flx_elim.make_elim_state state.syms bsym_table in
  Flx_elim.eliminate_unused elim_state;

  (* Convert functions into stack calls. *)
  let bsym_table, child_map = stack_calls state bsym_table in

  bsym_table, child_map


let lower_symbols state bsym_table child_map root_proc bexes bids =
  (* Add the symbols to the child map. *)
  List.iter begin fun bid ->
    let (_,parent,_,_) = Hashtbl.find bsym_table bid in
    match parent with
    | Some parent -> Flx_child.add_child child_map parent bid
    | None -> ()
  end bids;

  (* Check the typeclasses. *)
  let bids = Flx_typeclass.typeclass_instance_check_symbols
    state.syms
    bsym_table
    child_map
    bids
  in

  (* Optimize the symbols. *)
  let bsym_table, child_map = optimize
    state
    bsym_table
    root_proc
    false
  in

  (* Wrap closures. *)
  print_debug state "//Generating primitive wrapper closures";
  let bids = Flx_mkcls.make_closure state.closure_state bsym_table bids in

  (* Mark which functions are using global state. *)
  print_debug state "//Finding which functions use globals";

  (* Remove unused symbols. *)
  (* FIXME: This is disabled because it deletes all the symbols.
  let bsym_table = Flx_use.copy_used state.syms bsym_table in
  *)

  (* Mark all the global functions and values. *)
  let symbols = Flx_global.set_globals_for_symbols
    bsym_table
    state.use
    bids
  in

  bsym_table, child_map, bexes, bids

(* Prep the bsym_table for the backend by lowering and simplifying symbols. *)
let lower_bsym_table state bsym_table root_proc =
  (* Wrap closures. *)
  print_debug state "//Generating primitive wrapper closures";
  Flx_mkcls.make_closures state.closure_state bsym_table;

  (* Mark which functions are using global state. *)
  print_debug state "//Finding which functions use globals";

  (* Remove unused symbols. *)
  let bsym_table = Flx_use.copy_used state.syms bsym_table in

  (* Mark all the global functions and values. *)
  Flx_global.set_globals state.syms bsym_table;

  (* Instantiate type classes. *)
  print_debug state "//instantiating";

  let child_map = Flx_child.cal_children bsym_table in
  Flx_intpoly.cal_polyvars state.syms bsym_table child_map;
  Flx_inst.instantiate
    state.syms
    bsym_table
    false
    root_proc
    state.syms.Flx_mtypes2.bifaces;

  (* fix up root procedures so if they're not stackable,
     then they need a heap closure -- wrappers require
     one or the other *)
  Flx_set.IntSet.iter begin fun i ->
    let id,parent,sr,entry = Hashtbl.find bsym_table i in
    match entry with
    | Flx_types.BBDCL_procedure (props,vs,p,exes) ->
        let props = ref props in

        if List.mem `Stackable !props then begin
          (* The procedure is stackable, so mark that we can use a stack
           * closure. *)
          if not (List.mem `Stack_closure !props)
          then props := `Stack_closure :: !props
        end else begin

          (* The procedure isn't stackable, so mark that it needs a heap
           * closure. *)
          if not (List.mem `Heap_closure !props)
          then props := `Heap_closure :: !props
        end;

        (* Make sure the procedure will get a stack frame. *)
        if not (List.mem `Requires_ptf !props)
        then props := `Requires_ptf :: !props;

        (* Update the procedure with the new properties. *)
        let entry = Flx_types.BBDCL_procedure (!props, vs,p,exes) in
        Hashtbl.replace bsym_table i (id,parent,sr,entry)
    | _ -> ()
  end !(state.syms.Flx_mtypes2.roots);

  bsym_table, child_map
