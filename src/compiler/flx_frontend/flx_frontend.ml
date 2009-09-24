type frontend_state_t = { syms: Flx_mtypes2.sym_state_t }


let make_frontend_state syms = { syms=syms }


(* Convenience function for printing debug statements. *)
let print_debug state msg =
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag
  then print_endline msg


(* Convert curried functions to uncurried functions so they can ba applied
 * directly instead of requiring closures. *)
let uncurry_functions state bbdfns =
  let child_map = Flx_child.cal_children bbdfns in
  let bbdfns = ref bbdfns in
  let child_map = ref child_map in
  let counter = ref 0 in

  while Flx_uncurry.uncurry_gen state.syms (!child_map,!bbdfns) > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";

    (* Remove unused symbols. *)
    bbdfns := Flx_use.copy_used state.syms !bbdfns;
    child_map := Flx_child.cal_children !bbdfns;
  done;

  !bbdfns


let inline_functions state bbdfns root_proc =
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "INPUT TO OPTIMISATION PASS";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.dfns bbdfns
  end;

  (* Remove unused reductions. *)
  state.syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    state.syms
    bbdfns
    state.syms.Flx_mtypes2.reductions;

  Flx_typeclass.fixup_typeclass_instances state.syms bbdfns;

  (* Clean up the symbol table. *)
  let bbdfns = Flx_use.copy_used state.syms bbdfns in

  (* Perform the inlining. *)
  let child_map = Flx_child.cal_children bbdfns in
  Flx_inline.heavy_inlining state.syms (child_map,bbdfns);

  (* Clean up the symbol table. *)
  let bbdfns = Flx_use.copy_used state.syms bbdfns in

  print_debug state "PHASE 1 INLINING COMPLETE";
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST PHASE 1 FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.dfns bbdfns
  end;

  (* Clean up the inlining symbol properties. *)
  Hashtbl.iter begin fun i _ ->
    Flx_prop.rem_prop bbdfns `Inlining_started i;
    Flx_prop.rem_prop bbdfns `Inlining_complete i;
  end bbdfns;

  let child_map = Flx_child.cal_children bbdfns in
  Flx_intpoly.cal_polyvars state.syms bbdfns child_map;

  Flx_inst.instantiate
    state.syms
    bbdfns
    true
    root_proc
    state.syms.Flx_mtypes2.bifaces;

  (* EXPERIMENTAL!
    Adds monomorphic versions of all symbols.
    This will do nothing, because they're not
    actually instantiated!
  *)
  print_debug state "//MONOMORPHISING";
  Flx_mono.monomorphise state.syms bbdfns;
  print_debug state "//MONOMORPHISING DONE";

  let bbdfns = Flx_use.copy_used state.syms bbdfns in
  let child_map = Flx_child.cal_children bbdfns in

  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag then begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST MONOMORPHISATION FUNCTION SET";
    print_endline "---------------------------";
    print_endline "";
    Flx_print.print_symbols state.syms.Flx_mtypes2.dfns bbdfns
  end;

  (* Remove any newly unused reductions. *)
  print_debug state "//Removing useless reductions";
  state.syms.Flx_mtypes2.reductions <- Flx_reduce.remove_useless_reductions
    state.syms
    bbdfns
    state.syms.Flx_mtypes2.reductions;

  (* Do another inlining pass. *)
  print_debug state "//INLINING";
  Flx_typeclass.fixup_typeclass_instances state.syms bbdfns;
  let bbdfns = Flx_use.copy_used state.syms bbdfns in
  let child_map = Flx_child.cal_children bbdfns in
  Flx_inline.heavy_inlining state.syms (child_map,bbdfns);

  (*
  print_endline "INLINING DONE: RESULT:";
  print_symbols state.syms.dfns bbdfns;
  *)

  (* Remove unused symbols. *)
  bbdfns


let mkproc state bbdfns =
  (* Clean up the symbol table. *)
  let bbdfns = Flx_use.copy_used state.syms bbdfns in
  let child_map = Flx_child.cal_children bbdfns in

  (* XXX: What does mkproc do? *)
  let bbdfns = ref bbdfns in
  let child_map = ref child_map in
  let counter = ref 0 in
  while Flx_mkproc.mkproc_gen state.syms (!child_map,!bbdfns) > 0 do
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";
    bbdfns := Flx_use.copy_used state.syms !bbdfns;
    child_map := Flx_child.cal_children !bbdfns;
  done;

  !bbdfns


let stack_calls state bbdfns =
  (* Convert functions into stack calls. *)
  Flx_typeclass.fixup_typeclass_instances state.syms bbdfns;
  print_debug state "//Calculating stackable calls";

  let label_map = Flx_label.create_label_map
    bbdfns
    state.syms.Flx_mtypes2.counter
  in
  let label_usage = Flx_label.create_label_usage
    state.syms
    bbdfns
    label_map
  in

  let child_map = Flx_child.cal_children bbdfns in
  Flx_stack_calls.make_stack_calls
    state.syms
    (child_map, bbdfns)
    label_map
    label_usage;

  bbdfns, child_map


(* Do some platform independent optimizations of the code. *)
let optimize state bbdfns root_proc =
  print_debug state "//OPTIMISING";

  (* Find the root and exported functions and types. *)
  Flx_use.find_roots state.syms bbdfns root_proc state.syms.Flx_mtypes2.bifaces;

  (* Throw away all the unused symbols. *)
  let bbdfns = Flx_use.copy_used state.syms bbdfns in

  (* Uncurry curried functions. *)
  let bbdfns = uncurry_functions state bbdfns in

  (* Inline functions. *)
  let bbdfns =
    let compiler_options = state.syms.Flx_mtypes2.compiler_options in

    (* Exit early if we don't want to do any inlining. *)
    if compiler_options.Flx_mtypes2.max_inline_length <= 0
    then bbdfns
    else inline_functions state bbdfns root_proc
  in

  (* XXX: Not sure what this does. *)
  let bbdfns = mkproc state bbdfns in

  (* Eliminate dead code. *)
  let elim_state = Flx_elim.make_elim_state state.syms bbdfns in
  Flx_elim.eliminate_unused elim_state;

  (* Convert functions into stack calls. *)
  let bbdfns, child_map = stack_calls state bbdfns in

  bbdfns


(* Prep the bbdfns for the backend by lowering and simplifying symbols. *)
let lower_bbdfns state bbdfns root_proc =
  (* Wrap closures. *)
  print_debug state "//Generating primitive wrapper closures";
  let closure_state = Flx_mkcls.make_closure_state state.syms bbdfns in
  Flx_mkcls.make_closures closure_state;

  (* Mark which functions are using global state. *)
  print_debug state "//Finding which functions use globals";

  (* Remove unused symbols. *)
  let bbdfns = Flx_use.copy_used state.syms bbdfns in
  Flx_global.set_globals state.syms bbdfns;

  (* Instantiate type classes. *)
  print_debug state "//instantiating";

  let child_map = Flx_child.cal_children bbdfns in
  Flx_intpoly.cal_polyvars state.syms bbdfns child_map;
  Flx_inst.instantiate
    state.syms
    bbdfns
    false
    root_proc
    state.syms.Flx_mtypes2.bifaces;

  (* fix up root procedures so if they're not stackable,
     then they need a heap closure -- wrappers require
     one or the other *)
  Flx_set.IntSet.iter begin fun i ->
    let id,parent,sr,entry = Hashtbl.find bbdfns i in
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
        Hashtbl.replace bbdfns i (id,parent,sr,entry)
    | _ -> ()
  end !(state.syms.Flx_mtypes2.roots);

  bbdfns, child_map
