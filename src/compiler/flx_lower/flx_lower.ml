type lower_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  closure_state: Flx_mkcls.closure_state_t;
  use: Flx_call.usage_table_t;
}


let make_lower_state syms = {
  syms=syms;
  closure_state=Flx_mkcls.make_closure_state syms;
  use=Hashtbl.create 97;
}


(* Convenience function for printing debug statements. *)
let print_debug state msg =
  if state.syms.Flx_mtypes2.compiler_options.Flx_mtypes2.print_flag
  then print_endline msg


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
  Flx_types.BidSet.iter begin fun i ->
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


(* Prep the bexes and symbols for the backend by lowering and simplifying
 * symbols. *)
let lower state bsym_table child_map root_proc bids bexes =
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

  bsym_table, child_map, bids, bexes
