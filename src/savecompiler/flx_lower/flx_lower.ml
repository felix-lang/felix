open Flx_bid

type lower_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  use: Flx_call.usage_table_t;
}


let make_lower_state syms = {
  syms=syms;
  use=Hashtbl.create 97;
}


(* Convenience function for printing debug statements. *)
let print_debug state msg =
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
  then print_endline msg


let remove_module_parents bsym_table =
  (* Remove module parents. *)
  Flx_bsym_table.iter begin fun bid parent bsym ->
    match parent with
    | Some parent ->
        begin
          try
            match Flx_bsym_table.find_bbdcl bsym_table parent with
            | Flx_bbdcl.BBDCL_module ->
                Flx_bsym_table.remove bsym_table bid;
                Flx_bsym_table.add bsym_table bid None bsym
            | _ -> ()
          with Not_found -> ()
        end
    | None -> ()
  end bsym_table


(* Prep the bsym_table for the backend by lowering and simplifying symbols. *)
let lower_bsym_table state bsym_table root_proc =
(*
print_endline "RUNNING OLD LOWER PROCESS";
*)

  (* We have to remove module parents before we can do code generation. *)
  (* remove_module_parents bsym_table; *)

(*
  (* Wrap closures. *)
  print_debug state "//Generating primitive wrapper closures";
  Flx_mkcls.mark_heap_closures state.syms bsym_table;
*)
  (* Mark which functions are using global state. *)
  print_debug state "//Finding which functions use globals";

  (* Remove unused symbols. *)
  let bsym_table = Flx_use.copy_used state.syms bsym_table in

  (* strip uniq types out *)
  let bsym_table = Flx_struniq.struniq bsym_table in


  (* Mark all the global functions and values. *)
  Flx_global.set_globals bsym_table;

  (* Instantiate polymorphic entities . *)

  (* NOTE: with the new setup, everything is monomorphic.
    However this routine is still required to sequence generated
    code in dependency order
  *)
  print_debug state "//instantiating";

  Flx_inst.instantiate
    state.syms
    bsym_table
    true (* CHANGE: instantiate parameters! *)
    root_proc
    state.syms.Flx_mtypes2.bifaces;

  (* fix up root procedures so if they're not stackable,
     then they need a heap closure -- wrappers require
     one or the other *)
  (* JS: Change 2 sept 2012: also for functions! *)
  BidSet.iter begin fun i ->
    let bsym = Flx_bsym_table.find bsym_table i in
    match Flx_bsym.bbdcl bsym with
    | Flx_bbdcl.BBDCL_fun (props,vs,p,ret,effects,exes) ->
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
          then props := `Requires_ptf :: `Heap_closure :: !props
        end;

        (* Update the procedure with the new properties. *)
        Flx_bsym_table.update_bbdcl bsym_table i
          (Flx_bbdcl.bbdcl_fun ( !props, vs, p, ret,effects, exes))
    | _ -> ()
  end !(state.syms.Flx_mtypes2.roots);

  bsym_table



