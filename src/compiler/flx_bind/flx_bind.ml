open Flx_sym
type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  sym_table: Flx_sym_table.t;
  symtab: Flx_symtab.t;
  parent: Flx_types.bid_t option;
  bexe_state: Flx_bind_bexe.bexe_state_t;
  lookup_state: Flx_lookup_state.lookup_state_t;
  bbind_state: Flx_bbind.bbind_state_t;
}

type bound_t =
  | Bound_exe of Flx_bexe.t
  | Bound_symbol of (Flx_types.bid_t * Flx_bsym.t)

(** Constructs the bind state needed for a batch compiler. *)
let make_bind_state syms sym_table =
  let lookup_state = 
    Flx_lookup_state.make_lookup_state 
      syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
      syms.Flx_mtypes2.counter 
      syms.Flx_mtypes2.varmap
      syms.Flx_mtypes2.ticache
      sym_table 
  in
  let bbind_state = Flx_bbind.make_bbind_state 
   ~print_flag:syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
   ~counter:syms.Flx_mtypes2.counter
   ~sym_table 
   ~lookup_state: lookup_state
   ~ticache: syms.Flx_mtypes2.ticache  
   ~varmap: syms.Flx_mtypes2.varmap
   ~virtual_to_instances: syms.Flx_mtypes2.virtual_to_instances
   ~instances_of_typeclass: syms.Flx_mtypes2.instances_of_typeclass
   ~axioms: syms.Flx_mtypes2.axioms
   ~reductions: syms.Flx_mtypes2.reductions
 in
  {
    syms = syms;
    sym_table = sym_table;
    symtab = Flx_symtab.make sym_table;
    parent = None;
    bexe_state = Flx_bind_bexe.make_bexe_state
      syms.Flx_mtypes2.counter
      sym_table
      lookup_state
      []
      (Flx_btype.btyp_void ());
    lookup_state = lookup_state;
    bbind_state = bbind_state;
  }

let bind_asm state bsym_table handle_bound init asm =
  let print_flag = state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag in
  let counter_ref = state.syms.Flx_mtypes2.counter in
  (* We need to save the symbol index counter so we can bind all of the symbols
   * that we just added. *)
  let initial_index = !(state.syms.Flx_mtypes2.counter) in

  (* Add declarations to the symbol table *)
  let init =
    match asm with
    | Flx_types.Exe exe ->
        let bexes = Flx_bind_bexe.bind_exe state.bexe_state bsym_table exe in
        List.fold_left (fun acc bexe -> handle_bound acc (Bound_exe bexe)) init bexes

    | Flx_types.Dcl dcl ->
        ignore(Flx_symtab.add_dcl ?parent:state.parent print_flag counter_ref state.symtab dcl);
        init
    | Flx_types.Iface (sr,iface) ->
        let biface = Flx_bbind.bind_interface
          state.bbind_state
          bsym_table
          (sr, iface, state.parent)
        in
        state.syms.Flx_mtypes2.bifaces <-
          biface :: state.syms.Flx_mtypes2.bifaces;
        init
    | Flx_types.Dir dir -> init
  in

  let defered = ref [] in
  (* Now bind in order all of the symbols we added. *)
  Flx_mtypes2.iter_bids begin fun bid ->
    (* Skip this bid if we've already bound it. *)
    if Flx_bsym_table.mem bsym_table bid then () else

    (* Next, find the symbol to bind. *)
    match
      try Some (Flx_sym_table.find_with_parent state.sym_table bid)
      with Not_found -> None
    with
    | None -> ()
    | Some (parent, sym) ->
print_endline ("bind_symbol " ^ sym.Flx_sym.id ^ "??");
      begin match sym.symdef with
      | Flx_types.SYMDEF_function (([kind,pid,TYP_defer _,_],None),ret,props,exes) ->
print_endline ("bind_symbol " ^ sym.Flx_sym.id ^ " .. DEFERED");
        defered := bid :: !defered
      | _ -> 
print_endline ("bind_symbol " ^ sym.Flx_sym.id ^ " .. BINDING");
        (* Then, bind the symbol. *)
        ignore (Flx_bbind.bbind_symbol
          state.bbind_state
          bsym_table
          bid
          parent
          sym)
      end
  end initial_index !(state.syms.Flx_mtypes2.counter);

  print_endline ("Processing " ^ string_of_int (List.length (!defered)) ^ " defered syms");
  List.iter begin fun bid ->
    (* Skip this bid if we've already bound it. *)
    if Flx_bsym_table.mem bsym_table bid then () else

    (* Next, find the symbol to bind. *)
    match
      try Some (Flx_sym_table.find_with_parent state.sym_table bid)
      with Not_found -> None
    with
    | None -> ()
    | Some (parent, sym) ->
        (* Then, bind the symbol. *)
        ignore (Flx_bbind.bbind_symbol
          state.bbind_state
          bsym_table
          bid
          parent
          sym)
  end (!defered);


  (* Now that we've bound all the symbols, we can downgrade the types. *)
  let init = ref init in

  (* Finally, pass on the bound symbols to the client. *)
  Flx_mtypes2.iter_bids begin fun i ->
    (* First, find the symbol to bind. *)
    let symbol =
      try Some (Flx_bsym_table.find bsym_table i)
      with Not_found -> None
    in
    begin match symbol with
    | None -> ()
    | Some s ->
        (* ... and finally pass the symbol to the client *)
        init := handle_bound !init (Bound_symbol (i, s));
    end
  end initial_index !(state.syms.Flx_mtypes2.counter);

  (* Return the folded value. *)
  !init

let bind_asms bind_state bsym_table start_counter asms =
(*
print_endline "Flx_bind.bind_asms: Binding asms ..";
*)
  (* Add the symbols to the symtab. *)
  let print_flag = bind_state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag in
  let counter_ref = bind_state.syms.Flx_mtypes2.counter in
  Flx_symtab.add_asms print_flag counter_ref bind_state.symtab "root" 0 None asms;
(*
print_endline "Flx_bind.bind_asms: Making symbol table done";
*)
  let exes = Flx_symtab.get_init_exes bind_state.symtab in
  let ifaces = Flx_symtab.get_exports bind_state.symtab in
(*
print_endline ("Bind_asms: ifaces = " ^ string_of_int (List.length ifaces));
*)

(*
print_endline "Flx_bind.bind_asms: built symbol table";
*)
(*
print_endline (Flx_symtab.detail bind_state.symtab);
*)
  (* Now, bind all the symbols. *)
  Flx_bbind.bbind bind_state.bbind_state start_counter counter_ref bsym_table;
(*
print_endline "Flx_bind.bind_asms: bbind done";
*)
  (* Bind the interfaces. *)
  bind_state.syms.Flx_mtypes2.bifaces <- bind_state.syms.Flx_mtypes2.bifaces @ List.map
    (Flx_bbind.bind_interface bind_state.bbind_state bsym_table) ifaces
 
(*
;print_endline ("Flx_bind.bind_asms: " ^string_of_int (List.length bind_state.syms.Flx_mtypes2.bifaces)^ 
" interfaces bound, to state.syms.bifaces")
*)

(** Find the root module's init function index. *)
let find_root_module_init_function bind_state =
  (* Look up the root procedure index. *)
  let { Flx_sym.pubmap=pubmap; symdef=symdef } =
    try Flx_sym_table.find bind_state.sym_table 0 with Not_found ->
      failwith ("Can't find root entry " ^ Flx_print.string_of_bid 0 ^
        " in symbol table?")
  in

(* FIX ME *)
  begin match symdef with
    | Flx_types.SYMDEF_root _ -> ()
    | Flx_types.SYMDEF_module _ -> ()
    | _ -> failwith "Expected to find top level module ''"
  end;
  let entry =
    try Hashtbl.find pubmap "_init_" with Not_found ->
      failwith "Can't find name _init_ in top level module's name map"
  in
  let index =
    match entry with
    | Flx_btype.FunctionEntry [x] ->
        Flx_typing.sye x
    | Flx_btype.FunctionEntry [] ->
        failwith "Couldn't find '_init_'"
    | Flx_btype.FunctionEntry _ ->
        failwith "Too many top level procedures called '_init_'"
    | Flx_btype.NonFunctionEntry _ ->
        failwith "_init_ found but not procedure"
  in

  index


