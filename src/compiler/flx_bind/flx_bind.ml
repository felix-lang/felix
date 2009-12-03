type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  sym_table: Flx_sym_table.t;
  symtab: Flx_symtab.t;
  parent: Flx_types.bid_t option;
  bbind_bsym_table: Flx_bsym_table.t;
  strabs_state: Flx_strabs.strabs_state_t;
  bexe_state: Flx_bexe.bexe_state_t;
}

type bound_t =
  | Bound_exe of Flx_types.bexe_t
  | Bound_symbol of (Flx_types.bid_t * Flx_bsym.t)

let make_bind_state ?parent ?env syms sym_table =
  {
    syms = syms;
    sym_table = sym_table;
    symtab = Flx_symtab.make syms sym_table;
    parent = parent;
    bbind_bsym_table = Flx_bsym_table.create ();
    strabs_state = Flx_strabs.make_strabs_state ();
    bexe_state = Flx_bexe.make_bexe_state
      ?parent
      ?env
      syms
      sym_table
      []
      Flx_types.BTYP_void;
  }

let bind_asm state strabs_bsym_table handle_bound init asm =
  (* We need to save the symbol index counter so we can bind all of the symbols
   * that we just added. *)
  let initial_index = !(state.syms.Flx_mtypes2.counter) in

  (* Add declarations to the symbol table *)
  let init =
    match asm with
    | Flx_types.Exe exe ->
        Flx_bexe.bind_exe state.bexe_state begin fun bexe init ->
          handle_bound init (Bound_exe bexe)
        end exe init
    | Flx_types.Dcl dcl ->
        ignore(Flx_symtab.add_dcl ?parent:state.parent state.symtab dcl);
        init
    | Flx_types.Iface (sr,iface) ->
        let biface = Flx_bbind.bind_interface
          state.syms
          state.sym_table
          state.bbind_bsym_table
          (sr, iface, state.parent)
        in
        state.syms.Flx_mtypes2.bifaces <-
          biface :: state.syms.Flx_mtypes2.bifaces;
        init
    | Flx_types.Dir dir -> init
  in

  (* Now bind in order all of the symbols we added. *)
  Flx_mtypes2.iter_bids begin fun i ->
    (* First, find the symbol to bind. *)
    let symbol =
      try Some (Flx_sym_table.find state.sym_table i)
      with Not_found -> None
    in
    begin match symbol with
    | None -> ()
    | Some s ->
        (* Then, bind the symbol. *)
        ignore (Flx_bbind.bbind_symbol
          state.syms
          state.sym_table
          state.bbind_bsym_table
          i
          s)
    end
  end state.syms.Flx_mtypes2.counter initial_index;

  (* Now that we've bound all the symbols, we can downgrade the types. *)
  let init = ref init in

  Flx_mtypes2.iter_bids begin fun i ->
    (* First, find the symbol to bind. *)
    let symbol =
      try Some (Flx_bsym_table.find state.bbind_bsym_table i)
      with Not_found -> None
    in
    begin match symbol with
    | None -> ()
    | Some s ->
        (* Finally, downgrade abstract types. *)
        ignore (Flx_strabs.strabs_symbol
            state.strabs_state
            state.bbind_bsym_table
            strabs_bsym_table
            i
            s)
    end
  end state.syms.Flx_mtypes2.counter initial_index;

  (* Finally, pass on the bound symbols to the client. *)
  Flx_mtypes2.iter_bids begin fun i ->
    (* First, find the symbol to bind. *)
    let symbol =
      try Some (Flx_bsym_table.find strabs_bsym_table i)
      with Not_found -> None
    in
    begin match symbol with
    | None -> ()
    | Some s ->
        (* ... and finally pass the symbol to the client *)
        init := handle_bound !init (Bound_symbol (i, s));
    end
  end state.syms.Flx_mtypes2.counter initial_index;

  (* Return the folded value. *)
  !init

let find_module_init sym_table root =
  (* Look up the root procedure index. *)
  let { Flx_sym.pubmap=name_map; symdef=symdef } =
    try Flx_sym_table.find sym_table root
    with Not_found ->
      failwith ("Can't find root module " ^ Flx_print.string_of_bid root ^
        " in symbol table?")
  in
  begin match symdef with
    | Flx_types.SYMDEF_module -> ()
    | _ -> failwith "Expected to find top level module ''"
  end;
  let entry =
    try Hashtbl.find name_map "_init_"
    with Not_found ->
      failwith "Can't find name _init_ in top level module's name map"
  in
  let index =
    match entry with
    | Flx_types.FunctionEntry [x] ->
        Flx_typing.sye x
    | Flx_types.FunctionEntry [] ->
        failwith "Couldn't find '_init_'"
    | Flx_types.FunctionEntry _ ->
        failwith "Too many top level procedures called '_init_'"
    | Flx_types.NonFunctionEntry _ ->
        failwith "_init_ found but not procedure"
  in

  index

let bind_asms state asms root =
  (* Add the symbols to the symtab. *)
  let exes, ifaces = Flx_symtab.add_asms state.symtab asms in

  (* Now, bind all the symbols. *)
  Flx_bbind.bbind state.syms state.sym_table state.bbind_bsym_table;

  (* Downgrade abstract types. *)
  let bsym_table = Flx_strabs.strabs
    state.strabs_state
    state.bbind_bsym_table
  in

  (* Bind the interfaces. *)
  state.syms.Flx_mtypes2.bifaces <- List.map
    (Flx_bbind.bind_interface state.syms state.sym_table bsym_table) ifaces;

  (* Clear the type cache. *)
  Hashtbl.clear state.syms.Flx_mtypes2.ticache;

  bsym_table, (find_module_init state.sym_table root)
