type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  sym_table: Flx_sym_table.t;
  symtab: Flx_symtab.t;
  parent: Flx_types.bid_t option;
  strabs_state: Flx_strabs.strabs_state_t;
  bexe_state: Flx_bexe.bexe_state_t;
  lookup_state: Flx_lookup.lookup_state_t;
  bbind_state: Flx_bbind.bbind_state_t;
}

type bound_t =
  | Bound_exe of Flx_types.bexe_t
  | Bound_symbol of (Flx_types.bid_t * Flx_bsym.t)

(** Constructs the bind state needed for a batch compiler. *)
let make_bind_state syms =
  let sym_table = Flx_sym_table.create () in
  let lookup_state = Flx_lookup.make_lookup_state syms sym_table in
  let bbind_state = Flx_bbind.make_bbind_state syms sym_table lookup_state in
  {
    syms = syms;
    sym_table = sym_table;
    symtab = Flx_symtab.make syms sym_table;
    parent = None;
    strabs_state = Flx_strabs.make_strabs_state ();
    bexe_state = Flx_bexe.make_bexe_state
      syms
      sym_table
      lookup_state
      []
      Flx_types.BTYP_void;
    lookup_state = lookup_state;
    bbind_state = bbind_state;
  }

(** Constructs the bind state needed for an interactive toplevel compiler. *)
let make_toplevel_bind_state syms =
  let sym_table = Flx_sym_table.create () in
  let symtab = Flx_symtab.make syms sym_table in
  let bsym_table = Flx_bsym_table.create () in
  let lookup_state = Flx_lookup.make_lookup_state syms sym_table in
  let bbind_state:Flx_bbind.bbind_state_t = Flx_bbind.make_bbind_state
    syms
    sym_table
    lookup_state
  in

  (* Declare the root module to work within *)
  let module_index, _ = Flx_symtab.add_dcl symtab (
    Flx_srcref.dummy_sr, "", None, `Public, Flx_ast.dfltvs,
    Flx_types.DCL_module [])
  in
  let module_sym = Flx_sym_table.find sym_table module_index in

  (* Find the module's _init_ function *)
  let init_index =
    match Hashtbl.find module_sym.Flx_sym.pubmap "_init_" with
    | Flx_types.FunctionEntry [ { Flx_types.base_sym=base_sym } ] -> base_sym
    | _ -> assert false
  in
  let init_sym = Flx_sym_table.find sym_table init_index in

  (* Bind the module and init function. *)
  ignore (Flx_bbind.bbind_symbol
    bbind_state
    bsym_table
    module_index
    module_sym);
  ignore (Flx_bbind.bbind_symbol
    bbind_state
    bsym_table
    init_index
    init_sym);

  {
    syms = syms;
    sym_table = sym_table;
    symtab = symtab;
    parent = Some module_index;
    strabs_state = Flx_strabs.make_strabs_state ();
    bexe_state = Flx_bexe.make_bexe_state
      ~parent:module_index
      ~env:(Flx_lookup.build_env lookup_state (Some init_index))
      syms
      sym_table
      lookup_state
      []
      Flx_types.BTYP_void;
    lookup_state = lookup_state;
    bbind_state = bbind_state;
  }, bsym_table

let bind_asm state bsym_table handle_bound init asm =
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
          state.bbind_state
          bsym_table
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
          state.bbind_state
          bsym_table
          i
          s)
    end
  end state.syms.Flx_mtypes2.counter initial_index;

  (* Now that we've bound all the symbols, we can downgrade the types. *)
  let init = ref init in

  Flx_mtypes2.iter_bids begin fun i ->
    (* First, find the symbol to bind. *)
    let symbol =
      try Some (Flx_bsym_table.find bsym_table i)
      with Not_found -> None
    in
    begin match symbol with
    | None -> ()
    | Some s ->
        (* Finally, downgrade abstract types. *)
        Flx_strabs.strabs_symbol
            state.strabs_state
            bsym_table
            i
            s
    end
  end state.syms.Flx_mtypes2.counter initial_index;

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
  end state.syms.Flx_mtypes2.counter initial_index;

  (* Return the folded value. *)
  !init

let bind_asms state asms =
  (* Add the symbols to the symtab. *)
  let exes, ifaces = Flx_symtab.add_asms state.symtab asms in

  (* Create us a bound symbol table. *)
  let bsym_table = Flx_bsym_table.create () in

  (* Now, bind all the symbols. *)
  Flx_bbind.bbind state.bbind_state bsym_table;

  (* Downgrade abstract types. *)
  Flx_strabs.strabs state.strabs_state bsym_table;

  (* Bind the interfaces. *)
  state.syms.Flx_mtypes2.bifaces <- List.map
    (Flx_bbind.bind_interface state.bbind_state bsym_table) ifaces;

  (* Clear the type cache. *)
  Hashtbl.clear state.syms.Flx_mtypes2.ticache;

  bsym_table

(** Find the root module's init function index. *)
let find_root_module_init_function state root =
  (* Look up the root procedure index. *)
  let { Flx_sym.pubmap=pubmap; symdef=symdef } =
    try Flx_sym_table.find state.sym_table root with Not_found ->
      failwith ("Can't find root module " ^ Flx_print.string_of_bid root ^
        " in symbol table?")
  in
  begin match symdef with
    | Flx_types.SYMDEF_module -> ()
    | _ -> failwith "Expected to find top level module ''"
  end;
  let entry =
    try Hashtbl.find pubmap "_init_" with Not_found ->
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
