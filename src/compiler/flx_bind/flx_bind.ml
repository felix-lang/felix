type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  symtab: Flx_symtab.t;
  parent: Flx_ast.bid_t option;
  bbind_state: Flx_bbind.bbind_state_t;
  bbind_bbdfns: Flx_types.fully_bound_symbol_table_t;
  strabs_state: Flx_strabs.strabs_state_t;
  strabs_bbdfns: Flx_types.fully_bound_symbol_table_t;
}

let make_bind_state ?parent syms =
  let bbind_bbdfns = Hashtbl.create 97 in
  let strabs_bbdfns = Hashtbl.create 97 in
  {
    syms = syms;
    symtab = Flx_symtab.make syms;
    parent = parent;
    bbind_bbdfns = bbind_bbdfns;
    bbind_state = Flx_bbind.make_bbind_state syms bbind_bbdfns;
    strabs_bbdfns = strabs_bbdfns;
    strabs_state = Flx_strabs.make_strabs_state bbind_bbdfns strabs_bbdfns;
  }

let bind_asm state handle_symbol init asm =
  (* We need to save the symbol index counter so we can bind all of the symbols
   * that we just added. *)
  let i = ref !(state.syms.Flx_mtypes2.counter) in

  (* Add declarations to the symbol table *)
  begin
    match asm with
    | Flx_types.Exe exe -> ()
    | Flx_types.Dcl dcl ->
        ignore(Flx_symtab.add_dcl ?parent:state.parent state.symtab dcl)
    | Flx_types.Iface (sr,iface) ->
        let biface = Flx_bbind.bind_interface
          state.bbind_state
          (sr, iface, state.parent)
        in
        state.syms.Flx_mtypes2.bifaces <-
          biface :: state.syms.Flx_mtypes2.bifaces
    | Flx_types.Dir dir -> ()
  end;

  (* Now bind in order all of the symbols we added. *)
  let init = ref init in

  while !i < !(state.syms.Flx_mtypes2.counter) do
    (* First, find the symbol to bind. *)
    begin
      match Flx_hashtbl.find state.syms.Flx_mtypes2.dfns !i with
      | None -> ()
      | Some s ->
          (* Then, bind the symbol. *)
          match Flx_bbind.bbind_symbol state.bbind_state !i s with
          | None -> ()
          | Some s ->
              (* Finally, downgrade abstract types. *)
              match Flx_strabs.strabs_symbol state.strabs_state !i s with
              | None -> ()
              | Some s ->
                  init := handle_symbol !init !i s
    end;
    incr i
  done;

  !init

let bind_asms state asms =
  (* Add the symbols to the symtab. *)
  let exes, ifaces = Flx_symtab.add_asms state.symtab asms in

  (* Now, bind all the symbols. *)
  Flx_bbind.bbind state.bbind_state;

  (* Downgrade abstract types. *)
  Flx_strabs.strabs state.strabs_state;

  (* Bind the interfaces. *)
  state.syms.Flx_mtypes2.bifaces <- List.map
    (Flx_bbind.bind_interface state.bbind_state) ifaces;

  (* Clear the type cache. *)
  Hashtbl.clear state.syms.Flx_mtypes2.ticache;

  (* Return the bound symbol table. *)
  state.strabs_bbdfns
