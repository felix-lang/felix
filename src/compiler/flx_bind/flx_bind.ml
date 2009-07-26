type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbind_state: Flx_bbind.bbind_state_t;
  symtab: Flx_symtab.t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
}

let make_bind_state syms =
  let bbdfns = Hashtbl.create 97 in
  {
    syms = syms;
    bbind_state = Flx_bbind.make_bbind_state syms bbdfns;
    symtab = Flx_symtab.make syms;
    bbdfns = bbdfns;
  }

let bind_asms bind_state asms =
  (* Add the symbols to the symtab. *)
  let exes, ifaces = Flx_symtab.add_asms bind_state.symtab asms in

  (* Now, bind all the symbols. *)
  Flx_bbind.bbind bind_state.bbind_state;

  (* Downgrade abstract types. *)
  let bbdfns = Flx_strabs.strabs bind_state.syms bind_state.bbdfns in

  (* Bind the interfaces. *)
  bind_state.syms.Flx_mtypes2.bifaces <- List.map
    (Flx_bbind.bind_interface bind_state.bbind_state) ifaces;

  (* Clear the type cache. *)
  Hashtbl.clear bind_state.syms.Flx_mtypes2.ticache;

  (* Return the bound symbol table. *)
  bbdfns
