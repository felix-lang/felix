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

let bind_asm ?parent bind_state handle_symbol asm init =
  (* We need to save the symbol index counter so we can bind all of the symbols
   * that we just added. *)
  let i = ref !(bind_state.syms.Flx_mtypes2.counter) in

  (* Add declarations to the symbol table *)
  begin
    match asm with
    | Flx_types.Exe exe -> ()
    | Flx_types.Dcl dcl ->
        ignore(Flx_symtab.add_dcl ?parent bind_state.symtab dcl)
    | Flx_types.Iface (sr,iface) ->
        let biface = Flx_bbind.bind_interface
          bind_state.bbind_state
          (sr,iface,parent)
        in
        bind_state.syms.Flx_mtypes2.bifaces <-
          biface :: bind_state.syms.Flx_mtypes2.bifaces
    | Flx_types.Dir dir -> ()
  end;

  (* Now bind in order all of the symbols we added. *)
  let init = ref init in

  while !i < !(bind_state.syms.Flx_mtypes2.counter) do
    let entry =
      try Some (Hashtbl.find bind_state.syms.Flx_mtypes2.dfns !i)
      with Not_found -> None
    in
    begin
      match entry with
      | Some entry ->
          Flx_bbind.bbind_symbol bind_state.bbind_state !i entry;

          (* Look up the bound value in the bbdnfs *)
          init := handle_symbol !i (Hashtbl.find bind_state.bbdfns !i) !init
      | None -> ()
    end;
    incr i
  done;

  !init

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
