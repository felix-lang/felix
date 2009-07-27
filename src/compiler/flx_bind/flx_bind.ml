type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbind_state: Flx_bbind.bbind_state_t;
  symtab: Flx_symtab.t;
  bbind_bbdfns: Flx_types.fully_bound_symbol_table_t;
  strabs_state: Flx_strabs.strabs_state_t;
  strabs_bbdfns: Flx_types.fully_bound_symbol_table_t;
}

let make_bind_state syms =
  let bbind_bbdfns = Hashtbl.create 97 in
  let strabs_bbdfns = Hashtbl.create 97 in
  {
    syms = syms;
    symtab = Flx_symtab.make syms;
    bbind_bbdfns = bbind_bbdfns;
    bbind_state = Flx_bbind.make_bbind_state syms bbind_bbdfns;
    strabs_bbdfns = strabs_bbdfns;
    strabs_state = Flx_strabs.make_strabs_state bbind_bbdfns strabs_bbdfns;
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
    (* First, find the symbol to bind. *)
    begin
      match Flx_hashtbl.find bind_state.syms.Flx_mtypes2.dfns !i with
      | None -> ()
      | Some s ->
          (* Then, bind the symbol. *)
          match Flx_bbind.bbind_symbol bind_state.bbind_state !i s with
          | None -> ()
          | Some s ->
              (* Finally, downgrade abstract types. *)
              match Flx_strabs.strabs_symbol bind_state.strabs_state !i s with
              | None -> ()
              | Some s ->
                  init := handle_symbol !i s !init
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
  Flx_strabs.strabs bind_state.strabs_state;

  (* Bind the interfaces. *)
  bind_state.syms.Flx_mtypes2.bifaces <- List.map
    (Flx_bbind.bind_interface bind_state.bbind_state) ifaces;

  (* Clear the type cache. *)
  Hashtbl.clear bind_state.syms.Flx_mtypes2.ticache;

  (* Return the bound symbol table. *)
  bind_state.strabs_bbdfns
