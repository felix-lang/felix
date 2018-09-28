open Flx_types
open Flx_mtypes2
open Flxg_state


(** Desugar the statements *)
let make_module module_name asms =
  let asms =
    [Dcl (
      Flx_srcref.dummy_sr,
      module_name,
      None,
      `Public,
      Flx_ast.dfltvs,
      DCL_root asms)]
  in
  asms


(** Bind the assemblies *)
let flxg_bind_asms state sym_table bsym_table start_counter asms =
  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//BINDING";

  let bind_state = Flx_bind_state.make_bind_state state.syms sym_table in
  Flx_bind.bind_asms bind_state bsym_table start_counter asms;

  if state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then
  print_endline "//BINDING OK"


(** Bind the root module. *)
let bind_root_module state sym_table bsym_table module_name =
  let entry =
    try Hashtbl.find sym_table 0
    with Not_found -> failwith "flxg: can't find root"
  in

  let sym =
    match entry with
    | { Flx_sym_table.parent=None; sym=sym } -> sym
    | _ -> failwith "flxg: expected root entry to have parent None"
  in

  let root_proc =
    match sym with
    | { Flx_sym.symdef=SYMDEF_root init_proc } -> init_proc
    | _ -> failwith "flxg: expected root entry to be SYMDEF_root"
  in

(*
  (* this is a hack .. oh well .. *)
  let root_proc = Flx_mtypes2.fresh_bid (state.syms.counter) in
  let dcl = DCL_function (
    ([],None),
    Flx_ast.TYP_void Flx_srcref.dummy_sr,
    [],
    (List.map (fun x -> Exe x) exes))
  in

  let asm = Dcl (
    Flx_srcref.dummy_sr,
    "_init_",
    Some root_proc,
    `Public,
    Flx_ast.dfltvs,
    dcl)
  in

  let asms = make_module module_name [asm] in

  (* Finally, bind the root module's init procedure. *)
  flxg_bind_asms state sym_table bsym_table root_proc asms;

  (* Extra finally, let's do some paranoia checks to make sure everything went
   * well. *)
  if not (Flx_sym_table.mem sym_table root_proc) then
    failwith "flxg: can't find init proc in unbound symbol table";

  if not (Flx_bsym_table.mem bsym_table root_proc) then
    failwith "flxg: can't find init proc in bound symbol table";
*)

  root_proc


(** Bind the asms. *)
let bind state sym_table bsym_table module_name start_counter asms =
  (* Make the toplevel module. *)
  let asms = make_module module_name asms in

  (* Bind the assemblies. *)
  Flx_profile.call
    "Flxg_bind.bind_asms"
    (flxg_bind_asms state sym_table bsym_table !start_counter)
    asms;

  (* Bind the root module's init procedure. *)
  let root_proc = Flx_profile.call
    "Flxg_bind.bind_root_module"
    (bind_root_module state sym_table bsym_table)
    module_name
  in

  root_proc

