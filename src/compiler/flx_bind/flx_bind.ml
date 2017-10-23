open Flx_sym
open Flx_ast
open Flx_bid
open Flx_bind_state

let debug = false 

let bind_asms bind_state bsym_table start_counter asms =
if debug then
print_endline "Flx_bind.bind_asms: Binding asms ..";

  (* Add the symbols to the symtab. *)
  let print_flag = bind_state.syms.Flx_mtypes2.compiler_options.Flx_options.print_flag in
  let counter_ref = bind_state.syms.Flx_mtypes2.counter in
  Flx_symtab.add_asms print_flag counter_ref bind_state.symtab "root" 0 None asms;

if debug then
print_endline "Flx_bind.bind_asms: Making symbol table done";

Flx_subtypetable.report_subtypes bind_state.symtab.Flx_symtab.sym_table;



(*
  let exes = Flx_symtab.get_init_exes bind_state.symtab in
*)
  let ifaces = Flx_symtab.get_exports bind_state.symtab in
(*
print_endline "Flx_bind.bind_asms: built symbol table";
*)
(*
print_endline (Flx_symtab.detail bind_state.symtab);
*)
if debug then
print_endline "Flx_bind.bind_asms: binding symbol table";

  (* Now, bind all the symbols. *)
  Flx_bbind.bbind bind_state.bbind_state start_counter counter_ref bsym_table;

if debug then
print_endline ("Flx_bind.bind_asms: bsym_table created with " ^ 
string_of_int (Flx_bsym_table.length bsym_table));

if debug then
print_endline ("Flx_bind.bind_asms: binding " ^ string_of_int (List.length ifaces) ^ " ifaces");
  (* Bind the interfaces. *)
  bind_state.syms.Flx_mtypes2.bifaces <- bind_state.syms.Flx_mtypes2.bifaces @ List.map
    (Flx_bind_interfaces.bind_interface bind_state.bbind_state bsym_table) ifaces;

if debug then
print_endline "Flx_bind.bind_asms: interfaces bound";

Flx_breqs.simplify_reqs bsym_table;

if debug then
print_endline "Flx_bind.bind_asms: requirements simplified";

if debug then
print_endline "Flx_bind.bind_asms: start validation of bsym_table";

let f_btype t = if Flx_btype.complete_type t then () else
  print_endline ("BSYM_TABLE CONTAINS INCOMPLETE TYPE " ^ Flx_print.sbt bsym_table t ^ " = " ^ Flx_btype.st t)
in
begin try
  Flx_bsym_table.validate "post-construction" bsym_table
with Flx_bsym_table.IncompleteBsymTable (bid,bid2,_) ->
    print_endline ("Post construction, symbol " ^string_of_int bid2 
       ^ " used in " ^ string_of_int bid^ " missing from bound symbol table"
    );
    let bsym = Flx_bsym_table.find bsym_table bid in
    let bbdcl = Flx_bsym.bbdcl bsym in
    print_endline (Flx_print.string_of_bbdcl bsym_table bbdcl bid);
    Flx_print.print_bsym_table bsym_table;
    failwith "SYSTEM ERROR: construction failed"
end;

Flx_bsym_table.validate_types f_btype bsym_table;

if debug then
print_endline "Flx_bind.bind_asms: binding complete, validated closure, types"
 
(*
;print_endline ("Flx_bind.bind_asms: " ^string_of_int (List.length bind_state.syms.Flx_mtypes2.bifaces)^ 
" interfaces bound, to state.syms.bifaces")
*)

(** Find the root module's init function index. *)
let find_root_module_init_function_from_sym_table syms =
  (* Look up the root procedure index. *)
  let { Flx_sym.pubmap=pubmap; symdef=symdef } =
    try Flx_sym_table.find syms 0 with Not_found ->
      failwith ("Can't find root entry " ^ Flx_print.string_of_bid 0 ^
        " in symbol table?")
  in

  match symdef with
    | Flx_types.SYMDEF_root p -> None 
    (* | Flx_types.SYMDEF_module p -> p *)
    | _ -> failwith "Expected to find top level module ''"

let find_root_module_init_function_from_bind_state bind_state =
 find_root_module_init_function_from_sym_table bind_state.sym_table


