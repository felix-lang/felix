(* code generation driver *)

open Flx_options
open Flx_mtypes2
open Flx_types
open Flx_version
open Flxg_state
;;

(* We have to set the felix version first. *)
Flx_version_hook.set_version ()

(* -------------------------------------------------------------------------- *)
let generate_dep_file state =
  Flxg_file.output_string state.dep_file (String.concat "\n" (!(state.syms.include_files)) ^ "\n");
  Flxg_file.close_out state.dep_file
  (* ; print_endline ("Written dependency file " ^ Flxg_file.filename state.dep_file) *)
 
(* -------------------------------------------------------------------------- *)
let generate_static_link_thunk state module_name =
  let module_name = Flx_name.cid_of_flxid state.module_name in
  let s = 
     "extern \"C\" void "^ module_name ^ "_create_thread_frame();\n" ^
     "extern \"C\" void "^ module_name ^ "_flx_start();\n" ^
     "void* static_create_thread_frame = (void*)" ^ module_name ^ "_create_thread_frame;\n" ^
     "void* static_flx_start = (void*)" ^ module_name ^ "_flx_start;\n" 
  in
  Flxg_file.output_string state.static_link_thunk_file s;
  Flxg_file.close_out state.static_link_thunk_file

(* -------------------------------------------------------------------------- *)
(** Save basic profiling numbers. *)
let save_profile () =
  let fname = "flxg_stats.txt" in
  (* failure to save stats isn't fatal *)
  try
    let f = open_out fname in
    Flx_profile.print f;
    close_out f
  with _ -> ()


(* -------------------------------------------------------------------------- *)
let showtime (s:string) (t0:float) =
  let s = s ^ "                   " in 
  let s = String.sub s 0 8 in
  let elapsed = Unix.gettimeofday() -. t0 in
  let minutes = floor (elapsed /. 60.0) in
  let seconds = elapsed -. minutes *. 60.0 in
  print_endline ( " flxg: " ^ s ^ " : " ^ string_of_int (int_of_float minutes) ^ "m" ^ Printf.sprintf "%2.1f" seconds)

(* -------------------------------------------------------------------------- *)
(** Handle the assembly of the parse tree. *)
let handle_assembly state main_prog module_name =

  (* Load syntax automata from disk, either via image or recompile. *)
  let parser_state = Flx_profile.call
    "Flxg_parse.load_syntax"
    Flxg_parse.load_syntax
    state
  in

  (* print_endline "Flxg.HANDLE ASSEMBLY"; *)
  let start_counter = ref 100 in (* concordance! *)

  (* Load libs. Build them if necessary. *)
  let deps, excls, sym_table, bsym_table = Flxg_lib.process_libs
    state
    parser_state
    module_name
    start_counter
  in

  (* Create the symbol table assembly for the main program. *)
  let includes, depnames, stmtss, asmss = Flxg_assembly.assemble
    state
    parser_state
    !excls
    module_name
    (Flxg_assembly.NoSearch main_prog)
  in

  deps := depnames @ !deps;

  (* update the global include file list *)
  state.syms.include_files := !deps;

  (* print_endline "Flxg.HANDLE ASSEMBLY DONE"; *)
  start_counter, sym_table, bsym_table, stmtss, asmss


(* -------------------------------------------------------------------------- *)
(** Handle the parse phase (which is actually is integrated with the desugar
 * phase). *)
let handle_parse state main_prog module_name =

  (* Build assembly *)
  let start_counter, sym_table, bsym_table, stmtss, _ = handle_assembly
    state
    main_prog
    module_name
  in

  (* Group statements together *)
  let stmts = List.concat (List.rev stmtss) in

  start_counter, sym_table, bsym_table, stmts


(* -------------------------------------------------------------------------- *)
(** Handle the AST desugaring. *)
let handle_desugar state main_prog module_name =

  let t0 = Unix.gettimeofday () in

  (* Build assembly *)
  let start_counter, sym_table, bsym_table, _, asmss = handle_assembly
    state
    main_prog
    module_name
  in

  (* Group assemblies together *)
  let asms = List.concat (List.rev asmss) in

  if state.syms.compiler_options.showtime  then
    showtime "frontend" t0;

  start_counter, sym_table, bsym_table, asms


(* -------------------------------------------------------------------------- *)
(** Handle the type binding. *)
let handle_bind state main_prog module_name =

  (* Assemble again to grab all of the assemblies *)
  let start_counter, sym_table, bsym_table, asms = handle_desugar
    state
    main_prog
    module_name
  in
  
  let t0 = Unix.gettimeofday () in

  (* Do the binding here *)
  let root_proc = Flx_profile.call
    "Flxg_bind.bind"
    (Flxg_bind.bind state sym_table bsym_table module_name start_counter)
    asms
  in

  (* Build table for tracking virtual typeclasses to their instances. *)
  Flx_build_tctab.build_typeclass_to_instance_table state.syms bsym_table;

  (* generate axiom checks *)
  (* or not: the routine must be run to strip axiom checks out of the code *)
  Flx_axiom.axiom_check 
    state.syms 
    bsym_table
    state.syms.compiler_options.generate_axiom_checks;
  
  (* Walk through AST to verify labels are reachable from their calling 
     sites. Currently disabled. *)
  Flx_reachability.check_reachability bsym_table;

  (* Profiling. *)
  if state.syms.compiler_options.showtime  then
    showtime "bind" t0;

  (* Return *)
  bsym_table, root_proc


(* -------------------------------------------------------------------------- *)
(** Handle the optimization. *)
let handle_optimize state main_prog module_name =
  let bsym_table, root_proc = handle_bind state main_prog module_name in

(*
  (* Remove unused symbols. *)

  (* THIS DOESN'T WORK. WHY NOT? Seems like newtype isn't scanned
     properly. No idea why! After downgrading, optimise does this
     first thing, so, it has to be a problem with BBDCL_newtype!

     AH. I know. The scan is finding the newtype index, but it
     isn't propagating that to the representation .. wonder why?

     I mean, this HAS to work for say, structs.
  *)
  let bsym_table = Flx_use.copy_used state.syms bsym_table in
*)

  let t0 = Unix.gettimeofday () in

  (* Optimize the bound values. *)
  let bsym_table = Flx_profile.call
    "Flxg_opt.optimize"
    (Flxg_opt.optimize state bsym_table)
    root_proc
  in

  (* Profiling. *)
  if state.syms.compiler_options.showtime  then
    showtime "optimse" t0;

  (* Return. *)
  bsym_table, root_proc


(* -------------------------------------------------------------------------- *)
(** Handle the lowering of abstract types.  *)
let handle_lower state main_prog module_name =
  let bsym_table, root_proc = handle_optimize state main_prog module_name in

  let t0 = Unix.gettimeofday () in

  (* Lower the bound symbols for the backend. *)
  let bsym_table = Flx_profile.call
    "Flxg_lower.lower"
    (Flxg_lower.lower state bsym_table)
    root_proc
  in
  if state.syms.compiler_options.showtime  then
  showtime "lower" t0;
  bsym_table, root_proc


(* -------------------------------------------------------------------------- *)
(** Handle the code generation. *)
let handle_codegen state main_prog module_name =
  let bsym_table, root_proc = handle_lower state main_prog module_name in
  let t0 = Unix.gettimeofday () in
  (* Start working on the backend. *)
  Flx_profile.call
    "Flxg_codegen.codegen"
    (Flxg_codegen.codegen state bsym_table)
    root_proc
  ;
  generate_static_link_thunk state module_name;
  (* this HAS to be done last, in case the compiler is interrupted by, say,
     a Ctrl-C. We need the dep file to remain invalid until all the C++
     is emitted.
  *)
  generate_dep_file state;
  if state.syms.compiler_options.showtime  then
  showtime "codegen" t0

(* -------------------------------------------------------------------------- *)
let main () =
  let compiler_options = Flxg_options.parse_args () in
  let print_flag = compiler_options.print_flag in
  let d s = if print_flag then print_endline s in

  let state = Flxg_state.make_state compiler_options in

  (* The first file specified is the main program. *)
  let main_prog = List.hd compiler_options.files in

  (* Look up the name of the main module. *)
  let module_name = make_module_name main_prog in

  begin try
    begin match compiler_options.compiler_phase with
      | Phase_parse ->
          let _, _, _, stmts = handle_parse state (Flx_srcref.dummy_sr, main_prog) module_name in
          print_endline (Flx_print.string_of_compilation_unit stmts)

      | Phase_desugar ->
          let _, _, _, asms = handle_desugar state (Flx_srcref.dummy_sr, main_prog) module_name in
          print_endline (Flx_print.string_of_desugared asms)

      | Phase_bind ->
d "[flxg] Begin binding";
          let bsym_table, _ = handle_bind state (Flx_srcref.dummy_sr, main_prog) module_name in
d "[flxg] End binding";
          Flx_print.print_bsym_table bsym_table

      | Phase_optimize ->
d "[flxg] Begin optimisation";
          let bsym_table, _ = handle_optimize state (Flx_srcref.dummy_sr, main_prog) module_name in
d "[flxg] End optimisation";
          Flx_print.print_bsym_table bsym_table

      | Phase_lower ->
d "[flxg] Begin lowering";
          let bsym_table, _ = handle_lower state (Flx_srcref.dummy_sr, main_prog) module_name in
d "[flxg] End lowering";
          Flx_print.print_bsym_table bsym_table

      | Phase_codegen ->
d "[flxg] Begin codegen";
          handle_codegen state (Flx_srcref.dummy_sr, main_prog) module_name;
d "[flxg] End codegen";

          (* Not working at the moment for unknown reason, chucks Not_found.
          (* Generate the why file. *)
          generate_why_file state bsym_table root_proc;
          *)
    end
  with x ->
    Flxg_terminate.terminate compiler_options.reverse_return_parity x
  end;

  if compiler_options.reverse_return_parity then 1 else 0
;;

exit (Flx_util.finally
  save_profile
  (Flx_profile.call "Flxg.main" main)
  ())

