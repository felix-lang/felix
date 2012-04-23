open Flx_mtypes2
open Flx_options
open Flx_types
open Flxg_state

(* -------------------------------------------------------------------------- *)

let process_lib
  state
  parser_state
  sym_table_ref
  bsym_table_ref
  excls
  outdir
  module_name
  start_counter
  lib
=
  let lib_filedir,lib_filename =
    Flx_filesys.find_include_dir
      ~include_dirs:state.syms.compiler_options.include_dirs
      (lib ^ ".flx"),lib
  in
  let lib_name = Flx_filesys.join lib_filedir (lib_filename ^ ".flx") in

  (* this is wrong, should be he max of the included file times, but until
   * we load the cached value we don't know what files are included.
   * To fix this we could use a separate dep file, or simply validate
   * the file after it is loaded. Of course to do that I have to put
   * the include file list into the cache!
   *)
  let lib_time = Flx_filesys.virtual_filetime Flx_filesys.big_crunch lib_name in
  let libtab_name = Flx_filesys.join lib_filedir lib_filename ^ ".libtab" in
  let libtab_name = (Flx_filesys.mk_cache_name outdir libtab_name) in
print_endline ("Libtab name = " ^ libtab_name);

  (* Look up the time the file was cached. *)
  let lib_cache_time = Flx_filesys.virtual_filetime
    Flx_filesys.big_bang
    libtab_name
  in

  (* Return if the file has been changed since it was cached. *)
  let validate (_,depnames,_,_,_,_,_,_,_,_,_) =
    let filetimes = List.fold_left (fun acc f ->
(* print_endline ("dep file = " ^ f); *)
      max acc (Flx_filesys.virtual_filetime Flx_filesys.big_crunch (f^".flx")))
      Flx_filesys.big_bang depnames
    in
    filetimes < lib_cache_time
  in

  let
    includes,
    depnames,
    saved_counter,
    varmap,
    ticache,
    typeclass_to_instance,
    instances_of_typeclass,
    axioms,
    reductions,
    out_sym_table,
    out_bsym_table
  =
    Flx_filesys.cached_computation "libtab" libtab_name
      ~outfile:None
      ~force_calc:(false || state.syms.compiler_options.force_recompile)
      ~min_time:lib_time
      ~validate
      (Flx_profile.call ("binding library " ^ lib) begin fun () ->
        (* make assembly outputs stuff in reversed order, but this routine
         * reversed it back again *)
        print_endline ("Binding libary " ^ lib);
        let includes, depnames, _, asmss = Flxg_assembly.assemble
          state
          parser_state
          !excls
          lib
          (Flxg_assembly.Search lib)
        in

        (* already in the right order now *)
        let asms = List.concat asmss in
        let asms = Flxg_bind.make_module module_name asms in

        (* Bind the assemblies. *)
        Flx_profile.call "Flxg_bind.bind_asms"
          (Flxg_bind.bind_asms
            state
            !sym_table_ref
            !bsym_table_ref
            !start_counter)
          asms;

        print_endline ("binding library " ^ lib ^ " done");

        print_endline (
          "Exports = " ^ string_of_int (List.length (state.syms.bifaces)));

        includes,
        depnames,
        !(state.syms.counter),
        state.syms.varmap,
        state.syms.ticache,
        state.syms.typeclass_to_instance,
        state.syms.instances_of_typeclass,
        !(state.syms.axioms),
        !(state.syms.reductions),
        !sym_table_ref,
        !bsym_table_ref
      end)
  in

  state.syms.counter := max !(state.syms.counter) saved_counter;
  state.syms.varmap <- varmap;
  state.syms.ticache <- ticache;
  state.syms.typeclass_to_instance <- typeclass_to_instance;
  state.syms.instances_of_typeclass <- instances_of_typeclass;
  state.syms.axioms := axioms;
  state.syms.reductions := reductions;
  start_counter := !(state.syms.counter);
  sym_table_ref := out_sym_table;
  bsym_table_ref := out_bsym_table;

  (* already processed include files *)
  excls := includes @ !excls

(* -------------------------------------------------------------------------- *)

let make_sym_table () =
  let sym_table = Flx_sym_table.create () in

  (* We need the root symbol that will be the parent for all the other
   * symbols. *)
  let sym = {
    Flx_sym.id="root";
    sr=Flx_srcref.dummy_sr;
    vs=Flx_ast.dfltvs;
    pubmap=Hashtbl.create 97;
    privmap=Hashtbl.create 97;
    dirs=[];
    symdef=(SYMDEF_root []) }
  in

  (* The root symbol's index is 0. *)
  Hashtbl.add sym_table 0 { Flx_sym_table.parent=None; sym=sym };

  sym_table

(* -------------------------------------------------------------------------- *)

let make_bsym_table () =
  Flx_bsym_table.create ()

(* -------------------------------------------------------------------------- *)

let process_libs state parser_state module_name start_counter =
  (* Create the symbol tables. These may be changed while we're processing each
   * library, so we'll pass around references. *)
  let sym_table_ref = ref (make_sym_table ()) in
  let bsym_table_ref = ref (make_bsym_table ()) in

  (* The libraries are just all but the first specified files. *)
  let libs = List.rev (List.tl state.syms.compiler_options.files) in

  let excls = ref [] in
  List.iter
    (process_lib
      state
      parser_state
      sym_table_ref
      bsym_table_ref
      excls
      state.syms.compiler_options.cache_dir
      module_name
      start_counter)
    libs;

  excls, !sym_table_ref, !bsym_table_ref
