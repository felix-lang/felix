open Flx_mtypes2
open Flx_options
open Flx_types
open Flxg_state

(* -------------------------------------------------------------------------- *)
let string_of_bool b = if b then "true" else "false"

(* -------------------------------------------------------------------------- *)
let process_lib
  state
  parser_state
  sym_table_ref
  bsym_table_ref
  deps
  excls
  cache_dir
  module_name
  start_counter
  lib
=
  (* Get native lib file paths. *)
  let lib_filedir, lib_filename =
    Flx_filesys.find_include_dir
      ~include_dirs:state.syms.compiler_options.include_dirs
      (lib ^ ".flx"),lib
  in

  (* Join lib path with lib filename. *)
  (* TOP LEVEL LIBS HAVE TO BE *.flx -- should allow *.fdoc too *)
  let lib_name = Flx_filesys.join lib_filedir (lib_filename ^ ".flx") in

  (* This is wrong. It should be the max of the included file times, but until
   * we load the cached value we don't know what files are included.
   * To fix this we could use a separate dep file, or simply validate
   * the file after it is loaded. Of course to do that I have to put
   * the include file list into the cache! *)
  let lib_time = Flx_filesys.virtual_filetime Flx_filesys.big_crunch lib_name in
  let libcache_name = Flx_filesys.join lib_filedir lib_filename in
  let libtab_name = (Flx_filesys.mk_cache_name cache_dir libcache_name) in
  let libtab_name = libtab_name ^ ".libtab" in

  (* print_endline ("Libtab name = " ^ libtab_name); *)

  (* Look up the time the file was cached. *)
  let lib_cache_time = Flx_filesys.virtual_filetime
    Flx_filesys.big_bang
    libtab_name
  in

  (* If both files are deleted, then it's new. Otherwise just return the
     time of the most recent file. *)
  let filtim f = 
    let flxt = Flx_filesys.virtual_filetime Flx_filesys.big_bang (f^".flx") in
    let flxd = Flx_filesys.virtual_filetime Flx_filesys.big_bang (f^".fdoc") in

    let result = 
      if flxt = Flx_filesys.big_bang && 
         flxd = Flx_filesys.big_bang 
      then 
        begin
          print_endline ("Cannot find either " ^ f^".flx" ^ " or " ^ f ^".fdoc: rebuilding library");
          Flx_filesys.big_crunch
        end
      else 
        max flxt flxd

    in 
    (* print_endline ("File " ^ f ^ " time= " ^ string_of_float result); *)
    result

  in

  (* Determine whether the file has been changed since it was cached. *)
  let validate (_,depnames,_,_,_,_,_,_,_,_,_) =

    (* Of all the lib files, get the newest file time. *)
    let newest_file_time = 
      List.fold_left 
        (fun acc f ->
          (* print_endline ("dep file = " ^ f); *)
          max acc (filtim f))
        Flx_filesys.big_bang 
        depnames
    in
(*
    print_endline ("Dependent newest_file_time = " ^ string_of_float newest_file_time);
    print_endline ("cached library time = " ^ string_of_float lib_cache_time);
    print_endline (if newest_file_time < lib_cache_time then "Files unchanged using cache" else "Files changed, rebuilding");
*)
    newest_file_time < lib_cache_time
  in

  let
    includes,
    depnames,
    saved_counter,
    varmap,
    ticache,
    virtual_to_instances,
    instances_of_typeclass,
    axioms,
    reductions,
    out_sym_table,
    out_bsym_table
  =
(*
    print_endline ("Potentially cached computation: building library " ^ libtab_name);
    print_endline ("Force flag = " ^ string_of_bool state.syms.compiler_options.force_recompile);
*)
    Flx_filesys.cached_computation "libtab" libtab_name
      ~outfile:None
      ~force_calc:(false || state.syms.compiler_options.force_recompile)
      ~min_time:lib_time
      ~validate

      (* The following will happen when the libs are not pre-built (controlled by `validate`) *)
      (Flx_profile.call ("binding library " ^ lib) begin fun () ->

        (* make assembly outputs stuff in reversed order, but this routine
         * reversed it back again *)
        print_endline ("Binding library " ^ lib);

        let includes, depnames, _, asmss = Flxg_assembly.assemble
          state
          parser_state
          !excls
          lib
          (Flxg_assembly.Search (Flx_srcref.dummy_sr, lib))
        in

        (* already in the right order now *)
        let asms = List.concat asmss in
        let asms = Flxg_bind.make_module module_name asms in

        (* Bind the assemblies. *)
        Flx_profile.call "Flxg_bind.bind_asms"
          (Flxg_bind.flxg_bind_asms
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
        state.syms.virtual_to_instances,
        state.syms.instances_of_typeclass,
        !(state.syms.axioms),
        !(state.syms.reductions),
        !sym_table_ref,
        !bsym_table_ref
      end)
  in

(*
  print_endline ("LIBRARY LOADED or PARSED " ^ lib);
  print_endline ("Saved counter " ^ string_of_int saved_counter);
  print_endline ("Current counter " ^ string_of_int (!(state.syms.counter)));
*)
  state.syms.counter := max !(state.syms.counter) saved_counter;
(*
  print_endline ("Set counter " ^ string_of_int (!(state.syms.counter)));
*)
  state.syms.varmap <- varmap;
  state.syms.ticache <- ticache;
  state.syms.virtual_to_instances <- virtual_to_instances;
  state.syms.instances_of_typeclass <- instances_of_typeclass;
  state.syms.axioms := axioms;
  state.syms.reductions := reductions;
  start_counter := !(state.syms.counter);
  sym_table_ref := out_sym_table;
  bsym_table_ref := out_bsym_table;
  deps := depnames @ !deps;

  (* already processed include files *)
  excls := includes @ !excls

(* -------------------------------------------------------------------------- *)
(* Create a storage object for holding symbols during compilation phases. *)
let make_sym_table () =
  let sym_table = Flx_sym_table.create () in

  (* We need the root symbol that will be the parent for all the other
   * symbols. *)
  let sym = {
    Flx_sym.id="root";
    sr=Flx_srcref.dummy_sr;
    vs=Flx_types.dfltivs;
    pubmap=Hashtbl.create 97;
    privmap=Hashtbl.create 97;
    dirs=[];
    symdef=(SYMDEF_root None) }
  in

  (* The root symbol's index is 0. *)
  Hashtbl.add sym_table 0 { Flx_sym_table.parent=None; sym=sym };

  sym_table

(* -------------------------------------------------------------------------- *)
(* Create a storage object for symbol bindings during compilation phases. *)
let make_bsym_table () =
  Flx_bsym_table.create_fresh ()

(* -------------------------------------------------------------------------- *)
(* Build libs and configure state for program. *)
let process_libs state parser_state module_name start_counter =

  (* Create the symbol tables. These may be changed while we're processing each
   * library, so we'll pass around references. *)
  let sym_table_ref = ref (make_sym_table ()) in
  let bsym_table_ref = ref (make_bsym_table ()) in

  (* The libraries are just all but the first specified files. *)
  let libs = List.rev (List.tl state.syms.compiler_options.files) in

  let excls = ref [] in
  let deps = ref [] in

  (* Iterate over libs and call process_lib *)
  List.iter
    (process_lib
      state
      parser_state
      sym_table_ref
      bsym_table_ref
      deps
      excls
      state.syms.compiler_options.cache_dir
      module_name
      start_counter)
    libs;

  deps, excls, !sym_table_ref, !bsym_table_ref


