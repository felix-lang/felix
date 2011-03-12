(* code generation driver *)

open Format

open Flx_getopt
open Flx_mtypes2
open Flx_types
open Flx_version
open Flxg_state

module CS = Flx_code_spec
;;

(* We have to set the felix version first. *)
Flx_version_hook.set_version ()


(* Parse the felix arguments and do some option parsing while we're at it. *)
let parse_args () =
  (* Argument parsing *)
  let argc = Array.length Sys.argv in

  (* Error out if we don't have enough arguments. *)
  if argc <= 1 then begin
    print_endline "usage: flxg --key=value ... filename; -h for help";
    exit 1
  end;

  (* Now, parse those arguments *)
  let raw_options = parse_options Sys.argv in

  (* Print help and version out. *)
  if check_keys raw_options ["h"; "help"] then begin
    Flxg_options.print_options ();
    exit 0
  end;

  if check_key raw_options "version" then begin
    Printf.printf "Felix version %s\n" !version_data.version_string;
    exit 0
  end;

  (* Now extract the driver options. *)
  let compiler_options = Flxg_options.get_options raw_options in

  (* Error out if we didn't specify any files. *)
  if compiler_options.files = [] then begin
    Flxg_options.print_options ();
    exit 1
  end;

  (* Create a formatter for logging if debugging's enabled. Otherwise, create a
   * null formatter. *)
  let ppf =
    if compiler_options.print_flag
    then err_formatter
    else make_formatter (fun _ _ _ -> ()) (fun () -> ())
  in

  fprintf ppf "// Include directories = %s\n"
    (String.concat " " compiler_options.include_dirs);

  (* Make sure the current directory is in the search path. *)
  let include_dirs =
    Filename.current_dir_name :: compiler_options.include_dirs
  in
  let compiler_options = { compiler_options with
    include_dirs = include_dirs }
  in

  ppf, compiler_options


let make_module_name inbase =
  let n = String.length inbase in
  let i = ref (n-1) in
  while !i <> -1 && inbase.[!i] <> '/' && inbase.[!i] <> '\\' do decr i done;
  String.sub inbase (!i+1) (n - !i - 1)


(** Make the state needed for flxg compilation. *)
let make_flxg_state ppf compiler_options =
  let format_time tm =
    string_of_int (tm.Unix.tm_year + 1900) ^ "/" ^
    string_of_int (tm.Unix.tm_mon + 1) ^ "/" ^
    string_of_int tm.Unix.tm_mday ^ " " ^
    string_of_int tm.Unix.tm_hour ^ ":" ^
    string_of_int tm.Unix.tm_min ^ ":" ^
    string_of_int tm.Unix.tm_sec
  in

  (* Time initialisation *)
  let compile_start = Unix.time () in
  let compile_start_gm = Unix.gmtime compile_start in
  let compile_start_local = Unix.localtime compile_start in
  let compile_start_gm_string = format_time compile_start_gm ^ " UTC" in
  let compile_start_local_string = format_time compile_start_local ^ " (local)" in

  let filename = List.hd compiler_options.files in
  let inbase = filename in

  let input_filename = inbase ^ ".flx" in
  (*
  and iface_file_name = filebase ^ ".fix"
  *)
  let outbase =
    match compiler_options.output_dir with
    | None -> filename
    | Some d -> Filename.concat d (Filename.basename filename)
  in
  {
    ppf = ppf;
    compile_start_gm_string = compile_start_gm_string;
    compile_start_local_string = compile_start_local_string;
    syms = make_syms compiler_options;
    module_name = make_module_name inbase;
    input_filename = input_filename;
    header_file = Flxg_file.make (outbase ^ ".hpp");
    body_file = Flxg_file.make (outbase ^ ".cpp");
    ctors_file = Flxg_file.make (outbase ^ ".ctors_cpp");
    package_file = Flxg_file.make (outbase ^ ".resh");
    rtti_file = Flxg_file.make (outbase ^ ".rtti");
    report_file = Flxg_file.make (outbase ^ ".xref");
    why_file = Flxg_file.make (outbase ^ ".why");
    dep_file_name = outbase ^ ".dep";
    parse_time = 0.0;
    desugar_time = 0.0;
    bind_time = 0.0;
    opt_time = 0.0;
    lower_time = 0.0;
    instantiation_time = 0.0;
    codegen_time = 0.0;
  }


let generate_dep_file state =
(* NOTE: as is this can't work, because it lists the *.flx filename without the
 * flx, but it doesn't say where the *.par files are.. we need to list both,
 * since the *.par files might be in a --cache_dir directory.
 *
 * Still there's another way to use the information here:
 * We just check the time stamps relative to the main program *.par file
 * and/or generated program, whatever flx does now with the main program
 * filename. I.e. we just take the time stamp of the main program as the
 * largest of all the time stamps. If a file is deleted its stamp is 0,
 * which will only cause a problem if there's a dangling reference,
 * otherwise the including file had to be changed to stop this, and its
 * time stamp will be bigger.
 *)
  let chan = open_out state.dep_file_name in
  output_string chan (String.concat "\n" (!(state.syms.include_files)) ^ "\n");
  close_out chan



(** Save basic profiling numbers. *)
let save_profile state =
  let total_time =
    state.parse_time +.
    state.desugar_time +.
    state.bind_time +.
    state.opt_time +.
    state.instantiation_time +.
    state.codegen_time
  in
  fprintf state.ppf "//Felix compiler time %f\n" total_time;
  let fname = "flxg_stats.txt" in
  let
    old_parse_time,
    old_desugar_time,
    old_bind_time,
    old_opt_time,
    old_instantiation_time,
    old_codegen_time,
    old_total_time
  =
    let zeroes = 0.0,0.0,0.0,0.0,0.0,0.0,0.0 in
    let f = try Some (open_in fname) with _ -> None in
    begin match f with
    | None -> zeroes
    | Some f ->
      let x =
        try
          let id x1 x2 x3 x4 x5 x6 x7 = x1, x2, x3, x4, x5, x6, x7 in
          Scanf.fscanf f
          "parse=%f desugar=%f bind=%f opt=%f inst=%f gen=%f tot=%f"
          id
        with
        | Scanf.Scan_failure _
        | Failure _
        | End_of_file -> zeroes
      in close_in f; x
    end
  in

  (* *)
  (* failure to save stats isn't fatal *)
  try
    let f = open_out fname in
    Printf.fprintf
      f
      "parse=%f\ndesugar=%f\nbind=%f\nopt=%f\ninst=%f\ngen=%f\ntot=%f\n"
      (old_parse_time +. state.parse_time)
      (old_desugar_time +. state.desugar_time)
      (old_bind_time +. state.bind_time)
      (old_opt_time +. state.opt_time)
      (old_instantiation_time +. state.instantiation_time)
      (old_codegen_time +. state.codegen_time)
      (old_total_time +. total_time)
    ;
    close_out f
  with _ -> ()

(* Desugar takes a parse tree, which is an Ocaml list of STMT_* s
   and produces a list of include file names and a list of assmebly
   instructions. The only state used is a fresh id generator.
*)

(* make_assembly parses and desugars the transitive closure of the specified
   input file with respect to includes, and returns a list of pairs mapping
   files to assembly lists.

   The name specified is only used by the macroprocessor to create unique
   names. (?)
*)

let strip_extension s =
  let n = String.length s in
  if n>4 then
    if String.sub s (n-4) 4 = ".flx"
    then String.sub s 0 (n-4)
    else s
  else s

type include_entry_t = Search of string | NoSearch of string

(* if an including_file_base "a/b" in including_file_dir "c/d" includes
   with string
   "f" then we use include entry Search "f" (search the path)
   "./f" then we use NoSearch "c/d/a/f" (sibling of including file)
*)

let make_include_entry including_file_dir including_file_base  include_string =
  let n = String.length include_string in
  if n > 1 then
    if String.sub include_string 0 2 = "./"
    then begin
      let dirpart = Filename.dirname including_file_base in
      let dirpart = if dirpart = "." then "" else dirpart in
      let dir = Flx_filesys.join including_file_dir dirpart  in
      let basepart = String.sub include_string 2 (n-2) in
      let fullname = Flx_filesys.join dir basepart in
      NoSearch fullname
    end else Search include_string
  else Search include_string


(* filename: include string, with leading . replaced
 * depname: the complete pathname
*)
type ub_entry_t = { filename:string;  depname:string; asms: asm_t list }


(* make_assembly does NOT modify anything at all in the symbol state
 * except for the fresh bid counter *)

(* exclusions: if an include was of the form "x/y",
   then exclusions has the string "x/y".
   if an include was from "d/a", and of the form "./x/y"
   then exclusions has the string "d/x/y"
   This form is thus relative to the current search path directory:
   the sibling reference is factored out, but the form is invariant
   over search paths. So if the Felix installation is moved this form
   is invariant: we have to put this in the libtab cache so it is mobile.
   However the *.dep file generated requires fully resolved path names.

   This means "make_include_entry" should return only the canonical form,
   and NOT the resolved filename, which means it has to be called with
   the including_file_dir also being a canonical string and not a resolved
   filename, however that won't work, because NoSearch doesn't search.

*)

let make_assembly
  state
  parser_state
  (exclusions:string list)
  (module_name:string)
  (input:include_entry_t)
  : ub_entry_t list
=
  let fresh_bid () = Flx_mtypes2.fresh_bid state.syms.counter in
  let outputs = ref [] in

  (* PARSE THE IMPLEMENTATION FILES *)
  let processed = ref exclusions in
  let unprocessed = ref [input] in

  while !unprocessed != [] do
    (* Pop a candidate. *)
    let candidate = List.hd (!unprocessed) in
    unprocessed := List.tl (!unprocessed);

    (* Resolve the filename. *)
    let filedir,filename =
      match candidate with
      | Search s ->
          Flx_filesys.find_include_dir
            ~include_dirs:state.syms.compiler_options.include_dirs
            (s ^ ".flx"),s
      | NoSearch s -> "",s
    in
    let flx_base_name = Flx_filesys.join filedir filename in

    (* Check if already processed. *)
    if not (List.mem filename !processed) then begin
      (* flag already processed *)
      processed := filename :: !processed;

      (* Get the parse of the felix file, with caching. *)
      let stmts =
        (* check the felix file modification time *)
        let flx_name = flx_base_name ^ ".flx" in
        let flx_time = Flx_filesys.virtual_filetime
          Flx_filesys.big_crunch
          flx_name
        in

        let in_par_name = Flx_filesys.join filedir filename ^ ".par2" in
        let out_par_name =
           match state.syms.compiler_options.cache_dir with
           | Some d -> Some (Flx_filesys.join d filename ^ ".par2")
           | None -> None
        in
        let stmts = Flx_filesys.cached_computation "parse" in_par_name
          ~outfile:out_par_name
          ~min_time:flx_time
          (fun () -> Flxg_parse.parse_file state parser_state flx_name)
        in
        stmts
      in

      (* Desugare the parse tree, and also return list of include strings *)
      let include_files, asms =
        let desugar_state = Flx_desugar.make_desugar_state
          module_name
          fresh_bid
        in
        let include_files, asms = Flx_desugar.desugar_stmts
          desugar_state
          (Filename.dirname filename)
          stmts
        in

        let top_req =
          let sr = Flx_srcref.dummy_sr in
          let body = Flx_types.DCL_insert (
            CS.Str "",
            `Body,
            Flx_ast.NREQ_true)
          in
          Flx_types.Dcl (
            sr,
            "_rqs_" ^ module_name,
            None,
            `Public,
            Flx_ast.dfltvs,
            body)
        in
        let asms = top_req::asms in
        include_files, asms
      in

      (* Run through include strings found. Desugar outputs the include files
       * backwards, but we push them onto the unprocessed stack which means the
       * order is again reversed so we actually get the proper order of
       * initialisation. *)
      List.iter begin fun s ->
        (* Convert include string to an include term. *)
        let include_entry = make_include_entry filedir filename s in

        (* Add the name to the unprocessed include list, even if already
         * processed if it is already processed that will be found when it is
         * resolved at the top of this loop. *)
        if not (List.mem include_entry (!unprocessed)) then begin
          unprocessed := include_entry :: (!unprocessed)
        end
      end include_files;

      (* add record for processed file *)
      outputs :=
        { filename=filename; depname=flx_base_name; asms=asms } :: !outputs;
    end
  done;

  (* But again, the order is reversed here. *)
  !outputs


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
  let in_libtab_name = Flx_filesys.join lib_filedir lib_filename ^ ".libtab" in
  let out_libtab_name =
    match outdir with
    | Some d -> Some (Flx_filesys.join d lib_filename ^ ".libtab")
    | None -> None
  in

  (* Look up the time the file was cached. *)
  let lib_cache_time = Flx_filesys.virtual_filetime
    Flx_filesys.big_bang
    in_libtab_name
  in

  (* Return if the file has been changed since it was cached. *)
  let validate (_,depnames,_,_,_,_,_,_,_,_,_) =
    let filetimes = List.fold_left (fun acc f ->
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
    Flx_filesys.cached_computation "libtab" in_libtab_name
      ~outfile:out_libtab_name
      ~force_calc:(false || state.syms.compiler_options.force_recompile)
      ~min_time:lib_time
      ~validate
      (fun () ->
        (* make assembly outputs stuff in reversed order, but this routine
         * reversed it back again *)
        let cal_time = Flxg_profile.make_timer () in
        print_endline ("Binding libary " ^ lib);
        let assembly = make_assembly state parser_state !excls lib (Search lib) in

        (* Split the assembly into the includes, dependencies, and asms. *)
        let includes, depnames, asmss =
          let rec aux includes depnames asmss a =
            match a with
            | [] -> includes, depnames, asmss
            | { filename=filename; depname=depname; asms=asms; } :: t ->
                aux
                  (filename :: includes)
                  (depname :: depnames)
                  (asms :: asmss)
                  t
           in
           aux [] [] [] assembly
        in

        (* already in the right order now *)
        let asms = List.concat asmss in
        let asms = Flxg_bind.make_module module_name asms in

        (* Bind the assemblies. *)
        Flxg_bind.bind_asms
          state
          !sym_table_ref
          !bsym_table_ref
          !start_counter
          asms;

        print_endline (
          "binding library " ^ lib ^ " done in "^ string_of_float (cal_time()) ^
          " seconds");

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
      )
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

(* -------------------------------------------------------------------------- *)

let main () =
  let start_counter = ref 2 in
  let ppf, compiler_options = parse_args () in
  let state = make_flxg_state ppf compiler_options in

  (* The first file specified is the main program. *)
  let main_prog = List.hd compiler_options.files in

  (* Look up the name of the main module. *)
  let module_name = make_module_name main_prog in

  begin try
    let parser_state = Flxg_parse.load_syntax state in

    let excls, sym_table, bsym_table = process_libs
      state
      parser_state
      module_name
      start_counter
    in

    (* Create the symbol table assembly for the main program. *)
    let assembly = make_assembly
      state
      parser_state
      !excls
      module_name
      (NoSearch main_prog)
    in

    let includes, depnames, asmss =
      let rec aux includes depnames asmss a =
        match a with
        | [] -> includes, depnames, asmss
        | { filename=filename; depname=depname; asms=asms; } :: t ->
            aux (filename :: includes) (depname::depnames) (asms::asmss) t
      in
      aux [] [] [] assembly
    in
    let asms = List.concat (List.rev asmss) in

    (* update the global include file list *)
    state.syms.include_files := depnames;
    generate_dep_file state;

    (* Make the toplevel module. *)
    let asms = Flxg_bind.make_module module_name asms in

    (* Bind the assemblies. *)
    Flxg_bind.bind_asms
      state
      sym_table
      bsym_table
      !start_counter
      asms;

    (* Grab the last index created so we can determine which symbols were made
     * when we bind the root module. *)
    start_counter := !(state.syms.counter);

    (* Bind the root module's init procedure. *)
    let root_proc = Flxg_bind.bind_root_module
      state
      sym_table
      bsym_table
      module_name
    in

    Flx_typeclass.typeclass_instance_check state.syms bsym_table;

    (* generate axiom checks *)
    (* or not: the routine must be run to strip axiom checks out of the code *)
    Flx_axiom.axiom_check state.syms bsym_table
      state.syms.compiler_options.generate_axiom_checks;

    (* Not working at the moment for unknown reason, chucks Not_found.
    (* Generate the why file. *)
    generate_why_file state bsym_table root_proc;
    *)

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

    (* Optimize the bound values. *)
    let bsym_table = Flxg_opt.optimize state bsym_table root_proc in

    (* Downgrade abstract types now. *)
    Flx_strabs.strabs (Flx_strabs.make_strabs_state ()) bsym_table;

    (* Lower the bound symbols for the backend. *)
    let bsym_table = Flxg_lower.lower state bsym_table root_proc in

    (* Start working on the backend. *)
    Flxg_codegen.codegen state bsym_table root_proc

  with x ->
    Flxg_terminate.terminate compiler_options.reverse_return_parity x
  end;

  (* We're done! let's calculate some simple profile statistics. *)
  save_profile state;

  if compiler_options.reverse_return_parity then 1 else 0
;;

exit (main ())
