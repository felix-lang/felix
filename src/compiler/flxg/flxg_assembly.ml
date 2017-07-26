open Flx_ast
open Flx_mtypes2
open Flx_exceptions
open Flx_options
open Flx_types
open Flxg_state
open Flx_bid

module CS = Flx_code_spec


(* filename: include string, with leading . replaced
 * depname: the complete pathname
*)
type ub_entry_t = {
  filename: string;  (** include string, with leading '.' replaced' *)
  depname: string;   (** the complete pathname. *)
  stmts: statement_t list;
  asms: asm_t list
}

type t = ub_entry_t list

(* make_assembly parses and desugars the transitive closure of the specified
   input file with respect to includes, and returns a list of pairs mapping
   files to assembly lists.

   The name specified is only used by the macroprocessor to create unique
   names. (?)
*)

type include_entry_t = 
      Search of (Flx_srcref.t * string) 
    | NoSearch of (Flx_srcref.t * string)

(* if an including_file_base "a/b" in including_file_dir "c/d" includes
   with string
   "f" then we use include entry Search "f" (search the path)
   "./f" then we use NoSearch "c/d/a/f" (sibling of including file)
*)

let make_include_entry including_file_dir including_file_base (sr, include_string) =
  let n = String.length include_string in
  if n > 1 then
    if String.sub include_string 0 2 = "./"
    then begin
      let dirpart = Filename.dirname including_file_base in
      let dirpart = if dirpart = "." then "" else dirpart in
      let dir = Flx_filesys.join including_file_dir dirpart  in
      let basepart = String.sub include_string 2 (n-2) in
      let fullname = Flx_filesys.join dir basepart in
      NoSearch (sr, fullname)
    end else Search (sr, include_string)
  else Search (sr, include_string)


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

(*
let rec cal_file_time include_dirs time name =
  let ch = 
    try open_in_bin name 
    with _ ->  print_endline ("Can't open file '" ^ name ^ "'"); assert false
  in
  let parent_dir = Filename.dirname name in 
  try
    while true do 
      let line = input_line ch in
      let include_file = Flx_parse_driver.get_hash_include (parent_dir :: include_dirs) line in
      if include_file <> "" then begin
print_endline ("  ..  timestamp #include file '" ^ include_file ^ "'");
        (* the default 0.0 is a don't care because the file has to exist or we crash *)
        time := max (Flx_filesys.virtual_filetime 0.0 include_file) (!time);
        cal_file_time include_dirs time include_file;
      end
    done
  with End_of_file ->
    close_in ch
*)

let assemble state parser_state exclusions module_name input =
  let fresh_bid () = fresh_bid state.syms.counter in
  let outputs = ref [] in

  (* PARSE THE IMPLEMENTATION FILES *)
  let processed = ref exclusions in
  let unprocessed = ref [input] in

  while !unprocessed != [] do
    (* Pop a candidate. *)
    let candidate = List.hd (!unprocessed) in
    unprocessed := List.tl (!unprocessed);

    (* Resolve the filename. *)
    let filedir,filename,source_file_extension=
      match candidate with
      | Search (sr, s) ->
        let fdoc_filedir,fdoc_filename,fdoc_filetime = 
        try
          let f = 
          Flx_filesys.find_include_dir
            ~include_dirs:state.syms.compiler_options.include_dirs
            (s ^ ".fdoc")
          in 
          let t = Flx_filesys.virtual_filetime 0.0 (Flx_filesys.join f s ^ ".fdoc") in
          f,s,t
        with Flx_filesys.Missing_path _ ->  "","",0.0
        in
        let flx_filedir,flx_filename,flx_filetime = 
        try
          let f = 
          Flx_filesys.find_include_dir
            ~include_dirs:state.syms.compiler_options.include_dirs
            (s ^ ".flx")
          in 
          let t = Flx_filesys.virtual_filetime 0.0 (Flx_filesys.join f s ^ ".flx") in
          f,s,t
        with Flx_filesys.Missing_path _ ->  "","",0.0
        in
        if fdoc_filetime = 0.0 && flx_filetime = 0.0 then 
          clierr sr ("Specified include path doesn't exist: " ^ s ^ ".(flx|fdoc)")
        ;
        if fdoc_filetime > flx_filetime 
        then fdoc_filedir, fdoc_filename,".fdoc"
        else flx_filedir, flx_filename,".flx"
      | NoSearch (sr, s) -> "",s,""
    in
(*
print_endline ("DEBUG: Flxg_assembly.assemble dir=" ^ filedir ^ ", file=" ^ filename ^ ", extn=" ^ source_file_extension);
*)
    let flx_base_name = Flx_filesys.join filedir filename in

    (* Check if already processed. *)
    if not (List.mem filename !processed) then begin
      (* flag already processed *)
      processed := filename :: !processed;

      (* Get the parse of the felix file, with caching. *)
      let stmts =
        (* check the felix file modification time *)
        let flx_name = flx_base_name ^ source_file_extension in
        let flx_time = ref (Flx_filesys.virtual_filetime
          Flx_filesys.big_crunch
          flx_name)
        in
        (* 
        cal_file_time state.syms.compiler_options.include_dirs flx_time flx_name;
        *)
        let par_name = Flx_filesys.join filedir filename ^ ".par" in
        let par_name = Flx_filesys.mkabs par_name in
        let par_name =
           Flx_filesys.mk_cache_name state.syms.compiler_options.cache_dir par_name
        in
(*
print_endline ("Parsing or loading file " ^ par_name);
*)
        let stmts = Flx_filesys.cached_computation "parse" par_name ~outfile:None
          ~min_time:(!flx_time)
          (fun () -> Flx_profile.call
            "Flxg_parse.parse_file"
            (Flxg_parse.parse_file state parser_state)
            flx_name)
        in
        stmts
      in

(*
print_endline ("Desugaring file" ^ flx_base_name ^ " Counter= " ^ string_of_int (!(state.syms.counter)));
*)
      (* Desugar the parse tree, and also return list of include strings. *)
      let include_files, asms =
        let desugar_state = Flx_desugar_expr.make_desugar_state
          module_name
          state.syms.counter
        in
        let include_files, asms = Flx_desugar.desugar_stmts
          desugar_state
          (Filename.dirname filename)
          stmts
        in
(*
print_endline ("  AFTER Desugaring file" ^ flx_base_name ^ " Counter= " ^ string_of_int (!(state.syms.counter)));
*)
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
      outputs := {
        filename=filename;
        depname=flx_base_name;
        stmts=stmts;
        asms=asms } :: !outputs;
    end
  done;

  (* But again, the order is reversed here. *)
  let assembly = !outputs in

  (* Split the assembly into the includes, dependencies, and asms. *)
  let includes, depnames, stmtss, asmss =
    let rec aux includes depnames stmtss asmss a =
      match a with
      | [] -> includes, depnames, stmtss, asmss
      | { filename=filename; depname=depname; stmts=stmts; asms=asms; } :: t ->
          aux
            (filename :: includes)
            (depname :: depnames)
            (stmts :: stmtss)
            (asms :: asmss)
            t
     in
     aux [] [] [] [] assembly
  in

  includes, depnames, stmtss, asmss

