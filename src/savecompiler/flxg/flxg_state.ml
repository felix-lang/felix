open Flx_options
open Flx_mtypes2


(** The state needed for flxg compilation. *)
type t = {
  compile_start_gm_string: string;
  compile_start_local_string: string;
  syms: Flx_mtypes2.sym_state_t;
  module_name: string;
  input_filename: string;
  header_file: Flxg_file.t;
  body_file: Flxg_file.t;
  ctors_file: Flxg_file.t;
  package_file: Flxg_file.t;
  rtti_file: Flxg_file.t;
  felix_interface_file: Flxg_file.t;
  report_file: Flxg_file.t;
  why_file: Flxg_file.t;
  dep_file: Flxg_file.t;
  static_link_thunk_file: Flxg_file.t;
}


let make_module_name inbase =
  let n = String.length inbase in
  let i = ref (n-1) in
  while !i <> -1 && inbase.[!i] <> '/' && inbase.[!i] <> '\\' do decr i done;
  let x = String.sub inbase (!i+1) (n - !i - 1) in
  try Filename.chop_extension x 
  with Invalid_argument _ -> x


(** Make the state needed for flxg compilation. *)
let make_state compiler_options : t =
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

  let input_filename = inbase (* ^ ".flx" *) in
  (*
  and iface_file_name = filebase ^ ".fix"
  *)
  let outbase =
    (match compiler_options.bundle_dir with 
      Some bundle_dir -> Flx_filesys.join bundle_dir (Filename.basename filename) 
      | None -> Flx_filesys.mk_cache_name compiler_options.output_dir (Flx_filesys.mkabs filename)) 
  in
  (* this is technically wrong: should only chop .flx or .fdoc but Im lazy *)
  let outbase = try Filename.chop_extension outbase with Invalid_argument _ -> outbase in
  let module_name = make_module_name inbase in
(*
print_endline ("File inbase = " ^ inbase);
print_endline ("File outbase = " ^ outbase);
print_endline ("Input Filename = " ^ input_filename);
print_endline ("Module name = " ^ module_name);
*)
  {
    compile_start_gm_string = compile_start_gm_string;
    compile_start_local_string = compile_start_local_string;
    syms = make_syms compiler_options;
    module_name = module_name;
    input_filename = input_filename;
    header_file = Flxg_file.make (outbase ^ ".hpp");
    body_file = Flxg_file.make (outbase ^ ".cpp");
    ctors_file = Flxg_file.make (outbase ^ ".ctors_cpp");
    package_file = Flxg_file.make (outbase ^ ".resh");
    rtti_file = Flxg_file.make (outbase ^ ".rtti");
    felix_interface_file = Flxg_file.make (outbase ^ "_interface.flx");
    report_file = Flxg_file.make (outbase ^ ".xref");
    why_file = Flxg_file.make (outbase ^ ".why");
    dep_file = Flxg_file.make (outbase ^ ".dep");
    static_link_thunk_file = Flxg_file.make (outbase ^ "_static_link_thunk.cpp");
  }



