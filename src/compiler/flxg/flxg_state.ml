open Format


(** The state needed for flxg compilation. *)
type t = {
  ppf: formatter;
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
  report_file: Flxg_file.t;
  why_file: Flxg_file.t;
  dep_file_name: string;
  mutable parse_time: float;
  mutable desugar_time: float;
  mutable bind_time: float;
  mutable opt_time: float;
  mutable lower_time: float;
  mutable instantiation_time: float;
  mutable codegen_time: float;
}
