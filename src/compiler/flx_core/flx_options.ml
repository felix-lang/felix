type compiler_phase_t =
  | Phase_parse
  | Phase_desugar
  | Phase_bind
  | Phase_optimize
  | Phase_lower
  | Phase_codegen

type t =
{
  compiler_phase: compiler_phase_t;
  print_flag: bool;
  debug : bool;
  optimise : bool;
  doreductions : bool;
  trace : bool;
  include_dirs : string list;
  files : string list;
  raw_options: (string * string) list;
  reverse_return_parity: bool;
  max_inline_length : int;
  compile_only : bool;
  force_recompile : bool;
  with_comments : bool;
  mangle_names : bool;
  generate_axiom_checks : bool;
  auto_imports : string list;
  syntax: string list;
  automaton_filename: string;
  cache_dir : string;
  output_dir : string;
  bundle_dir : string option;
  showtime: bool;
}

