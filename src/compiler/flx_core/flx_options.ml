type t =
{
  print_flag: bool;
  debug : bool;
  optimise : bool;
  trace : bool;
  include_dirs : string list;
  files : string list;
  raw_options: (string * string) list;
  reverse_return_parity: bool;
  max_inline_length : int;
  compile_only : bool;
  force_recompile : bool;
  with_comments : bool;
  document_grammar: bool;
  document_typeclass: bool;
  mangle_names : bool;
  generate_axiom_checks : bool;
  auto_imports : string list;
  syntax: string list;
  cache_dir : string option;
  output_dir : string option;
}
