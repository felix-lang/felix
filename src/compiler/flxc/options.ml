let include_dirs = ref []
let files = ref []
let cache_dir = ref None
let output_dir = ref None
let imports = ref []

let parse_args () =
  let usage = "Usage: flxc <options> <files>\nOptions are:" in
  let options = [
    ("-I", Arg.String (fun i -> include_dirs := i :: !include_dirs),
      "Add <dir> to the list of include directories");
    ("--cache-dir", Arg.String (fun d -> cache_dir := Some d),
      "Add <dir> to the list of include directories");
    ("--output-dir", Arg.String (fun d -> output_dir := Some d),
      "Add <dir> to the list of include directories");
    ("--import", Arg.String (fun i -> imports := i :: !imports),
      "Add <dir> to the list of include directories")
  ] in
  let anonymous file = files := file :: !files in
  Arg.parse options anonymous usage

let make_felix_compiler_options () =
  {
    Flx_mtypes2.print_flag = false;
    Flx_mtypes2.debug = false;
    Flx_mtypes2.optimise = false;
    Flx_mtypes2.trace = false;
    Flx_mtypes2.include_dirs = !include_dirs;
    Flx_mtypes2.files = !files;
    Flx_mtypes2.raw_options = [];
    Flx_mtypes2.reverse_return_parity = false;
    Flx_mtypes2.max_inline_length = 50;
    Flx_mtypes2.compile_only = false;
    Flx_mtypes2.force_recompile = false;
    Flx_mtypes2.with_comments = false;
    Flx_mtypes2.document_grammar = false;
    Flx_mtypes2.document_typeclass = false;
    Flx_mtypes2.mangle_names = false;
    Flx_mtypes2.generate_axiom_checks = false;
    Flx_mtypes2.auto_imports = !imports;
    Flx_mtypes2.cache_dir = !cache_dir;
    Flx_mtypes2.output_dir = !output_dir;
  }
