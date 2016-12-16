let dypgen_version = "20120619"

let verbose = ref 1
let merge_warning = ref false

let emit_token_type = ref true
let emit_pp = ref true
let emit_obj_type = ref true
let werror = ref false

let process_verbose_mode () = verbose := 2

let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let pv_obj = ref false
let pv_token = ref false
let undef_nt = ref true

let process_version () = print_endline ("version "^dypgen_version)

let ocamlc_options = ref ""
let process_ocamlc opt = ocamlc_options := opt

let command = ref ""
let process_command cmd = command := cmd

let no_mli = ref false
let use_rule_order = ref false
let use_all_actions = ref false
let use_cpp = ref false
let cpp_options = ref ""
let process_cpp_options opt =
  use_cpp := true;
  if opt <> "" then cpp_options := opt^" "

let list_arg = [
("-v",Arg.Unit process_verbose_mode,"activates verbose mode: gives details of the parsing of the input file");

("--merge-warning",Arg.Set merge_warning,"activates merge warning: the generated parser will emit a warning on the standard output each time a merge happens");

("--Werror",Arg.Set werror,"warnings become errors, does not apply for merge warnings");

("--pv-obj",Arg.Set pv_obj,"the type constructor obj is made as a sum of polymorphic variants instead of a sum of constructors. This is useful when the maximum number of constructors allowed is reached.");

("--pv-token",Arg.Set pv_token,"the type token is made as a sum of polymorphic variants instead of a sum of constructors. This is useful when the maximum number of constructors allowed is reached.");

("--noemit-token-type",Arg.Clear emit_token_type,"the type token is not emitted in the mli or ml files, it must be provided by the user instead.");

("--no-pp",Arg.Clear emit_pp,"the value pp is not stated in the mli file.");

("--no-obj-type",Arg.Clear emit_obj_type,"the type obj_type is not stated in the mli file.");

("--no-undef-nt",Arg.Clear undef_nt,"prevents the exception Undefined_nt from being raised.");

("--ocamlc",Arg.String process_ocamlc,"\"options\" dypgen uses these options when calling ocamlc -i.");

("--command",Arg.String process_command,"\"command\" dypgen uses this command instead of calling ocamlc -i.");

("--no-mli",Arg.Set no_mli,"dypgen does not generate the .mli file.");

("--use-rule-order",Arg.Set use_rule_order,"dypgen uses rule order to decide which rule to reduce in case of reduce/reduce conflict, it is overriden by the variable dypgen_use_rule_order.");

("--use-all-actions",Arg.Set use_all_actions,"the parser will execute all the user actions bound to a rule instead of just the first that does not raise Giveup.");

("--cpp",Arg.Set use_cpp,"dypgen calls the C preprocessor cpp on the input file before processing it.");

("--cpp-options",Arg.String process_cpp_options,"\"options\" dypgen calls the C preprocessor cpp with options \"options\" on the input file before processing it.");

("--version",Arg.Unit process_version,"gives the version of dypgen and exits.")
]

let _ = Arg.parse list_arg process_argument "usage: dypgen [-v] [--merge-warning] [--Werror] [--pv-obj] [--pv-token] [--noemit-token-type] [--no-pp] [--no-obj-type] [--no-undef-nt] [--ocamlc \"options\" | --command \"command\"] [--no-mli] [--use-rule-order] [--use-all-actions] [--cpp | --cpp-options \"options\"] file_name.dyp\n"

let _ = if !string_ref = "" then
  let _ = print_string "usage: dypgen [-v] [--merge-warning] [--Werror] [--pv-obj] [--pv-token] [--noemit-token-type] [--no-pp] [--no-obj-type] [--no-undef-nt] [--ocamlc \"options\" | --command \"command\"] [--no-mli] [--use-rule-order] [--use-all-actions] [--cpp | --cpp-options \"options\"] file_name.dyp\n" in exit 0
