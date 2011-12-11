let verbose_ref = ref 1
let process_verbose_mode () = verbose_ref := 2
let string_ref = ref ""
let syntax_extension = ref None
let process_syntax file_name = syntax_extension := Some file_name
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let list_arg = [("-v",Arg.Unit process_verbose_mode,"activate verbose mode: gives details of the parsing of the input file");("-s",Arg.String process_syntax,"\"syntax_file\" loads a syntax extension before parsing")]
let _ = Arg.parse list_arg process_argument "usage: tinyML [-v] file_name.tiny"
let _ = if !string_ref = "" then
  let _ = print_string "usage: tinyML [-v] [-s \"syntax_file\"] file_name.tiny\n" in exit 0

let verbose = !verbose_ref
