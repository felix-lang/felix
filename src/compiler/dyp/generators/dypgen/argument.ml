let verbose = ref 1
let merge_warning = ref false
let lexer = ref "ocamllex"
let aut_kind = ref "`LR0"
let emit_token_type = ref true
let priority_enforcement = ref "PIA"
let process_verbose_mode () = verbose := 2
let process_merge_warning () = merge_warning := true
let process_lexer s = if s="ocamllex" || s="ulex" || s="other" then lexer := s else failwith "illegal --lexer option"

(*let process_aut_kind s = if s="LR0" || s="LALR" || s="LR1" then aut_kind := s else failwith "illegal --automaton option"*)

let process_aut_kind s =
  if s="LR0" then ()
  else if s="LALR" || s="LR1" then
    (Printf.fprintf stderr
    "Sorry, %s is not available in this version of dypgen\n" s;
    exit 2)
  else failwith "illegal --automaton option"

let process_prio_aut () = priority_enforcement:="PIA"
let process_prio_pt () = priority_enforcement:="PAR"

let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let pv_obj = ref false
let process_pv_obj () = pv_obj := true
let pv_token = ref false
let process_pv_token () = pv_token := true
let process_noemit_token_type () = emit_token_type := false

let undef_nt = ref true
let process_no_undef_nt () = undef_nt := false

let process_version () = print_endline "version 20070729"

let list_arg = [
("-v",Arg.Unit process_verbose_mode,"activates verbose mode: gives details of the parsing of the input file");
("--merge-warning",Arg.Unit process_merge_warning,"activates merge warning: the generated parser will emit a warning on the standard output each time a merge happens");
("--lexer",Arg.String process_lexer,"by default the lexer is ocamllex, use --lexer ulex to specify ulex as the lexer and --lexer other for another lexer, this has an effect on the interface of the parser.");

(*("--automaton",Arg.String process_aut_kind,"by default the automaton is LR(0), use --automaton LALR or --automaton LR1 if you prefer one of them");*)

("--automaton",Arg.String process_aut_kind,"");

(*("--prio-aut",Arg.Unit process_prio_aut,"use --prio-aut to make the priority enforcement embedded into the automaton, by default they are enforced while parsing.");*)

("--prio-pt",Arg.Unit process_prio_pt,"use --prio-pt to make the priorities  be enforced at parsing time, by default their enforcement is embedded into the automaton.");

("--pv-obj",Arg.Unit process_pv_obj,"the type constructor obj is made as a sum of polymorphic variants instead of a sum of constructors. This is useful when the maximum number of constructors allowed is reached.");
("--pv-token",Arg.Unit process_pv_token,"the type token is made as a sum of polymorphic variants instead of a sum of constructors. This is useful when the maximum number of constructors allowed is reached.");
("--noemit-token-type",Arg.Unit process_noemit_token_type,"the type token is not emitted in the mli or ml files, it must be provided by the user instead.");

("--no-undef-nt",Arg.Unit process_no_undef_nt,"prevents the exception Undefined_nt from being raised.");

("--version",Arg.Unit process_version,"gives the version of dypgen and exit.")
]

(*let _ = Arg.parse list_arg process_argument "usage: dypgen [-v] [--noemit-token-type] [--merge-warning] [--lexer (ocamllex|ulex|other)] [--automaton (LR0|LALR|LR1)] [--prio-aut] [--pv-obj] file_name.dyp"*)

let _ = Arg.parse list_arg process_argument "usage: dypgen [-v] [--merge-warning] [--lexer (ocamllex|ulex|other)] [--prio-pt] [--pv-obj] [--no-bad-cons] [--no-undef-nt] file_name.dyp"

(*let _ = if !string_ref = "" then
  let _ = print_string "usage: dypgen [-v] [--merge-warning] [--lexer (ocamllex|ulex|other)] [--automaton (LR0|LALR|LR1)] [--prio-aut] [--pv-obj] file_name.dyp\n" in exit 0*)

let _ = if !string_ref = "" then
  let _ = print_string "usage: dypgen [-v] [--merge-warning] [--lexer (ocamllex|ulex|other)] [--prio-pt] [--pv-obj] [--no-bad-cons] [--no-undef-nt] file_name.dyp\n" in exit 0

