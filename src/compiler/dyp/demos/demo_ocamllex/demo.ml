(* demo.ml *)

open Parse_tree

let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let _ = Arg.parse [] process_argument "usage: demo file_name"
let _ = if !string_ref = "" then
  (print_string "usage: demo file_name\n";
  exit 0)

let input_file = !string_ref
let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let prog = fst (List.hd (Parser.main Lexer.token lexbuf))

let s = str_expr prog
let () = Printf.printf "= %s\n" s