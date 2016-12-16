open Position_parser
open Position_lexer
open Parse_tree
open Lexing

let input_file = "input"

let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let _ = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file }
let pf = Position_parser.main Position_lexer.token lexbuf

let _ = print_forest (List.map (fun (x,_) -> x) pf)
