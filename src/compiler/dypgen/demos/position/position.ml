open Parse_tree
open Lexing

let input_file = "input"

let lexbuf = Dyp.from_channel
  (Position_parser.pp ()) (Pervasives.open_in input_file)

let std_lb = Dyp.std_lexbuf lexbuf
let _ = std_lb.lex_curr_p <-
  { std_lb.lex_curr_p with pos_fname = input_file }
let pf = Position_parser.main lexbuf

let _ = print_forest (List.map (fun (x,_) -> x) pf)
