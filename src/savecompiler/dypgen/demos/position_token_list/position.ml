open Position_parser
open Position_lexer
open Parse_tree
open Lexing

let input_file = "input"

let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)
let _ = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file }
let lexfun = Position_lexer.token

let rec make_token_list token_list =
  match lexfun lexbuf with
  | EOF,p -> List.rev ((EOF,p)::token_list)
  | t -> make_token_list (t::token_list)

let token_list = make_token_list []

let pos = {
  pos_fname = "input";
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = 0 }

let lexbuf2 = {
  token_list = token_list;
  token_position = pos, pos }

let lexfun2 lb = match lb.token_list with
  | [] -> failwith "empty token list"
  | [t,pos] -> t
  | (t,pos)::q -> lb.token_list <- q;
      lb.token_position <- pos;
      t

let pf = Position_parser.main lexfun2 lexbuf2

let _ = print_forest (List.map (fun (x,_) -> x) pf)
