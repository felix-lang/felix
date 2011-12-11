{
open Position_parser
open Lexing

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
                 | None -> pos.pos_fname
                 | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

}
rule token = parse
    [' ' '\t']   { token lexbuf }
  | '\n'  { update_loc lexbuf None 1 false 0; token lexbuf }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'               { PLUS }
  | '*'               { TIMES }
  | eof | 'q' | 'e'   { EOF }
