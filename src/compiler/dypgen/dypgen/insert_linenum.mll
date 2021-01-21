{
open Lexing

let buffer = ref ""

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

let newline = ('\010' | '\013' | "\013\010")

rule insert_linenum = parse
  | newline
      { update_loc lexbuf None 1 false 0;
      insert_linenum lexbuf }
  | eof { let result = !buffer in buffer := ""; result }
  | [^'#''\010''\013']+ { insert_linenum lexbuf }
  | _ { insert_linenum lexbuf }

