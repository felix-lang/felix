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
  | "# insert-line-number"
      { let pos = Lexing.lexeme_start_p lexbuf in
      let space = String.make 20 ' ' in
      let str = "# "^(string_of_int (pos.pos_lnum+1)) in
      String.blit str 0 space 0 (String.length str);
      String.blit space 0 !buffer pos.pos_cnum 20;
      insert_linenum lexbuf }
  | eof { let result = !buffer in buffer := ""; result }
  | [^'#''\010''\013']+ { insert_linenum lexbuf }
  | _ { insert_linenum lexbuf }

and replace_filename parser_code fn = parse
  | "# " ['0'-'9']+ " " { rf2 parser_code fn lexbuf }
  | [^'\010''\013''#']+ newline
  | '#' | newline { replace_filename parser_code fn lexbuf }
  | eof { () }

and rf2 parser_code fn = parse
  | [^'\010''\013']+ newline
      { let len = String.length fn in
      if String.sub (Lexing.lexeme lexbuf) 0 len = fn then
        let fn2 = fn^".ml     " in
        let pos = Lexing.lexeme_start_p lexbuf in
        String.blit fn2 0 parser_code pos.pos_cnum (len+8);
      replace_filename parser_code fn lexbuf }
  | eof { () }
  | _  { replace_filename parser_code fn lexbuf }

