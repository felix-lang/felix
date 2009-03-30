{
open Dypgen_parser
open Lexing

let ($) = Buffer.add_string
let ocaml_code_buffer = Buffer.create 100000
(*let paren_count = ref 0*)
let in_string = ref false
let comment_count = ref 0
(*let dypgen_comment = ref 0*)
let look_for_type = ref false

let start_ocaml_type = ref dummy_pos
let start_ocaml_code = ref dummy_pos
let start_curlyb = ref []
let start_bracket = ref []
let start_pattern = ref dummy_pos
let start_dypgen_comment = ref []
let start_ocaml_comment = ref []
let start_string = ref dummy_pos

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
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
  | newline
      { update_loc lexbuf None 1 false 0;
        token lexbuf
      }
  | blank +
      { token lexbuf }
  | "%token" { look_for_type:=true; KWD_TOKEN }
  | "%start" { look_for_type:=true; KWD_START }
  | "%relation" { look_for_type:=false; KWD_RELATION }
  | "%mlitop" { KWD_MLITOP }
  | "%mltop" { KWD_MLTOP }
  | "%mli" { KWD_MLI }
  | "%constructor" { KWD_CONSTRUCTOR }
  | "%for" { KWD_FOR }
  | "%non_terminal" { KWD_NON_TERMINAL }
  | "%type" { KWD_TYPE }
  | "%global_data_type" { look_for_type:=true; KWD_GLOBAL_DATA_TYPE }
  | "%local_data_type" { look_for_type:=true; KWD_LOCAL_DATA_TYPE }
  | "%merge" blank { KWD_MERGE }
  | lowercase identchar *
      { let pos = lexeme_start_p lexbuf in
        let line = pos.pos_lnum in
        let col1 = pos.pos_cnum - pos.pos_bol in
        let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        LIDENT((Lexing.lexeme lexbuf),(line,col1,col2)) }
  | uppercase identchar *
      { let pos = lexeme_start_p lexbuf in
        let line = pos.pos_lnum in
        let col1 = pos.pos_cnum - pos.pos_bol in
        let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        UIDENT((Lexing.lexeme lexbuf),(line,col1,col2)) }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "["
      { Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_pattern := pos;
        (*paren_count:=1;*)
        ocaml_code lexbuf;
        PATTERN (Buffer.contents ocaml_code_buffer,
          (pos.pos_lnum,pos.pos_cnum-pos.pos_bol))
      }
  | ","  { COMMA }
  | ";"  { SEMI }
  | ":"  { COLON }
  | ">"  { GREATER }
  | "%%" { look_for_type:=false; PERCENTPERCENT }
  | "<"
      { if !look_for_type=false then LESS
        else
          (Buffer.clear ocaml_code_buffer;
          start_ocaml_type := lexeme_start_p lexbuf;
          ocaml_type lexbuf;
          OCAML_TYPE (Buffer.contents ocaml_code_buffer))
      }
  | "{"
      { Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_ocaml_code := pos;
        ocaml_code lexbuf;
        OCAML_CODE (Buffer.contents ocaml_code_buffer,
          ((pos.pos_lnum,pos.pos_cnum-pos.pos_bol),false))
      }
  | "@" blank * "{"
      { Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_ocaml_code := pos;
        ocaml_code lexbuf;
        OCAML_CODE (Buffer.contents ocaml_code_buffer,
          ((pos.pos_lnum,pos.pos_cnum-pos.pos_bol),true))
      }
  | "/*"
       { (*dypgen_comment := !dypgen_comment+1;*)
         start_dypgen_comment := (lexeme_start_p lexbuf)::(!start_dypgen_comment);
         comment lexbuf; token lexbuf }
  | "|"  { BAR }
  | "="  { EQUAL }
  | eof { EOF }

and comment = parse
  | "/*" { (*dypgen_comment := !dypgen_comment+1;*)
           start_dypgen_comment := (lexeme_start_p lexbuf)::(!start_dypgen_comment);
           comment lexbuf }
  | "*/"
      { (*dypgen_comment := !dypgen_comment-1;*)
         start_dypgen_comment := List.tl (!start_dypgen_comment);
         if !start_dypgen_comment=[] then () else comment lexbuf }
  | newline
      { update_loc lexbuf None 1 false 0; comment lexbuf }
  | _ { comment lexbuf }

and ocaml_code = parse
  | "}"
      {
        if !in_string = false && !comment_count = 0 then
          begin
            match !start_curlyb with
              | [] ->
                if !start_ocaml_code=dummy_pos then (
                  ocaml_code_buffer $ "}";
                  ocaml_code lexbuf)
                else start_ocaml_code:=dummy_pos
              | _::tl ->
                  start_curlyb:=tl;
                  ocaml_code_buffer $ "}";
                  ocaml_code lexbuf

            (*if (!paren_count) = 0 then start_ocaml_code := dummy_pos
            else
              let _ = ocaml_code_buffer $
                (String.make 1 (Lexing.lexeme_char lexbuf 0)) in
              let _ = paren_count := ((!paren_count)-1) in
              ocaml_code lexbuf*)
          end
        else
          begin
            ocaml_code_buffer $ "}";
            ocaml_code lexbuf
          end
      }
  | "]" { if !in_string=false && !comment_count=0 then (
          match !start_bracket with
            | _::tl -> start_bracket := tl;
                ocaml_code_buffer $ "]";
                ocaml_code lexbuf
            | [] ->
                if !start_pattern=dummy_pos then (
                  ocaml_code_buffer $ "]";
                  ocaml_code lexbuf)
                else
                  start_pattern:=dummy_pos)
          else (
            ocaml_code_buffer $ "]";
            ocaml_code lexbuf) }
  | "[" { if !in_string=false && !comment_count=0 then
            start_bracket := (lexeme_start_p lexbuf)::(!start_bracket);
          ocaml_code_buffer $ "[";
          ocaml_code lexbuf }
  | "$"
      { (if !in_string then ocaml_code_buffer $ "$"
      else ocaml_code_buffer $ "_");
        ocaml_code lexbuf
      }
  | "\\\\"
      { ocaml_code_buffer $ "\\\\";
        ocaml_code lexbuf
      }
  | "\\\""
      { ocaml_code_buffer $ "\\\"";
        ocaml_code lexbuf
      }
  | "\""
      {
        if !in_string then (in_string := false; start_string := dummy_pos)
        else (in_string := true; start_string := lexeme_start_p lexbuf);
        ocaml_code_buffer $ "\"";
        ocaml_code lexbuf
      }
  | "{"
      { ocaml_code_buffer $ "{";
        if !in_string = false && !comment_count = 0 then
          start_curlyb := (lexeme_start_p lexbuf)::!start_curlyb;
          (*paren_count := (!paren_count)+1;*)
        ocaml_code lexbuf
      }
  | "(*"
      {
        if !in_string then () else (comment_count := !comment_count + 1;
          start_ocaml_comment :=
            (lexeme_start_p lexbuf)::(!start_ocaml_comment));
        ocaml_code_buffer $ "(*";
        ocaml_code lexbuf
      }
  | "*)"
      {
        if !in_string then () else (comment_count := !comment_count - 1;
          start_ocaml_comment := List.tl (!start_ocaml_comment));
        ocaml_code_buffer $ "*)";
        ocaml_code lexbuf
      }
  | newline
      { update_loc lexbuf None 1 false 0;
        ocaml_code_buffer $
          (String.make 1 (Lexing.lexeme_char lexbuf 0));
        ocaml_code lexbuf
      }
  | _
      { ocaml_code_buffer $
          (String.make 1 (Lexing.lexeme_char lexbuf 0));
        ocaml_code lexbuf
      }

and ocaml_type = parse
  | "->"
      { ocaml_code_buffer $ "->";
        ocaml_type lexbuf
      }
  | ">" { start_ocaml_type := dummy_pos; () }
  | newline
      { update_loc lexbuf None 1 false 0;
        ocaml_code_buffer $
          (String.make 1 (Lexing.lexeme_char lexbuf 0));
        ocaml_type lexbuf
      }
  | _
      { ocaml_code_buffer $
          (String.make 1 (Lexing.lexeme_char lexbuf 0));
        ocaml_type lexbuf
      }

