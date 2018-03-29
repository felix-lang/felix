{
open Dypgen_parser
open Lexing

let ($) = Buffer.add_string
let ocaml_code_buffer = Buffer.create 100000
let string_buf = Buffer.create 100
let in_string = ref false
let comment_count = ref 0
let look_for_type = ref false
let parser_def = ref false (* this flag is set to true once %% or %parser has been read *)

let in_pattern = ref false

let start_ocaml_type = ref dummy_pos
let start_ocaml_code = ref dummy_pos
let start_curlyb = ref []
(*let start_bracket = ref []*)
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

let decimal_code  c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let char_for_hexadecimal_code d u =
  let d1 = Char.code d in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code u in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c

let string_of_char c = let x = " " in x.[0] <- c; x

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '"' '\'' 'n' 't' 'b' 'r']

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
  | "%mlimid" { KWD_MLIMID }
  | "%mltop" { KWD_MLTOP }
  | "%mli" { KWD_MLI }
  | "%constructor" { KWD_CONSTRUCTOR }
  | "%for" { KWD_FOR }
  | "%non_terminal" { KWD_NON_TERMINAL }
  | "%type" { KWD_TYPE }
  | "%layout" { KWD_LAYOUT }
  | "%merge" blank { KWD_MERGE }
  | "let" { LET }
  | "%lexer" { KWD_LEXER }
  | "%parser" { look_for_type:=false; parser_def := true; KWD_PARSER }
  | "%%" { look_for_type:=false; parser_def := true; PERCENTPERCENT }
  | lowercase identchar *
      { let pos = lexeme_start_p lexbuf in
        let line = pos.pos_lnum in
        let col1 = pos.pos_cnum - pos.pos_bol in
        let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        let fname = pos.pos_fname in
        LIDENT((Lexing.lexeme lexbuf),(line,col1,col2,fname)) }
  | uppercase identchar *
      { let pos = lexeme_start_p lexbuf in
        let line = pos.pos_lnum in
        let col1 = pos.pos_cnum - pos.pos_bol in
        let col2 = lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol in
        let fname = pos.pos_fname in
        UIDENT((Lexing.lexeme lexbuf),(line,col1,col2,fname)) }
  | "("  { LPAREN }
  | "(" blank * "<"  { LPARENLESS }
  | "(" blank * ">"  { LPARENGREATER }
  | ")"  { RPAREN }
  | "["
      { (*if !parser_def then
        (Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_pattern := pos;
        (*paren_count:=1;*)
        ocaml_code lexbuf;
        PATTERN (Buffer.contents ocaml_code_buffer,
          (pos.pos_lnum,pos.pos_cnum-pos.pos_bol)))
      else*) LBRACK
      }
  | "]"  { RBRACK }
  | "^"  { CARET }
  | "-"  { DASH }
  | ","  { COMMA }
  | ";"  { SEMI }
  | ":"  { COLON }
  | "..." { THREEDOTS }
  | "?"  { QUESTION }
  | "+"  { PLUS }
  | "*"  { STAR }
  | "->" { ARROW }
  
  | "'" [^ '\\'] "'" { CHAR(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' backslash_escapes "'"
    { CHAR(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        failwith (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else CHAR (Char.chr v) }
  | "'" '\\' 'x'
       (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u) "'"
       { CHAR(char_for_hexadecimal_code d u) }
  
  | '"'
    { Buffer.clear string_buf;
      string lexbuf;
      STRING (Buffer.contents string_buf) }
  
  | "<"
      { if !look_for_type then
          (Buffer.clear ocaml_code_buffer;
          start_ocaml_type := lexeme_start_p lexbuf;
          ocaml_type lexbuf;
          OCAML_TYPE ("("^(Buffer.contents ocaml_code_buffer)^")"))
        else if !parser_def then
          (Buffer.clear ocaml_code_buffer;
          let pos = lexeme_start_p lexbuf in
          start_pattern := pos;
          in_pattern := true;
          ocaml_code lexbuf;
          PATTERN (Buffer.contents ocaml_code_buffer,pos))
            (*(pos.pos_lnum,pos.pos_cnum-pos.pos_bol)*)
        else LESS
      }
  | "{"
      { Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_ocaml_code := pos;
        ocaml_code lexbuf;
        OCAML_CODE (Buffer.contents ocaml_code_buffer,
          (pos,false))
          (*((pos.pos_lnum,pos.pos_cnum-pos.pos_bol),false)*)
      }
  | "@" blank * "{"
      { Buffer.clear ocaml_code_buffer;
        let pos = lexeme_start_p lexbuf in
        start_ocaml_code := pos;
        ocaml_code lexbuf;
        OCAML_CODE (Buffer.contents ocaml_code_buffer,
          (pos,true))
          (*(pos.pos_lnum,pos.pos_cnum-pos.pos_bol),true*)
      }
  | "/*"
       { (*dypgen_comment := !dypgen_comment+1;*)
         start_dypgen_comment := (lexeme_start_p lexbuf)::(!start_dypgen_comment);
         comment lexbuf; token lexbuf }
  | "|"  { BAR }
  | "!"  { BANG }
  | "="  { EQUAL }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * newline
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf
      }
  | eof { EOF }

(* String parsing comes from the ocaml compiler lexer *)
and string = parse
    '"'
    { () }
   | '\\' ("\010" | "\013" | "\013\010") ([' ' '\009'] * as spaces)
    { update_loc lexbuf None 1 false (String.length spaces);
      string lexbuf }
  | '\\' (backslash_escapes as c)
    { string_buf $ (string_of_char(char_for_backslash c));
      string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if v > 255 then
       failwith
        (Printf.sprintf
          "illegal backslash escape in string: `\\%c%c%c'" c d u);
      string_buf $ (string_of_char (Char.chr v));
      string lexbuf }
 | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { string_buf $ (string_of_char (char_for_hexadecimal_code d u));
      string lexbuf }
  | '\\' (_ as c)
    { failwith
        (Printf.sprintf "illegal backslash escape in string: `\\%c'" c) }
  | eof
    { failwith "unterminated string" }
  | '\010'
    { string_buf $ "\010";
      update_loc lexbuf None 1 false 0;
      string lexbuf }
  | _ as c
    { string_buf $ (string_of_char c);
      string lexbuf }

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
  | ">" { if !in_string=false && !comment_count=0 && !in_pattern
        then (in_pattern := false; start_pattern := dummy_pos)
        else (
          ocaml_code_buffer $ ">";
          ocaml_code lexbuf) }
  | "$" (['0'-'9']+ as n)
      { (if !in_string then ocaml_code_buffer $ (Lexing.lexeme lexbuf)
      else ocaml_code_buffer $ ("_"^n));
        ocaml_code lexbuf
      }
  | "$<"
      { (if !in_string then ocaml_code_buffer $ (Lexing.lexeme lexbuf)
      else ocaml_code_buffer $ ("dyp.Dyp.rhs_start_pos "));
        ocaml_code lexbuf
      }
  | "$>"
      { (if !in_string then ocaml_code_buffer $ (Lexing.lexeme lexbuf)
      else ocaml_code_buffer $ ("dyp.Dyp.rhs_end_pos "));
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
