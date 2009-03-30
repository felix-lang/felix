{
open Pgen_parser_param
let ocaml_code_buffer = ref ""
let paren_count = ref 0
let in_string = ref false
let comment_count = ref 0
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
  | newline
      { token lexbuf }
  | blank +
      { token lexbuf }
  | "%token" { KWD_TOKEN }
  | "%start" { KWD_START }
  | "%relation" { KWD_RELATION }
  | "%full" { KWD_FULL }
  | lowercase identchar *
      { LIDENT(Lexing.lexeme lexbuf) }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | ":"  { COLON }
  | "%%"  { PERCENTPERCENT }
  | "<"
      { ocaml_code_buffer := "";
        ocaml_type lexbuf;
        OCAML_TYPE (!ocaml_code_buffer)
      }
  | "{"
      { ocaml_code_buffer := "";
        ocaml_code lexbuf;
        OCAML_CODE (!ocaml_code_buffer)
      }
  | "|"  { BAR }
  | "="  { EQUAL }
  | eof { EOF }

and ocaml_code = parse
  | "}"
      {
        if !in_string = false && !comment_count = 0 then
          begin
            if (!paren_count) = 0 then ()
            else
              let _ = ocaml_code_buffer := ((!ocaml_code_buffer) ^
                (String.make 1 (Lexing.lexeme_char lexbuf 0))) in
              let _ = paren_count := ((!paren_count)-1) in
              ocaml_code lexbuf
          end
        else
          begin
            ocaml_code_buffer := (!ocaml_code_buffer) ^ "}";
            ocaml_code lexbuf
          end
      }
  | "$"
      { ocaml_code_buffer := ((!ocaml_code_buffer) ^
          "_");
        ocaml_code lexbuf
      }
  | "\\\""
      { ocaml_code_buffer := ((!ocaml_code_buffer) ^ "\\\"");
        ocaml_code lexbuf
      }
  | "\""
      {
        if !in_string then in_string := false else in_string := true;
        ocaml_code_buffer := (!ocaml_code_buffer) ^ "\"";
        ocaml_code lexbuf
      }
  | "{"
      { ocaml_code_buffer := (!ocaml_code_buffer) ^ "{";
        if !in_string = false && !comment_count = 0 then
          paren_count := (!paren_count)+1;
        ocaml_code lexbuf
      }
  | "(*"
      {
        if !in_string then () else comment_count := !comment_count + 1;
        ocaml_code_buffer := ((!ocaml_code_buffer) ^ "(*");
        ocaml_code lexbuf
      }
  | "*)"
      {
        if !in_string then () else comment_count := !comment_count - 1;
        ocaml_code_buffer := ((!ocaml_code_buffer) ^ "*)");
        ocaml_code lexbuf
      }
  | _
      { ocaml_code_buffer := ((!ocaml_code_buffer) ^
          (String.make 1 (Lexing.lexeme_char lexbuf 0)));
        ocaml_code lexbuf
      }

and ocaml_type = parse
  | ">" { () }
  | _
      { ocaml_code_buffer := ((!ocaml_code_buffer) ^
          (String.make 1 (Lexing.lexeme_char lexbuf 0)));
        ocaml_type lexbuf
      }

