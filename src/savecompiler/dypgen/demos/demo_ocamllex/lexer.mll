{ (* lexer.mll *)

open Parser
let lex_define = ref false }

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
  | "define" { DEFINE }
  | "in" { IN }
  | lowercase identchar *
      { LIDENT(Lexing.lexeme lexbuf) }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "=" { lex_define := false; EQUAL }
  | ":=" { lex_define := true; COLONEQUAL }
  | "[" { if !lex_define then TOKEN "[" else LBRACK }
  | "]" { if !lex_define then TOKEN "]" else RBRACK }
  | "::" { if !lex_define then TOKEN "::" else COLONCOLON }
  | ";" { if !lex_define then TOKEN ";" else SEMICOLON }
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | "," { COMMA }
  | eof { EOF }