open TinyML_parser
let lex_define = ref false

let regexp newline = ('\010' | '\013' | "\013\010")
let regexp blank = [' ' '\009' '\012']
let regexp lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let regexp uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let regexp identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

let rec token = lexer
  | newline
      -> token lexbuf
  | blank +
      -> token lexbuf
  | "match" -> MATCH
  | "with" -> WITH
  | "define" -> DEFINE
  | "in" -> IN
  | "and" -> AND
  | "let" -> LET
  | "rec" -> REC
  | lowercase identchar *
      -> LIDENT(Ulexing.utf8_lexeme lexbuf)
  | uppercase identchar *
      -> UIDENT(Ulexing.utf8_lexeme lexbuf)
  | "("  -> LPAREN
  | ")"  -> RPAREN
  | "|"  -> if !lex_define then TOKEN "|" else BAR
  | "=" -> let () = lex_define := false in EQUAL
  | ":=" -> let () = lex_define := true in COLONEQUAL
  | "[" -> if !lex_define then TOKEN "[" else LBRACK
  | "]" -> if !lex_define then TOKEN "]" else RBRACK
  | "::" -> if !lex_define then TOKEN "::" else COLONCOLON
  | ";" -> if !lex_define then TOKEN ";" else SEMICOLON
  | "<" -> if !lex_define then TOKEN "<" else LESS
  | ">" -> if !lex_define then TOKEN ">" else GREATER
  | "@" -> if !lex_define then TOKEN "@" else APPEND
  | ['0'-'9']+ -> INT(int_of_string (Ulexing.utf8_lexeme lexbuf))
  | "," -> COMMA
  | "->" -> ARROW
  | eof -> EOF
