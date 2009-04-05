val pre_flx_lex :
  Flx_lexstate.lexer_state ->
  Lexing.lexbuf ->
  Flx_token.token list

val parse_line :
  Flx_lexstate.lexer_state ->
  Lexing.lexbuf ->
  string
