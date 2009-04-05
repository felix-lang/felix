open Flx_ast
open Flx_token
open Flx_lexstate
open Lexing

val is_in_string : string -> char -> bool
val is_white : char -> bool
val is_digit : char -> bool
val strip_us : string -> string

val pre_tokens_of_lexbuf :
   (lexer_state -> lexbuf -> token list) ->
  lexbuf -> lexer_state ->
  token list

val pre_tokens_of_string :
  (lexer_state -> lexbuf -> token list) ->
  string -> string ->
  (string -> expr_t -> expr_t) ->
  token list

val line_directive :
  lexer_state -> range_srcref -> string ->  lexbuf ->
  token list

val include_directive :
  string ->
  lexer_state -> range_srcref -> string ->
  (lexer_state -> lexbuf -> token list) ->
  token list

val handle_preprocessor :
  lexer_state -> lexbuf -> string ->
  (lexer_state -> lexbuf -> token list) ->
  location ->
  Lexing.position ->
  token list
