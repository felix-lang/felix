open Flx_token
open Flx_ast

val print_pre_tokens : token list -> unit
val print_tokens : token list -> unit
class tokeniser :
  token list ->
  object
    val mutable current_token_index : int
    val mutable tokens : token list
    val mutable tokens_copy : token list
    method report_syntax_error : unit
    method put_back : token -> unit
    method get_loc: range_srcref
    method token_src : Lexing.lexbuf -> token
    method token_peek : Lexing.lexbuf -> token
  end

type 'a parser_t =
  (Lexing.lexbuf  -> token) ->
  Lexing.lexbuf ->
  'a

val parse_tokens:
  'a parser_t ->
  token list ->
  'a
