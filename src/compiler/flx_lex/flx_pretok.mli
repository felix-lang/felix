open Flx_token
open Flx_ast

val pre_tokens_of_filename :
  string -> string -> string list ->
  string option ->
  (string -> expr_t -> expr_t) ->
  string list -> (* auto imports *)
  token list

val pre_tokens_of_string :
  string -> string ->
  (string -> expr_t -> expr_t) ->
  token list
