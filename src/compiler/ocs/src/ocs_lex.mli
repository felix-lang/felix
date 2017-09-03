(* Lexer for Scheme.  *)

open Ocs_types
open Ocs_error

type token =
    Leof
  | Lopenv			(* #( *)
  | Lunqsplice			(* ,@ *)
  | Lident of bytes
  | Lstring of bytes
  | Lnumber of sval
  | Lbool of sval
  | Lchar of sval
  | Ltoken of char

type lexer

val make_lexer : Ocs_port.port -> bytes -> lexer
val get_loc : lexer -> location
val get_tok : lexer -> token

