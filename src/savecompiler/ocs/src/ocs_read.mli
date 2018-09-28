(* Reader of Scheme expressions.  *)

open Ocs_types
open Ocs_lex

val read_expr : lexer -> sval

val read_from_port : Ocs_port.port -> sval
val read_from_string : string -> sval

