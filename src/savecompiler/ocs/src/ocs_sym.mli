(* Symbol table interface.  *)

open Ocs_types

(* get_symbol returns a symbol corresponding to a string.  It is
   created if it doesn't exist.  *)
val get_symbol : string -> sval
val sym_name : sval -> string

(* Keywords are globally defined for convenience.  *)
val sym_quote : sval
val sym_lambda : sval
val sym_if : sval
val sym_set : sval
val sym_begin : sval
val sym_cond : sval
val sym_and : sval
val sym_or : sval
val sym_case : sval
val sym_let : sval
val sym_letstar : sval
val sym_letrec : sval
val sym_do : sval
val sym_delay : sval
val sym_quasiquote : sval
val sym_else : sval
val sym_arrow : sval
val sym_define : sval
val sym_unquote : sval
val sym_unquote_splicing : sval

val sym_define_syntax : sval
val sym_let_syntax : sval
val sym_letrec_syntax : sval
val sym_syntax_rules : sval
val sym_ellipsis : sval
