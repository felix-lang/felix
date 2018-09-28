(** {6 Type of sex lexical token} *)
type token =
 | LB
 | RB
 | EOF
 | STR of string
 | SYM of string
 | ID of string
 | INT of string
