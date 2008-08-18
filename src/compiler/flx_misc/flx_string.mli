(** String handling *)

exception Error of string

val bin_char2int : char -> int
val oct_char2int : char -> int
val dec_char2int : char -> int
val hex_char2int : char -> int

val binint_of_string : string -> int
val octint_of_string : string -> int
val decint_of_string : string -> int
val hexint_of_string : string -> int

val binbig_int_of_string : string -> Big_int.big_int
val octbig_int_of_string : string -> Big_int.big_int
val decbig_int_of_string : string -> Big_int.big_int
val hexbig_int_of_string : string -> Big_int.big_int

val floating_of_string : string -> float

val unescape : string -> string

val escape_of_string : char -> string -> string
val py_dquote_of_string : string -> string
val py_quote_of_string : string -> string
val c_quote_of_string : string -> string
val utf8_of_int : int -> string
val parse_utf8 : string -> int -> int * int
val hex2 : int -> string
val hex4 : int -> string
val hex8 : int -> string
