open Flx_ast
type charset_t
val charset_of_string: string -> charset_t
val charset_of_int_range: int -> int -> charset_t
val charset_of_range: string -> string -> charset_t
val charset_union: charset_t -> charset_t -> charset_t
val charset_inv: charset_t -> charset_t
val regexp_of_charset: charset_t -> regexp_t
val regexp_underscore: regexp_t
val eol: int
val regexp_dot: regexp_t
