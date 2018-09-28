type charset_t
val charset_of_string: string -> charset_t
val charset_of_int_range: int -> int -> charset_t
val charset_of_range: string -> string -> charset_t
val charset_union: charset_t -> charset_t -> charset_t
val charset_inv: charset_t -> charset_t
val eol: int
