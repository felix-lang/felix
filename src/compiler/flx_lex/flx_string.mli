(** String codecs *)

(** Exception thrown on decoding error *)
exception StringError of string

(* Character Decoders *)

(** decode binary digit 01 to int *)
val bin_char2int : char -> int

(** decode octal digit 01234567 to int *)
val oct_char2int : char -> int

(** decode decimal digit 0123456789 to int *)
val dec_char2int : char -> int

(** decode hexadecimal digit 0123456789abcdefABCDEF to int *)
val hex_char2int : char -> int

(* String Decoders *)

(** decode binary digit string 01 to int *)
val binint_of_string : string -> int

(** decode octal digit string 01234567 to int *)
val octint_of_string : string -> int

(** decode decimal digit  string 0123456789 to int *)
val decint_of_string : string -> int

(** decode hexadecimal digit string 0123456789abcdefABCDEF to int *)
val hexint_of_string : string -> int

(** decode floating point string to float *)
val floating_of_string : string -> float

(** Escape code Decoder *)
val unescape : string -> string

(** Escape a String *)
val escape_of_string : char -> string -> string

(** Make Python Style double quoted string *)
val py_dquote_of_string : string -> string

(** Make Python Style single quoted string *)
val py_quote_of_string : string -> string

(** Make C style quoted string *)
val c_quote_of_string : string -> string

(** Convert Integer value to Utf8 encoding *)
val utf8_of_int : int -> string

(** Parse a Utf8 encoded string 
   starting at a given position,
   return the code point and position one
   past the last character decoded
*)
val parse_utf8 : string -> int -> int * int

(** Convert integer to exactly 2 hex digits *)
val hex2 : int -> string

(** Convert integer to exactly 4 hex digits *)
val hex4 : int -> string

(** Convert integer to exactly 8 hex digits *)
val hex8 : int -> string
