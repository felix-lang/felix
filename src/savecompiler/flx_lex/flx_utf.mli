(** Internationalised identifier support *)
exception Utf8_to_Ucn_Error of string

val ucs_id_ranges : (int * int) list
val utf8_to_ucn : string -> string
