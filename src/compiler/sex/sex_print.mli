(** {6 Output text representation of sexp_t *)

(** Output using processor *)
val sex_out: (string->unit) -> Sex_types.sexp_t -> unit

(** Print to standard output *)
val sex_print: Sex_types.sexp_t -> unit

(** Output to a string *)
val string_of_sex: Sex_types.sexp_t -> string

