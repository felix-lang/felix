(** {6 Type of a sex s-exression} *)
type sexp_t =
  | Int of string
  | Str of string
  | Sym of string
  | Id of string
  | Lst of sexp_t list
