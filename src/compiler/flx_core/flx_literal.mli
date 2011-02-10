(** Literals recognized by the lexer. *)
type t =
  | Int of string * string (* first string is kind, second is value *)
  | String of string
  | Cstring of string
  | Wstring of string
  | Ustring of string
  | Float of string * string

(** Return if two literals are equivalent. *)
val eq : t -> t -> bool

(** Prints out a literal to a formatter. *)
val print : Format.formatter -> t -> unit
