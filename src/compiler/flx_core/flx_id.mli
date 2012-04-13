(** {7 Names}
 *
 * A simple name is an identifier, a qualified name is a dot (.) separated list
 * of instantiated names, and a instantiated name is a simple name optionally
 * followed by a square bracket enclosed list of type expressions. *)
type t = string

(** Create an identifier from a string. *)
val of_string : string -> t

(** Convert an identifier to a string. *)
val to_string : t -> string

(*
(** Prints out an to a formatter. *)
val print : Format.formatter -> t -> unit
*)
