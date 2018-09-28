(** Type of embedded C++ code. *)
type t =
  | Str_template of string
  | Str of string
  | Virtual
  | Identity

val isempty: t->bool

(*
(** Prints out a code specification to a formatter. *)
val print : Format.formatter -> t -> unit
*)
