(** Type of embedded C++ code. *)
type t =
  | Str_template of string
  | Str of string
  | Virtual
  | Identity

(*
(** Prints out a code specification to a formatter. *)
val print : Format.formatter -> t -> unit
*)
