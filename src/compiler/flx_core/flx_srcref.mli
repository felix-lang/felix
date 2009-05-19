(** {6 Routines to extract source reference from terms}
 *
 * Source reference manipulators. *)

(** type of a span between two positions in one file*)
type t =
  string * (* filename *)
  int * (* starting line number, 1 origin *)
  int * (* starting column, 1 origin *)
  int * (* ending line number, 1 origin *)
  int   (* ending column, 1 origin *)

val rsrange: t -> t -> t

val short_string_of_src: t -> string
val long_string_of_src: t -> string

val dummy_sr: t
