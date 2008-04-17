(* The Longarray module is designed to work around the maximum array size
 * imposed by OCaml's built-in Array module.  Longarray provides the
 * same interface as Array (well, a portion of it) and is implemented as
 * a list of arrays.  For arrays shorter than the maximum length, the
 * only cost is an additional level of indirection. *)

type 'a t

val create : int -> 'a -> 'a t
val init : int -> (int -> 'a) -> 'a t
val blit : 'a t -> int -> 'a t -> int -> int -> unit
val fill : 'a t -> int -> int -> 'a -> unit
val length : 'a t -> int
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val copy : 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t

val docArray : ?sep: Pretty.doc -> (int -> 'a -> Pretty.doc) ->
               unit -> 'a t -> Pretty.doc
