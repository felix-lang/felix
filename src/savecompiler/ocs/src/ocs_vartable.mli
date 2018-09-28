(* Utility type for variable binding tables.  *)

type 'a vartable

val vt_create : unit -> 'a vartable
val vt_inherit : 'a vartable -> 'a vartable
val vt_global : 'a vartable -> bool
val vt_copy : 'a vartable -> ('a -> 'a) -> 'a vartable

val var_insert : 'a vartable -> string -> 'a -> unit
val var_find : 'a vartable -> string -> 'a option
val var_get : 'a vartable -> string -> (unit -> 'a) -> 'a

