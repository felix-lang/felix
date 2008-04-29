module IntHashtbl : Hashtbl.S with type key = int
module StringMap : Map.S with type key = string

type string_string_map_t = string StringMap.t
module StringSet : Set.S with type elt = string

val stringset_map: (string -> string) -> StringSet.t -> StringSet.t

module IntSet : Set.S with type elt = int
module IntSetSet : Set.S with type elt = IntSet.t

val string_of_intset : IntSet.t -> string
val intset_of_list : int list -> IntSet.t
