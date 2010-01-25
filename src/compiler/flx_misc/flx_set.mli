module type S =
  sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val of_list : elt list -> t
  end;;

module Make (M:Set.OrderedType) : S with type elt = M.t

module StringSet : S with type elt = string

module IntSet : S with type elt = int

module IntSetSet : S with type elt = IntSet.t
