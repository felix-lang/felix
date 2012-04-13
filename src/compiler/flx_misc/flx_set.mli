module type S =
  sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val iteri : (int -> elt -> unit) -> t -> unit
    val union_list : elt list -> t -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
  end;;

module type OrderedType=
  sig
    include Set.OrderedType
  end;;

module Make (M:OrderedType) : S with type elt = M.t

module StringSet : S with type elt = string

module IntSet : S with type elt = int

module IntSetSet : S with type elt = IntSet.t
