module type S =
  sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val of_list : elt list -> t
  end;;

module Make (M:Set.OrderedType) : S with type elt = M.t =
  struct
    include Set.Make(M)
    let map f set = fold (fun x -> add (f x)) set empty
    let of_list l = List.fold_right add l empty
  end;;

module StringSet = Make (
  struct
    type t = string
    let compare = compare
  end)

module IntSet = Make (
  struct
    type t = int
    let compare = compare
  end)

(* set of IntSet's *)
module IntSetSet = Make (
  struct
    type t = IntSet.t
    let compare = compare
  end)
