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

module Make (M:OrderedType) : S with type elt = M.t =
  struct
    include Set.Make(M)
    let map f set = fold (fun x -> add (f x)) set empty
    let iteri f set = ignore (fold (fun x i -> f i x; i + 1) set 0)
    let union_list list set =
      List.fold_left (fun set elt -> add elt set) set list
    let of_list list = union_list list empty
    let to_list set = fold (fun elt list -> elt :: list) set []
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

