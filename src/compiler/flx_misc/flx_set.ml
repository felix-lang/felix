module type S =
  sig
    include Set.S
    val map : (elt -> elt) -> t -> t
    val iteri : (int -> elt -> unit) -> t -> unit
    val union_list : elt list -> t -> t
    val of_list : elt list -> t
    val to_list : t -> elt list
    val print : Format.formatter -> t -> unit
  end;;

module type OrderedTypePrintable =
  sig
    include Set.OrderedType
    val print : Format.formatter -> t -> unit
  end;;

module Make (M:OrderedTypePrintable) : S with type elt = M.t =
  struct
    include Set.Make(M)
    let map f set = fold (fun x -> add (f x)) set empty
    let iteri f set = ignore (fold (fun x i -> f i x; i + 1) set 0)
    let union_list list set =
      List.fold_left (fun set elt -> add elt set) set list
    let of_list list = union_list list empty
    let to_list set = fold (fun elt list -> elt :: list) set []
    let print f s =
      Format.fprintf f "@[<hv0>@[<hv2>{.@ ";
      let _ =
        fold begin fun elt first ->
          if not first then Format.fprintf f ";@ ";
          M.print f elt;
          false
        end s true
      in
      Format.fprintf f "@]@ .}@]";
  end;;

module StringSet = Make (
  struct
    type t = string
    let compare = compare
    let print f s = Format.fprintf f "%S" s
  end)

module IntSet = Make (
  struct
    type t = int
    let compare = compare
    let print = Format.pp_print_int
  end)

(* set of IntSet's *)
module IntSetSet = Make (
  struct
    type t = IntSet.t
    let compare = compare
    let print = IntSet.print
  end)
