module IntHashtbl = Hashtbl.Make(
  struct
    type t = int
    let equal = fun x y -> x = y
    let hash = fun x -> x
  end
)
;;

module StringMap = Map.Make(
  struct
    type t = string
    let compare = compare
  end
)
;;

type string_string_map_t = string StringMap.t

module StringSet = Set.Make (
  struct
    type t = string
    let compare=compare
  end
);;

module IntSet = Set.Make (
  struct
    type t = int
    let compare=compare
  end
);;

(* set of IntSet's *)
module IntSetSet = Set.Make (
  struct
    type t = IntSet.t
    let compare=compare
  end
);;

let stringset_map f s =
  let d = ref StringSet.empty in
  StringSet.iter
  (fun x -> d := StringSet.add (f x) !d)
  s
  ;
  !d

let list_of_intset ii =
  IntSet.fold (fun i lst -> i :: lst) ii []

let string_of_intset ii =
  "{"^
  String.concat ","
  (
    List.map string_of_int
    (
      list_of_intset ii
    )
  )
  ^"}"

let intset_of_list ii =
  List.fold_left (fun ii i -> IntSet.add i ii) IntSet.empty ii
