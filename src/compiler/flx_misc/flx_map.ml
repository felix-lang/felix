module type S =
  sig
    include Map.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : key -> 'a t -> 'a option
  end;;

module Make (M:Map.OrderedType) : S with type key = M.t =
  struct
    include Map.Make(M)

    let find_opt key table =
      try Some (find key table) with Not_found -> None
  end;;

module StringMap = Make (
  struct
    type t = string
    let compare = compare
  end);;

type string_string_map_t = string StringMap.t
