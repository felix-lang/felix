module type S =
  sig
    include Hashtbl.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : 'a t -> key -> 'a option
  end;;

module Make (M:Hashtbl.HashedType) : S with type key = M.t =
  struct
    include Hashtbl.Make(M)

    let find_opt table key =
      try Some (find table key) with Not_found -> None
  end;;

module IntHashtbl = Make (
  struct
    type t = int
    let equal = fun x y -> x = y
    let hash = fun x -> x
  end)

(** Search through the table and optionally return the key. *)
let find_opt table key =
  try
    Some (Hashtbl.find table key)
  with Not_found ->
    None
