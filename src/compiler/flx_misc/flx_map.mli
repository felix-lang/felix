module type S =
  sig
    include Map.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : key -> 'a t -> 'a option
  end;;

module Make (M:Map.OrderedType) : S with type key = M.t

module StringMap : S with type key = string

type string_string_map_t = string StringMap.t
