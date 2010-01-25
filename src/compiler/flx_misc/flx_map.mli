module type S =
  sig
    include Map.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : key -> 'a t -> 'a option

    (** Prints the table. *)
    val print :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit
  end;;

module type OrderedTypePrintable =
  sig
    include Set.OrderedType
    val print : Format.formatter -> t -> unit
  end;;

module Make (M:OrderedTypePrintable) : S with type key = M.t

module StringMap : S with type key = string

type string_string_map_t = string StringMap.t
