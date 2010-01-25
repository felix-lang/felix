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

module Make (M:OrderedTypePrintable) : S with type key = M.t =
  struct
    include Map.Make(M)

    let find_opt key table =
      try Some (find key table) with Not_found -> None

    let print pp_value f table =
      Format.fprintf f "@[<hv0>@[<hv2>{.@ ";
      let _ =
        fold begin fun key value first ->
          if not first then Format.fprintf f ";@ ";
          Format.fprintf f "%a@ =@ %a" M.print key pp_value value;
          false
        end table true
      in
      Format.fprintf f "@]@ .}@]"
  end;;

module StringMap = Make (
  struct
    type t = string
    let compare = compare
    let print f s = Format.fprintf f "%S" s
  end);;

type string_string_map_t = string StringMap.t
