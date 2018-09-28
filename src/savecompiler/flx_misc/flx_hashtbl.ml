module type S =
  sig
    include Hashtbl.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : 'a t -> key -> 'a option

    (** Prints the table. *)
    val print :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit
  end;;

module type HashedTypePrintable =
  sig
    include Hashtbl.HashedType
    val print : Format.formatter -> t -> unit
  end;;

module Make (M:HashedTypePrintable) : S with type key = M.t =
  struct
    include Hashtbl.Make(M)

    let find_opt table key =
      try Some (find table key) with Not_found -> None

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

module IntHashtbl = Make (
  struct
    type t = int
    let equal = fun x y -> x = y
    let hash = fun x -> x
    let print = Format.pp_print_int
  end)

(** Search through the table and optionally return the key. *)
let find_opt table key =
  try
    Some (Hashtbl.find table key)
  with Not_found ->
    None

let print pp_key pp_value f table =
  Format.fprintf f "@[<hv0>@[<hv2>{.@ ";
  let _ =
    Hashtbl.fold begin fun key value first ->
      if not first then Format.fprintf f ";@ ";
      Format.fprintf f "%a@ =@ %a" pp_key key pp_value value;
      false
    end table true
  in
  Format.fprintf f "@]@ .}@]"

