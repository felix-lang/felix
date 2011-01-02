type t = Flx_bparameter.t list * Flx_bexpr.t option

(** Returns the bound types from a list of bparameters. *)
let get_btypes (bparameters, _) = Flx_bparameter.get_btypes bparameters

let get_btype ps = match get_btypes ps with
  | [] -> Flx_btype.btyp_tuple  []
  | [x] -> x
  | xs -> Flx_btype.btyp_tuple xs

(** Prints a bparams to a formatter. *)
let print f (bparameters, tbexpr) =
  Flx_format.print_tuple2 f
    (Flx_list.print Flx_bparameter.print) bparameters
    (Flx_format.print_opt Flx_bexpr.print) tbexpr
