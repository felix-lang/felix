type t = Flx_bparameter.t list * Flx_types.tbexpr_t option

(** Returns the bound types from a list of bparameters. *)
let get_btypes (bparameters, _) = Flx_bparameter.get_btypes bparameters

(** Prints a bparams to a formatter. *)
let print f (bparameters, tbexpr) =
  Flx_format.print_tuple2 f
    (Flx_list.print Flx_bparameter.print) bparameters
    (Flx_format.print_opt Flx_types.print_tbexpr) tbexpr
