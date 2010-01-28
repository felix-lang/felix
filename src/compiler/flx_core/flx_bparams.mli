type t = Flx_bparameter.t list * Flx_bexpr.t option

(** Returns the bound types from a list of bparameters. *)
val get_btypes : t -> Flx_btype.t list

(** Prints a bparams to a formatter. *)
val print : Format.formatter -> t -> unit
