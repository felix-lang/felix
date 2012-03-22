type t = Flx_bparameter.t list * Flx_bexpr.t option

(** Returns the bound types from a list of bparameters. *)
val get_btypes : t -> Flx_btype.t list

(** Returns the bound type from a list of bparameters. *)
val get_btype : t -> Flx_btype.t


