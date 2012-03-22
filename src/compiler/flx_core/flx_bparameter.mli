type t = {
  pid: string;
  pindex: Flx_types.bid_t;
  pkind: Flx_ast.param_kind_t;
  ptyp: Flx_btype.t
}

(** Returns the names from a list of bparameters. *)
val get_names : t list -> string list

(** Returns the bids from a list of bparameters. *)
val get_bids : t list -> Flx_types.bid_t list

(** Returns the bound types from a list of bparameters. *)
val get_btypes : t list -> Flx_btype.t list


