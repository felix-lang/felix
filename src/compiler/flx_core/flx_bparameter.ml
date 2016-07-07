type t = {
  pid: string;
  pindex: Flx_types.bid_t;
  pkind: Flx_ast.param_kind_t;
  ptyp: Flx_btype.t
}

(** Returns the names from a list of bparameters. *)
let get_names ps =
  List.map (fun p -> p.pid) ps

(** Returns the bids from a list of bparameters. *)
let get_bids ps =
  List.map (fun p -> p.pindex) ps

(** Returns the bound types from a list of bparameters. *)
let get_btypes ps =
  List.map begin fun p -> p.ptyp end ps



