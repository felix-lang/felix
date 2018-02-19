type xps_t = Flx_bparameter.t Flx_ast.sexpr_t
type t = xps_t * Flx_bexpr.t option

val bparams_of_list : Flx_bparameter.t list -> xps_t

(** Returns the bids from a bparameters sexpr as a list. *)
val get_bids : t -> Flx_bid.bid_t list
val xget_bids : xps_t -> Flx_bid.bid_t list

(** Returns the bound types from a bparameter sexpr as a list *)
val get_btypes : t -> Flx_btype.t list

(** Returns the bound type of a of bparameter sexpr as a tuple *)
val get_btype : t -> Flx_btype.t
val xget_btype : xps_t -> Flx_btype.t

val xget_prjs : xps_t  -> (Flx_bparameter.t * Flx_bexpr.t option) list
val get_prjs : t  -> (Flx_bparameter.t * Flx_bexpr.t option) list
val get_params : t -> Flx_bparameter.t list
val get_names : t -> string list

val iter: 
  ?f_bid: (Flx_bid.bid_t -> unit)  -> 
  ?f_btype: (Flx_btype.t->unit) -> 
  ?f_bexpr:(Flx_bexpr.t -> unit) ->
  t -> 
  unit

val map:
  ?f_bid: (Flx_bid.bid_t -> Flx_bid.bid_t)  -> 
  ?f_btype: (Flx_btype.t -> Flx_btype.t) -> 
  ?f_bexpr:(Flx_bexpr.t ->Flx_bexpr.t) ->
  t -> 
  t

val piter:
  (Flx_bparameter.t -> unit)  -> t -> unit

val xpmap:
  (Flx_bparameter.t -> Flx_bparameter.t) ->
  xps_t -> xps_t
 
val unit_bparams : t
