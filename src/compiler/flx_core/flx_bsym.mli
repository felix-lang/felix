(** The bound symbol type. *)
type t = {
  id:Flx_id.t;
  sr:Flx_srcref.t;
  bbdcl:Flx_bbdcl.t;
}


(** Constructs a bound symbol value. *)
val create:
  ?sr:Flx_srcref.t ->
  Flx_id.t -> Flx_bbdcl.t ->
  t

(** Constructs a bound symbol based off the unbound symbol. *)
val of_sym: Flx_sym.t -> Flx_bbdcl.t -> t

(** Returns a copy of the bound symbol with the bbdcl set to the passed in
 * bbdcl.*)
val replace_bbdcl: t -> Flx_bbdcl.t -> t

val id: t -> Flx_id.t
val sr: t -> Flx_srcref.t
val bbdcl: t -> Flx_bbdcl.t

(** Return if the bound symbol is an identity function. *)
val is_identity: t -> bool

(** Return if the bound symbol is a val or var. *)
val is_variable: t -> bool

(** Return if the bound symbol is a function or procedure. *)
val is_function: t -> bool

(** Return if the bound symbol is a generator. *)
val is_generator: t -> bool

(** Returns the bound parameters of the bound symbol. *)
val get_bparams: t -> Flx_bparams.t

(** Returns the bound type value list of the bound symbol. *)
val get_bvs: t -> Flx_kind.bvs_t

(** Calls the function over every bid inside the bound symbol. *)
val iter_uses : (Flx_bid.bid_t -> unit) -> t -> unit
