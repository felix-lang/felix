(** The bound symbol type. *)
type t

(** Constructs a bound symbol value. *)
val create:
  ?sr:Flx_srcref.t ->
  ?vs:Flx_types.ivs_list_t ->
  ?pubmap:Flx_btype.name_map_t ->
  ?privmap:Flx_btype.name_map_t ->
  ?dirs:Flx_types.sdir_t list ->
  string -> Flx_bbdcl.t ->
  t

(** Constructs a bound symbol based off the unbound symbol. *)
val of_sym: Flx_sym.t -> Flx_bbdcl.t -> t

(** Returns a copy of the bound symbol with the bbdcl set to the passed in
 * bbdcl.*)
val replace_bbdcl: t -> Flx_bbdcl.t -> t

val id: t -> string
val sr: t -> Flx_srcref.t
val vs: t -> Flx_types.ivs_list_t
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
val get_bvs: t -> Flx_types.bvs_t

(** Prints a bound symbol to a formatter. *)
val print : Format.formatter -> t -> unit
