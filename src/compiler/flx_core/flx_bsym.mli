(** The bound symbol type. *)
type t = {
  id: string;
  sr: Flx_srcref.t;
  parent: Flx_types.bid_t option;
  vs:Flx_types.ivs_list_t;
  pubmap:Flx_types.name_map_t;
  privmap:Flx_types.name_map_t;
  dirs:Flx_types.sdir_t list;
  bbdcl: Flx_types.bbdcl_t;
}

(** Return if the bound symbol is an identity function. *)
val is_identity: t -> bool

(** Return if the bound symbol is a val or var. *)
val is_variable: t -> bool

(** Return if the bound symbol is a global val or var. *)
val is_global_var: t -> bool

(** Return if the bound symbol is a function or procedure. *)
val is_function: t -> bool

(** Return if the bound symbol is a generator. *)
val is_generator: t -> bool

(** Returns the bound type value list of the bound symbol. *)
val get_bvs: t -> Flx_types.bvs_t
