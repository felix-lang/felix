(** Bind executable statements *)

type bexe_state_t

val make_bexe_state:
  Flx_mtypes2.sym_state_t ->  (** The symbol state *)
  Flx_types.env_t ->          (** The local symbol lookup table *)
  string ->                   (** The parent symbol's name *)
  Flx_ast.bid_t ->            (** The parent symbol's index *)
  Flx_types.bvs_t ->          (** The parent symbol's bound type variables *)
  Flx_types.btypecode_t ->    (** The expected return type *)
  bexe_state_t

(** Bind a series of executables that are inside of a function. *)
val bind_exes:
  bexe_state_t ->             (** The state needed to bind the exes. *)
  Flx_srcref.t ->             (** The parent's srcref. *)
  Flx_ast.sexe_t list ->      (** The list of executables to bind. *)
  Flx_types.btypecode_t * Flx_types.bexe_t list
