type bind_state_t

type bound_t =
  | Bound_exe of Flx_types.bexe_t
  | Bound_symbol of (int * Flx_types.symbol_data3_t)

val make_bind_state:
  ?parent:Flx_ast.bid_t ->    (** The module index for all the symbols of the
                                  assemblies. *)
  ?env:Flx_types.env_t ->     (** Optionally specify the environment. *)
  Flx_mtypes2.sym_state_t ->
  Flx_types.fully_bound_symbol_table_t ->
  bind_state_t

(** Bind an individual assembly into a series of symbols. *)
val bind_asm:
  bind_state_t ->           (** The state needed for binding. *)
  ('a -> bound_t -> 'a) ->  (** Fold this over each assembly. *)
  'a ->                     (** The initial value. *)
  Flx_types.asm_t ->        (** The assembly to bind. *)
  'a

(** Bind all the symbols. *)
val bind_asms:
  bind_state_t ->           (** The state needed for binding. *)
  Flx_types.asm_t list ->   (** All the assemblies to bind. *)
  unit
