type bind_state_t

val make_bind_state: Flx_mtypes2.sym_state_t -> bind_state_t

(** Bind an individual assembly into a series of symbols. *)
val bind_asm:
  ?parent:int ->          (** The parent symbol for all the symbols of the
                              assemblies. *)
  bind_state_t ->         (** The state needed for binding. *)
  (int -> Flx_types.symbol_data3_t -> 'a -> 'a) ->  (** Fold this over each
                                                        assembly. *)
  Flx_types.asm_t ->      (** The assembly to bind. *)
  'a ->                   (** The initial value. *)
  'a

(** Bind all the symbols. *)
val bind_asms:
  bind_state_t ->           (** The state needed for binding. *)
  Flx_types.asm_t list ->   (** All the assemblies to bind. *)
  Flx_types.fully_bound_symbol_table_t
