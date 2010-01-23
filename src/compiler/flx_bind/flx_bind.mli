type bind_state_t

type bound_t =
  | Bound_exe of Flx_bexe.t
  | Bound_symbol of (Flx_types.bid_t * Flx_bsym.t)

(** Constructs the bind state needed for a batch compiler. *)
val make_bind_state:
  Flx_mtypes2.sym_state_t ->  (** The symbol state *)
  bind_state_t

(** Constructs the bind state needed for an interactive toplevel compiler. *)
val make_toplevel_bind_state:
  Flx_mtypes2.sym_state_t ->  (** The symbol state *)
  bind_state_t *
  Flx_bsym_table.t

(** Bind an individual assembly into a series of symbols. *)
val bind_asm:
  bind_state_t ->           (** The state needed for binding. *)
  Flx_bsym_table.t ->       (* The bound symbol table. *)
  ('a -> bound_t -> 'a) ->  (** Fold this over each assembly. *)
  'a ->                     (** The initial value. *)
  Flx_types.asm_t ->        (** The assembly to bind. *)
  'a

(** Bind all the symbols. *)
val bind_asms:
  bind_state_t ->         (** The state needed for binding. *)
  Flx_bsym_table.t ->     (** They bound symbol table. *)
  Flx_types.asm_t list -> (** All the assemblies to bind. *)
  unit

(** Find the root module's init function index. *)
val find_root_module_init_function:
  bind_state_t ->     (** The state needed for binding. *)
  Flx_types.bid_t ->  (** The root module index. *)
  Flx_types.bid_t     (** The root module index function. *)
