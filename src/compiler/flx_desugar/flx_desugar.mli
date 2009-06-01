type desugar_state_t

(** Construct a desugar state value needed for desugaring. *)
val make_desugar_state:
  string ->                   (** A unique prefix for new values. *)
  Flx_mtypes2.sym_state_t ->  (** A symbol table. *)
  desugar_state_t

(** Desugar all the statements in a compilation unit. *)
val desugar_compilation_unit:
  desugar_state_t ->            (** The state needed for desugaring. *)
  Flx_ast.compilation_unit_t -> (** The compilation unit. *)
  Flx_types.asm_t list

(** Desguar a statement. *)
val desugar_statement:
  desugar_state_t ->                (** The state needed for desugaring. *)
  (Flx_types.asm_t -> 'a -> 'a) ->  (** Fold this over each assembly. *)
  Flx_ast.statement_t ->            (** A statement. *)
  'a ->                             (** The initial value. *)
  'a
