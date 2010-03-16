type codegen_state_t

val make_codegen_state:
  Flx_mtypes2.sym_state_t ->
  int ->                      (** Optimization level. *)
  codegen_state_t

(** Generate code for the given symbols and exes. *)
val codegen:
  codegen_state_t ->        (** The state needed for code generation. *)
  Flx_bsym_table.t ->       (** The bound symbol table. *)
  Flx_types.bid_t list ->   (** The symbols to generate. *)
  Flx_bexe.t list ->        (** The executables to generate. *)
  Llvm.llvalue option

(** Generate code for the given symbols and exes. *)
val codegen_and_run:
  codegen_state_t ->        (** The state needed for code generation. *)
  Flx_bsym_table.t ->       (** The bound symbol table. *)
  Flx_types.bid_t list ->   (** The symbols to generate. *)
  Flx_bexe.t list ->        (** The executables to generate. *)
  Llvm.llvalue option
