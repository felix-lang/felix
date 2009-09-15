type codegen_state_t

val make_codegen_state:
  Flx_mtypes2.sym_state_t ->
  Flx_types.fully_bound_symbol_table_t ->
  Llvm.llcontext ->
  Llvm.llmodule ->
  [`Function] Llvm.PassManager.t ->
  Llvm_executionengine.ExecutionEngine.t ->
  codegen_state_t

(** Generate code for a statement. *)
val codegen_bexe:
  codegen_state_t ->    (** The state needed for code generation. *)
  Llvm.llbuilder ->     (** The function builder. *)
  Flx_types.bexe_t ->   (** The statement to generate. *)
  unit

(** Generate code for a symbol. *)
val codegen_symbol:
  codegen_state_t ->          (** The state needed for code generation. *)
  Flx_ast.bid_t ->            (** The index of the symbol. *)
  Flx_types.symbol_data3_t -> (** The symbol data. *)
  unit
