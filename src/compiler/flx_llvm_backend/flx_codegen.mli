type codegen_state_t

val make_codegen_state:
  Flx_mtypes2.sym_state_t ->
  Flx_types.fully_bound_symbol_table_t ->
  Llvm.llcontext ->
  Llvm.llmodule ->
  Llvm.llbuilder ->
  codegen_state_t

val codegen_bexe:
  codegen_state_t ->
  Flx_types.bexe_t ->
  Llvm.llvalue option

val codegen_symbol:
  codegen_state_t ->
  Flx_ast.bid_t ->
  Flx_types.symbol_data3_t ->
  unit
