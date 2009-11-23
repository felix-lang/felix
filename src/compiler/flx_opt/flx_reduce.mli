(** Reductions *)

val remove_useless_reductions:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.reduction_t list ->
  Flx_types.reduction_t list

val reduce_exes:
  Flx_mtypes2.sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  Flx_types.reduction_t list ->
  Flx_types.bexe_t list ->
  Flx_types.bexe_t list
