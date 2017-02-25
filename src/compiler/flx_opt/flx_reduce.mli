(** Reductions *)

(*
val remove_useless_reductions:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_mtypes2.reduction_t list ->
  Flx_mtypes2.reduction_t list
*)

val reduce_exes:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_mtypes2.reduction_t list ->
  Flx_bexe.t list ->
  Flx_bexe.t list

val reduce_all:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  unit

