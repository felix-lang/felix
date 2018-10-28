(** Reductions *)

(*
val remove_useless_reductions:
  Flx_mtypes2.sym_state_t ->
  Flx_bsym_table.t ->
  Flx_mtypes2.reduction_t list ->
  Flx_mtypes2.reduction_t list
*)

val reduce_exes:
  int ref -> (* reduction count *)
  int ref -> (* system counter *)
  Flx_bsym_table.t ->
  Flx_mtypes2.reduction_t list ->
  Flx_bexe.t list ->
  Flx_bexe.t list

val reduce_all:
  int ref ->
  Flx_bsym_table.t ->
  unit

val filter_viable_reductions:
  Flx_bsym_table.t ->
  Flx_mtypes2.reduction_t list ->
  Flx_mtypes2.reduction_t list

