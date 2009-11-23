(* Typeclass checker *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val typeclass_instance_check_symbols:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_child.t ->
  Flx_types.bid_t list -> (** The list of symbols to check. *)
  Flx_types.bid_t list

val typeclass_instance_check:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_child.t ->
  unit

val fixup_typeclass_instance:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  btypecode_t list ->
  bid_t * btypecode_t list

val maybe_fixup_typeclass_instance:
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  btypecode_t list ->
  bid_t * btypecode_t list

val fixup_typeclass_instances:
  sym_state_t ->
  Flx_bsym_table.t ->
  unit

val tcinst_chk:
  sym_state_t ->
  Flx_bsym_table.t ->
  bool ->
  bid_t ->
  btypecode_t list ->
  Flx_srcref.t ->
  bvs_t * btypecode_t * btypecode_t list * bid_t ->
  (bid_t * btypecode_t list) option
