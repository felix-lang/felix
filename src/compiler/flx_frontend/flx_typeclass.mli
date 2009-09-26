(* Typeclass checker *)

open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

val typeclass_instance_check_symbol:
  sym_state_t ->
  bsym_table_t ->
  Flx_child.t ->
  Flx_types.bid_t ->
  Flx_types.bsym_t ->
  unit

val typeclass_instance_check:
  sym_state_t ->
  bsym_table_t ->
  Flx_child.t ->
  unit

val fixup_typeclass_instance:
  sym_state_t ->
  bsym_table_t ->
  int ->
  btypecode_t list ->
  int * btypecode_t list

val maybe_fixup_typeclass_instance:
  sym_state_t ->
  bsym_table_t ->
  int ->
  btypecode_t list ->
  int * btypecode_t list

val fixup_typeclass_instances:
  sym_state_t ->
  bsym_table_t ->
  unit

val tcinst_chk:
  sym_state_t ->
  bool ->
  int ->
  btypecode_t list ->
  Flx_srcref.t ->
  bvs_t * btypecode_t * btypecode_t list * int ->
  (int * btypecode_t list) option
