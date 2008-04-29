(* Typeclass checker *)

open Flx_ast
open Flx_types
open Flx_mtypes1
open Flx_mtypes2
open Flx_child

val typeclass_instance_check:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  child_map_t ->
  unit

val fixup_typeclass_instance:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  btypecode_t list ->
  int * btypecode_t list

val maybe_fixup_typeclass_instance:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  int ->
  btypecode_t list ->
  int * btypecode_t list

val fixup_typeclass_instances:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  unit

val tcinst_chk:
  sym_state_t ->
  bool ->
  int ->
  btypecode_t list ->
  range_srcref ->
  bvs_t * btypecode_t * btypecode_t list * int ->
  (int * btypecode_t list) option
