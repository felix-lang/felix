(** Generic support *)

open Flx_types
open Flx_mtypes2
open Flx_ast

val find_split_vs:
  sym_state_t ->
  int ->
  plain_ivs_list_t *
  plain_ivs_list_t *
  vs_aux_t

val find_vs:
  sym_state_t ->
  int ->
  ivs_list_t

val adjust_ts:
  sym_state_t ->
  Flx_srcref.t ->
  int ->
  btypecode_t list ->
  btypecode_t list

val make_params:
  sym_state_t ->
  Flx_srcref.t ->
  int ->
  btypecode_t list ->
  (string * btypecode_t) list

val make_varmap:
  sym_state_t ->
  Flx_srcref.t ->
  int ->
  btypecode_t list ->
  (int, btypecode_t) Hashtbl.t
