(** Type generator *)

open Flx_types
open Flx_mtypes2

val gen_types :
  sym_state_t ->
  bsym_table_t ->
  (bid_t * btypecode_t) list -> string

val gen_type_names :
  sym_state_t ->
  bsym_table_t ->
  (bid_t * btypecode_t) list -> string
