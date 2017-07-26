open Flx_types
open Flx_mtypes2
open Flx_bid

val cpp_name :
  Flx_bsym_table.t ->
  bid_t ->
  string

val cpp_instance_name :
  sym_state_t ->
  Flx_bsym_table.t ->
  bid_t ->
  Flx_btype.t list ->
  string

val cpp_type_classname :
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  string

val cpp_structure_name:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  string

val cpp_typename :
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  string


val cpp_ltypename :
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  string


(** mangle a Felix identifier to a C one *)
val plain_cid_of_flxid:
 string -> string

(** mangle a Felix identifier to a C one, with hash if too long *)
val cid_of_flxid:
 string -> string


(** mangle a Felix bound symbol index to a C one. *)
val cid_of_bid: bid_t -> string
