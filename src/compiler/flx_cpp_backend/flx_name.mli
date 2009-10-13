open Flx_types
open Flx_mtypes2

val cpp_name :
  bsym_table_t ->
  bid_t ->
  string

val cpp_instance_name :
  sym_state_t ->
  bsym_table_t ->
  bid_t ->
  btypecode_t list ->
  string

val cpp_type_classname :
  sym_state_t ->
  btypecode_t ->
  string

val cpp_typename :
  sym_state_t ->
  btypecode_t ->
  string


val cpp_ltypename :
  sym_state_t ->
  btypecode_t ->
  string


(** mangle a Felix identifier to a C one *)
val cid_of_flxid:
 string-> string
