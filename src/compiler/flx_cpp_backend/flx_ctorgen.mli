open Flx_ast
open Flx_types
open Flx_mtypes2

val gen_ctor:
  sym_state_t ->
  bsym_table_t ->
  string ->                   (* name *)
  (int * int) list ->         (* display *)
  (int * btypecode_t) list -> (* funs *)
  (string * string) list ->   (* extra args *)
  string list ->              (* extra inits *)
  btypecode_t list ->         (* ts *)
  property_t list ->          (* properties *)
  string
