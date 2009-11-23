open Flx_ast
open Flx_types
open Flx_mtypes2

val gen_ctor:
  sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  string ->                     (* name *)
  (bid_t * int) list ->         (* display *)
  (bid_t * btypecode_t) list -> (* funs *)
  (string * string) list ->     (* extra args *)
  string list ->                (* extra inits *)
  btypecode_t list ->           (* ts *)
  property_t list ->            (* properties *)
  string
