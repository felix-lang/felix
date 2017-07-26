open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_bid

val gen_ctor:
  sym_state_t ->
  Flx_bsym_table.t ->
  string ->                     (* name *)
  (bid_t * int) list ->         (* display *)
  (bid_t * Flx_btype.t) list -> (* funs *)
  (string * string) list ->     (* extra args *)
  string list ->                (* extra inits *)
  Flx_btype.t list ->           (* ts *)
  property_t list ->            (* properties *)
  string
