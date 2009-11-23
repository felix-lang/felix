open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_child

type aentry_t =
  bid_t *
  (string * btypecode_t * tbexpr_t * BidSet.t)

val passign:
  sym_state_t ->
  Flx_sym_table.t ->
  Flx_bsym_table.t ->
  aentry_t list ->                (* list of assignments to fix *)
  btypecode_t list ->             (* ts to use when creating temporaries *)
  Flx_srcref.t ->                 (* source ref *)
  (btypecode_t * bid_t) list *
  bexe_t list
