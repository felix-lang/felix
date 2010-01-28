open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_child

type aentry_t =
  bid_t *
  (string * Flx_btype.t * Flx_bexpr.t * BidSet.t)

val passign:
  sym_state_t ->
  Flx_bsym_table.t ->
  aentry_t list ->                (* list of assignments to fix *)
  Flx_btype.t list ->             (* ts to use when creating temporaries *)
  Flx_srcref.t ->                 (* source ref *)
  (Flx_btype.t * bid_t) list *
  Flx_bexe.t list
