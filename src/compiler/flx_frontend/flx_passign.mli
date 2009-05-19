open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_call
open Flx_srcref
open Flx_child

type aentry_t =
  int *
  (string * btypecode_t * tbexpr_t * IntSet.t)

val passign:
  sym_state_t ->
  fully_bound_symbol_table_t ->
  aentry_t list ->                (* list of assignments to fix *)
  btypecode_t list ->             (* ts to use when creating temporaries *)
  Flx_srcref.t ->                 (* source ref *)
  (btypecode_t * int) list *
  bexe_t list
