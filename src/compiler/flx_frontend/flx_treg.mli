(** The type registry
 *
 * Flx supports algebraic type expressions. For some of these, we need to
 * construct a C++ class type. To ensure the same name is provided for each
 * such class, we use a registry which maps the (bound) type expression to an
 * integer.
 *
 * The routine register_type_rn is a non-recursive registration procedure.
 *
 * The routine register_type_r registers types recursively.  The 'exclude'
 * argument is a list of types which should not be registered, this is used to
 * break potential infinite recursions. Note carefully that components are
 * always registered before the type, so that they'll be defined before
 * they're used. There two exceptions: for a pointer an incomplete type is
 * sufficient, and sometimes necessary to break type recursion; and, unions may
 * be recursive, but are represented by pointers anyhow.
 *
 * Note that a function may accept an argument tuple one of whose arguments is
 * a pointer to a function of the same type.
 *
 * Note that the types of implicitly declared tuples will be caught here, since
 * the only thing you can do with a tuple is make it the argument of a
 * function. *)

open Flx_types
open Flx_ast
open Flx_mtypes2

val register_type_nr:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  unit

val register_tuple:
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t ->
  unit

val register_type_r:
  (bid_t -> Flx_btype.t list -> unit) ->
  sym_state_t ->
  Flx_bsym_table.t ->
  Flx_btype.t list ->
  Flx_srcref.t ->
  Flx_btype.t ->
  unit
