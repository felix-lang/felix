(** Build symbol tables
 *
 * Name binding, pass 1.
 *
 * This module is responsible for converting the AST into a symbol table, type
 * 1.  This table represents the raw information, nesting structure, and
 * associates each entity with a unique index.
 *
 * Types, expressions, and bodies of functions remain unbound. *)

open Flx_ast
open Flx_types
open Flx_mtypes2

val build_tables:
  sym_state_t ->
  string ->
  ivs_list_t ->
  int ->
  int option -> (* parent index *)
  int option -> (* grandparent index *)
  int -> (* root index *)
  bool -> (* true if parent is a class, false otherwise *)
  asm_t list ->
  (
    name_map_t *
    name_map_t *
    sexe_t list *
    (Flx_srcref.t * iface_t * int option) list *
    dir_t list
  )
