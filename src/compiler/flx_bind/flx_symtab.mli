(** Build symbol tables
 *
 * Name binding, pass 1.
 *
 * This module is responsible for converting the AST into a symbol table, type
 * 1.  This table represents the raw information, nesting structure, and
 * associates each entity with a unique index.
 *
 * Types, expressions, and bodies of functions remain unbound. *)

val build_tables:
  Flx_mtypes2.sym_state_t ->
  int ->                      (* root index *)
  Flx_types.asm_t list ->
  (
    Flx_types.name_map_t *
    Flx_types.name_map_t *
    Flx_ast.sexe_t list *
    (Flx_srcref.t * Flx_types.iface_t * int option) list *
    Flx_types.dir_t list
  )
