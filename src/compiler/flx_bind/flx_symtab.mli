(** Build symbol tables
 *
 * Name binding, pass 1.
 *
 * This module is responsible for converting the AST into a symbol table, type
 * 1.  This table represents the raw information, nesting structure, and
 * associates each entity with a unique index.
 *
 * Types, expressions, and bodies of functions remain unbound. *)

type t


(** Create the state needed for the symbol table. *)
val make: Flx_mtypes2.sym_state_t -> t


(** Add assemblies to the symbol table. *)
val add_asms:
  t ->                        (** the symbol table *)
  int ->                      (** root index *)
  Flx_types.asm_t list ->     (** the assemblies *)
  (
    Flx_ast.sexe_t list *     (** executables *)
    (Flx_srcref.t * Flx_types.iface_t * int option) list  (** interfaces *)
  )
