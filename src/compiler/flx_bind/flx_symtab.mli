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

val summary: t -> string (* debugging info *)
val detail : t -> string (* debugging info *)

(** Create the state needed for the symbol table. *)
val make:  Flx_sym_table.t -> t

val get_init_exes : t -> Flx_ast.sexe_t list
val get_exports : t -> Flx_types.bound_iface_t list
val get_directives : t -> Flx_types.sdir_t list

(*
(** Add interface to the symbol table. *)
val add_iface:
  t ->                    (** symbol table *)
  Flx_types.siface_t ->   (** interface *)
  Flx_types.bound_iface_t (** bound interface *)

*)

(** Add declaration to the symbol table. *)
val add_dcl:
  ?parent:Flx_types.bid_t ->          (** optional parent index symbol *)
  bool ->                             (** print flag *)
  Flx_types.bid_t ref ->                        (** fresh bid counter *)
  t ->                                (** symbol table *)
  Flx_types.sdcl_t ->                 (** the decl to add *)
  Flx_types.bid_t * Flx_types.bound_iface_t list  (** symbol index and bound
                                                      interfaces inside the
                                                      dcl *)

(** Add assemblies to the symbol table. *)
val add_asms:
  bool ->                         (** print flag *)
  Flx_types.bid_t ref ->          (** counter *)
  t ->                            (** symbol table *)
  string ->                       (** containing module name *)
  int ->                          (** nesting level *)
  Flx_types.bid_t option ->       (** parent *)
  Flx_types.bid_t ->              (** index of root module *)
  Flx_types.asm_t list ->         (** assemblies *)
  unit

val top_partial_symtab_create_from_asms:
  bool ->                         (** print flag *)
  Flx_types.bid_t ref ->          (** counter *)
  string ->                       (** containing module name *)
  Flx_types.asm_t list ->         (** assemblies *)
  t                               (** new table *)

val merge_top_partial_tables:
 t list -> t


