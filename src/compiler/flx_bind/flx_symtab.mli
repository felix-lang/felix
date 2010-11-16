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
val make:  Flx_sym_table.t -> t

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
  (
    Flx_ast.sexe_t list *         (** executables *)
    Flx_types.bound_iface_t list  (** interfaces *)
  )

