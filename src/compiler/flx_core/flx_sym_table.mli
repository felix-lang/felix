(** The type of the symbol table. *)
type t

(** Construct a symbol table. *)
val create : unit -> t

(** Adds the symbol with the bound index to the symbol table. *)
val add : t -> Flx_types.bid_t -> Flx_types.sym_t -> unit

(** Returns if the bound index is in the symbol table. *)
val mem : t -> Flx_types.bid_t -> bool

(** Searches the symbol table for the given symbol. Raises Not_found if the
 * index is not in the symbol table. *)
val find : t -> Flx_types.bid_t -> Flx_types.sym_t

(** Remove a binding from the bound symbol table. *)
val remove : t -> Flx_types.bid_t -> unit

(** Iterate over all the items in the symbol table. *)
val iter : (Flx_types.bid_t -> Flx_types.sym_t -> unit) -> t -> unit
