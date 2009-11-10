(** The type of the bound symbol table. *)
type t

(** Construct a bound symbol table. *)
val create : unit -> t

(** Adds the bound symbol with the index to the symbol table. *)
val add : t -> Flx_types.bid_t -> Flx_types.bsym_t -> unit

(** Returns if the bound index is in the bound symbol table. *)
val mem : t -> Flx_types.bid_t -> bool

(** Searches the bound symbol table for the given symbol. *)
val find : t -> Flx_types.bid_t -> Flx_types.bsym_t

(** Remove a binding from the bound symbol table. *)
val remove : t -> Flx_types.bid_t -> unit

(** Iterate over all the items in the bound symbol table. *)
val iter : (Flx_types.bid_t -> Flx_types.bsym_t -> unit) -> t -> unit
