open Flx_bid

(** The type of the (raw, unbound) symbol table. *)
type elt = {
  sym: Flx_sym.t;                 (** The symbol. *)
  parent: bid_t option; (** The parent of the symbol. *)
}

(** The type of the symbol table. *)
type t = (bid_t, elt) Hashtbl.t


(** quick debugging info *)
val summary: t -> string
val detail: t -> string

(** Construct a symbol table. *)
val create : unit -> t

(** add or replace the symbol with the index to the symbol table. *)
val replace : t -> bid_t -> bid_t option -> Flx_sym.t -> unit

(** As above but throws if the index is already in the table *)
val add: t -> bid_t -> bid_t option -> Flx_sym.t -> unit

(** Returns if the bound index is in the symbol table. *)
val mem : t -> bid_t -> bool

(** Searches the symbol table for the given symbol. Raises Not_found if the
 * index is not in the symbol table. *)
val find : t -> bid_t -> Flx_sym.t

(** find id by index, raises Not_found if missing  *)
val find_id : t -> bid_t -> Flx_id.t

(** Searches the bound symbol table for the given symbol. *)
val find_with_parent :
  t ->
  bid_t ->
  bid_t option * Flx_sym.t

(** Searches the symbol table for the given symbol's parent. *)
val find_parent : t -> bid_t -> bid_t option

(** Remove an entry from the symbol table. *)
val remove : t -> bid_t -> unit

(** Iterate over all the items in the symbol table. *)
val iter :
  (bid_t -> bid_t option -> Flx_sym.t -> unit) ->
  t ->
  unit

(** Fold over all the items in the symbol table. *)
val fold :
  (bid_t -> bid_t option -> Flx_sym.t -> 'a -> 'a) ->
  t ->
  'a ->
  'a

val find_children: t -> bid_t -> bid_t list
val find_descendants : t -> bid_t -> bid_t list
 
