(** The type of the bound symbol table. *)
type t = (Flx_types.bid_t, Flx_types.bsym_t) Hashtbl.t

(** Construct a bound symbol table. *)
let create () = Hashtbl.create 97

(** Adds the bound symbol with the index to the symbol table. *)
let add = Hashtbl.replace

(** Returns if the bound index is in the bound symbol table. *)
let mem = Hashtbl.mem

(** Searches the bound symbol table for the given symbol. *)
let find = Hashtbl.find

(** Remove a binding from the bound symbol table. *)
let remove = Hashtbl.remove

(** Iterate over all the items in the bound symbol table. *)
let iter = Hashtbl.iter
