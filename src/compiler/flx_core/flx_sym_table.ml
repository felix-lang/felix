(** The type of the symbol table. *)
type t = (Flx_types.bid_t, Flx_types.sym_t) Hashtbl.t

(** Construct a symbol table. *)
let create () = Hashtbl.create 97

(** Adds the symbol with the bound index to the symbol table. *)
let add = Hashtbl.replace

(** Returns if the bound index is in the symbol table. *)
let mem = Hashtbl.mem

(** Searches the symbol table for the given symbol. *)
let find = Hashtbl.find

(** Iterate over all the items in the symbol table. *)
let iter = Hashtbl.iter
