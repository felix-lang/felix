(** The type of the symbol table. *)
type t = (Flx_types.bid_t, Flx_sym.t) Hashtbl.t

(** Construct a symbol table. *)
let create () = Hashtbl.create 97

(** Adds the symbol with the bound index to the symbol table. *)
let add = Hashtbl.replace

(** Returns if the bound index is in the symbol table. *)
let mem = Hashtbl.mem

(** Searches the symbol table for the given symbol. *)
let find = Hashtbl.find

(** Searches the symbol table for the given symbol's parent. *)
let find_with_parent sym_table bid =
  let sym = find sym_table bid in
  sym.Flx_sym.parent, sym

(** Searches the bound symbol table for the given symbol's parent. *)
let find_parent sym_table bid = (find sym_table bid).Flx_sym.parent

(** Remove a binding from the bound symbol table. *)
let remove = Hashtbl.remove

(** Iterate over all the items in the symbol table. *)
let iter f sym_table =
  Hashtbl.iter (fun bid sym -> f bid sym.Flx_sym.parent sym) sym_table

(** Fold over all the items in the bound symbol table. *)
let fold f sym_table init =
  Hashtbl.fold
    (fun bid sym init -> f bid sym.Flx_sym.parent sym init)
    sym_table
    init
