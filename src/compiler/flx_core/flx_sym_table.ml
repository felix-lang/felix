type elt = {
  sym: Flx_sym.t;                 (** The symbol. *)
  parent: Flx_types.bid_t option; (** The parent of the symbol. *)
}

(** The type of the symbol table. *)
type t = (Flx_types.bid_t, elt) Hashtbl.t

(** debugging info *)
let summary x = 
  string_of_int (Hashtbl.length x) ^ " symbols"

let detail x = 
  let pd e = 
    (match e with | None -> " of root" | Some p -> " of parent " ^ string_of_int p) 
  in
  "symbols " ^
  Hashtbl.fold begin fun k v acc ->
    acc ^ "\n" ^
    "  " ^ string_of_int k ^ " -> " ^
    Flx_id.to_string v.sym.Flx_sym.id ^
    pd v.parent
  end x "" ^ 
  "\n"


(** Construct a symbol table. *)
let create () = Hashtbl.create 97

(** Adds the symbol with index bid to the symbol table. *)
let replace sym_table bid parent sym =
  Hashtbl.replace sym_table bid { parent=parent; sym=sym }

let add sym_table bid parent sym =
  if Hashtbl.mem sym_table bid
  then failwith ("Attempt to add existant index " ^ string_of_int bid ^
   " to symbol table")
  else Hashtbl.add sym_table bid { parent=parent; sym=sym }

(** Returns if the index is in the symbol table. *)
let mem = Hashtbl.mem

(** Helper function to find an elt in the table. *)
let find_elt sym_table = Hashtbl.find sym_table

(** Searches the symbol table for the given index. *)
let find sym_table bid = (find_elt sym_table bid).sym

(** Searches the symbol table for the index and returns string name. *)
let find_id sym_table bid = (find_elt sym_table bid).sym.Flx_sym.id

(** Searches the symbol table for the given symbol's parent. *)
let find_with_parent sym_table bid =
  let elt = find_elt sym_table bid in
  elt.parent, elt.sym

(** Searches the symbol table for the given symbol's parent. *)
let find_parent sym_table bid = (find_elt sym_table bid).parent

(** Remove a binding from the symbol table. *)
let remove = Hashtbl.remove

(** Iterate over all the items in the symbol table. *)
let iter f sym_table =
  Hashtbl.iter (fun bid elt -> f bid elt.parent elt.sym) sym_table

(** Fold over all the items in the symbol table. *)
let fold f sym_table init =
  Hashtbl.fold
    (fun bid elt init -> f bid elt.parent elt.sym init)
    sym_table
    init
