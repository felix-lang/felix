open Flx_bid
open Flx_types
open Flx_mtypes2

type bbind_state_t = {
  counter: bid_t ref;
  print_flag: bool;
  sym_table: Flx_sym_table.t;
  ticache : (bid_t, Flx_btype.t) Hashtbl.t;
  varmap : typevarmap_t;
  axioms: axiom_t list ref;
  lookup_state: Flx_lookup_state.lookup_state_t;

  (* Used to cache which symbols we've already processed. *)
  mutable visited: (bid_t, unit) Hashtbl.t;
}

(** The state needed for binding. *)
let make_bbind_state 
  ~counter 
  ~print_flag 
  ~ticache 
  ~varmap 
  ~sym_table 
  ~axioms 
  ~lookup_state 
=
  {
    print_flag = print_flag;
    counter = counter;
    sym_table = sym_table;
    ticache=ticache;
    varmap=varmap;
    axioms = axioms;
    lookup_state = lookup_state;
    visited = Hashtbl.create 97;
  }

