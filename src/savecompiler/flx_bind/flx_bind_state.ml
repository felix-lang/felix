open Flx_sym
open Flx_ast
open Flx_bid

type bind_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  sym_table: Flx_sym_table.t;
  symtab: Flx_symtab.t;
  parent: bid_t option;
  bexe_state: Flx_bexe_state.bexe_state_t;
  lookup_state: Flx_lookup_state.lookup_state_t;
  bbind_state: Flx_bbind_state.bbind_state_t;
}

type bound_t =
  | Bound_exe of Flx_bexe.t
  | Bound_symbol of (bid_t * Flx_bsym.t)

(** Constructs the bind state needed for a batch compiler. *)
let make_bind_state syms sym_table =
  let lookup_state = 
    Flx_lookup_state.make_lookup_state 
      syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
      syms.Flx_mtypes2.counter 
      syms.Flx_mtypes2.varmap
      syms.Flx_mtypes2.ticache
      syms.Flx_mtypes2.generic_cache
      sym_table 
  in
  let bbind_state = Flx_bbind_state.make_bbind_state 
   ~print_flag:syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
   ~counter:syms.Flx_mtypes2.counter
   ~sym_table 
   ~lookup_state: lookup_state
   ~ticache: syms.Flx_mtypes2.ticache  
   ~varmap: syms.Flx_mtypes2.varmap
   ~axioms: syms.Flx_mtypes2.axioms
   ~reductions: syms.Flx_mtypes2.reductions
 in
  {
    syms = syms;
    sym_table = sym_table;
    symtab = Flx_symtab.make sym_table;
    parent = None;
    bexe_state = Flx_bexe_state.make_bexe_state
      syms.Flx_mtypes2.counter
      sym_table
      lookup_state
      []
      (Flx_btype.btyp_void ());
    lookup_state = lookup_state;
    bbind_state = bbind_state;
  }


let set_nominal_typedefs (state:bind_state_t) = state.lookup_state.Flx_lookup_state.treat_typedefs_as_structural <- false
let set_structural_typedefs (state:bind_state_t) = state.lookup_state.Flx_lookup_state.treat_typedefs_as_structural <-true 
let get_structural_typedefs (state:bind_state_t) = state.lookup_state.Flx_lookup_state.treat_typedefs_as_structural


