open Flx_bid
type lookup_state_t = {
  counter : bid_t ref;
  print_flag: bool;
  ticache : (bid_t, Flx_btype.t) Hashtbl.t;
  varmap: Flx_mtypes2.typevarmap_t; 
    (* used by unification to fix the return types of functions
     * MUST be a reference to the global one because that's used
     * in the front and back ends extensively..
     *)
  sym_table: Flx_sym_table.t;
  mutable env_cache: (bid_t, Flx_mtypes2.env_t) Hashtbl.t;
  generic_cache: Flx_mtypes2.generic_cache_t;
  mutable decoder_cache: ((int * Flx_btype.t list) * Flx_bexpr.t) list;
    (* assoc list of union index, ts pair -> decoder fun closure *)
  mutable encoder_cache: ((int * Flx_btype.t list) * Flx_bexpr.t) list;
    (* assoc list of union index, ts pair -> encoder fun closure *)
  mutable treat_typedefs_as_structural : bool;
}

let make_lookup_state print_flag counter varmap ticache generic_cache sym_table =
  {
    counter = counter;
    print_flag = print_flag;
    ticache = ticache; 
    varmap = varmap;
    sym_table = sym_table;
    env_cache = Hashtbl.create 97;
    generic_cache = generic_cache;
    decoder_cache = [];
    encoder_cache = [];
    treat_typedefs_as_structural = false;
  }

let set_nominal_typedefs (state:lookup_state_t) = state.treat_typedefs_as_structural <- false 
let set_structural_typedefs (state:lookup_state_t) = state.treat_typedefs_as_structural <- true 
let get_structural_typedefs (state:lookup_state_t) = state.treat_typedefs_as_structural

let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_lookup Flx_sym_table.find failed " ^ msg);
    raise Not_found

let get_data table index =
  try Flx_sym_table.find table index
  with Not_found ->
    failwith ("[Flx_lookup.get_data] No definition of <" ^
      Flx_print.string_of_bid index ^ ">")

let get_varmap state = state.varmap


let rsground : Flx_types.recstop = {
  Flx_types.constraint_overload_trail = [];
  idx_fixlist = [];
  type_alias_fixlist = [];
  as_fixlist = [];
  expr_fixlist = [];
  depth = 0;
  open_excludes = [];
  strr_limit = 5;
}



