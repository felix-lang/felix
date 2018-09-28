open Flx_bid

type bexe_state_t = {
  counter: bid_t ref;
  sym_table: Flx_sym_table.t;
  lookup_state: Flx_lookup_state.lookup_state_t;
  env: Flx_mtypes2.env_t;
  id: string;
  parent: bid_t option;
  parent_vs: Flx_kind.bvs_t;
  mutable ret_type: Flx_btype.t;
  mutable reachable: bool;
  mutable return_count: int;
  mutable proc_return_count: int;
}

let make_bexe_state ?parent ?(env=[]) counter sym_table lookup_state parent_vs ret_type =
  let id =
    match parent with
    | None -> ""
    | Some index ->
        let symbol = Flx_sym_table.find sym_table index in
        symbol.Flx_sym.id
  in
  {
    counter = counter;
    sym_table = sym_table;
    lookup_state = lookup_state;
    env = env;
    id = id;
    parent = parent;
    parent_vs = parent_vs;
    ret_type = ret_type;
    reachable = true;
    return_count = 0;
    proc_return_count = 0;
  }



