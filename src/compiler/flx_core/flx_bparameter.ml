open Flx_bid
type t = {
  pid: string;
  pindex: bid_t;
  pkind: Flx_ast.param_kind_t;
  ptyp: Flx_btype.t
}

