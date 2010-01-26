type t = {
  pid: string;
  pindex: Flx_types.bid_t;
  pkind: Flx_ast.param_kind_t;
  ptyp: Flx_types.btypecode_t
}

(** Returns the names from a list of bparameters. *)
let get_names ps =
  List.map (fun p -> p.pid) ps

(** Returns the bids from a list of bparameters. *)
let get_bids ps =
  List.map (fun p -> p.pindex) ps

(** Returns the bound types from a list of bparameters. *)
let get_btypes ps =
  List.map begin fun p ->
    match p.pkind with
    | `PFun ->
        Flx_types.btyp_function (Flx_types.btyp_tuple [], p.ptyp)
    | _ -> p.ptyp
  end ps

(** Prints a bparameter to a formatter. *)
let print f p =
  Flx_format.print_record4 f
    "pkind" Flx_ast.print_param_kind p.pkind
    "pid" Flx_format.print_string p.pid
    "pindex" Flx_types.print_bid p.pindex
    "ptyp" Flx_types.print_btype p.ptyp
