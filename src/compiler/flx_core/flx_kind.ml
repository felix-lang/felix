
type kind =
  | KIND_type
  | KIND_tuple of kind list
  | KIND_function of kind * kind

let kind_eq k1 k2 = k1 = k2

let rec sk k =
  match k with
  | KIND_type -> "TYPE"
  | KIND_tuple ks -> "(" ^ Flx_util.catmap ", " sk ks ^")"
  | KIND_function (d,c) -> sk d ^ " -> " ^ sk c

let kind_type = KIND_type
let kind_function (d, c) = KIND_function (d,c)
let kind_tuple ks = KIND_tuple ks

(* this probably doesn't belong here .. *)
type bv_t = string * Flx_bid.bid_t * kind
type bvs_t = bv_t list


