(** bid_t is the bound symbol index type, which is used to uniquely identifies
 * the symbol. *)
type bid_t = int
let start_counter = 2000

let dummy_bid = 0

(** Create a set type for bound symbol indices. *)
module BidSet = Flx_set.Make (
  struct
    type t = bid_t
    let compare = compare
  end
)

(** Convert a list of bids into a bid set. *)
let bidset_of_list ii =
  List.fold_left (fun ii i -> BidSet.add i ii) BidSet.empty ii

let str_of_bidset x = 
  let elts = ref [] in
  BidSet.iter (fun i -> elts := i::!elts) x;
  String.concat "," (List.map string_of_int (!elts))


let fresh_bid counter =
  let bid = !counter in
  incr counter;
  bid

let iter_bids f start_bid end_bid =
  for bid = start_bid to end_bid do
    f bid
  done

let string_of_bid x = string_of_int x

