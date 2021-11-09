open Flx_btype
open Flx_bid

(* for diagnostics *)
open Flx_bsym
open Flx_bsym_table


(* FAIRY VARIABLES.

  Consider a variable of tuple type, the components of which
  may also be tuples. A non-tuple component is called a leaf.

  A variable of this kind is considered as a separate set of fairy
  variables, one for each leaf.

  An access to any component, is an access to its children, recursively,
  that is, all its descendant leaves.

  With these rules, we can eliminated tuples from uniqueness analysis,
  and consider only leaves. 
*)




(* A path item is either the final Uniq or share component, a tuple component index
or a record name. 
*)

type item_t = [`Uniq | `Share | `Tup of int | `Rec  of string | `Struct of string ]

(* A path is just an item list, ending in Uniq or Share *)
type path_t = item_t list 

(* and paths are just a list of paths .. *)
type paths_t = path_t list 

(* a chain is an actual variable follow by a path *)
type chain_t = bid_t * path_t 

(* The chain2ix datum is the actual variable index and
a path to a uniq or share component, followed by the identifying fairy variable index
*)
type chain2ix_t = (chain_t * bid_t) list

(* and the idx to chain is the inverse *)
type ix2chain_t = (bid_t * chain_t) list

(* Pretty printintg paths *)
let string_of_item item = match item with
  | `Uniq -> ":U"
  | `Share -> ":S"
  | `Tup i -> "." ^ string_of_int i
  | `Rec s -> "." ^ s 
  | `Struct s -> "." ^ s 

let string_of_path (p:path_t) =
  List.fold_left (fun acc item -> acc ^ string_of_item item) "" p

(* Find the chain of a given fairy variable *)
let find_chain ix2chain i = List.assoc i ix2chain 

(* Find the actual variable of a given fairy variable *)
let find_var ix2chain i =
  let v,prjs = find_chain ix2chain i in
  v

let string_of_vars bsym_table ix2chain bidset =
  "("^ String.concat "," (
  BidSet.fold (fun yidx acc -> 
    let idx,prjs = find_chain ix2chain yidx in
    let parent, bsym = Flx_bsym_table.find_with_parent bsym_table idx in
    let chain = match prjs with | [] -> "" | _ -> string_of_path prjs in
    (string_of_int yidx ^ ":->" ^Flx_bsym.id bsym ^ chain) :: acc
  ) 
  bidset
  []) ^ ")"

let def_of_vars bsym_table ix2chain bidset =
  let chains = string_of_vars bsym_table ix2chain bidset in
  let vars = 
    BidSet.fold (fun yidx acc ->
      let idx = find_var ix2chain yidx in
      BidSet.add idx acc
    ) BidSet.empty bidset
  in
  let varlocs = 
    BidSet.fold (fun yidx acc -> 
      let idx = find_var ix2chain yidx in
      let parent, bsym = Flx_bsym_table.find_with_parent bsym_table idx in
      acc ^
      "Variable " ^ Flx_bsym.id bsym ^ 
        " defined at\n" ^ Flx_srcref.long_string_of_src (Flx_bsym.sr bsym)
      ^ "\n"
    ) 
    vars 
    ""
  in chains ^ "\n" ^ varlocs

let id_of_index bsym_table index = 
  Flx_bsym.id (Flx_bsym_table.find bsym_table index)

let str_of_ix2chain_entry bsym_table (fairy, (variable, prjs)) =
  string_of_int fairy ^ " -> " ^ id_of_index bsym_table variable ^
  "<" ^ string_of_int variable ^ ">" ^ string_of_path prjs

let print_ix2chain bsym_table ix2chain =
  List.iter 
    (fun entry -> print_endline ("  " ^ str_of_ix2chain_entry bsym_table entry)) 
  ix2chain

(* this function analyses a type and returns a list of 
symbolic projection chains which reach a uniq component:
the lists are reversed from the order in which they
must be provided. It delves into uniqs which are products
too even though such projections are not allowed at the moment.
We might actually consider the coerion from uniq T -> T to be
a special kind of projection and allow it later.
*)

let rec uniq_anal_aux (path:path_t) typ (paths:paths_t): paths_t = 
  match typ with
  | BTYP_uniq t -> (* no index to continue *)
    let path = `Uniq :: path in
    uniq_anal_aux path  t (path::paths)

  | BTYP_tuple ts ->
    List.fold_left2 (fun acc t n -> 
      let path = `Tup n :: path in
      uniq_anal_aux path t acc
    )
    paths 
    ts (Flx_list.nlist (List.length ts))

  | BTYP_array (t,BTYP_unitsum n) -> 
     if n > 20 then begin (* 
        print_endline ("Flx_fairy: Array size "^string_of_int n^" too big for fairy variable construction, ignoring");
        *)
        paths
     end
     else List.fold_left (fun acc n -> 
       let path = `Tup n :: path in
       uniq_anal_aux path t acc
    )
    paths
    (Flx_list.nlist n)
     
  | BTYP_record rs ->
    List.fold_left (fun acc (s,t) ->
      let path = `Rec s :: path in
      uniq_anal_aux path t acc
    )
    paths
    rs

  (* Note: the path is dropped! *)
  | _ -> paths

(* shared variable analysis of products, it is the same as the
uniq_anal except the roles of the default non-product and uniq type 
constructor are reversed
*)

let rec shared_anal_aux (path:path_t) typ (paths:paths_t): paths_t = 
print_endline ("Shared anal aux " ^ Flx_btype.str_of_btype typ);
  match typ with
  | BTYP_uniq t -> paths (* the path is dropped *)

  | BTYP_tuple ts ->
    List.fold_left2 (fun acc t n -> 
      let path = `Tup n :: path in
      shared_anal_aux path t acc
    )
    paths 
    ts (Flx_list.nlist (List.length ts))

  | BTYP_array (t,BTYP_unitsum n) -> 
     if n > 20 then begin (* 
        print_endline ("Flx_fairy: Array size "^string_of_int n^" too big for fairy variable construction, ignoring");
        *)
        paths
     end
     else List.fold_left (fun acc n -> 
       let path = `Tup n :: path in
       shared_anal_aux path t acc
    )
    paths
    (Flx_list.nlist n)
     
  | BTYP_record rs ->
    List.fold_left (fun acc (s,t) ->
      let path = `Rec s :: path in
      shared_anal_aux path t acc
    )
    paths
    rs

  | t ->
    let path = `Share :: path in 
    path :: paths
 


(* this routine reverses the paths so they're in the correct order.
It also adds every extension of the reversed paths to the set, uniquely.
The U is lost.  For example if we have paths

  .1

then we might get back

  .1.2.4
  .1.2
  .1

Any projection chain from any variable of the given type,
including the empty chain, which matches any of these
chains, is either targeting a uniq component, or a component
containing one, directly or indirectly.
*)
let uniq_anal typ : paths_t =
  let rps = uniq_anal_aux [] typ [] in
  List.map (fun lst -> List.rev (List.tl lst)) rps

let shared_anal typ : paths_t =
  let rps = shared_anal_aux [] typ [] in
  List.map (fun lst -> List.rev (List.tl lst)) rps


let find_entries bsym_table (chain2ix:chain2ix_t) bid : chain2ix_t = 
  let paths = List.filter (fun ((bid2,path),ix) -> bid2 = bid) chain2ix in
  paths

  (* the chain2ix is required when analysing expressions to determine
which variables or projections of variables are unique, we match
the left term of the current projection chain against the list
of all chains, if there's a match the associated synthesised index
is what we track.

The ix2chain is a way to get back to the original code in case
we have an error we have to report to the user.

Now, when we analyse an term there are two cases of interest:

(a) its a plain variable
(b) its the application of a projection to an argument

The third case, neither of the above, just maps our analyser over
that term with a fresh (empty) path.

In case b, we find the last projection in a chain first,
and the final variable last, because application is forward
polish notation, but chains are reverse polish. So in order
to successively pick match our projection path, the index has
to store the path in reverse order, with the variable last,
and the innermost projection first.

*)

let build_once_maps bsym_table counter vidx typ : chain2ix_t * ix2chain_t =
  let paths = uniq_anal typ in
  let uxs = List.map (fun _ -> Flx_bid.fresh_bid counter) paths in
  let chain2ix = List.map2 (fun path idx -> (vidx,path),idx) paths uxs in
  let ix2chain = List.map2 (fun path idx -> idx,(vidx,path)) paths uxs in
  chain2ix,ix2chain


let build_shared_maps bsym_table counter vidx typ : chain2ix_t * ix2chain_t =
(*
print_endline ("Building shared maps for " ^string_of_int vidx ^ " type " ^ Flx_print.sbt bsym_table typ);
*)
  let paths = shared_anal typ in
  let uxs = List.map (fun _ -> Flx_bid.fresh_bid counter) paths in
  let chain2ix = List.map2 (fun path idx -> (vidx,path),idx) paths uxs in
  let ix2chain = List.map2 (fun path idx -> idx,(vidx,path)) paths uxs in
  chain2ix,ix2chain




