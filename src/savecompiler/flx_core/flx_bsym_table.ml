open Flx_bid
exception IncompleteBsymTable of int * int * string

type elt = {
  bsym: Flx_bsym.t;               (** The symbol. *)
  parent: bid_t option; (** The parent of the symbol. *)
}

(** The type of the bound symbol table. *)
type t = {
  table: (bid_t, elt) Hashtbl.t;
  childmap: (int, BidSet.t) Hashtbl.t; (** All of this bsym table's roots. *)
  mutable subtype_map: ((int * int) * int) list; (* super=cod:param, sub=dom:arg -> coercion *)
}

(** Construct a bound symbol table. *)
let create_fresh () =
  { table=Hashtbl.create 97;
    childmap=Hashtbl.create 97; 
    subtype_map = [];
  }

let create_from bsym_table =
  { table=Hashtbl.create 97;
    childmap=Hashtbl.create 97; 
    subtype_map = bsym_table.subtype_map
  }


type coercion_t = (bid_t * bid_t) * bid_t

(* temporary hackery *)
let add_supertype bsym_table x =
  bsym_table.subtype_map <- x :: bsym_table.subtype_map

let maybe_coercion bsym_table param arg  = 
  try Some (List.assoc (param,arg) bsym_table.subtype_map)
  with Not_found -> None

let is_supertype bsym_table param arg =
  List.mem_assoc (param, arg) bsym_table.subtype_map

let iter_coercions bsym_table f =
  List.iter f bsym_table.subtype_map 

let fold_coercions bsym_table f init =
  List.fold_left f init bsym_table.subtype_map 


(** Returns how many items are in the bound symbol table. *)
let length bsym_table = Hashtbl.length bsym_table.table

(** Returns if the bound index is in the bound symbol table. *)
let mem bsym_table = Hashtbl.mem bsym_table.table

(** Helper function to find an elt in the table. *)
let find_elt bsym_table bid = 
  try Hashtbl.find bsym_table.table bid
  with Not_found -> 
    (*
    print_endline ("[Flx_bsym_table:find_elt] Symbol " ^string_of_int bid^ " not found in (bound) bsym_table");
    *)
    raise Not_found

(** Searches the bound symbol table for the given symbol. *)
let find bsym_table bid = (find_elt bsym_table bid).bsym

(** Searches the bound symbol table for the given symbol. *)
let find_with_parent bsym_table bid =
  let elt = find_elt bsym_table bid in
  elt.parent, elt.bsym

(** Searches the bound symbol table for the given symbol's parent. *)
let find_parent bsym_table bid = (find_elt bsym_table bid).parent

(** Searches the bound symbol table for the given symbol's children. *)
let find_children bsym_table bid = 
   try Hashtbl.find bsym_table.childmap bid
   with Not_found -> BidSet.empty

(** Finds all the descendants of the given symbol. *)
let rec find_descendants bsym_table bid =
  let children = find_children bsym_table bid in
  let d = ref children in
  BidSet.iter begin fun bid ->
    d := BidSet.union !d (find_descendants bsym_table bid)
  end children;
  !d

(** Searches the bound symbol table for the given symbol's id. *)
let find_id bsym_table bid = Flx_bsym.id (find bsym_table bid)

(** Searches the bound symbol table for the given symbol's source reference. *)
let find_sr bsym_table bid = Flx_bsym.sr (find bsym_table bid)

(** Searches the bound symbol table for the given symbol's bbdcl. *)
let find_bbdcl bsym_table bid = Flx_bsym.bbdcl (find bsym_table bid)

(** Searches the bound symbol table for the given symbol's bparams. *)
let find_bparams bsym_table bid = Flx_bsym.get_bparams (find bsym_table bid)

(** Searches the bound symbol table for the given symbol's bvs. *)
let find_bvs bsym_table bid = Flx_bsym.get_bvs (find bsym_table bid)

(** Helper function to replace an elt in the table. *)
let replace_elt bsym_table = Hashtbl.replace bsym_table.table

(** Helper to add a bid to the table roots or it's parent's children. *)
let add_bid_to_parent bsym_table parent bid =
  assert (bid <> 0);
  match parent with
  | None -> ()
  | Some parent ->
    (* assert (parent <> 0); FAILS .. WHY? *)
    Hashtbl.replace bsym_table.childmap parent (BidSet.add bid (find_children bsym_table parent))

(** Helper to remove a bid from the table roots or it's parent's children. *)
let remove_bid_from_parent bsym_table parent bid =
  assert (bid <> 0);
  match parent with
  | None -> ()
  | Some parent ->
    assert (parent <> 0);
    let kids = 
      try Hashtbl.find bsym_table.childmap parent 
      with Not_found -> BidSet.empty 
    in
    let kids = BidSet.remove bid kids in
    if BidSet.is_empty kids then Hashtbl.remove bsym_table.childmap parent 
    else Hashtbl.replace bsym_table.childmap parent kids

(** Adds the bound symbol with the index to the symbol table. *)
let add bsym_table bid parent bsym =
  assert (bid <> 0);
  if mem bsym_table bid then begin
    print_endline ("Woops, index " ^ string_of_int bid ^ " already in table " ^ Flx_bsym.id bsym )
  end
  ;
  assert (not (mem bsym_table bid));

  (* Add this child to the parent's child list, or to the root list.*)
  add_bid_to_parent bsym_table parent bid;

  (* Then, add the child. *)
  replace_elt bsym_table bid {
    parent=parent;
    bsym=bsym }

(** Updates a bound symbol in place while preserving the child-parent
 * relationships. *)
let update bsym_table bid bsym =
  assert (bid <> 0);
  let elt = find_elt bsym_table bid in
  replace_elt bsym_table bid { elt with bsym=bsym }

(** Update a bound symbol's bbdcl in place. *)
let update_bbdcl bsym_table bid bbdcl =
  assert (bid <> 0);
  let elt = find_elt bsym_table bid in
  replace_elt bsym_table bid { elt with bsym=Flx_bsym.replace_bbdcl elt.bsym bbdcl }

(** Remove a binding and all descendants from the bound symbol table. *)
let rec remove bsym_table bid =
  assert (bid <> 0);
  (* It's not a big deal if the bid isn't in the symbol table. *)
  if not (mem bsym_table bid) then () else begin
    let elt = find_elt bsym_table bid in

    (* Remove ourselves from our parent. *)
    remove_bid_from_parent bsym_table elt.parent bid;

    (* And then remove the actual symbol from the symbol table. *)
    Hashtbl.remove bsym_table.table bid;

    (* Finally, remove all our children as well. *)
    BidSet.iter (remove bsym_table) (find_children bsym_table bid)
  end

(** Copies the bound symbol table. *)
let copy bsym_table =
  { bsym_table with 
      table=Hashtbl.copy bsym_table.table;
      childmap=Hashtbl.copy bsym_table.childmap;
      subtype_map=bsym_table.subtype_map
  }

(** Set's a symbol's parent. *)
let set_parent bsym_table bid parent =
  assert (bid <> 0);
  (* Find our bsym *)
  let elt = find_elt bsym_table bid in

  (* Don't do anything if we aren't changing the parent. *)
  if elt.parent = parent then () else

  (* Remove the bid from our old parent. *)
  remove_bid_from_parent bsym_table elt.parent bid;

  (* And add the bid to our new parent. *)
  add_bid_to_parent bsym_table parent bid;

  (* Finally, update the elt's parent. *)
  replace_elt bsym_table bid { elt with parent=parent }

(** Iterate over all the items in the bound symbol table. *)
let iter f bsym_table =
  Hashtbl.iter (fun bid elt -> f bid elt.parent elt.bsym) bsym_table.table

(** Fold over all the items in the bound symbol table. Excludes subtype map*)
let fold f bsym_table init =
  Hashtbl.fold
    (fun bid elt init -> f bid elt.parent elt.bsym init)
    bsym_table.table
    init

(** Returns whether or not one symbol is a child of another. *)
(** WARNING: ridiculously expensive! *)
let is_child bsym_table parent child =
  assert (child <> 0);
  BidSet.mem child (find_children bsym_table parent)

(** Returns whether or not one symbol is an ancestor of another. *)
(** WARNING: the logic is backards here, should be called is_descendant *)
let is_ancestor bsym_table child anc =
  assert (child <> 0);
  assert (anc <> 0);
  let rec is_anc child anc =
    match find_parent bsym_table child with
    | None -> false
    | Some x ->
      if x = anc then true
      else is_anc x anc
  in
  is_anc child anc

(** Return if the bound symbol index is an identity function. *)
let is_identity bsym_table bid =
  Flx_bsym.is_identity (find bsym_table bid)

(** Return if the bound symbol index is a val, var, ref, or tmp. *)
let is_variable bsym_table bid =
  Flx_bsym.is_variable (find bsym_table bid)

(** Return if the bound symbol index is a global val or var. *)
let is_global_var bsym_table bid =
  let elt = find_elt bsym_table bid in
  match elt.parent, Flx_bsym.bbdcl elt.bsym with
  | None,Flx_bbdcl.BBDCL_val (_,_,(`Val | `Var | `Once)) -> true
  | _ -> false

(** Return if the bound symbol index is an identity function. *)
let is_function bsym_table bid =
  Flx_bsym.is_function (find bsym_table bid)

(** Update all the bound function and procedure's bound exes. *)
let update_bexes f bsym_table =
  iter begin fun bid _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | Flx_bbdcl.BBDCL_fun (ps, bvs, bpar, rt, effects, bexes) ->
        let bbdcl = Flx_bbdcl.bbdcl_fun (ps, bvs, bpar, rt, effects, f bexes) in
        update bsym_table bid (Flx_bsym.replace_bbdcl bsym bbdcl)

    | _ -> ()
  end bsym_table

(** Assert that the bound symbol table is well formed. *)
let validate msg bsym_table =
  iter begin fun bid _ bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in

    (* Make sure we don't have any invalid bbdcls *)
    assert (Flx_bbdcl.is_valid bbdcl);

    (* Make sure the referenced bid is in the bsym_table. *)
    let f_bid_msg msg index = if not (index = 0 || mem bsym_table index) 
      then 
        begin
          print_endline (msg^ ": Missing symbol " ^ string_of_int index^ 
          " processing symbol " ^ Flx_bsym.id bsym ^ "<"^string_of_int bid^">");
          raise (IncompleteBsymTable (bid,index, msg)) 
        end
      else ()
    in 
    let f_bid bid = f_bid_msg ("primary table " ^ msg) bid in
    let f_btype = Flx_btype.iter ~f_bid in
    let f_bexpr = Flx_bexpr.iter ~f_bid ~f_btype in
    let f_bexe = Flx_bexe.iter ~f_bid ~f_btype ~f_bexpr in
    Flx_bbdcl.iter ~f_bid ~f_btype ~f_bexpr ~f_bexe bbdcl;
  end bsym_table
  ;
  let f_bid index = if not (index = 0 || mem bsym_table index) 
     then raise (IncompleteBsymTable (0,index,"subtype table"))
  in
  List.iter (fun ((a,b),c) -> f_bid a; f_bid b; f_bid c) bsym_table.subtype_map

let validate_types f_btype bsym_table =
  iter begin fun bid _ bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in
    Flx_bbdcl.iter ~f_btype bbdcl
  end bsym_table




