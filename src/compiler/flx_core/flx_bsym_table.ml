type elt = {
  bsym: Flx_bsym.t;               (** The symbol. *)
  parent: Flx_types.bid_t option; (** The parent of the symbol. *)
}

(** The type of the bound symbol table. *)
type t = {
  table: (Flx_types.bid_t, elt) Hashtbl.t;
  childmap: (int, Flx_types.BidSet.t) Hashtbl.t; (** All of this bsym table's roots. *)
}

(** Construct a bound symbol table. *)
let create () =
  { table=Hashtbl.create 97;
    childmap=Hashtbl.create 97; }

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
   with Not_found -> Flx_types.BidSet.empty

(** Finds all the descendants of the given symbol. *)
let rec find_descendants bsym_table bid =
  let children = find_children bsym_table bid in
  let d = ref children in
  Flx_types.BidSet.iter begin fun bid ->
    d := Flx_types.BidSet.union !d (find_descendants bsym_table bid)
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
    Hashtbl.replace bsym_table.childmap parent (Flx_types.BidSet.add bid (find_children bsym_table parent))

(** Helper to remove a bid from the table roots or it's parent's children. *)
let remove_bid_from_parent bsym_table parent bid =
  assert (bid <> 0);
  match parent with
  | None -> ()
  | Some parent ->
    assert (parent <> 0);
    let kids = 
      try Hashtbl.find bsym_table.childmap parent 
      with Not_found -> Flx_types.BidSet.empty 
    in
    let kids = Flx_types.BidSet.remove bid kids in
    if Flx_types.BidSet.is_empty kids then Hashtbl.remove bsym_table.childmap parent 
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
    Flx_types.BidSet.iter (remove bsym_table) (find_children bsym_table bid)
  end

(** Copies the bound symbol table. *)
let copy bsym_table =
  { bsym_table with 
      table=Hashtbl.copy bsym_table.table;
      childmap=Hashtbl.copy bsym_table.childmap }

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

(** Fold over all the items in the bound symbol table. *)
let fold f bsym_table init =
  Hashtbl.fold
    (fun bid elt init -> f bid elt.parent elt.bsym init)
    bsym_table.table
    init

(** Returns whether or not one symbol is a child of another. *)
let is_child bsym_table parent child =
  assert (child <> 0);
  Flx_types.BidSet.mem child (find_children bsym_table parent)

(** Returns whether or not one symbol is an ancestor of another. *)
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
  | None,Flx_bbdcl.BBDCL_val (_,_,(`Val | `Var)) -> true
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
let validate bsym_table =
  iter begin fun bid _ bsym ->
    let bbdcl = Flx_bsym.bbdcl bsym in

    (* Make sure we don't have any invalid bbdcls *)
    assert (Flx_bbdcl.is_valid bbdcl);

    (* Make sure the referenced bid is in the bsym_table. *)
    let f_bid bid = assert (mem bsym_table bid) in
    let f_btype = Flx_btype.iter ~f_bid in
    let f_bexpr = Flx_bexpr.iter ~f_bid ~f_btype in
    let f_bexe = Flx_bexe.iter ~f_bid ~f_btype ~f_bexpr in
    Flx_bbdcl.iter ~f_bid ~f_btype ~f_bexpr ~f_bexe bbdcl
  end bsym_table



