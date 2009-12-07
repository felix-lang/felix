open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open List
open Flx_util

type t = (bid_t, bid_t list) Hashtbl.t

let make () = Hashtbl.create 97

let find_children childmap parent =
  try Hashtbl.find childmap parent with Not_found -> []

let is_child childmap parent child =
  mem child (find_children childmap parent)

let add_child childmap parent child =
  let kids = find_children childmap parent in
  Hashtbl.replace childmap parent (child::kids)

(* failure to find a parent shouldn't happen,
  except perhaps for typeclass methods,
  but it seems to and appear harmless ..??
*)
let is_ancestor bsym_table child anc =
  let rec is_anc child anc =
    match Flx_bsym_table.find_parent bsym_table child with
    | None -> false
    | Some x ->
      if x = anc then true
      else is_anc x anc
  in
  try
    is_anc child anc
    with Not_found ->
      (*
      print_endline ("Can't find some ancestor of " ^ si child ^ " ..instance typeclass?");
      *)
      false

let remove_child childmap parent child =
  let kids = find_children childmap parent in
  let kids = filter (fun i -> i <> child) kids in
  Hashtbl.replace childmap parent kids

(* closure of index with respect to children, EXCLUDES self *)
let rec descendants child_map index =
  let d = ref BidSet.empty in
  let children = find_children child_map index in
  iter
  (fun i ->
    if not (BidSet.mem i !d) then
    begin
      d := BidSet.add i !d;
      d := BidSet.union !d (descendants child_map i)
    end
  )
  children
  ;
  !d

let cal_children bsym_table =
  let child_map = make () in
  Flx_bsym_table.iter begin fun i (_,parent,_,_) ->
    match parent with
    | Some parent -> add_child child_map parent i
    | None -> ()
  end bsym_table;
  child_map
