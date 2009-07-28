open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open List
open Flx_util

let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_child Hashtbl.find failed " ^ msg);
    raise Not_found


type child_map_t =
  (bid_t, bid_t list) Hashtbl.t

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
let is_ancestor bbdfns child anc =
  let rec is_anc child anc =
    (*
    let _,parent,_,_ = hfind ("is_ancestor " ^ si child) bbdfns child in
    *)
    let _,parent,_,_ = Hashtbl.find bbdfns child in
    match parent with
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
  let d = ref IntSet.empty in
  let children = find_children child_map index in
  iter
  (fun i ->
    if not (IntSet.mem i !d) then
    begin
      d := IntSet.add i !d;
      d := IntSet.union !d (descendants child_map i)
    end
  )
  children
  ;
  !d

let cal_children bbdfns =
  let child_map = Hashtbl.create 97 in
  Hashtbl.iter begin fun i (id,parent,sr,entry) ->
    match parent with
    | Some parent ->
      Hashtbl.replace child_map parent
      (i ::
        (
          try Hashtbl.find child_map parent
          with Not_found -> []
        )
      )
    | None -> ()
  end bbdfns;
  child_map
