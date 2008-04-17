(* The Longarray module is designed to work around the maximum array size
 * imposed by OCaml's built-in Array module.  Longarray provides the
 * same interface as Array (well, a portion of it) and is implemented as
 * a list of arrays.  For arrays shorter than the maximum length, the
 * only cost is an additional level of indirection. *)

open Pretty

module E = Errormsg

type 'a t = 'a array list

let split_len (len: int) : int * int =
  if len <= Sys.max_array_length then
    len, 0
  else
    Sys.max_array_length, len - Sys.max_array_length

let split_idx (idx: int) : int option =
  if idx < Sys.max_array_length then
    None
  else
    Some (idx - Sys.max_array_length)

let rec create (len: int) (init: 'a) : 'a t =
  let len1, len2 = split_len len in
  (Array.create len1 init) :: (if len2 > 0 then create len2 init else [])

let rec init (len: int) (fn: int -> 'a) : 'a t =
  let len1, len2 = split_len len in
  let fn2 i = fn (i + len1) in
  (Array.init len1 fn) :: (if len2 > 0 then init len2 fn2 else [])

let rec blit (src: 'a t) (srcidx: int)
             (dst: 'a t) (dstidx: int) (len: int) : unit =
  if srcidx != 0 || dstidx != 0 then
    E.s (E.unimp "Longarray.blit with nonzero src/dst indices");
  try
    let len1, len2 = split_len len in
    Array.blit (List.hd src) 0 (List.hd dst) 0 len1;
    if len2 > 0 then
      blit (List.tl src) 0 (List.tl dst) 0 len2
  with Failure ("hd" | "tl") ->
    raise (Invalid_argument "Longarray.blit")

let rec fill (a: 'a t) (idx: int) (len: int) (elt: 'a) : unit =
  try
    match split_idx idx with
    | None ->
        let end1, end2 = split_len (idx + len) in
        Array.fill (List.hd a) idx (end1 - idx) elt;
        if end2 > 0 then
          fill (List.tl a) 0 end2 elt
    | Some idx' ->
        fill (List.tl a) idx' len elt
  with Failure ("hd" | "tl") ->
    raise (Invalid_argument "Longarray.fill")

let rec length (a: 'a t) : int =
  match a with
  | hd :: tl -> Array.length hd + length tl
  | [] -> 0

let rec get (a: 'a t) (i: int) : 'a =
  try
    match split_idx i with
    | None -> Array.get (List.hd a) i
    | Some i' -> get (List.tl a) i'
  with Failure ("hd" | "tl") ->
    raise (Invalid_argument "(get) index out of bounds")

let rec set (a: 'a t) (i: int) (elt: 'a) : unit =
  try
    match split_idx i with
    | None -> Array.set (List.hd a) i elt
    | Some i' -> set (List.tl a) i' elt
  with Failure ("hd" | "tl") ->
    raise (Invalid_argument "(set) index out of bounds")

let rec copy (a: 'a t) : 'a t =
  match a with
  | hd :: tl -> Array.copy hd :: copy tl
  | [] -> []

let rec map (fn: 'a -> 'b) (a: 'a t) : 'b t =
  match a with
  | hd :: tl -> Array.map fn hd :: map fn tl
  | [] -> []

let docArray ?(sep = chr ',') (doit: int -> 'a -> doc)
             () (elements: 'a t) =
  let len = length elements in
  if len = 0 then 
    nil
  else
    let rec loop (acc: doc) i =
      if i >= len then acc else
      let fi = doit i (get elements i) in (* Make sure this is done first *)
      loop (acc ++ sep ++ fi) (i + 1)
    in
    let f0 = doit 0 (get elements 0) in
    loop f0 1
