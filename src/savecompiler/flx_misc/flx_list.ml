open List
let rec has_prefix prefix lst = 
  match prefix,lst with
  | [],_ -> true
  | ph::pt, lh::lt -> ph = lh && has_prefix pt lt
  | _ -> false

let transpose x =
  let dtor ls =
    List.split (List.map (fun x -> List.hd x, List.tl x) ls)
  in
  let rec cons ls (h, t) =
    match List.hd t with
    | [] -> h :: ls
    | _ -> cons (h :: ls) (dtor t)
  in

  match x with
  | [] -> []
  | _ -> List.tl (List.rev (cons [] ([], x)))

let rec list_last ls =
  match ls with
  | head :: [] -> head
  | head :: tail -> list_last tail
  | [] -> failwith "list_last"

let rec list_index l x =
 let rec aux l i =
   match l with
   | [] -> None
   | h::t ->
    if x = h then Some i
    else aux t (i+1)
  in aux l 0

let rec list_assoc_index l x =
 let rec aux l i =
   match l with
   | [] -> None
   | (h,_)::t ->
    if x = h then Some i
    else aux t (i+1)
  in aux l 0

let rec list_assoc_index_with_assoc l x =
 let rec aux l i =
   match l with
   | [] -> None
   | (h,v)::t ->
    if x = h then Some (i,v)
    else aux t (i+1)
  in aux l 0


let list_omap f ls =
  rev (rev_map f ls)

let nlist n =
  let lst = ref [] in
  for i = 1 to n do lst := (n-i) :: !lst done;
  !lst

(** repeat the element n times. *)
let repeat x i =
  let rec aux i xs =
    if i = 0 then xs
    else aux (i - 1) (x::xs)
  in
  aux i []

let list_prefix lst n =
  let rec aux ol nl n =
    if n>0 then aux (tl ol) (hd ol :: nl) (n-1)
    else rev nl
  in aux lst [] n

let list_split lst n =
  let rec aux ol nl n =
    if n>0 then aux (tl ol) (hd ol :: nl) (n-1)
    else rev nl,ol
  in aux lst [] n

let rec list_tail lst n =
  if n > 0 then list_tail (tl lst) (n-1)
  else lst

let splice_tail hlst tlst =
  let n = length hlst in
  if n >= length tlst then hlst else
  hlst @ list_tail tlst n

let uniq_add elt lst =
  if mem elt lst then lst else (elt::lst)

let uniq_cat u nu  =
  fold_left
  (fun l i -> if mem i l then l else i :: l)
  u
  nu

let uniq_list lst = uniq_cat [] lst

let iteri f lst =
  ignore (fold_left (fun i x -> f i x; i + 1) 0 lst)

let mapi (f:int -> 'a -> 'b) (lst:'a list) : 'b list =
  let i = ref 0 in
  List.map begin fun x ->
    let result = f !i x in
    incr i;
    result
  end lst

let fold_lefti f acc lst =
  let i = ref 0 in
  List.fold_left begin fun acc x ->
    let result = f !i acc x in
    incr i;
    result
  end acc lst

(** [range f N] is [f 0; f 1; ...; f N]. *)
let range f i =
  let rec aux i xs =
    if i < 0 then xs else aux (i - 1) (f i :: xs)
  in
  aux (i - 1) []



