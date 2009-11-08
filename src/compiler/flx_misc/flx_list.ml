open List

let transpose x =
  let dtor ls =
    split (map (fun x -> hd x, tl x) ls)
  in let rec cons ls (h,t) = match hd t with
    | [] -> h :: ls
    | _ -> cons (h :: ls) (dtor t)
  in tl (rev (cons [] ([],x)))

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

let list_omap f ls =
  rev (rev_map f ls)

let nlist n =
  let lst = ref [] in
  for i = 1 to n do lst := (n-i) :: !lst done;
  !lst

let list_prefix lst n =
  let rec aux ol nl n =
    if n>0 then aux (tl ol) (hd ol :: nl) (n-1)
    else rev nl
  in aux lst [] n

let list_tail lst n =
  let rec aux ol il n =
    if n > 0 then aux (hd il::ol) (tl il) (n-1)
    else rev ol
  in aux [] lst n

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
