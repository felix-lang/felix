open List

let spaces level = String.make (level*2) ' '
let catmap sep fn ls = String.concat sep (map fn ls)
let (+>) x f = f x (* reverse application *)

let transpose x =
  let dtor ls =
    split (map (fun x -> hd x, tl x) ls)
  in let rec cons ls (h,t) = match hd t with
    | [] -> h :: ls
    | _ -> cons (h :: ls) (dtor t)
  in tl (rev (cons [] ([],x)))

let list_last l = hd (rev l)

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

let catch_all f x =
  try Some (f x) with _ -> None

let is_some = function | Some _ -> true | None -> false

let nlist n =
  let lst = ref [] in
  for i = 1 to n do lst := (n-i) :: !lst done;
  !lst

let si = string_of_int
let cat = String.concat
let bcat = Buffer.add_string

let hashtable_of_list lst =
  let t = Hashtbl.create (length lst) in
  iter
  (fun (k,v) -> Hashtbl.add t k v)
  lst
  ;
  t

let rec fix f x = f (fix f) x

let uniq_add elt lst =
  if mem elt lst then lst else (elt::lst)

let uniq_cat u nu  =
  fold_left
  (fun l i -> if mem i l then l else i :: l)
  u
  nu

let uniq_list lst = uniq_cat [] lst
