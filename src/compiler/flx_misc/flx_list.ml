open List
open Format

let transpose x =
  let dtor ls =
    split (map (fun x -> hd x, tl x) ls)
  in
  let rec cons ls (h, t) =
    match hd t with
    | [] -> h :: ls
    | _ -> cons (h :: ls) (dtor t)
  in
  tl (rev (cons [] ([],x)))

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

(** Prints out the list to the formatter. *)
let print pp_elt ppf = function
  | [] -> fprintf ppf "[]"
  | xs ->
      fprintf ppf "@[<hv1>[";
      let _ =
        List.fold_left begin fun first elt ->
          if not first then fprintf ppf ";@ ";
          pp_elt ppf elt;
          false
        end true xs
  in
  fprintf ppf "@]]"

(** Prints out a list of tuples to a formatter. *)
let print_tuples2 pp_a pp_b ppf items =
  let f ppf (a, b) =
    Flx_format.print_tuple2 ppf pp_a a pp_b b
  in
  print f ppf items

let print_tuples3 pp_a pp_b pp_c ppf items =
  let f ppf (a, b, c) =
    Flx_format.print_tuple3 ppf pp_a a pp_b b pp_c c
  in
  print f ppf items

let print_tuples4 pp_a pp_b pp_c pp_d ppf items =
  let f ppf (a, b, c, d) =
    Flx_format.print_tuple4 ppf pp_a a pp_b b pp_c c pp_d d
  in
  print f ppf items
