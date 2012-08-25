open List

let spaces level = String.make (level*2) ' '
let catmap sep fn ls = String.concat sep (map fn ls)
let (+>) x f = f x (* reverse application *)


let finally fend f x =
  let r =
    try
      f x
    with e ->
      fend ();
      raise e
  in
  fend ();
  r

let catch_all f x =
  try Some (f x) with _ -> None

let is_some = function | Some _ -> true | None -> false

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

(** A series of functions that help to call a function that accepts a tuple with
 * curried arguments. *)
let curry2 f p1 p2 =
  f (p1,p2)

let curry3 f p1 p2 p3 =
  f (p1,p2,p3)

let curry4 f p1 p2 p3 p4 =
  f (p1,p2,p3,p4)

let curry5 f p1 p2 p3 p4 p5 =
  f (p1,p2,p3,p4,p5)

let curry6 f p1 p2 p3 p4 p5 p6 =
  f (p1,p2,p3,p4,p5,p6)

let curry7 f p1 p2 p3 p4 p5 p6 p7 =
  f (p1,p2,p3,p4,p5,p6,p7)

let curry8 f p1 p2 p3 p4 p5 p6 p7 p8 =
  f (p1,p2,p3,p4,p5,p6,p7,p8)

let curry9 f p1 p2 p3 p4 p5 p6 p7 p8 p9 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9)

let curry10 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

let curry11 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)

let curry12 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)

let curry13 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13)

let curry14 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14)

let curry15 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15)

let curry16 f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 =
  f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16)


(** A series of functions that help to call a curriable function with tuple
 * arguments. *)
let uncurry2 f (p1,p2) =
  f p1 p2

let uncurry3 f (p1,p2,p3) =
  f p1 p2 p3

let uncurry4 f (p1,p2,p3,p4) =
  f p1 p2 p3 p4

let uncurry5 f (p1,p2,p3,p4,p5) =
  f p1 p2 p3 p4 p5

let uncurry6 f (p1,p2,p3,p4,p5,p6) =
  f p1 p2 p3 p4 p5 p6

let uncurry7 f (p1,p2,p3,p4,p5,p6,p7) =
  f p1 p2 p3 p4 p5 p6 p7

let uncurry8 f (p1,p2,p3,p4,p5,p6,p7,p8) =
  f p1 p2 p3 p4 p5 p6 p7 p8

let uncurry9 f (p1,p2,p3,p4,p5,p6,p7,p8,p9) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9

let uncurry10 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10

let uncurry11 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11

let uncurry12 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12

let uncurry13 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13

let uncurry14 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14

let uncurry15 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15

let uncurry16 f (p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16) =
  f p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16
