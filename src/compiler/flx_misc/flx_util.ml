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


