let rec tak x y z =
  if y >= x then z
  else tak (tak (x -. 1.) y z) (tak (y -. 1.) z x) (tak (z -. 1.) x y)

let () =
  let n = 12. in
  let v = (tak (3. *. n) (2. *. n) n) in
  Printf.printf "%.2f\n" v;
