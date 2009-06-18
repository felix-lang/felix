let rec tak x y z =
  if y >= x then z
  else tak (tak (x -. 1.) y z) (tak (y -. 1.) z x) (tak (z -. 1.) x y)

let () =
  let n = 10. in
  let t0 = Unix.gettimeofday () in
  let v = (tak (3. *. n) (2. *. n) n) in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" v;
  Printf.printf "%f\n" (t1 -. t0)
