let rec tak x y z =
  if y >= x then z
  else tak (tak (x -. 1.) y z) (tak (y -. 1.) z x) (tak (z -. 1.) x y)

let () =
  let n = float_of_string(Array.get Sys.argv 1) in
  Printf.printf "%.1f\n" (tak (3. *. n) (2. *. n) n)
;;

