let rec ack m n = match m,n with
  | 0,n -> n + 1
  | m,0 -> ack (m - 1) 1
  | m,n -> ack (m - 1) (ack m (n - 1))
;;

let () =
  let n = 13 in
  let t0 = Unix.gettimeofday () in
  let v = ack 3 n in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Ack(3,%d): %d\n" n v;
  Printf.printf "%f\n" (t1 -. t0)
