let rec ack m n = match m,n with
  | 0,n -> n + 1
  | m,0 -> ack (m - 1) 1
  | m,n -> ack (m - 1) (ack m (n - 1))
;;

let () =
  let n = 13 in
  let v = ack 3 n in
  Printf.printf "Ack(3,%d): %d\n" n v;
