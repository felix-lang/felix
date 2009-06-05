let rec ack m n = match m,n with
  | 0,n -> n + 1
  | m,0 -> ack (m - 1) 1
  | m,n -> ack (m - 1) (ack m (n - 1))
;;

let n = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 1 in
Printf.printf "Ack(3,%d): %d\n" n (ack 3 n)
;;

