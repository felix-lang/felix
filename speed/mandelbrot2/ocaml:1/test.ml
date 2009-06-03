(* based off http://metamatix.org/~ocaml/price-of-abstraction.html *)

let resolution = 5000

let iters max_iter xc yc =
  let rec aux count x y =
    if count = max_iter then max_iter else begin
      if x *. x  +. y *. y >= 4.0 then count else
      aux (count+1) (x *. x -. y *. y +. xc) (2.0 *. x *. y
      +. yc)
    end in
  aux 0 xc yc

let _ =
  let t0 = Unix.gettimeofday () in
  let max_val = resolution / 2 in
  let min_val = - max_val in
  let mul = 2.0 /. float_of_int max_val in
  let count = ref 0 in
  for i=min_val to max_val do
    for j = min_val to max_val do
      let x = float_of_int i *. mul in
      let y = float_of_int j *. mul in
      count := !count + iters 100 x y;
    done
  done;
  let t1 = Unix.gettimeofday () in
  Printf.printf "%d\n" !count;
  Printf.printf "%f\n" (t1 -. t0)
