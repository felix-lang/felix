(* based off http://metamatix.org/~ocaml/price-of-abstraction.html *)

let resolution = 5000

type complex = {
    x : float;
    y : float;
  }

let norm_square c =
  c.x *. c.x +. c.y *. c.y

let mul c1 c2 =
  { x = c1.x *. c2.x -. c1.y *. c2.y;
    y = c1.x *. c2.y +. c1.y *. c2.x }

let add c1 c2 = { x = c1.x +. c2.x; y = c1.y +. c2.y }

let iters max_iter xc yc =
  let c = { x = xc; y = yc } in
  let rec aux count z =
    if count >= max_iter then max_iter else begin
      if norm_square z >= 4.0 then count else begin
        aux (count+1) (add (mul z z) c)
      end
    end in
  aux 0 c

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
