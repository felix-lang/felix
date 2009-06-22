(* based off http://metamatix.org/~ocaml/price-of-abstraction.html *)

let resolution = 5000

class complex_c x_init y_init =
  object
    val x = x_init
    val y = y_init
    method get_x = x
    method get_y = y
    method norm_square = x *. x  +. y *. y
    method add (c:complex_c) = new complex_c (x +. c#get_x) (y +. c#get_y)
    method mul (c:complex_c) = new complex_c (x *. c#get_x -. y *. c#get_y) (x *. c#get_y +. y *. c#get_x)
  end

let iters max_iter xc yc =
  let c = new complex_c xc yc in
  let rec aux count z =
    if count >= max_iter then max_iter else begin
      if z#norm_square >= 4.0 then count else begin
        aux (count+1) ((z#mul z)#add c)
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
