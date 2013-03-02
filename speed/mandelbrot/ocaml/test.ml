let runs = 1
let max_iterations = 99888

let iterate ci cr =
  let bailout = 4.0 in
  let rec loop zi zr i =
    if i <= max_iterations then
      let
        zr2 = zr *. zr  and
        zi2 = zi *. zi  in
      if zi2 +. zr2 <= bailout then
        loop (zr *. zi *. 2.0 +. ci) (zr2 -. zi2 +. cr) (i + 1)
      else
        i
    else
      0
  in
  loop 0.0 0.0 1

let mandelbrot n =
  for y = -39 to 38 do
    if 1 = n then print_endline "";
    for x = -39 to 38 do
      let i = iterate
        (float x /. 40.0) (float y /. 40.0 -. 0.5) in
      if 1 = n then
        print_string ( if 0 = i then "*" else " " );
    done
  done

let () =
  for iter = 1 to runs do
    mandelbrot iter
  done;
  print_newline ();
