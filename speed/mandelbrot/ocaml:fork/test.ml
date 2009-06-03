let max_iterations = 99888

let iterate ci cr =
  let zr = ref 0.0 in
  let zi = ref 0.0 in
  let i = ref 1 in
  try
    while true do
      if !i > max_iterations then raise Exit;
      let temp = !zr *. !zi and zr2 = !zr *. !zr and zi2 = !zi *. !zi in
      if zr2 +. zi2 > 4.0 then raise Exit;
      zr := zr2 -. zi2 +. cr;
      zi := temp +. temp +. ci;
      incr i
    done;
    0
  with Exit ->
    if !i > max_iterations then 0 else !i

let row y =
  let s = String.make 78 ' ' in
  for j = 0 to 77 do
    let x = j - 39 in
    let i = iterate
      (float x /. 40.0) (float y /. 40.0 -. 0.5) in
    if i=0 then s.[j] <- '*'
  done;
  s

let invoke (f : 'a -> 'b) x : unit -> 'b =
  let input, output = Unix.pipe() in
  match Unix.fork() with
  | -1 -> (let v = f x in fun () -> v)
  | 0 ->
      Unix.close input;
      let output = Unix.out_channel_of_descr output in
      Marshal.to_channel output (try `Res(f x) with e -> `Exn e) [];
      close_out output;
      exit 0
  | pid ->
      Unix.close output;
      let input = Unix.in_channel_of_descr input in
      fun () ->
        let v = Marshal.from_channel input in
        ignore (Unix.waitpid [] pid);
        close_in input;
        match v with
        | `Res x -> x
        | `Exn e -> raise e

let () =
  print_newline ();
  let t0 = Unix.gettimeofday () in
  Array.iter (fun f -> print_endline(f()))
    (Array.init 78 (fun i -> invoke row (i-39)));
  let t1 = Unix.gettimeofday () in
  print_float (t1 -. t0);
  print_newline ()
