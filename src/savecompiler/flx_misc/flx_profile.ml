(* A simple hashtable to store profile times of arbitrary blocks of code. *)
let statistics = Hashtbl.create 100

(** Return how long a profiled function took to run. *)
let find name =
  let old_times =
    try Hashtbl.find statistics name
    with Not_found -> []
  in
  match old_times with
  | [] -> 0.0
  | time :: _ -> time

(** A wrapper function to profile a function call. *)
let call name f arg =
  let start_time = Unix.gettimeofday () in
  let fend () =
    let end_time = Unix.gettimeofday () in
    let old_times =
      try Hashtbl.find statistics name
      with Not_found -> []
    in
    Hashtbl.replace statistics name ((end_time -. start_time) :: old_times)
  in
  Flx_util.finally fend f arg


(** Print out our gathered statistics. *)
let print f =
  let keys, key_length = Hashtbl.fold
    (fun k _ (ks, key_length) -> k :: ks, max key_length (String.length k))
    statistics
    ([], 0)
  in
  let keys = List.sort compare keys in

  let sum = List.fold_left (+.) 0. in
  let mean xs = (sum xs) /. (float_of_int (List.length xs)) in
  let std xs =
    let m = mean xs in
    sqrt (mean (List.map (fun x -> let d = x -. m in d *. d) xs))
  in

  List.iter begin fun key ->
    let times = Hashtbl.find statistics key in

    output_string f (
      key ^ ": count=" ^ string_of_int (List.length times) ^
      " total= " ^ string_of_float (sum times) ^
      " mean= " ^ string_of_float (mean times) ^ 
      " std= " ^ string_of_float (std times))
  end keys



