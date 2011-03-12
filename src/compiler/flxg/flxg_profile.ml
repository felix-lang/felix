(** Create a simple timer function. *)
let make_timer () =
  let last_time = ref 0.0 in
  fun () ->
    let now = (Unix.times()).Unix.tms_utime in
    let elapsed = now -. !last_time in
    last_time := now;
    elapsed
