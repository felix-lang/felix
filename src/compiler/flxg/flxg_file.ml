type state_t = [`NeverOpened | `Open of out_channel | `Closed ]

type t = {
  out_filename : string;
  mutable out_chan: state_t;
}

let make f = { out_filename=f; out_chan=`NeverOpened }

let open_out f =
  match f.out_chan with
  | `Open chan -> chan
  | _ ->
    let chan = Pervasives.open_out f.out_filename in
    f.out_chan <- `Open chan;
    chan

let close_out f =
  match f.out_chan with
  | `NeverOpened | `Closed -> ()
  | `Open chan ->
    Pervasives.close_out chan;
    f.out_chan <- `Closed

let filename f =
  f.out_filename

let output_string f s =
  let chan = open_out f in
  Pervasives.output_string chan s

let was_used f =
  match f.out_chan with
  | `NeverOpened -> false
  | _ -> true
