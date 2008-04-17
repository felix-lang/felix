(* The following functions are implemented in perfcount.c *) 

(* Returns true is we have the performance counters *)
external has_performance_counters: unit -> bool = "has_performance_counters"

(* Returns number of seconds since the first read *)
external read_pentium_perfcount : unit -> float = "read_pentium_perfcount"



(* Whether to use the performance counters (on Pentium only) *)

(* The performance counters are disabled by default. *)
let do_use_performance_counters = ref false

                                        (* A hierarchy of timings *)

type t = { name : string;
           mutable time : float; (* In seconds *)
           mutable sub  : t list}

                                        (* Create the top level *)
let top = { name = "TOTAL";
            time = 0.0;
            sub  = []; }

                                        (* The stack of current path through 
                                         * the hierarchy. The first is the 
                                         * leaf. *)
let current : t list ref = ref [top]

exception NoPerfCount
let reset (perfcount: bool) = 
  top.sub <- [];
  if perfcount then begin
    if not (has_performance_counters ()) then begin
      raise NoPerfCount
    end
  end;
  do_use_performance_counters := perfcount



let print chn msg = 
  (* Total up *)
  top.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 top.sub;
  let rec prTree ind node = 
    if !do_use_performance_counters then 
      (Printf.fprintf chn "%s%-20s          %8.5f s\n" 
         (String.make ind ' ') node.name node.time)
    else
      (Printf.fprintf chn "%s%-20s          %6.3f s\n" 
         (String.make ind ' ') node.name node.time);

   List.iter (prTree (ind + 2)) node.sub
  in
  Printf.fprintf chn "%s" msg; 
  List.iter (prTree 0) [ top ];
  Printf.fprintf chn "Timing used %s\n"
    (if !do_use_performance_counters then "performance counters" else "Unix.time");
  ()
        
  

(* Get the current time, in seconds *)
let get_current_time () : float = 
  if !do_use_performance_counters then 
    read_pentium_perfcount ()
  else
    (Unix.times ()).Unix.tms_utime

let repeattime limit str f arg = 
                                        (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | _ -> assert false in
    let rec loop = function
        h :: _ when h.name = str -> h
      | _ :: rest -> loop rest
      | [] -> 
          let nw = {name = str; time = 0.0; sub = []} in
          curr.sub <- nw :: curr.sub;
          nw
    in
    loop curr.sub
  in
  let oldcurrent = !current in
  current := stat :: oldcurrent;
  let start = get_current_time () in
  let rec loop count = 
    let res   = f arg in
    let diff = get_current_time () -. start in
    if diff < limit then
      loop (count + 1)
    else begin
      stat.time <- stat.time +. (diff /. float(count));
      current := oldcurrent;                (* Pop the current stat *)
      res                                   (* Return the function result *)
    end
  in
  loop 1


let time str f arg = repeattime 0.0 str f arg
    














