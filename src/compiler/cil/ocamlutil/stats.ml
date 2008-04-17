(* The following functions are implemented in perfcount.c *) 

(* Returns true if we have the performance counters *)
external has_performance_counters: unit -> bool = "has_performance_counters"

(* Initializes the CPU speed and returns true if we have
   the performance counters *)
external reset_performance_counters: unit -> bool = "reset_performance_counters"

(* Returns number of seconds since the first read *)
external read_pentium_perfcount : unit -> float = "read_pentium_perfcount"

(* Returns current cycle counter, divided by 1^20, and truncated to 30 bits *)
external sample_pentium_perfcount_20 : unit -> int = "sample_pentium_perfcount_20"

(* Returns current cycle counter, divided by 1^10, and truncated to 30 bits *)
external sample_pentium_perfcount_10 : unit -> int = "sample_pentium_perfcount_10"


(** Whether to use the performance counters (on Pentium only) *)
type timerModeEnum =
  | Disabled      (** Do not collect timing information *)
  | SoftwareTimer (** Use OCaml's [Unix.time] for timing information *)
  | HardwareTimer (** Use the Pentium's cycle counter to time code *)
  | HardwareIfAvail (** Use the hardware cycle counter if availible; 
                        otherwise use SoftwareTimer *)

(* The performance counters are disabled by default.
   This will always be one of Disabled | SoftwareTimer | HardwareTimer.
   HardwareIfAvail is handled in reset. *)
let timerMode = ref Disabled

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
let reset (mode: timerModeEnum) : unit = 
  top.sub <- [];
  match mode with
    Disabled
  | SoftwareTimer -> timerMode := mode
  | HardwareTimer -> 
      if not (reset_performance_counters ()) then begin
        timerMode := SoftwareTimer;
        raise NoPerfCount
      end;
      timerMode := mode
  | HardwareIfAvail ->
      if (reset_performance_counters ()) then
        timerMode := HardwareTimer
      else
        timerMode := SoftwareTimer



let print chn msg = 
  (* Total up *)
  top.time <- List.fold_left (fun sum f -> sum +. f.time) 0.0 top.sub;
  let rec prTree ind node = 
    if !timerMode = HardwareTimer then 
      (Printf.fprintf chn "%s%-25s      %8.5f s\n" 
         (String.make ind ' ') node.name node.time)
    else
      (Printf.fprintf chn "%s%-25s      %6.3f s\n" 
         (String.make ind ' ') node.name node.time);

   List.iter (prTree (ind + 2)) (List.rev node.sub)
  in
  Printf.fprintf chn "%s" msg; 
  List.iter (prTree 0) [ top ];
  Printf.fprintf chn "Timing used %s\n"
    (if !timerMode = HardwareTimer then "Pentium performance counters"
     else "Unix.time");
  let gc = Gc.quick_stat () in 
  let printM (w: float) : string = 
    Printf.sprintf "%.2fMb" (w *. 4.0 /. 1000000.0)
  in
  Printf.fprintf chn 
    "Memory statistics: total=%s, max=%s, minor=%s, major=%s, promoted=%s\n    minor collections=%d  major collections=%d compactions=%d\n"
    (printM (gc.Gc.minor_words +. gc.Gc.major_words 
               -. gc.Gc.promoted_words))
    (printM (float_of_int gc.Gc.top_heap_words))
    (printM gc.Gc.minor_words)
    (printM gc.Gc.major_words)
    (printM gc.Gc.promoted_words)
    gc.Gc.minor_collections
    gc.Gc.major_collections
    gc.Gc.compactions;
    
  ()
        
  

(* Get the current time, in seconds *)
let get_current_time () : float = 
  if !timerMode = HardwareTimer then
    read_pentium_perfcount ()
  else
    (Unix.times ()).Unix.tms_utime

let repeattime limit str f arg = 
                                        (* Find the right stat *)
  let stat : t = 
    let curr = match !current with h :: _ -> h | [] -> assert false in
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
  let rec repeatf count = 
    let res   = f arg in
    let diff = get_current_time () -. start in
    if diff < limit then
      repeatf (count + 1)
    else begin
      stat.time <- stat.time +. (diff /. float(count));
      current := oldcurrent;                (* Pop the current stat *)
      res                                   (* Return the function result *)
    end
  in
  repeatf 1


let time str f arg =
  if !timerMode = Disabled then
    f arg
  else
    repeattime 0.0 str f arg
    

let lastTime = ref 0.0
let timethis (f: 'a -> 'b) (arg: 'a) : 'b = 
  let start = get_current_time () in
  let res = f arg in 
  lastTime := get_current_time () -. start; 
  res
  
(** Return the cumulative time of all calls to {!Stats.time} and
  {!Stats.repeattime} with the given label. *)
(* Usually there will be only one occurence in the tree, but summing them all
   makes more sense than choosing one arbitrarily *)
let lookupTime (label:string) : float =
  let time : float ref = ref 0.0 in
  let rec search (x:t) : unit = 
    if x.name = label then time := !time +. x.time;
    List.iter search x.sub
  in
  search top;
  !time











