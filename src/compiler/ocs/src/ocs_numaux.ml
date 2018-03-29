(* Numeric utility functions.  *)

open Ocs_types
open Ocs_error

let promote_real =
  function
    Sint i -> Sreal (float_of_int i)
  | (Sreal _) as r -> r
  | _ -> raise (Error "bad number type")
;;

let float_of_snum =
  function
    Sint i -> float_of_int i
  | Sreal r -> r
  | _ -> raise (Error "bad number type")
;;

let snum_fixtypes a b =
  match (a, b) with
    (Sint _, Sint _) -> a, b
  | (Sreal _, _) -> a, promote_real b
  | (_, Sreal _) -> (promote_real a), b
  | _ -> raise (Error "snum_fixtypes: not numeric types")
;;

let round_float r =
  let d = floor (r +. 0.5)
  and e = ceil (r -. 0.5) in
    if d <> e && (mod_float e 2.0) = 0.0 then e
    else d
;;

let float_is_int f =
  let (x, _) = modf f in
    x = 0.0
;;

let max_f_int = 2.0 ** (float_of_int (Sys.word_size - 2)) -. 1.0;;
let min_f_int = -.max_f_int -. 1.0;;

(* We need to deconstruct IEEE floats to convert them.  *)
let fe_bits = Int64.of_string "0x7ff0000000000000";;
let fm_bits = Int64.of_string "0x000fffffffffffff";;
let fi_bit  = Int64.of_string "0x0010000000000000";;
let fs_bit  = Int64.of_string "0x8000000000000000";;

let fb_get_dm fb =
  Int64.logand fb fm_bits
;;

let fb_get_m fb =
  Int64.logor (fb_get_dm fb) fi_bit
;;

let fb_get_e fb =
  Int64.to_int (Int64.shift_right (Int64.logand fb fe_bits) 52) - 1023
;;

let fb_get_s fb =
  Int64.compare (Int64.logand fb fs_bit) Int64.zero <> 0;
;;

let f_is_int m e =
  if e < 0 then false
  else if e >= 52 then true
  else Int64.compare (Int64.logand fm_bits
				   (Int64.shift_left m e)) Int64.zero = 0
;;

let float_to_exact f =
  if float_is_int f && f >= min_f_int && f <= max_f_int then
    Sint (int_of_float f)
  else if f = infinity || f = neg_infinity || f = nan then
    raise (Error "invalid float")
  else
    raise (Error "invalid float")
;;


