(* Numeric utility functions.  *)

open Ocs_types
open Ocs_error

open Num
open Ratio
open Big_int

let fix_floating_precision () =
  let n = Arith_status.get_floating_precision () in
    if n < 25 then
      Arith_status.set_floating_precision 25
;;

fix_floating_precision ();;

Arith_status.set_normalize_ratio true;;

let promote_real =
  function
    Sint i -> Sreal (float_of_int i)
  | (Sreal _) as r -> r
  | (Scomplex _) as z -> z
  | Sbigint bi -> Sreal (float_of_big_int bi)
  | Srational r -> Sreal (float_of_ratio r)
  | _ -> raise (Error "bad number type")
;;

let float_of_snum =
  function
    Sint i -> float_of_int i
  | Sreal r -> r
    (* Note - the imaginary part is discarded! *)
  | Scomplex { Complex.re = r; Complex.im = _ } -> r
  | Sbigint bi -> float_of_big_int bi
  | Srational r -> float_of_ratio r
  | _ -> raise (Error "bad number type")
;;

let promote_complex =
  function
    (Scomplex _) as z -> z
  | x -> Scomplex { Complex.re = float_of_snum x; Complex.im = 0.0 }
;;

let complex_of_snum =
  function
    Scomplex z -> z
  | x -> { Complex.re = float_of_snum x; Complex.im = 0.0 }
;;

let rational_of_snum =
  function
    Sbigint bi -> ratio_of_big_int bi
  | Sint i -> ratio_of_int i
  | Srational r -> r
  | _ -> raise (Error "bad number type")
;;

let promote_rational s =
  Srational (rational_of_snum s)
;;

let bigint_of_snum =
  function
    Sint i -> big_int_of_int i
  | Sbigint bi -> bi
  | _ -> raise (Error "bad number type")
;;

let promote_bigint =
  function
    Sint i -> Sbigint (big_int_of_int i)
  | (Sbigint _) as bi -> bi
  | _ -> raise (Error "bad number type")
;;

let snum_fixtypes a b =
  match (a, b) with
    (Sint _, Sint _) -> a, b
  | (Scomplex _, _) -> a, promote_complex b
  | (_, Scomplex _) -> (promote_complex a), b
  | (Sreal _, _) -> a, promote_real b
  | (_, Sreal _) -> (promote_real a), b
  | (Srational _, _) -> a, promote_rational b
  | (_, Srational _) -> (promote_rational a), b
  | (Sbigint _, _) -> a, promote_bigint b
  | (_, Sbigint _) -> (promote_bigint a), b
  | _ -> raise (Error "snum_fixtypes: not numeric types")
;;

let snum_of_num =
  function
    Int x -> Sint x
  | Big_int x -> Sbigint x
  | Ratio x -> Srational x
;;

let num_of_snum =
  function
    Sint x -> Int x
  | Sbigint x -> Big_int x
  | Srational x -> Ratio x
  | _ -> raise (Error "bad number type")
;;

(* Return a result as the simplest representation of a given bigint *)
let bigint_res bi =
  if is_int_big_int bi then
    Sint (int_of_big_int bi)
  else
    Sbigint bi
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

(* Convert an int64 into a bigint, possibly ignoring the most
   significant 4 bits (on 32-bit machines).  This is good enough
   for the 52 + 1 -bit mantissa of 64-bit IEEE floats.  *)
let big_int_of_int64 i =
  if Sys.word_size = 64 then
    (big_int_of_int (Int64.to_int i))
  else (* Assume 32 *)
    let lo = Int64.to_int i land 0x3fffffff
    and hi = Int64.to_int (Int64.shift_right i 30) in
      add_big_int (big_int_of_int lo)
	(mult_big_int (big_int_of_int hi) (power_int_positive_int 2 30))
;;

let float_to_exact f =
  if float_is_int f && f >= min_f_int && f <= max_f_int then
    Sint (int_of_float f)
  else if f = infinity || f = neg_infinity || f = nan then
    raise (Error "invalid float")
  else
    let fb = Int64.bits_of_float f in
    let m = fb_get_m fb
    and e = fb_get_e fb
    and is_neg = fb_get_s fb in
    let bm = big_int_of_int64 m in
      if f_is_int m e then
	let wrap = if is_neg then minus_big_int else fun x -> x in
	  if e = 52 then
	    Sbigint (wrap bm)
	  else if e > 52 then
	    Sbigint (wrap (mult_big_int bm (power_int_positive_int 2 (e - 52))))
	  else
	    Sbigint (wrap (div_big_int bm (power_int_positive_int 2 (52 - e))))
      else
	let wrap = if is_neg then minus_ratio else fun x -> x in
	  if e < -1022 then (* not normalized, no implied mantissa bit *)
	    Srational (wrap (create_ratio (big_int_of_int64 (fb_get_dm fb))
			     (power_int_positive_int 2 (51 - e))))
	  else
	    Srational (wrap (create_ratio bm
			     (power_int_positive_int 2 (52 - e))))
;;

