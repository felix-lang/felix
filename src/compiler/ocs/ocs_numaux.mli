(* Numeric utility functions.  *)

open Ocs_types

open Num
open Ratio
open Big_int

val promote_real : sval -> sval
val promote_complex : sval -> sval
val promote_rational : sval -> sval
val promote_bigint : sval -> sval

val complex_of_snum : sval -> Complex.t
val float_of_snum : sval -> float
val rational_of_snum : sval -> ratio
val bigint_of_snum : sval -> big_int

val snum_fixtypes : sval -> sval -> sval * sval

val snum_of_num : num -> sval
val num_of_snum : sval -> num

val bigint_res : big_int -> sval

val round_float : float -> float
val float_is_int : float -> bool
val float_to_exact : float -> sval

