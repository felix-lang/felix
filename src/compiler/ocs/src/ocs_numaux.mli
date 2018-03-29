(* Numeric utility functions.  *)

open Ocs_types

val promote_real : sval -> sval

val float_of_snum : sval -> float

val snum_fixtypes : sval -> sval -> sval * sval

val round_float : float -> float
val float_is_int : float -> bool
val float_to_exact : float -> sval

