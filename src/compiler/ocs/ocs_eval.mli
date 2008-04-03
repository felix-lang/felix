(* Evaluation *)

open Ocs_types

val eval : thread -> (sval -> unit) -> code -> unit

