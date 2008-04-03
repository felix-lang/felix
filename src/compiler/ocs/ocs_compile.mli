(* Compile expressions *)

open Ocs_types

val compile : env -> sval -> code

val bind_lang : env -> unit

(* Internal, used by ocs_macro *)
val letsplit : (sval -> sval -> 'a) -> sval -> 'a
val mkseq : code array -> code
val mkbody : env -> sval array -> code array

