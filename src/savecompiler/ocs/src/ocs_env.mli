(* Compilation environment, variable bindings.  *)

open Ocs_types
open Ocs_sym

val top_env : unit -> env
val new_scope : env -> env
val new_frame : env -> env
val new_var : env -> vbind
val bind_name : env -> sval -> vbind -> unit
val bind_var : env -> sval -> vbind
val find_var : env -> sval -> vbind option
val get_var : env -> sval -> vbind
val set_glob : env -> sval -> sval -> unit
val env_copy : env -> env

val is_a_keyword : env -> sval -> bool
val is_keyword : env -> sval -> string -> bool
val safe_is_keyword : env -> sval -> string -> bool

val is_syntax : env -> sval -> (env -> sval array -> code) -> bool

val set_pf0 : env -> (unit -> sval) -> string -> unit
val set_pf1 : env -> (sval -> sval) -> string -> unit
val set_pf2 : env -> (sval -> sval -> sval) -> string -> unit
val set_pf3 : env -> (sval -> sval -> sval -> sval) -> string -> unit
val set_pfn : env -> (sval array -> sval) -> string -> unit
val set_pfcn :
  env -> (thread -> (sval -> unit) -> sval array -> unit) -> string -> unit

