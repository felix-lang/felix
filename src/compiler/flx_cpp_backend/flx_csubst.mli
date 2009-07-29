(** Code fragment inliner *)

val csubst:
  Flx_srcref.t ->
  Flx_srcref.t ->
  string ->
  Flx_ctypes.cexpr_t ->      (* value argument 'as is' use $t *)
  Flx_ctypes.cexpr_t list -> (* value arguments as strings *)
  string list ->             (* types of value arguments as strings *)
  string ->                  (* argument type as string *)
  string ->                  (* return type as string *)
  string list ->             (* generic arguments as strings *)
  string ->                  (* precedence *)
  string ->                  (* shape of argument *)
  string list ->             (* shape of arguments *)
  string list ->             (* display EXCLUDING thread frame *)
  string list ->             (* shape of generic type arguments as strings *)
  Flx_ctypes.cexpr_t
