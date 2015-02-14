(** {6 Routine to process Ocs Scheme} *)
val scheme_lex : Flx_srcref.t -> string -> Ocs_types.sval
val silly_scheme_lex : Ocs_types.sval -> Ocs_types.sval

val scheme_compile : Ocs_types.env -> Ocs_types.sval -> Ocs_types.code

val scheme_eval : Ocs_types.code -> Ocs_types.sval

val scheme_run_sexpr : Ocs_types.env -> Ocs_types.sval -> Ocs_types.sval

val scheme_run : Flx_srcref.t -> Ocs_types.env -> string -> Ocs_types.sval
val silly_scheme_run : Ocs_types.env -> Ocs_types.sval -> Ocs_types.sval

