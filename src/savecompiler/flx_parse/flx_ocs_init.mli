(** {6 Routine to initialise Ocs Scheme}
Add some non-standard symbols to Ocs Scheme.

- [(giveup) ...........:] raise Givup to cause parse thread to be abandoned
- [(raise s) ..........:] raise Scheme_error s
- [(unescape s) .......:] remove escape code from string
- [(c-quote-string s) .:] quote a string like C
- [(utf8->ucn s) ......:] convert utf8 encoded data to ucn
- [_sr ................:] source reference of production
- [_1 .. _20 ..........:] attribute of n'th production symbol

*)


exception Scheme_error of Ocs_types.sval

val init_env : unit -> Ocs_types.env
val flx_ocs_init : Ocs_types.env -> unit


