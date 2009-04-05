open Flx_token
val flx_keywords : (string * string) list
val flx_syms : (string * string) list
val map_flx_keywords : Flx_ast.srcref -> string -> token
