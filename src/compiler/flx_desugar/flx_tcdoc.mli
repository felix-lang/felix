open Flx_token
open Flx_ast

val record_tc: string -> vs_list_t * statement_t list -> unit
val record_module: string -> vs_list_t * statement_t list -> unit
val gen_doc : unit -> unit
