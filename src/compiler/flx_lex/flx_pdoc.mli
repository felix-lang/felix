open Flx_token
open Flx_ast

type action_t = [`Scheme of string | `None]
type symbol_t = [`Atom of token | `Group of dyalt_t list]
and dyalt_t = symbol_t list * range_srcref * action_t * anote_t
val document_grammar: bool ref

type page_entry_t = [
  | `Nt of string
  | `Subpage of string * page_entry_t list
]

val record_doc: string -> string -> string -> unit
val record_group : string -> page_entry_t list -> unit
val record_rule : string -> string -> dyalt_t list -> unit

val gen_doc : unit -> unit
