(** build a DFA from a regular expression *)

open Flx_ast
open Flx_mtypes2

val process_regexp:
  regexp_t ->
  CharSet.t                          * (* alphabet *)
  int                                * (* state count *)
  (int, expr_t) Hashtbl.t            * (* term_codes *)
  (int * int, int) Hashtbl.t           (* transition matrix *)
