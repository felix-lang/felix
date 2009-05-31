(** Name binding
 *
 * Name binding pass 2 *)

open Flx_ast
open Flx_types

type extract_t =
  | Proj_n of Flx_srcref.t * int             (* tuple projections 1 .. n *)
  | Udtor of Flx_srcref.t * qualified_name_t (* argument of union component s *)
  | Proj_s of Flx_srcref.t * string          (* record projection name *)

val gen_match_check:
  pattern_t ->
  expr_t ->
  expr_t

val get_pattern_vars:
  (string, Flx_srcref.t * extract_t list) Hashtbl.t ->
                              (* Hashtable of variable -> extractor *)
  pattern_t ->      (* pattern *)
  extract_t list -> (* extractor for this pattern *)
  unit

val gen_extractor:
  extract_t list ->
  expr_t ->
  expr_t
