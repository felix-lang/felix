(** {6 Routines to extract source reference from terms}
 *
 * Source reference manipulators. *)

open Flx_ast
open Flx_types

val rstoken: srcref -> srcref -> range_srcref
val rsrange: range_srcref -> range_srcref -> range_srcref
val slift: srcref -> range_srcref

val rsexpr: expr_t -> expr_t -> range_srcref
val rslist: expr_t list -> range_srcref

val src_of_bexe: bexe_t -> range_srcref
val src_of_expr: expr_t -> range_srcref
val src_of_stmt : statement_t -> range_srcref
val src_of_pat : pattern_t -> range_srcref
val src_of_qualified_name : qualified_name_t -> range_srcref
val src_of_suffixed_name: suffixed_name_t -> range_srcref

val short_string_of_src: range_srcref -> string
val long_string_of_src: range_srcref -> string

val dummy_sr: range_srcref
