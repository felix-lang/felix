open Flx_ast
open Flx_ctypes

val string_of_cexpr : cexpr_t -> string
val sc : prec_t -> cexpr_t -> string
val ce : prec_t -> string -> cexpr_t

val ce_atom : string -> cexpr_t
val ce_postfix : string -> cexpr_t -> cexpr_t
val ce_prefix : string -> cexpr_t -> cexpr_t
val ce_infix : string -> cexpr_t -> cexpr_t -> cexpr_t
val ce_call : cexpr_t -> cexpr_t list -> cexpr_t
val ce_array : cexpr_t -> cexpr_t -> cexpr_t
val ce_new : cexpr_t list -> string -> cexpr_t list -> cexpr_t
val ce_cast : string -> cexpr_t -> cexpr_t
val ce_cond : cexpr_t -> cexpr_t -> cexpr_t -> cexpr_t
val ce_expr : prec_t -> string -> cexpr_t
val ce_top : string -> cexpr_t
val ce_dot : cexpr_t -> string -> cexpr_t
val ce_arrow : cexpr_t -> string -> cexpr_t

val ce_add : cexpr_t -> cexpr_t -> cexpr_t
val ce_sub : cexpr_t -> cexpr_t -> cexpr_t
val ce_mul : cexpr_t -> cexpr_t -> cexpr_t
val ce_div : cexpr_t -> cexpr_t -> cexpr_t
val ce_rmd : cexpr_t -> cexpr_t -> cexpr_t
val ce_neg : cexpr_t -> cexpr_t
val ce_int : int -> cexpr_t




exception Unknown_prec of prec_t

val genprec: string -> prec_t -> string * prec_t
val reduce: cexpr_t -> cexpr_t

