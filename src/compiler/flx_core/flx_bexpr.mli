type bexpr_t =
  | BEXPR_deref of t
  | BEXPR_name of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_ref of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_literal of Flx_ast.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_variant of string * t
  | BEXPR_get_n of int * t (* tuple projection *)
  | BEXPR_closure of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_case of int * Flx_types.btypecode_t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_types.btypecode_t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_types.btypecode_t

and t = bexpr_t * Flx_types.btypecode_t

(* -------------------------------------------------------------------------- *)

(** Extract the type arguments of a bound expression. *)
val get_ts : t -> Flx_types.btypecode_t list

(** Return whether or not one bound expression is equivalent with another bound
 * expression. *)
val cmp : t -> t -> bool

(** Prints a bexpr to a formatter. *)
val print : Format.formatter -> t -> unit
