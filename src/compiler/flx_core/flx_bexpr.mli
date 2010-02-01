type bexpr_t = private
  | BEXPR_deref of t
  | BEXPR_name of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_ref of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_literal of Flx_ast.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_variant of string * t
  | BEXPR_get_n of int * t (* tuple projection *)
  | BEXPR_closure of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_btype.t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t

and t = bexpr_t * Flx_btype.t

(* -------------------------------------------------------------------------- *)

(** Construct a BEXPR_deref expression. *)
val bexpr_deref : Flx_btype.t -> t -> t

(** Construct a BEXPR_name expression. *)
val bexpr_name : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> t

(** Construct a BEXPR_ref expression. *)
val bexpr_ref : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> t

(** Construct a BEXPR_likely expression. *)
val bexpr_likely : Flx_btype.t -> t -> t

(** Construct a BEXPR_unlikely expression. *)
val bexpr_unlikely : Flx_btype.t -> t -> t

(** Construct a BEXPR_address expression. *)
val bexpr_address : Flx_btype.t -> t -> t

(** Construct a BEXPR_new expression. *)
val bexpr_new : Flx_btype.t -> t -> t

(** Construct a BEXPR_literal expression. *)
val bexpr_literal : Flx_btype.t -> Flx_ast.literal_t -> t

(** Construct a BEXPR_apply expression. *)
val bexpr_apply : Flx_btype.t -> t * t -> t

(** Construct a BEXPR_apply_prim expression. *)
val bexpr_apply_prim : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list * t -> t

(** Construct a BEXPR_direct expression. *)
val bexpr_apply_direct : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list * t -> t

(** Construct a BEXPR_apply_stack expression. *)
val bexpr_apply_stack : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list * t -> t

(** Construct a BEXPR_apply_struct expression. *)
val bexpr_apply_struct : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list * t -> t

(** Construct a BEXPR_tuple expression. *)
val bexpr_tuple : Flx_btype.t -> t list -> t

(** Construct a BEXPR_record expression. *)
val bexpr_record : Flx_btype.t -> (string * t) list -> t

(** Construct a BEXPR_variant expression. *)
val bexpr_variant : Flx_btype.t -> string * t -> t

(** Construct a BEXPR_get_n expression. *)
val bexpr_get_n : Flx_btype.t -> int * t -> t

(** Construct a BEXPR_closure expression. *)
val bexpr_closure : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> t

(** Construct a BEXPR_case expression. *)
val bexpr_case : Flx_btype.t -> int * Flx_btype.t -> t

(** Construct a BEXPR_match_case expression. *)
val bexpr_match_case : Flx_btype.t -> int * t -> t

(** Construct a BEXPR_case_arg expression. *)
val bexpr_case_arg : Flx_btype.t -> int * t -> t

(** Construct a BEXPR_case_index expression. *)
val bexpr_case_index : Flx_btype.t -> t -> t

(** Construct a BEXPR_expr expression. *)
val bexpr_expr : Flx_btype.t -> string * Flx_btype.t -> t

(** Construct a BEXPR_range_check expression. *)
val bexpr_range_check : Flx_btype.t -> t * t * t -> t

(** Construct a BEXPR_coerceexpression. *)
val bexpr_coerce : Flx_btype.t -> t * Flx_btype.t -> t

(* -------------------------------------------------------------------------- *)

(** Extract the type arguments of a bound expression. *)
val get_ts : t -> Flx_btype.t list

(** Return whether or not one bound expression is equivalent with another bound
 * expression. *)
val cmp : t -> t -> bool

(* -------------------------------------------------------------------------- *)

(** Iterate over one level a bound expression. *)
val flat_iter :
  ?fi:(Flx_types.bid_t -> unit) ->  (** Apply this to each bid. *)
  ?ft:(Flx_btype.t -> unit) ->      (** Apply this to each bound type. *)
  ?fe:(t -> unit) ->                (** Apply this to each bound expression. *)
  t -> unit

(** Recursively iterate over a bound expression. *)
val iter :
  ?fi:(Flx_types.bid_t -> unit) ->  (** Apply this to each bid. *)
  ?ft:(Flx_btype.t -> unit) ->      (** Apply this to each bound type. *)
  ?fe:(t -> unit) ->                (** Apply this to each bound expression. *)
  t ->
  unit

(** Recursively map functions over a bound expression. *)
val map :
  ?fi:(Flx_types.bid_t -> Flx_types.bid_t) -> (** Apply this to each bid. *)
  ?ft:(Flx_btype.t -> Flx_btype.t) ->         (** Apply this to each bound
                                                  type. *)
  ?fe:(t -> t) ->                             (** Apply this to each bound
                                                  expression. *)
  t ->
  t

(* -------------------------------------------------------------------------- *)

(** Simplify the bound expression. *)
val reduce : t -> t

(* -------------------------------------------------------------------------- *)

(** Prints a bexpr to a formatter. *)
val print : Format.formatter -> t -> unit
