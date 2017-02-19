
type btype = Flx_btype.t
type bexpr_t = private
    BEXPR_int of int
  | BEXPR_not of t
  | BEXPR_deref of t
  | BEXPR_varname of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_ref of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_class_new of Flx_btype.t * t
  | BEXPR_literal of Flx_literal.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_polyrecord of (string * t) list * t
  | BEXPR_remove_fields of t * string list
  | BEXPR_variant of string * t
  | BEXPR_closure of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_identity_function of Flx_btype.t
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_match_variant of string * t
  | BEXPR_case_arg of int * t
  | BEXPR_variant_arg of string * t
  | BEXPR_case_index of t
  | BEXPR_expr of Flx_code_spec.t * Flx_btype.t * t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t
  | BEXPR_compose of t * t
  | BEXPR_tuple_tail of t
  | BEXPR_tuple_head of t
  | BEXPR_tuple_cons of t * t
  | BEXPR_tuple_body of t
  | BEXPR_tuple_last of t
  | BEXPR_tuple_snoc of t * t
  | BEXPR_prj of int * Flx_btype.t * Flx_btype.t
  | BEXPR_rprj of string * int * Flx_btype.t * Flx_btype.t
  | BEXPR_aprj of t * Flx_btype.t * Flx_btype.t
  | BEXPR_inj of int * Flx_btype.t * Flx_btype.t
  | BEXPR_label of Flx_types.bid_t
  | BEXPR_unitptr of int
  | BEXPR_cond of t * t * t
  | BEXPR_funprod of t
  | BEXPR_funsum of t
  | BEXPR_lrangle of t
  | BEXPR_lrbrack of t
and t = bexpr_t * Flx_btype.t
val complete_check : Flx_btype.t -> Flx_btype.t
val complete_check_list : Flx_btype.t list -> Flx_btype.t list
val bexpr_cond :
  bexpr_t * Flx_btype.t ->
  bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_identity_function : Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_unitptr : int -> bexpr_t * Flx_btype.t
val bexpr_unit : bexpr_t * Flx_btype.t
val bexpr_bool : bool -> bexpr_t * Flx_btype.t
val bexpr_true : bexpr_t * Flx_btype.t
val bexpr_false : bexpr_t * Flx_btype.t
val bexpr_label : Flx_types.bid_t -> bexpr_t * Flx_btype.t
val bexpr_tuple_tail : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_tuple_head : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_tuple_cons : Flx_btype.t -> t * t -> bexpr_t * Flx_btype.t
val bexpr_tuple_body : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_tuple_last : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_tuple_snoc : Flx_btype.t -> t * t -> bexpr_t * Flx_btype.t
val bexpr_deref : Flx_btype.t -> t -> t
val bexpr_int : int -> bexpr_t * Flx_btype.t
val bexpr_not : bexpr_t * Flx_btype.t -> t
val bexpr_varname :
  Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> bexpr_t * Flx_btype.t
val bexpr_ref :
  Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> bexpr_t * Flx_btype.t
val bexpr_likely : bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_unlikely : bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_address : bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_new : bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_class_new : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_literal : Flx_btype.t -> Flx_literal.literal_t -> bexpr_t * Flx_btype.t
val bexpr_apply : Flx_btype.t -> t * t -> bexpr_t * Flx_btype.t
val bexpr_apply_prim :
  Flx_btype.t ->
  Flx_types.bid_t * Flx_btype.t list * t -> bexpr_t * Flx_btype.t
val bexpr_apply_direct :
  Flx_btype.t ->
  Flx_types.bid_t * Flx_btype.t list * t -> bexpr_t * Flx_btype.t
val bexpr_apply_stack :
  Flx_btype.t ->
  Flx_types.bid_t * Flx_btype.t list * t -> bexpr_t * Flx_btype.t
val bexpr_apply_struct :
  Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list * t -> bexpr_t * Flx_btype.t
val bexpr_tuple : Flx_btype.t -> t list -> bexpr_t * Flx_btype.t
val bexpr_coerce : t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_prj : int -> Flx_btype.t -> Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_rnprj :
  string -> int -> Flx_btype.t -> Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_rprj :
  string -> Flx_btype.t -> Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_record : (string * t) list -> t
val cal_removal :
  t ->
  (string * Flx_btype.t) list ->
  string list -> (string * (bexpr_t * Flx_btype.t)) list
val bexpr_remove_fields : t -> string list -> t
val bexpr_polyrecord : (string * t) list -> bexpr_t * btype -> t
val bexpr_variant : Flx_btype.t -> string * t -> bexpr_t * Flx_btype.t
val bexpr_aprj : t -> Flx_btype.t -> Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_inj : int -> Flx_btype.t -> Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_get_n :
  Flx_btype.t -> int -> bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_closure : Flx_btype.t -> Flx_types.bid_t * Flx_btype.t list -> bexpr_t * Flx_btype.t
val bexpr_const_case : int * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_nonconst_case :
  Flx_btype.t -> int * Flx_btype.t -> bexpr_t * Flx_btype.t
val bexpr_match_case : int * t -> bexpr_t * Flx_btype.t
val bexpr_match_variant : string * t -> bexpr_t * Flx_btype.t
val bexpr_case_arg : Flx_btype.t -> int * t -> bexpr_t * Flx_btype.t
val bexpr_variant_arg : Flx_btype.t -> string * t -> bexpr_t * Flx_btype.t
val bexpr_case_index : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_expr : Flx_code_spec.t * Flx_btype.t * t -> bexpr_t * Flx_btype.t
val bexpr_range_check : Flx_btype.t -> t * t * t -> bexpr_t * Flx_btype.t
val bexpr_compose : Flx_btype.t -> t * t -> bexpr_t * Flx_btype.t
val bexpr_unitsum_case : int -> int -> bexpr_t * Flx_btype.t
val bexpr_funprod : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_funsum : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_lrangle : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val bexpr_lrbrack : Flx_btype.t -> t -> bexpr_t * Flx_btype.t
val get_ts : bexpr_t * Flx_btype.t -> Flx_btype.t list
val cmp : t -> t -> bool
val flat_iter :
  ?f_bid:(Flx_types.bid_t -> unit) ->
  ?f_btype:(Flx_btype.t -> unit) ->
  ?f_bexpr:(t -> unit) ->
  ?f_label:(Flx_types.bid_t -> unit) -> bexpr_t * Flx_btype.t -> unit
val iter :
  ?f_bid:(Flx_types.bid_t -> unit) ->
  ?f_btype:(Flx_btype.t -> unit) ->
  ?f_bexpr:(bexpr_t * Flx_btype.t -> unit) ->
  ?f_label:(Flx_types.bid_t -> unit) -> t -> unit
val map :
  ?f_bid:(Flx_types.bid_t -> Flx_types.bid_t) ->
  ?f_btype:(Flx_btype.t -> Flx_btype.t) ->
  ?f_bexpr:(t -> t) -> bexpr_t * Flx_btype.t -> bexpr_t * Flx_btype.t
val reduce : t -> t
