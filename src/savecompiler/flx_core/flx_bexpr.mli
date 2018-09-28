open Flx_bid
type bexpr_t = private
  | BEXPR_lambda of bid_t * Flx_btype.t * (bexpr_t * Flx_btype.t)
  | BEXPR_int of int
  | BEXPR_not of t
  | BEXPR_deref of t
  | BEXPR_varname of bid_t * Flx_btype.t list
  | BEXPR_ref of bid_t * Flx_btype.t list
  | BEXPR_rref of bid_t * Flx_btype.t list
  | BEXPR_wref of bid_t * Flx_btype.t list

  | BEXPR_cltpointer of Flx_btype.t * Flx_btype.t * t * int 
  | BEXPR_cltpointer_prj of Flx_btype.t * Flx_btype.t * int

  | BEXPR_uniq of t
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_class_new of Flx_btype.t * t
  | BEXPR_literal of Flx_literal.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of bid_t * Flx_btype.t list * t
  | BEXPR_apply_direct of bid_t * Flx_btype.t list * t
  | BEXPR_apply_stack of bid_t * Flx_btype.t list * t
  | BEXPR_apply_struct of bid_t * Flx_btype.t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_polyrecord of (string * t) list * t
  | BEXPR_remove_fields of t * string list
  | BEXPR_closure of bid_t * Flx_btype.t list
  | BEXPR_identity_function of Flx_btype.t
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_rptsum_arg of t
  | BEXPR_case_index of t
  | BEXPR_expr of Flx_code_spec.t * Flx_btype.t * t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t
  | BEXPR_reinterpret_cast of t * Flx_btype.t
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
  | BEXPR_ainj of t * Flx_btype.t * Flx_btype.t 
  | BEXPR_label of bid_t
  | BEXPR_unitptr of int
  | BEXPR_cond of t * t * t
  | BEXPR_funprod of t
  | BEXPR_funsum of t
  | BEXPR_lrangle of t
  | BEXPR_lrbrack of t
and t = bexpr_t * Flx_btype.t
val complete_check : string -> Flx_btype.t  -> Flx_btype.t
val complete_check_list : Flx_btype.t list -> Flx_btype.t list
val bexpr_lambda : bid_t -> Flx_btype.t -> t -> t
val bexpr_cond :
  t ->
  t -> t -> t
val bexpr_identity_function : Flx_btype.t -> t
val bexpr_unitptr : int -> t
val bexpr_unit : t
val bexpr_bool : bool -> t
val bexpr_true : t
val bexpr_false : t
val bexpr_land: t -> t -> t
val bexpr_lor: t -> t -> t

val bexpr_label : bid_t -> t
val bexpr_tuple_tail : Flx_btype.t -> t -> t
val bexpr_tuple_head : Flx_btype.t -> t -> t
val bexpr_tuple_cons : t * t -> t
val bexpr_tuple_body : Flx_btype.t -> t -> t
val bexpr_tuple_last : Flx_btype.t -> t -> t
val bexpr_tuple_snoc : Flx_btype.t -> t * t -> t
val bexpr_deref : Flx_btype.t -> t -> t
val bexpr_int : int -> t
val bexpr_not : t -> t
val bexpr_varname :
  Flx_btype.t -> bid_t * Flx_btype.t list -> t

val bexpr_ref :
  Flx_btype.t -> bid_t * Flx_btype.t list -> t

val bexpr_wref :
  Flx_btype.t -> bid_t * Flx_btype.t list -> t
val bexpr_rref :
  Flx_btype.t -> bid_t * Flx_btype.t list -> t

val bexpr_uniq: t -> t
val bexpr_cltpointer_of_pointer:
  t -> t
val bexpr_cltpointer:
  Flx_btype.t -> Flx_btype.t -> t -> int -> t

val bexpr_likely : t -> t
val bexpr_unlikely : t -> t
val bexpr_address : t -> t
val bexpr_new : t -> t
val bexpr_class_new : Flx_btype.t -> t -> t
val bexpr_literal : Flx_btype.t -> Flx_literal.literal_t -> t
val bexpr_literal_int : int -> t
val bexpr_apply : Flx_btype.t -> t * t -> t
val bexpr_apply_prim :
  Flx_btype.t ->
  bid_t * Flx_btype.t list * t -> t
val bexpr_apply_direct :
  Flx_btype.t ->
  bid_t * Flx_btype.t list * t -> t
val bexpr_apply_stack :
  Flx_btype.t ->
  bid_t * Flx_btype.t list * t -> t
val bexpr_apply_struct :
  Flx_btype.t -> bid_t * Flx_btype.t list * t -> t
val bexpr_tuple : Flx_btype.t -> t list -> t
val bexpr_coerce : t * Flx_btype.t -> t
val bexpr_reinterpret_cast : t * Flx_btype.t -> t
val bexpr_prj : int -> Flx_btype.t -> Flx_btype.t -> t
val bexpr_rnprj :
  string -> int -> Flx_btype.t -> Flx_btype.t -> t
val bexpr_rprj :
  string -> Flx_btype.t -> Flx_btype.t -> t

val bexpr_cltpointer_prj:
  Flx_btype.t -> Flx_btype.t -> int -> t


val bexpr_record : (string * t) list -> t
val cal_removal :
  t ->
  (string * Flx_btype.t) list ->
  string list -> (string * (t)) list
val bexpr_remove_fields : t -> string list -> t
val bexpr_polyrecord : (string * t) list -> t -> t
val bexpr_variant : Flx_btype.t -> string * t -> t
val bexpr_aprj : t -> Flx_btype.t -> Flx_btype.t -> t
val bexpr_inj : int -> Flx_btype.t -> Flx_btype.t -> t
val bexpr_ainj : t -> Flx_btype.t -> Flx_btype.t -> t

val find_seq: 
  string -> (* field name *)
  int -> (* sequence number for name *)
  (string * Flx_btype.t) list -> (* record data *)
  (int * Flx_btype.t) option (* absolute field index and type *)


val bexpr_get_n :
  Flx_btype.t -> int -> t -> t
val bexpr_get_named :
  Flx_btype.t -> string -> t -> t


val bexpr_closure : Flx_btype.t -> bid_t * Flx_btype.t list -> t
val bexpr_const_case : int * Flx_btype.t -> t
val bexpr_nonconst_case :
  Flx_btype.t -> int * Flx_btype.t -> t
val bexpr_match_case : int * t -> t
val bexpr_match_variant : string * t -> t
val bexpr_case_arg : Flx_btype.t -> int * t -> t
val bexpr_rptsum_arg : t -> t
val bexpr_variant_arg : Flx_btype.t -> string * t -> t
val bexpr_case_index : Flx_btype.t -> t -> t
val bexpr_expr : Flx_code_spec.t * Flx_btype.t * t -> t
val bexpr_range_check : Flx_btype.t -> t * t * t -> t
val bexpr_compose : Flx_btype.t -> t * t -> t
val bexpr_revcompose : Flx_btype.t -> t * t -> t
val bexpr_unitsum_case : int -> int -> t
val bexpr_funprod : Flx_btype.t -> t -> t
val bexpr_funsum : Flx_btype.t -> t -> t
val bexpr_lrangle : Flx_btype.t -> t -> t
val bexpr_lrbrack : Flx_btype.t -> t -> t
val get_ts : t -> Flx_btype.t list
val cmp : t -> t -> bool
val flat_iter :
  ?f_bid:(bid_t -> unit) ->
  ?f_btype:(Flx_btype.t -> unit) ->
  ?f_bexpr:(t -> unit) ->
  ?f_label:(bid_t -> unit) -> t -> unit
val iter :
  ?f_bid:(bid_t -> unit) ->
  ?f_btype:(Flx_btype.t -> unit) ->
  ?f_bexpr:(t -> unit) ->
  ?f_label:(bid_t -> unit) -> t -> unit
val map :
  ?f_bid:(bid_t -> bid_t) ->
  ?f_btype:(Flx_btype.t -> Flx_btype.t) ->
  ?f_bexpr:(t -> t) -> t -> t
val reduce : t -> t
