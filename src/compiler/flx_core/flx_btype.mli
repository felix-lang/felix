open Flx_bid
open Flx_kind

exception Invalid_int_of_unitsum
type pmode = [
 | `RW (* read/write *)
 | `R (* read only *)
 | `W (* write only *)
 | `N (* pointer to unit : no read or write *)
]

val str_of_pmode: pmode -> string
type instkind_t = [
  | `Nominal (* nominal type: primitive or user defined *)
  | `Alias (* type alias, to be eliminated *)
]

val str_of_instkind: instkind_t -> string

type btpattern_t = {
  pattern : t;
  pattern_vars : BidSet.t;
  assignments : (bid_t * t) list;
}
and pvpiece_t = [`Ctor of (string * t) | `Base of t]
and t = private
  | BBOOL of bool 
  | BTYP_instancetype of Flx_srcref.t
  | BTYP_ellipsis (* only at end of a tuple, matches rest of argument tuple, for varargs *)
  | BTYP_none
  | BTYP_sum of t list
  | BTYP_compactsum of t list
  | BTYP_unitsum of int
  | BTYP_inst of instkind_t * bid_t * t list * Flx_kind.kind
  | BTYP_finst of bid_t * kind list * kind * kind (* type function instance with kind args, domain, codomain kinds  *)
  | BTYP_vinst of bid_t * t list * Flx_kind.kind
  | BTYP_intersect of t list
  | BTYP_tuple of t list
  | BTYP_compacttuple of t list
  | BTYP_array of t * t
  | BTYP_compactarray of t * t
  | BTYP_rptsum of t * t
  | BTYP_compactrptsum of t * t
  | BTYP_record of (string * t) list
  | BTYP_polyrecord of (string * t) list * string * t
  | BTYP_variant of (string * t) list
  | BTYP_polyvariant of pvpiece_t list

  | BTYP_ptr of pmode * t * t list

  | BTYP_function of t * t
  | BTYP_effector of t * t * t
  | BTYP_linearfunction of t * t
  | BTYP_lineareffector of t * t * t
  | BTYP_cfunction of t * t
  | BTYP_void
  | BTYP_label
  | BTYP_fix of int * kind 
  | BTYP_rev of t
  | BTYP_uniq of t
  | BTYP_borrowed of t
  | BTYP_type_tuple of t list
  | BTYP_type_function of (bid_t * kind) list * kind * t
  | BTYP_type_var of bid_t * kind
  | BTYP_type_apply of t * t
  | BTYP_type_map of t * t
  | BTYP_type_match of t * (btpattern_t * t) list
  | BTYP_subtype_match of t * (btpattern_t * t) list
  | BTYP_tuple_cons of t * t
  | BTYP_tuple_snoc of t * t
  | BTYP_in of t * t (* type in typeset *)
  | BTYP_type_set of t list
  | BTYP_type_set_union of t list
  | BTYP_type_set_intersection of t list
  (* the int is the binding context *)
  (* used during typedef binding process transiently *)
  | BTYP_typeof of int * Flx_ast.expr_t
  | BTYP_typeop of string * t * kind

type relmode_t = [`Eq | `Ge]
val string_of_relmode_t : relmode_t -> string

type tpair_t = t * t
type rel_t = relmode_t * tpair_t
type rels_t = rel_t list
type vassign_t = int * t
type mgu_t = vassign_t list
type maybe_vassign_t = vassign_t option
type reladd_t = tpair_t -> unit
type dvars_t = BidSet.t

(* Unification forward reference hack *)
type nominal_subtype_checker_t = t -> t -> unit (* Throws Not_found on fail *)
type unif_t = rels_t -> dvars_t -> mgu_t 
val unif_thunk: unif_t option ref
val set_unif_thunk: unif_t -> unit
val unif: unif_t

type overload_result =
    bid_t * t * t * (bid_t * t) list * t list
val trivorder : t -> int option
val istriv : t -> bool
val trivtype : int -> t
val str_of_btype : t -> string
val st : t -> string
val sts : t list -> string

val vhash: string * t -> int
val hash_variants: (string * t) list -> (int * (string * t)) list
val find_vdata: (string * t) list -> int -> string * t
val maybe_find_vdata: (string * t) list -> int -> (string * t) option
val vfind_argtype: (string * t) list -> string -> t
val maybe_vfind_argtype: (string * t) list -> string -> t option
 
exception Free_fixpoint of t
type breqs_t = (bid_t * t list) list
type biface_t =
    BIFACE_export_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_cfun of Flx_srcref.t * bid_t * string
  | BIFACE_export_python_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_type of Flx_srcref.t * t * string
  | BIFACE_export_struct of Flx_srcref.t * bid_t
  | BIFACE_export_union of Flx_srcref.t * bid_t * string
  | BIFACE_export_requirement of Flx_srcref.t * breqs_t
val complete_type : t -> bool
val btyp_instancetype: Flx_srcref.t -> t
val btyp_ellipsis : t
val btyp_label : unit -> t
val btyp_none : unit -> t
val btyp_int : unit -> t
val btyp_void : unit -> t
val btyp_unit : unit -> t
val btyp_bool : unit -> t
val btyp_any : unit -> t
val btyp_sum : t list -> t
val btyp_compactsum : t list -> t
val btyp_unitsum : int -> t
val btyp_inst : instkind_t * bid_t * t list * kind -> t
val btyp_finst : bid_t * kind list * kind * kind -> t
val btyp_vinst : bid_t * t list * kind -> t
val btyp_intersect : t list -> t
val btyp_tuple : t list -> t
val btyp_compacttuple : t list -> t
val btyp_rev : t -> t
val btyp_uniq : t -> t
val btyp_borrowed : t -> t
val btyp_tuple_cons : t -> t -> t
val btyp_tuple_snoc : t -> t -> t
val btyp_array : t * t -> t
val btyp_compactarray : t * t -> t
val btyp_rptsum : t * t -> t
val btyp_compactrptsum : t * t -> t
val btyp_record : (string * t) list -> t
val btyp_polyrecord : (string * t) list -> string -> t -> t
val btyp_variant : (string * t) list -> t
val btyp_polyvariant : pvpiece_t list -> t

val btyp_ptr: pmode -> t -> t list -> t
val reduce_ptr: pmode -> t -> t list -> t
val btyp_pointer : t -> t
val btyp_rref : t -> t
val btyp_wref : t -> t

val btyp_cltpointer : t -> t -> t
val btyp_cltrref : t -> t -> t
val btyp_cltwref : t -> t -> t


val btyp_function : t * t -> t
val btyp_effector : t * t * t -> t
val btyp_linearfunction : t * t -> t
val btyp_lineareffector : t * t * t -> t
val btyp_cfunction : t * t -> t
val btyp_fix : int -> kind -> t
val btyp_type_tuple : t list -> t
val btyp_type_function : (bid_t * kind) list * kind * t -> t
val btyp_type_var : bid_t * kind -> t
val btyp_type_apply : t * t -> t
val btyp_type_map : t * t -> t
val btyp_type_match : t * (btpattern_t * t) list -> t
val btyp_subtype_match : t * (btpattern_t * t) list -> t
val btyp_type_set : t list -> t
val btyp_type_set_union : t list -> t
val btyp_type_set_intersection : t list -> t
val btyp_typeof: (int * Flx_ast.expr_t) -> t
val btyp_typeop: string -> t -> kind -> t
val btyp_in: t * t -> t

val reduce_typeop : string -> t -> kind -> t

val bbool: bool -> t

val bmt: string -> Flx_ast.kindcode_t -> kind 

val unfold : 'a -> t -> t
val is_void : t -> bool
val is_unit : t -> bool
val all_voids : t list -> bool
val all_units : t list -> bool
val is_unitsum : t -> bool
val ipow : int -> int -> int
val int_of_linear_type : 'a -> t -> int
val islinear_type : t -> bool
val iscopyable_type : t -> bool
val sizeof_linear_type : 'a -> t -> int
val iscompact_linear_product: t -> bool
val ncases_of_sum : 'a -> t -> int
val flat_iter :
  ?f_bid:(bid_t -> unit) -> ?f_btype:(t -> unit) -> t -> unit
val iter :
  ?f_bid:(bid_t -> unit) -> ?f_btype:(t -> unit) -> t -> unit
val map :
  ?f_bid:(bid_t -> bid_t) -> ?f_btype:(t -> t) -> ?f_kind:(Flx_kind.kind -> Flx_kind.kind) -> t -> t
val contains_uniq: t -> bool
val adjust_fixpoint: t -> t
val widen_fixgap : int -> t -> t
val is_recursive_type : t -> bool

val eval_typeop: 
  (
    (string-> t -> Flx_kind.kind -> t) ->
    string -> 
    t -> 
    Flx_kind.kind ->
    t
  ) option ref

