open Flx_ast
open Flx_types

type btpattern_t = {
  pattern: t;

  (* pattern type variables, including 'any' vars *)
  pattern_vars: BidSet.t;

  (* assignments for 'as' vars *)
  assignments : (bid_t * t) list
}

(** general typing *)
and t = private
  | BTYP_sum of t list
  | BTYP_unitsum of int
  | BTYP_intersect of t list (** intersection type *)
  | BTYP_inst of bid_t * t list
  | BTYP_tuple of t list
  | BTYP_array of t * t
  | BTYP_record of (string * t) list
  | BTYP_variant of (string * t) list
  | BTYP_pointer of t
  | BTYP_function of t * t
  | BTYP_cfunction of t * t
  | BTYP_void
  | BTYP_fix of int

  | BTYP_type of int
  | BTYP_type_tuple of t list
  | BTYP_type_function of (bid_t * t) list * t * t
  | BTYP_type_var of bid_t * t
  | BTYP_type_apply of t * t
  | BTYP_type_match of t * (btpattern_t * t) list

  (* type sets *)
  | BTYP_type_set of t list (** open union *)
  | BTYP_type_set_union of t list (** open union *)
  | BTYP_type_set_intersection of t list (** open union *)

type entry_kind_t = {
  (* the function *)
  base_sym: bid_t;

  (* the type variables of the specialisation *)
  spec_vs: (string * bid_t) list;

  (* types to replace the old type variables expressed in terms of the new
   * ones *)
  sub_ts: t list
}

type entry_set_t =
  | FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t

type name_map_t = (string, entry_set_t) Hashtbl.t

type biface_t =
  | BIFACE_export_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_python_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_type of Flx_srcref.t * t * string

(* -------------------------------------------------------------------------- *)

(** The void type. *)
val btyp_void : t

(** Construct a BTYP_sum type. *)
val btyp_sum : t list -> t

(** Construct a BTYP_unitsum type. *)
val btyp_unitsum : int -> t

(** Construct a BTYP_intersect type. *)
val btyp_intersect : t list -> t

(** Construct a BTYP_inst type. *)
val btyp_inst : bid_t * t list -> t

(** Construct a BTYP_tuple type. *)
val btyp_tuple : t list -> t

(** Construct a BTYP_array type. *)
val btyp_array : t * t -> t

(** Construct a BTYP_record type. *)
val btyp_record : (string * t) list -> t

(** Construct a BTYP_variant type. *)
val btyp_variant : (string * t) list -> t

(** Construct a BTYP_pointer type. *)
val btyp_pointer : t -> t

(** Construct a BTYP_function type. *)
val btyp_function : t * t -> t

(** Construct a BTYP_cfunction type. *)
val btyp_cfunction : t * t -> t

(** Construct a BTYP_fix type. *)
val btyp_fix : int -> t

(** construct a BTYP_type type. *)
val btyp_type : int -> t

(** construct a BTYP_type_tuple type. *)
val btyp_type_tuple : t list -> t

(** Construct a BTYP_type_function type. *)
val btyp_type_function :
  (bid_t * t) list * t * t ->
  t

(** construct a BTYP_type_var type. *)
val btyp_type_var : bid_t * t -> t

(** construct a BTYP_type_apply type. *)
val btyp_type_apply : t * t -> t

(** construct a BTYP_type_match type. *)
val btyp_type_match :
  t * (btpattern_t * t) list ->
  t

(** construct a BTYP_type_set type. *)
val btyp_type_set : t list -> t

(** construct a BTYP_type_set_union type. *)
val btyp_type_set_union : t list -> t

(** construct a BTYP_type_set_intersection type. *)
val btyp_type_set_intersection : t list -> t

(* -------------------------------------------------------------------------- *)

(** Returns if the bound type list is all void types. *)
val all_voids : t list -> bool

(** Returns if the bound type list is all unit types. *)
val all_units : t list -> bool

(** Returns if the bound type is or is equivalent to a BTYP_unitsum. *)
val is_unitsum : t -> bool

(** Returns the integer value of the unit sum type. *)
val int_of_unitsum : t -> int

(* -------------------------------------------------------------------------- *)

(** Iterate over each bound type and call the function on it. *)
val flat_iter :
  ?f_bid:(Flx_types.bid_t -> unit) ->
  ?f_btype:(t -> unit) ->
  t ->
  unit

(** Recursively iterate over each bound type and call the function on it. *)
val iter :
  ?f_bid:(Flx_types.bid_t -> unit) ->
  ?f_btype:(t -> unit) ->
  t ->
  unit

(** Recursively iterate over each bound type and transform it with the
 * function. *)
val map :
  ?f_bid:(Flx_types.bid_t -> bid_t) ->
  ?f_btype:(t -> t) ->
  t ->
  t

(* -------------------------------------------------------------------------- *)

(** Prints a btype to a formatter. *)
val print : Format.formatter -> t -> unit

(** Prints a name_map_t to a formatter. *)
val print_name_map : Format.formatter -> name_map_t -> unit
