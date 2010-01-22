open Flx_ast

type partial_order_result_t =
[
  | `Less
  | `Equal
  | `Greater
  | `Incomparable
]

type bid_t = int

val dummy_bid : bid_t

module BidSet : Set.S with type elt = bid_t

(** Convert a list of bids into a bid set. *)
val bidset_of_list : bid_t list -> BidSet.t

type plain_ivs_list_t = (id_t * bid_t * typecode_t) list
type ivs_list_t = plain_ivs_list_t * vs_aux_t

type recstop = {
  constraint_overload_trail: bid_t list;
  idx_fixlist: bid_t list;
  type_alias_fixlist: (bid_t * int) list;
  as_fixlist: (string * int) list;
  expr_fixlist: (expr_t * int) list;
  depth: int;
  open_excludes: (ivs_list_t * qualified_name_t) list
}

(** {6 Pattern extractor}
 *
 * This type is used to extract components of a value, corresponding to a
 * match. *)
type dir_t =
  | DIR_open of ivs_list_t * qualified_name_t
  | DIR_inject_module of qualified_name_t
  | DIR_use of id_t * qualified_name_t

type sdir_t = Flx_srcref.t * dir_t

type dcl_t =
  (* data structures *)
  | DCL_axiom of         params_t * axiom_method_t
  | DCL_lemma of         params_t * axiom_method_t
  | DCL_reduce of        simple_parameter_t list * expr_t * expr_t
  | DCL_function of      params_t * typecode_t * property_t list * asm_t list
  | DCL_union of         (id_t * int option * vs_list_t * typecode_t) list
  | DCL_struct of        (id_t * typecode_t) list
  | DCL_cstruct of       (id_t * typecode_t) list
  | DCL_typeclass of     asm_t list
  | DCL_match_check of   pattern_t * (string * bid_t)
  | DCL_match_handler of pattern_t * (string * bid_t) * asm_t list

  (* variables *)
  | DCL_val of           typecode_t
  | DCL_var of           typecode_t
  | DCL_lazy of          typecode_t * expr_t
  | DCL_ref of           typecode_t
  | DCL_type_alias of    typecode_t
  | DCL_inherit of       qualified_name_t
  | DCL_inherit_fun of   qualified_name_t

  (* module system *)
  | DCL_module of        asm_t list
  | DCL_instance of      qualified_name_t * asm_t list

  (* binding structures [prolog] *)
  | DCL_newtype of       typecode_t
  | DCL_abs of           type_qual_t list * code_spec_t * named_req_expr_t
  | DCL_const of         property_t list * typecode_t * code_spec_t * named_req_expr_t
  | DCL_fun of           property_t list * typecode_t list * typecode_t * code_spec_t * named_req_expr_t * prec_t
  | DCL_callback of      property_t list * typecode_t list * typecode_t * named_req_expr_t
  | DCL_insert of        code_spec_t * ikind_t * named_req_expr_t

and access_t = [`Private | `Public ]

and sdcl_t = Flx_srcref.t * id_t * bid_t option * access_t * vs_list_t * dcl_t

and iface_t =
  | IFACE_export_fun of suffixed_name_t * string
  | IFACE_export_python_fun of suffixed_name_t * string
  | IFACE_export_type of typecode_t * string

and siface_t = Flx_srcref.t * iface_t

and asm_t =
  | Exe of sexe_t
  | Dcl of sdcl_t
  | Iface of siface_t
  | Dir of sdir_t

type bound_iface_t = Flx_srcref.t * iface_t * bid_t option

type btpattern_t = {
  pattern: btypecode_t;

  (* pattern type variables, including 'any' vars *)
  pattern_vars: BidSet.t;

  (* assignments for 'as' vars *)
  assignments : (bid_t * btypecode_t) list
}

(** general typing *)
and btypecode_t = private
  | BTYP_sum of btypecode_t list
  | BTYP_unitsum of int
  | BTYP_intersect of btypecode_t list (** intersection type *)
  | BTYP_inst of bid_t * btypecode_t list
  | BTYP_tuple of btypecode_t list
  | BTYP_array of btypecode_t * btypecode_t
  | BTYP_record of (string * btypecode_t) list
  | BTYP_variant of (string * btypecode_t) list
  | BTYP_pointer of btypecode_t
  | BTYP_function of btypecode_t * btypecode_t
  | BTYP_cfunction of btypecode_t * btypecode_t
  | BTYP_void
  | BTYP_fix of int

  | BTYP_type of int
  | BTYP_type_tuple of btypecode_t list
  | BTYP_type_function of (bid_t * btypecode_t) list * btypecode_t * btypecode_t
  | BTYP_type_var of bid_t * btypecode_t
  | BTYP_type_apply of btypecode_t * btypecode_t
  | BTYP_type_match of btypecode_t * (btpattern_t * btypecode_t) list

  (* type sets *)
  | BTYP_type_set of btypecode_t list (** open union *)
  | BTYP_type_set_union of btypecode_t list (** open union *)
  | BTYP_type_set_intersection of btypecode_t list (** open union *)

type entry_kind_t = {
  (* the function *)
  base_sym: bid_t;

  (* the type variables of the specialisation *)
  spec_vs: (string * bid_t) list;

  (* types to replace the old type variables expressed in terms of the new
   * ones *)
  sub_ts: btypecode_t list
}

and entry_set_t =
  | FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t

and module_rep_t =
  | Simple_module of bid_t * typecode_t list * name_map_t * sdir_t list

and name_map_t = (string, entry_set_t) Hashtbl.t

type biface_t =
  | BIFACE_export_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_python_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_type of Flx_srcref.t * btypecode_t * string

type regular_args_t =
    int list *                  (* alphabet *)
    int *                       (* state count *)
    (int, tbexpr_t) Hashtbl.t * (* state->expression map *)
    (int * int, int) Hashtbl.t  (* transition matrix *)

and bexe_t =
  | BEXE_label of Flx_srcref.t * string
  | BEXE_comment of Flx_srcref.t * string (* for documenting generated code *)
  | BEXE_halt of Flx_srcref.t * string  (* for internal use only *)
  | BEXE_trace of Flx_srcref.t * string * string  (* for internal use only *)
  | BEXE_goto of Flx_srcref.t * string  (* for internal use only *)
  | BEXE_ifgoto of Flx_srcref.t * tbexpr_t * string  (* for internal use only *)
  | BEXE_call of Flx_srcref.t * tbexpr_t * tbexpr_t
  | BEXE_call_direct of Flx_srcref.t * bid_t * btypecode_t list * tbexpr_t
  | BEXE_call_stack of Flx_srcref.t * bid_t * btypecode_t list * tbexpr_t
  | BEXE_call_prim of Flx_srcref.t * bid_t * btypecode_t list * tbexpr_t
  | BEXE_jump of Flx_srcref.t * tbexpr_t * tbexpr_t
  | BEXE_jump_direct of Flx_srcref.t * bid_t * btypecode_t list * tbexpr_t
  | BEXE_svc of Flx_srcref.t * bid_t
  | BEXE_fun_return of Flx_srcref.t * tbexpr_t
  | BEXE_yield of Flx_srcref.t * tbexpr_t
  | BEXE_proc_return of Flx_srcref.t
  | BEXE_nop of Flx_srcref.t * string
  | BEXE_code of Flx_srcref.t * code_spec_t
  | BEXE_nonreturn_code of Flx_srcref.t * code_spec_t
  | BEXE_assign of Flx_srcref.t * tbexpr_t * tbexpr_t
  | BEXE_init of Flx_srcref.t * bid_t * tbexpr_t
  | BEXE_begin
  | BEXE_end
  | BEXE_assert of Flx_srcref.t * tbexpr_t
  | BEXE_assert2 of Flx_srcref.t * Flx_srcref.t * tbexpr_t option * tbexpr_t
  | BEXE_axiom_check of Flx_srcref.t * tbexpr_t

and bexpr_t =
  | BEXPR_deref of tbexpr_t
  | BEXPR_name of bid_t * btypecode_t list
  | BEXPR_ref of bid_t * btypecode_t list
  | BEXPR_likely of tbexpr_t
  | BEXPR_unlikely of tbexpr_t
  | BEXPR_address of tbexpr_t
  | BEXPR_new of tbexpr_t
  | BEXPR_literal of literal_t
  | BEXPR_apply of tbexpr_t * tbexpr_t
  | BEXPR_apply_prim of bid_t * btypecode_t list * tbexpr_t
  | BEXPR_apply_direct of bid_t * btypecode_t list * tbexpr_t
  | BEXPR_apply_stack of bid_t * btypecode_t list * tbexpr_t
  | BEXPR_apply_struct of bid_t * btypecode_t list * tbexpr_t
  | BEXPR_tuple of tbexpr_t list
  | BEXPR_record of (string * tbexpr_t) list
  | BEXPR_variant of string * tbexpr_t
  | BEXPR_get_n of int * tbexpr_t (* tuple projection *)
  | BEXPR_closure of bid_t * btypecode_t list
  | BEXPR_case of int * btypecode_t
  | BEXPR_match_case of int * tbexpr_t
  | BEXPR_case_arg of int * tbexpr_t
  | BEXPR_case_index of tbexpr_t
  | BEXPR_expr of string * btypecode_t
  | BEXPR_range_check of tbexpr_t * tbexpr_t * tbexpr_t
  | BEXPR_coerce of tbexpr_t * btypecode_t

and tbexpr_t = bexpr_t * btypecode_t

and bparameter_t = {pkind:param_kind_t; pid:string; pindex:bid_t; ptyp:btypecode_t}
and breqs_t = (bid_t * btypecode_t list) list
and bvs_t = (string * bid_t) list
and bparams_t = bparameter_t list * tbexpr_t option

and btype_qual_t = [
  | base_type_qual_t
  | `Bound_needs_shape of btypecode_t
]

and baxiom_method_t = [`BPredicate of tbexpr_t | `BEquation of tbexpr_t * tbexpr_t]
and reduction_t = id_t * bvs_t * bparameter_t list * tbexpr_t * tbexpr_t
and axiom_t = id_t * Flx_srcref.t * bid_t option * axiom_kind_t * bvs_t * bparams_t * baxiom_method_t

and typevarmap_t = (bid_t, btypecode_t) Hashtbl.t

type env_t = (bid_t * id_t * name_map_t * name_map_t list * typecode_t) list
    (* env: container index, name, primary symbol map, directives, type
     * constraint
     *)

type symbol_definition_t =
  | SYMDEF_newtype of typecode_t
  | SYMDEF_abs of type_qual_t list * code_spec_t * named_req_expr_t
  | SYMDEF_parameter of  param_kind_t * typecode_t
  | SYMDEF_typevar of typecode_t (* usually type TYPE *)
  | SYMDEF_axiom of params_t * axiom_method_t
  | SYMDEF_lemma of params_t * axiom_method_t
  | SYMDEF_reduce of parameter_t list * expr_t * expr_t
  | SYMDEF_function of params_t * typecode_t * property_t list * sexe_t list
  | SYMDEF_match_check of pattern_t * (string * bid_t)
  | SYMDEF_module
  | SYMDEF_const_ctor of bid_t * typecode_t * int * ivs_list_t
  | SYMDEF_nonconst_ctor of bid_t * typecode_t * int * ivs_list_t * typecode_t
  | SYMDEF_const of property_t list * typecode_t * code_spec_t * named_req_expr_t
  | SYMDEF_var of typecode_t
  | SYMDEF_val of typecode_t
  | SYMDEF_ref of typecode_t
  | SYMDEF_lazy of typecode_t * expr_t
  | SYMDEF_fun of property_t list * typecode_t list * typecode_t * code_spec_t  * named_req_expr_t * prec_t
  | SYMDEF_callback of property_t list * typecode_t list * typecode_t * named_req_expr_t
  | SYMDEF_insert of code_spec_t * ikind_t * named_req_expr_t
  | SYMDEF_union of (id_t * int * vs_list_t * typecode_t) list
  | SYMDEF_struct of (id_t * typecode_t) list
  | SYMDEF_cstruct of (id_t * typecode_t) list
  | SYMDEF_typeclass
  | SYMDEF_type_alias of typecode_t
  | SYMDEF_inherit of qualified_name_t
  | SYMDEF_inherit_fun of qualified_name_t
  | SYMDEF_instance of qualified_name_t

type type_registry_t = (btypecode_t, bid_t) Hashtbl.t

(* -------------------------------------------------------------------------- *)

(** The void type. *)
val btyp_void : btypecode_t

(** Construct a BTYP_sum type. *)
val btyp_sum : btypecode_t list -> btypecode_t

(** Construct a BTYP_unitsum type. *)
val btyp_unitsum : int -> btypecode_t

(** Construct a BTYP_intersect type. *)
val btyp_intersect : btypecode_t list -> btypecode_t

(** Construct a BTYP_inst type. *)
val btyp_inst : bid_t * btypecode_t list -> btypecode_t

(** Construct a BTYP_tuple type. *)
val btyp_tuple : btypecode_t list -> btypecode_t

(** Construct a BTYP_array type. *)
val btyp_array : btypecode_t * btypecode_t -> btypecode_t

(** Construct a BTYP_record type. *)
val btyp_record : (string * btypecode_t) list -> btypecode_t

(** Construct a BTYP_variant type. *)
val btyp_variant : (string * btypecode_t) list -> btypecode_t

(** Construct a BTYP_pointer type. *)
val btyp_pointer : btypecode_t -> btypecode_t

(** Construct a BTYP_function type. *)
val btyp_function : btypecode_t * btypecode_t -> btypecode_t

(** Construct a BTYP_cfunction type. *)
val btyp_cfunction : btypecode_t * btypecode_t -> btypecode_t

(** Construct a BTYP_fix type. *)
val btyp_fix : int -> btypecode_t

(** construct a BTYP_type type. *)
val btyp_type : int -> btypecode_t

(** construct a BTYP_type_tuple type. *)
val btyp_type_tuple : btypecode_t list -> btypecode_t

(** Construct a BTYP_type_function type. *)
val btyp_type_function :
  (bid_t * btypecode_t) list * btypecode_t * btypecode_t ->
  btypecode_t

(** construct a BTYP_type_var type. *)
val btyp_type_var : bid_t * btypecode_t -> btypecode_t

(** construct a BTYP_type_apply type. *)
val btyp_type_apply : btypecode_t * btypecode_t -> btypecode_t

(** construct a BTYP_type_match type. *)
val btyp_type_match :
  btypecode_t * (btpattern_t * btypecode_t) list ->
  btypecode_t

(** construct a BTYP_type_set type. *)
val btyp_type_set : btypecode_t list -> btypecode_t

(** construct a BTYP_type_set_union type. *)
val btyp_type_set_union : btypecode_t list -> btypecode_t

(** construct a BTYP_type_set_intersection type. *)
val btyp_type_set_intersection : btypecode_t list -> btypecode_t

(* -------------------------------------------------------------------------- *)

val src_of_bexe : bexe_t -> Flx_srcref.t

val ts_of_bexpr : bexpr_t -> btypecode_t list
