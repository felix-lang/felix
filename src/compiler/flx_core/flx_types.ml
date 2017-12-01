open Flx_bid

(** Types
 *
 * These files declare the main data structures used by the compiler, and
 * provide routines to dump them to a string for debugging purposes. *)

(** {6 Generic partial ordering comparison result} *)
type partial_order_result_t =
[
  | `Less
  | `Equal
  | `Greater
  | `Incomparable
]

open Flx_ast

type plain_ivs_list_t = (Flx_id.t * bid_t * kindcode_t) list
type ivs_list_t = plain_ivs_list_t * vs_aux_t

let dfltivs : ivs_list_t = [], dfltvs_aux


type recstop = {
  constraint_overload_trail: bid_t list;
  idx_fixlist: bid_t list;
  type_alias_fixlist: (bid_t * int) list;
  as_fixlist: (string * int) list;
  expr_fixlist: (expr_t * int) list;
  depth: int;
  open_excludes: (ivs_list_t * qualified_name_t) list;
  strr_limit: int;
}


(** {6 Pattern extractor}
 *
 * This type is used to extract components of a value, corresponding to a
 * match. *)
type dir_t =
  | DIR_open of ivs_list_t * qualified_name_t
  | DIR_inject_module of ivs_list_t * qualified_name_t
  | DIR_use of Flx_id.t * qualified_name_t

type sdir_t = Flx_srcref.t * dir_t

(** Used to represent all the different value types. *)
type value_kind_t = [ `Val | `Var | `Ref | `Once | `Lazy of expr_t ]

type dcl_t =
  (* data structures *)
  | DCL_virtual_type
  | DCL_axiom of         params_t * axiom_method_t
  | DCL_lemma of         params_t * axiom_method_t
  | DCL_reduce of        (vs_list_t * simple_parameter_t list * expr_t * expr_t) list
  | DCL_function of      params_t * typecode_t * typecode_t * property_t list * asm_t list
  | DCL_union of         (Flx_id.t * int option * vs_list_t * typecode_t * typecode_t option) list
  | DCL_struct of        (Flx_id.t * typecode_t) list
  | DCL_cstruct of       (Flx_id.t * typecode_t) list * named_req_expr_t
  | DCL_typeclass of     asm_t list
  | DCL_match_handler of pattern_t * (string * bid_t) * asm_t list

  (* variables *)
  | DCL_value of         typecode_t * value_kind_t
  | DCL_type_alias of    typecode_t
  | DCL_inherit of       qualified_name_t
  | DCL_inherit_fun of   qualified_name_t

  (* module system *)
  | DCL_root of          asm_t list
  | DCL_module of        asm_t list
  | DCL_library of       asm_t list
  | DCL_instance of      qualified_name_t * asm_t list

  (* binding structures [prolog] *)
  | DCL_newtype of       typecode_t
  | DCL_instance_type of typecode_t
  | DCL_abs of           type_qual_t list * Flx_code_spec.t * named_req_expr_t
  | DCL_const of         property_t list * typecode_t * Flx_code_spec.t * named_req_expr_t
  | DCL_fun of           property_t list * typecode_t list * typecode_t * Flx_code_spec.t * named_req_expr_t * prec_t
  | DCL_callback of      property_t list * typecode_t list * typecode_t * named_req_expr_t
  | DCL_insert of        Flx_code_spec.t * ikind_t * named_req_expr_t

and access_t = [`Private | `Public ]

and sdcl_t = Flx_srcref.t * Flx_id.t * bid_t option * access_t * vs_list_t * dcl_t

and iface_t =
  | IFACE_export_fun of suffixed_name_t * string
  | IFACE_export_cfun of suffixed_name_t * string
  | IFACE_export_python_fun of suffixed_name_t * string
  | IFACE_export_type of typecode_t * string
  | IFACE_export_struct of string
  | IFACE_export_union of suffixed_name_t * string
  | IFACE_export_requirement of named_req_expr_t

and siface_t = Flx_srcref.t * iface_t 

and asm_t =
  | Exe of sexe_t
  | Dcl of sdcl_t
  | Iface of siface_t
  | Dir of sdir_t

type bound_iface_t = Flx_srcref.t * iface_t * bid_t option

type symbol_definition_t =
  | SYMDEF_virtual_type
  | SYMDEF_label of string
  | SYMDEF_newtype of typecode_t
  | SYMDEF_instance_type of typecode_t
  | SYMDEF_abs of type_qual_t list * Flx_code_spec.t * named_req_expr_t
  | SYMDEF_parameter of  param_kind_t * typecode_t
  | SYMDEF_typevar of kindcode_t (* usually KND_type *)
  | SYMDEF_axiom of params_t * axiom_method_t
  | SYMDEF_lemma of params_t * axiom_method_t
  | SYMDEF_reduce of (ivs_list_t * parameter_t list * expr_t * expr_t) list
  | SYMDEF_function of params_t * typecode_t * typecode_t * property_t list * sexe_t list

  | SYMDEF_root of bid_t option (* initialiser procedure *)
  | SYMDEF_library 
  | SYMDEF_module
  | SYMDEF_typeclass

  | SYMDEF_const_ctor of bid_t * typecode_t * int * ivs_list_t
  | SYMDEF_nonconst_ctor of bid_t * typecode_t * int * ivs_list_t * typecode_t 
  | SYMDEF_const of property_t list * typecode_t * Flx_code_spec.t * named_req_expr_t
  | SYMDEF_var of typecode_t
  | SYMDEF_once of typecode_t
  | SYMDEF_val of typecode_t
  | SYMDEF_ref of typecode_t
  | SYMDEF_lazy of typecode_t * expr_t
  | SYMDEF_fun of property_t list * typecode_t list * typecode_t * Flx_code_spec.t  * named_req_expr_t * prec_t
  | SYMDEF_callback of property_t list * typecode_t list * typecode_t * named_req_expr_t
  | SYMDEF_insert of Flx_code_spec.t  * ikind_t * named_req_expr_t
  | SYMDEF_union of (Flx_id.t * int *  ivs_list_t * typecode_t * typecode_t * bool) list
  | SYMDEF_struct of (Flx_id.t * typecode_t) list
  | SYMDEF_cstruct of (Flx_id.t * typecode_t) list * named_req_expr_t 
  | SYMDEF_type_alias of typecode_t
  | SYMDEF_inherit of qualified_name_t
  | SYMDEF_inherit_fun of qualified_name_t
  | SYMDEF_instance of qualified_name_t

(* -------------------------------------------------------------------------- *)


