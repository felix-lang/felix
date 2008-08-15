(** Types
 *
 * These files declare the main data structures used by the compiler, and
 * provide routines to dump them to a string for debugging purposes. *)

(** {6 Generic partial ordering comparison result} *)

open Flx_mtypes1

type partial_order_result_t =
[
  | `Less
  | `Equal
  | `Greater
  | `Incomparable
]

open Flx_ast

(** {6 Pattern extractor}
 *
 * This type is used to extract components of a value, corresponding to a
 * match. *)
type dir_t =
  | DIR_open of ivs_list_t * qualified_name_t
  | DIR_inject_module of qualified_name_t
  | DIR_use of id_t * qualified_name_t

type dcl_t =
  [

  (* data structures *)
  | `DCL_axiom of       params_t * axiom_method_t
  | `DCL_lemma of       params_t * axiom_method_t
  | `DCL_reduce of       simple_parameter_t list * expr_t * expr_t
  | `DCL_function of     params_t * typecode_t * property_t list * asm_t list
  | `DCL_union of        (id_t * int option * vs_list_t * typecode_t) list
  | `DCL_struct of       (id_t * typecode_t) list
  | `DCL_cstruct of      (id_t * typecode_t) list
  | `DCL_cclass of       class_member_t list
  | `DCL_typeclass of    asm_t list
  | `DCL_match_check of pattern_t * (string * int)
  | `DCL_match_handler of pattern_t * (string * int) * asm_t list
  | `DCL_glr of          typecode_t * (reduced_production_t * expr_t)

  (* variables *)
  | `DCL_val of          typecode_t
  | `DCL_var of          typecode_t
  | `DCL_lazy of          typecode_t * expr_t
  | `DCL_ref of          typecode_t
  | `DCL_type_alias of   typecode_t
  | `DCL_inherit of   qualified_name_t
  | `DCL_inherit_fun of   qualified_name_t

  (* module system *)
  | `DCL_module of       asm_t list
  | `DCL_class of        asm_t list
  | `DCL_instance of     qualified_name_t * asm_t list

  (* binding structures [prolog] *)
  | `DCL_newtype of     typecode_t
  | `DCL_abs of         type_qual_t list * c_t * named_req_expr_t
  | `DCL_const of       property_t list * typecode_t * c_t * named_req_expr_t
  | `DCL_fun of         property_t list * typecode_t list * typecode_t * c_t * named_req_expr_t * prec_t
  | `DCL_callback of    property_t list * typecode_t list * typecode_t * named_req_expr_t
  | `DCL_insert of      c_t * ikind_t * named_req_expr_t
  | `DCL_regdef of      regexp_t
  | `DCL_regmatch of    (regexp_t * expr_t) list
  | `DCL_reglex of      (regexp_t * expr_t) list
  ]

and access_t = [`Private | `Public ]

and asm_t =
  [
  | `Exe of range_srcref * exe_t
  | `Dcl of range_srcref * id_t * int option * access_t * vs_list_t * dcl_t
  | `Iface of range_srcref * iface_t
  | `Dir of dir_t
  ]

and iface_t =
  [
  | `IFACE_export_fun of suffixed_name_t * string
  | `IFACE_export_type of typecode_t * string
  ]

(** value typing *)
type 't b0typecode_t' =
  [
  | `BTYP_inst of bid_t * 't list
  | `BTYP_tuple of 't list
  | `BTYP_record of (string * 't) list
  | `BTYP_variant of (string * 't) list
  | `BTYP_unitsum of int
  | `BTYP_sum of 't list
  | `BTYP_function of 't * 't
  | `BTYP_cfunction of 't * 't
  | `BTYP_pointer  of 't
(*  | `BTYP_lvalue  of 't *)
  | `BTYP_array of 't * 't
  | `BTYP_void
  | `BTYP_fix of int
  | `BTYP_intersect of 't list (** intersection type *)
  ]

type 't btpattern_t' = {
  pattern: 't;
  pattern_vars: IntSet.t; (* pattern type variables, including 'any' vars *)
  assignments : (int * 't) list (* assignments for 'as' vars *)
}


(** meta typing *)
type 't b1typecode_t' =
  [
  | `BTYP_var of int * 't
  | `BTYP_apply of 't * 't
  | `BTYP_typefun of (int * 't) list * 't * 't
  | `BTYP_type of int
  | `BTYP_type_tuple of 't list
  | `BTYP_type_match of 't * ('t btpattern_t' * 't) list

  (* type sets *)
  | `BTYP_typeset of 't list (** open union *)
  | `BTYP_typesetunion of 't list (** open union *)
  | `BTYP_typesetintersection of 't list (** open union *)

  (* Barry Jay pattern calculus *)
  | `BTYP_case of 't * IntSet.t * 't

  | `BTYP_lift of 't (* lift type to metatype *)
  ]

(** general typing *)
type 't btypecode_t' =
  [
  | 't b0typecode_t'
  | 't b1typecode_t'
  ]

type b0typecode_t = 't b0typecode_t' as 't
type btypecode_t = 't btypecode_t' as 't
type btpattern_t = btypecode_t btpattern_t'

type entry_kind_t = {
  base_sym:int;                 (* the function *)
  spec_vs:(string * int) list;  (* the type variables of the specialisation *)
  sub_ts:btypecode_t list      (* types to replace the old type variables
                               expressed in terms of the new ones
                            *)
}

and entry_set_t =
  [
  | `FunctionEntry of entry_kind_t list
  | `NonFunctionEntry of entry_kind_t
  ]

and module_rep_t =
  | Simple_module of bid_t * typecode_t list * name_map_t * dir_t list

and name_map_t = (string, entry_set_t) Hashtbl.t


type biface_t =
  [
  | `BIFACE_export_fun of range_srcref * bid_t * string
  | `BIFACE_export_type of range_srcref * btypecode_t * string
  ]

type regular_args_t =
    int list * (* alphabet *)
    int *      (* state count *)
    (int, tbexpr_t) Hashtbl.t * (* state->expression map *)
    (int * int, int) Hashtbl.t (* transition matrix *)

and bexe_t =
  [
  | `BEXE_label of range_srcref * string
  | `BEXE_comment of range_srcref * string (* for documenting generated code *)
  | `BEXE_halt of range_srcref * string  (* for internal use only *)
  | `BEXE_trace of range_srcref * string * string  (* for internal use only *)
  | `BEXE_goto of range_srcref * string  (* for internal use only *)
  | `BEXE_ifgoto of range_srcref * tbexpr_t * string  (* for internal use only *)
  | `BEXE_call of range_srcref * tbexpr_t * tbexpr_t
  | `BEXE_call_direct of range_srcref * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_call_method_direct of range_srcref * tbexpr_t * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_call_method_stack of range_srcref * tbexpr_t * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_call_stack of range_srcref * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_call_prim of range_srcref * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_jump of range_srcref * tbexpr_t * tbexpr_t
  | `BEXE_jump_direct of range_srcref * bid_t * btypecode_t list * tbexpr_t
  | `BEXE_loop of range_srcref * int * tbexpr_t
  | `BEXE_svc of range_srcref * bid_t
  | `BEXE_fun_return of range_srcref * tbexpr_t
  | `BEXE_yield of range_srcref * tbexpr_t
  | `BEXE_proc_return of range_srcref
  | `BEXE_nop of range_srcref * string
  | `BEXE_code of range_srcref * c_t
  | `BEXE_nonreturn_code of range_srcref * c_t
  | `BEXE_assign of range_srcref * tbexpr_t * tbexpr_t
  | `BEXE_init of range_srcref * bid_t * tbexpr_t
  | `BEXE_begin
  | `BEXE_end
  | `BEXE_assert of range_srcref * tbexpr_t
  | `BEXE_assert2 of range_srcref * range_srcref * tbexpr_t option * tbexpr_t
  | `BEXE_axiom_check of range_srcref * tbexpr_t

  | `BEXE_apply_ctor of range_srcref * bid_t * bid_t * btypecode_t list * bid_t * tbexpr_t
  | `BEXE_apply_ctor_stack of range_srcref * bid_t * bid_t * btypecode_t list * bid_t * tbexpr_t
    (* For classes! Note this case works as so:
     * arg0 denotes a variable to store the closure pointer in
     * arg1 and 2 denote the closure to be created,
     * arg3 and 4 s a procedure call executed in the
       context of the closure to initialise the frame.
     * The expression just returns the initialised closure.
     * This is rougly equivalent to (in Ocaml notation):

      let frame = closure (arg1,arg2) in
      closure(arg3,arg2).call(arg4).run();
      closure

      except that the second closure create takes
      the first as its parent. This combinator is
      needed to intervene in the construction of
      the display (list of environment pointers)
      to ensure that the first closure is passed
      as a display variable to the second, so that
      the class constructor can refer to the class object
      as its parent, that is, so references to member variables
      resolve correctly to the instance object.

      NOTE: the stack version applies to the constructor procedure
      NOT the class, which is always heaped
    *)
   ]

and bexpr_t =
  [
  | `BEXPR_parse of tbexpr_t * int list
  | `BEXPR_deref of tbexpr_t
  | `BEXPR_name of bid_t * btypecode_t list
  | `BEXPR_ref of bid_t * btypecode_t list
  | `BEXPR_likely of tbexpr_t
  | `BEXPR_unlikely of tbexpr_t
  | `BEXPR_new of tbexpr_t
  | `BEXPR_not of tbexpr_t
  | `BEXPR_literal of literal_t
  | `BEXPR_apply of tbexpr_t * tbexpr_t
  | `BEXPR_apply_prim of bid_t * btypecode_t list * tbexpr_t
  | `BEXPR_apply_direct of bid_t * btypecode_t list * tbexpr_t
  | `BEXPR_apply_stack of bid_t * btypecode_t list * tbexpr_t
  | `BEXPR_apply_method_direct of tbexpr_t * bid_t * btypecode_t list * tbexpr_t
  | `BEXPR_apply_method_stack of tbexpr_t * bid_t * btypecode_t list * tbexpr_t
  | `BEXPR_apply_struct of bid_t * btypecode_t list * tbexpr_t

  | `BEXPR_tuple of tbexpr_t list
  | `BEXPR_record of (string * tbexpr_t) list
  | `BEXPR_variant of string * tbexpr_t
  | `BEXPR_get_n of int * tbexpr_t (* tuple projection *)
  | `BEXPR_get_named of int * tbexpr_t (* struct/class projection *)
  | `BEXPR_closure of bid_t * btypecode_t list
  | `BEXPR_method_closure of tbexpr_t * bid_t * btypecode_t list
  | `BEXPR_case of int * btypecode_t
  | `BEXPR_match_case of int * tbexpr_t
  | `BEXPR_case_arg of int * tbexpr_t
  | `BEXPR_case_index of tbexpr_t
  | `BEXPR_expr of string * btypecode_t
  | `BEXPR_range_check of tbexpr_t * tbexpr_t * tbexpr_t
  | `BEXPR_coerce of tbexpr_t * btypecode_t
  ]

and tbexpr_t = bexpr_t * btypecode_t

and glr_symbol_t = [`Term of int | `Nonterm of int list]
and bglr_entry_t = string option * glr_symbol_t
and bproduction_t = bglr_entry_t list

and bparameter_t = {pkind:param_kind_t; pid:string; pindex:int; ptyp:btypecode_t}
and breqs_t = (bid_t * btypecode_t list) list
and bvs_t = (string * int) list
and bparams_t = bparameter_t list * tbexpr_t option
and bclass_member_t = [
  | `BMemberVal of id_t * btypecode_t
  | `BMemberVar of id_t * btypecode_t
  | `BMemberFun of id_t * bvs_t * btypecode_t
  | `BMemberProc of id_t * bvs_t * btypecode_t
  | `BMemberCtor of id_t * btypecode_t
]

and btype_qual_t = [
  | base_type_qual_t
  | `Bound_needs_shape of btypecode_t
]

and bbdcl_t =
  [
  | `BBDCL_function of   property_t list * bvs_t * bparams_t * btypecode_t * bexe_t list
  | `BBDCL_procedure of  property_t list * bvs_t * bparams_t * bexe_t list
  | `BBDCL_val of        bvs_t * btypecode_t
  | `BBDCL_var of        bvs_t * btypecode_t
  | `BBDCL_ref of        bvs_t * btypecode_t
  | `BBDCL_tmp of        bvs_t * btypecode_t
  | `BBDCL_glr of        property_t list * bvs_t * btypecode_t * (bproduction_t * bexe_t list)
  | `BBDCL_regmatch of   property_t list * bvs_t * bparams_t * btypecode_t * regular_args_t
  | `BBDCL_reglex of     property_t list * bvs_t * bparams_t * int * btypecode_t * regular_args_t

  (* binding structures [prolog] *)
  | `BBDCL_newtype of    bvs_t * btypecode_t
  | `BBDCL_abs of        bvs_t * btype_qual_t list * c_t * breqs_t
  | `BBDCL_const of      property_t list * bvs_t * btypecode_t * c_t * breqs_t
  | `BBDCL_fun of        property_t list * bvs_t * btypecode_t list * btypecode_t * c_t  * breqs_t * prec_t
  | `BBDCL_callback of   property_t list * bvs_t * btypecode_t list * btypecode_t list * int * btypecode_t * breqs_t * prec_t
  | `BBDCL_proc of       property_t list * bvs_t * btypecode_t list * c_t  * breqs_t
  | `BBDCL_insert of     bvs_t * c_t * ikind_t * breqs_t

  | `BBDCL_union of      bvs_t * (id_t * int * btypecode_t) list
  | `BBDCL_struct of     bvs_t * (id_t * btypecode_t) list
  | `BBDCL_cstruct of     bvs_t * (id_t * btypecode_t) list
  | `BBDCL_cclass of     bvs_t * bclass_member_t list
  | `BBDCL_class of      property_t list * bvs_t
  | `BBDCL_typeclass of  property_t list * bvs_t
  | `BBDCL_instance of   property_t list *
                         bvs_t *
                         btypecode_t (* constraint *) *
                         bid_t *
                         btypecode_t list
  | `BBDCL_nonconst_ctor of bvs_t * int * btypecode_t * int * btypecode_t *
                         bvs_t * btypecode_t (* existentials and constraint for GADTs *)
  ]

and baxiom_method_t = [`BPredicate of tbexpr_t | `BEquation of tbexpr_t * tbexpr_t]
and reduction_t = id_t * bvs_t * bparameter_t list * tbexpr_t * tbexpr_t
and axiom_t = id_t * range_srcref * int option * axiom_kind_t * bvs_t * bparams_t * baxiom_method_t

and typevarmap_t = (int,btypecode_t) Hashtbl.t

type env_t = (bid_t * id_t * name_map_t * name_map_t list) list

type symbol_definition_t =
  [
  | `SYMDEF_newtype of typecode_t
  | `SYMDEF_abs of type_qual_t list * c_t * named_req_expr_t
  | `SYMDEF_regdef of regexp_t
  | `SYMDEF_regmatch of params_t * (regexp_t * expr_t) list
  | `SYMDEF_reglex of params_t * int * (regexp_t * expr_t) list
  | `SYMDEF_glr of typecode_t * (reduced_production_t * sexe_t list)

  | `SYMDEF_parameter of  param_kind_t * typecode_t
  | `SYMDEF_typevar of typecode_t (* usually type TYPE *)

  | `SYMDEF_axiom of params_t * axiom_method_t
  | `SYMDEF_lemma of params_t * axiom_method_t
  | `SYMDEF_reduce of parameter_t list * expr_t * expr_t
  | `SYMDEF_function of params_t * typecode_t * property_t list * sexe_t list

  | `SYMDEF_match_check of  pattern_t * (string *int)
  | `SYMDEF_module
  | `SYMDEF_class

  | `SYMDEF_const_ctor of int * typecode_t * int * ivs_list_t
  | `SYMDEF_nonconst_ctor of int * typecode_t * int * ivs_list_t * typecode_t

  | `SYMDEF_const of property_t list * typecode_t * c_t * named_req_expr_t
  | `SYMDEF_var of  typecode_t
  | `SYMDEF_val of  typecode_t
  | `SYMDEF_ref of  typecode_t
  | `SYMDEF_lazy of  typecode_t * expr_t
  | `SYMDEF_fun of property_t list * typecode_t list * typecode_t * c_t  * named_req_expr_t * prec_t
  | `SYMDEF_callback of property_t list * typecode_t list * typecode_t * named_req_expr_t
  | `SYMDEF_insert of c_t  * ikind_t * named_req_expr_t
  | `SYMDEF_union of  (id_t * int *  vs_list_t * typecode_t) list
  | `SYMDEF_struct of  (id_t * typecode_t) list
  | `SYMDEF_cstruct of  (id_t * typecode_t) list
  | `SYMDEF_cclass of  class_member_t list
  | `SYMDEF_typeclass
  | `SYMDEF_type_alias of   typecode_t
  | `SYMDEF_inherit of   qualified_name_t
  | `SYMDEF_inherit_fun of   qualified_name_t
  | `SYMDEF_instance of qualified_name_t
  ]

type symbol_data_t = {
  id:string;
  sr:range_srcref;
  parent:int option;
  vs:ivs_list_t;
  pubmap:name_map_t;
  privmap:name_map_t;
  dirs:dir_t list;
  symdef:symbol_definition_t;
}

type symbol_table_t = (int, symbol_data_t) Hashtbl.t

type symbol_data3_t = string * int option * range_srcref * bbdcl_t
type fully_bound_symbol_table_t = (int, symbol_data3_t) Hashtbl.t

type type_registry_t = (btypecode_t,int) Hashtbl.t
