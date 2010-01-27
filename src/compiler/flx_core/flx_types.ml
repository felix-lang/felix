open Format
open Flx_format

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

(** bid_t is the bound symbol index type, which is used to uniquely identifies
 * the symbol. *)
type bid_t = int

let dummy_bid = 0

(** Create a set type for bound symbol indices. *)
module BidSet = Flx_set.Make (
  struct
    type t = bid_t
    let compare = compare
    let print = pp_print_int
  end
)

(** Convert a list of bids into a bid set. *)
let bidset_of_list ii =
  List.fold_left (fun ii i -> BidSet.add i ii) BidSet.empty ii

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
and btypecode_t = 
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

type entry_set_t =
  | FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t

type name_map_t = (string, entry_set_t) Hashtbl.t

type biface_t =
  | BIFACE_export_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_python_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_type of Flx_srcref.t * btypecode_t * string

type bexpr_t =
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

type breqs_t = (bid_t * btypecode_t list) list
type bvs_t = (string * bid_t) list

type btype_qual_t = [
  | base_type_qual_t
  | `Bound_needs_shape of btypecode_t
]

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
  | SYMDEF_insert of code_spec_t  * ikind_t * named_req_expr_t
  | SYMDEF_union of (id_t * int *  vs_list_t * typecode_t) list
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
let btyp_void = BTYP_void

(** Construct a BTYP_sum type. *)
let btyp_sum ts =
  BTYP_sum ts

(** Construct a BTYP_unitsum type. *)
let btyp_unitsum n =
  BTYP_unitsum n

(** Construct a BTYP_intersect type. *)
let btyp_intersect ts =
  BTYP_intersect ts

let btyp_inst (bid, ts) =
  BTYP_inst (bid, ts)

(** Construct a BTYP_tuple type. *)
let btyp_tuple = function
  | [] -> BTYP_tuple []
  | [t] -> t
  | (head :: tail) as ts ->
      (* If all the types are the same, reduce the type to a BTYP_array. *)
      try
        List.iter (fun t -> if t <> head then raise Not_found) tail;
        BTYP_array (head, (BTYP_unitsum (List.length ts)))
      with Not_found ->
        BTYP_tuple ts

(** Construct a BTYP_array type. *)
let btyp_array (t, n) =
  match n with
  | BTYP_void
  | BTYP_unitsum 0 -> BTYP_tuple []
  | BTYP_unitsum 1 -> t
  | _ -> BTYP_array (t, n)

(** Construct a BTYP_record type. *)
let btyp_record = function
  | [] -> BTYP_tuple []
  | ts ->
      (* Make sure all the elements are sorted by name. *)
      let ts = List.sort compare ts in
      BTYP_record ts

(** Construct a BTYP_variant type. *)
let btyp_variant = function
  | [] -> BTYP_tuple []
  | ts ->
      (* Make sure all the elements are sorted by name. *)
      let ts = List.sort compare ts in
      BTYP_variant ts

(** Construct a BTYP_pointer type. *)
let btyp_pointer ts =
  BTYP_pointer ts

(** Construct a BTYP_function type. *)
let btyp_function (args, ret) =
  BTYP_function (args, ret)

(** Construct a BTYP_cfunction type. *)
let btyp_cfunction (args, ret) =
  BTYP_cfunction (args, ret)

(** Construct a BTYP_fix type. *)
let btyp_fix i =
  BTYP_fix i

(** Construct a BTYP_type type. *)
let btyp_type i =
  BTYP_type i

(** Construct a BTYP_type_tuple type. *)
let btyp_type_tuple ts =
  BTYP_type_tuple ts

(** Construct a BTYP_function type. *)
let btyp_type_function (args, ret, body) =
  BTYP_type_function (args, ret, body)

(** Construct a BTYP_type_var type. *)
let btyp_type_var (bid, t) =
  BTYP_type_var (bid, t)

(** Construct a BTYP_type_apply type. *)
let btyp_type_apply (f, a) =
  BTYP_type_apply (f, a)

(** Construct a BTYP_type_match type. *)
let btyp_type_match (t, ps) =
  BTYP_type_match (t, ps)

(** Construct a BTYP_type_set type. *)
let btyp_type_set ts =
  BTYP_type_set ts

(** Construct a BTYP_type_set_union type. *)
let btyp_type_set_union ts =
  BTYP_type_set_union ts

(** Construct a BTYP_type_set_intersection type. *)
let btyp_type_set_intersection ts =
  BTYP_type_set_intersection ts

(* -------------------------------------------------------------------------- *)

let ts_of_bexpr = function
  | BEXPR_name (_, ts)
  | BEXPR_closure (_, ts)
  | BEXPR_ref (_, ts)
  | BEXPR_apply_prim (_, ts, _)
  | BEXPR_apply_direct (_, ts, _)
  | BEXPR_apply_struct (_, ts, _) -> ts
  | _ -> []

(* -------------------------------------------------------------------------- *)

let print_bid = pp_print_int

(** Prints out a bvs_t to a formatter. *)
let print_bvs f xs =
  Flx_list.print begin fun f (name, bid) ->
    Flx_format.print_tuple2 f
      Flx_format.print_string name
      print_bid bid
  end f xs

let rec print_bexpr f = function
  | BEXPR_deref e ->
      Flx_format.print_variant1 f "BEXPR_deref" print_tbexpr e
  | BEXPR_name (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_name" print_bid bid print_btypes ts
  | BEXPR_ref (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_ref" print_bid bid print_btypes ts
  | BEXPR_likely e ->
      Flx_format.print_variant1 f "BEXPR_likely" print_tbexpr e
  | BEXPR_unlikely e ->
      Flx_format.print_variant1 f "BEXPR_unlikely" print_tbexpr e
  | BEXPR_address e ->
      Flx_format.print_variant1 f "BEXPR_address" print_tbexpr e
  | BEXPR_new e ->
      Flx_format.print_variant1 f "BEXPR_new" print_tbexpr e
  | BEXPR_literal l ->
      Flx_format.print_variant1 f "BEXPR_literal" print_literal l
  | BEXPR_apply (e1, e2) ->
      Flx_format.print_variant2 f "BEXPR_apply" print_tbexpr e1 print_tbexpr e2
  | BEXPR_apply_prim (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_prim"
        print_bid bid
        print_btypes ts
        print_tbexpr e
  | BEXPR_apply_direct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_direct"
        print_bid bid
        print_btypes ts
        print_tbexpr e
  | BEXPR_apply_stack (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_stack"
        print_bid bid
        print_btypes ts
        print_tbexpr e
  | BEXPR_apply_struct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_struct"
        print_bid bid
        print_btypes ts
        print_tbexpr e
  | BEXPR_tuple es ->
      Flx_format.print_variant1 f "BEXPR_tuple" (Flx_list.print print_tbexpr) es
  | BEXPR_record es ->
      Flx_format.print_variant1 f "BEXPR_record"
        (Flx_list.print begin fun f (s, e) ->
          Flx_format.print_tuple2 f print_string s print_tbexpr e
        end)
        es
  | BEXPR_variant (s, e) ->
      Flx_format.print_variant2 f "BEXPR_variant" print_string s print_tbexpr e
  | BEXPR_get_n (i, e) ->
      Flx_format.print_variant2 f "BEXPR_get_n" pp_print_int i print_tbexpr e
  | BEXPR_closure (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_closure" print_bid bid print_btypes ts
  | BEXPR_case (i, t) ->
      Flx_format.print_variant2 f "BEXPR_match_case"
        pp_print_int i
        print_btype t
  | BEXPR_match_case (i, e) ->
      Flx_format.print_variant2 f "BEXPR_match_case"
        pp_print_int i
        print_tbexpr e
  | BEXPR_case_arg (i, e) ->
      Flx_format.print_variant2 f "BEXPR_case_arg" pp_print_int i print_tbexpr e
  | BEXPR_case_index e ->
      Flx_format.print_variant1 f "BEXPR_case_index" print_tbexpr e
  | BEXPR_expr (s, t) ->
      Flx_format.print_variant2 f "BEXPR_closure"
        Flx_format.print_string s
        print_btype t
  | BEXPR_range_check (e1, e2, e3) ->
      Flx_format.print_variant3 f "BEXPR_range_check"
        print_tbexpr e1
        print_tbexpr e2
        print_tbexpr e3
  | BEXPR_coerce (e, t) ->
      Flx_format.print_variant2 f "BEXPR_coerce"
        print_tbexpr e
        print_btype t

and print_tbexpr f (e, t) =
  Flx_format.print_tuple2 f print_bexpr e print_btype t

and print_btpattern f pat =
  Flx_format.print_record3 f
    "pattern" print_btype pat.pattern
    "pattern_vars" BidSet.print pat.pattern_vars
    "assignments" print_bid_btypes pat.assignments

and print_btype f =
  let print_string_btypes =
    Flx_list.print begin fun f (s, btype) ->
      Flx_format.print_tuple2 f Flx_format.print_string s print_btype btype
    end
  in
  function
  | BTYP_sum ts ->
      print_variant1 f "BTYP_sum" print_btypes ts
  | BTYP_unitsum n ->
      print_variant1 f "BTYP_unitsum" pp_print_int n
  | BTYP_intersect ts ->
      print_variant1 f "BTYP_sum" print_btypes ts
  | BTYP_inst (i, ts) ->
      print_variant2 f "BTYP_inst" pp_print_int i print_btypes ts
  | BTYP_tuple ts ->
      print_variant1 f "BTYP_tuple" print_btypes ts
  | BTYP_array (t, n) ->
      print_variant2 f "BTYP_array" print_btype t print_btype n
  | BTYP_record ls ->
      print_variant1 f "BTYP_record" print_string_btypes ls
  | BTYP_variant ls ->
      print_variant1 f "BTYP_variant" print_string_btypes ls
  | BTYP_pointer t ->
      print_variant1 f "BTYP_pointer" print_btype t
  | BTYP_function (a, r) ->
      print_variant2 f "BTYP_function" print_btype a print_btype r
  | BTYP_cfunction (a, r) ->
      print_variant2 f "BTYP_cfunction" print_btype a print_btype r
  | BTYP_void ->
      print_variant0 f "BTYP_void"
  | BTYP_fix n ->
      print_variant1 f "BTYP_fix" pp_print_int n
  | BTYP_type n ->
      print_variant1 f "BTYP_type" pp_print_int n
  | BTYP_type_tuple ts ->
      print_variant1 f "BTYP_type_tuple" print_btypes ts
  | BTYP_type_function (a, r, b) ->
      print_variant3 f "BTYP_type_function"
        print_bid_btypes a
        print_btype r
        print_btype b
  | BTYP_type_var (i, t) ->
      print_variant2 f "BTYP_type_var" pp_print_int i print_btype t
  | BTYP_type_apply (t1, t2) ->
      print_variant2 f "BTYP_type_apply" print_btype t1 print_btype t2
  | BTYP_type_match (t, ps) ->
      print_variant2 f "BTYP_type_match"
        print_btype t
        (Flx_list.print begin fun f (pat, t) ->
          Flx_format.print_tuple2 f
            print_btpattern pat
            print_btype t
        end) ps
  | BTYP_type_set ts ->
      print_variant1 f "BTYP_type_set" print_btypes ts
  | BTYP_type_set_union ts ->
      print_variant1 f "BTYP_type_set_union" print_btypes ts
  | BTYP_type_set_intersection ts ->
      print_variant1 f "BTYP_type_set_intersection" print_btypes ts

and print_btypes f ts =
  Flx_list.print print_btype f ts

and print_bid_btypes btypes =
  Flx_list.print begin fun f (bid, btype) ->
    Flx_format.print_tuple2 f
      Format.pp_print_int bid
      print_btype btype
  end btypes

let print_btype_qual f = function
  | #base_type_qual_t as qual ->
      print_base_type_qual f qual
  | `Bound_needs_shape t ->
      print_variant1 f "`Bound_needs_shape"
        print_btype t

let print_breqs f breqs =
  Flx_list.print begin fun f (bid, ts) ->
    print_tuple2 f print_bid bid print_btypes ts
  end f breqs
