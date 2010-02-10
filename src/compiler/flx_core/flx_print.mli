(** Print module.
 *
 * Routines to print various terms. *)

open Flx_ast
open Flx_types

val string_of_bid : bid_t -> string
val string_of_bidset : BidSet.t -> string

val string_of_typecode : typecode_t -> string
val string_of_maybe_typecode : typecode_t -> string
val string_of_btypecode : Flx_bsym_table.t -> Flx_btype.t -> string
val sbt: Flx_bsym_table.t -> Flx_btype.t -> string
val special_string_of_typecode : typecode_t -> string
val string_of_expr : expr_t -> string
val string_of_bound_expression :
  Flx_bsym_table.t ->
  Flx_bexpr.t ->
  string
val string_of_bound_expression_with_type :
  Flx_bsym_table.t ->
  Flx_bexpr.t ->
  string
val sbe:
  Flx_bsym_table.t ->
  Flx_bexpr.t ->
  string
val tsbe:
  Flx_bsym_table.t ->
  Flx_bexpr.t ->
  string

val string_of_properties : property_t list -> string

val string_of_pattern : pattern_t -> string
val string_of_tpattern : tpattern_t -> string
val string_of_literal : literal_t -> string
val string_of_parameters : params_t -> string
val string_of_arguments : expr_t list -> string
val string_of_statement : int -> statement_t -> string
val string_of_compilation_unit : compilation_unit_t -> string
val string_of_asm : int -> asm_t -> string
val string_of_desugared : asm_t list -> string
val string_of_suffixed_name : suffixed_name_t -> string
val string_of_qualified_name : qualified_name_t -> string
val string_of_dcl : int -> id_t -> bid_t option -> vs_list_t -> dcl_t -> string
val string_of_bexe : Flx_bsym_table.t -> int -> Flx_bexe.t -> string
val sbx: Flx_bsym_table.t -> Flx_bexe.t -> string
val string_of_exe : int -> exe_t -> string
val qualified_name_of_index : Flx_sym_table.t -> bid_t -> string
val qualified_name_of_bindex :
  Flx_bsym_table.t ->
  bid_t ->
  string
val string_of_bbdcl :
  Flx_bsym_table.t ->
  Flx_bbdcl.t ->
  bid_t ->
  string

val string_of_symdef :
  symbol_definition_t -> string -> ivs_list_t ->
  string

(** [string_of_entry_kind entry-kind] converts the [entry-kind] to a string. *)
val string_of_entry_kind:
  Flx_btype.entry_kind_t -> string

val full_string_of_entry_kind:
  Flx_bsym_table.t -> Flx_btype.entry_kind_t -> string

(** [string_of_entry_set entry-set] converts the [entry-set] to a string. *)
val string_of_entry_set:
  Flx_btype.entry_set_t -> string

val full_string_of_entry_set:
  Flx_bsym_table.t -> Flx_btype.entry_set_t -> string

val print_name_table:
  Flx_bsym_table.t -> Flx_btype.name_map_t -> unit

val string_of_myentry:
  Flx_bsym_table.t -> Flx_btype.entry_kind_t -> string

val string_of_varlist:
  Flx_bsym_table.t ->
  (int * Flx_btype.t) list ->
  string

val string_of_bsym:
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  string

val print_env: Flx_mtypes2.env_t -> unit

val print_env_short: Flx_mtypes2.env_t -> unit

val print_functions:
  Flx_bsym_table.t ->
  unit

val print_symbols:
  Flx_bsym_table.t ->
  unit

val print_function_body:
  Flx_bsym_table.t ->
  string ->
  bid_t ->
  bvs_t ->
  Flx_bparams.t ->
  Flx_bexe.t list ->
  unit

val print_function:
  Flx_bsym_table.t ->
  bid_t ->
  unit

val print_sym:
  Flx_sym_table.t ->
  Flx_types.bid_t ->
  unit

val print_sym_table:
  Flx_sym_table.t ->
  unit

val print_bsym:
  Flx_bsym_table.t ->
  Flx_types.bid_t ->
  unit

val print_bsym_table:
  Flx_bsym_table.t ->
  unit

(** [string_of_name_map name-map] converts the [name-map] to a string. *)
val string_of_name_map: Flx_btype.name_map_t -> string

val string_of_vs: vs_list_t -> string
val string_of_ivs: ivs_list_t -> string

val string_of_ast_term: int -> ast_term_t -> string
val string_of_string: string -> string
val string_of_bquals: Flx_bsym_table.t -> Flx_bbdcl.btype_qual_t list -> string
val string_of_bvs: bvs_t -> string
val string_of_code_spec: code_spec_t -> string
val string_of_raw_reqs: raw_req_expr_t -> string
val string_of_ikind: ikind_t -> string

val string_of_quals: type_qual_t list -> string
