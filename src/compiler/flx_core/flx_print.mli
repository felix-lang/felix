(** Print module.
 *
 * Routines to print various terms. *)

open Flx_ast
open Flx_types

val string_of_typecode : typecode_t -> string
val string_of_btypecode : symbol_table_t -> btypecode_t -> string
val sbt: symbol_table_t -> btypecode_t -> string
val special_string_of_typecode : typecode_t -> string
val string_of_expr : expr_t -> string
val string_of_bound_expression :
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  tbexpr_t ->
  string
val string_of_bound_expression_with_type :
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  tbexpr_t ->
  string
val sbe:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  tbexpr_t ->
  string
val tsbe:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  tbexpr_t ->
  string

val string_of_properties : property_t list -> string

val string_of_pattern : pattern_t -> string
val string_of_tpattern : tpattern_t -> string
val string_of_literal : literal_t -> string
val string_of_parameters : params_t -> string
val string_of_arguments : expr_t list -> string
val string_of_statement : int -> statement_t -> string
val string_of_compilation_unit : compilation_unit_t -> string
val string_of_desugared : asm_t list -> string
val string_of_suffixed_name : suffixed_name_t -> string
val string_of_qualified_name : qualified_name_t -> string
val string_of_dcl : int -> id_t -> int option -> vs_list_t -> dcl_t -> string
val string_of_bexe : symbol_table_t -> fully_bound_symbol_table_t -> int -> bexe_t -> string
val sbx: symbol_table_t -> fully_bound_symbol_table_t -> bexe_t -> string
val string_of_exe : int -> exe_t -> string
val qualified_name_of_index : symbol_table_t -> int -> string
val qualified_name_of_bindex :
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  int -> string
val string_of_bbdcl :
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  bbdcl_t ->
  int ->
  string

val string_of_symdef :
  symbol_definition_t -> string -> ivs_list_t ->
  string

val string_of_entry_kind:
  entry_kind_t -> string

val full_string_of_entry_kind:
  symbol_table_t -> entry_kind_t -> string

val string_of_entry_set:
  entry_set_t -> string

val full_string_of_entry_set:
  symbol_table_t -> entry_set_t -> string

val print_name_table:
  symbol_table_t -> name_map_t -> unit

val string_of_myentry:
  symbol_table_t -> entry_kind_t -> string

val string_of_varlist:
  symbol_table_t ->
  (int * btypecode_t) list ->
  string

val string_of_bigint: bigint -> string

val print_env: env_t -> unit
val print_env_short: env_t -> unit

val print_functions:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  unit

val print_symbols:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  unit

val print_function_body:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  string -> int -> bvs_t -> bparams_t -> bexe_t list -> int option ->
  unit

val print_function:
  symbol_table_t ->
  fully_bound_symbol_table_t ->
  int ->
  unit

val print_vs: vs_list_t -> string
val print_ivs: ivs_list_t -> string
val print_ivs_with_index: ivs_list_t -> string

val string_of_ast_term: int -> ast_term_t -> string
val string_of_string: string -> string
val string_of_bquals: symbol_table_t -> btype_qual_t list -> string
val print_bvs: bvs_t -> string
val string_of_code_spec: c_t -> string
val string_of_raw_reqs: raw_req_expr_t -> string
val string_of_ikind: ikind_t -> string

val string_of_quals: type_qual_t list -> string
