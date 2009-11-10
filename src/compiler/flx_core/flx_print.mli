(** Print module.
 *
 * Routines to print various terms. *)

open Flx_ast
open Flx_types

val string_of_bid : bid_t -> string
val string_of_bidset : BidSet.t -> string

val string_of_typecode : typecode_t -> string
val string_of_maybe_typecode : typecode_t -> string
val string_of_btypecode : Flx_sym_table.t -> btypecode_t -> string
val sbt: Flx_sym_table.t -> btypecode_t -> string
val special_string_of_typecode : typecode_t -> string
val string_of_expr : expr_t -> string
val string_of_bound_expression :
  Flx_sym_table.t ->
  bsym_table_t ->
  tbexpr_t ->
  string
val string_of_bound_expression_with_type :
  Flx_sym_table.t ->
  bsym_table_t ->
  tbexpr_t ->
  string
val sbe:
  Flx_sym_table.t ->
  bsym_table_t ->
  tbexpr_t ->
  string
val tsbe:
  Flx_sym_table.t ->
  bsym_table_t ->
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
val string_of_asm : int -> asm_t -> string
val string_of_desugared : asm_t list -> string
val string_of_suffixed_name : suffixed_name_t -> string
val string_of_qualified_name : qualified_name_t -> string
val string_of_dcl : int -> id_t -> bid_t option -> vs_list_t -> dcl_t -> string
val string_of_bexe : Flx_sym_table.t -> bsym_table_t -> int -> bexe_t -> string
val sbx: Flx_sym_table.t -> bsym_table_t -> bexe_t -> string
val string_of_exe : int -> exe_t -> string
val qualified_name_of_index : Flx_sym_table.t -> bid_t -> string
val qualified_name_of_bindex :
  Flx_sym_table.t ->
  bsym_table_t ->
  bid_t ->
  string
val string_of_bbdcl :
  Flx_sym_table.t ->
  bsym_table_t ->
  bbdcl_t ->
  bid_t ->
  string

val string_of_symdef :
  symbol_definition_t -> string -> ivs_list_t ->
  string

(** [string_of_entry_kind entry-kind] converts the [entry-kind] to a string. *)
val string_of_entry_kind:
  entry_kind_t -> string

val full_string_of_entry_kind:
  Flx_sym_table.t -> entry_kind_t -> string

(** [string_of_entry_set entry-set] converts the [entry-set] to a string. *)
val string_of_entry_set:
  entry_set_t -> string

val full_string_of_entry_set:
  Flx_sym_table.t -> entry_set_t -> string

val print_name_table:
  Flx_sym_table.t -> name_map_t -> unit

val string_of_myentry:
  Flx_sym_table.t -> entry_kind_t -> string

val string_of_varlist:
  Flx_sym_table.t ->
  (int * btypecode_t) list ->
  string

val print_env: env_t -> unit

val print_env_short: env_t -> unit

val print_functions:
  Flx_sym_table.t ->
  bsym_table_t ->
  unit

val print_symbols:
  Flx_sym_table.t ->
  bsym_table_t ->
  unit

val print_function_body:
  Flx_sym_table.t ->
  bsym_table_t ->
  string -> bid_t -> bvs_t -> bparams_t -> bexe_t list -> bid_t option ->
  unit

val print_function:
  Flx_sym_table.t ->
  bsym_table_t ->
  bid_t ->
  unit

val print_sym_table:
  Flx_sym_table.t ->
  unit

val print_bsym_table:
  Flx_sym_table.t ->
  bsym_table_t ->
  unit

(** [string_of_name_map name-map] converts the [name-map] to a string. *)
val string_of_name_map: name_map_t -> string

val string_of_vs: vs_list_t -> string
val string_of_ivs: ivs_list_t -> string

val string_of_ast_term: int -> ast_term_t -> string
val string_of_string: string -> string
val string_of_bquals: Flx_sym_table.t -> btype_qual_t list -> string
val string_of_bvs: bvs_t -> string
val string_of_code_spec: code_spec_t -> string
val string_of_raw_reqs: raw_req_expr_t -> string
val string_of_ikind: ikind_t -> string

val string_of_quals: type_qual_t list -> string
