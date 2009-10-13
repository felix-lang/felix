module VarMap : Map.S with type key = string

type varmap_t = string VarMap.t

module TypecodeSet : Set.S with type elt = Flx_ast.typecode_t

type typecodeset_t = TypecodeSet.t
val typecodeset_of_list : TypecodeSet.elt list -> TypecodeSet.t
val typecodeset_map :
  (TypecodeSet.elt -> TypecodeSet.elt) -> TypecodeSet.t -> TypecodeSet.t

(* generic entity instances: functions, variables *)
type instance_registry_t = (Flx_types.bid_t * Flx_types.btypecode_t list, Flx_types.bid_t) Hashtbl.t

type felix_compiler_options_t =
{
  print_flag: bool;
  debug : bool;
  optimise : bool;
  trace : bool;
  include_dirs : string list;
  files : string list;
  raw_options: (string * string) list;
  reverse_return_parity: bool;
  max_inline_length : int;
  compile_only : bool;
  force_recompile : bool;
  with_comments : bool;
  document_grammar: bool;
  document_typeclass: bool;
  mangle_names : bool;
  generate_axiom_checks : bool;
  auto_imports : string list;
  cache_dir : string option;
  output_dir : string option;
}

type sym_state_t =
{
  sym_table : Flx_types.sym_table_t;
  counter : Flx_types.bid_t ref;
  varmap : Flx_types.typevarmap_t;
  ticache : (Flx_types.bid_t, Flx_types.btypecode_t) Hashtbl.t;
  env_cache : (Flx_types.bid_t, Flx_types.env_t) Hashtbl.t;
  registry : Flx_types.type_registry_t;
  compiler_options : felix_compiler_options_t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : Flx_types.BidSet.t ref;
  quick_names : (string, (Flx_types.bid_t * Flx_types.btypecode_t list)) Hashtbl.t;
  mutable bifaces : Flx_types.biface_t list;
  mutable reductions : Flx_types.reduction_t list;
  mutable axioms : Flx_types.axiom_t list;
  variant_map: (Flx_types.btypecode_t * Flx_types.btypecode_t, Flx_types.bid_t) Hashtbl.t;
  typeclass_to_instance: (Flx_types.bid_t, (Flx_types.bvs_t * Flx_types.btypecode_t * Flx_types.btypecode_t list * Flx_types.bid_t) list) Hashtbl.t;
  instances_of_typeclass: (Flx_types.bid_t, (Flx_types.bid_t * (Flx_types.bvs_t * Flx_types.btypecode_t * Flx_types.btypecode_t list)) list) Hashtbl.t;
  transient_specialisation_cache: (Flx_types.bid_t * Flx_types.btypecode_t list, Flx_types.bid_t * Flx_types.btypecode_t list) Hashtbl.t;
}

val make_syms: felix_compiler_options_t -> sym_state_t

module Drules : Map.S with type key = string
