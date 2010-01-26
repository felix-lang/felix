module TypecodeSet : Set.S with type elt = Flx_ast.typecode_t

type typecodeset_t = TypecodeSet.t
val typecodeset_of_list : TypecodeSet.elt list -> TypecodeSet.t
val typecodeset_map :
  (TypecodeSet.elt -> TypecodeSet.elt) -> TypecodeSet.t -> TypecodeSet.t

(* generic entity instances: functions, variables *)
type instance_registry_t = (Flx_types.bid_t * Flx_types.btypecode_t list, Flx_types.bid_t) Hashtbl.t

type typevarmap_t = (Flx_types.bid_t, Flx_types.btypecode_t) Hashtbl.t

type baxiom_method_t = [
  | `BPredicate of Flx_types.tbexpr_t
  | `BEquation of Flx_types.tbexpr_t * Flx_types.tbexpr_t
]

type axiom_t =
  Flx_ast.id_t *
  Flx_srcref.t *
  Flx_types.bid_t option *
  Flx_ast.axiom_kind_t *
  Flx_types.bvs_t *
  Flx_bparams.t *
  baxiom_method_t

type reduction_t =
  Flx_ast.id_t *
  Flx_types.bvs_t *
  Flx_bparameter.t list *
  Flx_types.tbexpr_t *
  Flx_types.tbexpr_t

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
  counter : Flx_types.bid_t ref;
  varmap : typevarmap_t;
  ticache : (Flx_types.bid_t, Flx_types.btypecode_t) Hashtbl.t;
  env_cache : (Flx_types.bid_t, Flx_types.env_t) Hashtbl.t;
  registry : Flx_types.type_registry_t;
  compiler_options : felix_compiler_options_t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : Flx_types.BidSet.t ref;
  quick_names : (string, (Flx_types.bid_t * Flx_types.btypecode_t list)) Hashtbl.t;
  mutable bifaces : Flx_types.biface_t list;
  mutable reductions : reduction_t list;
  mutable axioms : axiom_t list;
  variant_map: (Flx_types.btypecode_t * Flx_types.btypecode_t, Flx_types.bid_t) Hashtbl.t;
  typeclass_to_instance: (Flx_types.bid_t, (Flx_types.bvs_t * Flx_types.btypecode_t * Flx_types.btypecode_t list * Flx_types.bid_t) list) Hashtbl.t;
  instances_of_typeclass: (Flx_types.bid_t, (Flx_types.bid_t * (Flx_types.bvs_t * Flx_types.btypecode_t * Flx_types.btypecode_t list)) list) Hashtbl.t;
  transient_specialisation_cache: (Flx_types.bid_t * Flx_types.btypecode_t list, Flx_types.bid_t * Flx_types.btypecode_t list) Hashtbl.t;
}

val make_syms: felix_compiler_options_t -> sym_state_t

val fresh_bid: Flx_types.bid_t ref -> Flx_types.bid_t

val iter_bids:
  (Flx_types.bid_t -> unit) ->
  Flx_types.bid_t ref ->
  Flx_types.bid_t ->
  unit

module Drules : Map.S with type key = string
