module TypecodeSet : Set.S with type elt = Flx_ast.typecode_t

type generic_rebind_entry_t = Flx_types.bid_t * Flx_btype.t
type generic_cache_t =
  (Flx_types.bid_t * generic_rebind_entry_t list,  Flx_btype.overload_result) Hashtbl.t


type typecodeset_t = TypecodeSet.t
val typecodeset_of_list : TypecodeSet.elt list -> TypecodeSet.t
val typecodeset_map :
  (TypecodeSet.elt -> TypecodeSet.elt) -> TypecodeSet.t -> TypecodeSet.t

(* generic entity instances: functions, variables *)
type instance_registry_t = (Flx_types.bid_t * Flx_btype.t list, Flx_types.bid_t) Hashtbl.t

type type_registry_t = (Flx_btype.t * Flx_types.bid_t) list
type type_array_as_tuple_registry_t = (Flx_types.bid_t, unit) Hashtbl.t

type array_sum_offset_data_t = string * int list (* name, values *)
type array_sum_offset_table_t = (Flx_btype.t, array_sum_offset_data_t) Hashtbl.t
type power_table_t= (int, int list) Hashtbl.t

type typevarmap_t = (Flx_types.bid_t, Flx_btype.t) Hashtbl.t

type baxiom_method_t = [
  | `BPredicate of Flx_bexpr.t
  | `BEquation of Flx_bexpr.t * Flx_bexpr.t
]

type env_t = (
  Flx_types.bid_t *           (** container index *)
  string *                    (** name *)
  Flx_btype.name_map_t *      (** primary symbol map *)
  Flx_btype.name_map_t list * (** directives *)
  Flx_ast.typecode_t          (** type constraint *)
) list

type axiom_t =
  Flx_id.t *
  Flx_srcref.t *
  Flx_types.bid_t option *
  Flx_ast.axiom_kind_t *
  Flx_types.bvs_t *
  Flx_bparams.t *
  baxiom_method_t

type reduction_t =
  Flx_id.t *
  Flx_types.bvs_t *
  Flx_bparameter.t list *
  Flx_bexpr.t *
  Flx_bexpr.t

type sym_state_t =
{
  counter : Flx_types.bid_t ref;
  mutable varmap : typevarmap_t;
  mutable ticache : (Flx_types.bid_t, Flx_btype.t) Hashtbl.t;
  generic_cache: generic_cache_t;
  env_cache : (Flx_types.bid_t, env_t) Hashtbl.t;
  mutable registry : type_registry_t;
  array_as_tuple_registry : type_array_as_tuple_registry_t;
  compiler_options : Flx_options.t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : Flx_types.BidSet.t ref;
  quick_names : (string, (Flx_types.bid_t * Flx_btype.t list)) Hashtbl.t;
  mutable bifaces : Flx_btype.biface_t list;
  reductions : reduction_t list ref;
  axioms : axiom_t list ref;
  variant_map: (Flx_btype.t * Flx_btype.t, Flx_types.bid_t) Hashtbl.t;
  mutable virtual_to_instances: (Flx_types.bid_t, (Flx_types.bvs_t * Flx_btype.t * Flx_btype.t list * Flx_types.bid_t) list) Hashtbl.t;
  mutable instances_of_typeclass: (Flx_types.bid_t, (Flx_types.bid_t * (Flx_types.bvs_t * Flx_btype.t * Flx_btype.t list)) list) Hashtbl.t;
  transient_specialisation_cache: (Flx_types.bid_t , Flx_types.bid_t ) Hashtbl.t;
  array_sum_offset_table: array_sum_offset_table_t;
  power_table: power_table_t;
}

val make_syms: Flx_options.t -> sym_state_t

val fresh_bid: Flx_types.bid_t ref -> Flx_types.bid_t

val iter_bids:
  (Flx_types.bid_t -> unit) ->
  Flx_types.bid_t ->
  Flx_types.bid_t ->
  unit

