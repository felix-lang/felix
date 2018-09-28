open Flx_set
open Flx_types
open List
open Flx_bid

type generic_rebind_entry_t = bid_t * Flx_btype.t
type generic_cache_t =
  (bid_t * generic_rebind_entry_t list,  Flx_btype.overload_result) Hashtbl.t

(* generic entity instances: functions, variables *)
type instance_registry_t = (
  bid_t * Flx_btype.t list,
  bid_t
) Hashtbl.t

type type_registry_t = (Flx_btype.t * bid_t) list
type type_array_as_tuple_registry_t = (bid_t, unit) Hashtbl.t

(* used when indexing concatenated arrays to find the offset
  of the n'th array.
*)
type array_sum_offset_data_t = string * int list (* name, values *)
type array_sum_offset_table_t = (Flx_btype.t, array_sum_offset_data_t) Hashtbl.t
type power_table_t = (int,int list) Hashtbl.t

type typevarmap_t = (bid_t, Flx_btype.t) Hashtbl.t

type baxiom_method_t = [
  | `BPredicate of Flx_bexpr.t
  | `BEquation of Flx_bexpr.t * Flx_bexpr.t
]

type env_t = (
  bid_t *           (** container index *)
  string *                    (** name *)
  Flx_name_map.name_map_t *      (** primary symbol map *)
  Flx_name_map.name_map_t list * (** directives *)
  Flx_ast.typecode_t          (** type constraint *)
) list

type axiom_t =
  Flx_id.t *
  Flx_srcref.t *
  bid_t option *
  Flx_ast.axiom_kind_t *
  Flx_kind.bvs_t *
  Flx_bparams.t *
  baxiom_method_t

type reduction_case_t = 
    Flx_kind.bvs_t *
    Flx_bparameter.t list *
    Flx_bexpr.t *
    Flx_bexpr.t

type reduction_t =
  Flx_id.t * reduction_case_t list


type sym_state_t =
{
  counter : bid_t ref;
  mutable varmap : typevarmap_t;
  mutable ticache : (bid_t, Flx_btype.t) Hashtbl.t;
  generic_cache: generic_cache_t;
  env_cache : (bid_t, env_t) Hashtbl.t;
  mutable registry : type_registry_t;
  array_as_tuple_registry : type_array_as_tuple_registry_t;
  compiler_options : Flx_options.t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : BidSet.t ref;
  quick_names : (string, (bid_t * Flx_btype.t list)) Hashtbl.t;
  mutable bifaces : Flx_btype.biface_t list;
  reductions : reduction_t list ref;
  axioms: axiom_t list ref;
  variant_map: (Flx_btype.t * Flx_btype.t, bid_t) Hashtbl.t;
  mutable virtual_to_instances: (bid_t, (Flx_kind.bvs_t * Flx_btype.t * Flx_btype.t list * bid_t) list) Hashtbl.t;
  mutable instances_of_typeclass: (bid_t, (bid_t * (Flx_kind.bvs_t * Flx_btype.t * Flx_btype.t list)) list) Hashtbl.t;
  transient_specialisation_cache: (bid_t , bid_t) Hashtbl.t;
  array_sum_offset_table: array_sum_offset_table_t;
  power_table: power_table_t;
} 

let make_syms options =
  {
    counter = ref Flx_bid.start_counter;
    varmap = Hashtbl.create 97;
    ticache = Hashtbl.create 97;
    generic_cache = Hashtbl.create 97;
    env_cache = Hashtbl.create 97;
    registry = [];
    array_as_tuple_registry = Hashtbl.create 97;
    compiler_options = options;
    instances = Hashtbl.create 97;
    include_files = ref [];
    roots = ref BidSet.empty;
    quick_names = Hashtbl.create 97;
    bifaces = [];
    reductions = ref [];
    axioms = ref [];
    variant_map = Hashtbl.create 97;
    virtual_to_instances = Hashtbl.create 97;
    instances_of_typeclass = Hashtbl.create 97;
    transient_specialisation_cache = Hashtbl.create 97;
    array_sum_offset_table = Hashtbl.create 97;
    power_table = Hashtbl.create 97;
  }

module TypecodeSet = Set.Make(
  struct type t = Flx_ast.typecode_t let compare = compare end
)
type typecodeset_t = TypecodeSet.t

let typecodeset_of_list x =
  let rec tsol x = match x with
  | h :: t -> TypecodeSet.add h (tsol t)
  | [] -> TypecodeSet.empty
  in tsol x

let typecodeset_map f x = typecodeset_of_list (map f (TypecodeSet.elements x))


