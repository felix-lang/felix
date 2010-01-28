open Flx_set
open Flx_types
open List

(* generic entity instances: functions, variables *)
type instance_registry_t = (bid_t * btypecode_t list, bid_t) Hashtbl.t

type typevarmap_t = (Flx_types.bid_t, Flx_types.btypecode_t) Hashtbl.t

type baxiom_method_t = [
  | `BPredicate of Flx_bexpr.t
  | `BEquation of Flx_bexpr.t * Flx_bexpr.t
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
  Flx_bexpr.t *
  Flx_bexpr.t

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
  counter : bid_t ref;
  varmap : typevarmap_t;
  ticache : (bid_t, btypecode_t) Hashtbl.t;
  env_cache : (bid_t, env_t) Hashtbl.t;
  registry : type_registry_t;
  compiler_options : felix_compiler_options_t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : BidSet.t ref;
  quick_names : (string, (bid_t * btypecode_t list)) Hashtbl.t;
  mutable bifaces : biface_t list;
  mutable reductions : reduction_t list;
  mutable axioms: axiom_t list;
  variant_map: (btypecode_t * btypecode_t, bid_t) Hashtbl.t;
  typeclass_to_instance: (bid_t, (bvs_t * btypecode_t * btypecode_t list * bid_t) list) Hashtbl.t;
  instances_of_typeclass: (bid_t, (bid_t * (bvs_t * btypecode_t * btypecode_t list)) list) Hashtbl.t;
  transient_specialisation_cache: (bid_t * btypecode_t list, bid_t * btypecode_t list) Hashtbl.t;
}

let make_syms options =
  {
    counter = ref 1;
    varmap = Hashtbl.create 97;
    ticache = Hashtbl.create 97;
    env_cache = Hashtbl.create 97;
    registry = Hashtbl.create 97;
    compiler_options = options;
    instances = Hashtbl.create 97;
    include_files = ref [];
    roots = ref BidSet.empty;
    quick_names = Hashtbl.create 97;
    bifaces = [];
    reductions = [];
    axioms = [];
    variant_map = Hashtbl.create 97;
    typeclass_to_instance = Hashtbl.create 97;
    instances_of_typeclass = Hashtbl.create 97;
    transient_specialisation_cache = Hashtbl.create 97;
  }

let fresh_bid counter =
  let bid = !counter in
  incr counter;
  bid

let iter_bids f counter start_bid =
  let bid = !counter in
  for i = start_bid to bid do
    f i
  done

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

(* for regular expressions *)

module Drules = Map.Make(struct
  type t = string
  let compare = compare
end)
