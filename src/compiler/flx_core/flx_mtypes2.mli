open Flx_types
open Flx_set

module VarMap : Map.S with type key = string

type varmap_t = string VarMap.t

module TypecodeSet : Set.S with type elt = Flx_ast.typecode_t

type typecodeset_t = TypecodeSet.t
val typecodeset_of_list : TypecodeSet.elt list -> TypecodeSet.t
val typecodeset_map :
  (TypecodeSet.elt -> TypecodeSet.elt) -> TypecodeSet.t -> TypecodeSet.t

module PosSet : Set.S with type elt = int
module PosSetSet : Set.S with type elt = PosSet.t
module CharSet : Set.S with type elt = int

(* generic entity instances: functions, variables *)
type instance_registry_t = (int * btypecode_t list, int) Hashtbl.t

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
  dfns : symbol_table_t;
  counter : int ref;
  varmap : typevarmap_t;
  ticache : (int, btypecode_t) Hashtbl.t;
  glr_cache : (int, btypecode_t) Hashtbl.t;
  env_cache : (int, env_t) Hashtbl.t;
  registry : type_registry_t;
  compiler_options : felix_compiler_options_t;
  instances : instance_registry_t;
  include_files : string list ref;
  roots : IntSet.t ref;
  wrappers : (int, int) Hashtbl.t;
  lexers : (int * tbexpr_t, int) Hashtbl.t;
  parsers : (int * btypecode_t * int list, int) Hashtbl.t;
  quick_names : (string, (int * btypecode_t list)) Hashtbl.t;
  mutable bifaces : biface_t list;
  mutable reductions : reduction_t list;
  mutable axioms : axiom_t list;
  variant_map: (btypecode_t * btypecode_t,int) Hashtbl.t;
  typeclass_to_instance: (int, (bvs_t * btypecode_t * btypecode_t list * int) list) Hashtbl.t;
  instances_of_typeclass: (int, (int * (bvs_t * btypecode_t * btypecode_t list)) list) Hashtbl.t;
  transient_specialisation_cache: (int * btypecode_t list, int * btypecode_t list) Hashtbl.t;
}

module Drules : Map.S with type key = string
