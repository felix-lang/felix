(** Label management *)

open Flx_types
open Flx_mtypes2

type label_map_t =
  (Flx_types.bid_t, (string, Flx_types.bid_t) Hashtbl.t) Hashtbl.t

type goto_kind_t =
[
  | `Local of int          (* index *)
  | `Nonlocal of int * int (* index, parent *)
  | `Unreachable
]

(** Construct a map of containers to the label name to the label's location. *)
val create_label_map:
  fully_bound_symbol_table_t -> (** The symbol table. *)
  int ref ->                    (** An index to create unique ids. *)
  label_map_t

(** Find the type of the label in the label map. *)
val find_label:
  fully_bound_symbol_table_t -> (** The symbol table. *)
  label_map_t ->                (** The label map. *)
  Flx_types.bid_t ->            (** The index of the container. *)
  string ->                     (** The label name. *)
  goto_kind_t


type label_kind_t = [`Far | `Near | `Unused]
type label_usage_t = (Flx_types.bid_t, label_kind_t) Hashtbl.t

(** Construct a map that identifies whether the label is local or remote. *)
val create_label_usage:
  sym_state_t ->                (** The symbol state. *)
  fully_bound_symbol_table_t -> (** The symbol table. *)
  label_map_t ->                (** The label map. *)
  label_usage_t

(** For a given container and label name, identify the label kind. *)
val get_label_kind:
  label_map_t ->      (** The label map. *)
  label_usage_t ->    (** The label usage map. *)
  Flx_types.bid_t ->  (** The index of the container. *)
  string ->           (** The label name. *)
  label_kind_t

(* For a given label index, identify the label kind. *)
val get_label_kind_from_index:
  label_usage_t ->    (** The label usage map. *)
  Flx_types.bid_t ->  (** The label index. *)
  label_kind_t
