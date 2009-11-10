(** Label management *)

type label_map_t =
  (Flx_types.bid_t, (string, Flx_types.bid_t) Hashtbl.t) Hashtbl.t

type goto_kind_t =
[
  | `Local of Flx_types.bid_t                      (* index *)
  | `Nonlocal of Flx_types.bid_t * Flx_types.bid_t (* index, parent *)
  | `Unreachable
]

(** Construct a map of containers to the label name to the label's location. *)
val create_label_map:
  Flx_bsym_table.t ->     (** The symbol table. *)
  Flx_types.bid_t ref ->  (** An index to create unique ids. *)
  label_map_t

(** Add a new symbol to the label map. *)
val update_label_map:
  Flx_types.bid_t ref ->  (** An index to create unique ids. *)
  label_map_t ->          (** The label map. *)
  Flx_types.bid_t ->      (** The symbol to add. *)
  Flx_types.bsym_t ->     (** The symbol data. *)
  unit

(** Find the type of the label in the label map. *)
val find_label:
  Flx_bsym_table.t -> (** The symbol table. *)
  label_map_t ->      (** The label map. *)
  Flx_types.bid_t ->  (** The index of the container. *)
  string ->           (** The label name. *)
  goto_kind_t


type label_kind_t = [`Far | `Near | `Unused]
type label_usage_t = (Flx_types.bid_t, label_kind_t) Hashtbl.t

(** Construct a map that identifies whether the label is local or remote. *)
val create_label_usage:
  Flx_mtypes2.sym_state_t ->  (** The symbol state. *)
  Flx_bsym_table.t ->         (** The symbol table. *)
  label_map_t ->              (** The label map. *)
  label_usage_t

(** Add a new symbol to the label map. *)
val update_label_usage:
  Flx_mtypes2.sym_state_t ->  (** The symbol state. *)
  Flx_bsym_table.t ->         (** The symbol table. *)
  label_map_t ->              (** The label map. *)
  label_usage_t ->            (** The label usage map to update. *)
  Flx_types.bid_t ->          (** The symbol to add. *)
  Flx_types.bsym_t ->         (** The symbol data. *)
  unit

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
