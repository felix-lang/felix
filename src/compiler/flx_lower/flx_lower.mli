type lower_state_t
open Flx_bid

(** Create the state needed for the frontend. *)
val make_lower_state :
  Flx_mtypes2.sym_state_t ->
  lower_state_t


(** Prep the bound symbol table for the backend by lowering and simplifying
 * symbols. *)
val lower_bsym_table :
  lower_state_t ->    (** The felix state. *)
  Flx_bsym_table.t -> (** The bound symbol table. *)
  bid_t option ->  (** The root symbol. *)
  Flx_bsym_table.t


