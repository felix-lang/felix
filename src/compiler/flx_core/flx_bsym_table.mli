(** The type of the bound symbol table. *)
type t

(** Construct a bound symbol table. *)
val create : unit -> t

(** Copies the bound symbol table. *)
val copy : t -> t

(** Adds the bound symbol with the index to the symbol table. *)
val add : t -> Flx_types.bid_t -> Flx_bsym.t -> unit

(** Returns if the bound index is in the bound symbol table. *)
val mem : t -> Flx_types.bid_t -> bool

(** Searches the bound symbol table for the given symbol. *)
val find : t -> Flx_types.bid_t -> Flx_bsym.t

(** Searches the bound symbol table for the given symbol's id. *)
val find_id : t -> Flx_types.bid_t -> string

(** Searches the bound symbol table for the given symbol's source reference. *)
val find_sr : t -> Flx_types.bid_t -> Flx_srcref.t

(** Searches the bound symbol table for the given symbol's parent. *)
val find_parent : t -> Flx_types.bid_t -> Flx_types.bid_t option

(** Searches the bound symbol table for the given symbol's bbdcl. *)
val find_bbdcl : t -> Flx_types.bid_t -> Flx_types.bbdcl_t

(** Searches the bound symbol table for the given symbol's bvs. *)
val find_bvs : t -> Flx_types.bid_t -> Flx_types.bvs_t

(** Remove a binding from the bound symbol table. *)
val remove : t -> Flx_types.bid_t -> unit

(** Iterate over all the items in the bound symbol table. *)
val iter : (Flx_types.bid_t -> Flx_bsym.t -> unit) -> t -> unit

(** Return if the bound symbol index is an identity function. *)
val is_identity : t -> Flx_types.bid_t -> bool

(** Return if the bound symbol index is a val or var. *)
val is_variable : t -> Flx_types.bid_t -> bool

(** Return if the bound symbol index is a global val or var. *)
val is_global_var : t -> Flx_types.bid_t -> bool

(** Return if the bound symbol index is an identity function. *)
val is_function : t -> Flx_types.bid_t -> bool

(** Update all the bound function and procedure's bound exes. *)
val update_bexes : (Flx_types.bexe_t list -> Flx_types.bexe_t list) -> t -> unit
