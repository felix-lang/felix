(** Remap symbols from an old bound symbol table to a new one by offsetting the
 * bound index by a constant amount. *)
val remap : int -> Flx_bsym_table.t -> Flx_bsym_table.t -> unit
