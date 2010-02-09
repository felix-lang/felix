module type S =
  sig
    include Hashtbl.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : 'a t -> key -> 'a option

    (** Prints the table. *)
    val print :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a t ->
      unit
  end;;

module type HashedTypePrintable =
  sig
    include Hashtbl.HashedType
    val print : Format.formatter -> t -> unit
  end;;

module Make (M:HashedTypePrintable) : S with type key = M.t

module IntHashtbl : S with type key = int

(** Search through the table and optionally return the key. *)
val find_opt : ('a, 'b) Hashtbl.t -> 'a -> 'b option

(** Print a hash table. *)
val print :
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter ->
  ('a, 'b) Hashtbl.t ->
  unit
