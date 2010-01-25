module type S =
  sig
    include Hashtbl.S

    (** Searches the table for the key and returns either `Some key` or
     * `None`. *)
    val find_opt : 'a t -> key -> 'a option
  end;;

module Make (M:Hashtbl.HashedType) : S with type key = M.t

module IntHashtbl : S with type key = int

(** Search through the table and optionally return the key. *)
val find : ('a, 'b) Hashtbl.t -> 'a -> 'b option
