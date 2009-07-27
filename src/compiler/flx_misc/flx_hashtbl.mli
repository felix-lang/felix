(** Search through the table and optionally return the key. *)
val find : ('a, 'b) Hashtbl.t -> 'a -> 'b option
