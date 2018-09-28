(** Utilities
 *
 * Generic (non Felix dependent) utilities.
 *)

(** n spaces *)
val spaces : int -> string

(** String.concat sep (map fun lst) *)
val catmap : string -> ('a -> string) -> 'a list -> string

(** reverse application *)
val (+>) : 'a -> ('a ->'b) -> 'b

(** convert exception to option *)
val catch_all : ('a -> 'b) -> 'a -> 'b option

(** test if option not None *)
val is_some: 'a option -> bool

(** synonym for string_of_int *)
val si: int -> string

(** synonym for String.concat *)
val cat: string -> string list -> string

(** synonym for Buffer.add_string *)
val bcat: Buffer.t -> string -> unit

(** make a hashtable from an assoc list *)
val hashtable_of_list:
  ('a * 'b) list ->
  ('a,'b) Hashtbl.t

(** fixpoint combinator *)
val fix:
 (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

(** `finally fend f x` calls `f x` then fend, even if `f x` raises an
  * exception. *)
val finally: (unit -> unit) -> ('a -> 'b) -> 'a -> 'b
