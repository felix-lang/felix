(* Utilities
 *
 * Generic (non Felix dependent) utilities.
 *)

(** n spaces *)
val spaces : int -> string

(** String.concat sep (map fun lst) *)
val catmap : string -> ('a -> string) -> 'a list -> string

(** reverse application *)
val (+>) : 'a -> ('a ->'b) -> 'b

(** hmmm *)
val transpose: 'a list list -> 'a list list

(** last element of list *)
val list_last: 'a list -> 'a

(** position in list of value *)
val list_index: 'a list -> 'a -> int option
val list_assoc_index: ('a * 'b) list -> 'a -> int option

(** forward order map *)
val list_omap: ('a -> 'b) -> 'a list -> 'b list

(** convert exception to option *)
val catch_all : ('a -> 'b) -> 'a -> 'b option

(** test if option not None *)
val is_some: 'a option -> bool

(** list of n integers 0  to n-1 *)
val nlist: int -> int list

(** first n elements of a list *)
val list_prefix: 'a list -> int -> 'a list

(** remaining elements of a list after first n removed,
 * the second list must be long enough or an exception is thrown *)
val list_tail: 'a list -> int -> 'a list

(** the first list with the tail of the second list
 * appended. If the second list isn't long enough return just the first list
 * (routine cannot fail) *)
val splice_tail: 'a list -> 'a list -> 'a list

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

(** add element to unique list *)
val uniq_add: 'a -> 'a list -> 'a list

(** add elements to unique list *)
val uniq_cat: 'a list -> 'a list -> 'a list

(** make a list of unique elements *)
val uniq_list: 'a list -> 'a list
