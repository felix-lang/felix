(** Generic list functions *)

(** true if first list is equal to or a prefix of the second *)
val has_prefix: 'a list -> 'a list -> bool

(** Transpose a matrix list of lists.
 * [[1; 2]; [3; 4]] -> [[1; 3]; [2; 4]] *)
val transpose: 'a list list -> 'a list list

(** last element of list *)
val list_last: 'a list -> 'a

(** position in list of value *)
val list_index: 'a list -> 'a -> int option
val list_assoc_index: ('a * 'b) list -> 'a -> int option
val list_assoc_index_with_assoc: ('a * 'b) list -> 'a -> (int * 'b) option

(** forward order map *)
val list_omap: ('a -> 'b) -> 'a list -> 'b list

(** list of n integers 0 to n-1 *)
val nlist: int -> int list

(** repeat the element n times. *)
val repeat: 'a -> int -> 'a list

(** first n elements of a list *)
val list_prefix: 'a list -> int -> 'a list

(** remaining elements of a list after first n removed,
 * the second list must be long enough or an exception is thrown *)
val list_tail: 'a list -> int -> 'a list

(** split a list into a head list length n, and a tail,
 * equivalent to list_prefix lst n, list_tail lst n *)
val list_split: 'a list -> int -> 'a list * 'a list

(** the first list with the tail of the second list
 * appended. If the second list isn't long enough return just the first list
 * (routine cannot fail) *)
val splice_tail: 'a list -> 'a list -> 'a list

(** add element to unique list *)
val uniq_add: 'a -> 'a list -> 'a list

(** add elements to unique list *)
val uniq_cat: 'a list -> 'a list -> 'a list

(** make a list of unique elements *)
val uniq_list: 'a list -> 'a list

(** [iteri f [a1; ...; an]] applies function [f] to turn to [a1; ...; an]. It is
 * equivalent to [f 0 a1; ...; f n an]. *)
val iteri: (int -> 'a -> unit) -> 'a list -> unit

(** [mapi f [a1; ...; an]] applies function [f] to [a1, ..., an], and builds
 * the list [[f 0 a1; ...; f n an]] with the results returned by f. Not tail
 * recursive. *)
val mapi: (int -> 'a -> 'b) -> 'a list -> 'b list

(** [fold_lefti f a [b1; ...; bn] is [f 0 a1 (f 1 a2 (... (f n an b) ...))]]. *)
val fold_lefti: (int -> 'a -> 'b -> 'a) -> 'a -> 'b list -> 'a

(** [range f N] is [f 0; f 1; ...; f N]. *)
val range: (int -> 'a) -> int -> 'a list

