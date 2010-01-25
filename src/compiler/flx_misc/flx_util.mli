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

(** A series of functions that help to call a function that accepts a tuple with
 * curried arguments. *)
val curry2:  ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val curry3:  ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val curry4:  ('a * 'b * 'c * 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val curry5:  ('a * 'b * 'c * 'd * 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val curry6:  ('a * 'b * 'c * 'd * 'e * 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val curry7:  ('a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
val curry8:  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i
val curry9:  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j
val curry10: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k
val curry11: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l
val curry12: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm
val curry13: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n
val curry14: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o
val curry15: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o -> 'p) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p
val curry16: ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o * 'p -> 'q) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q

(** A series of functions that help to call a curriable function with tuple
 * arguments. *)
val uncurry2:  ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val uncurry3:  ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
val uncurry4:  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a * 'b * 'c * 'd -> 'e
val uncurry5:  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a * 'b * 'c * 'd * 'e -> 'f
val uncurry6:  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a * 'b * 'c * 'd * 'e * 'f -> 'g
val uncurry7:  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> 'h
val uncurry8:  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> 'i
val uncurry9:  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i -> 'j
val uncurry10: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j -> 'k
val uncurry11: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k -> 'l
val uncurry12: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l -> 'm
val uncurry13: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm -> 'n
val uncurry14: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n -> 'o
val uncurry15: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o -> 'p
val uncurry16: ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'l -> 'm -> 'n -> 'o -> 'p -> 'q) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k * 'l * 'm * 'n * 'o * 'p -> 'q
