(** Return how long a profiled function took to run. *)
val find : string -> float

(** A wrapper function to profile a function call. *)
val call :
  string ->     (** The name of the profile. *)
  ('a -> 'b) -> (** The function. *)
  'a ->         (** The function's argument. *)
  'b

(** Print out our gathered statistics. *)
val print : out_channel -> unit
