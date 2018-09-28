type t

(** Make a flxg file. *)
val make: string -> t

(** Open the file for writing. *)
val open_out: t -> out_channel

(** Close the file for writing. *)
val close_out: t -> unit

(** Return the filename of the file. *)
val filename: t -> string

(** Write the string to the file. *)
val output_string: t -> string -> unit

(** Return whether or not the file was written to. *)
val was_used: t -> bool
