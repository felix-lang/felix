(** System dependent path handling *)

val filetime : string -> float
val find_file_in_path:
  string list -> string -> string
val find_file:
  bool -> string list -> string -> string
