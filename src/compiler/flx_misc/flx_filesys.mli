(** System dependent path handling *)

exception Missing_path of string

(** Look up the file's modification time. *)
val filetime: string -> float

(** Look in the filesystem for the path. Raises Missing_path if not found. *)
val find_path:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The path name. *)
  string

(** Look in the filesystem for the path. Raises Missing_path if not found or is
 * not a file. *)
val find_file:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The file name. *)
  string

(** Look in the filesystem for the path. Raises Missing_path if not found or is
 * not a directory. *)
val find_dir:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The directory name. *)
  string
