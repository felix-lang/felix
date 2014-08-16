(** {6 System dependent path handling. } *)

val unix2native : string -> string

(** For search functions the filename to search for must be in Unix format,
whilst the include file list members must be in native file format. The
functions return native filenames in all cases.
*)


(** Look up the file's modification time, None if not found. *)
val filetime: string -> float option

(** A time long in the past *)
val big_bang: float

(** A time way in the future *)
val big_crunch: float

(** [virtual_filetime dflt filename] calculates the filetime
of native syntax [filename] as a [float] if it is found, or returns the 
default value [dflt] otherwise *)
val virtual_filetime: 
  float ->                     (** default if file not found *)
  string ->                    (** native filename *)
  float                        (** file modification time *)

exception Missing_path of string

(** Directory Separator. / on Unix, \ on Windows *)
val dir_sep : string

(** Root directory. / on Unix, \ on Windows *)
val root_dir : string

(** Look in the filesystem for the path. Raises Missing_path if not found. 
Search the given list of directories if given, otherwise the current directory.
*)
val find_path:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The path name. *)
  string

(** Look in the filesystem for the path. Returns containing dir from include_dirs,
  or just "" if path found directly.  Raises Missing_path if not found. 
*)
val find_include_dir:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The path name. *)
  string

(** Workaround bug in Ocaml Filename.concat. Returns filename if 
  directory name is "", otherwise Filename.concat
*)
val join : 
  string ->                    (** directory name or "" *)
  string ->                    (** filename *)
  string                       (** full pathname *)

(** Given the name of a cache in native format and 
  a filename in native format, join them to make
  a full pathname to be used in the cache.

  This routine allows the RHS argument to start with C:
  which is translated to C/ to form the cache name. 
*)
val mk_cache_name: 
  string ->                    (** native directory name *)
  string ->                    (** native filename *)
  string                       (** full pathname *)

(** check if a native filename is an absolute filename *)
val is_abs: string -> bool 

(** Same as find_path except expects a native filename not a Unix one *)
val find_native_path:
  ?include_dirs:string list ->  (** Optional directories to search. *)
  string ->                     (** The file name. *)
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

(** Open the file for reading and pass the channel to the subfunction. The file
 * is then closed when the subfunction exits. *)
val with_file_in:
  string ->             (** The file name. *)
  (in_channel -> 'a) -> (** The subfunction. *)
  'a

(** Open the file for writing and pass the channel to the subfunction. The file
 * is then closed when the subfunction exits. *)
val with_file_out:
  string ->               (** The file name. *)
  (out_channel -> 'a) ->  (** The subfunction. *)
  'a

(** write some binary data to disk with the specified kind tag,
 * current time, and compiler version information.  *)
val marshal_out:
  string ->
  string ->
  'a ->
  unit

(** read marshal'd binary data from disk. The kind is a lightweight
 * type check. This routine returns None if the file can't be found,
 * if the version information saved doesn't match the current compiler
 * version, or, optionally, if the write time is earlier than the specified
 * minimum time. In other words, it is intended to return the cached result
 * of a computation provided it is up to date.  *)
val marshal_in:
  string ->               (** kind string *)
  string ->               (** input filename *)
  ?min_time:float ->      (** minimum write time of the file *)
  'a option               (** result *)

(** Make all the directories of the given path. Should be absolute name.
  Example: /usr/local/lib/felix/bin
  Will make sure /usr/local/lib/felix/bin is a directory, by creating
  /usr, /usr/local, /usr/local/lib, /usr/local/lib/felix, /usr/local/lib/felix/bin
  in turn. Might fail due to permissions. No error is returned: you'll find
  out when you try to create or read something in the directory.
 *)
val mkdirs : string -> unit

(** Convert a filename to an absolute one if it isn't already,
  by prefixing with current working directory absolute 
  path name
*)
val mkabs: string -> string

(** Try to load the cached result of a computation. If it is found
  * and up to date, return it. Otherwise perform the computation
  * and save the result to disk for subsequent use.
  *
  * Normally the read attempt and write of the cache is to the specified
  * filename, however if the optional outfile is given, we try to
  * read it first, then fallback on the main filename.
  *
  * If the outfile is given and the cache is out of date or non-existant
  * the computed result is written to the outfile, otherwise it is 
  * written to the main filename.
  *
  * The intent is: given a felix source file x.flx, we normally write
  * the cache of a partial evaluation in a file named something 
  * like x.cache in the same directory as x.flx, i.e. the cache is a sibling
  * file. However if the user specifies a cache directory, the cache goes in 
  * that, and of course we have to try reading it as well. That is,
  * the cache dir has priority on read and if specified forces any write.
  *)
val cached_computation:
  string ->                         (** kind label *)
  string ->                         (** input filename of the cached data *)
  outfile:string option ->          (** output filename for cached data *)
  ?force_calc:bool ->               (** ignore input cache, force calculation *)
  ?nowrite:bool ->                  (** don't write output *)
  ?min_time: float ->               (** minimum time the cache must have to be considered up to date *)
  ?validate: ('a -> bool) ->        (** function to validate loaded data *)
  (unit -> 'a) ->                   (** function to calculate the result if the cache is invalid *)
  'a                                (** the result *)

