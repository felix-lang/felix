exception Found_path of string
exception Missing_path of string

(** Look up the file's modification time. *)
let filetime f =
  if f = "" then 0.0
  else
    try (Unix.stat f).Unix.st_mtime
    with | _ -> 0.0

(* Note: empty list of components is not allowed *)
let fcat fs =
  if fs = [] then failwith "Empty include filename not permitted";
  List.fold_left Filename.concat (List.hd fs) (List.tl fs)

(* Note: a Felix include filename should be a lower case
   C identifier, or a sequence of such separated by /
   character, possible followed by an extension . and
   lower case alphanumerics. These names come from
   Felix source code.

   The include dir path can contain any directory names
   for the local OS.

   A native filename is calculated by translating the
   Felix name to native format, and using native
   concatenation.
*)

let dir_sep, is_dir_sep =
  let unix_is_dir_sep s i = s.[i] = '/' in
  let win32_is_dir_sep s i = begin
    let c = s.[i] in
    c = '/' || c = '\\'
  end in
  match Sys.os_type with
    "Unix" -> "/", unix_is_dir_sep
  | "Win32" -> "\\", win32_is_dir_sep
  | "Cygwin" -> "/", win32_is_dir_sep
  | _ -> assert false

let slosh = Str.regexp "/"
let split_unix f =
  let fs = Str.split slosh f in
  if is_dir_sep f 0 then dir_sep :: fs else fs

(** Convert a unix path into a native path. *)
let unix2native f = fcat (split_unix f)

(** Check if the native path is an absolute path. *)
let is_abs =
  match Sys.os_type with
  | "Win32" ->
    (* A path is native on windows if it starts with '\' or '[A-Z]:\'. *)
    (fun s ->
      let n = String.length s in
      if n = 0 then false else
      if s.[0] = '\\' then true else
      if n > 2 then (s.[1]=':') && (s.[2]='\\') else
      false
    )
  | _ ->
    (* A path is native on unix if it starts with '/'. *)
    (fun s ->
      let n = String.length s in
      if n = 0 then false else
      s.[0] = '/'
    )

(** Look in the filesystem for the path. Raises Missing_path if not found. *)
let find_path ?(include_dirs=[]) path =
  let path = unix2native path in
  try
    (* Check first if the path can be found directly. *)
    if Sys.file_exists path then raise (Found_path path);

    (* Nope, so search through the include dirs for the path. *)
    List.iter begin fun d ->
      let path = Filename.concat d path in
      if Sys.file_exists path then raise (Found_path path)
    end include_dirs;

    (* We still didn't find it? Then error out. *)
    raise (Missing_path path)
  with Found_path path -> path

(** Look in the filesystem for the path. Raises Missing_path if not found or is
 * not a file. *)
let find_file ?(include_dirs=[]) path =
  let file = find_path ~include_dirs path in
  if Sys.is_directory file then raise (Missing_path file) else
  file

(** Look in the filesystem for the path. Raises Missing_path if not found or is
 * not a directory. *)
let find_dir ?(include_dirs=[]) path =
  let dir = find_path ~include_dirs path in
  if not (Sys.is_directory dir) then raise (Missing_path dir) else
  dir
