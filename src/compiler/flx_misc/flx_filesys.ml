exception Found_file of string

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
let unix2native f = fcat (split_unix f)

let is_abs : string -> bool = match Sys.os_type with
  | "Win32" -> 
    (fun s ->
      let n = String.length s in
      if n = 0 then false else
      if s.[0] = '\\' then true else
      if n > 2 then (s.[1]=':') && (s.[2]='\\') else
      false
    )
  | _ ->
    (fun s ->
      let n = String.length s in
      if n = 0 then false else
      s.[0] = '/' 
    )

let find_file_in_path incdirs f =
  let f = unix2native f in
  try
    List.iter
    (fun d ->
      let f =  Filename.concat d f in
      if Sys.file_exists f
      then raise (Found_file f)
    )
    incdirs
    ;
    ""
  with Found_file s -> s

let find_file lookup incdirs f =
  if String.length f = 0
  then failwith "Empty include file name"
  ;
  if is_abs f || not lookup then unix2native f
  else find_file_in_path incdirs f

let filetime f =
  if f = "" then 0.0
  else
    try (Unix.stat f).Unix.st_mtime
    with | _ -> 0.0
