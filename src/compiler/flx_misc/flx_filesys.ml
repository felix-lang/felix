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

let slosh = Str.regexp "/"
let split_unix f =  Str.split slosh f
let unix2native f = fcat (split_unix f)

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
  if f.[0] = '/' || not lookup then unix2native f
  else find_file_in_path incdirs f

let filetime f =
  if f = "" then 0.0
  else
    try (Unix.stat f).Unix.st_mtime
    with | _ -> 0.0
