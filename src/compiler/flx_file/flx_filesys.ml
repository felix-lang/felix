
exception Found_path of string
exception Missing_path of string

(** Look up the file's modification time. *)
let filetime f =
  if f = "" then None
  else
    try Some ((Unix.stat f).Unix.st_mtime)
    with | _ -> None

let big_bang = neg_infinity
let big_crunch = infinity

let virtual_filetime dflt f =
  match filetime f with
  | Some t -> t
  | None -> dflt

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

let root_dir = dir_sep

let slosh = Str.regexp "/"
let split_unix f =
  let fs = Str.split slosh f in
  if is_dir_sep f 0 then dir_sep :: fs else fs

let native_join dir file =
  let n = String.length dir in
  (* chop non-leading trailing / off *)
  let dir = 
     if n>2 then 
       if String.sub dir (n-1) 1 = dir_sep then String.sub dir 0 (n-1)
       else dir
     else dir
  in

  let joined = 
    if dir = "" then file 
    else 
      if file = "" then dir 
      else Filename.concat dir file
  in joined

(* Note: empty list of components is not allowed *)
let fcat fs =
  if fs = [] then failwith "Empty include filename not permitted";
  List.fold_left native_join (List.hd fs) (List.tl fs)

(** Convert a unix path into a native path. *)
let unix2native f = if f = "" then "" else fcat (split_unix f)

(** Check if the native path is an absolute path. *)
let is_abs s : bool =
  match Sys.os_type with
  | "Win32" ->
    (* A path is native on windows if it starts with '\' or '[A-Z]:\'. *)
      let n = String.length s in
      if n = 0 then false else
      if s.[0] = '\\' then true else
      if n > 2 then (s.[1]=':') && (s.[2]='\\') else
      false
  | _ ->
    (* A path is native on unix if it starts with '/'. *)
      let n = String.length s in
      if n = 0 then false else
      s.[0] = '/'

(* This code removes leading / (on unix) or C:\ (on Windows
  so the result can be added to the end of another pathname
  to form a cache name
*)
let strip_drive f = 
  let n = String.length f in
  match  Sys.os_type with 
  | "Win32" ->
    if n >2 then
      if f.[1] = ':' then (* hack: replace ":" with separator *)
        if n>3 then 
          if f.[2] = '\\' then (* unless C:\ is seen, then just elide the ":" *)
            String.sub f 0 1 ^ String.sub f 2 (n-2) 
          else
            String.sub f 0 1 ^ "\\" ^ String.sub f 2 (n-2)
        else
          String.sub f 0 1 ^ "\\" ^ String.sub f 2 (n-2)
      else 
        f
    else 
      f
  | _ -> 
    if n>1 then
      if f.[0]='/' then String.sub f 1 (n-1)
      else f
    else f

let mk_cache_name cache_name file_name =
  let file_name = strip_drive file_name in
  native_join cache_name file_name

(** Workaround bug in Ocaml Filename.concat *)
let join dir file =
  let file = unix2native file in
  native_join dir file


(** Look in the filesystem for the path. Raises Missing_path if not found. *)
let find_native_path ?(include_dirs=[]) path =
  try
    (* Check first if the path can be found directly. *)
    if Sys.file_exists path then raise (Found_path path);

    (* Nope, so search through the include dirs for the path. *)
    List.iter begin fun d ->
      let path = join d path in
      if Sys.file_exists path then raise (Found_path path)
    end include_dirs;

    (* We still didn't find it? Then error out. *)
    raise (Missing_path path)
  with Found_path path -> path

let find_path ?(include_dirs=[]) path =
  let path = unix2native path in
  find_native_path ~include_dirs path


(** Look in file system for file, returns the directory in which it is contained,
   if found directly that will be "". This is the same as find_path, except that
   it returns the containing directly rather than the full pathname. Useful
   for finding siblings of a file. 
*)
let find_include_dir ?(include_dirs=[]) path =
  let path = unix2native path in
  try
    (* Check first if the path can be found directly. *)
    if Sys.file_exists path then raise (Found_path "");

    (* Nope, so search through the include dirs for the path. *)
    List.iter begin fun d ->
      let fullpath = join d path in
      if Sys.file_exists fullpath then raise (Found_path d)
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

(** Open the file for reading and pass the channel to the subfunction. The file
 * is then closed when the subfunction exits. *)
let with_file_in path f =
  let file = open_in path in
  Flx_util.finally (fun () -> close_in file) f file

(** Open the file for writing and pass the channel to the subfunction. The file
 * is then closed when the subfunction exits. *)
let with_file_out path f =
  let file = open_out path in
  Flx_util.finally (fun () -> close_out file) f file

let rec mkdirs d =
  let p = Filename.dirname d in
  if p = d then () else mkdirs p;
  try Unix.mkdir d 0o777 with _ -> ()

(** Throws an I/O of some kind if the output can't be written
The kind string identifies the type of data being written, and is used
for lightweight type checking. This function writes out compiler
version information so that marshal_in can reject a file if 
the compiler has been upgraded since the last write.
*)

let marshal_out (kind:string) (filename:string) (data:'a) =
  let this_version = !Flx_version.version_data in
  let time_now = Unix.time () in
(* print_endline ("Marshal out filename = " ^ filename); *)
  mkdirs (Filename.dirname filename);
  let file = open_out_bin filename in
  Marshal.to_channel file this_version [];
  Marshal.to_channel file kind [];
  Marshal.to_channel file time_now [];
  Marshal.to_channel file data [];
  close_out file

(** Read back marshalled data, checking the data is the right
kind, and checking the version of the compiler that wrote it
is identical to this one. We can also optionally give a minimum
write time for the file: this allows us to optionally thwart
touching games or problems with time-stamps when copying files.
*)

let marshal_in (kind:string) (filename:string) ?(min_time=big_bang) : 'a option =
  let this_version= !Flx_version.version_data in
(* print_endline ("Marhsal in filename " ^ filename); *)
  let file = open_in_bin filename in
(* print_endline "Opened cache"; *)
  let result = 
    let that_version = Marshal.from_channel file in
    if this_version <> that_version then None else begin
(* print_endline "Version matches"; *)
    let that_kind = Marshal.from_channel file in
(* print_endline "Kind matches"; *)
    if kind <> that_kind then None else begin
    let that_time = Marshal.from_channel file in
    if that_time < min_time then None else begin
(* print_endline "It is up to date, grab the data"; *)
    let data = Marshal.from_channel file in
    Some data
    end end end (* Caml sucks sometimes *)
  in
  close_in file;
  result

(** load the file cached result of a computation, or,
  perform the computation and store the result.
  Throws whatever the computation throws if it is invoked.
*)

let mkabs d =
  if not (Filename.is_relative d) then d else
  let cd = Sys.getcwd() in
  if Filename.is_implicit d then 
    Filename.concat cd d
  else
    let _ = () in
    print_endline ("Nasty name " ^ d);
    Filename.concat cd d


let cached_computation 
  (result_kind:string) 
  (filename:string)
  ~(outfile: string option)
  ?(force_calc:bool=false)
  ?(nowrite:bool=false)
  ?(min_time:float=big_bang)
  ?(validate: ('a-> bool)=(fun x -> true))
  (f: unit -> 'a) 
  : 'a
=
(* print_endline ("Input: Filename: " ^ filename ^ ", outfilename = " ^ (match outfile with | None -> "NONE" | Some f -> f)); *)

  let filename = mkabs filename in
  let outfile = match outfile with | None -> None | Some f -> Some (mkabs f) in

(* print_endline ("Absolutised: Filename: " ^ filename ^  ", outfilename = " ^ (match outfile with | None -> "NONE" | Some f -> f)); *)

  let cached_data = 
    if not force_calc then
    (* try to read the output file first *)
      let cached_data = 
        match outfile with
        | Some f -> 
          begin
(* print_endline ("Try to read cached output data " ^ f); *)
            try marshal_in result_kind f ~min_time
            with _ -> None
          end
        | None -> None
      in
      (* if that fails or it isn't specified read the input file *)
      let cached_data = 
        match cached_data with
        | None -> 
          begin 
(* print_endline ("Try to read cached input data " ^ filename); *)
            try marshal_in result_kind filename ~min_time 
            with _ -> None 
          end
        | x -> x
      in
      cached_data
    else None
  in
  (* if that fails generate the data *)
  let cached_data =
    match cached_data with
    | Some data -> if validate data then Some data else None
    | None -> None
  in 
  match cached_data with
  | Some data -> data
  | None ->
    let data = f () in
    (* write to the output file if specified otherwise the input file *)
    let outname = 
      match outfile with 
      | Some f -> f 
      | None -> filename
    in
    (* it's faster to just try writing and make directories then
     * retry if there's a failure
     *)
    if not nowrite then
      begin try 
(* print_endline ("Try to write cached output data " ^ outname); *)
        marshal_out result_kind outname data
      with _ ->
(* print_endline ("Write failed, trying to make directories for " ^ outname); *)
        mkdirs (Filename.dirname outname);
        marshal_out result_kind outname data
      end
    ;
    data
 

