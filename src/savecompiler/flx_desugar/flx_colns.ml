(* Desugaring
 *
 * Two routines: one to build interfaces from modules, and one to lift lambdas
 * and also blocks.
 *)

open List

open Flx_ast
open Flx_exceptions
open Flx_mtypes2
open Flx_options
open Flx_types
open Flx_util
open Flx_version

let ocs2flx r =
  let sex = Ocs2sex.ocs2sex r in
  (*
  print_endline "OCS scheme term converted to s-expression:";
  Sex_print.sex_print sex;
  *)
  let sr = Flx_srcref.dummy_sr in
  let flx = Flx_sex2flx.xstatement_t sr sex in
  (*
  print_endline "s-expression converted to Felix statement!";
  print_endline (string_of_statement 0 flx);
  *)
  flx


(* return the given filename as a singleton list,
 *  unless it starts with @, in which case return the
 * list of lines in it (rendered recursively) 
 * To support --import=@files,
 * instead of having to --import each one on the command line,
 * which I want mainly so I can split the grammar up into
 * multiple files, which I can't do with #include, since I 
 * can't do that with Dypgen at the moment..
 *)

(* Assumes native filenames *)
let xlocate_file ?(include_dirs=[]) f =
  if Flx_filesys.is_abs f then 
    if Sys.file_exists f then f else raise (Flx_filesys.Missing_path f)
  else Flx_filesys.find_native_path ~include_dirs f
 

let locate_file ?(include_dirs=[]) f =
  let p = xlocate_file ~include_dirs f in
  if Sys.is_directory p then raise (Flx_filesys.Missing_path p) else p

(* Native filenames, respect absolute filename *)
let rec render path f = 
  if String.length f < 1 then [] else (* failwith "Empty --import filename"; *)
  if String.sub f 0 1 = "@" then
    let f = locate_file ~include_dirs:path (String.sub f 1 (String.length f - 1)) in
    let f = open_in f in
    let res = ref [] in
    try
      let rec aux () =
        let line = input_line f in
        res := !res @ render path line;
        aux ()
      in aux ()
    with End_of_file ->
    close_in f;
    !res
  else [f]


