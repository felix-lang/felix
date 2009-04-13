open Flx_token
open Flx_exceptions
open List
open Flx_srcref

(*

NOTES on token filters
----------------------

These routine are all meant to be short and simple
so its easy to see what they do -- the result
of composition is inefficient!

The results of filter application may depend on
ordering! In other words, each filter has
a postcondition, but also may have several
stronger postconditions given a pre-condition.

Composition of these routines is inefficient!
*)

let dummysr = ("<eol>",0,0,0)

(* throw out C comments; replace newline terminated C++
   comment with a newline
*)
let filter_comments x =
  let rec filter x' result  =
    match x' with
    | COMMENT_NEWLINE _ :: t -> filter t (NEWLINE::result)
    | COMMENT _ :: t  -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out all white space *)
let filter_white x =
  let rec filter x' result  =
    match x' with
    | WHITE _ :: t -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out all newlines *)
let filter_newline x =
  let rec filter x' result  =
    match x' with
    | NEWLINE :: t -> filter t result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

(* throw out duplicate SEMI *)
let filter_dup_semi x =
  let rec filter x' result  =
    match x' with
    | SEMI _ as s :: SEMI _ :: t -> filter (s :: t) result
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

let check_nowhite x =
  let rec filter x' = match x' with
    | WHITE _ :: t -> failwith "UNEXPECTED WHITESPACE"
    | h :: t -> filter t
    | [] -> x
  in filter x

let check_nonewline x =
  let rec filter x' = match x' with
    | NEWLINE :: t -> failwith "UNEXPECTED NEWLINE"
    | h :: t -> filter t
    | [] -> x
  in filter x

(* remove comments, whitespace, newlines, trailing sloshes,
  and a trailing hash on the first line
*)
let filter_preprocessor x =
  let rec filter first_line x' result  =
    match x' with
    | WHITE _ :: t
    | COMMENT _ :: t
      -> filter first_line t result

    | COMMENT_NEWLINE _ :: t
    | NEWLINE :: t
    | SLOSH :: NEWLINE :: t
    | SLOSH :: WHITE _ :: NEWLINE :: t
      -> filter false t result

    | HASH _ :: NEWLINE :: t
    | HASH _ :: WHITE _ :: NEWLINE :: t
      when first_line  -> filter false t result

    | h :: t -> filter first_line t (h::result)
    | [] -> rev result
  in filter true x []


let compress_ctypes x =
  let rec filter x' result =
    match x' with
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "uvlong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "uvlong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "ulong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"long") :: NAME(_,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"long") :: NAME(_,"double") :: NAME(_,"float") :: t -> 
    filter t (NAME (sr, "ldouble") :: result)
| NAME(sr,"unsigned") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "ulong") :: result)
| NAME(sr,"unsigned") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "uint") :: result)
| NAME(sr,"unsigned") :: NAME(_,"char") :: t -> 
    filter t (NAME (sr, "utiny") :: result)
| NAME(sr,"signed") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"signed") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "int") :: result)
| NAME(sr,"signed") :: NAME(_,"char") :: t -> 
    filter t (NAME (sr, "tiny") :: result)
| NAME(sr,"long") :: NAME(_,"long") :: t -> 
    filter t (NAME (sr, "vlong") :: result)
| NAME(sr,"long") :: NAME(_,"int") :: t -> 
    filter t (NAME (sr, "long") :: result)
| NAME(sr,"float") :: NAME(_,"double") :: t -> 
    filter t (NAME (sr, "double") :: result)
| NAME(sr,"double") :: NAME(_,"float") :: t -> 
    filter t (NAME (sr, "double") :: result)
| NAME(sr,"unsigned") :: t -> 
    filter t (NAME (sr, "uint") :: result)
| NAME(sr,"long") :: t -> 
    filter t (NAME (sr, "long") :: result)
    | h :: t -> filter t (h::result)
    | [] -> rev result
  in filter x []

let translate ts =
  let filters = [
    filter_comments;
    filter_white;
    filter_newline;
    filter_dup_semi;
    compress_ctypes;
    ]
  and reverse_apply dat fn = fn dat
  in
  let result = List.fold_left reverse_apply ts filters in
  result

let translate_preprocessor ts =
  let filters = [
    (* 1 *) filter_preprocessor ;
    (* 2 *) compress_ctypes ;
    ]
  and reverse_apply dat fn = fn dat
  in List.fold_left reverse_apply ts filters
