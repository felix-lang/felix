(* Character primitives *)

open Ocs_types
open Ocs_error
open Ocs_env

(* Character name equivalents (long).  *)
let char_long_names =
  [| 
    "space",		' ';
    "newline",		'\n';

    (* The rest are extensions.  *)
    "return",		'\r';
    "tab",		'\t';
    "backspace",	'\008';
    "escape",		'\027';
    "backslash",	'\\';
    "alarm",		'\007';
    "vtab",		'\011';
    "del",		'\127' |]
;;

(* Table of short names for 0 .. 31 *)
let char_short_names =
  [| "nul"; "soh"; "stx"; "etx"; "eot"; "enq"; "ack"; "bel";
     "bs";  "ht";  "nl";  "vt";  "np";  "cr";  "so";  "si";
     "dle"; "dc1"; "dc2"; "dc3"; "dc4"; "nak"; "syn"; "etb";
     "can"; "em";  "sub"; "esc"; "fs";  "gs";  "rs";  "us"  |]
;;

let name_to_char name =
  let name = (String.lowercase name)
  and ln = Array.length char_long_names
  and sn = Array.length char_short_names in
  let rec lloop i =
    if i = ln then sloop 0
    else
      match char_long_names.(i) with
	(n, c) -> if n = name then Some c else lloop (i + 1)
  and sloop i =
    if i = sn then None
    else if char_short_names.(i) = name then Some (char_of_int i)
    else sloop (i + 1)
  in
    lloop 0
;;

(* Generate the preferred printed literal form of a character.  *)
let char_to_name =
  function
    '\n' -> "newline"
  | ' ' -> "space"
  | '\127' -> "del"
  | '\000' .. '\031' as c -> char_short_names.(int_of_char c)
  | '\033' .. '\126' as c -> String.make 1 c
  | c -> Printf.sprintf "x%02x" (int_of_char c)
;;

let char_cmp op c1 c2 =
  match (c1, c2) with
    (Schar c1, Schar c2) -> if op c1 c2 then Strue else Sfalse
  | _ -> raise (Error "args not characters")
;;

let char_eq = char_cmp (=);;
let char_lt = char_cmp (<);;
let char_gt = char_cmp (>);;
let char_le = char_cmp (<=);;
let char_ge = char_cmp (>=);;

let char_ci_cmp op c1 c2 =
  match (c1, c2) with
    (Schar c1, Schar c2) ->
      if op (Char.lowercase c1) (Char.lowercase c2) then Strue else Sfalse
  | _ -> raise (Error "args not characters")
;;

let char_ci_eq = char_ci_cmp (=);;
let char_ci_lt = char_ci_cmp (<);;
let char_ci_gt = char_ci_cmp (>);;
let char_ci_le = char_ci_cmp (<=);;
let char_ci_ge = char_ci_cmp (>=);;

let char_unop op =
  function
    Schar c -> op c
  | _ -> raise (Error "arg not character")
;;

let char_alphabetic =
  char_unop (function 'A' .. 'Z' | 'a' ..'z' -> Strue | _ -> Sfalse)
;;

let char_numeric =
  char_unop (function '0' ..'9' -> Strue | _ -> Sfalse)
;;

let char_whitespace =
  char_unop (function ' ' | '\t' | '\r' | '\n' | '\012' -> Strue | _ -> Sfalse)
;;

let char_uppercase =
  char_unop (function 'A' .. 'Z' -> Strue | _ -> Sfalse)
;;

let char_lowercase =
  char_unop (function 'a' .. 'z' -> Strue | _ -> Sfalse)
;;

let char_integer =
  char_unop (fun c -> Sint (int_of_char c))
;;

let integer_char =
  function
    Sint i -> Schar (char_of_int i)
  | _ -> raise (Error "arg not int")
;;

let char_upcase =
  char_unop (fun c -> Schar (Char.uppercase c))
;;

let char_downcase =
  char_unop (fun c -> Schar (Char.lowercase c))
;;

let init e =
  set_pf2 e char_eq "char=?";
  set_pf2 e char_lt "char<?";
  set_pf2 e char_gt "char>?";
  set_pf2 e char_le "char<=?";
  set_pf2 e char_ge "char>=?";

  set_pf2 e char_ci_eq "char-ci=?";
  set_pf2 e char_ci_lt "char-ci<?";
  set_pf2 e char_ci_gt "char-ci>?";
  set_pf2 e char_ci_le "char-ci<=?";
  set_pf2 e char_ci_ge "char-ci>=?";

  set_pf1 e char_alphabetic "char-alphabetic?";
  set_pf1 e char_numeric "char-numeric?";
  set_pf1 e char_whitespace "char-whitespace?";
  set_pf1 e char_uppercase "char-upper-case?";
  set_pf1 e char_lowercase "char-lower-case?";

  set_pf1 e char_integer "char->integer";
  set_pf1 e integer_char "integer->char";

  set_pf1 e char_upcase "char-upcase";
  set_pf1 e char_downcase "char-downcase";
;;
