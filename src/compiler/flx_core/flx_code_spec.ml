(*
open Flx_format
*)

type t =
  | Str_template of string
  | Str of string
  | Virtual
  | Identity

let isempty = function
  | Str_template "" 
  | Str "" -> true
  | _ -> false

(*
(** Prints out a code specification to a formatter. *)
let print ppf = function
  | Str_template s ->
      print_variant1 ppf "Str_template" print_string s
  | Str s ->
      print_variant1 ppf "Str" print_string s
  | Virtual ->
      print_variant0 ppf "Virtual"
  | Identity ->
      print_variant0 ppf "Identity"
*)

