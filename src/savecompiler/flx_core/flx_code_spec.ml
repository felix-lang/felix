type t =
  | Str_template of string
  | Str of string
  | Virtual
  | Identity

let isempty = function
  | Str_template "" 
  | Str "" -> true
  | _ -> false


