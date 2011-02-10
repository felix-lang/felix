open Flx_format

type t =
  | Int of string * string (* first string is kind, second is value *)
  | String of string
  | Cstring of string
  | Wstring of string
  | Ustring of string
  | Float of string * string

(* Note floats are equal iff they're textually identical, we don't make any
 * assumptions about the target machine FP model.  OTOH, int comparisons are
 * infinite precision, for the same int kind, even if the underlying machine
 * model is not
 *)
let eq lhs rhs =
  match lhs, rhs with
  | Int (a,b), Int (a',b') ->
      a = a' &&
      Big_int.eq_big_int
        (Big_int.big_int_of_string b)
        (Big_int.big_int_of_string b')

  | Float (a,b), Float (a',b') -> a = a' && b = b'
  | String s, String s' -> s = s'
  | Cstring s, Cstring s' -> s = s'
  | Wstring s, Wstring s' -> s = s'
  | Ustring s, Ustring s' -> s = s'
  | _ -> false

(** Prints out a literal to a formatter. *)
let print ppf = function
  | Int (s, i) ->
      print_variant2 ppf "Int"
        print_string s
        print_string i
  | String s ->
      print_variant1 ppf "String" print_string s
  | Cstring s ->
      print_variant1 ppf "Cstring" print_string s
  | Wstring s ->
      print_variant1 ppf "Wstring" print_string s
  | Ustring s ->
      print_variant1 ppf "Ustring" print_string s
  | Float (s1, s2) ->
      print_variant2 ppf "Float"
        print_string s1
        print_string s2
