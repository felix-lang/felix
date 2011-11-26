open Flx_format

module Int_kind =
  struct
    type t =
      | Tiny
      | Short
      | Int
      | Long
      | Vlong
      | Utiny
      | Ushort
      | Uint
      | Ulong
      | Uvlong
      | Int8
      | Int16
      | Int32
      | Int64
      | Uint8
      | Uint16
      | Uint32
      | Uint64

      | Intptr
      | Uintptr
      | Intmax
      | Uintmax
      | Ptrdiff
      | Uptrdiff
      | Ssize
      | Size

    let to_string = function
      | Tiny -> "Tiny"
      | Short -> "Short"
      | Int -> "Int"
      | Long -> "Long"
      | Vlong -> "Vlong"
      | Utiny -> "Utiny"
      | Ushort -> "Ushort"
      | Uint -> "Uint"
      | Ulong -> "Ulong"
      | Uvlong -> "Uvlong"
      | Int8 -> "Int8"
      | Int16 -> "Int16"
      | Int32 -> "Int32"
      | Int64 -> "Int64"
      | Uint8 -> "Uint8"
      | Uint16 -> "Uint16"
      | Uint32 -> "Uint32"
      | Uint64 -> "Uint64"

      | Intptr-> "Intptr"
      | Uintptr-> "Uintptr"
      | Intmax-> "Intmax"
      | Uintmax-> "Uintmax"
      | Ptrdiff-> "Ptrdiff"
      | Uptrdiff-> "Uptrdiff"
      | Ssize -> "Ssize"
      | Size -> "Size"

    let print ppf = function
      | Tiny -> print_variant0 ppf "Tiny"
      | Short -> print_variant0 ppf "Short"
      | Int -> print_variant0 ppf "Int"
      | Long -> print_variant0 ppf "Long"
      | Vlong -> print_variant0 ppf "Vlong"
      | Utiny -> print_variant0 ppf "Utiny"
      | Ushort -> print_variant0 ppf "Ushort"
      | Uint -> print_variant0 ppf "Uint"
      | Ulong -> print_variant0 ppf "Ulong"
      | Uvlong -> print_variant0 ppf "Uvlong"
      | Int8 -> print_variant0 ppf "Int8"
      | Int16 -> print_variant0 ppf "Int16"
      | Int32 -> print_variant0 ppf "Int32"
      | Int64 -> print_variant0 ppf "Int64"
      | Uint8 -> print_variant0 ppf "Uint8"
      | Uint16 -> print_variant0 ppf "Uint16"
      | Uint32 -> print_variant0 ppf "Uint32"
      | Uint64 -> print_variant0 ppf "Uint64"
      | Intptr-> print_variant0 ppf "Intptr"
      | Uintptr-> print_variant0 ppf "Uintptr"
      | Intmax-> print_variant0 ppf "Intmax"
      | Uintmax-> print_variant0 ppf "Uintmax"
      | Ptrdiff-> print_variant0 ppf "Ptrdiff"
      | Uptrdiff-> print_variant0 ppf "Uptrdiff"
      | Ssize -> print_variant0 ppf "Ssize"
      | Size -> print_variant0 ppf "Size"

end

module Float_kind =
  struct
    type t =
      | Float
      | Double
      | Ldouble

    let to_string = function
      | Float -> "Float"
      | Double -> "Double"
      | Ldouble -> "Ldouble"

    let print ppf = function
      | Float -> print_variant0 ppf "Float"
      | Double -> print_variant0 ppf "Double"
      | Ldouble -> print_variant0 ppf "Ldouble"
  end

type t =
  | Int of Int_kind.t * string (* first string is kind, second is value *)
  | Float of Float_kind.t * string
  | String of string
  | Cstring of string
  | Wstring of string
  | Ustring of string

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

let print ppf = function
  | Int (kind, i) ->
      print_variant2 ppf "Int"
        Int_kind.print kind
        print_string i
  | String s ->
      print_variant1 ppf "String" print_string s
  | Cstring s ->
      print_variant1 ppf "Cstring" print_string s
  | Wstring s ->
      print_variant1 ppf "Wstring" print_string s
  | Ustring s ->
      print_variant1 ppf "Ustring" print_string s
  | Float (kind, f) ->
      print_variant2 ppf "Float"
        Float_kind.print kind
        print_string f

