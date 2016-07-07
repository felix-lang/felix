open Flx_ctypes

(* suffixes apply first, then dereferences so
  int *t[2]

is an array of 2 pointers, brackets are needed
for a pointer to an array of 2 ints:

  int ( *t )[2]

Lower value in table indicates higher precedence.
*)

let prec = function
| `Ct_base _ -> 0
| `Ct_array _
| `Ct_varray _
| `Ct_fun _
| `Ct_vfun _ -> 1
| `Ct_ptr _
| `Ct_cptr _
| `Ct_vptr _
| `Ct_cvptr _
| `Ct_ptm _
| `Ct_cptm _
| `Ct_vptm _
| `Ct_cvptm _ -> 2

let rec plist ps =
  String.concat ", "
  (
    List.map (fun t -> aux t "" ) ps
  )


and aux (t:ctype_t) s =
  let br s = "(" ^ s ^ ")" in
  match t with
  | `Ct_base x ->
    if String.length x = 0
    then s
    else if String.length s = 0 then x
    else x ^ " " ^ s

  | `Ct_ptr t -> aux t ("*"^s)
  | `Ct_cptr t -> aux t (" const*"^s)
  | `Ct_vptr t -> aux t (" volatile*"^s)
  | `Ct_cvptr t -> aux t (" const volatile*"^s)

  | `Ct_ptm (k,t) -> aux t (k ^ "::*" ^ s)
  | `Ct_cptm (k,t) -> aux t ("const "^ k ^ "::*" ^ s)
  | `Ct_vptm (k,t) -> aux t ("volatile " ^ k ^ "::*"^ s)
  | `Ct_cvptm (k,t) -> aux t ("const volatile " ^ k ^ "::*" ^ s)

  | `Ct_array (i,t) -> aux t (br s ^ "["^string_of_int i^"]" )
  | `Ct_varray t -> aux t (br s ^ "[]")
  | `Ct_fun (t,ps) ->
    let args =  plist ps in
    aux t (br s ^ "(" ^ args ^ ")" )

  | `Ct_vfun (t,ps) ->
    let args = plist ps ^ ", ..." in
    aux t (br s ^ "(" ^ args ^ ")")

let string_of_ctype t = aux t ""

let string_of_cdecl_type n t =
  match t with
  | `Cdt_value t -> aux t n
  | `Cdt_const t -> aux t ("const " ^ n)
  | `Cdt_volatile t -> aux t ("volatile " ^n)
  | `Cdt_const_volatile t -> aux t ("const volatile " ^n)
  | `Cdt_ref t -> aux t ("& " ^ n)
  | `Cdt_cref t -> aux t ("const &" ^n)
  | `Cdt_vref t -> aux t ("volatile &"^n)
  | `Cdt_cvref t -> aux t ("const volatile &"^n)

