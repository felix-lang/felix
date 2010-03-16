type cexpr_t =
[
  | `Ce_atom of string
  | `Ce_postfix of string * cexpr_t
  | `Ce_prefix of string  * cexpr_t
  | `Ce_infix of string * cexpr_t * cexpr_t

  | `Ce_call of cexpr_t * cexpr_t list
  | `Ce_array of cexpr_t * cexpr_t
  | `Ce_new of cexpr_t list * string * cexpr_t list
  | `Ce_cast of string * cexpr_t
  | `Ce_cond of cexpr_t * cexpr_t * cexpr_t
  | `Ce_expr of string * string
]


(* These are constructors for C terms representing
   actual C types. This includes incomplete types,
   functions and arrays (which have real types,
   even though there are no corresponding first class values).

  Note there are no refernce types, and no const types.
*)

type ctype_t =
[
  | `Ct_base of string
  | `Ct_ptr of ctype_t
  | `Ct_cptr of ctype_t
  | `Ct_vptr of ctype_t
  | `Ct_cvptr of ctype_t
  | `Ct_ptm of string * ctype_t
  | `Ct_cptm of string * ctype_t
  | `Ct_vptm of string * ctype_t
  | `Ct_cvptm of string * ctype_t
  | `Ct_array of int * ctype_t
  | `Ct_varray of ctype_t
  | `Ct_fun of ctype_t * ctype_t list
  | `Ct_vfun of ctype_t * ctype_t list
    (* argument list must not be empty for varags *)
]

(* The decl type is the type term used in a declaration,
  it is not a type, but the combination of a type
  and an access modifier. For example:

  int const * const x;

  declares x to be const lvalue
  of type pointer-const to int
*)

type cdecl_type_t =
[
  | `Cdt_value of ctype_t
  | `Cdt_const of ctype_t
  | `Cdt_volatile of ctype_t
  | `Cdt_const_volatile of ctype_t
  | `Cdt_ref of ctype_t
  | `Cdt_cref of ctype_t
  | `Cdt_vref of ctype_t
  | `Cdt_cvref of ctype_t
]

val print_cexpr : Format.formatter -> cexpr_t -> unit
