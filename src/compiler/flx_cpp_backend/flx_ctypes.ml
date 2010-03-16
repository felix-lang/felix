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

let rec print_cexpr ppf cexpr =
  match cexpr with
  | `Ce_atom s ->
      Flx_format.print_variant1 ppf "`Ce_atom" Flx_format.print_string s
  | `Ce_postfix (s,e) ->
      Flx_format.print_variant2 ppf "`Ce_postfix"
        Flx_format.print_string s
        print_cexpr e
  | `Ce_prefix (s,e) ->
      Flx_format.print_variant2 ppf "`Ce_prefix"
        Flx_format.print_string s
        print_cexpr e
  | `Ce_infix (s,e1,e2) ->
      Flx_format.print_variant3 ppf "`Ce_infix"
        Flx_format.print_string s
        print_cexpr e1
        print_cexpr e2
  | `Ce_call (f,es) ->
      Flx_format.print_variant2 ppf "`Ce_call"
        print_cexpr f
        (Flx_list.print print_cexpr) es
  | `Ce_array (f,e) ->
      Flx_format.print_variant2 ppf "`Ce_array"
        print_cexpr f
        print_cexpr e
  | `Ce_new (ps,cls,args) ->
      Flx_format.print_variant3 ppf "`Ce_new"
        (Flx_list.print print_cexpr) ps
        Flx_format.print_string cls
        (Flx_list.print print_cexpr) args
  | `Ce_cast (cast,e) ->
      Flx_format.print_variant2 ppf "`Ce_cast"
        Flx_format.print_string cast
        print_cexpr e
  | `Ce_cond (e,e1,e2) ->
      Flx_format.print_variant3 ppf "`Ce_cond"
        print_cexpr e
        print_cexpr e1
        print_cexpr e2
  | `Ce_expr (p, s) ->
      Flx_format.print_variant2 ppf "`Ce_expr"
        Flx_format.print_string p
        Flx_format.print_string s
