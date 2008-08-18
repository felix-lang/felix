open Flx_ast
val typecode_of_expr:
  expr_t -> typecode_t

val typeof_list:
  typecode_t list -> typecode_t

val paramtype:
  (param_kind_t * string * typecode_t * expr_t option) list -> typecode_t

val qualified_name_of_expr:
  expr_t -> qualified_name_t
