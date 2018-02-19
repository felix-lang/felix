open Flx_ast

val typecode_of_expr:
  expr_t -> typecode_t

val kindcode_of_expr:
  expr_t -> kindcode_t

val expr_of_typecode:
  Flx_srcref.t -> typecode_t -> expr_t 

val type_of_list:
  typecode_t list -> typecode_t

val kind_of_list:
  kindcode_t list -> kindcode_t

val qualified_name_of_expr:
  expr_t -> qualified_name_t option

val suffixed_name_of_expr:
  expr_t -> suffixed_name_t option

val expr_of_qualified_name:
  qualified_name_t -> expr_t

val expr_of_suffixed_name:
  suffixed_name_t -> expr_t

val string_of_type_name : typecode_t -> string
