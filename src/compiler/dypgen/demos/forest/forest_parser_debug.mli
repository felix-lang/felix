
type 'a obj =
    Lexeme_matched of string
  | Obj_expr of 'a
  | Dypgen__dummy_obj_cons

val pp :
  unit ->
  (unit, Parse_tree.tree obj, unit, unit, 'a Dyp.dyplexbuf) Dyp.parser_pilot

val expr :
  ?global_data:unit ->
  ?local_data:unit ->
  Parse_tree.tree obj Dyp.dyplexbuf -> (Parse_tree.tree * string) list


