(* parse_tree.ml *)

type expr =
  | Lident of string
  | Int of int
  | Pair of (expr * expr)
  | Cons of string * (int * (expr list))

type rhs = Token of string | Nt of (string * string)

let rec str_expr exp = match exp with
  | Int i -> string_of_int i
  | Pair (a,b) -> "("^(str_expr a)^","^(str_expr b)^")"
  | Cons (cons,(0,_)) -> cons
  | Cons (cons,(1,[o])) ->
      cons^"("^(str_expr o)^")"
  | Cons (cons,(2,[o1;o2])) ->
      cons^"("^(str_expr o1)^","^(str_expr o2)^")"
  | Lident x -> x
  | _ -> failwith "str_expr"

module String_map = Map.Make(String)

let rec substitute env expr = match expr with
  | Int i -> Int i
  | Lident s ->
      begin try String_map.find s env
      with Not_found -> Lident s end
  | Pair (a,b) -> Pair (substitute env a,substitute env b)
  | Cons (c,(n,l)) ->
      Cons (c,(n,(List.map (substitute env) l)))