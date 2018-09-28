type expr =
  | Lident of string
  | Int of int
  | Match_with of expr * ((expr * expr) list)
  | Tuple of int * (expr list)
  | Open_tuple of int * (expr list)
  | Cons of string * (int * (expr list))
  | Let_rec of (string * string * expr * expr)
  | App of (string * expr)
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Times of (expr * expr)
  | Div of (expr * expr)
  | Opposite of expr
  | String of string

type statement =
  | Expr_stm of expr
  | Define_syntax
  | Let_rec_stm of (string * string * expr)
  | Infix_stm

type rhs = Token of string | Nt of (string * string)
  | Rhs_string of string

let rec str_tuple sl = match sl with
  | [] -> ""
  | [a] -> a
  | a::tl -> a^","^(str_tuple tl)

let rec str_expr exp = match exp with
  | Int i -> string_of_int i
  | String s -> "\""^s^"\""
  | Tuple (n,ol) ->
      let sl = List.map str_expr ol in
      "("^(str_tuple sl)^")"
  | Open_tuple (n,ol) ->
      let sl = List.map str_expr ol in
      str_tuple sl
  | Cons (cons,(n,ol)) ->
      if n=0 then cons else
      let sl = List.map str_expr ol in
      cons^"("^(str_tuple sl)^")"
  | Lident x -> x
  | App (fname,arg) -> fname^" "^(str_expr arg)
  | Let_rec (fname,argname,fbody,inexp) ->
      "let rec "^fname^" "^argname^" = "^(str_expr fbody)^" in "^
      (str_expr inexp)
  | Match_with (e,eel) ->
      let f s (e1,e2) =
        s^" | "^(str_expr e1)^" -> "^(str_expr e2)
      in
      "match "^(str_expr e)^" with "^(List.fold_left f "" eel)
  | Plus (e1,e2) -> (str_expr e1)^" + "^(str_expr e2)
  | Minus (e1,e2) -> (str_expr e1)^" - "^(str_expr e2)
  | Times (e1,e2) -> (str_expr e1)^" * "^(str_expr e2)
  | Div (e1,e2) -> (str_expr e1)^" / "^(str_expr e2)
  | Opposite e -> "-"^(str_expr e)
  (*| _ -> "_expr"*)

module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module String_map = Map.Make(Ordered_string)

let rec trim_env env pat = match pat with
  | Int _ | String _ -> env
  | Lident s -> String_map.remove s env
  | Tuple (_,l) -> List.fold_left trim_env env l
  | Open_tuple (_,l) -> List.fold_left trim_env env l
  | Cons (_,(n,l)) -> List.fold_left trim_env env l
  | Match_with _ -> failwith "trim_env - match with in pattern"
  | App _ -> failwith "trim_env - application in pattern"
  | Let_rec _ -> failwith "trim_env - let rec in pattern"
  | Plus _ -> failwith "trim_env - + in pattern"
  | Minus _ -> failwith "trim_env - - in pattern"
  | Times _ -> failwith "trim_env - * in pattern"
  | Div _ -> failwith "trim_env - / in pattern"
  | Opposite _ -> failwith "trim_env - - in pattern"


let rec substitute env expr = match expr with
  | Int _ | String _ -> expr
  | App (fname,arg) -> App (fname,substitute env arg)
  | Lident s ->
      begin try String_map.find s env
      with Not_found -> Lident s end
  | Tuple (n,l) -> Tuple (n,(List.map (substitute env) l))
  | Open_tuple (n,l) ->
      Open_tuple (n,(List.map (substitute env) l))
  | Cons (c,(n,l)) ->
      Cons (c,(n,(List.map (substitute env) l)))
  | Let_rec (fname,a,f,exp) ->
      let env = String_map.remove fname env in
      Let_rec (fname,a,substitute env f,substitute env exp)
  | Match_with (exp,ool) ->
      let exp = substitute env exp in
      let f (pat,expr) =
        let env = trim_env env pat in
        pat,(substitute env expr)
      in
      let ool = List.map f ool in
      Match_with (exp,ool)
  | Plus (e1,e2) -> Plus (substitute env e1,substitute env e2)
  | Minus (e1,e2) -> Minus (substitute env e1,substitute env e2)
  | Times (e1,e2) -> Times (substitute env e1,substitute env e2)
  | Div (e1,e2) -> Div (substitute env e1,substitute env e2)
  | Opposite e -> Opposite (substitute env e)

module Ordered_op =
struct
  type t = string
  let compare = Pervasives.compare
end

module Op_map = Map.Make(Ordered_op)

type associativity = Non_assoc | Left_assoc | Right_assoc
