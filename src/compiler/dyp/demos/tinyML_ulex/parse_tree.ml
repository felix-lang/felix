type expr =
  | Lident of string
  | Int of int
  | Match_with of expr * ((expr * expr) list)
  | Tuple of int * (expr list)
  | Open_tuple of int * (expr list)
  | Cons of string * (int * (expr list))
  | Let_rec of (string * string * expr * expr)
  | App of (string * expr)

type rhs = Token of string | Nt of (string * string)

let rec str_tuple sl = match sl with
  | [] -> ""
  | [a] -> a
  | a::tl -> a^","^(str_tuple tl)

let rec str_expr exp = match exp with
  | Int i -> string_of_int i
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
  (*| _ -> "_expr"*)

module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module String_map = Map.Make(Ordered_string)

let rec trim_env env pat = match pat with
  | Int _ -> env
  | Lident s -> String_map.remove s env
  | Tuple (_,l) -> List.fold_left trim_env env l
  | Open_tuple (_,l) -> List.fold_left trim_env env l
  | Cons (_,(n,l)) -> List.fold_left trim_env env l
  | Match_with (_,_) -> failwith "trim_env - match with in pattern"
  | App (_,_) -> failwith "trim_env - application in pattern"
  | Let_rec (_,_,_,_) -> failwith "trim_env - let rec in pattern"


let rec substitute env expr = match expr with
  | Int _ -> expr
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
