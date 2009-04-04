open Parse_tree
open TinyML_parser

let input_file_name = !(Argument.string_ref)
let input_file = Pervasives.open_in input_file_name

let pp = pp ()

let prog = match fst (List.hd ((Dyp.lexparse pp "main"
  (Dyp.from_channel pp input_file)))) with
  | `Obj_main p -> p
  | _ -> assert false

exception Match_pattern_failed

let rec match_pattern env pat exp = match (pat,exp) with
  | (Lident s),x -> String_map.add s x env
  | (Int i),(Int j) when i=j -> env
  | (Tuple (n1,l1)),(Tuple (n2,l2)) when n1=n2 ->
      List.fold_left2 match_pattern env l1 l2
  | (Open_tuple (n1,l1)),(Open_tuple (n2,l2)) when n1=n2 ->
      List.fold_left2 match_pattern env l1 l2
  | (Cons (c1,(n1,l1))),(Cons (c2,(n2,l2))) when n1=n2 && c1=c2 ->
      List.fold_left2 match_pattern env l1 l2
  | _ -> raise Match_pattern_failed

let rec match_pattern_list exp l = match l with
  | [] -> failwith "match_pattern_list"
  | (pat,expr)::tl ->
      try
        (match_pattern String_map.empty pat exp),expr
      with Match_pattern_failed -> match_pattern_list exp tl

type env = (string * expr * env) String_map.t

let eval (env:env) stm =
  let rec eval (env:env) exp =
    let int_bin_op env f e1 e2 =
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      match v1,v2 with
      | Int i1, Int i2 -> Int (f i1 i2)
      | _ -> failwith "integer expected"
    in
  match exp with
  | Int _ | String _ -> exp
  | Plus (e1,e2) -> int_bin_op env (fun x y -> x+y) e1 e2
  | Minus (e1,e2) -> int_bin_op env (fun x y -> x-y) e1 e2
  | Times (e1,e2) -> int_bin_op env (fun x y -> x*y) e1 e2
  | Div (e1,e2) -> int_bin_op env (fun x y -> x/y) e1 e2
  | Opposite e -> (match eval env e with
    | Int i -> Int (-i)
    | _ -> failwith "integer expected")
  | Tuple (n,ol) -> Tuple (n,(List.map (eval env) ol))
  | Open_tuple (n,ol) -> Open_tuple (n,(List.map (eval env) ol))
  | Cons (cons,(n,ol)) -> Cons (cons,(n,(List.map (eval env) ol)))
  | Let_rec (fname,argname,f,in_expr) ->
      let env = String_map.add fname (argname,f,env) env in
      eval env in_expr
  | App (fname,arg) ->
      let argname,fbody,fenv =
        (try String_map.find fname env
        with Not_found -> failwith ("unknown function `"^fname^"'"))
      in
      let arg = eval env arg in
      let argenv = String_map.add argname arg String_map.empty in
      let fbody_sub = substitute argenv fbody in
      let env = String_map.add fname (argname,fbody,env) fenv in
      eval env fbody_sub
  | Match_with (exp,ool) ->
      let pat_env,expr =
        match_pattern_list (eval env exp) ool
      in
      let new_expr = substitute pat_env expr in
      eval env new_expr
  | Lident var -> failwith ("eval - free variable `"^var^"'")
  in
  match stm with
  | Let_rec_stm (fname,argname,f) ->
      stm, String_map.add fname (argname,f,env) env
  | Expr_stm exp -> Expr_stm (eval env exp), env
  | Define_syntax -> stm, env
  | Infix_stm -> stm, env

(*let _ = print_endline ("prog:\n"^(str_expr prog))*)

let str_stm = function
  | Define_syntax -> "define_syntax"
  | Infix_stm -> "infix statement"
  | Expr_stm exp -> str_expr exp
  | Let_rec_stm _ -> "let rec binding"

let print_res env prog =
  let stm, env = eval env prog in
  let s = str_stm stm in
  Printf.printf "= %s\n" s;
  env

let _ = List.fold_left print_res String_map.empty (List.rev prog)
