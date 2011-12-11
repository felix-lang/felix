open Parse_tree
open TinyML_parser
open TinyML_lexer

let input_file = !(Argument.string_ref)

let lexbuf = Ulexing.from_utf8_channel (Pervasives.open_in input_file)

let pf = TinyML_parser.main TinyML_lexer.token lexbuf
let prog = fst (List.hd pf)

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

let rec eval (env:env) exp = match exp with
  | Int i -> Int i
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

(*let _ = print_endline ("prog:\n"^(str_expr prog))*)
let s = str_expr (eval String_map.empty prog)
let () = Printf.printf "= %s\n" s
