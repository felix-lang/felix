open Parse_tree
open TinyML_parser
open Dyp

let input_file_name = !(Argument.string_ref)
let input_file = Pervasives.open_in input_file_name

let pp, se_code =
  let (pp:(unit,
        [ `Dypgen__dummy_obj_cons
          | `EXPR of Parse_tree.expr
          | `Lex_string of unit
          | `Lexeme_matched of Parse_tree.Op_map.key
          | `Obj_INT of int
          | `Obj_LIDENT of string
          | `Obj_STRING of string
          | `Obj_UIDENT of string
          | `Obj___dypgen_layout
          | `Obj_define_cont of
              (string * Parse_tree.rhs list * Parse_tree.expr) list
          | `Obj_define_in of unit
          | `Obj_dypgen__nested_nt_0 of Parse_tree.Op_map.key
          | `Obj_dypgen__nested_nt_1 of Parse_tree.Op_map.key list
          | `Obj_dypgen__nested_nt_2 of Parse_tree.Op_map.key list
          | `Obj_dypgen__nested_nt_3 of Parse_tree.expr * Parse_tree.expr
          | `Obj_dypgen__nested_nt_4 of
              (Parse_tree.expr * Parse_tree.expr) list
          | `Obj_dypgen__nested_nt_5 of
              (Parse_tree.expr * Parse_tree.expr) list
          | `Obj_infix of Parse_tree.associativity
          | `Obj_load_statement of Parse_tree.statement list
          | `Obj_main of Parse_tree.statement list
          | `Obj_match of Parse_tree.expr * Parse_tree.expr
          | `Obj_rhs of Parse_tree.rhs list
          | `Obj_statement of Parse_tree.statement
          | `Obj_statements of Parse_tree.statement list ]
        as 'a,
        (int * Parse_tree.associativity) Parse_tree.Op_map.t *
        (string * Parse_tree.rhs list * Parse_tree.expr) list, unit,
        'a Dyp.dyplexbuf)
       Dyp.parser_pilot)
        = pp () in
  match !(Argument.syntax_extension) with
    | None -> pp, []
    | Some filename ->
      (let ic = open_in filename in
      let stmt_list, define_cont, pdev = Marshal.from_channel ic in
      close_in ic;
      let ral = List.map a_define_in define_cont in
      let pdev = import_functions pdev pp ral in
      { pp with pp_dev = pdev}, stmt_list)


let prog =
  let prog0 =
    match fst (List.hd ((Dyp.lexparse pp "main"
      (Dyp.from_channel pp input_file)))) with
      | `Obj_main p -> p
      | _ -> assert false
  in
  prog0@se_code

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
