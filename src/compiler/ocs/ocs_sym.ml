(* Symbol table implementation.  *)

open Ocs_types
open Ocs_error

(* Symbols are stored in a hash table of weak references.  This
   guarantees that they are unique, but they needn't be permanent.  *)

module HashSymbol =
  struct
    type t = sval
    let equal a b =
      match (a, b) with
	(Ssymbol s1, Ssymbol s2) -> s1 = s2
	| _ -> false
    let hash = Hashtbl.hash
  end

module SymTable = Weak.Make (HashSymbol)

let symt = SymTable.create 307

let get_symbol s =
  SymTable.merge symt (Ssymbol s)
;;

let sym_name =
  function
    Ssymbol s -> s
  | _ -> raise (Error "sym_name: not a symbol")
;;

let sym_quote = get_symbol "quote"
let sym_lambda = get_symbol "lambda"
let sym_if = get_symbol "if"
let sym_set = get_symbol "set!"
let sym_begin = get_symbol "begin"
let sym_cond = get_symbol "cond"
let sym_and = get_symbol "and"
let sym_or = get_symbol "or"
let sym_case = get_symbol "case"
let sym_let = get_symbol "let"
let sym_letstar = get_symbol "let*"
let sym_letrec = get_symbol "letrec"
let sym_do = get_symbol "do"
let sym_delay = get_symbol "delay"
let sym_quasiquote = get_symbol "quasiquote"
let sym_else = get_symbol "else"
let sym_arrow = get_symbol "=>"
let sym_define = get_symbol "define"
let sym_unquote = get_symbol "unquote"
let sym_unquote_splicing = get_symbol "unquote-splicing"

let sym_define_syntax = get_symbol "define-syntax"
let sym_let_syntax = get_symbol "let-syntax"
let sym_letrec_syntax = get_symbol "letrec-syntax"
let sym_syntax_rules = get_symbol "syntax-rules"
let sym_ellipsis = get_symbol "..."

