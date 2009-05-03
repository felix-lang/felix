open Flx_ast
open Flx_mtypes2

type anote_t = string

type ntprio_t = [
  | `No_prio
  | `Eq_prio of string
  | `Less_prio of string
  | `Lesseq_prio of string
  | `Greater_prio of string
  | `Greatereq_prio of string
]

type token =
  | ERRORTOKEN of (srcref * string)
  | ENDMARKER
  | NEWLINE
  | SLOSH
  | NAME of (srcref * string)
  | NONTERMINAL of (srcref * string * ntprio_t)
  | INTEGER of (srcref * string * Flx_ast.bigint)
  | FLOAT of (srcref * string * string)
  | STRING of (srcref * string)
  | CSTRING of (srcref * string)
  | FSTRING of (srcref * string)
  | QSTRING of (srcref * string)
  | WSTRING of (srcref * string)
  | USTRING of (srcref * string)
  | USER_KEYWORD of (srcref * string)
  | HASH_INCLUDE_FILES of string list
  | DUMMY
  | LOAD_SYNTAX of (local_data_t)
  | SAVE_SYNTAX of (string)

  | QUEST of srcref
  | LPAR of srcref
  | RPAR of srcref
  | LSQB of srcref
  | RSQB of srcref
  | LBRACE of srcref
  | RBRACE of srcref
  | COMMA of srcref
  | PLUS of srcref
  | STAR of srcref
  | VBAR of srcref
  | LESS of srcref
  | GREATER of srcref
  | EQUAL of srcref
  | EQEQUAL of srcref
  | NOTEQUAL of srcref
  | LESSEQUAL of srcref
  | GREATEREQUAL of srcref
  | UNDERSCORE of srcref

and prio_t = [`Default | `Priority of string]

and rule_t = string * prio_t * token list * string * anote_t * Flx_ast.range_srcref

and dssl_t = {
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Drules.t; (* string -> string *)
}

and local_data_t = {
  dssls : dssl_t Drules.t;
  loaded_dssls : string list;
  scm : (range_srcref * string) list;
}

and global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
}
