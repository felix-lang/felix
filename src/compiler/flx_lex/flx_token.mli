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
  | ERRORTOKEN of (Flx_srcref.t * string)
  | ENDMARKER
  | NEWLINE
  | SLOSH
  | NAME of (Flx_srcref.t * string)
  | NONTERMINAL of (Flx_srcref.t * string * ntprio_t)
  | INTEGER of (Flx_srcref.t * string * Flx_ast.bigint)
  | FLOAT of (Flx_srcref.t * string * string)
  | STRING of (Flx_srcref.t * string)
  | CSTRING of (Flx_srcref.t * string)
  | FSTRING of (Flx_srcref.t * string)
  | QSTRING of (Flx_srcref.t * string)
  | WSTRING of (Flx_srcref.t * string)
  | USTRING of (Flx_srcref.t * string)
  | USER_KEYWORD of (Flx_srcref.t * string)
  | HASH_INCLUDE_FILES of string list
  | DUMMY
  | LOAD_SYNTAX of (local_data_t)
  | SAVE_SYNTAX of (string)

  | QUEST of Flx_srcref.t
  | LPAR of Flx_srcref.t
  | RPAR of Flx_srcref.t
  | LSQB of Flx_srcref.t
  | RSQB of Flx_srcref.t
  | LBRACE of Flx_srcref.t
  | RBRACE of Flx_srcref.t
  | COMMA of Flx_srcref.t
  | PLUS of Flx_srcref.t
  | STAR of Flx_srcref.t
  | VBAR of Flx_srcref.t
  | LESS of Flx_srcref.t
  | GREATER of Flx_srcref.t
  | EQUAL of Flx_srcref.t
  | EQEQUAL of Flx_srcref.t
  | NOTEQUAL of Flx_srcref.t
  | LESSEQUAL of Flx_srcref.t
  | GREATEREQUAL of Flx_srcref.t
  | UNDERSCORE of Flx_srcref.t

and prio_t = [`Default | `Priority of string]

and rule_t = string * prio_t * token list * string * anote_t * Flx_srcref.t

and dssl_t = {
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Drules.t; (* string -> string *)
}

and local_data_t = {
  dssls : dssl_t Drules.t;
  loaded_dssls : string list;
  scm : (Flx_srcref.t * string) list;
}

and global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
}
