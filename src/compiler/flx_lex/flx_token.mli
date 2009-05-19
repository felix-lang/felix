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
  | ERRORTOKEN of (Flx_srcref.srcref * string)
  | ENDMARKER
  | NEWLINE
  | SLOSH
  | NAME of (Flx_srcref.srcref * string)
  | NONTERMINAL of (Flx_srcref.srcref * string * ntprio_t)
  | INTEGER of (Flx_srcref.srcref * string * Flx_ast.bigint)
  | FLOAT of (Flx_srcref.srcref * string * string)
  | STRING of (Flx_srcref.srcref * string)
  | CSTRING of (Flx_srcref.srcref * string)
  | FSTRING of (Flx_srcref.srcref * string)
  | QSTRING of (Flx_srcref.srcref * string)
  | WSTRING of (Flx_srcref.srcref * string)
  | USTRING of (Flx_srcref.srcref * string)
  | USER_KEYWORD of (Flx_srcref.srcref * string)
  | HASH_INCLUDE_FILES of string list
  | DUMMY
  | LOAD_SYNTAX of (local_data_t)
  | SAVE_SYNTAX of (string)

  | QUEST of Flx_srcref.srcref
  | LPAR of Flx_srcref.srcref
  | RPAR of Flx_srcref.srcref
  | LSQB of Flx_srcref.srcref
  | RSQB of Flx_srcref.srcref
  | LBRACE of Flx_srcref.srcref
  | RBRACE of Flx_srcref.srcref
  | COMMA of Flx_srcref.srcref
  | PLUS of Flx_srcref.srcref
  | STAR of Flx_srcref.srcref
  | VBAR of Flx_srcref.srcref
  | LESS of Flx_srcref.srcref
  | GREATER of Flx_srcref.srcref
  | EQUAL of Flx_srcref.srcref
  | EQEQUAL of Flx_srcref.srcref
  | NOTEQUAL of Flx_srcref.srcref
  | LESSEQUAL of Flx_srcref.srcref
  | GREATEREQUAL of Flx_srcref.srcref
  | UNDERSCORE of Flx_srcref.srcref

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
