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
  | ERRORTOKEN of string
  | ENDMARKER
  | NEWLINE
  | SLOSH
  | NAME of string
  | NONTERMINAL of (string * ntprio_t)
  | STRING of string
  | DUMMY
  | QUEST
  | LPAR
  | RPAR
  | LSQB
  | RSQB
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | STAR
  | VBAR
  | LESS
  | GREATER
  | EQUAL
  | EQEQUAL
  | NOTEQUAL
  | LESSEQUAL
  | GREATEREQUAL
  | UNDERSCORE

and prio_t = [`Default | `Priority of string]

and rule_t = string * prio_t * token list * string * anote_t * Flx_srcref.t

and dssl_t = {
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Flx_mtypes2.Drules.t; (* string -> string *)
}

and local_data_t = {
  dssls : dssl_t Flx_mtypes2.Drules.t;
  loaded_dssls : string list;
  scm : (Flx_srcref.t * string) list;
  rev_stmts: Flx_ast.statement_t list;
}

and 'obj global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
}
