type anote_t = string

type ntprio_t = [
  | `No_prio
  | `Eq_prio of string
  | `Less_prio of string
  | `Lesseq_prio of string
  | `Greater_prio of string
  | `Greatereq_prio of string
]

(* NOTE!! These tokens have NOTHING to do with the parser.
  In fact they're just constructors used by the code that
  translates the dynamically loaded Felix grammar.
*)
type token =
  | ERRORTOKEN of string
  | ENDMARKER
  | NEWLINE
  | NAME of string
  | NONTERMINAL of (string * ntprio_t)
  | STRING of string
  | REGEX of Dyp.regexp
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
  regexps : (string * Dyp.regexp) list;
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Flx_drules.Drules.t; (* string -> string *)
}

and local_data_t = {
  global_regexps : (string * Dyp.regexp) list;
  drules : dssl_t Flx_drules.Drules.t;
  installed_dssls : string list;
  scm : (Flx_srcref.t * string) list;
(*
  rev_stmts: Flx_ast.statement_t list;
*)
  rev_stmts_as_scheme: (Flx_srcref.t * Ocs_types.sval) list;
}

and 'obj global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
  parsing_device: string option ref
}
