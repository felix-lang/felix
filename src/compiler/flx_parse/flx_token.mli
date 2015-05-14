(** {6 Data Types for Parsing} *)
type anote_t = string

type priority_level_t = Priority_Default | Priority_Name of string
type privacy_t = Privacy_Public | Privacy_Private

type priority_relation_t = 
  | Priority_Var
  | Priority_None
  | Priority_Eq of string 
  | Priority_Less of string
  | Priority_Lesseq of string
  | Priority_Greater of string
  | Priority_Greatereq of string


(* NOTE!! These tokens have NOTHING to do with the parser.
  In fact they're just constructors used by the code that
  translates the dynamically loaded Felix grammar.
*)
type token =
  | ERRORTOKEN of string
  | ENDMARKER
  | NEWLINE
  | NAME of string
  | NONTERMINAL of (string * priority_relation_t)
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

and rule_t = 
  privacy_t * (* scope indicator *)
  string * (* non terminal base name *)
  priority_level_t *  (* non terminal priority index *)
  token list *  (* the RHS production *)
  string *  (* Scheme action code *)
  anote_t *  (* comment *)
  Flx_srcref.t (* source reference *)

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
  rev_stmts_as_scheme: Ocs_types.sval list;
}

and 'obj global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
  parsing_device: string option ref
}
