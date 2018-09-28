(** {6 Data Types for Parsing} *)
type anote_t = string
type action_t = 
 | Action_Scheme of string 
 | Action_None 
 | Action_Expr of Ocs_types.sval 
 | Action_Statements of Ocs_types.sval


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

type symbol_t = 
  | Grammar_Nonterminal of string * priority_relation_t
  | Grammar_String of string
  | Grammar_Regex of Dyp.regexp 
  | Grammar_Group of dyalt_t list
  | Grammar_Macro_Call of string * symbol_t list
  | Grammar_External_Macro_Call of string * string * symbol_t list
  | Grammar_Star of symbol_t
  | Grammar_Plus of symbol_t
  | Grammar_Quest of symbol_t

and dyalt_t = symbol_t list * Flx_srcref.t * action_t * anote_t

type unprocessed_rule_t =
  privacy_t * string * priority_level_t * string list * dyalt_t list

type grammar_rule_t = 
  | Rule_Unprocessed_Scheme_rule of unprocessed_rule_t
  | Rule_Processed_Scheme_rule of rule_t
  | Rule_Requires of string list 
  | Rule_Priorities of string list
  | Rule_Regex of string * Dyp.regexp 
  | Rule_Nop

and rule_t = 
  privacy_t * (* scope indicator *)
  string * (* non terminal base name *)
  priority_level_t *  (* non terminal priority index *)
  token list *  (* the RHS production *)
  string *  (* Scheme action code *)
  anote_t *  (* comment *)
  Flx_srcref.t (* source reference *)

type dssl_t = {
  macros : (string * grammar_rule_t) list; 
  regexps : (string * Dyp.regexp) list;
  prios : string list list;
  rules : rule_t list;
  deps : string list;
  privacy : string Flx_drules.Drules.t; (* string -> string *)
}

type local_data_t = {
  global_regexps : (string * Dyp.regexp) list;
  drules : dssl_t Flx_drules.Drules.t;
  installed_dssls : string list;
  scm : (Flx_srcref.t * string) list;
  rev_stmts_as_scheme: Ocs_types.sval list;
}

type 'obj global_data_t = {
  pcounter : int ref;
  env : Ocs_types.env;
  pdebug : bool ref;
  parsing_device: string option ref
}
