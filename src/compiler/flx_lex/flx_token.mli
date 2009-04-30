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
  | COMMENT of string
  | COMMENT_NEWLINE of string
  | WHITE of int
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
  | TOKEN_LIST of token list
  | DUMMY
  | LOAD_SYNTAX of (local_data_t)
  | SAVE_SYNTAX of (string)
  | SCHEME_CODE of (srcref * string * Ocs_types.code)

  | DOLLAR of srcref
  | QUEST of srcref
  | EXCLAMATION of srcref
  | LPAR of srcref
  | RPAR of srcref
  | LSQB of srcref
  | RSQB of srcref
  | LBRACE of srcref
  | RBRACE of srcref
  | COLON of srcref
  | COMMA of srcref
  | SEMI of srcref
  | PLUS of srcref
  | MINUS of srcref
  | STAR of srcref
  | SLASH of srcref
  | VBAR of srcref
  | AMPER of srcref
  | LESS of srcref
  | GREATER of srcref
  | EQUAL of srcref
  | DOT of srcref
  | PERCENT of srcref
  | BACKQUOTE of srcref
  | TILDE of srcref
  | CIRCUMFLEX of srcref
  | HASH of srcref
  | DOLLARDOLLAR of srcref
  | ANDLESS of srcref
  | ANDGREATER of srcref
  | EQEQUAL of srcref
  | NOTEQUAL of srcref
  | LESSEQUAL of srcref
  | GREATEREQUAL of srcref
  | LEFTSHIFT of srcref
  | RIGHTSHIFT of srcref
  | STARSTAR of srcref
  | LESSCOLON of srcref
  | COLONGREATER of srcref
  | DOTDOT of srcref
  | COLONCOLON of srcref
  | PLUSPLUS of srcref
  | MINUSMINUS of srcref
  | PLUSEQUAL of srcref
  | MINUSEQUAL of srcref
  | STAREQUAL of srcref
  | SLASHEQUAL of srcref
  | PERCENTEQUAL of srcref
  | CARETEQUAL of srcref
  | VBAREQUAL of srcref
  | AMPEREQUAL of srcref
  | TILDEEQUAL of srcref
  | COLONEQUAL of srcref
  | RIGHTARROW of srcref
  | EQRIGHTARROW of srcref
  | LEFTARROW of srcref
  | LSQBAR of srcref
  | RSQBAR of srcref
  | AMPERAMPER of srcref
  | VBARVBAR of srcref
  | SLOSHAMPER of srcref
  | SLOSHVBAR of srcref
  | SLOSHCIRCUMFLEX of srcref
  | HASHBANG of srcref
  | LEFTSHIFTEQUAL of srcref
  | RIGHTSHIFTEQUAL of srcref
  | LEFTRIGHTARROW of srcref
  | ANDEQEQUAL of srcref
  | ANDNOTEQUAL of srcref
  | ANDLESSEQUAL of srcref
  | ANDGREATEREQUAL of srcref
  | DOTDOTDOT of srcref
  | LONGRIGHTARROW of srcref
  | PARSE_ACTION of srcref
  | HASHBANGSLASH of srcref
  | ALL of srcref
  | ASSERT of srcref
  | AXIOM of srcref
  | BODY of srcref
  | CALL of srcref
  | CASE of srcref
  | CASENO of srcref
  | CFUNCTION of srcref
  | CLASS of srcref
  | COMMENT_KEYWORD of srcref
  | COMPOUND of srcref
  | CONST of srcref
  | CPROCEDURE of srcref
  | CSTRUCT of srcref
  | CTOR of srcref
  | CTYPES of srcref
  | DEF of srcref
  | DO of srcref
  | DONE of srcref
  | ELIF of srcref
  | ELSE of srcref
  | ENDCASE of srcref
  | ENDIF of srcref
  | ENDMATCH of srcref
  | ENUM of srcref
  | EXPECT of srcref
  | EXPORT of srcref
  | EXTERN of srcref
  | FOR of srcref
  | FORGET of srcref
  | FORK of srcref
  | FUNCTOR of srcref
  | FUNCTION of srcref
  | GENERATOR of srcref
  | GOTO of srcref
  | HALT of srcref
  | HEADER of srcref
  | IDENT of srcref
  | INCLUDE of srcref
  | INCOMPLETE of srcref
  | INF of srcref
  | IN of srcref
  | INSTANCE of srcref
  | IS of srcref
  | INHERIT of srcref
  | INLINE of srcref
  | JUMP of srcref
  | LEMMA of srcref
  | LET of srcref
  | LOOP of srcref
  | LVAL of srcref
  | MACRO of srcref
  | MODULE of srcref
  | NAN of srcref
  | NEW of srcref
  | NOINLINE of srcref
  | NONTERM of srcref
  | NORETURN of srcref
  | NOT of srcref
  | OPEN of srcref
  | PACKAGE of srcref
  | POD of srcref
  | PRIVATE of srcref
  | PROCEDURE of srcref
  | PROPERTY of srcref
  | REDUCE of srcref
  | REF of srcref
  | RENAME of srcref
  | REQUIRES of srcref
  | RETURN of srcref
  | SCHEME of srcref
  | SYNTAX of srcref
  | STATIC of srcref
  | STRUCT of srcref
  | THEN of srcref
  | TODO of srcref
  | TO of srcref
  | TYPEDEF of srcref
  | TYPE of srcref
  | TYPECLASS of srcref
  | UNION of srcref
  | USE of srcref
  | VAL of srcref
  | VAR of srcref
  | VIRTUAL of srcref
  | WHERE of srcref
  | WHEN of srcref
  | WITH of srcref
  | YIELD of srcref
  | GC_POINTER of srcref
  | GC_TYPE of srcref
  | SVC of srcref
  | DEREF of srcref
  | AND of srcref
  | AS of srcref
  | CALLBACK of srcref
  | CODE of srcref
  | FALSE of srcref
  | IF of srcref
  | ISIN of srcref
  | MATCH of srcref
  | NOEXPAND of srcref
  | OF of srcref
  | OR of srcref
  | THE of srcref
  | TRUE of srcref
  | TYPEMATCH of srcref
  | TYPECASE of srcref
  | WHENCE of srcref
  | UNLESS of srcref
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
