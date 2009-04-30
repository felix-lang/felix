open Flx_token

let string_of_string s = "\"" ^  Flx_string.c_quote_of_string s ^ "\""

let sp p = match p with
  | `No_prio -> ""
  | `Eq_prio p -> "[="^p^"]"
  | `Less_prio p -> "[<"^p^"]"
  | `Lesseq_prio p -> "[<="^p^"]"
  | `Greater_prio p -> "[>"^p^"]"
  | `Greatereq_prio p -> "[>="^p^"]"

let string_of_token (tok :Flx_token.token): string =
  match tok with
  | DUMMY -> "DUMMY"
  | NAME (sr,s) -> s
  | NONTERMINAL (sr,s,p) -> s ^ sp p
  | INTEGER (sr,t,i) -> Big_int.string_of_big_int i
  | FLOAT (sr,t,v) -> v
  | STRING (sr,s) -> Flx_string.c_quote_of_string s
  | CSTRING (sr,s) -> Flx_string.c_quote_of_string s
  | FSTRING (sr,s) -> Flx_string.c_quote_of_string s
  | QSTRING (sr,s) -> Flx_string.c_quote_of_string s
  | WSTRING (sr,s) -> Flx_string.c_quote_of_string s
  | USTRING (sr,s) -> Flx_string.c_quote_of_string s
  | USER_KEYWORD (sr,s) -> s
  | HASH_INCLUDE_FILES fs -> "include_files(" ^ String.concat "," fs ^ ")"
  | TOKEN_LIST ts -> "<<token list>>"
  | LOAD_SYNTAX _ -> "<<load syntax>>"
  | SAVE_SYNTAX f -> "<<save syntax "^f^">>"
  | SCHEME_CODE (sr,scm,_) -> "@("^scm^")"
  (*
  | PARSE_ACTION sr -> "=>#"
  *)

  | DOLLAR _ -> "$"
  | QUEST _ -> "?"
  | EXCLAMATION _ -> "!"
  | LPAR _ -> "("
  | RPAR _ -> ")"
  | LSQB _ -> "["
  | RSQB _ -> "]"
  | LBRACE _ -> "{"
  | RBRACE _ -> "}"
  | COLON _ -> ":"
  | COMMA _ -> ","
  | SEMI _ -> ";"
  | PLUS _ -> "+"
  | MINUS _ -> "-"
  | STAR _ -> "*"
  | SLASH _ -> "/"
  | VBAR _ -> "|"
  | AMPER _ -> "&"
  | LESS _ -> "<"
  | GREATER _ -> ">"
  | EQUAL _ -> "="
  | DOT _ -> "."
  | PERCENT _ -> "%"
  | BACKQUOTE _ -> "`"
  | TILDE _ -> "~"
  | CIRCUMFLEX _ -> "^"
  | HASH _ -> "#"
  | DOLLARDOLLAR _ -> "$$"
  | ANDLESS _ -> "&<"
  | ANDGREATER _ -> "&>"
  | EQEQUAL _ -> "=="
  | NOTEQUAL _ -> "!="
  | LESSEQUAL _ -> "<="
  | GREATEREQUAL _ -> ">="
  | LEFTSHIFT _ -> "<<"
  | RIGHTSHIFT _ -> ">>"
  | STARSTAR _ -> "**"
  | LESSCOLON _ -> "<:"
  | COLONGREATER _ -> ":>"
  | DOTDOT _ -> ".."
  | COLONCOLON _ -> "::"
  | PLUSPLUS _ -> "++"
  | MINUSMINUS _ -> "--"
  | PLUSEQUAL _ -> "+="
  | MINUSEQUAL _ -> "-="
  | STAREQUAL _ -> "*="
  | SLASHEQUAL _ -> "/="
  | PERCENTEQUAL _ -> "%="
  | CARETEQUAL _ -> "^="
  | VBAREQUAL _ -> "|="
  | AMPEREQUAL _ -> "&="
  | TILDEEQUAL _ -> "~="
  | COLONEQUAL _ -> ":="
  | RIGHTARROW _ -> "->"
  | EQRIGHTARROW _ -> "=>"
  | LEFTARROW _ -> "<-"
  | LSQBAR _ -> "[|"
  | RSQBAR _ -> "|]"
  | AMPERAMPER _ -> "&&"
  | VBARVBAR _ -> "||"
  | SLOSHAMPER _ -> "\\&"
  | SLOSHVBAR _ -> "\\|"
  | SLOSHCIRCUMFLEX _ -> "\\^"
  | HASHBANG _ -> "#!"
  | LEFTSHIFTEQUAL _ -> "<<="
  | RIGHTSHIFTEQUAL _ -> ">>="
  | LEFTRIGHTARROW _ -> "<->"
  | ANDEQEQUAL _ -> "&=="
  | ANDNOTEQUAL _ -> "&!="
  | ANDLESSEQUAL _ -> "&<="
  | ANDGREATEREQUAL _ -> "&>="
  | DOTDOTDOT _ -> "..."
  | LONGRIGHTARROW _ -> "-->"
  | PARSE_ACTION _ -> "=>#"
  | HASHBANGSLASH _ -> "#!/"
  | ALL _ -> "all"
  | ASSERT _ -> "assert"
  | AXIOM _ -> "axiom"
  | BODY _ -> "body"
  | CALL _ -> "call"
  | CASE _ -> "case"
  | CASENO _ -> "caseno"
  | CFUNCTION _ -> "cfun"
  | CLASS _ -> "class"
  | COMMENT_KEYWORD _ -> "comment"
  | COMPOUND _ -> "compound"
  | CONST _ -> "const"
  | CPROCEDURE _ -> "cproc"
  | CSTRUCT _ -> "cstruct"
  | CTOR _ -> "ctor"
  | CTYPES _ -> "ctypes"
  | DEF _ -> "def"
  | DO _ -> "do"
  | DONE _ -> "done"
  | ELIF _ -> "elif"
  | ELSE _ -> "else"
  | ENDCASE _ -> "endcase"
  | ENDIF _ -> "endif"
  | ENDMATCH _ -> "endmatch"
  | ENUM _ -> "enum"
  | EXPECT _ -> "expect"
  | EXPORT _ -> "export"
  | EXTERN _ -> "extern"
  | FOR _ -> "for"
  | FORGET _ -> "forget"
  | FORK _ -> "fork"
  | FUNCTOR _ -> "functor"
  | FUNCTION _ -> "fun"
  | GENERATOR _ -> "gen"
  | GOTO _ -> "goto"
  | HALT _ -> "halt"
  | HEADER _ -> "header"
  | IDENT _ -> "ident"
  | INCLUDE _ -> "include"
  | INCOMPLETE _ -> "incomplete"
  | INF _ -> "inf"
  | IN _ -> "in"
  | INSTANCE _ -> "instance"
  | IS _ -> "is"
  | INHERIT _ -> "inherit"
  | INLINE _ -> "inline"
  | JUMP _ -> "jump"
  | LEMMA _ -> "lemma"
  | LET _ -> "let"
  | LOOP _ -> "loop"
  | LVAL _ -> "lval"
  | MACRO _ -> "macro"
  | MODULE _ -> "module"
  | NAN _ -> "NaN"
  | NEW _ -> "new"
  | NOINLINE _ -> "noinline"
  | NONTERM _ -> "nonterm"
  | NORETURN _ -> "noreturn"
  | NOT _ -> "not"
  | OPEN _ -> "open"
  | PACKAGE _ -> "package"
  | POD _ -> "pod"
  | PRIVATE _ -> "private"
  | PROCEDURE _ -> "proc"
  | PROPERTY _ -> "property"
  | REDUCE _ -> "reduce"
  | REF _ -> "ref"
  | RENAME _ -> "rename"
  | REQUIRES _ -> "requires"
  | RETURN _ -> "return"
  | SCHEME _ -> "SCHEME"
  | SYNTAX _ -> "syntax"
  | STATIC _ -> "static"
  | STRUCT _ -> "struct"
  | THEN _ -> "then"
  | TODO _ -> "todo"
  | TO _ -> "to"
  | TYPEDEF _ -> "typedef"
  | TYPE _ -> "type"
  | TYPECLASS _ -> "typeclass"
  | UNION _ -> "union"
  | USE _ -> "use"
  | VAL _ -> "val"
  | VAR _ -> "var"
  | VIRTUAL _ -> "virtual"
  | WHERE _ -> "where"
  | WHEN _ -> "when"
  | WITH _ -> "with"
  | YIELD _ -> "yield"
  | GC_POINTER _ -> "_gc_pointer"
  | GC_TYPE _ -> "_gc_type"
  | SVC _ -> "_svc"
  | DEREF _ -> "_deref"
  | AND _ -> "and"
  | AS _ -> "as"
  | CALLBACK _ -> "callback"
  | CODE _ -> "code"
  | FALSE _ -> "false"
  | IF _ -> "if"
  | ISIN _ -> "isin"
  | MATCH _ -> "match"
  | NOEXPAND _ -> "noexpand"
  | OF _ -> "of"
  | OR _ -> "or"
  | THE _ -> "the"
  | TRUE _ -> "true"
  | TYPEMATCH _ -> "typematch"
  | TYPECASE _ -> "typecase"
  | WHENCE _ -> "whence"
  | UNLESS _ -> "unless"
  | UNDERSCORE _ -> "_"
  | COMMENT s -> s (* C style comment, includes the /* */ pair *)
  | COMMENT_NEWLINE s -> "// " ^ s ^ "<NEWLINE>"
  | WHITE i -> String.make i ' '
  | NEWLINE -> "<NEWLINE>"
  | ENDMARKER -> "<<EOF>>"
  | ERRORTOKEN (sref,s) -> "<<ERROR '"^ s ^"'>>"
  | SLOSH -> "\\"

let name_of_token (tok :Flx_token.token): string =
  match tok with
  | DUMMY -> "DUMMY"
  | NAME (sr,s) -> "NAME"
  | NONTERMINAL (sr,s,_) -> "NONTERMINAL"
  | INTEGER (sr,t,i) -> "INTEGER"
  | FLOAT (sr,t,v) -> "FLOAT"
  | STRING (sr,s) -> "STRING"
  | CSTRING (sr,s) -> "CSTRING"
  | FSTRING (sr,s) -> "FSTRING"
  | QSTRING (sr,s) -> "QSTRING"
  | WSTRING (sr,s) -> "WSTRING"
  | USTRING (sr,s) -> "USTRING"
  | USER_KEYWORD (sr,s) -> s
  | HASH_INCLUDE_FILES _ -> "HASH_INCLUDE_FILES"
  | TOKEN_LIST _ -> "TOKEN_LIST"
  | LOAD_SYNTAX _ -> "LOAD_SYNTAX"
  | SAVE_SYNTAX _ -> "SAVE_SYNTAX"
  | SCHEME_CODE _ -> "SCHEME_CODE"
  (*
  | PARSE_ACTION sr -> "PARSE_ACTION"
  *)
  | DOLLAR _ -> "DOLLAR"
  | QUEST _ -> "QUEST"
  | EXCLAMATION _ -> "EXCLAMATION"
  | LPAR _ -> "LPAR"
  | RPAR _ -> "RPAR"
  | LSQB _ -> "LSQB"
  | RSQB _ -> "RSQB"
  | LBRACE _ -> "LBRACE"
  | RBRACE _ -> "RBRACE"
  | COLON _ -> "COLON"
  | COMMA _ -> "COMMA"
  | SEMI _ -> "SEMI"
  | PLUS _ -> "PLUS"
  | MINUS _ -> "MINUS"
  | STAR _ -> "STAR"
  | SLASH _ -> "SLASH"
  | VBAR _ -> "VBAR"
  | AMPER _ -> "AMPER"
  | LESS _ -> "LESS"
  | GREATER _ -> "GREATER"
  | EQUAL _ -> "EQUAL"
  | DOT _ -> "DOT"
  | PERCENT _ -> "PERCENT"
  | BACKQUOTE _ -> "BACKQUOTE"
  | TILDE _ -> "TILDE"
  | CIRCUMFLEX _ -> "CIRCUMFLEX"
  | HASH _ -> "HASH"
  | DOLLARDOLLAR _ -> "DOLLARDOLLAR"
  | ANDLESS _ -> "ANDLESS"
  | ANDGREATER _ -> "ANDGREATER"
  | EQEQUAL _ -> "EQEQUAL"
  | NOTEQUAL _ -> "NOTEQUAL"
  | LESSEQUAL _ -> "LESSEQUAL"
  | GREATEREQUAL _ -> "GREATEREQUAL"
  | LEFTSHIFT _ -> "LEFTSHIFT"
  | RIGHTSHIFT _ -> "RIGHTSHIFT"
  | STARSTAR _ -> "STARSTAR"
  | LESSCOLON _ -> "LESSCOLON"
  | COLONGREATER _ -> "COLONGREATER"
  | DOTDOT _ -> "DOTDOT"
  | COLONCOLON _ -> "COLONCOLON"
  | PLUSPLUS _ -> "PLUSPLUS"
  | MINUSMINUS _ -> "MINUSMINUS"
  | PLUSEQUAL _ -> "PLUSEQUAL"
  | MINUSEQUAL _ -> "MINUSEQUAL"
  | STAREQUAL _ -> "STAREQUAL"
  | SLASHEQUAL _ -> "SLASHEQUAL"
  | PERCENTEQUAL _ -> "PERCENTEQUAL"
  | CARETEQUAL _ -> "CARETEQUAL"
  | VBAREQUAL _ -> "VBAREQUAL"
  | AMPEREQUAL _ -> "AMPEREQUAL"
  | TILDEEQUAL _ -> "TILDEEQUAL"
  | COLONEQUAL _ -> "COLONEQUAL"
  | RIGHTARROW _ -> "RIGHTARROW"
  | EQRIGHTARROW _ -> "EQRIGHTARROW"
  | LEFTARROW _ -> "LEFTARROW"
  | LSQBAR _ -> "LSQBAR"
  | RSQBAR _ -> "RSQBAR"
  | AMPERAMPER _ -> "AMPERAMPER"
  | VBARVBAR _ -> "VBARVBAR"
  | SLOSHAMPER _ -> "SLOSHAMPER"
  | SLOSHVBAR _ -> "SLOSHVBAR"
  | SLOSHCIRCUMFLEX _ -> "SLOSHCIRCUMFLEX"
  | HASHBANG _ -> "HASHBANG"
  | LEFTSHIFTEQUAL _ -> "LEFTSHIFTEQUAL"
  | RIGHTSHIFTEQUAL _ -> "RIGHTSHIFTEQUAL"
  | LEFTRIGHTARROW _ -> "LEFTRIGHTARROW"
  | ANDEQEQUAL _ -> "ANDEQEQUAL"
  | ANDNOTEQUAL _ -> "ANDNOTEQUAL"
  | ANDLESSEQUAL _ -> "ANDLESSEQUAL"
  | ANDGREATEREQUAL _ -> "ANDGREATEREQUAL"
  | DOTDOTDOT _ -> "DOTDOTDOT"
  | LONGRIGHTARROW _ -> "LONGRIGHTARROW"
  | PARSE_ACTION _ -> "PARSE_ACTION"
  | HASHBANGSLASH _ -> "HASHBANGSLASH"
  | ALL _ -> "ALL"
  | ASSERT _ -> "ASSERT"
  | AXIOM _ -> "AXIOM"
  | BODY _ -> "BODY"
  | CALL _ -> "CALL"
  | CASE _ -> "CASE"
  | CASENO _ -> "CASENO"
  | CFUNCTION _ -> "CFUNCTION"
  | CLASS _ -> "CLASS"
  | COMMENT_KEYWORD _ -> "COMMENT_KEYWORD"
  | COMPOUND _ -> "COMPOUND"
  | CONST _ -> "CONST"
  | CPROCEDURE _ -> "CPROCEDURE"
  | CSTRUCT _ -> "CSTRUCT"
  | CTOR _ -> "CTOR"
  | CTYPES _ -> "CTYPES"
  | DEF _ -> "DEF"
  | DO _ -> "DO"
  | DONE _ -> "DONE"
  | ELIF _ -> "ELIF"
  | ELSE _ -> "ELSE"
  | ENDCASE _ -> "ENDCASE"
  | ENDIF _ -> "ENDIF"
  | ENDMATCH _ -> "ENDMATCH"
  | ENUM _ -> "ENUM"
  | EXPECT _ -> "EXPECT"
  | EXPORT _ -> "EXPORT"
  | EXTERN _ -> "EXTERN"
  | FOR _ -> "FOR"
  | FORGET _ -> "FORGET"
  | FORK _ -> "FORK"
  | FUNCTOR _ -> "FUNCTOR"
  | FUNCTION _ -> "FUNCTION"
  | GENERATOR _ -> "GENERATOR"
  | GOTO _ -> "GOTO"
  | HALT _ -> "HALT"
  | HEADER _ -> "HEADER"
  | IDENT _ -> "IDENT"
  | INCLUDE _ -> "INCLUDE"
  | INCOMPLETE _ -> "INCOMPLETE"
  | INF _ -> "INF"
  | IN _ -> "IN"
  | INSTANCE _ -> "INSTANCE"
  | IS _ -> "IS"
  | INHERIT _ -> "INHERIT"
  | INLINE _ -> "INLINE"
  | JUMP _ -> "JUMP"
  | LEMMA _ -> "LEMMA"
  | LET _ -> "LET"
  | LOOP _ -> "LOOP"
  | LVAL _ -> "LVAL"
  | MACRO _ -> "MACRO"
  | MODULE _ -> "MODULE"
  | NAMESPACE _ -> "NAMESPACE"
  | NAN _ -> "NAN"
  | NEW _ -> "NEW"
  | NOINLINE _ -> "NOINLINE"
  | NONTERM _ -> "NONTERM"
  | NORETURN _ -> "NORETURN"
  | NOT _ -> "NOT"
  | OPEN _ -> "OPEN"
  | PACKAGE _ -> "PACKAGE"
  | POD _ -> "POD"
  | PRIVATE _ -> "PRIVATE"
  | PROCEDURE _ -> "PROCEDURE"
  | PROPERTY _ -> "PROPERTY"
  | REDUCE _ -> "REDUCE"
  | REF _ -> "REF"
  | RENAME _ -> "RENAME"
  | REQUIRES _ -> "REQUIRES"
  | RETURN _ -> "RETURN"
  | SCHEME _ -> "SCHEME"
  | SYNTAX _ -> "SYNTAX"
  | STATIC _ -> "STATIC"
  | STRUCT _ -> "STRUCT"
  | THEN _ -> "THEN"
  | TODO _ -> "TODO"
  | TO _ -> "TO"
  | TYPEDEF _ -> "TYPEDEF"
  | TYPE _ -> "TYPE"
  | TYPECLASS _ -> "TYPECLASS"
  | UNION _ -> "UNION"
  | USE _ -> "USE"
  | VAL _ -> "VAL"
  | VAR _ -> "VAR"
  | VIRTUAL _ -> "VIRTUAL"
  | WHERE _ -> "WHERE"
  | WHEN _ -> "WHEN"
  | WITH _ -> "WITH"
  | YIELD _ -> "YIELD"
  | GC_POINTER _ -> "GC_POINTER"
  | GC_TYPE _ -> "GC_TYPE"
  | SVC _ -> "SVC"
  | DEREF _ -> "DEREF"
  | AND _ -> "AND"
  | AS _ -> "AS"
  | CALLBACK _ -> "CALLBACK"
  | CODE _ -> "CODE"
  | FALSE _ -> "FALSE"
  | IF _ -> "IF"
  | ISIN _ -> "ISIN"
  | MATCH _ -> "MATCH"
  | NOEXPAND _ -> "NOEXPAND"
  | OF _ -> "OF"
  | OR _ -> "OR"
  | THE _ -> "THE"
  | TRUE _ -> "TRUE"
  | TYPEMATCH _ -> "TYPEMATCH"
  | TYPECASE _ -> "TYPECASE"
  | WHENCE _ -> "WHENCE"
  | UNLESS _ -> "UNLESS"
  | UNDERSCORE _ -> "UNDERSCORE"
  | COMMENT s -> "COMMENT"
  | COMMENT_NEWLINE s -> "COMMENT_NEWLINE"
  | WHITE i -> "WHITE"
  | NEWLINE -> "NEWLINE"
  | ENDMARKER -> "ENDMARKER"
  | ERRORTOKEN (sref,s) -> "ERRORTOKEN"
  | SLOSH -> "SLOSH"

let src_of_token t = match t with
  | DUMMY
  | NEWLINE
  | COMMENT _
  | COMMENT_NEWLINE _
  | WHITE _
  | ENDMARKER
  | SLOSH
  | HASH_INCLUDE_FILES _
  | TOKEN_LIST _
  | LOAD_SYNTAX _
  | SAVE_SYNTAX _
    -> ("",0,0,0)

  | SCHEME_CODE (s,_,_)
  | NAME    (s,_)
  | NONTERMINAL (s,_,_)
  | INTEGER (s,_,_)
  | FLOAT   (s,_,_)
  | STRING  (s,_)
  | CSTRING  (s,_)
  | FSTRING  (s,_)
  | QSTRING  (s,_)
  | WSTRING  (s,_)
  | USTRING  (s,_)
  | USER_KEYWORD (s,_)
  (*
  | PARSE_ACTION s
  *)
  | ERRORTOKEN (s,_)

  | DOLLAR s
  | QUEST s
  | EXCLAMATION s
  | LPAR s
  | RPAR s
  | LSQB s
  | RSQB s
  | LBRACE s
  | RBRACE s
  | COLON s
  | COMMA s
  | SEMI s
  | PLUS s
  | MINUS s
  | STAR s
  | SLASH s
  | VBAR s
  | AMPER s
  | LESS s
  | GREATER s
  | EQUAL s
  | DOT s
  | PERCENT s
  | BACKQUOTE s
  | TILDE s
  | CIRCUMFLEX s
  | HASH s
  | DOLLARDOLLAR s
  | ANDLESS s
  | ANDGREATER s
  | EQEQUAL s
  | NOTEQUAL s
  | LESSEQUAL s
  | GREATEREQUAL s
  | LEFTSHIFT s
  | RIGHTSHIFT s
  | STARSTAR s
  | LESSCOLON s
  | COLONGREATER s
  | DOTDOT s
  | COLONCOLON s
  | PLUSPLUS s
  | MINUSMINUS s
  | PLUSEQUAL s
  | MINUSEQUAL s
  | STAREQUAL s
  | SLASHEQUAL s
  | PERCENTEQUAL s
  | CARETEQUAL s
  | VBAREQUAL s
  | AMPEREQUAL s
  | TILDEEQUAL s
  | COLONEQUAL s
  | RIGHTARROW s
  | EQRIGHTARROW s
  | LEFTARROW s
  | LSQBAR s
  | RSQBAR s
  | AMPERAMPER s
  | VBARVBAR s
  | SLOSHAMPER s
  | SLOSHVBAR s
  | SLOSHCIRCUMFLEX s
  | HASHBANG s
  | LEFTSHIFTEQUAL s
  | RIGHTSHIFTEQUAL s
  | LEFTRIGHTARROW s
  | ANDEQEQUAL s
  | ANDNOTEQUAL s
  | ANDLESSEQUAL s
  | ANDGREATEREQUAL s
  | DOTDOTDOT s
  | LONGRIGHTARROW s
  | PARSE_ACTION s
  | HASHBANGSLASH s
  | ALL s
  | ASSERT s
  | AXIOM s
  | BODY s
  | CALL s
  | CASE s
  | CASENO s
  | CFUNCTION s
  | CLASS s
  | COMMENT_KEYWORD s
  | COMPOUND s
  | CONST s
  | CPROCEDURE s
  | CSTRUCT s
  | CTOR s
  | CTYPES s
  | DEF s
  | DO s
  | DONE s
  | ELIF s
  | ELSE s
  | ENDCASE s
  | ENDIF s
  | ENDMATCH s
  | ENUM s
  | EXPECT s
  | EXPORT s
  | EXTERN s
  | FOR s
  | FORGET s
  | FORK s
  | FUNCTOR s
  | FUNCTION s
  | GENERATOR s
  | GOTO s
  | HALT s
  | HEADER s
  | IDENT s
  | INCLUDE s
  | INCOMPLETE s
  | INF s
  | IN s
  | INSTANCE s
  | IS s
  | INHERIT s
  | INLINE s
  | JUMP s
  | LEMMA s
  | LET s
  | LOOP s
  | LVAL s
  | MACRO s
  | MODULE s
  | NAMESPACE s
  | NAN s
  | NEW s
  | NOINLINE s
  | NONTERM s
  | NORETURN s
  | NOT s
  | OPEN s
  | PACKAGE s
  | POD s
  | PRIVATE s
  | PROCEDURE s
  | PROPERTY s
  | REDUCE s
  | REF s
  | RENAME s
  | REQUIRES s
  | RETURN s
  | SCHEME s
  | SYNTAX s
  | STATIC s
  | STRUCT s
  | THEN s
  | TODO s
  | TO s
  | TYPEDEF s
  | TYPE s
  | TYPECLASS s
  | UNION s
  | USE s
  | VAL s
  | VAR s
  | VIRTUAL s
  | WHERE s
  | WHEN s
  | WITH s
  | YIELD s
  | GC_POINTER s
  | GC_TYPE s
  | SVC s
  | DEREF s
  | AND s
  | AS s
  | CALLBACK s
  | CODE s
  | FALSE s
  | IF s
  | ISIN s
  | MATCH s
  | NOEXPAND s
  | OF s
  | OR s
  | THE s
  | TRUE s
  | TYPEMATCH s
  | TYPECASE s
  | WHENCE s
  | UNLESS s
  | UNDERSCORE s
  -> s
