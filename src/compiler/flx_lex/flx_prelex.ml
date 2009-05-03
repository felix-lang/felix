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
  | LOAD_SYNTAX _ -> "<<load syntax>>"
  | SAVE_SYNTAX f -> "<<save syntax "^f^">>"
  | PARSE_ACTION sr -> "=>#"

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
  | HASHBANGSLASH _ -> "#!/"
  | UNDERSCORE _ -> "_"
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
  | LOAD_SYNTAX _ -> "LOAD_SYNTAX"
  | SAVE_SYNTAX _ -> "SAVE_SYNTAX"
  | PARSE_ACTION _ -> "PARSE_ACTION"
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
  | HASHBANGSLASH _ -> "HASHBANGSLASH"
  | UNDERSCORE _ -> "UNDERSCORE"
  | NEWLINE -> "NEWLINE"
  | ENDMARKER -> "ENDMARKER"
  | ERRORTOKEN (sref,s) -> "ERRORTOKEN"
  | SLOSH -> "SLOSH"

let src_of_token t = match t with
  | DUMMY
  | NEWLINE
  | ENDMARKER
  | SLOSH
  | HASH_INCLUDE_FILES _
  | LOAD_SYNTAX _
  | SAVE_SYNTAX _
    -> ("",0,0,0)

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
  | PARSE_ACTION s
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
  | HASHBANGSLASH s
   | UNDERSCORE s
  -> s
