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

  | QUEST _ -> "?"
  | LPAR _ -> "("
  | RPAR _ -> ")"
  | LSQB _ -> "["
  | RSQB _ -> "]"
  | LBRACE _ -> "{"
  | RBRACE _ -> "}"
  | COMMA _ -> ","
  | PLUS _ -> "+"
  | STAR _ -> "*"
  | VBAR _ -> "|"
  | LESS _ -> "<"
  | GREATER _ -> ">"
  | EQUAL _ -> "="
  | EQEQUAL _ -> "=="
  | NOTEQUAL _ -> "!="
  | LESSEQUAL _ -> "<="
  | GREATEREQUAL _ -> ">="
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
  | QUEST _ -> "QUEST"
  | LPAR _ -> "LPAR"
  | RPAR _ -> "RPAR"
  | LSQB _ -> "LSQB"
  | RSQB _ -> "RSQB"
  | LBRACE _ -> "LBRACE"
  | RBRACE _ -> "RBRACE"
  | COMMA _ -> "COMMA"
  | PLUS _ -> "PLUS"
  | STAR _ -> "STAR"
  | VBAR _ -> "VBAR"
  | LESS _ -> "LESS"
  | GREATER _ -> "GREATER"
  | EQUAL _ -> "EQUAL"
  | EQEQUAL _ -> "EQEQUAL"
  | NOTEQUAL _ -> "NOTEQUAL"
  | LESSEQUAL _ -> "LESSEQUAL"
  | GREATEREQUAL _ -> "GREATEREQUAL"
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
    -> Flx_srcref.dummy_sr

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
  | ERRORTOKEN (s,_)

  | QUEST s
  | LPAR s
  | RPAR s
  | LSQB s
  | RSQB s
  | LBRACE s
  | RBRACE s
  | COMMA s
  | PLUS s
  | STAR s
  | VBAR s
  | LESS s
  | GREATER s
  | EQUAL s
  | EQEQUAL s
  | NOTEQUAL s
  | LESSEQUAL s
  | GREATEREQUAL s
   | UNDERSCORE s
  -> s
