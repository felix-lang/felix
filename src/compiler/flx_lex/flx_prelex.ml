open Flx_token

let string_of_string s = "\"" ^  Flx_string.c_quote_of_string s ^ "\""

let sp p = match p with
  | `No_prio -> ""
  | `Eq_prio p -> "[="^p^"]"
  | `Less_prio p -> "[<"^p^"]"
  | `Lesseq_prio p -> "[<="^p^"]"
  | `Greater_prio p -> "[>"^p^"]"
  | `Greatereq_prio p -> "[>="^p^"]"

let string_of_token = function
  | DUMMY -> "DUMMY"
  | NAME s -> s
  | NONTERMINAL (s,p) -> s ^ sp p
  | INTEGER (t,i) -> Big_int.string_of_big_int i
  | FLOAT (t,v) -> v
  | STRING s -> Flx_string.c_quote_of_string s
  | CSTRING s -> Flx_string.c_quote_of_string s
  | FSTRING s -> Flx_string.c_quote_of_string s
  | QSTRING s -> Flx_string.c_quote_of_string s
  | WSTRING s -> Flx_string.c_quote_of_string s
  | USTRING s -> Flx_string.c_quote_of_string s
  | USER_KEYWORD s -> s
  | HASH_INCLUDE_FILES fs -> "include_files(" ^ String.concat "," fs ^ ")"
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
  | ERRORTOKEN s -> "<<ERROR '"^ s ^"'>>"
  | SLOSH -> "\\"

let name_of_token = function
  | DUMMY -> "DUMMY"
  | NAME _ -> "NAME"
  | NONTERMINAL _ -> "NONTERMINAL"
  | INTEGER _ -> "INTEGER"
  | FLOAT _ -> "FLOAT"
  | STRING _ -> "STRING"
  | CSTRING _ -> "CSTRING"
  | FSTRING _ -> "FSTRING"
  | QSTRING _ -> "QSTRING"
  | WSTRING _ -> "WSTRING"
  | USTRING _ -> "USTRING"
  | USER_KEYWORD s -> s
  | HASH_INCLUDE_FILES _ -> "HASH_INCLUDE_FILES"
  | QUEST _ -> "QUEST"
  | LPAR -> "LPAR"
  | RPAR -> "RPAR"
  | LSQB -> "LSQB"
  | RSQB -> "RSQB"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | PLUS -> "PLUS"
  | STAR -> "STAR"
  | VBAR -> "VBAR"
  | LESS -> "LESS"
  | GREATER -> "GREATER"
  | EQUAL -> "EQUAL"
  | EQEQUAL -> "EQEQUAL"
  | NOTEQUAL -> "NOTEQUAL"
  | LESSEQUAL -> "LESSEQUAL"
  | GREATEREQUAL -> "GREATEREQUAL"
  | UNDERSCORE -> "UNDERSCORE"
  | NEWLINE -> "NEWLINE"
  | ENDMARKER -> "ENDMARKER"
  | ERRORTOKEN _ -> "ERRORTOKEN"
  | SLOSH -> "SLOSH"
