
# 499 "dypgen_parser.dyp"
       
module String_set : Set.S with type elt = string

type gd = {
  gd_count : int;
  gd_symbs : String_set.t;
  gd_regexp_decl : String_set.t }

val use_dyplex : bool ref

# 14                 "dypgen_parser.mli"
type token =
    EOF
  | STRING of string
  | CHAR of char
  | LIDENT of (string * (int * int * int * string))
  | UIDENT of (string * (int * int * int * string))
  | PATTERN of (string * Lexing.position)
  | OCAML_TYPE of string
  | OCAML_CODE of (string * (Lexing.position * bool))
  | KWD_LAYOUT
  | LET
  | KWD_PARSER
  | KWD_LEXER
  | KWD_NON_TERMINAL
  | KWD_TYPE
  | KWD_FOR
  | KWD_CONSTRUCTOR
  | KWD_MERGE
  | KWD_MLTOP
  | KWD_MLIMID
  | KWD_MLITOP
  | KWD_MLI
  | KWD_RELATION
  | KWD_START
  | KWD_TOKEN
  | BANG
  | LPARENGREATER
  | LPARENLESS
  | DASH
  | RBRACK
  | LBRACK
  | CARET
  | ARROW
  | PLUS
  | STAR
  | QUESTION
  | EQUAL
  | LESS
  | BAR
  | RBRACE
  | LBRACE
  | PERCENTPERCENT
  | THREEDOTS
  | COLON
  | SEMI
  | COMMA
  | RPAREN
  | LPAREN
type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l, 'm, 'n, 'o, 'p, 'q, 'r,
      's, 't, 'u, 'v, 'w, 'x, 'y, 'z, 'a1, 'b1, 'c1, 'd1, 'e1, 'f1, 'g1, 'h1,
      'i1, 'j1, 'k1, 'l1, 'm1, 'n1, 'o1, 'p1, 'q1, 'r1, 's1, 't1, 'u1, 'v1,
      'w1, 'x1, 'y1, 'z1, 'a2, 'b2, 'c2, 'd2)
     obj =
    Lexeme_matched of string
  | Obj_ARROW
  | Obj_BANG
  | Obj_BAR
  | Obj_CARET
  | Obj_CHAR of char
  | Obj_COLON
  | Obj_COMMA
  | Obj_DASH
  | Obj_EOF
  | Obj_EQUAL
  | Obj_KWD_CONSTRUCTOR
  | Obj_KWD_FOR
  | Obj_KWD_LAYOUT
  | Obj_KWD_LEXER
  | Obj_KWD_MERGE
  | Obj_KWD_MLI
  | Obj_KWD_MLIMID
  | Obj_KWD_MLITOP
  | Obj_KWD_MLTOP
  | Obj_KWD_NON_TERMINAL
  | Obj_KWD_PARSER
  | Obj_KWD_RELATION
  | Obj_KWD_START
  | Obj_KWD_TOKEN
  | Obj_KWD_TYPE
  | Obj_LBRACE
  | Obj_LBRACK
  | Obj_LESS
  | Obj_LET
  | Obj_LIDENT of (string * (int * int * int * string))
  | Obj_LPAREN
  | Obj_LPARENGREATER
  | Obj_LPARENLESS
  | Obj_OCAML_CODE of (string * (Lexing.position * bool))
  | Obj_OCAML_TYPE of string
  | Obj_PATTERN of (string * Lexing.position)
  | Obj_PERCENTPERCENT
  | Obj_PLUS
  | Obj_QUESTION
  | Obj_RBRACE
  | Obj_RBRACK
  | Obj_RPAREN
  | Obj_SEMI
  | Obj_STAR
  | Obj_STRING of string
  | Obj_THREEDOTS
  | Obj_UIDENT of (string * (int * int * int * string))
  | Obj_action_prio of 'a
  | Obj_aux_lexer of 'b
  | Obj_aux_lexer_and of 'c
  | Obj_aux_lexer_def of 'd
  | Obj_aux_lexer_rule of 'e
  | Obj_char_elt of 'f
  | Obj_dypgen__nested_nt_0 of 'g
  | Obj_dypgen__nested_nt_1 of 'h
  | Obj_dypgen__nested_nt_10 of 'i
  | Obj_dypgen__nested_nt_11 of 'j
  | Obj_dypgen__nested_nt_2 of 'k
  | Obj_dypgen__nested_nt_3 of 'l
  | Obj_dypgen__nested_nt_4 of 'm
  | Obj_dypgen__nested_nt_5 of 'n
  | Obj_dypgen__nested_nt_6 of 'o
  | Obj_dypgen__nested_nt_7 of 'p
  | Obj_dypgen__nested_nt_8 of 'q
  | Obj_dypgen__nested_nt_9 of 'r
  | Obj_dypgen__option_BAR of 's
  | Obj_dypgen__option_SEMI of 't
  | Obj_dypgen__option_aux_lexer of 'b option
  | Obj_dypgen__option_lexer of 'x option
  | Obj_dypgen__plus___char_elt of 'f list
  | Obj_dypgen__plus___lident of 'z list
  | Obj_dypgen__star___aux_lexer_and of 'c list
  | Obj_dypgen__star___lexer_rule of 'y list
  | Obj_dypgen__star___regexp_decl of 't1 list
  | Obj_entry_def of 'u
  | Obj_grammar of 'v
  | Obj_ident_list of 'w
  | Obj_lexer of 'x
  | Obj_lexer_rule of 'y
  | Obj_lident of 'z
  | Obj_lident_list of 'a1
  | Obj_main of 'b1
  | Obj_main_lexer of 'c1
  | Obj_opt_bang of 'd1
  | Obj_opt_dash of 'e1
  | Obj_opt_pattern of 'f1
  | Obj_opt_token_name of 'g1
  | Obj_optional_code of 'h1
  | Obj_optional_mli of 'i1
  | Obj_optional_mlimid of 'j1
  | Obj_optional_mlitop of 'k1
  | Obj_optional_mltop of 'l1
  | Obj_optional_trailer of 'm1
  | Obj_optional_type of 'n1
  | Obj_parser_begin of 'o1
  | Obj_parser_param_info of 'p1
  | Obj_parser_param_infos of 'q1
  | Obj_priority of 'r1
  | Obj_regexp of 's1
  | Obj_regexp_decl of 't1
  | Obj_regexp_ter of 'u1
  | Obj_relation of 'v1
  | Obj_relation_list of 'w1
  | Obj_rhs of 'x1
  | Obj_rhs_list of 'y1
  | Obj_symb of 'z1
  | Obj_symbol of 'a2
  | Obj_symbol_list of 'b2
  | Obj_token_list of 'c2
  | Obj_uident_list of 'd2

val pp :
  unit ->
  (token,
   ((string * (Lexing.position * bool)) *
    (string * (int * int * int * string)) * bool,
    (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
    list,
    string list * (Dyp.regexp * (string * (Lexing.position * bool))) list,
    (Dyp.regexp * (string * (Lexing.position * bool))) list,
    string list * (Dyp.regexp * (string * (Lexing.position * bool))) list,
    char * char, (String_set.elt * Dyp.regexp) list,
    (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
    list option * (Dyp.regexp * string * Parse_tree.code_desc) list,
    (char * char) list, Parse_tree.literal_desc,
    ((string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
     list option * (Dyp.regexp * string * Parse_tree.code_desc) list)
    option, (Dyp.regexp * string * Parse_tree.code_desc) list,
    (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
    list, string list, Dyp.regexp * (string * (Lexing.position * bool)),
    Dyp.regexp * (string * (Lexing.position * bool)),
    (Dyp.regexp * (string * (Lexing.position * bool))) list,
    (Dyp.regexp * (string * (Lexing.position * bool))) list, 'a option,
    'b option,
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list,
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list, string list,
    (String_set.elt * Dyp.regexp) list *
    (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
    list * (Dyp.regexp * string * Parse_tree.code_desc) list,
    Dyp.regexp * string * Parse_tree.code_desc, string, string list,
    (string * Lexing.position) * Parse_tree.code_desc *
    Parse_tree.parser_param_info *
    ((String_set.elt * Dyp.regexp) list *
     (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
     list * (Dyp.regexp * string * Parse_tree.code_desc) list)
    option *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list * (string * Lexing.position) * (string * Lexing.position) *
    (string * Lexing.position) * (string * Lexing.position),
    (Dyp.regexp * string * Parse_tree.code_desc) list, bool, bool,
    string * Lexing.position, string, Parse_tree.code_desc,
    string * Lexing.position, string * Lexing.position,
    string * Lexing.position, string * Lexing.position,
    string * Lexing.position, string, unit, Parse_tree.parser_param_info,
    Parse_tree.parser_param_info, string * (int * int * int * string),
    Dyp.regexp, String_set.elt * Dyp.regexp, Dyp.regexp,
    Parse_tree.relation_desc list, string list,
    (string * (int * int * int * string)) *
    (Parse_tree.literal_desc *
     (string * Parse_tree.pat_type * Lexing.position))
    list * (string * (Lexing.position * bool)) * (bool * bool) *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list,
    ((string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool) *
     ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
      (string * (int * int * int * string)) *
      (Parse_tree.literal_desc *
       (string * Parse_tree.pat_type * Lexing.position))
      list * (string * (Lexing.position * bool)) * (bool * bool))
     list)
    list,
    Parse_tree.literal_desc *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list,
    Parse_tree.literal_desc *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list,
    (Parse_tree.literal_desc *
     (string * Parse_tree.pat_type * Lexing.position))
    list *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list, Parse_tree.token_desc list, string list)
   obj, gd, unit, Lexing.lexbuf)
  Dyp.parser_pilot

val main :
  ?global_data:gd ->
  ?local_data:unit ->
  (Lexing.lexbuf -> token) ->
  Lexing.lexbuf ->
  (((string * Lexing.position) * Parse_tree.code_desc *
    Parse_tree.parser_param_info *
    ((String_set.elt * Dyp.regexp) list *
     (string list * (Dyp.regexp * (string * (Lexing.position * bool))) list)
     list * (Dyp.regexp * string * Parse_tree.code_desc) list)
    option *
    ((string * (string * Parse_tree.pat_type * Lexing.position) list) *
     (string * (int * int * int * string)) *
     (Parse_tree.literal_desc *
      (string * Parse_tree.pat_type * Lexing.position))
     list * (string * (Lexing.position * bool)) * (bool * bool))
    list * (string * Lexing.position) * (string * Lexing.position) *
    (string * Lexing.position) * (string * Lexing.position)) *
   string)
  list


