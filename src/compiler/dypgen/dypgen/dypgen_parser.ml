
type token =
  | EOF
  | STRING of (string)
  | CHAR of (char)
  | LIDENT of (string * (int * int * int * string))
  | UIDENT of (string * (int * int * int * string))
  | PATTERN of (string * Lexing.position)
  | OCAML_TYPE of (string)
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

module Dyp_symbols =
struct
  let get_token_name t = match t with
    | ARROW -> 0
    | BANG -> 1
    | BAR -> 2
    | CARET -> 3
    | CHAR _ -> 4
    | COLON -> 5
    | COMMA -> 6
    | DASH -> 7
    | EOF -> 8
    | EQUAL -> 9
    | KWD_CONSTRUCTOR -> 10
    | KWD_FOR -> 11
    | KWD_LAYOUT -> 12
    | KWD_LEXER -> 13
    | KWD_MERGE -> 14
    | KWD_MLI -> 15
    | KWD_MLIMID -> 16
    | KWD_MLITOP -> 17
    | KWD_MLTOP -> 18
    | KWD_NON_TERMINAL -> 19
    | KWD_PARSER -> 20
    | KWD_RELATION -> 21
    | KWD_START -> 22
    | KWD_TOKEN -> 23
    | KWD_TYPE -> 24
    | LBRACE -> 25
    | LBRACK -> 26
    | LESS -> 27
    | LET -> 28
    | LIDENT _ -> 29
    | LPAREN -> 30
    | LPARENGREATER -> 31
    | LPARENLESS -> 32
    | OCAML_CODE _ -> 33
    | OCAML_TYPE _ -> 34
    | PATTERN _ -> 35
    | PERCENTPERCENT -> 36
    | PLUS -> 37
    | QUESTION -> 38
    | RBRACE -> 39
    | RBRACK -> 40
    | RPAREN -> 41
    | SEMI -> 42
    | STAR -> 43
    | STRING _ -> 44
    | THREEDOTS -> 45
    | UIDENT _ -> 46
  let str_token t = match t with
    | ARROW -> "ARROW"
    | BANG -> "BANG"
    | BAR -> "BAR"
    | CARET -> "CARET"
    | CHAR _ -> "CHAR"
    | COLON -> "COLON"
    | COMMA -> "COMMA"
    | DASH -> "DASH"
    | EOF -> "EOF"
    | EQUAL -> "EQUAL"
    | KWD_CONSTRUCTOR -> "KWD_CONSTRUCTOR"
    | KWD_FOR -> "KWD_FOR"
    | KWD_LAYOUT -> "KWD_LAYOUT"
    | KWD_LEXER -> "KWD_LEXER"
    | KWD_MERGE -> "KWD_MERGE"
    | KWD_MLI -> "KWD_MLI"
    | KWD_MLIMID -> "KWD_MLIMID"
    | KWD_MLITOP -> "KWD_MLITOP"
    | KWD_MLTOP -> "KWD_MLTOP"
    | KWD_NON_TERMINAL -> "KWD_NON_TERMINAL"
    | KWD_PARSER -> "KWD_PARSER"
    | KWD_RELATION -> "KWD_RELATION"
    | KWD_START -> "KWD_START"
    | KWD_TOKEN -> "KWD_TOKEN"
    | KWD_TYPE -> "KWD_TYPE"
    | LBRACE -> "LBRACE"
    | LBRACK -> "LBRACK"
    | LESS -> "LESS"
    | LET -> "LET"
    | LIDENT _ -> "LIDENT"
    | LPAREN -> "LPAREN"
    | LPARENGREATER -> "LPARENGREATER"
    | LPARENLESS -> "LPARENLESS"
    | OCAML_CODE _ -> "OCAML_CODE"
    | OCAML_TYPE _ -> "OCAML_TYPE"
    | PATTERN _ -> "PATTERN"
    | PERCENTPERCENT -> "PERCENTPERCENT"
    | PLUS -> "PLUS"
    | QUESTION -> "QUESTION"
    | RBRACE -> "RBRACE"
    | RBRACK -> "RBRACK"
    | RPAREN -> "RPAREN"
    | SEMI -> "SEMI"
    | STAR -> "STAR"
    | STRING _ -> "STRING"
    | THREEDOTS -> "THREEDOTS"
    | UIDENT _ -> "UIDENT"
  let ter_string_list = [
      ("ARROW",0);
      ("BANG",1);
      ("BAR",2);
      ("CARET",3);
      ("CHAR",4);
      ("COLON",5);
      ("COMMA",6);
      ("DASH",7);
      ("EOF",8);
      ("EQUAL",9);
      ("KWD_CONSTRUCTOR",10);
      ("KWD_FOR",11);
      ("KWD_LAYOUT",12);
      ("KWD_LEXER",13);
      ("KWD_MERGE",14);
      ("KWD_MLI",15);
      ("KWD_MLIMID",16);
      ("KWD_MLITOP",17);
      ("KWD_MLTOP",18);
      ("KWD_NON_TERMINAL",19);
      ("KWD_PARSER",20);
      ("KWD_RELATION",21);
      ("KWD_START",22);
      ("KWD_TOKEN",23);
      ("KWD_TYPE",24);
      ("LBRACE",25);
      ("LBRACK",26);
      ("LESS",27);
      ("LET",28);
      ("LIDENT",29);
      ("LPAREN",30);
      ("LPARENGREATER",31);
      ("LPARENLESS",32);
      ("OCAML_CODE",33);
      ("OCAML_TYPE",34);
      ("PATTERN",35);
      ("PERCENTPERCENT",36);
      ("PLUS",37);
      ("QUESTION",38);
      ("RBRACE",39);
      ("RBRACK",40);
      ("RPAREN",41);
      ("SEMI",42);
      ("STAR",43);
      ("STRING",44);
      ("THREEDOTS",45);
      ("UIDENT",46);]
end

type ('dypgen__Obj_action_prio, 'dypgen__Obj_aux_lexer, 'dypgen__Obj_aux_lexer_and, 'dypgen__Obj_aux_lexer_def, 'dypgen__Obj_aux_lexer_rule, 'dypgen__Obj_char_elt, 'dypgen__Obj_dypgen__nested_nt_0, 'dypgen__Obj_dypgen__nested_nt_1, 'dypgen__Obj_dypgen__nested_nt_10, 'dypgen__Obj_dypgen__nested_nt_11, 'dypgen__Obj_dypgen__nested_nt_2, 'dypgen__Obj_dypgen__nested_nt_3, 'dypgen__Obj_dypgen__nested_nt_4, 'dypgen__Obj_dypgen__nested_nt_5, 'dypgen__Obj_dypgen__nested_nt_6, 'dypgen__Obj_dypgen__nested_nt_7, 'dypgen__Obj_dypgen__nested_nt_8, 'dypgen__Obj_dypgen__nested_nt_9, 'dypgen__Obj_dypgen__option_BAR, 'dypgen__Obj_dypgen__option_SEMI, 'dypgen__Obj_entry_def, 'dypgen__Obj_grammar, 'dypgen__Obj_ident_list, 'dypgen__Obj_lexer, 'dypgen__Obj_lexer_rule, 'dypgen__Obj_lident, 'dypgen__Obj_lident_list, 'dypgen__Obj_main, 'dypgen__Obj_main_lexer, 'dypgen__Obj_opt_bang, 'dypgen__Obj_opt_dash, 'dypgen__Obj_opt_pattern, 'dypgen__Obj_opt_token_name, 'dypgen__Obj_optional_code, 'dypgen__Obj_optional_mli, 'dypgen__Obj_optional_mlimid, 'dypgen__Obj_optional_mlitop, 'dypgen__Obj_optional_mltop, 'dypgen__Obj_optional_trailer, 'dypgen__Obj_optional_type, 'dypgen__Obj_parser_begin, 'dypgen__Obj_parser_param_info, 'dypgen__Obj_parser_param_infos, 'dypgen__Obj_priority, 'dypgen__Obj_regexp, 'dypgen__Obj_regexp_decl, 'dypgen__Obj_regexp_ter, 'dypgen__Obj_relation, 'dypgen__Obj_relation_list, 'dypgen__Obj_rhs, 'dypgen__Obj_rhs_list, 'dypgen__Obj_symb, 'dypgen__Obj_symbol, 'dypgen__Obj_symbol_list, 'dypgen__Obj_token_list, 'dypgen__Obj_uident_list) obj =
  | Lexeme_matched of string
  | Obj_ARROW
  | Obj_BANG
  | Obj_BAR
  | Obj_CARET
  | Obj_CHAR of (char)
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
  | Obj_OCAML_TYPE of (string)
  | Obj_PATTERN of (string * Lexing.position)
  | Obj_PERCENTPERCENT
  | Obj_PLUS
  | Obj_QUESTION
  | Obj_RBRACE
  | Obj_RBRACK
  | Obj_RPAREN
  | Obj_SEMI
  | Obj_STAR
  | Obj_STRING of (string)
  | Obj_THREEDOTS
  | Obj_UIDENT of (string * (int * int * int * string))
  | Obj_action_prio of 'dypgen__Obj_action_prio
  | Obj_aux_lexer of 'dypgen__Obj_aux_lexer
  | Obj_aux_lexer_and of 'dypgen__Obj_aux_lexer_and
  | Obj_aux_lexer_def of 'dypgen__Obj_aux_lexer_def
  | Obj_aux_lexer_rule of 'dypgen__Obj_aux_lexer_rule
  | Obj_char_elt of 'dypgen__Obj_char_elt
  | Obj_dypgen__nested_nt_0 of 'dypgen__Obj_dypgen__nested_nt_0
  | Obj_dypgen__nested_nt_1 of 'dypgen__Obj_dypgen__nested_nt_1
  | Obj_dypgen__nested_nt_10 of 'dypgen__Obj_dypgen__nested_nt_10
  | Obj_dypgen__nested_nt_11 of 'dypgen__Obj_dypgen__nested_nt_11
  | Obj_dypgen__nested_nt_2 of 'dypgen__Obj_dypgen__nested_nt_2
  | Obj_dypgen__nested_nt_3 of 'dypgen__Obj_dypgen__nested_nt_3
  | Obj_dypgen__nested_nt_4 of 'dypgen__Obj_dypgen__nested_nt_4
  | Obj_dypgen__nested_nt_5 of 'dypgen__Obj_dypgen__nested_nt_5
  | Obj_dypgen__nested_nt_6 of 'dypgen__Obj_dypgen__nested_nt_6
  | Obj_dypgen__nested_nt_7 of 'dypgen__Obj_dypgen__nested_nt_7
  | Obj_dypgen__nested_nt_8 of 'dypgen__Obj_dypgen__nested_nt_8
  | Obj_dypgen__nested_nt_9 of 'dypgen__Obj_dypgen__nested_nt_9
  | Obj_dypgen__option_BAR of 'dypgen__Obj_dypgen__option_BAR
  | Obj_dypgen__option_SEMI of 'dypgen__Obj_dypgen__option_SEMI
  | Obj_dypgen__option_aux_lexer of 'dypgen__Obj_aux_lexer option
  | Obj_dypgen__option_lexer of 'dypgen__Obj_lexer option
  | Obj_dypgen__plus___char_elt of 'dypgen__Obj_char_elt list
  | Obj_dypgen__plus___lident of 'dypgen__Obj_lident list
  | Obj_dypgen__star___aux_lexer_and of 'dypgen__Obj_aux_lexer_and list
  | Obj_dypgen__star___lexer_rule of 'dypgen__Obj_lexer_rule list
  | Obj_dypgen__star___regexp_decl of 'dypgen__Obj_regexp_decl list
  | Obj_entry_def of 'dypgen__Obj_entry_def
  | Obj_grammar of 'dypgen__Obj_grammar
  | Obj_ident_list of 'dypgen__Obj_ident_list
  | Obj_lexer of 'dypgen__Obj_lexer
  | Obj_lexer_rule of 'dypgen__Obj_lexer_rule
  | Obj_lident of 'dypgen__Obj_lident
  | Obj_lident_list of 'dypgen__Obj_lident_list
  | Obj_main of 'dypgen__Obj_main
  | Obj_main_lexer of 'dypgen__Obj_main_lexer
  | Obj_opt_bang of 'dypgen__Obj_opt_bang
  | Obj_opt_dash of 'dypgen__Obj_opt_dash
  | Obj_opt_pattern of 'dypgen__Obj_opt_pattern
  | Obj_opt_token_name of 'dypgen__Obj_opt_token_name
  | Obj_optional_code of 'dypgen__Obj_optional_code
  | Obj_optional_mli of 'dypgen__Obj_optional_mli
  | Obj_optional_mlimid of 'dypgen__Obj_optional_mlimid
  | Obj_optional_mlitop of 'dypgen__Obj_optional_mlitop
  | Obj_optional_mltop of 'dypgen__Obj_optional_mltop
  | Obj_optional_trailer of 'dypgen__Obj_optional_trailer
  | Obj_optional_type of 'dypgen__Obj_optional_type
  | Obj_parser_begin of 'dypgen__Obj_parser_begin
  | Obj_parser_param_info of 'dypgen__Obj_parser_param_info
  | Obj_parser_param_infos of 'dypgen__Obj_parser_param_infos
  | Obj_priority of 'dypgen__Obj_priority
  | Obj_regexp of 'dypgen__Obj_regexp
  | Obj_regexp_decl of 'dypgen__Obj_regexp_decl
  | Obj_regexp_ter of 'dypgen__Obj_regexp_ter
  | Obj_relation of 'dypgen__Obj_relation
  | Obj_relation_list of 'dypgen__Obj_relation_list
  | Obj_rhs of 'dypgen__Obj_rhs
  | Obj_rhs_list of 'dypgen__Obj_rhs_list
  | Obj_symb of 'dypgen__Obj_symb
  | Obj_symbol of 'dypgen__Obj_symbol
  | Obj_symbol_list of 'dypgen__Obj_symbol_list
  | Obj_token_list of 'dypgen__Obj_token_list
  | Obj_uident_list of 'dypgen__Obj_uident_list

module Dyp_symbols_array =
struct
  let token_name_array =
  [|"ARROW";
    "BANG";
    "BAR";
    "CARET";
    "CHAR";
    "COLON";
    "COMMA";
    "DASH";
    "EOF";
    "EQUAL";
    "KWD_CONSTRUCTOR";
    "KWD_FOR";
    "KWD_LAYOUT";
    "KWD_LEXER";
    "KWD_MERGE";
    "KWD_MLI";
    "KWD_MLIMID";
    "KWD_MLITOP";
    "KWD_MLTOP";
    "KWD_NON_TERMINAL";
    "KWD_PARSER";
    "KWD_RELATION";
    "KWD_START";
    "KWD_TOKEN";
    "KWD_TYPE";
    "LBRACE";
    "LBRACK";
    "LESS";
    "LET";
    "LIDENT";
    "LPAREN";
    "LPARENGREATER";
    "LPARENLESS";
    "OCAML_CODE";
    "OCAML_TYPE";
    "PATTERN";
    "PERCENTPERCENT";
    "PLUS";
    "QUESTION";
    "RBRACE";
    "RBRACK";
    "RPAREN";
    "SEMI";
    "STAR";
    "STRING";
    "THREEDOTS";
    "UIDENT"|]
  let nt_cons_list =
  [
    ("action_prio",8);
    ("aux_lexer",9);
    ("aux_lexer_and",10);
    ("aux_lexer_def",11);
    ("aux_lexer_rule",12);
    ("char_elt",13);
    ("dypgen__nested_nt_0",14);
    ("dypgen__nested_nt_1",15);
    ("dypgen__nested_nt_10",16);
    ("dypgen__nested_nt_11",17);
    ("dypgen__nested_nt_2",18);
    ("dypgen__nested_nt_3",19);
    ("dypgen__nested_nt_4",20);
    ("dypgen__nested_nt_5",21);
    ("dypgen__nested_nt_6",22);
    ("dypgen__nested_nt_7",23);
    ("dypgen__nested_nt_8",24);
    ("dypgen__nested_nt_9",25);
    ("dypgen__option_BAR",26);
    ("dypgen__option_SEMI",27);
    ("dypgen__option_aux_lexer",28);
    ("dypgen__option_lexer",29);
    ("dypgen__plus___char_elt",30);
    ("dypgen__plus___lident",31);
    ("dypgen__star___aux_lexer_and",32);
    ("dypgen__star___lexer_rule",33);
    ("dypgen__star___regexp_decl",34);
    ("entry_def",35);
    ("grammar",36);
    ("ident_list",37);
    ("lexer",38);
    ("lexer_rule",39);
    ("lident",40);
    ("lident_list",41);
    ("main",42);
    ("main_lexer",43);
    ("opt_bang",44);
    ("opt_dash",45);
    ("opt_pattern",46);
    ("opt_token_name",47);
    ("optional_code",48);
    ("optional_mli",49);
    ("optional_mlimid",50);
    ("optional_mlitop",51);
    ("optional_mltop",52);
    ("optional_trailer",53);
    ("optional_type",54);
    ("parser_begin",55);
    ("parser_param_info",56);
    ("parser_param_infos",57);
    ("priority",58);
    ("regexp",59);
    ("regexp_decl",60);
    ("regexp_ter",61);
    ("relation",62);
    ("relation_list",63);
    ("rhs",64);
    ("rhs_list",65);
    ("symb",66);
    ("symbol",67);
    ("symbol_list",68);
    ("token_list",69);
    ("uident_list",70)]
  let str_cons o = match o with
    | Lexeme_matched _ -> "Lexeme_matched"
    | Obj_CHAR _ -> "Obj_CHAR"
    | Obj_LIDENT _ -> "Obj_LIDENT"
    | Obj_OCAML_CODE _ -> "Obj_OCAML_CODE"
    | Obj_OCAML_TYPE _ -> "Obj_OCAML_TYPE"
    | Obj_PATTERN _ -> "Obj_PATTERN"
    | Obj_STRING _ -> "Obj_STRING"
    | Obj_UIDENT _ -> "Obj_UIDENT"
    | Obj_action_prio _ -> "Obj_action_prio"
    | Obj_aux_lexer _ -> "Obj_aux_lexer"
    | Obj_aux_lexer_and _ -> "Obj_aux_lexer_and"
    | Obj_aux_lexer_def _ -> "Obj_aux_lexer_def"
    | Obj_aux_lexer_rule _ -> "Obj_aux_lexer_rule"
    | Obj_char_elt _ -> "Obj_char_elt"
    | Obj_dypgen__nested_nt_0 _ -> "Obj_dypgen__nested_nt_0"
    | Obj_dypgen__nested_nt_1 _ -> "Obj_dypgen__nested_nt_1"
    | Obj_dypgen__nested_nt_10 _ -> "Obj_dypgen__nested_nt_10"
    | Obj_dypgen__nested_nt_11 _ -> "Obj_dypgen__nested_nt_11"
    | Obj_dypgen__nested_nt_2 _ -> "Obj_dypgen__nested_nt_2"
    | Obj_dypgen__nested_nt_3 _ -> "Obj_dypgen__nested_nt_3"
    | Obj_dypgen__nested_nt_4 _ -> "Obj_dypgen__nested_nt_4"
    | Obj_dypgen__nested_nt_5 _ -> "Obj_dypgen__nested_nt_5"
    | Obj_dypgen__nested_nt_6 _ -> "Obj_dypgen__nested_nt_6"
    | Obj_dypgen__nested_nt_7 _ -> "Obj_dypgen__nested_nt_7"
    | Obj_dypgen__nested_nt_8 _ -> "Obj_dypgen__nested_nt_8"
    | Obj_dypgen__nested_nt_9 _ -> "Obj_dypgen__nested_nt_9"
    | Obj_dypgen__option_BAR _ -> "Obj_dypgen__option_BAR"
    | Obj_dypgen__option_SEMI _ -> "Obj_dypgen__option_SEMI"
    | Obj_dypgen__option_aux_lexer _ -> "Obj_dypgen__option_aux_lexer"
    | Obj_dypgen__option_lexer _ -> "Obj_dypgen__option_lexer"
    | Obj_dypgen__plus___char_elt _ -> "Obj_dypgen__plus___char_elt"
    | Obj_dypgen__plus___lident _ -> "Obj_dypgen__plus___lident"
    | Obj_dypgen__star___aux_lexer_and _ -> "Obj_dypgen__star___aux_lexer_and"
    | Obj_dypgen__star___lexer_rule _ -> "Obj_dypgen__star___lexer_rule"
    | Obj_dypgen__star___regexp_decl _ -> "Obj_dypgen__star___regexp_decl"
    | Obj_entry_def _ -> "Obj_entry_def"
    | Obj_grammar _ -> "Obj_grammar"
    | Obj_ident_list _ -> "Obj_ident_list"
    | Obj_lexer _ -> "Obj_lexer"
    | Obj_lexer_rule _ -> "Obj_lexer_rule"
    | Obj_lident _ -> "Obj_lident"
    | Obj_lident_list _ -> "Obj_lident_list"
    | Obj_main _ -> "Obj_main"
    | Obj_main_lexer _ -> "Obj_main_lexer"
    | Obj_opt_bang _ -> "Obj_opt_bang"
    | Obj_opt_dash _ -> "Obj_opt_dash"
    | Obj_opt_pattern _ -> "Obj_opt_pattern"
    | Obj_opt_token_name _ -> "Obj_opt_token_name"
    | Obj_optional_code _ -> "Obj_optional_code"
    | Obj_optional_mli _ -> "Obj_optional_mli"
    | Obj_optional_mlimid _ -> "Obj_optional_mlimid"
    | Obj_optional_mlitop _ -> "Obj_optional_mlitop"
    | Obj_optional_mltop _ -> "Obj_optional_mltop"
    | Obj_optional_trailer _ -> "Obj_optional_trailer"
    | Obj_optional_type _ -> "Obj_optional_type"
    | Obj_parser_begin _ -> "Obj_parser_begin"
    | Obj_parser_param_info _ -> "Obj_parser_param_info"
    | Obj_parser_param_infos _ -> "Obj_parser_param_infos"
    | Obj_priority _ -> "Obj_priority"
    | Obj_regexp _ -> "Obj_regexp"
    | Obj_regexp_decl _ -> "Obj_regexp_decl"
    | Obj_regexp_ter _ -> "Obj_regexp_ter"
    | Obj_relation _ -> "Obj_relation"
    | Obj_relation_list _ -> "Obj_relation_list"
    | Obj_rhs _ -> "Obj_rhs"
    | Obj_rhs_list _ -> "Obj_rhs_list"
    | Obj_symb _ -> "Obj_symb"
    | Obj_symbol _ -> "Obj_symbol"
    | Obj_symbol_list _ -> "Obj_symbol_list"
    | Obj_token_list _ -> "Obj_token_list"
    | Obj_uident_list _ -> "Obj_uident_list"
    | _ -> failwith "str_cons, unexpected constructor"
  let cons_array = [|
    "Lexeme_matched";
    "Obj_CHAR";
    "Obj_LIDENT";
    "Obj_OCAML_CODE";
    "Obj_OCAML_TYPE";
    "Obj_PATTERN";
    "Obj_STRING";
    "Obj_UIDENT";
    "Obj_action_prio";
    "Obj_aux_lexer";
    "Obj_aux_lexer_and";
    "Obj_aux_lexer_def";
    "Obj_aux_lexer_rule";
    "Obj_char_elt";
    "Obj_dypgen__nested_nt_0";
    "Obj_dypgen__nested_nt_1";
    "Obj_dypgen__nested_nt_10";
    "Obj_dypgen__nested_nt_11";
    "Obj_dypgen__nested_nt_2";
    "Obj_dypgen__nested_nt_3";
    "Obj_dypgen__nested_nt_4";
    "Obj_dypgen__nested_nt_5";
    "Obj_dypgen__nested_nt_6";
    "Obj_dypgen__nested_nt_7";
    "Obj_dypgen__nested_nt_8";
    "Obj_dypgen__nested_nt_9";
    "Obj_dypgen__option_BAR";
    "Obj_dypgen__option_SEMI";
    "Obj_dypgen__option_aux_lexer";
    "Obj_dypgen__option_lexer";
    "Obj_dypgen__plus___char_elt";
    "Obj_dypgen__plus___lident";
    "Obj_dypgen__star___aux_lexer_and";
    "Obj_dypgen__star___lexer_rule";
    "Obj_dypgen__star___regexp_decl";
    "Obj_entry_def";
    "Obj_grammar";
    "Obj_ident_list";
    "Obj_lexer";
    "Obj_lexer_rule";
    "Obj_lident";
    "Obj_lident_list";
    "Obj_main";
    "Obj_main_lexer";
    "Obj_opt_bang";
    "Obj_opt_dash";
    "Obj_opt_pattern";
    "Obj_opt_token_name";
    "Obj_optional_code";
    "Obj_optional_mli";
    "Obj_optional_mlimid";
    "Obj_optional_mlitop";
    "Obj_optional_mltop";
    "Obj_optional_trailer";
    "Obj_optional_type";
    "Obj_parser_begin";
    "Obj_parser_param_info";
    "Obj_parser_param_infos";
    "Obj_priority";
    "Obj_regexp";
    "Obj_regexp_decl";
    "Obj_regexp_ter";
    "Obj_relation";
    "Obj_relation_list";
    "Obj_rhs";
    "Obj_rhs_list";
    "Obj_symb";
    "Obj_symbol";
    "Obj_symbol_list";
    "Obj_token_list";
    "Obj_uident_list";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
    "";
  |]
  let entry_points = [
    "main";]
end

let dypgen_lexbuf_position lexbuf =
  (lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)

module Dyp_aux_functions =
struct
  let get_token_value t = match t with
    | ARROW -> Obj_ARROW
    | BANG -> Obj_BANG
    | BAR -> Obj_BAR
    | CARET -> Obj_CARET
    | CHAR x -> Obj_CHAR x
    | COLON -> Obj_COLON
    | COMMA -> Obj_COMMA
    | DASH -> Obj_DASH
    | EOF -> Obj_EOF
    | EQUAL -> Obj_EQUAL
    | KWD_CONSTRUCTOR -> Obj_KWD_CONSTRUCTOR
    | KWD_FOR -> Obj_KWD_FOR
    | KWD_LAYOUT -> Obj_KWD_LAYOUT
    | KWD_LEXER -> Obj_KWD_LEXER
    | KWD_MERGE -> Obj_KWD_MERGE
    | KWD_MLI -> Obj_KWD_MLI
    | KWD_MLIMID -> Obj_KWD_MLIMID
    | KWD_MLITOP -> Obj_KWD_MLITOP
    | KWD_MLTOP -> Obj_KWD_MLTOP
    | KWD_NON_TERMINAL -> Obj_KWD_NON_TERMINAL
    | KWD_PARSER -> Obj_KWD_PARSER
    | KWD_RELATION -> Obj_KWD_RELATION
    | KWD_START -> Obj_KWD_START
    | KWD_TOKEN -> Obj_KWD_TOKEN
    | KWD_TYPE -> Obj_KWD_TYPE
    | LBRACE -> Obj_LBRACE
    | LBRACK -> Obj_LBRACK
    | LESS -> Obj_LESS
    | LET -> Obj_LET
    | LIDENT x -> Obj_LIDENT x
    | LPAREN -> Obj_LPAREN
    | LPARENGREATER -> Obj_LPARENGREATER
    | LPARENLESS -> Obj_LPARENLESS
    | OCAML_CODE x -> Obj_OCAML_CODE x
    | OCAML_TYPE x -> Obj_OCAML_TYPE x
    | PATTERN x -> Obj_PATTERN x
    | PERCENTPERCENT -> Obj_PERCENTPERCENT
    | PLUS -> Obj_PLUS
    | QUESTION -> Obj_QUESTION
    | RBRACE -> Obj_RBRACE
    | RBRACK -> Obj_RBRACK
    | RPAREN -> Obj_RPAREN
    | SEMI -> Obj_SEMI
    | STAR -> Obj_STAR
    | STRING x -> Obj_STRING x
    | THREEDOTS -> Obj_THREEDOTS
    | UIDENT x -> Obj_UIDENT x
  let cons_table = Dyp.Tools.hashtbl_of_array Dyp_symbols_array.cons_array
end

module Dyp_priority_data =
struct
  let relations = [
    ["p1";"p2";"pSeq";"pAlt";];
  ]
end

let global_data = ()
let local_data = ()
let global_data_equal = (==)
let local_data_equal = (==)

let dyp_merge_Lexeme_matched = Dyp.Tools.keep_zero
let dyp_merge_Obj_CHAR = Dyp.Tools.keep_zero
let dyp_merge_Obj_LIDENT = Dyp.Tools.keep_zero
let dyp_merge_Obj_OCAML_CODE = Dyp.Tools.keep_zero
let dyp_merge_Obj_OCAML_TYPE = Dyp.Tools.keep_zero
let dyp_merge_Obj_PATTERN = Dyp.Tools.keep_zero
let dyp_merge_Obj_STRING = Dyp.Tools.keep_zero
let dyp_merge_Obj_UIDENT = Dyp.Tools.keep_zero
let dyp_merge_Obj_action_prio = Dyp.Tools.keep_zero
let dyp_merge_Obj_aux_lexer = Dyp.Tools.keep_zero
let dyp_merge_Obj_aux_lexer_and = Dyp.Tools.keep_zero
let dyp_merge_Obj_aux_lexer_def = Dyp.Tools.keep_zero
let dyp_merge_Obj_aux_lexer_rule = Dyp.Tools.keep_zero
let dyp_merge_Obj_char_elt = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_0 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_1 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_10 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_11 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_2 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_3 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_4 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_5 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_6 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_7 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_8 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__nested_nt_9 = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_BAR = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_SEMI = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_aux_lexer = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__option_lexer = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__plus___char_elt = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__plus___lident = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__star___aux_lexer_and = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__star___lexer_rule = Dyp.Tools.keep_zero
let dyp_merge_Obj_dypgen__star___regexp_decl = Dyp.Tools.keep_zero
let dyp_merge_Obj_entry_def = Dyp.Tools.keep_zero
let dyp_merge_Obj_grammar = Dyp.Tools.keep_zero
let dyp_merge_Obj_ident_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_lexer = Dyp.Tools.keep_zero
let dyp_merge_Obj_lexer_rule = Dyp.Tools.keep_zero
let dyp_merge_Obj_lident = Dyp.Tools.keep_zero
let dyp_merge_Obj_lident_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_main = Dyp.Tools.keep_zero
let dyp_merge_Obj_main_lexer = Dyp.Tools.keep_zero
let dyp_merge_Obj_opt_bang = Dyp.Tools.keep_zero
let dyp_merge_Obj_opt_dash = Dyp.Tools.keep_zero
let dyp_merge_Obj_opt_pattern = Dyp.Tools.keep_zero
let dyp_merge_Obj_opt_token_name = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_code = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_mli = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_mlimid = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_mlitop = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_mltop = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_trailer = Dyp.Tools.keep_zero
let dyp_merge_Obj_optional_type = Dyp.Tools.keep_zero
let dyp_merge_Obj_parser_begin = Dyp.Tools.keep_zero
let dyp_merge_Obj_parser_param_info = Dyp.Tools.keep_zero
let dyp_merge_Obj_parser_param_infos = Dyp.Tools.keep_zero
let dyp_merge_Obj_priority = Dyp.Tools.keep_zero
let dyp_merge_Obj_regexp = Dyp.Tools.keep_zero
let dyp_merge_Obj_regexp_decl = Dyp.Tools.keep_zero
let dyp_merge_Obj_regexp_ter = Dyp.Tools.keep_zero
let dyp_merge_Obj_relation = Dyp.Tools.keep_zero
let dyp_merge_Obj_relation_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_rhs = Dyp.Tools.keep_zero
let dyp_merge_Obj_rhs_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_symb = Dyp.Tools.keep_zero
let dyp_merge_Obj_symbol = Dyp.Tools.keep_zero
let dyp_merge_Obj_symbol_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_token_list = Dyp.Tools.keep_zero
let dyp_merge_Obj_uident_list = Dyp.Tools.keep_zero
let dyp_merge = Dyp.keep_one
let dypgen_match_length = `shortest
let dypgen_choose_token = `first
let dypgen_keep_data = `both
let dypgen_use_rule_order = false
let dypgen_use_all_actions = false

# 1 "dypgen_parser.dyp"

open Printf
open Parse_tree
open Dyp

let () = dypgen_verbose := 0

let use_dyplex = ref false

let empty_ppi = {
  token_list = [];
  relation = [];
  start = [];
  generic_merge = [];
  cons = [];
  additional_cons = [];
  nt_type = [];
  single_nt = [];
  layout = [] }

module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module String_set = Set.Make(Ordered_string)

type gd = {
  gd_count : int;
  gd_symbs : String_set.t;
  gd_regexp_decl : String_set.t }

let global_data = {
  gd_count = 0;
  gd_symbs = String_set.empty;
  gd_regexp_decl = String_set.empty }

let is_nested nt =
  try
    let sub = String.sub nt 0 18 in
    if sub = "dypgen__nested_nt_" then true else false
  with Invalid_argument _ -> false

let dummy_code = ("",Lexing.dummy_pos)
let dummy_p = -1,-1,-1,""
(*let dummy_pat_syn = "_", (Pat_syn ""), Lexing.dummy_pos*)
let dummy_pat_inh = "_", (Pat_inh ""), Lexing.dummy_pos

let symbol_list dyp op (symb, add_rules) =
  let { gd_count=count; gd_symbs=symbs} = dyp.global_data in
  let pat_typ = match symb with
    | Symb_non_terminal ((s,_),_,_,_)
    | Symb_non_terminal_NL ((s,_),_,_,_) -> s
    | Symb_terminal s | Symb_terminal_NL s -> fst s
    | _ -> assert false
  in
  let ssymb = "dypgen__"^op^"___"^pat_typ in
  if String_set.mem ssymb symbs then
    ((Symb_non_terminal
     ((ssymb,dummy_p),("No_priority",dummy_p),Pr_eq,dummy_code)),
    add_rules), []
  else
  let count, symbs, lnt2, lnt1 =
    if is_nested pat_typ then
      count+2, symbs,
      "dypgen__nested_nt_"^(string_of_int count),
      "dypgen__nested_nt_"^(string_of_int (count+1))
    else
      count+1, (String_set.add ssymb symbs), ssymb,
      "dypgen__nested_nt_"^(string_of_int count)
  in
  let rule1 =
    if op = "star" then
      (lnt1,[dummy_pat_inh]),("default_priority",(-1,-1,-1,"")),[],
      ("[]",(Lexing.dummy_pos,false)), (true,true)
    else
      (lnt1,[dummy_pat_inh]),("default_priority",(-1,-1,-1,"")),
      [(symb,("_",(Pat_syn pat_typ),Lexing.dummy_pos))],
      ("[_1]",(Lexing.dummy_pos,false)), (true,true)
  in
  let lnt1_pack, lnt2_pack =
    (Symb_non_terminal ((lnt1,dummy_p),
      ("No_priority",dummy_p),Pr_eq,dummy_code)),
    (Symb_non_terminal ((lnt2,dummy_p),
      ("No_priority",dummy_p),Pr_eq,dummy_code))
  in
  let rule2 =
    (lnt1,[dummy_pat_inh]),("default_priority",dummy_p),
    [(lnt1_pack,("_",(Pat_syn lnt1),Lexing.dummy_pos));
    (symb,("_",(Pat_syn pat_typ),Lexing.dummy_pos))],
    ("_2::_1",(Lexing.dummy_pos,false)), (true,true)
  in
  let rule3 =
    (lnt2,[dummy_pat_inh]),("default_priority",dummy_p),
    [(lnt1_pack,("_",(Pat_syn lnt1),Lexing.dummy_pos))],
    ("List.rev _1",(Lexing.dummy_pos,false)), (true,true)
  in
  (lnt2_pack, (rule1::rule2::rule3::add_rules)),
  [Dyp.Global_data {dyp.global_data with gd_count=count; gd_symbs=symbs}]

let discard_nt nt =
  if nt = "eof" then true else
  is_nested nt

let rec dyp_merge_Obj_symbol_list = function
  | ((o1,gd,ld) as h)::((o2,_,_)::t as l1) ->
  begin match o1 with
  (*| (((Symb_regexp (RE_Seq _)),_)::_,_,_,_)
    | (((Symb_regexp_NL (RE_Seq _)),_)::_,_,_,_) ->
      dyp_merge_Obj_symbol_list l1 *)
      (* prevents several non terminals to be mistaken with
      a sequence of regular expressions *)
    | (((Symb_non_terminal ((nt,_),_,_,_)),_)::_,_)
    | (((Symb_non_terminal_NL ((nt,_),_,_,_)),_)::_,_)
      when (discard_nt nt) ->
      dyp_merge_Obj_symbol_list l1
      (* eof is a regexp rather than a non terminal symbol *)
      (* nested rules are discarded when in competition with other
      interpretations: ['a'-'z'] is always a character set and not
      a nested rule *)
    | _ -> dyp_merge_Obj_symbol_list (h::t)
  end
  | [o,gd,ld] -> [o],gd,ld
  | _ -> assert false

let dypgen_keep_data = `none


let _ = () (* dummy line to improve OCaml error location *)
# 895                "dypgen_parser_temp.ml"
let __dypgen_ra_list, __dypgen_main_lexer, __dypgen_aux_lexer =
[
(("main",[Dyp.Non_ter ("optional_mltop",Dyp.No_priority );Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Non_ter ("parser_param_infos",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_lexer",Dyp.No_priority );Dyp.Non_ter ("parser_begin",Dyp.No_priority );Dyp.Non_ter ("grammar",Dyp.No_priority );Dyp.Non_ter ("optional_trailer",Dyp.No_priority );Dyp.Non_ter ("optional_mlitop",Dyp.No_priority );Dyp.Non_ter ("optional_mlimid",Dyp.No_priority );Dyp.Non_ter ("optional_mli",Dyp.No_priority );Dyp.Ter "EOF"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_optional_mltop ( (
(_:'dypgen__Obj_optional_mltop)
# 902                "dypgen_parser_temp.ml"
 as _1));Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 905                "dypgen_parser_temp.ml"
 as _2));Obj_parser_param_infos ( (
(_:'dypgen__Obj_parser_param_infos)
# 908                "dypgen_parser_temp.ml"
 as _3));Obj_dypgen__option_lexer ( (
(_:'dypgen__Obj_lexer option)
# 911                "dypgen_parser_temp.ml"
 as _4));Obj_parser_begin ( (
(_:'dypgen__Obj_parser_begin)
# 914                "dypgen_parser_temp.ml"
 as _5));Obj_grammar ( (
(_:'dypgen__Obj_grammar)
# 917                "dypgen_parser_temp.ml"
 as _6));Obj_optional_trailer ( (
(_:'dypgen__Obj_optional_trailer)
# 920                "dypgen_parser_temp.ml"
 as _7));Obj_optional_mlitop ( (
(_:'dypgen__Obj_optional_mlitop)
# 923                "dypgen_parser_temp.ml"
 as _8));Obj_optional_mlimid ( (
(_:'dypgen__Obj_optional_mlimid)
# 926                "dypgen_parser_temp.ml"
 as _9));Obj_optional_mli ( (
(_:'dypgen__Obj_optional_mli)
# 929                "dypgen_parser_temp.ml"
 as _10)); _11] -> Obj_main 
# 146 "dypgen_parser.dyp"
(
  ( (_1,_2,_3,_4,(*List.rev*) _6,_7,_8,_9,_10) ):'dypgen__Obj_main)
# 934                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_lexer",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_lexer 
(
(None):'dypgen__Obj_lexer option)
# 943                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_lexer",[Dyp.Non_ter ("lexer",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lexer ( (
(_:'dypgen__Obj_lexer)
# 951                "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__option_lexer 
(
(Some _1):'dypgen__Obj_lexer option)
# 955                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_begin",[Dyp.Ter "PERCENTPERCENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_parser_begin 
# 149 "dypgen_parser.dyp"
(
                  ():'dypgen__Obj_parser_begin)
# 965                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_begin",[Dyp.Ter "KWD_PARSER"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_parser_begin 
# 150 "dypgen_parser.dyp"
(
                   ():'dypgen__Obj_parser_begin)
# 975                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lexer",[Dyp.Ter "KWD_LEXER";Dyp.Non_ter ("dypgen__star___regexp_decl",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_2",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__star___regexp_decl ( (
(_:'dypgen__Obj_regexp_decl list)
# 983                "dypgen_parser_temp.ml"
 as _2));Obj_dypgen__nested_nt_2 ( (
(_:'dypgen__Obj_dypgen__nested_nt_2)
# 986                "dypgen_parser_temp.ml"
 as _3))] -> Obj_lexer 
# 154 "dypgen_parser.dyp"
(
    ( match _2, _3 with
    | [], None -> [], [], []
    | _, None -> use_dyplex := true; (_2, [], [])
    | _, Some (auxl, mainl) ->
    use_dyplex := true;
    (_2, (match auxl with None -> [] | Some l -> l), mainl) ):'dypgen__Obj_lexer)
# 996                "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__star___regexp_decl",[Dyp.Non_ter ("dypgen__nested_nt_0",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_0 ( (
(_:'dypgen__Obj_dypgen__nested_nt_0)
# 1004               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__star___regexp_decl 
(
(List.rev _1):'dypgen__Obj_regexp_decl list)
# 1008               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_0",[Dyp.Non_ter ("dypgen__nested_nt_0",Dyp.No_priority );Dyp.Non_ter ("regexp_decl",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_0 ( (
(_:'dypgen__Obj_dypgen__nested_nt_0)
# 1016               "dypgen_parser_temp.ml"
 as _1));Obj_regexp_decl ( (
(_:'dypgen__Obj_regexp_decl)
# 1019               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_0 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_0)
# 1023               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_0",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_0 
(
([]):'dypgen__Obj_dypgen__nested_nt_0)
# 1032               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_1",[Dyp.Non_ter ("dypgen__option_aux_lexer",Dyp.No_priority );Dyp.Non_ter ("main_lexer",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_aux_lexer ( (
(_:'dypgen__Obj_aux_lexer option)
# 1040               "dypgen_parser_temp.ml"
 as _1));Obj_main_lexer ( (
(_:'dypgen__Obj_main_lexer)
# 1043               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_1 
# 153 "dypgen_parser.dyp"
(
                                                  (_1,_2):'dypgen__Obj_dypgen__nested_nt_1)
# 1048               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_aux_lexer",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_aux_lexer 
(
(None):'dypgen__Obj_aux_lexer option)
# 1057               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_aux_lexer",[Dyp.Non_ter ("aux_lexer",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_aux_lexer ( (
(_:'dypgen__Obj_aux_lexer)
# 1065               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__option_aux_lexer 
(
(Some _1):'dypgen__Obj_aux_lexer option)
# 1069               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_2",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_2 
(
(None):'dypgen__Obj_dypgen__nested_nt_2)
# 1078               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_2",[Dyp.Non_ter ("dypgen__nested_nt_1",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_1 ( (
(_:'dypgen__Obj_dypgen__nested_nt_1)
# 1086               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__nested_nt_2 
(
(Some _1):'dypgen__Obj_dypgen__nested_nt_2)
# 1090               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("main_lexer",[Dyp.Ter "LIDENT";Dyp.Ter "LIDENT";Dyp.Ter "EQUAL";Dyp.Non_ter ("dypgen__star___lexer_rule",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 163 "dypgen_parser.dyp"
        ("main",_:(string * (int * int * int * string)))
# 1099               "dypgen_parser_temp.ml"
 as _1);Obj_LIDENT  (
# 163 "dypgen_parser.dyp"
                         ("lexer",_:(string * (int * int * int * string)))
# 1103               "dypgen_parser_temp.ml"
 as _2); _3;Obj_dypgen__star___lexer_rule ( (
(_:'dypgen__Obj_lexer_rule list)
# 1106               "dypgen_parser_temp.ml"
 as _4))] -> Obj_main_lexer 
# 162 "dypgen_parser.dyp"
(
                                                        ( _4 ):'dypgen__Obj_main_lexer)
# 1111               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__star___lexer_rule",[Dyp.Non_ter ("dypgen__nested_nt_3",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_3 ( (
(_:'dypgen__Obj_dypgen__nested_nt_3)
# 1119               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__star___lexer_rule 
(
(List.rev _1):'dypgen__Obj_lexer_rule list)
# 1123               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_3",[Dyp.Non_ter ("dypgen__nested_nt_3",Dyp.No_priority );Dyp.Non_ter ("lexer_rule",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_3 ( (
(_:'dypgen__Obj_dypgen__nested_nt_3)
# 1131               "dypgen_parser_temp.ml"
 as _1));Obj_lexer_rule ( (
(_:'dypgen__Obj_lexer_rule)
# 1134               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_3 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_3)
# 1138               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_3",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_3 
(
([]):'dypgen__Obj_dypgen__nested_nt_3)
# 1147               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lexer_rule",[Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Ter "ARROW";Dyp.Non_ter ("opt_token_name",Dyp.No_priority );Dyp.Non_ter ("optional_code",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1155               "dypgen_parser_temp.ml"
 as _1)); _2;Obj_opt_token_name ( (
(_:'dypgen__Obj_opt_token_name)
# 1158               "dypgen_parser_temp.ml"
 as _3));Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 1161               "dypgen_parser_temp.ml"
 as _4))] -> Obj_lexer_rule 
# 165 "dypgen_parser.dyp"
(
                                            ( (_1,_3,_4) ):'dypgen__Obj_lexer_rule)
# 1166               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_token_name",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_opt_token_name 
# 168 "dypgen_parser.dyp"
(
    ( "__dypgen_layout" ):'dypgen__Obj_opt_token_name)
# 1176               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_token_name",[Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_UIDENT  (
# 170 "dypgen_parser.dyp"
          (n,_:(string * (int * int * int * string)))
# 1185               "dypgen_parser_temp.ml"
 as _1)] -> Obj_opt_token_name 
# 169 "dypgen_parser.dyp"
(
                ( n ):'dypgen__Obj_opt_token_name)
# 1190               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp_decl",[Dyp.Ter "LET";Dyp.Ter "LIDENT";Dyp.Ter "EQUAL";Dyp.Non_ter ("regexp",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
# 173 "dypgen_parser.dyp"
            (n,_:(string * (int * int * int * string)))
# 1199               "dypgen_parser_temp.ml"
 as _2); _3;Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1202               "dypgen_parser_temp.ml"
 as _4))] ->  let res = 
# 173 "dypgen_parser.dyp"
(
    ( (n,_4),
    [Dyp.Global_data {dyp.global_data with gd_regexp_decl =
      String_set.add n dyp.global_data.gd_regexp_decl}] ):'dypgen__Obj_regexp_decl * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 1209               "dypgen_parser_temp.ml"
  in Obj_regexp_decl(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("aux_lexer",[Dyp.Non_ter ("aux_lexer_rule",Dyp.No_priority );Dyp.Non_ter ("dypgen__star___aux_lexer_and",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_aux_lexer_rule ( (
(_:'dypgen__Obj_aux_lexer_rule)
# 1218               "dypgen_parser_temp.ml"
 as _1));Obj_dypgen__star___aux_lexer_and ( (
(_:'dypgen__Obj_aux_lexer_and list)
# 1221               "dypgen_parser_temp.ml"
 as _2))] -> Obj_aux_lexer 
# 178 "dypgen_parser.dyp"
(
                                 ( _1::_2 ):'dypgen__Obj_aux_lexer)
# 1226               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__star___aux_lexer_and",[Dyp.Non_ter ("dypgen__nested_nt_4",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_4 ( (
(_:'dypgen__Obj_dypgen__nested_nt_4)
# 1234               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__star___aux_lexer_and 
(
(List.rev _1):'dypgen__Obj_aux_lexer_and list)
# 1238               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_4",[Dyp.Non_ter ("dypgen__nested_nt_4",Dyp.No_priority );Dyp.Non_ter ("aux_lexer_and",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_4 ( (
(_:'dypgen__Obj_dypgen__nested_nt_4)
# 1246               "dypgen_parser_temp.ml"
 as _1));Obj_aux_lexer_and ( (
(_:'dypgen__Obj_aux_lexer_and)
# 1249               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_4 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_4)
# 1253               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_4",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_4 
(
([]):'dypgen__Obj_dypgen__nested_nt_4)
# 1262               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("aux_lexer_rule",[Dyp.Ter "LIDENT";Dyp.Non_ter ("dypgen__plus___lident",Dyp.No_priority );Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Non_ter ("aux_lexer_def",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 182 "dypgen_parser.dyp"
        ("rule",_:(string * (int * int * int * string)))
# 1271               "dypgen_parser_temp.ml"
 as _1);Obj_dypgen__plus___lident ( (
(_:'dypgen__Obj_lident list)
# 1274               "dypgen_parser_temp.ml"
 as _2)); _3;Obj_LIDENT  (
# 182 "dypgen_parser.dyp"
                                        ("parse",_:(string * (int * int * int * string)))
# 1278               "dypgen_parser_temp.ml"
 as _4);Obj_aux_lexer_def ( (
(_:'dypgen__Obj_aux_lexer_def)
# 1281               "dypgen_parser_temp.ml"
 as _5))] -> Obj_aux_lexer_rule 
# 182 "dypgen_parser.dyp"
(
  ( (_2,_5) ):'dypgen__Obj_aux_lexer_rule)
# 1286               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__plus___lident",[Dyp.Non_ter ("dypgen__nested_nt_5",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_5 ( (
(_:'dypgen__Obj_dypgen__nested_nt_5)
# 1294               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__plus___lident 
(
(List.rev _1):'dypgen__Obj_lident list)
# 1298               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_5",[Dyp.Non_ter ("dypgen__nested_nt_5",Dyp.No_priority );Dyp.Non_ter ("lident",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_5 ( (
(_:'dypgen__Obj_dypgen__nested_nt_5)
# 1306               "dypgen_parser_temp.ml"
 as _1));Obj_lident ( (
(_:'dypgen__Obj_lident)
# 1309               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_5 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_5)
# 1313               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_5",[Dyp.Non_ter ("lident",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lident ( (
(_:'dypgen__Obj_lident)
# 1321               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__nested_nt_5 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_5)
# 1325               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("aux_lexer_and",[Dyp.Ter "LIDENT";Dyp.Non_ter ("dypgen__plus___lident",Dyp.No_priority );Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Non_ter ("aux_lexer_def",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 186 "dypgen_parser.dyp"
        ("and",_:(string * (int * int * int * string)))
# 1334               "dypgen_parser_temp.ml"
 as _1);Obj_dypgen__plus___lident ( (
(_:'dypgen__Obj_lident list)
# 1337               "dypgen_parser_temp.ml"
 as _2)); _3;Obj_LIDENT  (
# 186 "dypgen_parser.dyp"
                                       ("parse",_:(string * (int * int * int * string)))
# 1341               "dypgen_parser_temp.ml"
 as _4);Obj_aux_lexer_def ( (
(_:'dypgen__Obj_aux_lexer_def)
# 1344               "dypgen_parser_temp.ml"
 as _5))] -> Obj_aux_lexer_and 
# 186 "dypgen_parser.dyp"
(
  ( (_2,_5) ):'dypgen__Obj_aux_lexer_and)
# 1349               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lident",[Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 189 "dypgen_parser.dyp"
              (name,_:(string * (int * int * int * string)))
# 1358               "dypgen_parser_temp.ml"
 as _1)] -> Obj_lident 
# 188 "dypgen_parser.dyp"
(
                       ( name ):'dypgen__Obj_lident)
# 1363               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("aux_lexer_def",[Dyp.Non_ter ("dypgen__option_BAR",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_6",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_8",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__option_BAR ( (
(_:'dypgen__Obj_dypgen__option_BAR)
# 1371               "dypgen_parser_temp.ml"
 as _1));Obj_dypgen__nested_nt_6 ( (
(_:'dypgen__Obj_dypgen__nested_nt_6)
# 1374               "dypgen_parser_temp.ml"
 as _2));Obj_dypgen__nested_nt_8 ( (
(_:'dypgen__Obj_dypgen__nested_nt_8)
# 1377               "dypgen_parser_temp.ml"
 as _3))] -> Obj_aux_lexer_def 
# 193 "dypgen_parser.dyp"
(
  ( _2::_3 ):'dypgen__Obj_aux_lexer_def)
# 1382               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_BAR",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_BAR 
(
(None):'dypgen__Obj_dypgen__option_BAR)
# 1391               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_BAR",[Dyp.Ter "BAR"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_dypgen__option_BAR 
(
(None):'dypgen__Obj_dypgen__option_BAR)
# 1400               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_6",[Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1408               "dypgen_parser_temp.ml"
 as _1));Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 1411               "dypgen_parser_temp.ml"
 as _2)] -> Obj_dypgen__nested_nt_6 
# 191 "dypgen_parser.dyp"
(
                           (_1,_2):'dypgen__Obj_dypgen__nested_nt_6)
# 1416               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_7",[Dyp.Ter "BAR";Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1424               "dypgen_parser_temp.ml"
 as _2));Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 1427               "dypgen_parser_temp.ml"
 as _3)] -> Obj_dypgen__nested_nt_7 
# 192 "dypgen_parser.dyp"
(
                         (_2,_3):'dypgen__Obj_dypgen__nested_nt_7)
# 1432               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_8",[Dyp.Non_ter ("dypgen__nested_nt_9",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_9 ( (
(_:'dypgen__Obj_dypgen__nested_nt_9)
# 1440               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__nested_nt_8 
(
(List.rev _1):'dypgen__Obj_dypgen__nested_nt_8)
# 1444               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_9",[Dyp.Non_ter ("dypgen__nested_nt_9",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_7",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_9 ( (
(_:'dypgen__Obj_dypgen__nested_nt_9)
# 1452               "dypgen_parser_temp.ml"
 as _1));Obj_dypgen__nested_nt_7 ( (
(_:'dypgen__Obj_dypgen__nested_nt_7)
# 1455               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_9 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_9)
# 1459               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_9",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__nested_nt_9 
(
([]):'dypgen__Obj_dypgen__nested_nt_9)
# 1468               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "CHAR"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_CHAR  (
(_:(char))
# 1476               "dypgen_parser_temp.ml"
 as _1)] -> Obj_regexp 
# 196 "dypgen_parser.dyp"
(
         ( RE_Char _1 ):'dypgen__Obj_regexp)
# 1481               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "LBRACK";Dyp.Non_ter ("dypgen__plus___char_elt",Dyp.No_priority );Dyp.Ter "RBRACK"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_dypgen__plus___char_elt ( (
(_:'dypgen__Obj_char_elt list)
# 1489               "dypgen_parser_temp.ml"
 as _2)); _3] -> Obj_regexp 
# 197 "dypgen_parser.dyp"
(
                             ( RE_Char_set _2 ):'dypgen__Obj_regexp)
# 1494               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "LBRACK";Dyp.Ter "CARET";Dyp.Non_ter ("dypgen__plus___char_elt",Dyp.No_priority );Dyp.Ter "RBRACK"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1; _2;Obj_dypgen__plus___char_elt ( (
(_:'dypgen__Obj_char_elt list)
# 1502               "dypgen_parser_temp.ml"
 as _3)); _4] -> Obj_regexp 
# 198 "dypgen_parser.dyp"
(
                                   ( RE_Char_set_exclu _3 ):'dypgen__Obj_regexp)
# 1507               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "STRING"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_STRING  (
(_:(string))
# 1515               "dypgen_parser_temp.ml"
 as _1)] -> Obj_regexp 
# 199 "dypgen_parser.dyp"
(
           ( RE_String _1 ):'dypgen__Obj_regexp)
# 1520               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Ter "BAR";Dyp.Non_ter ("regexp",Dyp.Less_priority "pAlt")],"pAlt",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1528               "dypgen_parser_temp.ml"
 as _1)); _2;Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1531               "dypgen_parser_temp.ml"
 as _3))] -> Obj_regexp 
# 200 "dypgen_parser.dyp"
(
                             ( RE_Alt [_1;_3] ):'dypgen__Obj_regexp)
# 1536               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Non_ter ("regexp",Dyp.Lesseq_priority "pSeq");Dyp.Non_ter ("regexp",Dyp.Less_priority "pSeq")],"pSeq",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1544               "dypgen_parser_temp.ml"
 as _1));Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1547               "dypgen_parser_temp.ml"
 as _2))] -> Obj_regexp 
# 201 "dypgen_parser.dyp"
(
                                 ( RE_Seq [_1;_2] ):'dypgen__Obj_regexp)
# 1552               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Non_ter ("regexp",Dyp.Eq_priority "p1");Dyp.Ter "STAR"],"p2",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1560               "dypgen_parser_temp.ml"
 as _1)); _2] -> Obj_regexp 
# 202 "dypgen_parser.dyp"
(
                     ( RE_Star _1 ):'dypgen__Obj_regexp)
# 1565               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Non_ter ("regexp",Dyp.Eq_priority "p1");Dyp.Ter "PLUS"],"p2",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1573               "dypgen_parser_temp.ml"
 as _1)); _2] -> Obj_regexp 
# 203 "dypgen_parser.dyp"
(
                     ( RE_Plus _1 ):'dypgen__Obj_regexp)
# 1578               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Non_ter ("regexp",Dyp.Eq_priority "p1");Dyp.Ter "QUESTION"],"p2",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1586               "dypgen_parser_temp.ml"
 as _1)); _2] -> Obj_regexp 
# 204 "dypgen_parser.dyp"
(
                         ( RE_Option _1 ):'dypgen__Obj_regexp)
# 1591               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "LIDENT"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 206 "dypgen_parser.dyp"
          (name,_:(string * (int * int * int * string)))
# 1600               "dypgen_parser_temp.ml"
 as _1)] -> Obj_regexp 
# 205 "dypgen_parser.dyp"
(
                   (
      if name = "eof" then RE_Eof_char
      else if name = "_" then
        RE_Char_set [((Char.chr 0),(Char.chr 255))]
      else if String_set.mem name dyp.global_data.gd_regexp_decl
      then RE_Name name
      else raise Giveup ):'dypgen__Obj_regexp)
# 1611               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp",[Dyp.Ter "LPAREN";Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Ter "RPAREN"],"p1",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1619               "dypgen_parser_temp.ml"
 as _2)); _3] -> Obj_regexp 
# 212 "dypgen_parser.dyp"
(
                         ( _2 ):'dypgen__Obj_regexp)
# 1624               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__plus___char_elt",[Dyp.Non_ter ("dypgen__nested_nt_10",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_10 ( (
(_:'dypgen__Obj_dypgen__nested_nt_10)
# 1632               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__plus___char_elt 
(
(List.rev _1):'dypgen__Obj_char_elt list)
# 1636               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_10",[Dyp.Non_ter ("dypgen__nested_nt_10",Dyp.No_priority );Dyp.Non_ter ("char_elt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_dypgen__nested_nt_10 ( (
(_:'dypgen__Obj_dypgen__nested_nt_10)
# 1644               "dypgen_parser_temp.ml"
 as _1));Obj_char_elt ( (
(_:'dypgen__Obj_char_elt)
# 1647               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_10 
(
(_2::_1):'dypgen__Obj_dypgen__nested_nt_10)
# 1651               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_10",[Dyp.Non_ter ("char_elt",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_char_elt ( (
(_:'dypgen__Obj_char_elt)
# 1659               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__nested_nt_10 
(
([_1]):'dypgen__Obj_dypgen__nested_nt_10)
# 1663               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("regexp_ter",[Dyp.Non_ter ("regexp",Dyp.Less_priority "pSeq")],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1671               "dypgen_parser_temp.ml"
 as _1))] -> Obj_regexp_ter 
# 214 "dypgen_parser.dyp"
(
                          ( _1 ):'dypgen__Obj_regexp_ter)
# 1676               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("char_elt",[Dyp.Ter "CHAR"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_CHAR  (
(_:(char))
# 1684               "dypgen_parser_temp.ml"
 as _1)] -> Obj_char_elt 
# 217 "dypgen_parser.dyp"
(
         ( (_1,_1) ):'dypgen__Obj_char_elt)
# 1689               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("char_elt",[Dyp.Ter "CHAR";Dyp.Ter "DASH";Dyp.Ter "CHAR"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_CHAR  (
(_:(char))
# 1697               "dypgen_parser_temp.ml"
 as _1); _2;Obj_CHAR  (
(_:(char))
# 1700               "dypgen_parser_temp.ml"
 as _3)] -> Obj_char_elt 
# 218 "dypgen_parser.dyp"
(
                   ( (_1,_3) ):'dypgen__Obj_char_elt)
# 1705               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_trailer",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_trailer 
# 221 "dypgen_parser.dyp"
(
                              ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_trailer)
# 1715               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_trailer",[Dyp.Ter "PERCENTPERCENT";Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 1723               "dypgen_parser_temp.ml"
 as _2)] -> Obj_optional_trailer 
# 222 "dypgen_parser.dyp"
(
                              ( (fst _2,fst (snd _2)) ):'dypgen__Obj_optional_trailer)
# 1728               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_parser_param_info 
# 225 "dypgen_parser.dyp"
(
     ( empty_ppi ):'dypgen__Obj_parser_param_info)
# 1738               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_START";Dyp.Non_ter ("optional_type",Dyp.No_priority );Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_optional_type ( (
(_:'dypgen__Obj_optional_type)
# 1746               "dypgen_parser_temp.ml"
 as _2));Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 1749               "dypgen_parser_temp.ml"
 as _3)] -> Obj_parser_param_info 
# 227 "dypgen_parser.dyp"
(
      ( {empty_ppi with start = [((fst _3),_2)]} ):'dypgen__Obj_parser_param_info)
# 1754               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Non_ter ("token_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_token_list ( (
(_:'dypgen__Obj_token_list)
# 1762               "dypgen_parser_temp.ml"
 as _1))] -> Obj_parser_param_info 
# 228 "dypgen_parser.dyp"
(
                                 ( {empty_ppi with token_list = _1} ):'dypgen__Obj_parser_param_info)
# 1767               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Non_ter ("relation",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_relation ( (
(_:'dypgen__Obj_relation)
# 1775               "dypgen_parser_temp.ml"
 as _1))] -> Obj_parser_param_info 
# 229 "dypgen_parser.dyp"
(
                                 ( {empty_ppi with relation = _1} ):'dypgen__Obj_parser_param_info)
# 1780               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_CONSTRUCTOR";Dyp.Ter "UIDENT";Dyp.Ter "KWD_FOR";Dyp.Non_ter ("ident_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 1788               "dypgen_parser_temp.ml"
 as _2); _3;Obj_ident_list ( (
(_:'dypgen__Obj_ident_list)
# 1791               "dypgen_parser_temp.ml"
 as _4))] -> Obj_parser_param_info 
# 231 "dypgen_parser.dyp"
(
      ( { empty_ppi with cons = [(fst _2,List.rev _4)] }  ):'dypgen__Obj_parser_param_info)
# 1796               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_CONSTRUCTOR";Dyp.Non_ter ("uident_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_uident_list ( (
(_:'dypgen__Obj_uident_list)
# 1804               "dypgen_parser_temp.ml"
 as _2))] -> Obj_parser_param_info 
# 233 "dypgen_parser.dyp"
(
      ( { empty_ppi with additional_cons = List.rev _2 }  ):'dypgen__Obj_parser_param_info)
# 1809               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_MERGE";Dyp.Ter "LIDENT";Dyp.Non_ter ("uident_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 1817               "dypgen_parser_temp.ml"
 as _2);Obj_uident_list ( (
(_:'dypgen__Obj_uident_list)
# 1820               "dypgen_parser_temp.ml"
 as _3))] -> Obj_parser_param_info 
# 235 "dypgen_parser.dyp"
(
      ( {empty_ppi with generic_merge = [((fst _2),List.rev _3)]} ):'dypgen__Obj_parser_param_info)
# 1825               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_TYPE";Dyp.Ter "OCAML_TYPE";Dyp.Non_ter ("lident_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_TYPE  (
(_:(string))
# 1833               "dypgen_parser_temp.ml"
 as _2);Obj_lident_list ( (
(_:'dypgen__Obj_lident_list)
# 1836               "dypgen_parser_temp.ml"
 as _3))] -> Obj_parser_param_info 
# 237 "dypgen_parser.dyp"
(
      ( {empty_ppi with nt_type = [_2,List.rev _3]} ):'dypgen__Obj_parser_param_info)
# 1841               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_LAYOUT";Dyp.Non_ter ("regexp",Dyp.No_priority );Dyp.Non_ter ("optional_code",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_regexp ( (
(_:'dypgen__Obj_regexp)
# 1849               "dypgen_parser_temp.ml"
 as _2));Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 1852               "dypgen_parser_temp.ml"
 as _3))] -> Obj_parser_param_info 
# 239 "dypgen_parser.dyp"
(
      ( {empty_ppi with layout = [_2,_3]} ):'dypgen__Obj_parser_param_info)
# 1857               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_info",[Dyp.Ter "KWD_NON_TERMINAL";Dyp.Non_ter ("lident_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_lident_list ( (
(_:'dypgen__Obj_lident_list)
# 1865               "dypgen_parser_temp.ml"
 as _2))] -> Obj_parser_param_info 
# 241 "dypgen_parser.dyp"
(
      ( {empty_ppi with single_nt = List.rev _2} ):'dypgen__Obj_parser_param_info)
# 1870               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_infos",[Dyp.Non_ter ("parser_param_info",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_parser_param_info ( (
(_:'dypgen__Obj_parser_param_info)
# 1878               "dypgen_parser_temp.ml"
 as _1))] -> Obj_parser_param_infos 
# 244 "dypgen_parser.dyp"
(
                      ( _1 ):'dypgen__Obj_parser_param_infos)
# 1883               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("parser_param_infos",[Dyp.Non_ter ("parser_param_infos",Dyp.No_priority );Dyp.Non_ter ("parser_param_info",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_parser_param_infos ( (
(_:'dypgen__Obj_parser_param_infos)
# 1891               "dypgen_parser_temp.ml"
 as _1));Obj_parser_param_info ( (
(_:'dypgen__Obj_parser_param_info)
# 1894               "dypgen_parser_temp.ml"
 as _2))] ->  let res = 
# 246 "dypgen_parser.dyp"
(
     (
      { token_list = (_2.token_list@_1.token_list);
        relation = _2.relation@_1.relation;
        start = _2.start@_1.start;
        generic_merge = _2.generic_merge@_1.generic_merge;
        cons = _2.cons@_1.cons;
        additional_cons = _2.additional_cons@_1.additional_cons;
        nt_type = _2.nt_type@_1.nt_type;
        single_nt = _2.single_nt@_1.single_nt;
        layout = _2.layout@_1.layout },
        [Dyp.Dont_shift] ):'dypgen__Obj_parser_param_infos * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 1909               "dypgen_parser_temp.ml"
  in Obj_parser_param_infos(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lident_list",[Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 1918               "dypgen_parser_temp.ml"
 as _1)] -> Obj_lident_list 
# 259 "dypgen_parser.dyp"
(
                       ( [(fst _1)] ):'dypgen__Obj_lident_list)
# 1923               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("lident_list",[Dyp.Non_ter ("lident_list",Dyp.No_priority );Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_lident_list ( (
(_:'dypgen__Obj_lident_list)
# 1931               "dypgen_parser_temp.ml"
 as _1));Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 1934               "dypgen_parser_temp.ml"
 as _2)] -> Obj_lident_list 
# 260 "dypgen_parser.dyp"
(
                       ( (fst _2)::_1 ):'dypgen__Obj_lident_list)
# 1939               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("uident_list",[Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 1947               "dypgen_parser_temp.ml"
 as _1)] -> Obj_uident_list 
# 263 "dypgen_parser.dyp"
(
                       ( [(fst _1)] ):'dypgen__Obj_uident_list)
# 1952               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("uident_list",[Dyp.Non_ter ("uident_list",Dyp.No_priority );Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_uident_list ( (
(_:'dypgen__Obj_uident_list)
# 1960               "dypgen_parser_temp.ml"
 as _1));Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 1963               "dypgen_parser_temp.ml"
 as _2)] -> Obj_uident_list 
# 264 "dypgen_parser.dyp"
(
                       ( (fst _2)::_1 ):'dypgen__Obj_uident_list)
# 1968               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ident_list",[Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 1976               "dypgen_parser_temp.ml"
 as _1)] -> Obj_ident_list 
# 267 "dypgen_parser.dyp"
(
                       ( [(fst _1)] ):'dypgen__Obj_ident_list)
# 1981               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ident_list",[Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 1989               "dypgen_parser_temp.ml"
 as _1)] -> Obj_ident_list 
# 268 "dypgen_parser.dyp"
(
                       ( [(fst _1)] ):'dypgen__Obj_ident_list)
# 1994               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ident_list",[Dyp.Non_ter ("ident_list",Dyp.No_priority );Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ident_list ( (
(_:'dypgen__Obj_ident_list)
# 2002               "dypgen_parser_temp.ml"
 as _1));Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2005               "dypgen_parser_temp.ml"
 as _2)] -> Obj_ident_list 
# 269 "dypgen_parser.dyp"
(
                      ( (fst _2)::_1 ):'dypgen__Obj_ident_list)
# 2010               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("ident_list",[Dyp.Non_ter ("ident_list",Dyp.No_priority );Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_ident_list ( (
(_:'dypgen__Obj_ident_list)
# 2018               "dypgen_parser_temp.ml"
 as _1));Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2021               "dypgen_parser_temp.ml"
 as _2)] -> Obj_ident_list 
# 270 "dypgen_parser.dyp"
(
                      ( (fst _2)::_1 ):'dypgen__Obj_ident_list)
# 2026               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_code",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_code 
# 273 "dypgen_parser.dyp"
(
               ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_code)
# 2036               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_code",[Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2044               "dypgen_parser_temp.ml"
 as _1)] -> Obj_optional_code 
# 274 "dypgen_parser.dyp"
(
               ( (fst _1,fst (snd _1)) ):'dypgen__Obj_optional_code)
# 2049               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_type",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_type 
# 277 "dypgen_parser.dyp"
(
               ( "No type" ):'dypgen__Obj_optional_type)
# 2059               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_type",[Dyp.Ter "OCAML_TYPE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_OCAML_TYPE  (
(_:(string))
# 2067               "dypgen_parser_temp.ml"
 as _1)] -> Obj_optional_type 
# 278 "dypgen_parser.dyp"
(
               ( _1 ):'dypgen__Obj_optional_type)
# 2072               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mli",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_mli 
# 281 "dypgen_parser.dyp"
(
                       ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_mli)
# 2082               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mli",[Dyp.Ter "KWD_MLI";Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2090               "dypgen_parser_temp.ml"
 as _2)] -> Obj_optional_mli 
# 282 "dypgen_parser.dyp"
(
                       ( (fst _2,fst (snd _2)) ):'dypgen__Obj_optional_mli)
# 2095               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mlimid",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_mlimid 
# 285 "dypgen_parser.dyp"
(
                       ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_mlimid)
# 2105               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mlimid",[Dyp.Ter "KWD_MLIMID";Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2113               "dypgen_parser_temp.ml"
 as _2)] -> Obj_optional_mlimid 
# 286 "dypgen_parser.dyp"
(
                          ( (fst _2,fst (snd _2)) ):'dypgen__Obj_optional_mlimid)
# 2118               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mlitop",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_mlitop 
# 289 "dypgen_parser.dyp"
(
                       ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_mlitop)
# 2128               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mlitop",[Dyp.Ter "KWD_MLITOP";Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2136               "dypgen_parser_temp.ml"
 as _2)] -> Obj_optional_mlitop 
# 290 "dypgen_parser.dyp"
(
                          ( (fst _2,fst (snd _2)) ):'dypgen__Obj_optional_mlitop)
# 2141               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mltop",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_optional_mltop 
# 293 "dypgen_parser.dyp"
(
                       ( ("",Lexing.dummy_pos) ):'dypgen__Obj_optional_mltop)
# 2151               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("optional_mltop",[Dyp.Ter "KWD_MLTOP";Dyp.Ter "OCAML_CODE"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2159               "dypgen_parser_temp.ml"
 as _2)] -> Obj_optional_mltop 
# 294 "dypgen_parser.dyp"
(
                         ( (fst _2,fst (snd _2)) ):'dypgen__Obj_optional_mltop)
# 2164               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("token_list",[Dyp.Ter "KWD_TOKEN";Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2172               "dypgen_parser_temp.ml"
 as _2)] -> Obj_token_list 
# 298 "dypgen_parser.dyp"
(
                                 ( [((fst _2),"No_type")] ):'dypgen__Obj_token_list)
# 2177               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("token_list",[Dyp.Ter "KWD_TOKEN";Dyp.Ter "OCAML_TYPE";Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_OCAML_TYPE  (
(_:(string))
# 2185               "dypgen_parser_temp.ml"
 as _2);Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2188               "dypgen_parser_temp.ml"
 as _3)] -> Obj_token_list 
# 299 "dypgen_parser.dyp"
(
                                 ( [((fst _3),_2)] ):'dypgen__Obj_token_list)
# 2193               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("token_list",[Dyp.Non_ter ("token_list",Dyp.No_priority );Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_token_list ( (
(_:'dypgen__Obj_token_list)
# 2201               "dypgen_parser_temp.ml"
 as _1));Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2204               "dypgen_parser_temp.ml"
 as _2)] -> Obj_token_list 
# 300 "dypgen_parser.dyp"
(
                                 ( ((fst _2),"No_type")::_1 ):'dypgen__Obj_token_list)
# 2209               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("token_list",[Dyp.Non_ter ("token_list",Dyp.No_priority );Dyp.Ter "OCAML_TYPE";Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_token_list ( (
(_:'dypgen__Obj_token_list)
# 2217               "dypgen_parser_temp.ml"
 as _1));Obj_OCAML_TYPE  (
(_:(string))
# 2220               "dypgen_parser_temp.ml"
 as _2);Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2223               "dypgen_parser_temp.ml"
 as _3)] -> Obj_token_list 
# 301 "dypgen_parser.dyp"
(
                                 ( ((fst _3),_2)::_1 ):'dypgen__Obj_token_list)
# 2228               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("relation",[Dyp.Ter "KWD_RELATION"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_relation 
# 304 "dypgen_parser.dyp"
(
                 ( [] ):'dypgen__Obj_relation)
# 2238               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("relation",[Dyp.Non_ter ("relation",Dyp.No_priority );Dyp.Non_ter ("relation_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_relation ( (
(_:'dypgen__Obj_relation)
# 2246               "dypgen_parser_temp.ml"
 as _1));Obj_relation_list ( (
(_:'dypgen__Obj_relation_list)
# 2249               "dypgen_parser_temp.ml"
 as _2))] -> Obj_relation 
# 305 "dypgen_parser.dyp"
(
                           ( (Rel_list (List.rev _2))::_1 ):'dypgen__Obj_relation)
# 2254               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("relation",[Dyp.Non_ter ("relation",Dyp.No_priority );Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_relation ( (
(_:'dypgen__Obj_relation)
# 2262               "dypgen_parser_temp.ml"
 as _1));Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2265               "dypgen_parser_temp.ml"
 as _2)] -> Obj_relation 
# 306 "dypgen_parser.dyp"
(
                    ( (Rel_single (fst _2))::_1 ):'dypgen__Obj_relation)
# 2270               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("relation_list",[Dyp.Ter "LIDENT";Dyp.Ter "LESS";Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2278               "dypgen_parser_temp.ml"
 as _1); _2;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2281               "dypgen_parser_temp.ml"
 as _3)] -> Obj_relation_list 
# 309 "dypgen_parser.dyp"
(
                       ( [(fst _3);(fst _1)] ):'dypgen__Obj_relation_list)
# 2286               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("relation_list",[Dyp.Non_ter ("relation_list",Dyp.No_priority );Dyp.Ter "LESS";Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_relation_list ( (
(_:'dypgen__Obj_relation_list)
# 2294               "dypgen_parser_temp.ml"
 as _1)); _2;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2297               "dypgen_parser_temp.ml"
 as _3)] -> Obj_relation_list 
# 310 "dypgen_parser.dyp"
(
                              ( (fst _3)::_1 ):'dypgen__Obj_relation_list)
# 2302               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("grammar",[Dyp.Non_ter ("grammar",Dyp.No_priority );Dyp.Non_ter ("entry_def",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_grammar ( (
(_:'dypgen__Obj_grammar)
# 2310               "dypgen_parser_temp.ml"
 as _1));Obj_entry_def ( (
(_:'dypgen__Obj_entry_def)
# 2313               "dypgen_parser_temp.ml"
 as _2))] -> Obj_grammar 
# 313 "dypgen_parser.dyp"
(
                      ( _2@_1 ):'dypgen__Obj_grammar)
# 2318               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("grammar",[Dyp.Non_ter ("entry_def",Dyp.No_priority );Dyp.Non_ter ("dypgen__option_SEMI",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_entry_def ( (
(_:'dypgen__Obj_entry_def)
# 2326               "dypgen_parser_temp.ml"
 as _1));Obj_dypgen__option_SEMI ( (
(_:'dypgen__Obj_dypgen__option_SEMI)
# 2329               "dypgen_parser_temp.ml"
 as _2))] -> Obj_grammar 
# 314 "dypgen_parser.dyp"
(
                     ( _1 ):'dypgen__Obj_grammar)
# 2334               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_SEMI",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_dypgen__option_SEMI 
(
(None):'dypgen__Obj_dypgen__option_SEMI)
# 2343               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__option_SEMI",[Dyp.Ter "SEMI"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_dypgen__option_SEMI 
(
(None):'dypgen__Obj_dypgen__option_SEMI)
# 2352               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("entry_def",[Dyp.Ter "LIDENT";Dyp.Non_ter ("opt_pattern",Dyp.No_priority );Dyp.Ter "COLON";Dyp.Non_ter ("dypgen__option_BAR",Dyp.No_priority );Dyp.Non_ter ("rhs_list",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 318 "dypgen_parser.dyp"
        (lhs,_:(string * (int * int * int * string)))
# 2361               "dypgen_parser_temp.ml"
 as _1);Obj_opt_pattern ( (
# 318 "dypgen_parser.dyp"
                            (pat_code, pat_pos:'dypgen__Obj_opt_pattern)
# 2365               "dypgen_parser_temp.ml"
 as _2)); _3;Obj_dypgen__option_BAR ( (
(_:'dypgen__Obj_dypgen__option_BAR)
# 2368               "dypgen_parser_temp.ml"
 as _4));Obj_rhs_list ( (
(_:'dypgen__Obj_rhs_list)
# 2371               "dypgen_parser_temp.ml"
 as _5))] -> Obj_entry_def 
# 318 "dypgen_parser.dyp"
(
  ( let f (rl1,rl2) (prio,symbl,code,rule_options,add_rules) =
      ((lhs, [pat_code, (Pat_inh lhs), pat_pos]), prio, symbl, code, rule_options)
       ::rl1,add_rules@rl2 in
    let rl1, rl2 = List.fold_left f ([], []) (List.rev _5) in
    rl2@rl1 ):'dypgen__Obj_entry_def)
# 2380               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rhs_list",[Dyp.Non_ter ("rhs",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_rhs ( (
(_:'dypgen__Obj_rhs)
# 2388               "dypgen_parser_temp.ml"
 as _1))] -> Obj_rhs_list 
# 325 "dypgen_parser.dyp"
(
        ( [_1] ):'dypgen__Obj_rhs_list)
# 2393               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rhs_list",[Dyp.Non_ter ("rhs_list",Dyp.No_priority );Dyp.Ter "BAR";Dyp.Non_ter ("rhs",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_rhs_list ( (
(_:'dypgen__Obj_rhs_list)
# 2401               "dypgen_parser_temp.ml"
 as _1)); _2;Obj_rhs ( (
(_:'dypgen__Obj_rhs)
# 2404               "dypgen_parser_temp.ml"
 as _3))] -> Obj_rhs_list 
# 326 "dypgen_parser.dyp"
(
                     ( _3::_1 ):'dypgen__Obj_rhs_list)
# 2409               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("rhs",[Dyp.Non_ter ("opt_bang",Dyp.No_priority );Dyp.Non_ter ("symbol_list",Dyp.No_priority );Dyp.Non_ter ("opt_dash",Dyp.No_priority );Dyp.Non_ter ("action_prio",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_opt_bang ( (
(_:'dypgen__Obj_opt_bang)
# 2417               "dypgen_parser_temp.ml"
 as _1));Obj_symbol_list ( (
(_:'dypgen__Obj_symbol_list)
# 2420               "dypgen_parser_temp.ml"
 as _2));Obj_opt_dash ( (
(_:'dypgen__Obj_opt_dash)
# 2423               "dypgen_parser_temp.ml"
 as _3));Obj_action_prio ( (
# 329 "dypgen_parser.dyp"
                                              (action, prio, b:'dypgen__Obj_action_prio)
# 2427               "dypgen_parser_temp.ml"
 as _4))] -> Obj_rhs 
# 329 "dypgen_parser.dyp"
(
    ( let symbl, additional_rules = _2 in
      let action =
        if b then
          if symbl=[] then raise Giveup
          else ("_"^(string_of_int (List.length symbl))),
            (Lexing.dummy_pos, false)
        else action
      in
      (prio, (List.rev symbl), action, (_1,_3), additional_rules) ):'dypgen__Obj_rhs)
# 2440               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_bang",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_opt_bang 
# 340 "dypgen_parser.dyp"
(
    ( true ):'dypgen__Obj_opt_bang)
# 2450               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_bang",[Dyp.Ter "BANG"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_opt_bang 
# 341 "dypgen_parser.dyp"
(
         ( false ):'dypgen__Obj_opt_bang)
# 2460               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_dash",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_opt_dash 
# 344 "dypgen_parser.dyp"
(
    ( true ):'dypgen__Obj_opt_dash)
# 2470               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_dash",[Dyp.Ter "DASH"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1] -> Obj_opt_dash 
# 345 "dypgen_parser.dyp"
(
         ( false ):'dypgen__Obj_opt_dash)
# 2480               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("action_prio",[Dyp.Ter "OCAML_CODE";Dyp.Non_ter ("priority",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2488               "dypgen_parser_temp.ml"
 as _1);Obj_priority ( (
(_:'dypgen__Obj_priority)
# 2491               "dypgen_parser_temp.ml"
 as _2))] -> Obj_action_prio 
# 348 "dypgen_parser.dyp"
(
                        ( (_1,_2,false) ):'dypgen__Obj_action_prio)
# 2496               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("action_prio",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_action_prio 
# 349 "dypgen_parser.dyp"
(
       ( ("", (Lexing.dummy_pos,false)),
       ("default_priority", (-1,-1,-1,"")), true ):'dypgen__Obj_action_prio)
# 2507               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol_list",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_symbol_list 
# 353 "dypgen_parser.dyp"
(
          ( ([],[]) ):'dypgen__Obj_symbol_list)
# 2517               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol_list",[Dyp.Non_ter ("symbol_list",Dyp.No_priority );Dyp.Non_ter ("dypgen__nested_nt_11",Dyp.No_priority );Dyp.Non_ter ("opt_pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symbol_list ( (
# 355 "dypgen_parser.dyp"
                (l, add_rule:'dypgen__Obj_symbol_list)
# 2526               "dypgen_parser_temp.ml"
 as _1));Obj_dypgen__nested_nt_11 ( (
# 356 "dypgen_parser.dyp"
                                                                           (symb:'dypgen__Obj_dypgen__nested_nt_11)
# 2530               "dypgen_parser_temp.ml"
 as _2));Obj_opt_pattern ( (
# 357 "dypgen_parser.dyp"
                (pat_code, pat_pos:'dypgen__Obj_opt_pattern)
# 2534               "dypgen_parser_temp.ml"
 as _3))] -> Obj_symbol_list 
# 357 "dypgen_parser.dyp"
(
      ( use_dyplex := true;
      ((symb, (pat_code, (Pat_syn "#Lexeme_matched"), pat_pos))::l), add_rule ):'dypgen__Obj_symbol_list)
# 2540               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol_list",[Dyp.Non_ter ("symbol_list",Dyp.No_priority );Dyp.Ter "THREEDOTS";Dyp.Ter "OCAML_CODE";Dyp.Non_ter ("opt_pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symbol_list ( (
# 361 "dypgen_parser.dyp"
                (l, add_rule:'dypgen__Obj_symbol_list)
# 2549               "dypgen_parser_temp.ml"
 as _1)); _2;Obj_OCAML_CODE  (
(_:(string * (Lexing.position * bool)))
# 2552               "dypgen_parser_temp.ml"
 as _3);Obj_opt_pattern ( (
# 362 "dypgen_parser.dyp"
                                     (pat_code, pat_pos:'dypgen__Obj_opt_pattern)
# 2556               "dypgen_parser_temp.ml"
 as _4))] -> Obj_symbol_list 
# 362 "dypgen_parser.dyp"
(
      (
      (*let count = dyp.global_data.gd_count in
      let nt = "dypgen__nested_nt_"^(string_of_int count) in
      let symb =
        Symb_non_terminal_NL ((nt, dummy_p),
          ("No_priority", dummy_p), Pr_eq, dummy_code) in
      let rule =
        (nt, [dummy_pat_inh]), ("default_priority", dummy_p), [],
        ("_2::_1", (Lexing.dummy_pos, false)), (true, true)
      in*)
      let symb = Symb_early_action _3 in
      ((symb, (pat_code, (Pat_syn ""), pat_pos))::l), add_rule
      ):'dypgen__Obj_symbol_list)
# 2573               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol_list",[Dyp.Non_ter ("symbol_list",Dyp.No_priority );Dyp.Non_ter ("symbol",Dyp.No_priority );Dyp.Non_ter ("opt_pattern",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symbol_list ( (
# 376 "dypgen_parser.dyp"
                (l, rules1:'dypgen__Obj_symbol_list)
# 2582               "dypgen_parser_temp.ml"
 as _1));Obj_symbol ( (
# 376 "dypgen_parser.dyp"
                                   (symb, rules2:'dypgen__Obj_symbol)
# 2586               "dypgen_parser_temp.ml"
 as _2));Obj_opt_pattern ( (
# 376 "dypgen_parser.dyp"
                                                              (pat_code, pat_pos:'dypgen__Obj_opt_pattern)
# 2590               "dypgen_parser_temp.ml"
 as _3))] -> Obj_symbol_list 
# 376 "dypgen_parser.dyp"
(
      ( let pat_typ = match symb with
          | Symb_terminal (s,_)
          | Symb_terminal_NL (s,_)
          | Symb_non_terminal ((s,_),_,_,_)
          | Symb_non_terminal_NL ((s,_),_,_,_) -> s
          | _ -> assert false
        in
        let pat_code =
          try
            if String.sub pat_typ 0 15 = "dypgen__option_" then
              if pat_code = "_" then "_"
              else "(Some ("^pat_code^")|None)"
            else pat_code
          with Invalid_argument _ -> pat_code
        in
        ( ((symb, (pat_code, (Pat_syn pat_typ), pat_pos))::l), rules2@rules1 ) ):'dypgen__Obj_symbol_list)
# 2610               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_11",[Dyp.Non_ter ("regexp_ter",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_regexp_ter ( (
(_:'dypgen__Obj_regexp_ter)
# 2618               "dypgen_parser_temp.ml"
 as _1))] -> Obj_dypgen__nested_nt_11 
# 355 "dypgen_parser.dyp"
(
                ( Symb_regexp _1 ):'dypgen__Obj_dypgen__nested_nt_11)
# 2623               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("dypgen__nested_nt_11",[Dyp.Ter "DASH";Dyp.Non_ter ("regexp_ter",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_regexp_ter ( (
(_:'dypgen__Obj_regexp_ter)
# 2631               "dypgen_parser_temp.ml"
 as _2))] -> Obj_dypgen__nested_nt_11 
# 355 "dypgen_parser.dyp"
(
                                                     ( Symb_regexp_NL _2 ):'dypgen__Obj_dypgen__nested_nt_11)
# 2636               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol",[Dyp.Non_ter ("symb",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symb ( (
(_:'dypgen__Obj_symb)
# 2644               "dypgen_parser_temp.ml"
 as _1))] -> Obj_symbol 
# 394 "dypgen_parser.dyp"
(
         ( _1 ):'dypgen__Obj_symbol)
# 2649               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol",[Dyp.Non_ter ("symb",Dyp.No_priority );Dyp.Ter "STAR"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symb ( (
(_:'dypgen__Obj_symb)
# 2657               "dypgen_parser_temp.ml"
 as _1)); _2] ->  let res = 
# 395 "dypgen_parser.dyp"
(
              ( symbol_list dyp "star" _1 ):'dypgen__Obj_symbol * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 2662               "dypgen_parser_temp.ml"
  in Obj_symbol(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol",[Dyp.Non_ter ("symb",Dyp.No_priority );Dyp.Ter "PLUS"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symb ( (
(_:'dypgen__Obj_symb)
# 2671               "dypgen_parser_temp.ml"
 as _1)); _2] ->  let res = 
# 396 "dypgen_parser.dyp"
(
              ( symbol_list dyp "plus" _1 ):'dypgen__Obj_symbol * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 2676               "dypgen_parser_temp.ml"
  in Obj_symbol(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symbol",[Dyp.Non_ter ("symb",Dyp.No_priority );Dyp.Ter "QUESTION"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_symb ( (
# 398 "dypgen_parser.dyp"
        (symb,add_rules:'dypgen__Obj_symb)
# 2686               "dypgen_parser_temp.ml"
 as _1)); _2] ->  let res = 
# 398 "dypgen_parser.dyp"
(
    ( let {gd_count=count; gd_symbs=symbs} = dyp.global_data in
      let pat_typ = match symb with
        | Symb_non_terminal ((s,_),_,_,_)
        | Symb_non_terminal_NL ((s,_),_,_,_) -> s
        | Symb_terminal s | Symb_terminal_NL s -> fst s
        | _ -> assert false
      in
      let qsymb = "dypgen__option_"^pat_typ in
      if String_set.mem qsymb symbs then
        ((Symb_non_terminal
         ((qsymb,dummy_p),("No_priority",dummy_p),Pr_eq,dummy_code)),
        add_rules), []
      else
      let is_nested =
        try
          let sub = String.sub pat_typ 0 18 in
          if sub = "dypgen__nested_nt_" then true else false
        with Invalid_argument _ -> false
      in
      let count, symbs, left_nt =
        if is_nested then
          count+1, symbs,
          "dypgen__nested_nt_"^(string_of_int count)
        else
          count, (String_set.add qsymb symbs), qsymb
      in
      let rule1 =
        (left_nt,[dummy_pat_inh]),("default_priority",(-1,-1,-1,"")),
        [symb,("_",(Pat_syn pat_typ),Lexing.dummy_pos)],
        ("###",(Lexing.dummy_pos,false)), (true,true)
        (* The string ### is replaced by None in dypgen.ml if symb
        is a terminal with no argument and by Some _1 otherwise. *)
      in
      let rule2 =
        (left_nt,[dummy_pat_inh]),("default_priority",(-1,-1,-1,"")),[],
        ("None",(Lexing.dummy_pos,false)), (true,true)
      in
      ((Symb_non_terminal ((left_nt,dummy_p),
        ("No_priority",dummy_p),Pr_eq,dummy_code)),
      (rule1::rule2::add_rules)),
      [Dyp.Global_data {dyp.global_data with gd_count=count;
        gd_symbs=symbs}] ):'dypgen__Obj_symbol * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 2732               "dypgen_parser_temp.ml"
  in Obj_symbol(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPAREN";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2741               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2744               "dypgen_parser_temp.ml"
 as _2)); _3; _4;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2747               "dypgen_parser_temp.ml"
 as _5); _6] -> Obj_symb 
# 443 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal (_1,_5,Pr_eq,_2)),[] ):'dypgen__Obj_symb)
# 2752               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENLESS";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2760               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2763               "dypgen_parser_temp.ml"
 as _2)); _3; _4;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2766               "dypgen_parser_temp.ml"
 as _5); _6] -> Obj_symb 
# 445 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal (_1,_5,Pr_lesseq,_2)),[] ):'dypgen__Obj_symb)
# 2771               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENLESS";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2779               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2782               "dypgen_parser_temp.ml"
 as _2)); _3;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2785               "dypgen_parser_temp.ml"
 as _4); _5] -> Obj_symb 
# 447 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal (_1,_4,Pr_less,_2)),[] ):'dypgen__Obj_symb)
# 2790               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENGREATER";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2798               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2801               "dypgen_parser_temp.ml"
 as _2)); _3; _4;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2804               "dypgen_parser_temp.ml"
 as _5); _6] -> Obj_symb 
# 449 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal (_1,_5,Pr_greatereq,_2)),[] ):'dypgen__Obj_symb)
# 2809               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENGREATER";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2817               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2820               "dypgen_parser_temp.ml"
 as _2)); _3;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2823               "dypgen_parser_temp.ml"
 as _4); _5] -> Obj_symb 
# 451 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal (_1,_4,Pr_greater,_2)),[] ):'dypgen__Obj_symb)
# 2828               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
# 453 "dypgen_parser.dyp"
          (n,_:(string * (int * int * int * string)))
# 2837               "dypgen_parser_temp.ml"
 as _1);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2840               "dypgen_parser_temp.ml"
 as _2))] -> Obj_symb 
# 452 "dypgen_parser.dyp"
(
                              (
      if String_set.mem n dyp.global_data.gd_regexp_decl
      then raise Giveup else
      (Symb_non_terminal
      ((n,dummy_p),("No_priority",dummy_p),Pr_eq,_2)), [] ):'dypgen__Obj_symb)
# 2849               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2857               "dypgen_parser_temp.ml"
 as _1)] -> Obj_symb 
# 457 "dypgen_parser.dyp"
(
           ( (Symb_terminal _1),[] ):'dypgen__Obj_symb)
# 2862               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPAREN";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2870               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2873               "dypgen_parser_temp.ml"
 as _3)); _4; _5;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2876               "dypgen_parser_temp.ml"
 as _6); _7] -> Obj_symb 
# 459 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal_NL (_2,_6,Pr_eq,_3)),[] ):'dypgen__Obj_symb)
# 2881               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENLESS";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2889               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2892               "dypgen_parser_temp.ml"
 as _3)); _4; _5;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2895               "dypgen_parser_temp.ml"
 as _6); _7] -> Obj_symb 
# 461 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal_NL (_2,_6,Pr_lesseq,_3)),[] ):'dypgen__Obj_symb)
# 2900               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENLESS";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2908               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2911               "dypgen_parser_temp.ml"
 as _3)); _4;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2914               "dypgen_parser_temp.ml"
 as _5); _6] -> Obj_symb 
# 463 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal_NL (_2,_5,Pr_less,_3)),[] ):'dypgen__Obj_symb)
# 2919               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENGREATER";Dyp.Ter "EQUAL";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2927               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2930               "dypgen_parser_temp.ml"
 as _3)); _4; _5;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2933               "dypgen_parser_temp.ml"
 as _6); _7] -> Obj_symb 
# 465 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal_NL (_2,_6,Pr_greatereq,_3)),[] ):'dypgen__Obj_symb)
# 2938               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority );Dyp.Ter "LPARENGREATER";Dyp.Ter "LIDENT";Dyp.Ter "RPAREN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2946               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2949               "dypgen_parser_temp.ml"
 as _3)); _4;Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 2952               "dypgen_parser_temp.ml"
 as _5); _6] -> Obj_symb 
# 467 "dypgen_parser.dyp"
(
      ( (Symb_non_terminal_NL (_2,_5,Pr_greater,_3)),[] ):'dypgen__Obj_symb)
# 2957               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "LIDENT";Dyp.Non_ter ("optional_code",Dyp.No_priority )],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_LIDENT  (
# 469 "dypgen_parser.dyp"
               (n,_:(string * (int * int * int * string)))
# 2966               "dypgen_parser_temp.ml"
 as _2);Obj_optional_code ( (
(_:'dypgen__Obj_optional_code)
# 2969               "dypgen_parser_temp.ml"
 as _3))] -> Obj_symb 
# 468 "dypgen_parser.dyp"
(
                                   (
      (Symb_non_terminal_NL
      ((n,dummy_p),("No_priority",dummy_p),Pr_eq,_3)), [] ):'dypgen__Obj_symb)
# 2976               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "DASH";Dyp.Ter "UIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_UIDENT  (
(_:(string * (int * int * int * string)))
# 2984               "dypgen_parser_temp.ml"
 as _2)] -> Obj_symb 
# 471 "dypgen_parser.dyp"
(
                ( (Symb_terminal_NL _2),[] ):'dypgen__Obj_symb)
# 2989               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("symb",[Dyp.Ter "LBRACK";Dyp.Non_ter ("rhs_list",Dyp.No_priority );Dyp.Ter "RBRACK"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [ _1;Obj_rhs_list ( (
(_:'dypgen__Obj_rhs_list)
# 2997               "dypgen_parser_temp.ml"
 as _2)); _3] ->  let res = 
# 473 "dypgen_parser.dyp"
(
    ( let { gd_count=count; gd_symbs=symbs} = dyp.global_data in
      let nt = "dypgen__nested_nt_"^(string_of_int count) in
      let f (rl1,rl2) (prio,symbl,code,allow_layout,add_rules) =
        ((nt,[dummy_pat_inh]),prio,symbl,code,allow_layout)::rl1,add_rules@rl2
      in
      let rl1, rl2 = List.fold_left f ([],[]) (List.rev _2) in
      ((Symb_non_terminal
        ((nt,dummy_p),("No_priority",dummy_p),Pr_eq,dummy_code)), rl2@rl1),
      [Dyp.Global_data
        {dyp.global_data with gd_count=count+1; gd_symbs=symbs}] ):'dypgen__Obj_symb * ('t,'obj,'gd,'ld,'l) Dyp.dyp_action list)
# 3011               "dypgen_parser_temp.ml"
  in Obj_symb(fst res), snd res
 | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("priority",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_priority 
# 485 "dypgen_parser.dyp"
(
          ( ("default_priority",(-1,-1,-1,"")) ):'dypgen__Obj_priority)
# 3022               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("priority",[Dyp.Ter "LIDENT"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_LIDENT  (
(_:(string * (int * int * int * string)))
# 3030               "dypgen_parser_temp.ml"
 as _1)] -> Obj_priority 
# 486 "dypgen_parser.dyp"
(
           ( _1 ):'dypgen__Obj_priority)
# 3035               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_pattern",[],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [] -> Obj_opt_pattern 
# 495 "dypgen_parser.dyp"
(
           ( ("_",Lexing.dummy_pos) ):'dypgen__Obj_opt_pattern)
# 3045               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])
;
(("opt_pattern",[Dyp.Ter "PATTERN"],"default_priority",[]),
Dyp.Dypgen_action (fun __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl ->
(Dyp.Tools.transform_action (fun dyp __dypgen_av_list -> (match (__dypgen_av_list) with [Obj_PATTERN  (
(_:(string * Lexing.position))
# 3053               "dypgen_parser_temp.ml"
 as _1)] -> Obj_opt_pattern 
# 496 "dypgen_parser.dyp"
(
            ( _1 ):'dypgen__Obj_opt_pattern)
# 3058               "dypgen_parser_temp.ml"
,[] | _ -> raise Dyp.Giveup))) __dypgen_ol __dypgen_pos __dypgen_posl __dypgen_gd __dypgen_ld __dypgen_lld __dypgen_di __dypgen_p __dypgen_nl),
[])],

(["dummy_entry",Dyp.RE_Eof_char],
[0,(fun _ -> Lexeme_matched "")]),

[]

let __dypgen_regexp_decl = []

let dyp_merge_Lexeme_matched l =
  match dyp_merge_Lexeme_matched l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_CHAR l =
  match dyp_merge_Obj_CHAR l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_LIDENT l =
  match dyp_merge_Obj_LIDENT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_OCAML_CODE l =
  match dyp_merge_Obj_OCAML_CODE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_OCAML_TYPE l =
  match dyp_merge_Obj_OCAML_TYPE l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_PATTERN l =
  match dyp_merge_Obj_PATTERN l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_STRING l =
  match dyp_merge_Obj_STRING l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_UIDENT l =
  match dyp_merge_Obj_UIDENT l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_action_prio l =
  match dyp_merge_Obj_action_prio l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_aux_lexer l =
  match dyp_merge_Obj_aux_lexer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_aux_lexer_and l =
  match dyp_merge_Obj_aux_lexer_and l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_aux_lexer_def l =
  match dyp_merge_Obj_aux_lexer_def l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_aux_lexer_rule l =
  match dyp_merge_Obj_aux_lexer_rule l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_char_elt l =
  match dyp_merge_Obj_char_elt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_0 l =
  match dyp_merge_Obj_dypgen__nested_nt_0 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_1 l =
  match dyp_merge_Obj_dypgen__nested_nt_1 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_10 l =
  match dyp_merge_Obj_dypgen__nested_nt_10 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_11 l =
  match dyp_merge_Obj_dypgen__nested_nt_11 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_2 l =
  match dyp_merge_Obj_dypgen__nested_nt_2 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_3 l =
  match dyp_merge_Obj_dypgen__nested_nt_3 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_4 l =
  match dyp_merge_Obj_dypgen__nested_nt_4 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_5 l =
  match dyp_merge_Obj_dypgen__nested_nt_5 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_6 l =
  match dyp_merge_Obj_dypgen__nested_nt_6 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_7 l =
  match dyp_merge_Obj_dypgen__nested_nt_7 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_8 l =
  match dyp_merge_Obj_dypgen__nested_nt_8 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__nested_nt_9 l =
  match dyp_merge_Obj_dypgen__nested_nt_9 l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_BAR l =
  match dyp_merge_Obj_dypgen__option_BAR l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_SEMI l =
  match dyp_merge_Obj_dypgen__option_SEMI l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_aux_lexer l =
  match dyp_merge_Obj_dypgen__option_aux_lexer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__option_lexer l =
  match dyp_merge_Obj_dypgen__option_lexer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__plus___char_elt l =
  match dyp_merge_Obj_dypgen__plus___char_elt l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__plus___lident l =
  match dyp_merge_Obj_dypgen__plus___lident l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__star___aux_lexer_and l =
  match dyp_merge_Obj_dypgen__star___aux_lexer_and l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__star___lexer_rule l =
  match dyp_merge_Obj_dypgen__star___lexer_rule l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_dypgen__star___regexp_decl l =
  match dyp_merge_Obj_dypgen__star___regexp_decl l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_entry_def l =
  match dyp_merge_Obj_entry_def l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_grammar l =
  match dyp_merge_Obj_grammar l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_ident_list l =
  match dyp_merge_Obj_ident_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lexer l =
  match dyp_merge_Obj_lexer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lexer_rule l =
  match dyp_merge_Obj_lexer_rule l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lident l =
  match dyp_merge_Obj_lident l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_lident_list l =
  match dyp_merge_Obj_lident_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_main l =
  match dyp_merge_Obj_main l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_main_lexer l =
  match dyp_merge_Obj_main_lexer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_opt_bang l =
  match dyp_merge_Obj_opt_bang l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_opt_dash l =
  match dyp_merge_Obj_opt_dash l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_opt_pattern l =
  match dyp_merge_Obj_opt_pattern l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_opt_token_name l =
  match dyp_merge_Obj_opt_token_name l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_code l =
  match dyp_merge_Obj_optional_code l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_mli l =
  match dyp_merge_Obj_optional_mli l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_mlimid l =
  match dyp_merge_Obj_optional_mlimid l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_mlitop l =
  match dyp_merge_Obj_optional_mlitop l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_mltop l =
  match dyp_merge_Obj_optional_mltop l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_trailer l =
  match dyp_merge_Obj_optional_trailer l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_optional_type l =
  match dyp_merge_Obj_optional_type l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_parser_begin l =
  match dyp_merge_Obj_parser_begin l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_parser_param_info l =
  match dyp_merge_Obj_parser_param_info l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_parser_param_infos l =
  match dyp_merge_Obj_parser_param_infos l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_priority l =
  match dyp_merge_Obj_priority l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_regexp l =
  match dyp_merge_Obj_regexp l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_regexp_decl l =
  match dyp_merge_Obj_regexp_decl l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_regexp_ter l =
  match dyp_merge_Obj_regexp_ter l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_relation l =
  match dyp_merge_Obj_relation l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_relation_list l =
  match dyp_merge_Obj_relation_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rhs l =
  match dyp_merge_Obj_rhs l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_rhs_list l =
  match dyp_merge_Obj_rhs_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_symb l =
  match dyp_merge_Obj_symb l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_symbol l =
  match dyp_merge_Obj_symbol l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_symbol_list l =
  match dyp_merge_Obj_symbol_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_token_list l =
  match dyp_merge_Obj_token_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res
let dyp_merge_Obj_uident_list l =
  match dyp_merge_Obj_uident_list l with
    | ([],_,_) -> dyp_merge l
    | res -> res

let __dypgen_merge_list = [(fun l -> (
  let f1 (o,gd,ld) = match o with Lexeme_matched ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Lexeme_matched"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Lexeme_matched l in
  let f2 o = Lexeme_matched o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_CHAR ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_CHAR"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_CHAR l in
  let f2 o = Obj_CHAR o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_LIDENT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_LIDENT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_LIDENT l in
  let f2 o = Obj_LIDENT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_OCAML_CODE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_OCAML_CODE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_OCAML_CODE l in
  let f2 o = Obj_OCAML_CODE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_OCAML_TYPE ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_OCAML_TYPE"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_OCAML_TYPE l in
  let f2 o = Obj_OCAML_TYPE o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_PATTERN ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_PATTERN"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_PATTERN l in
  let f2 o = Obj_PATTERN o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_STRING ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_STRING"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_STRING l in
  let f2 o = Obj_STRING o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_UIDENT ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_UIDENT"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_UIDENT l in
  let f2 o = Obj_UIDENT o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_action_prio ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_action_prio"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_action_prio l in
  let f2 o = Obj_action_prio o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_aux_lexer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_aux_lexer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_aux_lexer l in
  let f2 o = Obj_aux_lexer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_aux_lexer_and ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_aux_lexer_and"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_aux_lexer_and l in
  let f2 o = Obj_aux_lexer_and o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_aux_lexer_def ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_aux_lexer_def"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_aux_lexer_def l in
  let f2 o = Obj_aux_lexer_def o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_aux_lexer_rule ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_aux_lexer_rule"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_aux_lexer_rule l in
  let f2 o = Obj_aux_lexer_rule o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_char_elt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_char_elt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_char_elt l in
  let f2 o = Obj_char_elt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_0 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_0"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_0 l in
  let f2 o = Obj_dypgen__nested_nt_0 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_1 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_1"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_1 l in
  let f2 o = Obj_dypgen__nested_nt_1 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_10 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_10"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_10 l in
  let f2 o = Obj_dypgen__nested_nt_10 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_11 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_11"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_11 l in
  let f2 o = Obj_dypgen__nested_nt_11 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_2 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_2"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_2 l in
  let f2 o = Obj_dypgen__nested_nt_2 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_3 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_3"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_3 l in
  let f2 o = Obj_dypgen__nested_nt_3 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_4 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_4"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_4 l in
  let f2 o = Obj_dypgen__nested_nt_4 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_5 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_5"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_5 l in
  let f2 o = Obj_dypgen__nested_nt_5 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_6 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_6"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_6 l in
  let f2 o = Obj_dypgen__nested_nt_6 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_7 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_7"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_7 l in
  let f2 o = Obj_dypgen__nested_nt_7 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_8 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_8"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_8 l in
  let f2 o = Obj_dypgen__nested_nt_8 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__nested_nt_9 ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__nested_nt_9"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__nested_nt_9 l in
  let f2 o = Obj_dypgen__nested_nt_9 o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_BAR ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_BAR"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_BAR l in
  let f2 o = Obj_dypgen__option_BAR o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_SEMI ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_SEMI"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_SEMI l in
  let f2 o = Obj_dypgen__option_SEMI o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_aux_lexer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_aux_lexer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_aux_lexer l in
  let f2 o = Obj_dypgen__option_aux_lexer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__option_lexer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__option_lexer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__option_lexer l in
  let f2 o = Obj_dypgen__option_lexer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__plus___char_elt ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__plus___char_elt"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__plus___char_elt l in
  let f2 o = Obj_dypgen__plus___char_elt o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__plus___lident ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__plus___lident"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__plus___lident l in
  let f2 o = Obj_dypgen__plus___lident o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__star___aux_lexer_and ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__star___aux_lexer_and"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__star___aux_lexer_and l in
  let f2 o = Obj_dypgen__star___aux_lexer_and o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__star___lexer_rule ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__star___lexer_rule"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__star___lexer_rule l in
  let f2 o = Obj_dypgen__star___lexer_rule o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_dypgen__star___regexp_decl ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_dypgen__star___regexp_decl"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_dypgen__star___regexp_decl l in
  let f2 o = Obj_dypgen__star___regexp_decl o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_entry_def ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_entry_def"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_entry_def l in
  let f2 o = Obj_entry_def o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_grammar ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_grammar"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_grammar l in
  let f2 o = Obj_grammar o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_ident_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_ident_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_ident_list l in
  let f2 o = Obj_ident_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lexer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lexer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lexer l in
  let f2 o = Obj_lexer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lexer_rule ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lexer_rule"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lexer_rule l in
  let f2 o = Obj_lexer_rule o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lident ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lident"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lident l in
  let f2 o = Obj_lident o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_lident_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_lident_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_lident_list l in
  let f2 o = Obj_lident_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_main ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_main"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_main l in
  let f2 o = Obj_main o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_main_lexer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_main_lexer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_main_lexer l in
  let f2 o = Obj_main_lexer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_opt_bang ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_opt_bang"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_opt_bang l in
  let f2 o = Obj_opt_bang o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_opt_dash ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_opt_dash"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_opt_dash l in
  let f2 o = Obj_opt_dash o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_opt_pattern ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_opt_pattern"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_opt_pattern l in
  let f2 o = Obj_opt_pattern o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_opt_token_name ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_opt_token_name"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_opt_token_name l in
  let f2 o = Obj_opt_token_name o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_code ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_code"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_code l in
  let f2 o = Obj_optional_code o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_mli ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_mli"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_mli l in
  let f2 o = Obj_optional_mli o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_mlimid ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_mlimid"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_mlimid l in
  let f2 o = Obj_optional_mlimid o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_mlitop ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_mlitop"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_mlitop l in
  let f2 o = Obj_optional_mlitop o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_mltop ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_mltop"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_mltop l in
  let f2 o = Obj_optional_mltop o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_trailer ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_trailer"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_trailer l in
  let f2 o = Obj_optional_trailer o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_optional_type ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_optional_type"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_optional_type l in
  let f2 o = Obj_optional_type o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_parser_begin ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_parser_begin"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_parser_begin l in
  let f2 o = Obj_parser_begin o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_parser_param_info ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_parser_param_info"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_parser_param_info l in
  let f2 o = Obj_parser_param_info o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_parser_param_infos ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_parser_param_infos"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_parser_param_infos l in
  let f2 o = Obj_parser_param_infos o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_priority ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_priority"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_priority l in
  let f2 o = Obj_priority o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_regexp ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_regexp"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_regexp l in
  let f2 o = Obj_regexp o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_regexp_decl ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_regexp_decl"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_regexp_decl l in
  let f2 o = Obj_regexp_decl o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_regexp_ter ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_regexp_ter"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_regexp_ter l in
  let f2 o = Obj_regexp_ter o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_relation ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_relation"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_relation l in
  let f2 o = Obj_relation o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_relation_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_relation_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_relation_list l in
  let f2 o = Obj_relation_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rhs ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rhs"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rhs l in
  let f2 o = Obj_rhs o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_rhs_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_rhs_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_rhs_list l in
  let f2 o = Obj_rhs_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_symb ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_symb"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_symb l in
  let f2 o = Obj_symb o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_symbol ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_symbol"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_symbol l in
  let f2 o = Obj_symbol o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_symbol_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_symbol_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_symbol_list l in
  let f2 o = Obj_symbol_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_token_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_token_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_token_list l in
  let f2 o = Obj_token_list o in
  (List.map f2 ol, gd, ld)));
(fun l -> (
  let f1 (o,gd,ld) = match o with Obj_uident_list ob -> (ob,gd,ld)
    | _ -> failwith "type error, bad obj in dyp_merge_Obj_uident_list"
  in
  let l = List.map f1 l in
  let (ol,gd,ld) = dyp_merge_Obj_uident_list l in
  let f2 o = Obj_uident_list o in
  (List.map f2 ol, gd, ld)))]



let __dypgen_test_cons () =  [|
  (fun x -> match x with Lexeme_matched _ -> true | _ -> false);
  (fun x -> match x with Obj_CHAR _ -> true | _ -> false);
  (fun x -> match x with Obj_LIDENT _ -> true | _ -> false);
  (fun x -> match x with Obj_OCAML_CODE _ -> true | _ -> false);
  (fun x -> match x with Obj_OCAML_TYPE _ -> true | _ -> false);
  (fun x -> match x with Obj_PATTERN _ -> true | _ -> false);
  (fun x -> match x with Obj_STRING _ -> true | _ -> false);
  (fun x -> match x with Obj_UIDENT _ -> true | _ -> false);
  (fun x -> match x with Obj_action_prio _ -> true | _ -> false);
  (fun x -> match x with Obj_aux_lexer _ -> true | _ -> false);
  (fun x -> match x with Obj_aux_lexer_and _ -> true | _ -> false);
  (fun x -> match x with Obj_aux_lexer_def _ -> true | _ -> false);
  (fun x -> match x with Obj_aux_lexer_rule _ -> true | _ -> false);
  (fun x -> match x with Obj_char_elt _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_0 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_1 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_10 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_11 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_2 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_3 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_4 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_5 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_6 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_7 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_8 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__nested_nt_9 _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_BAR _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_SEMI _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_aux_lexer _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__option_lexer _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__plus___char_elt _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__plus___lident _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__star___aux_lexer_and _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__star___lexer_rule _ -> true | _ -> false);
  (fun x -> match x with Obj_dypgen__star___regexp_decl _ -> true | _ -> false);
  (fun x -> match x with Obj_entry_def _ -> true | _ -> false);
  (fun x -> match x with Obj_grammar _ -> true | _ -> false);
  (fun x -> match x with Obj_ident_list _ -> true | _ -> false);
  (fun x -> match x with Obj_lexer _ -> true | _ -> false);
  (fun x -> match x with Obj_lexer_rule _ -> true | _ -> false);
  (fun x -> match x with Obj_lident _ -> true | _ -> false);
  (fun x -> match x with Obj_lident_list _ -> true | _ -> false);
  (fun x -> match x with Obj_main _ -> true | _ -> false);
  (fun x -> match x with Obj_main_lexer _ -> true | _ -> false);
  (fun x -> match x with Obj_opt_bang _ -> true | _ -> false);
  (fun x -> match x with Obj_opt_dash _ -> true | _ -> false);
  (fun x -> match x with Obj_opt_pattern _ -> true | _ -> false);
  (fun x -> match x with Obj_opt_token_name _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_code _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_mli _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_mlimid _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_mlitop _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_mltop _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_trailer _ -> true | _ -> false);
  (fun x -> match x with Obj_optional_type _ -> true | _ -> false);
  (fun x -> match x with Obj_parser_begin _ -> true | _ -> false);
  (fun x -> match x with Obj_parser_param_info _ -> true | _ -> false);
  (fun x -> match x with Obj_parser_param_infos _ -> true | _ -> false);
  (fun x -> match x with Obj_priority _ -> true | _ -> false);
  (fun x -> match x with Obj_regexp _ -> true | _ -> false);
  (fun x -> match x with Obj_regexp_decl _ -> true | _ -> false);
  (fun x -> match x with Obj_regexp_ter _ -> true | _ -> false);
  (fun x -> match x with Obj_relation _ -> true | _ -> false);
  (fun x -> match x with Obj_relation_list _ -> true | _ -> false);
  (fun x -> match x with Obj_rhs _ -> true | _ -> false);
  (fun x -> match x with Obj_rhs_list _ -> true | _ -> false);
  (fun x -> match x with Obj_symb _ -> true | _ -> false);
  (fun x -> match x with Obj_symbol _ -> true | _ -> false);
  (fun x -> match x with Obj_symbol_list _ -> true | _ -> false);
  (fun x -> match x with Obj_token_list _ -> true | _ -> false);
  (fun x -> match x with Obj_uident_list _ -> true | _ -> false)|]

let __dypgen_dummy_marker_2 = ()
let pp () = Dyp.make_parser
  __dypgen_ra_list Dyp_priority_data.relations global_data local_data
  (Dyp.Tools.make_nt_cons_map Dyp_symbols_array.nt_cons_list)
  Dyp_symbols_array.entry_points
  
  false 47 true
  
  Dyp_aux_functions.get_token_value
  Dyp_symbols.get_token_name Dyp_symbols.str_token
  global_data_equal local_data_equal (__dypgen_test_cons ())
  Dyp_symbols_array.str_cons
  Dyp_symbols_array.cons_array Dyp_aux_functions.cons_table
  (Dyp.Tools.array_of_list __dypgen_merge_list)
  dypgen_lexbuf_position __dypgen_regexp_decl __dypgen_main_lexer
  __dypgen_aux_lexer Dyp_symbols.ter_string_list
  (fun lexbuf -> Lexeme_matched (Dyp.lexeme lexbuf))
  false


let __dypgen_dummy_marker_5 = ()

let __dypgen_dummy_marker_3 = ()

let main ?(global_data=global_data) ?(local_data=local_data) f lexbuf =
  let pf = Dyp.parse (pp ()) "main" ~global_data:global_data
    ~local_data:local_data ~match_len:dypgen_match_length
    ~keep_data:dypgen_keep_data
    ~use_rule_order:dypgen_use_rule_order
    ~use_all_actions:dypgen_use_all_actions
    ~lexpos:dypgen_lexbuf_position f lexbuf in
  let aux1 (o,p) = match o with
    | Obj_main r -> (r,p)
    | _ -> failwith "Wrong type for entry result" in
  List.map aux1 pf


let __dypgen_dummy_marker_4 = ()

