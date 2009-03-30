(*
pgen: a simple GLR parser generator for Objective Caml,
priority_by_relation is used.
Dyp_special_types.Dypgen_action actions are not possible with pgen and actions cannot use the structures
data and priority_data, priorities of rules are constant only.
Cyclic grammars are not handled.
If no priority are stated for a rule in pgen input file, then default_priority
(i.e. 0) is chosen.
merge functions (for two sub parse trees reduced to the same non terminal) cannot be
stated by the user and is always the choice of the first parse tree.

An input file for pgen follows this frame :

{
optional header code for the Parser_parameters module.
}

%token Token1 <type> Token2 (* Constructors for token *)
%start <type> main
%relation (* optional field *)
p1:p2
p1:p3 (* means the relation is true for p1->p2, the field %relation is optional *)

%%

main:
 | sequence of terminals and non terminals { ocaml action code } priority
...

non_terminal:
 | ... {...} priority
...

{
Optional ocaml code, trailer of the main parser code.
}

This is the end of the input file.

In action code $1 is the name of the obj associated to the first literal in the
rhs of the rule, $2 the second, etc.
*)

module P = Dyp.Make_dyp(Pgen_parser_param)

open Pgen_parser_param
open P
open P.Parser_PAR
open Printf
open Dyp
let priority_data, default_priority =
  Dyp.insert_priority Dyp.empty_priority_data "default_priority"

let str_token t = match t with
  | LPAREN -> "("
  | RPAREN -> ")"
  | COLON -> ":"
  | PERCENTPERCENT -> "%%"
  | LBRACE -> "}"
  | RBRACE -> "}"
  | BAR -> "|"
  | EQUAL -> "="
  | EOF -> "EOF"
  | KWD_TOKEN -> "%token"
  | KWD_START -> "%start"
  | KWD_RELATION -> "%relation"
  | KWD_FULL -> "%full"
  | OCAML_CODE c -> c
  | OCAML_TYPE t -> t
  | UIDENT i -> i
  | LIDENT i -> i

let get_name t = match t with
  | LPAREN -> tn_lparen
  | RPAREN -> tn_rparen
  | COLON -> tn_colon
  | PERCENTPERCENT -> tn_percentpercent
  | LBRACE -> tn_lbrace
  | RBRACE -> tn_rbrace
  | BAR -> tn_bar
  | EQUAL -> tn_equal
  | EOF -> tn_EOF
  | KWD_TOKEN -> tn_kwd_token
  | KWD_START -> tn_kwd_start
  | KWD_RELATION -> tn_kwd_relation
  | KWD_FULL -> tn_kwd_full
  | OCAML_CODE _ -> tn_ocaml_code
  | OCAML_TYPE _ -> tn_ocaml_type
  | UIDENT _ -> tn_uident
  | LIDENT _ -> tn_lident


(*let () = dypgen_verbose := 2*)
(*let () = number_of_tokens := 19*)

let verbose_ref = ref 0
let process_verbose_mode () = verbose_ref := 1
let string_ref = ref ""
let process_argument s =
  if s = "" then raise (Arg.Bad "missing input file name")
  else string_ref := s
let list_arg = [("-v",Arg.Unit process_verbose_mode,"activate verbose mode: gives details of the parsing of the input file")]
let _ = Arg.parse list_arg process_argument "usage: pgen [-v] file_name.dyp"
let _ = if !string_ref = "" then
  let _ = print_string "usage: pgen [-v] file_name.dyp\n" in exit 0

let prio = 0


let ntn_start = 1
let ntn_parser_param_info = 2
let ntn_token_list = 3
let ntn_relation = 4
let ntn_start_def = 5
let ntn_grammar = 6
let ntn_bar_opt = 7
let ntn_literal_list = 8
let ntn_priority = 9
let ntn_optional_code = 10
(*let ntn_startprime = 0*)

type data = Data_void

type obj =
    Obj_void
  | Code of string
  | Obj_type of string
  | Parser_parameters_info of parser_param_info
  | Token_list of (token_desc list)
  | Obj_start of string * string
  | Relation of relation_desc
  | Grammar of (rule_desc list)
  | Literal_list of (literal_desc list)
  | Obj_uident of string
  | Obj_lident of string
  | Obj_lparen
  | Obj_rparen
  | Obj_equal
  | Pgen_input of (string * parser_param_info * (rule_desc list) * string)
(* Pgen_input : (header code of module Parser_parameters, token list, eof token, relation (optional), trailer code of module Parser_parameters, header code of parser (optional), grammar, trailer code of parser (optional))*)

let r_pgen_input = (ntn_start,[Non_ter (ntn_optional_code, No_priority); Non_ter (ntn_parser_param_info,No_priority); Ter tn_percentpercent; Non_ter (ntn_grammar,No_priority); Non_ter (ntn_optional_code, No_priority); Ter tn_EOF],default_priority)
(* Start -> optional_code parser_param_inf PercentPercent grammar optional_code Eof *)

let a_pgen_input = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Code c1; Parser_parameters_info (tl,rel,start); _; Grammar g; Code c2; _] ->
        Pgen_input (c1,(tl,rel,start),g,c2)
    | _ -> failwith "a_pgen_input"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_start_def = (ntn_start_def,[Ter tn_kwd_start; Ter tn_ocaml_type; Ter tn_lident],default_priority)
(* start_def -> Kwd_start Ocaml_type Lident *)

let a_start_def = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Obj_void; Obj_type t; Obj_lident s] -> Obj_start (s,t)
    | _ -> failwith "a_start_def"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_parser_param_info_0 = (ntn_parser_param_info,[Non_ter (ntn_start_def,No_priority)],default_priority)
(* parser_param_info -> start_def *)
let r_parser_param_info_1 = (ntn_parser_param_info,[Non_ter (ntn_token_list,No_priority)],default_priority)
(* parser_param_info -> token_list *)
let r_parser_param_info_3 = (ntn_parser_param_info,[Non_ter (ntn_relation,No_priority)],default_priority)
(* parser_param_info -> relation *)
let r_parser_param_info_4 = (ntn_parser_param_info,[Non_ter (ntn_parser_param_info, No_priority); Non_ter (ntn_parser_param_info, No_priority)],default_priority)
(* parser_param_info -> parser_param_info parser_param_info *)

let a_parser_param_info = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Obj_start (st,t)] -> Parser_parameters_info ([],[],(st,t))
    | [Token_list tl] -> Parser_parameters_info (tl,[],("",""))
    | [Relation rel] -> Parser_parameters_info ([],rel,("",""))
    | [Parser_parameters_info (tl1,rel1,st1); Parser_parameters_info (tl2,rel2,st2)] ->
        let st = if fst st2 = "" then st1 else st2 in
        Parser_parameters_info (tl1@tl2,rel1@rel2,st)
    | _ -> failwith "a_parser_param_info"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_optional_code_0 = (ntn_optional_code,[],default_priority)
(* optional_code -> *)
let r_optional_code_1 = (ntn_optional_code,[Ter tn_ocaml_code],default_priority)
(* optional_code -> Ocaml_code *)

let a_optional_code = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [] -> Code ""
    | [Code c] -> Code c
    | _ -> failwith "a_optional_code"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_token_list_1 = (ntn_token_list,[Ter tn_kwd_token; Ter tn_uident],default_priority)
(* token_list -> Kwd_token Uident *)
let r_token_list_2 = (ntn_token_list,[Ter tn_kwd_token; Ter tn_ocaml_type;  Ter tn_uident],default_priority)
(* token_list -> Kwd_token Ocaml_type Uident *)
let r_token_list_3 = (ntn_token_list,[Non_ter (ntn_token_list,No_priority); Ter tn_uident],default_priority)
(* token_list -> token_list Uident *)
let r_token_list_4 = (ntn_token_list,[Non_ter (ntn_token_list,No_priority); Ter tn_ocaml_type;  Ter tn_uident],default_priority)
(* token_list -> token_list Ocaml_type Uident *)

let a_token_list = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Obj_void; Obj_uident tok] -> Token_list [(tok,"No_type")]
    | [Obj_void; Obj_type typ; Obj_uident tok] ->
        Token_list [(tok,typ)]
    | [Token_list tl; Obj_uident tok] -> Token_list ((tok,"No_type")::tl)
    | [Token_list tl; Obj_type typ; Obj_uident tok] ->
        Token_list ((tok,typ)::tl)
    | _ -> failwith "a_token_list"
),true,false,Data_void,dd,ld,[],[],prd,None,None)


let r_relation_1 = (ntn_relation,[Ter tn_kwd_relation],default_priority)
(* relation -> Kwd_relation *)
let r_relation_2 = (ntn_relation,[Ter tn_kwd_relation; Ter tn_lident; Ter tn_colon; Ter tn_lident],default_priority)
(* relation -> Kwd_relation Lident Colon Lident *)
let r_relation_3 = (ntn_relation,[Non_ter (ntn_relation,No_priority); Ter tn_lident; Ter tn_colon; Ter tn_lident],default_priority)
(* relation -> relation Lident Colon Lident *)

let a_relation = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Obj_void] -> Relation []
    | [Obj_void; Obj_lident p1; Obj_void; Obj_lident p2] -> Relation [(p1,p2)]
    | [Relation rel; Obj_lident p1; Obj_void; Obj_lident p2] -> Relation ((p1,p2)::rel)
    | _ -> failwith "a_relation"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_grammar_1 = (ntn_grammar,[Ter tn_lident; Ter tn_colon; Non_ter (ntn_bar_opt,No_priority); Non_ter (ntn_literal_list,No_priority); Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> Lident Colon opt_bar literal_list Ocaml_code priority *)
let r_grammar_2 = (ntn_grammar,[Non_ter (ntn_grammar,No_priority); Ter tn_lident; Ter tn_colon; Non_ter (ntn_bar_opt,No_priority); Non_ter (ntn_literal_list,No_priority); Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> grammar Lident Colon opt_bar literal_list Ocaml_code priority *)
let r_grammar_3 = (ntn_grammar,[Non_ter (ntn_grammar,No_priority);Ter tn_bar; Non_ter (ntn_literal_list,No_priority); Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> grammar Bar literal_list Ocaml_code priority *)

let r_grammar_4 = (ntn_grammar,[Ter tn_lident; Ter tn_colon; Non_ter (ntn_bar_opt,No_priority); Non_ter (ntn_literal_list,No_priority); Ter tn_kwd_full; Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> Lident Colon opt_bar literal_list KWD_FULL Ocaml_code priority *)
let r_grammar_5 = (ntn_grammar,[Non_ter (ntn_grammar,No_priority); Ter tn_lident; Ter tn_colon; Non_ter (ntn_bar_opt,No_priority); Non_ter (ntn_literal_list,No_priority); Ter tn_kwd_full; Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> grammar Lident Colon opt_bar literal_list KWD_FULL Ocaml_code priority *)
let r_grammar_6 = (ntn_grammar,[Non_ter (ntn_grammar,No_priority);Ter tn_bar; Non_ter (ntn_literal_list,No_priority); Ter tn_kwd_full; Ter tn_ocaml_code; Non_ter (ntn_priority,No_priority)],default_priority)
(* grammar -> grammar Bar literal_list KWD_FULL Ocaml_code priority *)

let a_grammar = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [Obj_lident nt;Obj_void;Obj_void;Literal_list ll;Code c;Obj_lident p] ->
        Grammar [(nt,p,List.rev ll,Classic_action c)]
    | [Grammar g;Obj_lident nt;Obj_void;Obj_void;Literal_list ll;Code c;Obj_lident p] ->
        Grammar ((nt,p,List.rev ll,Classic_action c)::g)
    | [Grammar g;Obj_void;Literal_list ll;Code c;Obj_lident p] ->
        let last_rule = List.hd g in
        let lhs_non_terminal,_,_,_ = last_rule in
        Grammar ((lhs_non_terminal,p,List.rev ll,Classic_action c)::g)
    | [Obj_lident nt;Obj_void;Obj_void;Literal_list ll;Obj_void;Code c;Obj_lident p] ->
        Grammar [(nt,p,List.rev ll,Full_action c)]
    | [Grammar g;Obj_lident nt;Obj_void;Obj_void;Literal_list ll;Obj_void;Code c;Obj_lident p] ->
        Grammar ((nt,p,List.rev ll,Full_action c)::g)
    | [Grammar g;Obj_void;Literal_list ll;Obj_void;Code c;Obj_lident p] ->
        let last_rule = List.hd g in
        let lhs_non_terminal,_,_,_ = last_rule in
        Grammar ((lhs_non_terminal,p,List.rev ll,Full_action c)::g)
    | _ -> failwith "a_grammar"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_bar_opt_0 = (ntn_bar_opt,[],default_priority)
(* bar_opt -> *)
let r_bar_opt_1 = (ntn_bar_opt,[Ter tn_bar],default_priority)
(* bar_opt -> Bar *)

let a_bar_opt = Dyp_special_types.Dypgen_action(fun _ _ _ d dd ld _ prd _ -> Obj_void,true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_literal_list_0 = (ntn_literal_list,[],default_priority)
(* literal_list ->  *)
let r_literal_list_1 = (ntn_literal_list,[Ter tn_uident],default_priority)
(* literal_list -> Uident *)
let r_literal_list_2 = (ntn_literal_list,[Ter tn_lident],default_priority)
(* literal_list -> Lident *)
let r_literal_list_3 = (ntn_literal_list,[Ter tn_lident; Ter tn_lparen; Ter tn_equal; Ter tn_lident; Ter tn_rparen],default_priority)
(* literal_list -> Lident Lparen Equal Lident Rparen *)
let r_literal_list_4 = (ntn_literal_list,[Ter tn_lident; Ter tn_lparen; Ter tn_lident; Ter tn_rparen],default_priority)
(* literal_list -> Lident Lparen Lident Rparen *)
let r_literal_list_5 = (ntn_literal_list,[Non_ter (ntn_literal_list,No_priority); Ter tn_uident],default_priority)
(* literal_list -> literal_list Uident *)
let r_literal_list_6 = (ntn_literal_list,[Non_ter (ntn_literal_list,No_priority); Ter tn_lident],default_priority)
(* literal_list -> literal_list Lident *)
let r_literal_list_7 = (ntn_literal_list,[Non_ter (ntn_literal_list,No_priority); Ter tn_lident; Ter tn_lparen; Ter tn_equal; Ter tn_lident; Ter tn_rparen],default_priority)
(* literal_list -> literal_list Lident Lparen Equal Lident Rparen *)
let r_literal_list_8 = (ntn_literal_list,[Non_ter (ntn_literal_list,No_priority); Ter tn_lident; Ter tn_lparen; Ter tn_lident; Ter tn_rparen],default_priority)
(* literal_list -> literal_list Lident Lparen Lident Rparen *)

let a_literal_list = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [] -> Literal_list []
    | [Obj_uident tok] ->
        Literal_list [Obj_terminal tok]
    | [Obj_lident nt] ->
        Literal_list [Obj_non_terminal (nt,"No_priority",false)]
    | [Obj_lident nt; Obj_lparen; Obj_equal; Obj_lident p; Obj_rparen] ->
        Literal_list [Obj_non_terminal (nt,p,true)]
    | [Obj_lident nt; Obj_lparen; Obj_lident p; Obj_rparen] ->
        Literal_list [Obj_non_terminal (nt,p,false)]
    | [Literal_list ll; Obj_uident tok] ->
        Literal_list ((Obj_terminal tok)::ll)
    | [Literal_list ll; Obj_lident nt] ->
        Literal_list ((Obj_non_terminal (nt,"No_priority",false))::ll)
    | [Literal_list ll; Obj_lident nt; Obj_lparen; Obj_equal; Obj_lident p; Obj_rparen] ->
        Literal_list ((Obj_non_terminal (nt,p,true))::ll)
    | [Literal_list ll; Obj_lident nt; Obj_lparen; Obj_lident p; Obj_rparen] ->
       Literal_list ((Obj_non_terminal (nt,p,false))::ll)
    | _ -> failwith "a_literal_list"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let r_priority_0 = (ntn_priority,[],default_priority)
let r_priority_1 = (ntn_priority,[Ter tn_lident],default_priority)

let a_priority = Dyp_special_types.Dypgen_action(fun l _ _ d dd ld _ prd _ ->
  (match l with
    | [] -> Obj_lident "default priority"
    | [Obj_lident s] -> Obj_lident s
    | _ -> failwith "a_priority"
),true,false,Data_void,dd,ld,[],[],prd,None,None)

let prio_data = empty_priority_data

let ra_list = [
(r_pgen_input,a_pgen_input);
(r_optional_code_0,a_optional_code);
(r_optional_code_1,a_optional_code);
(r_start_def,a_start_def);
(r_parser_param_info_0,a_parser_param_info);
(r_parser_param_info_1,a_parser_param_info);
(r_parser_param_info_3,a_parser_param_info);
(r_parser_param_info_4,a_parser_param_info);
(r_token_list_1,a_token_list);
(r_token_list_2,a_token_list);
(r_token_list_3,a_token_list);
(r_token_list_4,a_token_list);
(r_relation_1,a_relation);
(r_relation_2,a_relation);
(r_relation_3,a_relation);
(r_grammar_1,a_grammar);
(r_grammar_2,a_grammar);
(r_grammar_3,a_grammar);
(r_grammar_4,a_grammar);
(r_grammar_5,a_grammar);
(r_grammar_6,a_grammar);
(r_bar_opt_0,a_bar_opt);
(r_bar_opt_1,a_bar_opt);
(r_literal_list_0,a_literal_list);
(r_literal_list_1,a_literal_list);
(r_literal_list_2,a_literal_list);
(r_literal_list_3,a_literal_list);
(r_literal_list_4,a_literal_list);
(r_literal_list_5,a_literal_list);
(r_literal_list_6,a_literal_list);
(r_literal_list_7,a_literal_list);
(r_literal_list_8,a_literal_list);
(r_priority_0,a_priority);
(r_priority_1,a_priority)
]

let parsing_device = create_parsing_device ra_list empty_priority_data `LR0 Data_void 0 P.Tools.empty_datadyn nt_names (Array.make 11 0) priority_names

let input_file = !string_ref
let output_file = (Filename.chop_extension input_file)^".ml"
let output_file_mli = (Filename.chop_extension input_file)^".mli"

let lexbuf = Lexing.from_channel (Pervasives.open_in input_file)

let get_value t = match t with
  | LIDENT s -> Obj_lident s
  | UIDENT s -> Obj_uident s
  | OCAML_CODE s -> Code s
  | OCAML_TYPE t -> Obj_type t
  | LPAREN -> Obj_lparen
  | RPAREN -> Obj_rparen
  | EQUAL -> Obj_equal
  | _ -> Obj_void

let data_equal = {
P.Tools.global_data_equal = (==);
P.Tools.local_data_equal = (==) }

let parse_result = glrParse parsing_device get_value get_name str_token ntn_start data_equal [|fun _ -> true|] (fun _ -> "")
[|Dyp.keep_one|] Data_void 0
ra_list empty_priority_data nt_names
Pgen_lexer.token lexbuf
(fun _ -> (Lexing.dummy_pos,Lexing.dummy_pos))

let header_main, token_list, relation, non_terminal_start, start_type,
  grammar, trailer_main = match fst (List.hd parse_result) with
  | Pgen_input (c1,(b,d,(st,start_type)),g,c2) -> (c1,b,d,st,start_type,g,c2)
  | _ -> failwith "parse tree"
let header_main = header_main^"\n"
let trailer_main = trailer_main^"\n"

module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module String_set = Set.Make(Ordered_string)
module String_map = Map.Make(Ordered_string)


(* string ["a";"b";"c"] returns "[a;b;c]" *)
let string_list sl =
  let aux code s = code^s^";" in
  let code = List.fold_left aux "[" sl in
  let string_length = String.length code in
  (String.sub code 0 (string_length-1))^"]"


let code_token_decl,(*code_export_module,*)token_map =
  let code_token_decl = "type token =" in
  let aux (code_token_decl,token_map) (tok,typ) =
    if typ = "No_type"
    then (code_token_decl^" | "^tok),
      (String_map.add tok typ token_map)
    else (code_token_decl^" | "^tok^" of ("^typ^")"),
      (String_map.add tok typ token_map)
  in
  let code_token_decl,token_map =
    List.fold_left aux (code_token_decl,String_map.empty) token_list
  in
  let code_token_decl = code_token_decl^"\n" in
  code_token_decl,
  (*"module Export_type = \nstruct\n"^
  code_token_decl ^"end\ninclude Export_type\n\n",*)
  token_map



let code_token_name_decl,token_name_map(*, code_number_of_tokens*) =
  let code_token_name_decl = "" in
  let aux (code,n,token_name_map) (tok,_) =
    (code^"let token_name_"^tok^" = "^(string_of_int n)^"\n",
    (n+1),String_map.add tok n token_name_map)
  in
  let code_token_name_decl,n,token_name_map =
    List.fold_left aux (code_token_name_decl,2,String_map.empty) token_list
  in
  (*let code_token_name_decl =
    code_token_name_decl^"let dummy_token_name = "^(string_of_int n)^"\n"
    ^"let token_name_epsilon = "^(string_of_int (n+1))^"\n"
    ^"let token_epsilon = token_name_epsilon\n"
  in*)
  code_token_name_decl^"let token_nb = "^(string_of_int n)^"\n", token_name_map
  (*String_map.add "dummy" n token_name_map,
  "let () = number_of_tokens := "^(string_of_int (n+2))^"\n"*)



let code_token_functions, code_str_token_name =
  let code_token_functions =
    (*"let compare_token_name t1 t2 = Pervasives.compare t1 t2\n"^*)
    "let get_name t = match t with"
  in
  let aux code (tok,typ) =
    if typ = "No_type" then code^" | "^tok^" -> token_name_"^tok
    else code^" | "^tok^" _ -> token_name_"^tok
  in
  let code_token_functions = List.fold_left aux code_token_functions token_list in
  let code_token_functions =
    code_token_functions^"\n"^
    "let str_token t = match t with\n"
  in
  let aux code (tok,typ) =
    if typ = "No_type" then code^" | "^tok^" -> \""^tok^"\""
    else if typ = "int" then code^" | "^tok^" i -> string_of_int i"
    else if typ = "string" then code^" | "^tok^" s -> s"
    else code^" | "^tok^" _ -> \""^tok^"\""
  in
  let code_token_functions =
    (List.fold_left aux code_token_functions token_list)^"\n"
  in
  let code_str_token_name =
    let rec aux token_list = match token_list with
      | (tok,_)::t::l -> "\""^tok^"\";"^(aux (t::l))
      | [(tok,_)] -> "\""^tok^"\""
      | _ -> assert false
    in
    "let token_name_array = [|\"dummy_token\";\"token_epsilon\";"^
    (aux token_list)^"|]\n"^
    "let str_token_name t = token_name_array.(t)\n"
  in
  code_token_functions, code_str_token_name



(*
let code_token_decl = "type token ="
let aux (code_token_decl,token_map) (tok,typ) =
  if typ = "No_type" then (code_token_decl^" | "^tok),(String_map.add tok typ token_map)
  else (code_token_decl^" | "^tok^" of ("^typ^")"),(String_map.add tok typ token_map)
let code_token_decl,token_map = List.fold_left aux (code_token_decl,String_map.empty) token_list
let code_token_decl = code_token_decl^"\n"

let code_token_name_decl = "type token_name ="
let aux code (tok,_) = code^" | token_name_"^tok
let code_token_name_decl = List.fold_left aux code_token_name_decl token_list
let code_token_name_decl = code_token_name_decl^" | token_name_epsilon | token_name_dummy\n"
^"let dummy_token_name = token_name_dummy\n"
^"let token_epsilon = token_name_epsilon\n"

let code_token_functions =
"let compare_token_name t1 t2 = Pervasives.compare t1 t2
let get_name t = match t with"
let aux code (tok,typ) =
  if typ = "No_type" then code^" | "^tok^" -> token_name_"^tok
  else code^" | "^tok^" _ -> token_name_"^tok
let code_token_functions = List.fold_left aux code_token_functions token_list

let code_token_functions = code_token_functions^"\nlet str_token t = match t with\n"
let aux code (tok,typ) =
  if typ = "No_type" then code^" | "^tok^" -> \""^tok^"\""
  else if typ = "int" then code^" | "^tok^" i -> string_of_int i"
  else if typ = "string" then code^" | "^tok^" s -> s"
  else code^" | "^tok^" _ -> \""^tok^"\""
let code_token_functions = List.fold_left aux code_token_functions token_list

let code_token_functions = code_token_functions^"let str_token_name t = match t with\n"
let aux code (tok,_) = code^" | token_name_"^tok^" -> \""^tok^"\""
let code_token_functions = List.fold_left aux code_token_functions token_list
let code_token_functions = code_token_functions^" | token_name_epsilon -> \"epsilon\"\n"
^" | token_name_dummy -> \"dummy\"\n\n"
*)


(*let code_non_terminal_decl = "type non_ter = "
let aux1 st_set ld = match ld with
  | Obj_terminal _ -> st_set
  | Obj_non_terminal (nt,_,_) -> String_set.add nt st_set
let aux2 st_set (lhs_nt,_,ld_list,_) =
  List.fold_left aux1 (String_set.add lhs_nt st_set) ld_list
let non_terminal_set = List.fold_left aux2 String_set.empty grammar
let aux nt_string code = code^" | Non_terminal_name_"^nt_string
let code_non_terminal_decl = String_set.fold aux non_terminal_set code_non_terminal_decl
let code_non_terminal_decl = code_non_terminal_decl^" | Non_terminal_name_startprime\n"*)

let code_non_terminal_decl,non_terminal_set =
  let aux1 st_set ld = match ld with
    | Obj_terminal _ -> st_set
    | Obj_non_terminal (nt,_,_) -> String_set.add nt st_set
  in
  let aux2 st_set (lhs_nt,_,ld_list,_) =
    List.fold_left aux1 (String_set.add lhs_nt st_set) ld_list
  in
  let non_terminal_set = List.fold_left aux2 String_set.empty grammar in
  let aux nt_string (code1,n) =
    code1^"let "^nt_string^" = "^(string_of_int n)^"\n",
    (n+1)
  in
  let code_non_terminal_decl,_ =
    String_set.fold aux non_terminal_set
     ("",1)
  in
  code_non_terminal_decl,
  non_terminal_set



let code_nt_functions =
  let code = "let entry_points = [("^non_terminal_start^",1)]\n"(*^
    "let str_non_terminal nt = nt_names.(nt)\n"*)
  in
  let aux nt_string code = code^";\""^nt_string^"\"" in
  let code_nt_names =
    String_set.fold aux non_terminal_set ""
  in
  let code_nt_names = "let nt_names = [|\"0\""^code_nt_names^"|]\n" in
  code_nt_names^code




let code_main_1 =
"module P = Dyp.Make_dyp(Parser_parameters_module)
open Parser_parameters_module
open P
open P.Parser_PAR
open Dyp
type priority = Dyp.priority
let priority_data, default_priority =
  Dyp.insert_priority Dyp.empty_priority_data \"default_priority\"\n\n"

(*let code_priority_def = "let default_priority = 0\n"*)

let aux (str_set:String_set.t) (p1,p2) =
  String_set.add p2 (String_set.add p1 str_set)
let priority_set = List.fold_left aux String_set.empty relation

let aux1 st_set ld = match ld with
  | Obj_terminal _ -> st_set
  | Obj_non_terminal (_,p,_) -> if p="No_priority" then st_set
      else String_set.add p st_set
let aux2 st_set (_,p,ld_list,_) =
  List.fold_left aux1 (String_set.add p st_set) ld_list
let priority_set = List.fold_left aux2 priority_set grammar

let aux p (st_map,n) = (String_map.add p n st_map,n+1)
let priority_map,_ = String_set.fold aux priority_set (String_map.empty,1)

let priority_map = String_map.add "default priority" 0 priority_map

(*let code_var_list n =
  if n = 0 then "" else
  let rec aux p =
    if n = p then "__dypgen_av_"^(string_of_int p)
    else "__dypgen_av_"^(string_of_int p)^";"^(aux (p+1))
  in
  aux 1*)

let aux (lhs_nt,p,ld_list,ocaml_code) =
  let aux2 code ld = match ld with
    | Obj_terminal ter -> code^"Ter token_name_"^ter^";"
    | Obj_non_terminal (ntn,p,eq) ->
        let code_p =
          if p = "No_priority" then "No_priority "
          else
            (if eq then "Lesseq_priority " else "Less_priority ")^
            (string_of_int (String_map.find p priority_map))
        in
        code^"Non_ter ("^ntn^","^code_p^");"
  in
  (*let code_priority = (string_of_int (String_map.find p priority_map)) in*)
  let code_literal_list = List.fold_left aux2 "" ld_list in
  let string_length = (String.length code_literal_list) in
  let code_literal_list =
    if string_length = 0 then code_literal_list
    else String.sub code_literal_list 0 (string_length-1)
  in
  let code_rule = "("^ lhs_nt^ ",["^ code_literal_list^ "],default_priority)" in

  (*let aux_av (code,n) lit =
    let typ = match lit with
      | Obj_terminal ter -> let typ = String_map.find ter token_map in
          if typ = "No_type" then "unit" else typ
      | Obj_non_terminal (nt,_,_) -> "'"^nt
    in code^"let _"^(string_of_int n)^" = (Obj.obj __dypgen_av_"^
      (string_of_int n)^" : "^typ^") in \n",(n+1)
  in
  let action_variable_code,_ = List.fold_left aux_av ("",1) ld_list in*)

  (*let rule_type = if lhs_nt = non_terminal_start then start_type else "'"^lhs_nt in*)

  let code_var_list =
    let f (code,n) lit = match lit with
      | Obj_terminal ter -> let typ = String_map.find ter token_map in
          if typ = "No_type" then
            code^" _"^(string_of_int n)^";", n+1
          else code^"Obj_"^ter^" _"^(string_of_int n)^";", n+1
      | Obj_non_terminal (nt,_,_) -> code^"Obj_"^nt^" _"^(string_of_int n)^";", n+1
    in
    let c,_ = List.fold_left f ("",1) ld_list in
    let s_length = (String.length c) in
    if s_length = 0 then c
    else String.sub c 0 (s_length-1)
  in

  let code_action = match ocaml_code with
    | Classic_action oc_code ->
        "Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _lld _prd _ -> (match action_variable__l with ["^
        code_var_list^"] -> Obj_"^lhs_nt^" ("^oc_code^
        ") | _ -> failwith \"Invalid number of arguments in action\"),true,false,_data,_dd,_ld,[],[],_prd,None,None)"
    | Full_action oc_code ->
        "Dyp_special_types.Dypgen_action(fun action_variable__l _ _ _data _dd _ld _lld  _prd _ -> (match action_variable__l with ["^
        code_var_list^"] -> "^
        "let ob,b,d = "^oc_code^
        " in ((Obj_"^lhs_nt^" ob),b,false,d,_dd,_ld,[],[],_prd,None,None)\n"^
        " | _ -> failwith \"Invalid number or kind of arguments in action\"))"
  in
  "("^code_rule^","^code_action^")"

let list_code_rapf = List.map aux grammar
let code_grammar =
  let rec aux sl = match sl with
    | [] -> ""
    | [s] -> s
    | s::t -> s^"\n;\n"^(aux t)
  in "let rapf_list = [\n"^(aux list_code_rapf)^"]\n"

(*let code_grammar = code_grammar^"let current_grammar,nt_nb,map_po,user_g = make_grammar \n"*)

let code_prio_data = "let current_priority_data = empty_priority_data\n"
let aux code (p1,p2) =
  let i1 = string_of_int (String_map.find p1 priority_map) in
  let i2 = string_of_int (String_map.find p2 priority_map) in
  code^"let current_priority_data = update_priority current_priority_data [("^
   i1^","^i2^",true)]\n"
let code_prio_data = (List.fold_left aux code_prio_data relation)^"\n"

let code_main_2 = "let parsing_device = create_parsing_device rapf_list empty_priority_data `LR0 0 0 P.Tools.empty_datadyn nt_names (Array.make (Array.length nt_names) 0) priority_names\n"

let code_main_2 = code_main_2^
"let "^non_terminal_start^" f lexbuf =
  let data_equal = {
    P.Tools.global_data_equal = (==);
    P.Tools.local_data_equal = (==) }
  in
  let lexbuf_position lexbuf = (lexbuf.Lexing.lex_curr_p,lexbuf.Lexing.lex_start_p) in
  let pf = glrParse parsing_device __dypgen_get_value get_name str_token "^
  non_terminal_start^
  " data_equal  [|fun _ -> true|] (fun _ -> \"\")
    [|Dyp.keep_one|]
    0 0
    rapf_list empty_priority_data nt_names
    f lexbuf lexbuf_position in
  let aux1 (o,p) = match o with
    | Obj_"^non_terminal_start^
    " r -> (r,p) | _ -> failwith \"Wrong type for entry result\" in
  List.map aux1 pf\n\n"

let parser_param_header = "
module Parser_parameters_module =
struct\n\n"

(*let code_export_module = "module Export_type = \nstruct\n"^ code_token_decl^"end\ninclude Export_type\n\n"*)

let code_type_obj =
  let code_obj =
    let aux nt code =
      if nt = non_terminal_start then code
      else code^" '"^nt^","
    in
    let type_param =(String_set.fold aux non_terminal_set "") in
    let string_length = String.length type_param in
    let type_param = String.sub type_param 0 (string_length-1) in
    "("^type_param^") obj"
  in
  let aux1 tok typ code = if typ = "No_type" then code^"  | Obj_"^tok^"\n"
    else code^"  | Obj_"^tok^" of ("^typ^")\n"
  in
  let aux2 nt code =
    if nt = non_terminal_start then
      code^"  | Obj_"^nt^" of "^start_type^"\n"
    else code^"  | Obj_"^nt^" of '"^nt^"\n"
  in
  "type "^code_obj^" =\n"^
  (String_map.fold aux1 token_map "")^
  (String_set.fold aux2 non_terminal_set "")^
  "type data = Data_void\n"



(*let code_merge_functions =
  let aux nt_string code = code^"let merge_"^nt_string^" ol _ = ol\n" in
  String_set.fold aux non_terminal_set ""

let code_merge_map =
  let code_merge_function_list = string_list merge_function_list in
  "let merge_map = P.Tools.init_merge_map "^
  code_merge_function_list*)
  (*let aux nt_string code = code^"let merge_map = Nt_map.add "^
    nt_string^" merge_"^nt_string^" merge_map\n" in
  "let merge_map = Nt_map.empty\n"^
  (String_set.fold aux non_terminal_set "")*)

(*let code_merge_map =
  let aux2 nt_string nt_int mfl = ("(fun ol o -> (
  let f1 o = match o with "^obj_pref^"Obj_"^nt_string^" ob -> ob
    | _ -> failwith \"type error, bad obj in dyp_merge_"^nt_string^"\"
  in
  let o = f1 o in
  let ol = List.map f1 ol in
  let ol = dyp_merge_"^nt_string^" ol o in
  let f2 o = "^obj_pref^"Obj_"^nt_string^" o in
  List.map f2 ol)),"^(string_of_int nt_int))::mfl
  in
  let merge_function_list = String_map.fold aux2 non_terminal_map [] in
  let code_merge_function_list = string_list merge_function_list in
  (String_set.fold aux1 non_terminal_set "")^
  "let merge_map = P.Tools.init_merge_map "^code_merge_function_list^"\n"
*)


let code_get_value =
  let aux code (tok,typ) =
    let s = if typ = "No_type" then " -> Obj_"^tok^"\n"
      else " x -> Obj_"^tok^" x\n"
    in
    code^"  | "^tok^s
  in
  "let __dypgen_get_value t = match t with\n"^
  (List.fold_left aux "" token_list)^"\n"

let parser_code = parser_param_header^
  (*code_export_module^*)

  code_token_name_decl^
  code_str_token_name^

  code_non_terminal_decl^
  code_nt_functions^

  (*code_parser_param_info^*)
  "let priority_names = [|\"0\"|]"^
  "let merge_warning = false\n"^
  "let undef_nt = true\n"^
  "end\n\n"^

  code_main_1^
  code_token_decl^
  code_token_functions^
  (*code_number_of_tokens^*)
  code_type_obj^
  code_get_value^
  (*"let merge ol _ = ol\n"^*)
  header_main^
  code_grammar^
  code_prio_data^
  (*code_merge_functions^*)
  trailer_main^
  (*code_merge_map^*)
  code_main_2

let parser_code_mli =
  "type priority\n"^
  code_token_decl^"val "^non_terminal_start^
  " : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ("^start_type^" * priority) list\n"

let dest_file = open_out output_file
let dest_file_mli = open_out output_file_mli

let () = output_string dest_file parser_code
let () = output_string dest_file_mli parser_code_mli

