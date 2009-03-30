(*module Prio =
struct
  type priority = int
  let compare_priority p1 p2 = Pervasives.compare p1 p2
end

include Prio
include Dyp.Priority_by_relation.Make (Prio)

let init_priority = 0
let str_prio p = (string_of_int p) *)

(*type data = Data_void
let global_data_equal = (==)
let local_data_equal = (==)*)


let undef_nt = true

let tn_lparen = 0
let tn_rparen = 1
let tn_colon = 2
let tn_percentpercent = 3
let tn_lbrace = 4
let tn_rbrace = 5
let tn_bar = 6
let tn_equal = 7
let tn_EOF = 8
let tn_kwd_token = 9
let tn_kwd_start = 10
let tn_kwd_relation = 11
let tn_kwd_full = 12
let tn_ocaml_code = 13
let tn_ocaml_type = 14
let tn_uident = 15
let tn_lident = 16

let token_nb = 17

(*let compare_token_name t1 t2 = Pervasives.compare t1 t2*)
let token_names = [|"dummy";"epsilon";"(";")";":";"%%";"{";"}";"|";"=";"EOF";"%token";"%start";"%relation";"%full";"ocaml_code";"ocaml_type";"Uident";"Lident"|]
let str_token_name t = token_names.(t)

type token = LPAREN | RPAREN | COLON | PERCENTPERCENT | LBRACE | RBRACE | BAR | EQUAL | EOF | KWD_TOKEN | KWD_START | KWD_RELATION | KWD_FULL | OCAML_CODE of string | OCAML_TYPE of string | UIDENT of string | LIDENT of string


(*type token_assoc = Token_assoc_left | Token_assoc_right | Token_nonassoc | Token_assoc
module Ordered_token_name =
struct
  type t = token_name
  let compare = Pervasives.compare
end
module TN_map = Map.Make(Ordered_token_name)
let token_assoc_map = TN_map.empty*)


let entry_points = [(1,1)]

let nt_names = [|"S'";"start";"parser_param_info";"token_list";"relation";"start_def";"grammar";"bar_opt";"literal_list";"priority";"optional_code"|]

(*let str_non_terminal nt = nt_names.(nt)*)

type token_desc = string * string (* 2nd string is for type, if no type is mentioned then the string No_type is chosen *)
type literal_desc = Obj_terminal of string | Obj_non_terminal of (string * string * bool) (* 2nd string for the priority identifier, bool is true=Toeq, bool is false=To *)
type action_desc = Classic_action of string | Full_action of string
type rule_desc = (string * string * (literal_desc list) * action_desc)
type relation_desc = ((string * string) list)
type parser_param_info = (token_desc list) * relation_desc * (string * string)
(*last string is for start statement *)
let priority_names = [|"0"|]

let merge_warning = false

