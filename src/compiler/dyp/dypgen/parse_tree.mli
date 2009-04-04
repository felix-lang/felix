type token_desc = string * string
(* 2nd string is for type, if no type is mentioned then the string No_type is chosen *)
type nt_priority_desc = Pr_eq | Pr_less | Pr_lesseq | Pr_greater | Pr_greatereq
type code_desc = string * Lexing.position
type pat_type = Pat_syn of string | Pat_inh of string
type literal_desc =
  | Symb_terminal of (string * (int * int * int * string))
    (* (line,col1,col2,file_name) *)
  | Symb_non_terminal of
     ((string * (int * int * int * string)) *
     (string * (int * int * int * string))
      * nt_priority_desc * code_desc)
  | Symb_non_terminal_NL of
     ((string * (int * int * int * string)) *
     (string * (int * int * int * string))
      * nt_priority_desc * code_desc)
  | Symb_regexp of Dyp.regexp
  | Symb_terminal_NL of (string * (int * int * int * string))
      (* NL = not preceded by layout character *)
  | Symb_regexp_NL of Dyp.regexp
  | Symb_early_action of (string * (Lexing.position * bool))
(* 2nd string for the priority identifier, bool is true=Toeq, bool is false=To *)
(* the last int is the number of arguments, the parser always returns 1 for it. It is used for partial actions, a rule with partial actions is split and new non terminals are created by dypgen. The result of partial action is a (n+1)-tuple if there are n arguments for this action, thus the following partial actions or the action, can access these arguments. The n arguments are the n first values of the (n+1)-tuple and the last value is the value computed by the partial action.*)
type priority_desc = (string * (int * int * int)) (*| Prio_fun of (string * int)*)
type action_desc = string * (Lexing.position * bool)
(* the bool tells whether the code is preceded by '@', i.e. if it returns a list of type dyp_action. *)
type pattern_desc = (string * string * Lexing.position) list
(*
(((action_desc * int) * (string list)) list) is the list of the partial actions, the int is the place of the partial action in the right-hand side.
The string lists after literal_desc and action_desc are patterns. the parser only retuns list of one element, but the processing of patterns of partial actions makes use of the list.
*)
type relation_desc = Rel_list of (string list) | Rel_single of string
type set_desc = string * (string list)
type parser_param_info = {
  token_list : token_desc list;
  relation : relation_desc list;
  start : (string * string) list;
  generic_merge :(string * (string list)) list;
  cons : (string * (string list)) list;
  additional_cons : string list;
  nt_type : (string * string list) list;
  single_nt : string list;
  layout : (Dyp.regexp * code_desc) list
  (*lexbuf_type : string*) }
(*type lexer_def =
  | Regexp_decl of (string * Dyp.regexp)
  | Lexer_rule of (Dyp.regexp * string * code_desc)
type obj = code_desc * code_desc * parser_param_info * (lexer_def list) * (rule_desc list) * code_desc * code_desc * code_desc * code_desc*)

