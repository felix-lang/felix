val dypgen_verbose : int ref
(** by default = 0, gives infos about the parsing if set>0,
  breaks re-entrancy if set>2. *)
(*type token_name = int*)
type 'a nt_prio =
  | No_priority
  | Eq_priority of 'a
  | Less_priority of 'a
  | Lesseq_priority of 'a
  | Greater_priority of 'a
  | Greatereq_priority of 'a
(** This type makes possible to assign precedence to non terminals in
the rhs of rules.
If the non_terminal_priority of the non terminal E in the following 
rule : A -> E  is Less_priority pc1, and that the parser has so far 
reduced a substring to E yielding the priority class pc2 for this
substring, then the parser reduces with A -> E to A only if we have
the relation pc1 -> pc2 in the priority set used to construct the 
parsing_device (see below create_parsing_device).
  The Toeq constructor behaves the same way except that it also 
accepts pc1 for priority class of the substring even if we don't
have pc1 -> pc1 in the priority set. *)

type regexp =
  | RE_Char of char
  | RE_Char_set of (char * char) list
  | RE_Char_set_exclu of (char * char) list
  | RE_String of string
  | RE_Alt of regexp list
  | RE_Seq of regexp list
  | RE_Star of regexp
  | RE_Plus of regexp
  | RE_Option of regexp
  | RE_Name of string (* name of a regexp declared with let *)
  (*| RE_Bind of (regexp * string)*)
  | RE_Eof_char

type symb =
  | Ter of string
  | Ter_NL of string (* no layout char can precede the token *)
  | Non_ter of string * string nt_prio
  | Non_ter_NL of string * string nt_prio
  | Regexp of regexp
  | Regexp_NL of regexp

type rule_options = No_layout_inside | No_layout_follows

(*type rule = string * (symb list) * string * bool*)
type rule = string * (symb list) * string * rule_options list
type nt_cons_map

type ('obj,'gd,'ld) merge_function =
  ('obj * 'gd * 'ld) list -> ('obj list * 'gd * 'ld)

type debug_infos = {
  prt_state : out_channel -> unit;
  prt_grammar : out_channel -> unit; }

type ('t,'o,'gd,'ld,'l) action =
    Dypgen_action of ( 'o list -> (Lexing.position * Lexing.position) ->
      (Lexing.position * Lexing.position) list -> 'gd ->
      'ld -> 'ld -> debug_infos ->
      ('t,'o,'gd,'ld,'l) parser_pilot ->
      (unit -> string list) ->
      ('o * bool * bool *
      'gd * 'ld *
      ((rule * ('t,'o,'gd,'ld,'l) action *
        (int * ('t,'o,'gd,'ld,'l) inherited_val) list) list) *
      (string * string) list *
    (* list of couples (nt,cons) to bind new non terminals to constructors *)
      string list list * (* those are the new relations *)
     (out_channel option) * (out_channel option) *
     ('t,'o,'gd,'ld,'l) parsing_device option) )

and ('t,'o,'gd,'ld,'l) inherited_val =
  'o list -> (Lexing.position * Lexing.position) ->
  (Lexing.position * Lexing.position) list -> 'gd ->
  'ld -> 'ld -> debug_infos ->
  ('t,'o,'gd,'ld,'l) parser_pilot ->
  (unit -> string list) ->
  'o
and ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot = {
  pp_dev : ('token,'obj,'global_data,'local_data,'lexbuf) parsing_device;
  pp_par : ('token,'obj,'global_data,'local_data,'lexbuf) parser_parameters;
  pp_gd : 'global_data;
  pp_ld : 'local_data }
and ('token,'obj,'global_data,'local_data,'lexbuf) parsing_device
and ('token,'obj,'global_data,'local_data,'lexbuf) parser_parameters

type ('token,'obj,'gd,'ld,'l) dypgen_toolbox = {
  parser_pilot : ('token,'obj,'gd,'ld,'l) parser_pilot;
  global_data : 'gd;
  local_data : 'ld;
  last_local_data : 'ld;
  next_lexeme : unit -> string list;
  symbol_start : unit -> int;
  symbol_start_pos : unit -> Lexing.position;
  symbol_end : unit -> int;
  symbol_end_pos : unit -> Lexing.position;
  rhs_start : int -> int;
  rhs_start_pos : int -> Lexing.position;
  rhs_end : int -> int;
  rhs_end_pos : int -> Lexing.position;
  print_state : out_channel -> unit;
  print_grammar : out_channel -> unit;
}

type ('token,'obj,'gd,'ld,'l) dyp_action =
  | Add_rules of
      (rule * (('token,'obj,'gd,'ld,'l) dypgen_toolbox ->
      'obj list -> 'obj * ('token,'obj,'gd,'ld,'l) dyp_action list)) list
  | Bind_to_cons of (string * string) list
  | Global_data of 'gd
  | Keep_grammar
  | Parser of ('token,'obj,'gd,'ld,'l) parsing_device
  | Local_data of 'ld
  | Next_grammar of out_channel
  | Next_state of out_channel
  | Relation of string list list
  | Dont_shift

exception Giveup
(** This exception can be raised by an action, then the parser gives
up the current reduction and the parsing along the current path is
stopped. *)

exception Undefined_nt of string
(** This exception is raised when there is in the grammar a non
terminal that is in a right-hand side but never in a left-hand
side (i.e. it is never defined). The string represents this non
terminal. *)

exception Undefined_ter of string
(** Same as above but for terminals instead. *)

exception Bad_constructor of (string * string * string)
(** This exception is raised when a value is returned by a user action
with a bad constructor (not corresponding to the non terminal). This
can only happen with rules defined dynamically.
1st string is the rule and can be used to be printed.
2nd string is the name of the constructor that should have been used.
3rd string is the name of the constructor that has been used. *)

exception Constructor_mismatch of (string * string)
(** This exception is raised when a nt is added with a constructor cons
but it already exists with another constructor.
1st string is the name of the previous constructor,
2nd string is the name of the constructor one tried to add. *)

exception Syntax_error
(** This exception is raised if the parser is stuck in a
situtation where no shift and no reduction is possible. *)

val keep_all : ('obj,'gd,'ld) merge_function
val keep_one : ('obj,'gd,'ld) merge_function
val dummy_lexbuf_position : 'a -> (Lexing.position * Lexing.position)

module Tools :
sig
  val transform_action :
    (('t,'o,'gd,'ld,'l) dypgen_toolbox ->
    'o list -> 'o * ('t,'o,'gd,'ld,'l) dyp_action list) ->
    'o list -> (Lexing.position * Lexing.position) ->
    (Lexing.position * Lexing.position) list -> 'gd ->
    'ld -> 'ld -> debug_infos ->
    ('t,'o,'gd,'ld,'l) parser_pilot ->
    (unit -> string list) ->
    ('o * bool * bool *
    'gd * 'ld *
    ((rule * ('t,'o,'gd,'ld,'l) action *
      (int * ('t,'o,'gd,'ld,'l) inherited_val) list
      ) list) *
    (string * string) list *
    (* list of couples (nt,cons) to bind new non terminals to constructors *)
    string list list * (* those are the new relations *)
    (out_channel option) * (out_channel option) *
     ('t,'o,'gd,'ld,'l) parsing_device option)
  
  val transform_inh_val :
    (('t,'o,'gd,'ld,'l) dypgen_toolbox -> 'o list -> 'o) ->
    'o list -> (Lexing.position * Lexing.position) ->
    (Lexing.position * Lexing.position) list -> 'gd ->
    'ld -> 'ld -> debug_infos ->
    ('t,'o,'gd,'ld,'l) parser_pilot ->
    (unit -> string list) ->
    'o
  
  val keep_zero : ('obj,'gd,'ld) merge_function
  val array_of_list : 'a list -> 'a array
  val hashtbl_of_array : 'a array -> ('a, int) Hashtbl.t
  val make_nt_cons_map : (string * int) list -> nt_cons_map
end

val print_regexp : regexp -> string

type 'obj dyplexbuf

val lexeme : 'obj dyplexbuf -> string
val lexeme_char : 'obj dyplexbuf -> int -> char
val lexeme_start : 'obj dyplexbuf -> int
val lexeme_end : 'obj dyplexbuf -> int
val lexeme_start_p : 'obj dyplexbuf -> Lexing.position
val lexeme_end_p : 'obj dyplexbuf -> Lexing.position
val flush_input : 'obj dyplexbuf -> unit

val from_string :
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot ->
  string ->
  'obj dyplexbuf

val from_channel :
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot ->
  in_channel ->
  'obj dyplexbuf

val from_function :
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot ->
  (bytes -> int -> int) ->
  'obj dyplexbuf

val dyplex_lexbuf_position :
  'obj dyplexbuf -> (Lexing.position * Lexing.position)

val std_lexbuf : 'obj dyplexbuf -> Lexing.lexbuf
val set_newline : 'obj dyplexbuf -> unit
val set_fname : 'obj dyplexbuf -> string -> unit

val make_parser :
  (rule * ('token,'obj,'global_data,'local_data,'lexbuf) action *
    (int *
    ('token,'obj,'global_data,'local_data,'lexbuf) inherited_val) list
  ) list ->
  string list list -> (* code for the relations between priorities *)
  'global_data ->
  'local_data ->
  nt_cons_map ->
  string list ->
  bool ->
  int ->
  bool ->
  ('token -> 'obj) ->
  ('token -> int) ->
  ('token -> string) ->
  ('global_data -> 'global_data -> bool) ->
  ('local_data -> 'local_data -> bool) ->
  ('obj -> bool) array ->
  ('obj -> string) ->
  string array ->
  (string, int) Hashtbl.t ->
  ('obj,'global_data,'local_data) merge_function array ->
  ('lexbuf -> (Lexing.position * Lexing.position)) ->
  (string * regexp) list ->
  (string * regexp) list * (int * ('obj dyplexbuf -> 'obj)) list ->
  (string * (regexp list * ('obj list -> 'obj dyplexbuf -> 'obj) list)) list ->
  (string * int) list ->
  ('obj dyplexbuf -> 'obj) ->
  bool ->
  
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot

val update_pp :
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot ->
  ('token,'obj,'global_data,'local_data,'lexbuf) dyp_action list ->
  ('token,'obj,'global_data,'local_data,'lexbuf) parser_pilot

val lex :
  string -> (* name of the auxiliary lexer that is called *)
  'obj list ->
  'obj dyplexbuf ->
  'obj
(* lex is meant to be called from user actions of the main
or auxiliary lexers *)

val parse :
  ('token, 'obj,'global_data,'local_data,'lexbuf) parser_pilot ->
  string ->
  ?global_data:'global_data ->
  ?local_data:'local_data ->
  ?match_len:[`longest|`shortest] ->
  ?keep_data:[`both|`global|`local|`none] ->
  ?lexpos:('lexbuf -> (Lexing.position * Lexing.position)) ->
  ?use_rule_order:bool ->
  ?use_all_actions:bool ->
  ('lexbuf -> 'token) ->
  'lexbuf ->
  (('obj * string) list)

val lexparse :
  ('token, 'obj,'global_data,'local_data,'obj dyplexbuf) parser_pilot ->
  string ->
  ?global_data:'global_data ->
  ?local_data:'local_data ->
  ?match_len:[`longest|`shortest] ->
  ?keep_data:[`both|`global|`local|`none] ->
  ?choose_token:[`first|`all] ->
  ?use_rule_order:bool ->
  ?use_all_actions:bool ->
  'obj dyplexbuf ->
  (('obj * string) list)

val log_channel : out_channel ref

val function_free_pdev :
  ('t,'o,'gd,'ld,'lb) parsing_device -> ('t,'o,'gd,'ld,'lb) parsing_device

val import_functions :
  ('t,'o,'gd,'ld,'l) parsing_device ->
  ('t,'o,'gd,'ld,'l) parser_pilot ->
  (rule * (('t,'o,'gd,'ld,'l) dypgen_toolbox ->
      'o list -> 'o * ('t,'o,'gd,'ld,'l) dyp_action list)) list ->
  ('t,'o,'gd,'ld,'l) parsing_device

val is_re_name : ('t,'o,'gd,'ld,'l) parser_pilot -> string -> bool

val version : string
