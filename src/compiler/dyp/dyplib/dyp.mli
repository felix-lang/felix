module type Dyp_parameters_type =
sig
  val str_token_name : int -> string
    (** Makes possible to display relevant error messages about
        tokens' names and rules *)
  val entry_points : (int * int) list
    (** These are the entry points of the grammar
      (with their corresponding dummy_token) *)
  (*val str_non_terminal : int -> string*)
    (** Makes possible to display relevant error messages about non terminals'
        names and rules *)
(*   val priority_names : string array *)
  val merge_warning : bool
    (** If set to true then the parser will emits a warning each time a merge
    happen. *)
  val token_nb : int
    (** The number of terminal symbols in the grammar. *)
  val undef_nt : bool
end
(** Input signature of the functor [Parser] *)


val dypgen_verbose : int ref
(** by default = 0, gives infos about the parsing if set>0,
  breaks re-entrancy if set>2. *)
type token_name = int
type non_ter = int
type 'a pliteral =
  | Ter of token_name
  | Non_ter of 'a
type priority

val default_priority : priority

type non_terminal_priority =
  | No_priority
  | Eq_priority of priority
  | Less_priority of priority
  | Lesseq_priority of priority
  | Greater_priority of priority
  | Greatereq_priority of priority
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

type priority_data
val empty_priority_data : priority_data
val is_relation : priority_data -> priority -> priority -> bool
val insert_priority : priority_data -> string -> (priority_data * priority)
val find_priority : priority_data -> string -> priority

val set_relation : priority_data -> bool -> priority -> priority ->
  priority_data
(** this set p1<p2 true if b=true and false if b=false *)

val update_priority : priority_data -> (priority * priority * bool) list ->
priority_data
  (** [update_priority ps [pc1,pc2,true]]
  adds the binary relation [pc1] -> [pc2] to [ps]
  [update_priority ps [pc1,pc2,false]]
  removes the relation [pc1] -> [pc2] from [ps] if it exists. *)

val add_list_relations : priority_data -> (priority list) -> priority_data
  (** [add_list_relation ps [p1;...;p2]] adds the relations
  [p1:p2],...,[p1:pn],[p2:p3],...,[p2:pn],...,[p(n-1):pn] to ps. *)

type lit = (int * non_terminal_priority) pliteral
type rule = non_ter * (lit list) * priority

type ('obj,'gd,'ld) dypgen_toolbox = {
  global_data : 'gd;
  local_data : 'ld;
  last_local_data : 'ld;
  priority_data : priority_data;
  symbol_start : unit -> int;
  symbol_start_pos : unit -> Lexing.position;
  symbol_end : unit -> int;
  symbol_end_pos : unit -> Lexing.position;
  rhs_start : int -> int;
  rhs_start_pos : int -> Lexing.position;
  rhs_end : int -> int;
  rhs_end_pos : int -> Lexing.position;
  add_nt : string -> string -> non_ter;
  find_nt : string -> non_ter * string;
  print_state : out_channel -> unit;
  print_grammar : out_channel -> unit;
}

type ('obj,'gd,'ld) dyp_action =
  | Global_data of 'gd
  | Local_data of 'ld
  | Priority_data of priority_data
  | Add_rules of
      (rule * (('obj,'gd,'ld) dypgen_toolbox ->
               ('obj list -> 'obj * ('obj,'gd,'ld) dyp_action list))) list
  | Remove_rules of rule list
  | Will_shift of bool
  | Keep_grammar of bool
  | Next_state of out_channel
  | Next_grammar of out_channel

exception Giveup
(** This exception can be raised by an action, then the parser gives
up the current reduction and the parsing along the current path is
stopped. *)

exception Undefined_nt of string
(** This exception is raised when there is in the grammar a non
terminal that is in a right-hand side but never in a left-hand
side (i.e. it is never defined). The string represents this non
terminal. *)

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
(** This exception is raised by glrParse if the parser is stuck in a
situtation where no shift and no reduction is possible. *)

type ('obj,'gd,'ld) merge_function =
  ('obj * 'gd * 'ld) list -> ('obj list * 'gd * 'ld)

val keep_all : ('obj,'gd,'ld) merge_function
val keep_one : ('obj,'gd,'ld) merge_function

module Dyp_special_types :
sig
  type automaton_kind = [ `LR0 | `LALR | `LR1 ]
  type datadyn

  type debug_infos = {
    prt_state : out_channel -> unit;
    prt_grammar : out_channel -> unit;
  }

  type ('obj,'data,'local_data) action =
    Dypgen_action of ( 'obj list -> (Lexing.position * Lexing.position) ->
      (Lexing.position * Lexing.position) list -> 'data -> datadyn ->
      'local_data -> 'local_data -> priority_data -> debug_infos ->
      ('obj * bool * bool *
      'data * datadyn * 'local_data *
      ((rule * ('obj,'data,'local_data) action) list)
      * (rule list) * priority_data * (out_channel option)
      * (out_channel option)) )
  (** Type of the actions bound to rules in the grammar. A classic action
    takes as argument one obj for each symbol in the right hand side
    of its associated rule and returns an obj. This makes possible to
    build an abstract syntax tree, arguments objects being subtrees, or
    to compute values. A dynamic action does the same thing and in
    addition it returns a list of couples (rule,action) to be added to
    the grammar and a list of rules to be removed from the grammar.
    The boolean tells whether the possibility of a shift must be
    regarded or not. data and priority_data are accessible to the
    actions and local_data for the dynamic ones.
    The two (out_channel option) are for displaying the next automaton
    state and next grammar. *)
end


module Make_dyp :
functor (E : Dyp_parameters_type) ->
sig
  module Urule_map : Map.S

  module Tools :
  sig
    val add_nt : string -> string -> Dyp_special_types.datadyn ref -> non_ter
    val find_nt : string -> Dyp_special_types.datadyn -> (non_ter * string)
    val init_datadyn : (string * int * string) list -> string list ->
      Dyp_special_types.datadyn
    (*val init_merge_map : ('obj merge_function * int) list ->
      'obj merge_map*)
    val empty_datadyn : Dyp_special_types.datadyn
    (*val empty_merge_map : 'obj merge_map*)
    type ('global_data,'local_data) data_equal = {
      global_data_equal : 'global_data -> 'global_data -> bool;
      local_data_equal : 'local_data -> 'local_data -> bool }
    val automaton_kind : Dyp_special_types.automaton_kind ref
    val transform_action :
      (('obj,'gd,'ld) dypgen_toolbox ->
        'obj list -> 'obj * ('obj,'gd,'ld) dyp_action list) ->
      ('obj,'gd,'ld) Dyp_special_types.action
    val keep_zero : ('obj,'gd,'ld) merge_function
    val array_of_list : 'a list -> 'a array
  end

  module type Parser_type =
  sig

type ('obj,'data,'local_data) parsing_device

type deb_rhs
type deb_lhs
type ('obj,'data,'local_data) deb_action
type ('obj,'data,'local_data) deb_user_grammar
type deb_item_set
type deb_lit_trans
type deb_lhslists
type deb_non_terminal


type ('gd,'ld) deb_parsing_device = {
  deb_gram_lhs : ((int list) * (int option)) array;
  deb_gram_rhs : deb_rhs array;
  deb_lhs_table : deb_lhs array;
(*   deb_actions : ('obj,'gd,'ld) pdev_action list array; *)
  deb_g_nb : int;
(*   deb_user_g : ('obj,'gd,'ld) pdev_user_grammar; *)
  deb_table : int array array array;
  deb_table_it : deb_item_set array;
  deb_table_lit_trans : deb_lit_trans array;
  deb_lhslists : deb_lhslists;
  deb_po : bool option array array;
  deb_data : 'gd;
  deb_prio : priority_data;
  deb_local_data : 'ld;
  deb_datadyn : Dyp_special_types.datadyn;
  deb_aut_kind : Dyp_special_types.automaton_kind;
  deb_nt_nb : int;
  deb_non_ter_of_ind : int array;
  deb_prio_of_ind : int array;
  deb_nt_of_ind : deb_non_terminal array;
  deb_lhs_of_ind : deb_lhs array;
  deb_str_non_ter : string array;
  deb_cons_of_nt : int array;
  deb_str_prio : string array
}

    val create_parsing_device :
      (rule * ('obj,'data,'local_data) Dyp_special_types.action) list ->
      priority_data -> Dyp_special_types.automaton_kind -> 'data ->
      'local_data ->
      Dyp_special_types.datadyn -> string array -> int array -> string array ->
    (*('obj,'data,'local_data) parsing_device*)
    ('data,'local_data) deb_parsing_device
      (** Returns the parsing_device which parses strings written with
        the input grammar and assuming the relations between priority
        classes which are contained in the input priority data. *)

    (*val update_parsing_device_data :
      ('obj,'data,'local_data) parsing_device ->
      'data -> 'local_data -> ('obj,'data,'local_data) parsing_device*)

    val glrParse : ('data,'local_data) deb_parsing_device ->
          ('token -> 'obj) -> ('token -> int) -> ('token -> string) -> int ->
          ('data,'local_data) Tools.data_equal ->
          ('obj -> bool) array -> ('obj -> string) ->
          ('obj,'data,'local_data) merge_function array ->
          'data -> 'local_data ->
          (rule * ('obj,'data,'local_data) Dyp_special_types.action) list ->
          priority_data -> string array ->
          ('a -> 'token) -> 'a ->
          ('a -> (Lexing.position * Lexing.position)) ->
          (('obj * priority) list)
    (** Given a parsing_device and a list of tokens (the input string),
        [glrParse] returns the list of the parse objects of the input string.
        If there is no ambiguity there is only one object in the list. The
        list may be a forest of abstract syntax trees or a list of computed
        values.
        [int] is the name of the entry point. *)
  end

  module Parser_PIA : Parser_type
  module Parser_PAR : Parser_type
end

