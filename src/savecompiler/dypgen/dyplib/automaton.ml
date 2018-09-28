let log_channel = ref stdout
let log_stack_channel = ref stdout

(*let debug_log = open_out "debug_dypgen.txt"*)

let dypgen_verbose = ref 0

let list_append l1 l2 = List.rev_append (List.rev l1) l2
let (@) l1 l2 = list_append l1 l2

type token_name = int

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

include Priority_by_relation

type symb =
  | Ter of string
  | Ter_NL of string (* no layout can precede the token *)
  | Non_ter of string * (string nt_prio)
  | Non_ter_NL of string * (string nt_prio)
  | Regexp of regexp
  | Regexp_NL of regexp

type ('a,'b) psymbol =
  | Ps_Ter of 'b
  | Ps_Ter_NL of 'b
  | Ps_Non_ter of 'a
  | Ps_Non_ter_NL of 'a

type ('lhs,'nt_lit) prule_bis = 'lhs * (('nt_lit,int) psymbol array)
type 'nt_lit p_rhs = ('nt_lit,int) psymbol array

type non_ter = int
type rule_options = No_layout_inside | No_layout_follows
type rule = string * (symb list) * string * rule_options list
type nlit = ((non_ter * (priority nt_prio)),int) psymbol
type nrule = non_ter * (nlit list) * priority * int
(* last int:
first bit = 1 -> layout char inside are allowed
second bit = 1 -> layout char after are allowed  *)

module Ordered_non_ter =
struct
  type t = non_ter
  let compare = Pervasives.compare
end
module Nt_map = Map.Make(Ordered_non_ter)

type ('obj,'gd,'ld) merge_function =
  ('obj * 'gd * 'ld) list -> ('obj list) * 'gd * 'ld

let keep_all l = match l with
  | (_,gd,ld)::_ -> List.map (fun (o,_,_) -> o) l, gd, ld
  | [] -> assert false

let keep_one l = match l with
  | (o,gd,ld)::_ -> [o], gd, ld
  | [] -> assert false

let dummy_lexbuf_position _ = (Lexing.dummy_pos, Lexing.dummy_pos)

module Ordered_int =
struct
  type t = int
  let compare = Pervasives.compare
end
module Int_set = Set.Make(Ordered_int)
module Int_map = Map.Make(Ordered_int)


type lhs = non_ter * priority * int
type lit_nt = non_ter * (priority nt_prio)
let ind_of_lhs (_, _, i) = i
let str_lhs (nt, _, i) str_non_ter = str_non_ter.(nt)


type literal = (lit_nt, int) psymbol
type rule_bis = (lhs, lit_nt) prule_bis
type rhs = lit_nt p_rhs

module Ordered_rhs =
  struct
  type t = rhs * int
   (* the int tells whether the rule allows layout characters
    inside or afterwards (see rule options) *)
  let compare l1 l2 =
    Pervasives.compare l1 l2
end
module Map_rhs = Map.Make (Ordered_rhs)

(** [int] is the dot position in the item *)
type item = (int * int)
type item_rhs = rhs * int

module Ordered_items =
struct
  type t = item
  let compare = Pervasives.compare
end
module Item_map = Map.Make(Ordered_items)

module Ordered_token_name =
struct
  type t = token_name
  let compare = Pervasives.compare
end
module TNS = Set.Make (Ordered_token_name)

module Ordered_intc =
struct
  type t = int * int
  let compare = Pervasives.compare
end
module Intc_map = Map.Make(Ordered_intc)
module Intc_set = Set.Make(Ordered_intc)

module Ordered_predict =
struct
  type t = (non_ter * (priority nt_prio))
  let compare = Pervasives.compare
end
module Predict = Set.Make(Ordered_predict)


  type item_set = {
    mutable reducible : Int_set.t;
    mutable kernel_nt : Intc_set.t;
    mutable kernel_t : Intc_set.t;
    mutable non_kernel : int list;
    mutable predict : Predict.t
  }
  
  let new_item_set () = {
    reducible = Int_set.empty;
    kernel_nt = Intc_set.empty;
    kernel_t = Intc_set.empty;
    non_kernel = [];
    predict = Predict.empty
  }
  let dummy_item_set = {
    reducible = Int_set.empty;
    kernel_nt = Intc_set.empty;
    kernel_t = Intc_set.empty;
    non_kernel = [];
    predict = Predict.empty
  }
  let copy_item_set is = {
    reducible = is.reducible;
    kernel_nt = is.kernel_nt;
    kernel_t = is.kernel_t;
    non_kernel = is.non_kernel;
    predict = is.predict
  }

let is_size = ref []

let avg_list l =
  let w = 1. /. (float_of_int (List.length l)) in
  List.fold_left (fun r x -> r +. w *. x) 0. l

let incr_is_size is =
  let f _ s = s +. 2. in
  let s = Int_set.fold (fun _ s -> s+.1.) is.reducible 0. in
  let s = Intc_set.fold f is.kernel_t s in
  let s = Intc_set.fold f is.kernel_nt s in
  let s = List.fold_left (fun s _ -> s+.1.) s is.non_kernel in
  let s = Predict.fold (fun _ s -> s+.3.) is.predict s in
  is_size := s::!is_size

let size_of_is is =
  let f _ s = s +. 2. in
  let s = Int_set.fold (fun _ s -> s+.1.) is.reducible 0. in
  let s = Intc_set.fold f is.kernel_t s in
  let s = Intc_set.fold f is.kernel_nt s in
  let s = List.fold_left (fun s _ -> s+.1.) s is.non_kernel in
  Predict.fold (fun _ s -> s+.3.) is.predict s


let compare_is is1 is2 =
    let c = Int_set.compare is1.reducible is2.reducible in
    if c<>0 then c else
    let c = Intc_set.compare is1.kernel_t is2.kernel_t in
    if c<>0 then c else
    let c = Intc_set.compare is1.kernel_nt is2.kernel_nt in
    if c<>0 then c else
    let c = Pervasives.compare is1.non_kernel is2.non_kernel in
    if c<>0 then c else Predict.compare is1.predict is2.predict

let soc c = Char.escaped c
let pr_cil l =
  String.concat ";"
  (List.map (fun (a,b) -> "('"^(soc a)^"','"^(soc b)^"')") l)
let pr_pretty_cil l =
  String.concat ""
  (List.map (fun (a,b) ->
    if a=b then "'"^(soc a)^"'" else
   "'"^(soc a)^"'-'"^(soc b)^"'") l)
let rec print_regexp = function
  | RE_Char c -> "Dyp.RE_Char '"^(soc c)^"'"
  | RE_Char_set l -> "Dyp.RE_Char_set ["^(pr_cil l)^"]"
  | RE_Char_set_exclu l -> "Dyp.RE_Char_set_exclu ["^(pr_cil l)^"]"
  | RE_String s -> "Dyp.RE_String \""^(String.escaped s)^"\""
  | RE_Alt rl -> let sl = List.map print_regexp rl in
      "Dyp.RE_Alt ["^(String.concat ";" sl)^"]"
  | RE_Seq rl -> let sl = List.map print_regexp rl in
      "Dyp.RE_Seq ["^(String.concat ";" sl)^"]"
  | RE_Star r -> "Dyp.RE_Star ("^(print_regexp r)^")"
  | RE_Plus r -> "Dyp.RE_Plus ("^(print_regexp r)^")"
  | RE_Option r -> "Dyp.RE_Option ("^(print_regexp r)^")"
  | RE_Name n -> "Dyp.RE_Name \""^n^"\""
  (*| RE_Bind (r,n) -> "Dyp.RE_Bind ("^(print_regexp r)^",\""^n^"\")"*)
  | RE_Eof_char -> "Dyp.RE_Eof_char"

let rec print_pretty_regexp = function
  | RE_Char c -> "'"^(soc c)^"'"
  | RE_Char_set l -> "["^(pr_pretty_cil l)^"]"
  | RE_Char_set_exclu l -> "[^"^(pr_pretty_cil l)^"]"
  | RE_String s -> "\""^(String.escaped s)^"\""
  | RE_Alt rl -> let sl = List.map print_pretty_regexp rl in
      String.concat "|" sl
  | RE_Seq rl -> let sl = List.map print_pretty_regexp rl in
      String.concat " " sl
  | RE_Star r -> "("^(print_pretty_regexp r)^")*"
  | RE_Plus r -> "("^(print_pretty_regexp r)^")+"
  | RE_Option r -> "("^(print_pretty_regexp r)^")?"
  | RE_Name n -> n
  | RE_Eof_char -> "eof"



  let str_literal symb str_non_ter str_ter regexp_array = match symb with
    | Ps_Ter tk -> (try str_ter.(tk) with _ ->
        (Printf.sprintf "<%s>"
        (print_pretty_regexp regexp_array.(tk-(Array.length str_ter)))))
    | Ps_Ter_NL tk -> (try "- "^str_ter.(tk) with _ ->
        (Printf.sprintf "- <%s>"
        (print_pretty_regexp regexp_array.(tk-(Array.length str_ter)))))
    | Ps_Non_ter (nt,_) -> str_non_ter.(nt)
    | Ps_Non_ter_NL (nt,_) -> "- "^str_non_ter.(nt)

  let str_handle litl dp str_non_ter str_ter regexp_array =
    let rec aux i s =
      let s1 = if dp=i then s^"." else s in
      if i=Array.length litl then s1
      else aux (i+1) (s1^(str_literal litl.(i)
        str_non_ter str_ter regexp_array)^" ")
    in
    aux 0 ""

    (*| [] when dp = 0 -> "."
    | [] -> ""
    | t::q -> if dp=0 then ("."^(str_literal t)^" "^(str_handle q (dp-1)))
      else ((str_literal t)^" "^(str_handle q (dp-1))) *)

  let str_token_set tns str_ter =
    let f tn str = str^(try str_ter.(tn) with _ -> (Printf.sprintf "<regexp:%d>" tn))^"," in
    let str = TNS.fold f tns "" in
    if str = "" then "" else
    let string_length = (String.length str) in
    String.sub str 0 (string_length-1)

  (*let print_item ((nt,litl,length),dp) (tns:TNS.t) =
    Printf.fprintf !log_channel "   %s -> %s, (%s) ; length=%d\n" (str_lhs nt)
      (str_handle litl dp) (str_token_set tns) length*)
  let print_reducible chan gram_rhs lhs_table str_non_ter str_ter regexp_array rn =
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs lhs str_non_ter)
      (str_handle rhs (Array.length rhs) str_non_ter str_ter regexp_array)
      (* (str_token_set tns str_ter)*)

  let print_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array (rn,dp) =
    (*Printf.fprintf chan "  ** DEBUG ** rn=%d, lhs_table len=%d, gram_rhs len=%d\n"
      rn (Array.length lhs_table) (Array.length gram_rhs);*)
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs lhs str_non_ter) (str_handle rhs dp str_non_ter str_ter regexp_array)

  let print_non_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array rn =
    print_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array (rn,0)

  let print_item chan nt_of_ind prio_of_ind ind str_non_ter str_ter regexp_array (rhs,dp) =
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs (nt_of_ind.(ind),prio_of_ind.(ind),ind) str_non_ter)
      (str_handle rhs dp str_non_ter str_ter regexp_array)

module Ordered_ntp =
struct
  type t = priority nt_prio
  let compare = Pervasives.compare
end
module Ntp_map = Map.Make(Ordered_ntp)


(** this type is used to construct the automaton. Each state in the automaton
    has a field of type [lit_trans] which is the literal of transition to
    this state. The difference with the type [literal] is that there is no
    priority attached to the non terminals. *)
type lit_trans = (non_ter, int) psymbol
(*  | Ter of token_name
  | Ps_Non_ter of non_ter*)

let lit_trans (symb:(lit_nt,int) psymbol) = match symb with
  | Ps_Non_ter (nt, _) -> Ps_Non_ter nt
  | Ps_Ter t -> Ps_Ter t
  | Ps_Non_ter_NL (nt, _) -> Ps_Non_ter_NL nt
  | Ps_Ter_NL t -> Ps_Ter_NL t

module Ordered_lit_trans=
struct
  type t = lit_trans
  let compare = Pervasives.compare
end

module Map_lit_trans = Map.Make(Ordered_lit_trans)

module Ordered_item_set =
struct
  type t = item_set * int (* int is the number of items : This shouldn't be of any help, get rid of it *)
  let compare (is1,n1) (is2,n2) =
    if n1>n2 then 1
    else if n2>n1 then -1 else
    let c = Int_set.compare is1.reducible is2.reducible in
    if c<>0 then c else
    let c = Intc_set.compare is1.kernel_nt is2.kernel_nt in
    if c<>0 then c else
    Intc_set.compare is1.kernel_t is2.kernel_t
    (*else Pervasives.compare
      (is1.reducible,is1.kernel_nt,is1.kernel_t)
      (is2.reducible,is2.kernel_nt,is2.kernel_t)*)
end

module Map_is = Map.Make(Ordered_item_set)


  type state = {
      number : int;
      li : lit_trans;
      items : item_set;
      mergeable : bool;
      bestowing_nt : int option;
      mutable succ_states : (state * priority) list;
      (*mutable succ_states : (state * priority) list;*)
      (*mutable pred_states : State.State_set.t Li_map.t*)
    }
  module Ordered_States =
  struct
    type t = state
    let compare s1 s2 = Pervasives.compare s1.number s2.number
      (*compare_is s1.items s2.items*)
  end
  module State_set = Set.Make(Ordered_States)

  type debug_infos = {
    prt_state : out_channel -> unit;
    prt_grammar : out_channel -> unit; }

let size_of_state_list l =
  List.fold_left (fun s v -> s +. (size_of_is v.items)) 0. l


module Dyp_special_types =
struct
  type is_trace =
    ((state Map_is.t) array * (state Map_is.t) array) *
     ((state Map_is.t) array * (state Map_is.t) array)
  type array_nt_prio = int Prio_map.t array
end
open Dyp_special_types

type nt_cons_map = int String_map.t

(*type lex_pos = {
  mutable dyplex_start_pos : int;
  mutable dyplex_curr_pos : int }*)

type node = {
  id : int;
  mutable trans : ((int * int) list * node) list;
  mutable eps : node list;
  matched : Int_set.t }
  (* the couple (256,256) represents eof *)


(*type node = {
  id : int;
  mutable trans : ((int * int) list * node) list;
  mutable eps : (node * int) list;
  (* int is the id of the tag, -1 if no tag *)
  matched : Int_set.t }
  (* the couple (256,256) represents eof *)*)

type lex_table = {
  tbl_trans : int array;
  tbl_final : int list array;
  tbl_notrans : bool array }

(*type next_lexeme = NL_Match of (string * bool) | NL_Nomatch | NL_Eof*)
(* The bool tells whether the next lexeme was matched with or without layout characters before it. *)

type bool_option = True | False | NA

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

and ('token,'obj,'data,'local_data,'lexbuf) parser_parameters = {
  merge_warning : bool;
  undef_nt : bool;
  get_value : 'token -> 'obj;
  get_name : 'token -> int;
  str_token : 'token -> string;
  global_data_equal : 'data -> 'data -> bool;
  local_data_equal : 'local_data -> 'local_data -> bool;
  find_rightSib_global_data_equal : 'data -> 'data -> bool;
  find_rightSib_local_data_equal : 'local_data -> 'local_data -> bool;
  test_cons : ('obj -> bool) array;
  str_cons : 'obj -> string;
  cons_str : string array;
  cons_table : (string, int) Hashtbl.t;
  merge_array : ('obj,'data,'local_data) merge_function array;
  lexbuf_position_fun : 'lexbuf -> (Lexing.position * Lexing.position);
  regexp_fun : 'obj dyplexbuf -> 'obj;
  use_dyplex : bool;
  use_rule_order : bool;
  use_all_actions : bool;
  main_lexer_action_nb : int (* tells the number of actions in the initial main lexer (excluding those corresponding to regexps in grammar rules) *)
  }

and ('t,'o,'gd,'ld,'l) parsing_device = {
  ra_list :
    (rule * ('t,'o,'gd,'ld,'l) action *
      (int * ('t,'o,'gd,'ld,'l) inherited_val) list)
    list;
  gram_lhs : ((int list) * (int option)) array;
  gram_rhs : rhs array;
  gram_parnt : (int * int) list array;
    (* one index by rule, the list contains the places in the rhs where
    a parameterized nt lies (a nt that sends down an inherited attribute)
    and the ids of its associated inherited value *)
  lhs_table : lhs array;
  bnt_array : bool array;
   (* tells whether a nt is "bestowing" values i.e. parameterized *)
  (*dypgen_epsilon : int;
    (* the id of the non terminal dypgen__epsilon *)*)
  actions : ('t,'o,'gd,'ld,'l) action list array;
  inherited :
    (('t,'o,'gd,'ld,'l) inherited_val list * int * int * int * bool) array;
  (* 1st int is the number of synthesized attributes that arguments
  i.e. the number of symbols preceding the inherited att in the rhs,
  the 2nd int is the nt of the lhs in case it is parameterized,
  the 3rd int is the nt that is using this action.
  the bool tells whether the first symbol in the rhs is dypgen__epsilon. *)
  entry_point : int;
  entry_points : (int, state) Hashtbl.t; (* this field is redundant with stations *)
  g_nb : int;
   (* the id of the grammar of the parser *)
  lex_nb : int;
   (* the id of the main lexer *)
  nt_table : (string, int) Hashtbl.t;
  stations : state option array;
  state_list : state list;
  is_trace : Dyp_special_types.is_trace;
  (*array_nt_prio : Dyp_special_types.array_nt_prio;*)
  st_nb : int;
  table : (int, int) Hashtbl.t array array;
  (* 1st index for state
  2nd index: 0 for terminals, 1 for non terminals
  3rd index to choose which terminal or nt. *)
  table_it : item_set array;
  state_is_mergeable : bool array;
  state_bnt : int option array; (*Int_set.t array;*)
  (* for each state, id of the action to produce inherited attributes. *)
  (*table_lex_trans : bool array;*)
  (* index: (state id * nb of ter) + ter id,
  says whether a transition with this terminal exists.
  to be improved : an int array *)
  table_lit_trans : lit_trans array;
  (*lhslists : lhslists;*)
  r_L : int list array;
  (*po : bool option array array;*)
  (*po : bool array array;*)
  po : ((int * int), bool) Hashtbl.t ;
    (** Partial order between non terminal name couples. A couple of non
     terminals is bound to a bool. This may be implemented alternatively
     with a set : the presence of a couple (nt1,nt2) in the set would
     denote that nt1<=nt2 is true. This would save memory space. *)
  cyclic_rules : int list array;
    (* For each rule tells whether the rule is cyclic. If the rule is not
    cyclic then the list is empty, otherwise it contains the places of the
    non terminals in the rhs which make the rule cyclic. *)
  (*data : 'gd;*)
  (*loc_data : 'ld;*)
  (*prio : priority_data;*)
  token_nb : int;
  nt_nb : int;
    (* number of non terminals in the internal grammar
     (as opposed to the user grammar. *)
  (*prio_of_ind : int array;
  nt_of_ind : non_ter array;*)
  (*lhs_of_ind : lhs array;*)
  str_non_ter : string array;
  str_non_ter_prio : (string * string) array;
  cons_of_nt : int array;
  relations : string list list;
  nt_cons_map : nt_cons_map;
  rn_of_rule : (rule, int) Hashtbl.t;
  (*regexp_decl :
   (string, (node -> int -> int -> node * int * int)) Hashtbl.t;*)
  regexp_decl : (string, (node -> int -> node * int)) Hashtbl.t;
  regexp_decl_list : (string * regexp) list;
  (* regexp_decl_list is useful to generate regexp_decl again in
  import_closures *)
  main_lexer_start : node;
  main_lexer_table : lex_table;
  main_lexer_actions : ('o dyplexbuf -> 'o) array;
  main_lexer_ter_id : int array;
  (* the int is the id of the terminal that is produced *)
  main_lexer_init_id : int;
  (* the id of the first action that is in the initial main lexer, it is useful in import_closures *)
  aux_lexer : 'o aux_lexer;
  str_ter : string array;
  ter_table : (string, int) Hashtbl.t;
  layout_id : int;
  regexp_table : (regexp, int) Hashtbl.t;
  (*regexp_actions : (int * ('o dyplexbuf -> 'o)) array;*)
  regexp_array : regexp array;
  rule_options : int array;
  (* tells for each rule whether it allows layout characters inside
   or afterward *)
  nt_ntl_array : Int_set.t array;
    (* for each nt(p) gives the set of all the implicit non terminals
    that accept nt(p) *)
  implicit_rule : bool array;
  left_rec_rule : bool array;
}

and ('t,'o,'gd,'ld,'l) parser_pilot = {
  pp_dev : ('t,'o,'gd,'ld,'l) parsing_device;
  pp_par : ('t,'o,'gd,'ld,'l) parser_parameters;
  pp_gd : 'gd;
  pp_ld : 'ld }

and 'o aux_lexer = {
  aux_lexer_start : (string, node) Hashtbl.t;
  aux_lexer_table : (string, lex_table) Hashtbl.t;
  aux_lexer_actions : (string, ('o list -> 'o dyplexbuf -> 'o) array) Hashtbl.t
}

and 'o dyplexbuf = {
  lb_lexbuf : Lexing.lexbuf;
  lb_aux_lex : 'o aux_lexer }


type ('t,'o,'gd,'ld,'l) grammar = ((('t,'o,'gd,'ld,'l) action) list Map_rhs.t) array

let dummy_inherited _ _ _ _ _ _ _ _ _ = assert false


(** STRING functions used to print the states of the automaton *)


let str_literal_trans symb str_non_ter str_ter = match symb with
  | Ps_Ter tk -> if tk= -1 then "#station" else
      (try str_ter.(tk) with _ -> (Printf.sprintf "<regexp:%d>" tk))
  | Ps_Ter_NL tk -> if tk= -1 then assert false else
      (try "- "^str_ter.(tk) with _ -> (Printf.sprintf "- <regexp:%d>" tk))
  | Ps_Non_ter nt -> str_non_ter.(nt)
  | Ps_Non_ter_NL nt -> "- "^str_non_ter.(nt)

let rec str_tok_list ll str_non_ter str_ter regexp_array = match ll with
  | [] -> ""
  | [tok] -> str_literal (Ps_Ter tok) str_non_ter str_ter regexp_array
  | tok::tl -> (str_literal (Ps_Ter tok) str_non_ter str_ter regexp_array)^","^
      (str_tok_list tl str_non_ter str_ter regexp_array)


let str_state_succ succ_states str_non_ter str_ter =
  let f str (state,prio) =
    str^" ["^(string_of_int state.number)^","^
    (str_literal_trans state.li str_non_ter str_ter)^"]"
  in
  List.fold_left f "" succ_states




let print_item_set chan is gram_rhs lhs_table str_non_ter str_ter regexp_array =
  Printf.fprintf chan "  nb of items: %d\n"
  ((Int_set.cardinal is.reducible)+(Intc_set.cardinal is.kernel_nt)+
  (Intc_set.cardinal is.kernel_t)+(List.length is.non_kernel));
  Int_set.iter
    (print_reducible chan gram_rhs lhs_table str_non_ter str_ter regexp_array)
    is.reducible;
  Intc_set.iter
    (print_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array)
    is.kernel_t;
  Intc_set.iter
    (print_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array)
    is.kernel_nt;
  List.iter
    (print_non_kernel chan gram_rhs lhs_table str_non_ter str_ter regexp_array)
    is.non_kernel


let str_predict predict str_non_ter =
  Predict.fold
  (fun (nt,p) str ->
    str^(Printf.sprintf "(%s:%d)" str_non_ter.(nt) nt))
  predict ""



let print_state s gram_rhs lhs_table (*nt_of_ind prio_of_ind*) str_non_ter str_ter regexp_array =
  Printf.fprintf !log_channel " State %d\n" s.number;
  Printf.fprintf !log_channel "  li : %s\n"
    (str_literal_trans s.li str_non_ter str_ter);
  Printf.fprintf !log_channel "  items :\n";
  print_item_set !log_channel s.items gram_rhs lhs_table
    str_non_ter str_ter regexp_array;
  Printf.fprintf !log_channel "  next states : %s\n"
    (str_state_succ s.succ_states str_non_ter str_ter);
  Printf.fprintf !log_channel "  predict : %s\n"
    (str_predict s.items.predict str_non_ter)

let rec str_lit_list litl str_non_ter str_ter regexp_array = match litl with
  | [] -> ""
  | symb::tl -> (str_literal symb str_non_ter str_ter regexp_array)^" "^
      (str_lit_list tl str_non_ter str_ter regexp_array)

let str_rule (nt,litl,_) str_non_ter str_ter regexp_array =
  (str_lhs nt str_non_ter)^" -> "^
  (str_lit_list litl str_non_ter str_ter regexp_array)

let rec print_rule_list rl str_non_ter str_ter regexp_array = match rl with
  | [] -> ()
  | r::t -> Printf.fprintf !log_channel " %s\n"
    (str_rule r str_non_ter str_ter regexp_array);
     print_rule_list t str_non_ter str_ter regexp_array


let print_map m gram_rhs lhs_table str_non_ter str_ter regexp_array =
  let f (is,_) s =
    let () = Printf.fprintf !log_channel "state %d\n" (s.number) in
    print_item_set !log_channel is gram_rhs lhs_table str_non_ter str_ter
      regexp_array
  in
  Map_is.iter f m


let countst = ref 0
(** This ref counts the number of state creations. *)
let count_trans = ref 0
(** This ref counts the number of transitions between states. *)

let closure_v0_LR0 (is:item_set) gram_rhs gram_lhs nt_to_add non_kernel_array =
  let g nk rn =
    if non_kernel_array.(rn) then nk
    else (non_kernel_array.(rn) <- true; rn::nk)
  in
  let f ind (nk, red) =
    match gram_lhs.(ind) with
      | rn_l, None ->
          List.fold_left g nk rn_l, red
      | rn_l, (Some rn) ->
          List.fold_left g nk rn_l,
          Int_set.add rn red
  in
  let non_kernel, reducible =
    Int_set.fold f nt_to_add (is.non_kernel, is.reducible) in
  List.iter (fun rn -> non_kernel_array.(rn) <- false) non_kernel;
  is.reducible <- (*Int_set.union is.reducible*) reducible;
  is.non_kernel <- (*Int_set.union is.non_kernel*) non_kernel(*;
  is.nt_to_add <- Int_set.empty*)



let lit_of_symb s = match s with
  | Ps_Non_ter (nt,_,_) -> Ps_Non_ter nt
  | Ps_Ter t -> Ps_Ter t
  | Ps_Non_ter_NL (nt,_,_) -> Ps_Non_ter_NL nt
  | Ps_Ter_NL t -> Ps_Ter_NL t



let ga_set a i v =
  try !a.(i) <- v
  with Invalid_argument e ->
    if i<0 then raise (Invalid_argument e) else
    let len = Array.length !a in
    if i<2*len then
      (let b = Array.init (2*len) (fun j -> if i<len then !a.(j) else false) in
      b.(i) <- v; a := b)
    else
      (let b = Array.init (i+1) (fun j -> if i<len then !a.(j) else false) in
      b.(i) <- v; a := b)



let ga_get a i =
  try !a.(i) with Invalid_argument e ->
  if i<0 then raise (Invalid_argument e) else false



let rec list_exists p = function
  | a::t -> if p a then true else list_exists p t
  | [] -> false



let is_mergeable is gram_parnt =
  let f (rn,dp) = list_exists (fun (i, _) -> dp<i) gram_parnt.(rn) in
  if Intc_set.exists f is.kernel_nt then false else
  if Intc_set.exists f is.kernel_t then false else true



let print_bnt bnt =
  print_endline "bestowing nt :";
  Int_set.iter (fun i -> Printf.printf "%d " i) bnt;
  print_newline ()



let make_bestowing_nt is gram_parnt =
  
  let f (rn,dp) bnt =
    try
      let inh = List.assoc (dp+1) gram_parnt.(rn) in
      Some inh
    with Not_found -> bnt
  in
  
  Intc_set.fold f is.kernel_nt None



let close_state is_trace_tok is_trace_tok_nl is_trace_nt is_trace_nt_nl succ_states_array (*prd_nb*) gram_rhs gram_lhs gram_parnt (*lhslists prio_dat array_nt_prio*) r_L non_kernel_array symb (*prio*) (vl, succ_states_list) is =
  
  count_trans := !count_trans+1;
  let it_nb =
    (Int_set.cardinal is.reducible)
    + (Intc_set.cardinal is.kernel_nt)
    + (Intc_set.cardinal is.kernel_t)
  in
  let old_reducible = is.reducible in
  (* epsilon rules may be added to reducible by closure_LR0 *)
  try
    let v1 = match symb with
      | Ps_Ter t -> Map_is.find (is, it_nb) is_trace_tok.(t)
      | Ps_Non_ter (nt, _, _) -> Map_is.find (is, it_nb) is_trace_nt.(nt)
      | Ps_Ter_NL t -> Map_is.find (is, it_nb) is_trace_tok_nl.(t)
      | Ps_Non_ter_NL (nt, _, _) -> Map_is.find (is, it_nb) is_trace_nt_nl.(nt)
    in
    let succ_states_list =
      if ga_get succ_states_array (v1.number) then succ_states_list
      else (ga_set succ_states_array (v1.number) true;
        (v1, 0)::succ_states_list)
    in
    (*s.succ_states <- Succ_states.add (v1,prio) s.succ_states;*)
    vl, succ_states_list
  with Not_found ->
    (*countst := (!countst+1);*)
    let f1 (rn, dp) (nt_to_add, predict) = match gram_rhs.(rn).(dp) with
      | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
          (*let lhs_l =
            comp_lhslist nt lhslists prio_dat array_nt_prio
          in*)
          let g1 nt_to_add ind =
            Int_set.add ind nt_to_add
          in
          let g2 nt_to_add (nt, _) =
            let ind_list = r_L.(nt) in
            List.fold_left g1 nt_to_add ind_list
          in
          (g2 nt_to_add nt), Predict.add nt predict
      | _ -> assert false
    in
    let nt_to_add, predict =
      Intc_set.fold f1 is.kernel_nt (Int_set.empty,Predict.empty)
    in
    let g nk rn =
      if non_kernel_array.(rn) then nk
      else (non_kernel_array.(rn) <- true; rn::nk)
    in
    let f ind (nk, red) = match gram_lhs.(ind) with
      | rn_l, None ->
          List.fold_left g nk rn_l, red
      | rn_l, (Some rn) ->
          List.fold_left g nk rn_l,
          Int_set.add rn red
    in
    let non_kernel, reducible =
      Int_set.fold f nt_to_add ([], is.reducible)
    in
    List.iter (fun rn -> non_kernel_array.(rn) <- false) non_kernel;
    let predict =
      List.fold_left
      (fun predict rn -> try (match gram_rhs.(rn).(0) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt -> Predict.add nt predict
        | _ -> predict)
        with Invalid_argument _ -> assert false)
      predict non_kernel
    in
    is.predict <- predict;
    is.reducible <- reducible;
    is.non_kernel <- non_kernel;
    let v1 = {
      li = lit_of_symb symb;
      items = is;
      number = !countst;
      mergeable = is_mergeable is gram_parnt;
      bestowing_nt = make_bestowing_nt is gram_parnt;
      succ_states = [] }
    in
    (*Printf.printf "state: %d\n" v1.number;
    print_bnt v1.bestowing_nt;*)
    incr countst;
    let old_is = { is with reducible = old_reducible } in
    let () = match symb with
      | Ps_Ter t -> is_trace_tok.(t) <-
          Map_is.add (old_is,it_nb) v1 is_trace_tok.(t)
      | Ps_Non_ter (nt,_,_) -> is_trace_nt.(nt) <-
          Map_is.add (old_is,it_nb) v1 is_trace_nt.(nt)
      | Ps_Ter_NL t -> is_trace_tok_nl.(t) <-
          Map_is.add (old_is,it_nb) v1 is_trace_tok_nl.(t)
      | Ps_Non_ter_NL (nt,_,_) -> is_trace_nt_nl.(nt) <-
          Map_is.add (old_is,it_nb) v1 is_trace_nt_nl.(nt)
    in
    ga_set succ_states_array (v1.number) true;
    v1::vl, (v1, 0)::succ_states_list



let aux_move_nt array_lt_nt (is_list, lt_list) (nt, _) =
  (*let nt, _, ind = lhs in*)
  match array_lt_nt.(nt) with
    | None ->
        let is = new_item_set () in
        array_lt_nt.(nt) <- Some is;
        is::is_list, (Ps_Non_ter (nt, 0, nt))::lt_list
    | Some is -> is::is_list, lt_list



let aux_move_nt_nl array_lt_nt_nl (is_list, lt_list, nl_list) (nt, _) =
  (*let (nt,_,ind) = lhs in*)
  match array_lt_nt_nl.(nt) with
    | None ->
        let is = new_item_set () in
        array_lt_nt_nl.(nt) <- Some is;
        let symb = Ps_Non_ter_NL (nt, 0, nt) in
        let nl_list = (symb,is)::nl_list in
        is::is_list, symb::lt_list, nl_list
    | Some is -> is::is_list, lt_list, nl_list



(*let splitting_symbol s gram_rhs bnt_array splitting_ter splitting_nt =
  let splitting_symb (rn, dp) l =
    if dp+1 >= Array.length gram_rhs.(rn) then l else
    match gram_rhs.(rn).(dp+1) with
    | Ps_Ter _ | Ps_Ter_NL _ -> l
    | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_) when bnt_array.(nt) ->
      (match gram_rhs.(rn).(dp) with
      | Ps_Ter t | Ps_Ter_NL t -> splitting_ter.(t) <- true; t::l
      | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_) ->
          splitting_nt.(nt) <- true; nt::l)
    | _ -> l
  in
  let spl_symb_nk (t_l, nt_l) rn =
    match gram_rhs.(rn).(0) with
    | Ps_Ter _ | Ps_Ter_NL _ -> splitting_symb (rn,0) t_l, nt_l
    | Ps_Non_ter _ | Ps_Non_ter_NL _ -> t_l, splitting_symb (rn,0) nt_l
  in
  
  let spl_ter_l = Intc_set.fold splitting_symb s.items.kernel_t [] in
  let spl_nt_l = Intc_set.fold splitting_symb s.items.kernel_nt [] in
  List.fold_left spl_symb_nk (spl_ter_l, spl_nt_l) s.items.non_kernel*)



let add_items_to_nl_state array_lt_ter array_lt_nt nl_list =
  let add_to_is is0 = function
    | None -> ()
    | Some is1 ->
        is0.kernel_nt <- Intc_set.union is0.kernel_nt is1.kernel_nt;
        is0.kernel_t <- Intc_set.union is0.kernel_t is1.kernel_t;
        is0.reducible <- Int_set.union is0.reducible is1.reducible
  in
  List.iter (function
    | (Ps_Ter_NL t), is0 -> add_to_is is0 array_lt_ter.(t)
    | (Ps_Non_ter_NL (_,_,ind)), is0 -> add_to_is is0 array_lt_nt.(ind)
    | _ -> assert false) nl_list


let succ_states_count = ref 0

(* FIXME array_lhs is still not used, it should be used to avoid calling
Int_set.add too many times in f1. Then we could give up the Int_set for a list.*)
let move_LR0 s is_trace gram_rhs gram_lhs (gram_parnt:(int*int) list array) bnt_array r_L (*prio_dat array_nt_prio lhslists*) array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array non_kernel_array =
  (*output_string !log_channel "move_LR0 called\n";*)
  
  let (is_trace_tok, is_trace_tok_nl), (is_trace_nt, is_trace_nt_nl) =
    is_trace in
  
  let f1 (rn, dp) (lt_list, nl_list) =
    let rhs = gram_rhs.(rn) in
    let (is_list, lt_list), nl_list = match rhs.(dp) with
      | (Ps_Ter t) as symb -> (match array_lt_ter.(t) with
        | None ->
            let is = new_item_set () in
            array_lt_ter.(t) <- Some is;
            ([is], symb::lt_list), nl_list
        | Some is -> ([is], lt_list), nl_list)
      | Ps_Non_ter nt ->
          (*let lhslist = comp_lhslist nt lhslists prio_dat array_nt_prio in*)
          (*Printf.fprintf !log_channel "lhslist length = %d\n"
          (List.length lhslist);*)
          (aux_move_nt array_lt_nt) ([], lt_list) nt, nl_list
      | (Ps_Ter_NL t) as symb -> (match array_lt_ter_nl.(t) with
        | None ->
            let is = new_item_set () in
            array_lt_ter_nl.(t) <- Some is;
            let nl_list = (symb, is)::nl_list in
            ([is], symb::lt_list), nl_list
        | Some is -> ([is], lt_list), nl_list)
      | Ps_Non_ter_NL nt ->
          (*let lhslist = comp_lhslist nt lhslists prio_dat array_nt_prio in*)
          let a, b, c =
            (aux_move_nt_nl array_lt_nt_nl)
              ([], lt_list, nl_list) nt
          in (a, b), c
    in
    List.iter
      (if dp+1 = Array.length rhs then
        (fun is -> is.reducible <- Int_set.add rn is.reducible)
      else
      match rhs.(dp+1) with
        | Ps_Ter _ | Ps_Ter_NL _ ->
            (fun is -> is.kernel_t <- Intc_set.add (rn, dp+1) is.kernel_t)
        | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
            (fun is -> (is.kernel_nt <- Intc_set.add (rn, dp+1) is.kernel_nt)))
      is_list;
    lt_list, nl_list
  in
  
  let lt_list, nl_list = Intc_set.fold f1 s.items.kernel_nt ([], []) in
  let lt_list, nl_list = Intc_set.fold f1 s.items.kernel_t (lt_list, nl_list) in
  let f3 (lt_list, nl_list) rn = f1 (rn, 0) (lt_list, nl_list) in
  let lt_list, nl_list =
    List.fold_left f3 (lt_list, nl_list) s.items.non_kernel in
  
  (*Printf.fprintf !log_channel "lt_list length = %d\n" (List.length lt_list);*)
  
  add_items_to_nl_state array_lt_ter array_lt_nt nl_list;
  
  (*let prd_nb = prio_dat.prd_nb in*)
  List.iter (fun (s, _) -> ga_set succ_states_array (s.number) true)
    s.succ_states;
  
  let f2 (vl, succ_states_list) symb =
    
    let is_opt = match symb with
      | Ps_Ter t -> array_lt_ter.(t)
      | Ps_Non_ter (nt, _, _) -> array_lt_nt.(nt)
      | Ps_Ter_NL t -> array_lt_ter_nl.(t)
      | Ps_Non_ter_NL (nt, _, _) -> array_lt_nt_nl.(nt)
    in
    
    match is_opt with Some is ->
      close_state
      is_trace_tok is_trace_tok_nl is_trace_nt is_trace_nt_nl
      succ_states_array (*prd_nb*) gram_rhs gram_lhs gram_parnt (*lhslists*)
      (*prio_dat array_nt_prio*) r_L non_kernel_array symb (*prio*)
      (vl, succ_states_list) is
    | None -> assert false
  in
  let vl, succ_states_list = List.fold_left f2 ([], s.succ_states) lt_list in
  s.succ_states <- succ_states_list;
  (*succ_states_count := !succ_states_count + (List.length succ_states_list);*)
   (* ^ debugging ^ *)
  List.iter (fun (s, _) -> !succ_states_array.(s.number) <- false)
    s.succ_states;
  let clear_array symb = match symb with
    | Ps_Ter t -> array_lt_ter.(t) <- None
    | Ps_Non_ter (nt, _, _) -> array_lt_nt.(nt) <- None
    | Ps_Ter_NL t -> array_lt_ter_nl.(t) <- None
    | Ps_Non_ter_NL (nt, _, _) -> array_lt_nt_nl.(nt) <- None
  in
  List.iter clear_array lt_list;
  vl




let init_is gram_rhs rule_list r_L (*array_nt_prio lhslists prio_dat*) non_kernel_array =
  (*let non_ter_of_lit nt_lit = non_ter_of_nt (nt_of_lit nt_lit) in*)
  let is = new_item_set () in
  let aux (non_kernel, nt_to_add) rn =
  (*print_endline ("> "^(string_of_int (Array.length gram_rhs.(rn))));
  print_endline ("> "^(str_handle gram_rhs.(rn) 0));*)
    let non_kernel =
      if non_kernel_array.(rn) then non_kernel
      else (non_kernel_array.(rn) <- true; rn::non_kernel)
    in
    (*is.non_kernel <- Int_set.add rn is.non_kernel;*)
    try (match gram_rhs.(rn).(0) with
      | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
        let g1 nt_to_add ind =
          Int_set.add ind nt_to_add
        in
        let g2 nt_to_add ind =
          let ind_list = r_L.(ind) in
          List.fold_left g1 nt_to_add ind_list
        in
        non_kernel, g2 nt_to_add (fst nt)
      | _ -> non_kernel,nt_to_add)
    with Invalid_argument _ -> assert false
  in
  
  let non_kernel, nt_to_add =
    List.fold_left aux ([], Int_set.empty) rule_list
  in
  is.non_kernel <- non_kernel;
  is, nt_to_add



let merge_non_kernel nk1 nk2 non_kernel_array =
  let f nk rn =
    if non_kernel_array.(rn) then nk
    else (non_kernel_array.(rn) <- true; rn::nk)
  in
  let g rn = non_kernel_array.(rn) <- false in
  let nk = List.fold_left f [] nk1 in
  let nk = List.fold_left f nk nk2 in
  List.iter g nk;
  nk



let map_succ gram_rhs gram_lhs gram_parnt bnt_array lhs_table (*nt_of_ind prio_of_ind*) str_non_ter str_ter regexp_array r_L (*prio_dat array_nt_prio lhslists*) array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl array_lhs succ_states_array non_kernel_array is_trace_state_list vl =
  let rec map_succ_aux (is_trace, state_list) = function
    | v::tl ->
    (*Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_ter prio_dat.prd_names regexp_array;
    Printf.fprintf !log_channel "\n";
    flush_all ();*)
    (*Printf.printf "build state %d\n" v.number;
    flush_all ();*)
    let vl =
      move_LR0 v is_trace gram_rhs gram_lhs gram_parnt bnt_array r_L (*prio_dat*)
      (*array_nt_prio lhslists*) array_lt_ter array_lt_ter_nl array_lt_nt
      array_lt_nt_nl array_lhs succ_states_array non_kernel_array
    in
    map_succ_aux (is_trace, v::state_list) (vl@tl)
    | [] -> is_trace, state_list
  in
  map_succ_aux is_trace_state_list vl




let make_entry_point_state entry_points stations gram_rhs gram_lhs gram_parnt bnt_array lhs_table str_non_ter str_ter regexp_array r_L array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl array_lhs succ_states_array non_kernel_array (is_trace, state_list) ep =
  (*let lhs_ind_set =
    Prio_map.fold
    (fun p lhs_ind set -> Int_set.add lhs_ind set)
    array_nt_prio.(ep) Int_set.empty
  in*)
  (*if Int_set.cardinal lhs_ind_set = 1 then*)
    match stations.(ep) with Some s ->
    (Hashtbl.add entry_points ep s;
    is_trace, state_list)
    | None -> assert false
  (*else
    (let is = new_item_set () in
    Int_set.iter
      (fun i ->
        match stations.(i) with Some s ->
        is.reducible <- Int_set.union is.reducible s.items.reducible;
        is.non_kernel <- merge_non_kernel is.non_kernel
          s.items.non_kernel non_kernel_array;
        is.predict <- Predict.union is.predict s.items.predict
        | None -> assert false)
      lhs_ind_set;
    let v = {
      li = Ps_Ter (-1);
      items = is;
      number = !countst;
      mergeable = true;
      bestowing_nt = Int_set.empty;
      succ_states = [] }
    in
    incr countst;
    let is_trace, state_list =
      map_succ gram_rhs gram_lhs gram_parnt bnt_array lhs_table nt_of_ind
      prio_of_ind str_non_ter
      str_ter regexp_array r_L prio_dat
      array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
      array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array
      non_kernel_array
     (is_trace,state_list) [v]
    in
    Hashtbl.add entry_points ep v;
    is_trace, v::state_list)*)
    (* is it really necessary to have v in state_list ? *)


let build_automaton_LR0 is_trace (gram_rhs:rhs array) (gram_lhs:((int list) * (int option)) array) gram_lhs' gram_parnt bnt_array (*prio_dat*) it_nb (*array_nt_prio nt_of_ind prio_of_ind lhslists*) r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list regexp_array implicit_rule =
  
  let array_lt_ter = Array.make token_nb None in
  let array_lt_ter_nl = Array.make token_nb None in
  let nt_nb = Array.length gram_lhs in
  let array_lt_nt = Array.make nt_nb None in
  let array_lt_nt_nl = Array.make nt_nb None in
  let succ_states_array = ref (Array.make 10000 false) in
  (*let succ_states_array = Hashtbl.create 10000 in*)
  let non_kernel_array = Array.make (Array.length gram_rhs) false in
  
  let array_lhs = Array.make nt_nb false in
  (* May be replaced by array of int 31 times shorter *)
  let lhs_nb = Array.length gram_lhs in
  let stations = Array.make lhs_nb None in
  (*Printf.fprintf !log_channel "lhs_nb = %d\n" lhs_nb;*)
  
  let rec map_nt n state_list =
    if n>=lhs_nb then state_list else
    let is, nt_to_add =
      init_is gram_rhs (fst gram_lhs.(n)) r_L non_kernel_array
    in
    
    (match snd gram_lhs.(n) with
      | None -> ()
      | Some rn -> is.reducible <- Int_set.add rn Int_set.empty);
    
    (*Printf.fprintf !log_channel "\nstation: (%s,%s,%d) = (%d,%d)\n"
      str_non_ter.(nt_of_ind.(n)) prio_dat.prd_names.(prio_of_ind.(n))
      n nt_of_ind.(n) prio_of_ind.(n);
    print_item_set !log_channel is gram_rhs lhs_table nt_of_ind prio_of_ind
      str_non_ter str_ter prio_dat.prd_names;*)
    
    closure_v0_LR0 is gram_rhs gram_lhs' nt_to_add non_kernel_array;
    
    is.non_kernel <- List.filter (fun rn -> not implicit_rule.(rn)) is.non_kernel;
    
    let predict =
      List.fold_left
      (fun predict rn -> try (match gram_rhs.(rn).(0) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt -> Predict.add nt predict
        | _ -> predict)
        with Invalid_argument _ -> assert false)
      Predict.empty is.non_kernel
    in
    let predict =
      Intc_set.fold
      (fun (rn,dp) predict -> try (match gram_rhs.(rn).(dp) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt -> Predict.add nt predict
        | _ -> assert false)
        with Invalid_argument _ -> assert false)
      is.kernel_nt predict
    in
    is.predict <- predict;
    let v = {
      li = Ps_Ter (-1);
      items = is;
      number = n;
      mergeable = true;
      bestowing_nt = None;
      succ_states = [] }
    in
    (*Printf.fprintf !log_channel "lhs : %s\n"
    (str_lhs (nt_of_ind.(n),prio_of_ind.(n),n) str_non_ter prio_dat.prd_names);
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind
      str_non_ter str_ter prio_dat.prd_names;*)
    stations.(n) <- Some v;
    map_nt (n+1) (v::state_list)
  in
  (*Printf.fprintf !log_channel "prio.prd_names length:%d\n"
  (Array.length prio_dat.prd_names);*)
  (*Printf.fprintf !log_channel "Begin Stations\n";*)
  let state_list = map_nt 0 [] in
  (*Printf.fprintf !log_channel "End Stations\n";*)
  
  countst := nt_nb;
  (*Printf.fprintf !log_channel "nt_nb=%d\n" nt_nb;*)
  
  let map_succ is_trace_state_list vl =
    map_succ gram_rhs gram_lhs' gram_parnt bnt_array lhs_table (*nt_of_ind*)
    (*prio_of_ind*) str_non_ter
    str_ter regexp_array r_L (*prio_dat*)
    (*array_nt_prio lhslists*) array_lt_ter array_lt_ter_nl array_lt_nt
    array_lt_nt_nl array_lhs succ_states_array non_kernel_array
    is_trace_state_list vl
  in
  
  
  let is_trace, state_list = map_succ (is_trace, []) state_list in
  
  (*List.iter (fun v ->
    Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_ter prio_dat.prd_names;
    Printf.fprintf !log_channel "\n")
    state_list;*)
  
  let entry_points = Hashtbl.create (List.length entry_points_list) in
  
  let is_trace, state_list =
    List.fold_left
    (make_entry_point_state entry_points stations gram_rhs gram_lhs
    gram_parnt bnt_array lhs_table str_non_ter str_ter regexp_array r_L
    array_lt_ter array_lt_ter_nl array_lt_nt
    array_lt_nt_nl array_lhs succ_states_array non_kernel_array)
    (is_trace, state_list) entry_points_list
  in
  
  (*List.iter (fun v ->
    Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table str_non_ter str_ter regexp_array;
    Printf.fprintf !log_channel "\n") state_list;*)
  
  is_trace, state_list, stations, entry_points


let build_automaton is_trace gram_rhs gram_lhs gram_lhs' (gram_parnt:(int * int) list array) bnt_array verbose (*prio_dat*) it_nb (*array_nt_prio nt_of_ind prio_of_ind lhslists*) r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list regexp_array implicit_rule =
  countst := 0;
  count_trans := 0;
  let time1 = Sys.time () in
  let is_trace, state_list, stations, entry_points =
    let is_trace, state_list, stations, entry_points =
          build_automaton_LR0 is_trace gram_rhs gram_lhs gram_lhs'
          gram_parnt bnt_array
          (*prio_dat*) it_nb (*array_nt_prio nt_of_ind prio_of_ind lhslists*) r_L
          lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list
          regexp_array implicit_rule
    in is_trace, state_list, stations, entry_points
  in
  let time2 = Sys.time () in
  if verbose>0 then
    (Printf.fprintf !log_channel
      "LR(0) automaton built, %d states, %d transitions, %.3f sec\n"
    (!countst) (!count_trans+1) (time2-.time1);
    flush stdout) else ();
  state_list, !countst, stations, entry_points, is_trace
