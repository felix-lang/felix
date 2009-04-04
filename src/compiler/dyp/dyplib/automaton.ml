let log_channel = ref stdout
let log_stack_channel = ref stdout

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

(*module Priority = Priority_by_relation.Make(struct type priority = int end)*)

type ('lhs,'nt_lit) prule_bis = 'lhs * (('nt_lit,int) psymbol array)
type 'nt_lit p_rhs = ('nt_lit,int) psymbol array

type non_ter = int
(*type symb = ((string * (string nt_prio)),string) psymbol*)
(*type symb = (string * non_terminal_priority) psymbol*)
type rule_options = No_layout_inside | No_layout_follows

(*type rule = string * (symb list) * string * bool*)
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

(*type ('obj,'gd,'ld) merge_result =
  Merge of ('obj list * 'gd * 'ld) | Dont_merge
type ('obj,'gd,'ld) merge_function =
  'obj list -> 'gd -> 'ld -> 'obj -> 'gd -> 'ld ->
  ('obj,'gd,'ld) merge_result*)

type ('obj,'gd,'ld) merge_function =
  ('obj * 'gd * 'ld) list -> ('obj list) * 'gd * 'ld

let keep_all l = match l with
  | (_,gd,ld)::_ -> List.map (fun (o,_,_) -> o) l, gd, ld
  | [] -> assert false

let keep_one l = match l with
  | (o,gd,ld)::_ -> [o], gd, ld
  | [] -> assert false

(*let keep_all l = l

let keep_one l = match l with
  | x::_ -> x
  | [] -> assert false*)

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
let ind_of_lhs (_,_,i) = i
let str_lhs (nt,p,i) str_non_ter priority_names =
    "("^(str_non_ter.(nt))^","^(priority_names.(p))^ ","^(string_of_int i)^")"

let str_lhs_bis (nt,p,i) str_non_ter priority_names =
  if priority_names.(p) = "default_priority" then str_non_ter.(nt)
  else str_non_ter.(nt)^"("^priority_names.(p)^")"




type literal = (lit_nt,int) psymbol
type rule_bis = (lhs,lit_nt) prule_bis
type rhs = lit_nt p_rhs

(*module Ordered_rule_bis =
  struct
  type t = rule_bis
  let compare (nt1,l1,_) (nt2,l2,_) =
    Pervasives.compare (nt1,l1) (nt2,l2)
end
module Map_r = Map.Make (Ordered_rule_bis)
type ('a,'b,'c) grammar = (('a,'b,'c) action) list Map_r.t*)

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

(*module Ordered_item_rhs =
struct
  type t = item_rhs
  let compare = Pervasives.compare
end
module Irhs_map = Map.Make(Ordered_item_rhs)*)

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

(*module Ordered_reducible =
struct
  type t = int * TNS.t
  let compare (i1,s1) (i2,s2) =
    if i1>i2 then 1
    else if i1<i2 then -1
    else TNS.compare s1 s2
end
module Int_set = Set.Make(Ordered_reducible)*)

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
      String.concat "" sl
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
  let print_reducible chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array rn =
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs lhs str_non_ter priority_names)
      (str_handle rhs (Array.length rhs) str_non_ter str_ter regexp_array)
      (* (str_token_set tns str_ter)*)

  let print_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array (rn,dp) =
    (*Printf.fprintf chan "  ** DEBUG ** rn=%d, lhs_table len=%d, gram_rhs len=%d\n"
      rn (Array.length lhs_table) (Array.length gram_rhs);*)
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs lhs str_non_ter priority_names) (str_handle rhs dp str_non_ter str_ter regexp_array)

  let print_non_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array rn =
    print_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array (rn,0)

  let print_item chan nt_of_ind prio_of_ind ind str_non_ter str_ter priority_names regexp_array (rhs,dp) =
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs (nt_of_ind.(ind),prio_of_ind.(ind),ind) str_non_ter priority_names)
      (str_handle rhs dp str_non_ter str_ter regexp_array)



let lhs_list (nt,_) prio_dat array_nt_prio =
  let rec f p =
    if p = prio_dat.prd_nb then [] else
      try
        let ind = Prio_map.find p array_nt_prio.(nt) in
        (nt,p,ind)::(f (p+1))
      with Not_found -> f (p+1)
  in
  f 0

module Ordered_ntp =
struct
  type t = priority nt_prio
  let compare = Pervasives.compare
end
module Ntp_map = Map.Make(Ordered_ntp)

type lhslists = ((lhs list) Ntp_map.t) array
(*let lhslists_init i = Array.make i Ntp_map.empty*)

let comp_lhslist (nt,nt_p) lhslists prio_dat array_nt_prio =
  (*print_endline ("comp_lhslist sur "^(str_nt_lit (nt,nt_p)));*)
  try Ntp_map.find nt_p lhslists.(nt)
  with Not_found ->(
  let test = match nt_p with
    | No_priority -> (fun _ -> true)
    | Eq_priority q -> (fun p -> p=q)
    | Less_priority q -> (fun p -> is_relation prio_dat p q)
    | Lesseq_priority q -> (fun p -> (is_relation prio_dat p q) || (p=q))
    | Greater_priority q -> (fun p -> is_relation prio_dat q p)
    | Greatereq_priority q -> (fun p -> (is_relation prio_dat q p) || (p=q))
  in
  let rec f p =
    if p = prio_dat.prd_nb then [] else
    if test p then
      try
        let ind = Prio_map.find p array_nt_prio.(nt) in
        (nt,p,ind)::(f (p+1))
      with Not_found -> f (p+1)
        | e -> (Printf.printf "array_nt_prio.(nt), nt=%d, len=%d\n"
          nt (Array.length array_nt_prio); raise e)
    else f (p+1)
  in
  let l = f 0 in
  lhslists.(nt) <- Ntp_map.add nt_p l lhslists.(nt);
  (*List.iter (fun lhs -> print_endline (str_lhs lhs)) l;*)
  l)
  (*| e -> (Printf.printf "lhslists length=%d\n"
     (Array.length lhslists); raise e)*)


(** this type is used to construct the automaton. Each state in the automaton
    has a field of type [lit_trans] which is the literal of transition to
    this state. The difference with the type [literal] is that there is no
    priority attached to the non terminals. *)
type lit_trans = (non_ter,int) psymbol
(*  | Ter of token_name
  | Ps_Non_ter of non_ter*)

let lit_trans (symb:(lit_nt,int) psymbol) = match symb with
  | Ps_Non_ter (nt,_) -> Ps_Non_ter nt
  | Ps_Ter t -> Ps_Ter t
  | Ps_Non_ter_NL (nt,_) -> Ps_Non_ter_NL nt
  | Ps_Ter_NL t -> Ps_Ter_NL t

module Ordered_lit_trans=
struct
  type t = lit_trans
  let compare = Pervasives.compare
end

module Map_lit_trans = Map.Make(Ordered_lit_trans)


(*type nfa_item =
    Station of int  (* nt nb *)
  | Non_station of (int * int) (* (rule nb, dot pos)  *)*)


(*module rec Nfa_state :
sig
  type nfa_state = {
    item : int * int; (* (rule nb, dot pos)  *)
    eps_trans : Nfa_state_set.t;
    trans : nfa_state Map_lit_trans
  }
  module Nfa_state_set : Set.S with type elt = nfa_state
end
=
struct
  type nfa_state = {
    item : nfa_item;
    eps_trans : Nfa_state_set.t;
    trans : nfa_state
  }
  module Nfa_state_set =
  struct
    module Ordered_nfa_state =
    struct
      type t = nfa_state
      let compare s1 s2 = Pervasives.compare s1.item s2.item
    end
    module Nfa_state_set_prime = Set.Make(Ordered_nfa_state)
    include (State_set_prime:Set.S with type elt = nfa_state)
  end
end

include State*)


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


  (** [li] is the literal of transition from the previous states *)
(*module rec State :
sig
  type state = {
    number : int;
    li : lit_trans;
    items : item_set;
    mutable succ_states : State.Succ_states.t;
    (*mutable succ_states : (state * priority) list;*)
    (*mutable pred_states : State.State_set.t Li_map.t*)
  }
  module State_set : Set.S with type elt = state
  module Succ_states : Set.S with type elt = state * priority
end
=*)
  type state = {
      number : int;
      li : lit_trans;
      items : item_set;
      mergeable : bool;
      bestowing_nt : Int_set.t;
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
     (out_channel option) * (out_channel option)) )

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
  use_all_actions : bool }

and ('t,'o,'gd,'ld,'l) parsing_device = {
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
  entry_points : (int, state) Hashtbl.t;
  g_nb : int;
   (* the id of the grammar of the parser *)
  lex_nb : int;
   (* the id of the main lexer *)
  nt_table : (string, int) Hashtbl.t;
  stations : state option array;
  state_list : state list;
  is_trace : Dyp_special_types.is_trace;
  array_nt_prio : Dyp_special_types.array_nt_prio;
  st_nb : int;
  table : int array array array;
  (* 1st index for state
  2nd index: 0 for terminals, 1 for non terminals
  3rd index to choose which terminal or nt. *)
  table_it : item_set array;
  state_is_mergeable : bool array;
  state_bnt : Int_set.t array;
  (* Set of id of the actions to produce inherited attributes. *)
  (*table_lex_trans : bool array;*)
  (* index: (state id * nb of ter) + ter id,
  says whether a transition with this terminal exists.
  to be improved : an int array *)
  table_lit_trans : lit_trans array;
  lhslists : lhslists;
  r_L : int list array;
  (*po : bool option array array;*)
  po : bool array array;
    (** Partial order between non terminal name couples. A couple of non
     terminals is bound to a bool. This may be implemented alternatively
     with a set : the presence of a couple (nt1,nt2) in the set would
     denote that nt1<=nt2 is true. This would save memory space. *)
  cyclic_rules : int list array;
    (* For each rule tells whether the rule is cyclic. If the rule is not
    cyclic then the list is empty, otherwise it contains the places of the
    non terminals in the rhs which make the rule cyclic. *)
  data : 'gd;
  loc_data : 'ld;
  prio : priority_data;
  token_nb : int;
  nt_nb : int;
    (* number of non terminals in the internal grammar
     (as opposed to the user grammar. *)
  prio_of_ind : int array;
  nt_of_ind : non_ter array;
  (*lhs_of_ind : lhs array;*)
  str_non_ter : string array;
  cons_of_nt : int array;
  relations : string list list;
  nt_cons_map : nt_cons_map;
  rn_of_rule : (rule, int) Hashtbl.t;
  (*regexp_decl :
   (string, (node -> int -> int -> node * int * int)) Hashtbl.t;*)
  regexp_decl : (string, (node -> int -> node * int)) Hashtbl.t;
  main_lexer_start : node;
  main_lexer_table : lex_table;
  main_lexer_actions : (int * ('o dyplexbuf -> 'o)) array;
  (* the int is the id of the terminal that is produced *)
  aux_lexer : 'o aux_lexer;
  str_ter : string array;
  ter_table : (string, int) Hashtbl.t;
  layout_id : int;
  regexp_table : (regexp, int) Hashtbl.t;
  (*regexp_actions : (int * ('o dyplexbuf -> 'o)) array;*)
  regexp_array : regexp array;
  rule_options : int array
  (* tells for each rule whether it allows layout characters inside
   or afterward *)
}

and ('t,'o,'gd,'ld,'l) parser_pilot =
  ('t,'o,'gd,'ld,'l) parsing_device * ('t,'o,'gd,'ld,'l) parser_parameters

and 'o aux_lexer = {
  aux_lexer_start : (string, node) Hashtbl.t;
  aux_lexer_table : (string, lex_table) Hashtbl.t;
  aux_lexer_actions : (string, ('o list -> 'o dyplexbuf -> 'o) array) Hashtbl.t
}

and 'o dyplexbuf = {
  (*mutable lb_start_p : Lexing.position;
  mutable lb_curr_p : Lexing.position;
  lb_string : string;*)
   (* the input string, to be replaced by a real buffer *)
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


let str_state_succ succ_states str_non_ter str_ter priority_names =
  let f str (state,prio) =
    str^" ["^(string_of_int state.number)^",("^
    (str_literal_trans state.li str_non_ter str_ter)^
    ","^priority_names.(prio)^")]"
  in
  List.fold_left f "" succ_states




let print_item_set chan is gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names regexp_array =
  Printf.fprintf chan "  nb of items: %d\n"
  ((Int_set.cardinal is.reducible)+(Intc_set.cardinal is.kernel_nt)+
  (Intc_set.cardinal is.kernel_t)+(List.length is.non_kernel));
  Int_set.iter
    (print_reducible chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array)
    is.reducible;
  Intc_set.iter
    (print_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array)
    is.kernel_t;
  Intc_set.iter
    (print_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array)
    is.kernel_nt;
  List.iter
    (print_non_kernel chan gram_rhs lhs_table str_non_ter str_ter priority_names regexp_array)
    is.non_kernel



let str_nt_prio ntp pnames = match ntp with
  | No_priority -> "_"
  | Eq_priority p -> "="^pnames.(p)^":"^(string_of_int p)
  | Less_priority p -> "<"^pnames.(p)^":"^(string_of_int p)
  | Lesseq_priority p -> "<="^pnames.(p)^":"^(string_of_int p)
  | Greater_priority p -> ">"^pnames.(p)^":"^(string_of_int p)
  | Greatereq_priority p -> ">="^pnames.(p)^":"^(string_of_int p)




let str_predict predict nt_of_ind prio_of_ind str_non_ter priority_names =
  Predict.fold
  (fun (nt,p) str ->
    str^(Printf.sprintf "(%s:%d,%s)" str_non_ter.(nt) nt
    (str_nt_prio p priority_names)))
  predict ""



let print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names regexp_array =
  Printf.fprintf !log_channel " State %d\n" s.number;
  Printf.fprintf !log_channel "  li : %s\n"
    (str_literal_trans s.li str_non_ter str_ter);
  Printf.fprintf !log_channel "  items :\n";
  print_item_set !log_channel s.items gram_rhs lhs_table nt_of_ind prio_of_ind
    str_non_ter str_ter priority_names regexp_array;
  Printf.fprintf !log_channel "  next states : %s\n"
    (str_state_succ s.succ_states str_non_ter str_ter priority_names);
  Printf.fprintf !log_channel "  predict : %s\n"
    (str_predict s.items.predict nt_of_ind prio_of_ind str_non_ter priority_names)

let rec str_lit_list litl str_non_ter str_ter regexp_array = match litl with
  | [] -> ""
  | symb::tl -> (str_literal symb str_non_ter str_ter regexp_array)^" "^
      (str_lit_list tl str_non_ter str_ter regexp_array)

let str_rule (nt,litl,_) str_non_ter str_ter priority_names regexp_array =
  (str_lhs nt str_non_ter priority_names)^" -> "^
  (str_lit_list litl str_non_ter str_ter regexp_array)

let rec print_rule_list rl str_non_ter str_ter priority_names regexp_array = match rl with
  | [] -> ()
  | r::t -> Printf.fprintf !log_channel " %s\n"
    (str_rule r str_non_ter str_ter priority_names regexp_array);
     print_rule_list t str_non_ter str_ter priority_names regexp_array


(** Used to print the content of an automaton. *)
(*let print_aut v0 gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names =
  output_string !log_channel "\n"; output_string !log_channel "\n";
  output_string !log_channel
"----------------------------------- Automaton ----------------------------------\n";
  output_string !log_channel "\n";
  let f s visited_v =
    if State_set.mem s visited_v then (visited_v,[]) else
      (print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names;
      print_newline ();
      (State_set.add s visited_v,s.succ_states))
  in
  let rec map_succ (state,_) visited_v  =
    let visited_v,succ_states = f state visited_v in
    Succ_states.fold map_succ succ_states visited_v
  in
  let visited_v,succ_states = f v0 State_set.empty in
  Succ_states.fold map_succ succ_states visited_v*)

let print_map m gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names regexp_array =
  let f (is,_) s =
    let () = Printf.fprintf !log_channel "state %d\n" (s.number) in
    print_item_set !log_channel is gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter str_ter priority_names regexp_array
  in
  Map_is.iter f m



(**** CLOSURE functions used to close an items set *****)


module Ordered_rule_bis =
struct
  type t = rule_bis
  let compare = Pervasives.compare
end
module RbS = Set.Make(Ordered_rule_bis)

module First =
struct
  type t = TNS.t * bool (* true=contains epsilon *)
  let tns (tns,_) = tns
  let union (tns1,b1) (tns2,b2) = (TNS.union tns1 tns2, b1||b2)
  let empty = (TNS.empty,false)
  let add tok (tns,b) = (TNS.add tok tns,b)
  let mem tok (tns,_) = TNS.mem tok tns
  let mem_epsilon (_,b) = b
  let add_epsilon (tns,_) = (tns,true)
end


(*let compute_first first_memo grammar nt_nb prio_dat array_nt_prio nt_of_ind prio_of_ind =
  let rec first_on_nt nt vrules =
    (*output_string !log_channel ("first_on_nt "^(string_of_int n));*)
    let f lhs litl _ rbs = RbS.add (lhs,litl) rbs in
    let lhs_l = lhs_list nt prio_dat array_nt_prio in
    let foldfun rbs lhs = Map_rhs.fold (f lhs) grammar.(ind_of_lhs lhs) rbs in
    let nt_rules = List.fold_left foldfun RbS.empty lhs_l in
    let aux r tns =
      if RbS.mem r vrules then tns else
      let vrules = RbS.add r vrules in
      let (_,litl) = r in
      let tns2 = first_on_litl litl 0 None vrules in
      (First.union tns tns2)
    in
    let tns = RbS.fold aux nt_rules First.empty in
    let ind = fst nt in
    first_memo.(ind) <- Some tns;
    tns
  
  and first_on_litl litl p la vrules =
    let rec aux i tns =
      if i=Array.length litl then match la with
        | Some tok -> First.add tok tns
        | None -> First.add_epsilon tns
      else match litl.(i) with
        | Ps_Ter ter -> (First.add ter tns)
        | Ps_Non_ter nt ->
          let tns2 =
            match first_memo.(fst nt) with
            | Some tns -> tns
            | None -> first_on_nt nt vrules
          in
          if First.mem_epsilon tns2 then
            aux (i+1) (First.union tns tns2)
          else (First.union tns tns2)
    in
    aux p First.empty
  in
  
  for i=0 to nt_nb-1 do
    let nt = nt_of_ind.(i) in
    (*let j = ind_of_nt nt in*)
    match first_memo.(nt) with
      | None ->
          let nt_lit = (nt,No_priority) in
          first_memo.(nt) <- Some (first_on_nt nt_lit RbS.empty)
      | _ -> ()
  done*)


(* Not useful for LR(0), useful for LALR.
let first litl la p first_memo =
  let first_on_litl litl p la vrules =
    let rec aux i tns =
      if i=Array.length litl then match la with
        | Some tok -> First.add tok tns
        | None -> tns (*TNS.add token_epsilon tns*)
      else match litl.(i) with
        | Ps_Ter ter -> (First.add ter tns)
        | Ps_Non_ter nt ->
          let tns2 =
            match first_memo.(fst nt) with
            | Some tns -> tns
            | None -> assert false
          in
          if First.mem_epsilon tns2 then
            aux (i+1) (First.union tns tns2)
          else (First.union tns tns2)
    in
    First.tns (aux p First.empty)
  in
  
  first_on_litl litl p la RbS.empty
  *)



(* AUTOMATON CREATION *)

let countst = ref 0
(** This ref counts the number of state creations. *)
let count_trans = ref 0
(** This ref counts the number of transitions between states. *)

(* ---------------------------- LR(1) automaton ---------------------------- *)

(*

let nonkernel_items (symb:literal) (gram:('a,'b,'c) grammar) (tns:TNS.t) prio_dat array_nt_prio nt_nb =
  match symb with
  | Ps_Non_ter nt ->
      let f lhs litl _ items_set =
        IS.add (ind_of_lhs lhs) (litl,0) tns items_set
      in
      let lhs_list = select_lhs nt prio_dat array_nt_prio in
      let f2 is lhs = Map_rhs.fold (f lhs) gram.(ind_of_lhs lhs) is in
      List.fold_left f2 (IS.make nt_nb) lhs_list
  | Ps_Ter _ -> failwith "Non terminal expected"

let closure (is:IS.t) (first_memo:First.t option array) (gram:('a,'b,'c) grammar) prio_dat array_nt_prio nt_nb =
  let aux2 _ (litl,dp) (tns:TNS.t) its =
    try
      let a1 = litl.(dp) in
      match lit_trans a1 with
        | Ps_Ter _ -> (IS.make nt_nb)
        | Ps_Non_ter a1nt ->
          let tok_s =
            if dp=(Array.length litl)-1 then tns
            else
              let f1 tok tok_s =
                let tns =
                  first litl (Some tok) (dp+1) first_memo
                in
                (TNS.union tns tok_s)
              in
              TNS.fold f1 tns TNS.empty
          in
          let its3 = nonkernel_items a1 gram tok_s prio_dat array_nt_prio nt_nb in
          (IS.diff its3 its)
    with Invalid_argument("index out of bounds") -> (IS.make nt_nb)
  in
  let rec aux1 (its1:IS.t) (its2:IS.t) =
    (*Printf.fprintf !log_channel "compt_bug=%d\n" compt_bug; flush_all ();*)
    (* its1 = the new items one has just added, its2 = the items one has added
       since closure's call. *)
    let f ind irhs tns (its1,its2) =
      let its3 = aux2 ind irhs tns its2 in
      (IS.union its3 its1),(IS.union its3 its2)
    in
    let its1,its2 = IS.fold f its1 ((IS.make nt_nb),its2) in
    if its1 = (IS.make nt_nb) then its2 else (aux1 its1 its2)
  in
  aux1 is is
*)

(*module Ordered_lit_trans=
struct
  type t = lit_trans
  let compare = Pervasives.compare
end

module Map_lit_trans = Map.Make(Ordered_lit_trans)*)

(*
let move s is_trace first_memo gram prio_dat array_nt_prio nt_nb =
  let is_trace_tok,is_trace_nt = is_trace in
  let f1 ind (rhs,dp) tns lit_map =
    if dp = Array.length rhs then lit_map
    else
      let symb = lit_trans rhs.(dp) in
      let is =
        try Map_lit_trans.find symb lit_map
        with Not_found -> (IS.make nt_nb),0
      in
      let new_is = (IS.add ind (rhs,dp+1) tns (fst is)),((snd is)+1) in
      Map_lit_trans.add symb new_is lit_map
  in
  let lit_map = IS.fold f1 s.items Map_lit_trans.empty in
  let f2 (symb:lit_trans) (is:IS.t * int) vl =
    count_trans := !count_trans+1;
    try
      let v1 = match symb with
        | Ter t -> Map_is.find is is_trace_tok.(t)
        | Ps_Non_ter nt -> Map_is.find is is_trace_nt.(ind_of_nt nt)
      in
      add_edge s v1;
      vl
    with Not_found ->
      let () = countst := (!countst+1) in
      let is1 = closure (fst is) first_memo gram prio_dat array_nt_prio nt_nb in
      let v1 = {
        li = symb;
        items = is1;
        number = !countst;
        succ_states = Li_map.empty;
        pred_states = Li_map.empty
      } in
      add_edge s v1;
      let () = match symb with
        | Ter t -> is_trace_tok.(t) <- Map_is.add is v1 is_trace_tok.(t)
        | Ps_Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
            Map_is.add is v1 is_trace_nt.(ind_of_nt nt)
      in
      (Li_map.add v1.li v1 vl)
  in
  Map_lit_trans.fold f2 lit_map Li_map.empty

let build_automaton_LR1 (is0:IS.t) (lit0:lit_trans) is_trace first_memo (gram:('a,'b,'c) grammar) prio_dat it_nb array_nt_prio nt_nb lhs_of_ind =
  let rec map_succ _ state is_trace =
    (*Printf.fprintf !log_channel "-state built-";
    print_state state lhs_of_ind;
    Printf.fprintf !log_channel "\n";*)
    let v_map = move state is_trace first_memo gram prio_dat array_nt_prio nt_nb in
    Li_map.fold map_succ v_map is_trace
  in
  let is1 = closure is0 first_memo gram prio_dat array_nt_prio nt_nb in
  let v0 = {
    li = lit0;
    items = is1;
    number = !countst;
    succ_states = Li_map.empty;
    pred_states = Li_map.empty
  } in
  let is_trace_tok,is_trace_nt = is_trace in
  let () = match lit0 with
    | Ter t -> is_trace_tok.(t) <- Map_is.add (is0,it_nb) v0 is_trace_tok.(t)
    | Ps_Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
        Map_is.add (is0,it_nb) v0 is_trace_nt.(ind_of_nt nt)
  in
  (*let is_trace = Map_is.add is0 v0 is_trace in*)
  let _,_(*first_memo*) =
    Li_map.fold map_succ
      (Li_map.add v0.li v0 Li_map.empty)
      is_trace
  in
  (*first_memo_print first_memo;*)
  v0
*)

(* --------------------------- `LALR(1) automaton --------------------------- *)


(*let closure_LR0 (is:item_set) gram_rhs gram_lhs =
  let f ind (nk,red) =
    match gram_lhs.(ind) with
      | rn_l, None -> (rn_l@nk,red)
      | rn_l, (Some rn) -> (rn_l@nk,(rn,TNS.empty)::red)
  in
  let non_kernel,reducible = Int_set.fold f is.nt_to_add ([],is.reducible) in
  is.reducible <- reducible;
  is.non_kernel <- non_kernel;
  is.nt_to_add <- Int_set.empty*)



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
    Int_set.fold f nt_to_add (is.non_kernel,is.reducible) in
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
      Int_set.add inh bnt
    with Not_found -> bnt
  in
  
  Intc_set.fold f is.kernel_nt Int_set.empty



let close_state is_trace_tok is_trace_tok_nl is_trace_nt is_trace_nt_nl succ_states_array prd_nb gram_rhs gram_lhs gram_parnt lhslists prio_dat array_nt_prio r_L non_kernel_array symb prio (vl, succ_states_list) is =
  
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
      | Ps_Ter t -> Map_is.find (is,it_nb) is_trace_tok.(t)
      | Ps_Non_ter (nt,_,_) -> Map_is.find (is,it_nb) is_trace_nt.(nt)
      | Ps_Ter_NL t -> Map_is.find (is,it_nb) is_trace_tok_nl.(t)
      | Ps_Non_ter_NL (nt,_,_) -> Map_is.find (is,it_nb) is_trace_nt_nl.(nt)
    in
    let succ_states_list =
      if ga_get succ_states_array (v1.number*prd_nb+prio) then succ_states_list
      else (ga_set succ_states_array (v1.number*prd_nb+prio) true;
        (v1,prio)::succ_states_list)
    in
    (*s.succ_states <- Succ_states.add (v1,prio) s.succ_states;*)
    vl, succ_states_list
  with Not_found ->
    (*countst := (!countst+1);*)
    let f1 (rn,dp) (nt_to_add,predict) = match gram_rhs.(rn).(dp) with
      | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
          let lhs_l =
            comp_lhslist nt lhslists prio_dat array_nt_prio
          in
          let g1 nt_to_add ind =
            Int_set.add ind nt_to_add
          in
          let g2 nt_to_add (_,_,ind) =
            let ind_list = r_L.(ind) in
            List.fold_left g1 nt_to_add ind_list
          in
          (List.fold_left g2 nt_to_add lhs_l), Predict.add nt predict
      | _ -> assert false
    in
    let nt_to_add, predict =
      Intc_set.fold f1 is.kernel_nt (Int_set.empty,Predict.empty)
    in
    let g nk rn =
      if non_kernel_array.(rn) then nk
      else (non_kernel_array.(rn) <- true; rn::nk)
    in
    let f ind (nk,red) = match gram_lhs.(ind) with
      | rn_l, None ->
          List.fold_left g nk rn_l, red
      | rn_l, (Some rn) ->
          List.fold_left g nk rn_l,
          Int_set.add rn red
    in
    let non_kernel,reducible =
      Int_set.fold f nt_to_add ([],is.reducible)
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
    ga_set succ_states_array (v1.number*prd_nb+prio) true;
    v1::vl, (v1,prio)::succ_states_list



let aux_move_nt array_lt_nt (*splitting_nt*) (is_list, lt_list) lhs =
  let nt, _, ind = lhs in
  match array_lt_nt.(ind) with
    | None ->
        let is = new_item_set () in
        array_lt_nt.(ind) <- Some is;
        is::is_list, (Ps_Non_ter lhs)::lt_list
    | Some is -> is::is_list, lt_list



let aux_move_nt_nl array_lt_nt_nl (is_list, lt_list, nl_list) lhs =
  let (nt,_,ind) = lhs in
  match array_lt_nt_nl.(ind) with
    | None ->
        let is = new_item_set () in
        array_lt_nt_nl.(ind) <- Some is;
        let symb = Ps_Non_ter_NL lhs in
        let nl_list = (symb,is)::nl_list
        in
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


(* FIXME array_lhs is still not used, it should be used to avoid calling
Int_set.add too many times in f1. Then we could give up the Int_set for a list.*)
let move_LR0 s is_trace gram_rhs gram_lhs (gram_parnt:(int*int) list array) bnt_array r_L prio_dat array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array non_kernel_array =
  (*output_string !log_channel "move_LR0 called\n";*)
  
  let (is_trace_tok,is_trace_tok_nl), (is_trace_nt,is_trace_nt_nl) =
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
          let lhslist = comp_lhslist nt lhslists prio_dat array_nt_prio in
          (*Printf.fprintf !log_channel "lhslist length = %d\n"
          (List.length lhslist);*)
          (List.fold_left (aux_move_nt array_lt_nt)
            ([], lt_list) lhslist), nl_list
      | (Ps_Ter_NL t) as symb -> (match array_lt_ter_nl.(t) with
        | None ->
            let is = new_item_set () in
            array_lt_ter_nl.(t) <- Some is;
            let nl_list = (symb,is)::nl_list
            in
            ([is], symb::lt_list), nl_list
        | Some is -> ([is], lt_list), nl_list)
      | Ps_Non_ter_NL nt ->
          let lhslist = comp_lhslist nt lhslists prio_dat array_nt_prio in
          let a, b, c =
            (List.fold_left
              (aux_move_nt_nl array_lt_nt_nl)
              ([], lt_list, nl_list) lhslist)
          in (a, b), c
    in
    List.iter
      (if dp+1 = Array.length rhs then
        (fun is -> is.reducible <- Int_set.add rn is.reducible)
      else
      match rhs.(dp+1) with
        | Ps_Ter _ | Ps_Ter_NL _ ->
            (fun is -> is.kernel_t <- Intc_set.add (rn,dp+1) is.kernel_t)
        | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
            (fun is -> (is.kernel_nt <- Intc_set.add (rn,dp+1) is.kernel_nt)))
      is_list;
    lt_list, nl_list
  in
  
  let lt_list, nl_list = Intc_set.fold f1 s.items.kernel_nt ([], []) in
  let lt_list, nl_list = Intc_set.fold f1 s.items.kernel_t (lt_list, nl_list) in
  let f3 (lt_list, nl_list) rn = f1 (rn,0) (lt_list, nl_list) in
  let lt_list, nl_list =
    List.fold_left f3 (lt_list, nl_list) s.items.non_kernel in
  
  (*Printf.fprintf !log_channel "lt_list length = %d\n" (List.length lt_list);*)
  
  add_items_to_nl_state array_lt_ter array_lt_nt nl_list;
  
  let prd_nb = prio_dat.prd_nb in
  List.iter (fun (s,p) -> ga_set succ_states_array (s.number*prd_nb+p) true)
    s.succ_states;
  (* On peut améliorer : dans le cas des états où la transition se fait sur
  un terminal on n'a pas besoin de décliner toutes les priorités. *)
  
  let f2 (vl, succ_states_list) symb =
    
    let is_opt, prio = match symb with
      | Ps_Ter t -> array_lt_ter.(t), 0
      | Ps_Non_ter (_,prio,ind) -> array_lt_nt.(ind), prio
      | Ps_Ter_NL t -> array_lt_ter_nl.(t), 0
      | Ps_Non_ter_NL (_,prio,ind) -> array_lt_nt_nl.(ind), prio
    in
    
    match is_opt with Some is ->
      close_state
      is_trace_tok is_trace_tok_nl is_trace_nt is_trace_nt_nl
      succ_states_array prd_nb gram_rhs gram_lhs gram_parnt lhslists
      prio_dat array_nt_prio r_L non_kernel_array symb prio
      (vl, succ_states_list) is
    | None -> assert false
  in
  let vl, succ_states_list = List.fold_left f2 ([],s.succ_states) lt_list in
  s.succ_states <- succ_states_list;
  List.iter (fun (s,p) -> !succ_states_array.(s.number*prd_nb+p) <- false)
    s.succ_states;
  let clear_array symb = match symb with
    | Ps_Ter t -> array_lt_ter.(t) <- None
    | Ps_Non_ter (_,_,ind) -> array_lt_nt.(ind) <- None
    | Ps_Ter_NL t -> array_lt_ter_nl.(t) <- None
    | Ps_Non_ter_NL (_,_,ind) -> array_lt_nt_nl.(ind) <- None
  in
  List.iter clear_array lt_list;
  vl




let init_is gram_rhs rule_list r_L array_nt_prio lhslists prio_dat non_kernel_array =
  (*let non_ter_of_lit nt_lit = non_ter_of_nt (nt_of_lit nt_lit) in*)
  let is = new_item_set () in
  let aux (non_kernel,nt_to_add) rn =
  (*print_endline ("> "^(string_of_int (Array.length gram_rhs.(rn))));
  print_endline ("> "^(str_handle gram_rhs.(rn) 0));*)
    let non_kernel =
      if non_kernel_array.(rn) then non_kernel
      else (non_kernel_array.(rn) <- true; rn::non_kernel)
    in
    (*is.non_kernel <- Int_set.add rn is.non_kernel;*)
    try (match gram_rhs.(rn).(0) with
      | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
        (let lhs_l =
          comp_lhslist nt lhslists prio_dat array_nt_prio
        in
        let g1 nt_to_add ind =
          Int_set.add ind nt_to_add
        in
        let g2 nt_to_add (_,_,ind) =
          let ind_list = r_L.(ind) in
          List.fold_left g1 nt_to_add ind_list
        in
        non_kernel,(List.fold_left g2 nt_to_add lhs_l))
      | _ -> non_kernel,nt_to_add)
    with Invalid_argument _ -> assert false
  in
  
  let non_kernel, nt_to_add =
    List.fold_left aux ([],Int_set.empty) rule_list
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



let map_succ gram_rhs gram_lhs gram_parnt bnt_array lhs_table nt_of_ind prio_of_ind str_non_ter str_ter regexp_array r_L prio_dat array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array non_kernel_array (is_trace,state_list) v =
  let rec map_succ_aux (is_trace,state_list) v =
    (*Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_ter prio_dat.prd_names regexp_array;
    Printf.fprintf !log_channel "\n";
    flush_all ();*)
    (*Printf.printf "build state %d\n" v.number;
    flush_all ();*)
    let vl =
      move_LR0 v is_trace gram_rhs gram_lhs gram_parnt bnt_array r_L prio_dat
      array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
      array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array
      non_kernel_array
    in
    List.fold_left map_succ_aux (is_trace,v::state_list) vl
  in map_succ_aux (is_trace,state_list) v



let make_entry_point_state entry_points stations gram_rhs gram_lhs gram_parnt bnt_array lhs_table nt_of_ind prio_of_ind str_non_ter str_ter regexp_array r_L prio_dat array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array non_kernel_array (is_trace, state_list) ep =
  let lhs_ind_set =
    Prio_map.fold
    (fun p lhs_ind set -> Int_set.add lhs_ind set)
    array_nt_prio.(ep) Int_set.empty
  in
  if Int_set.cardinal lhs_ind_set = 1 then
    match stations.(Int_set.choose lhs_ind_set) with Some s ->
    (Hashtbl.add entry_points ep s;
    is_trace, state_list)
    | None -> assert false
  else
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
     (is_trace,state_list) v
    in
    Hashtbl.add entry_points ep v;
    is_trace, v::state_list)
    (* is it really necessary to have v in state_list ? *)


let build_automaton_LR0 is_trace (gram_rhs:rhs array) (gram_lhs:((int list) * (int option)) array) gram_parnt bnt_array prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind lhslists r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list regexp_array =
  
  let array_lt_ter = Array.make token_nb None in
  let array_lt_ter_nl = Array.make token_nb None in
  (*let splitting_ter =  Array.make token_nb false in*)
  let nt_nb = Array.length gram_lhs in
  let array_lt_nt = Array.make nt_nb None in
  let array_lt_nt_nl = Array.make nt_nb None in
  (*let splitting_nt = Array.make (Array.length str_non_ter) false in*)
  let succ_states_array = ref (Array.make 10000 false) in
  let non_kernel_array = Array.make (Array.length gram_rhs) false in
  
  (*Gc.set { (Gc.get()) with Gc.space_overhead = 100 };*)
  
  let array_lhs = Array.make nt_nb false in
  (* May be replaced by array of int 31 times shorter *)
  (*let station = Array.make nt_nb (-1) in*)
  
  (*let rec map_succ (is_trace,state_list) v =
    (*Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_ter prio_dat.prd_names;
    Printf.fprintf !log_channel "\n";
    flush_all ();*)
    (*Printf.printf "build state %d\n" v.number;
    flush_all ();*)
    let vl =
      move_LR0 v is_trace gram_rhs gram_lhs r_L prio_dat
        array_nt_prio lhslists array_lt_ter array_lt_nt
        array_lhs succ_states_array non_kernel_array
    in
    List.fold_left map_succ (is_trace,v::state_list) vl
  in*)
  
  (*closure_v0_LR0 is0 gram_rhs gram_lhs nt_to_add;
  let v0 = {
    li = lit0;
    items = is0;
    number = !countst; (* should always be 0 *)
    succ_states = [];
    (*pred_states = Li_map.empty*)
  } in
  let is_trace_tok,is_trace_nt = is_trace in
  let () = match lit0 with
    | Ps_Ter t -> is_trace_tok.(t) <- Map_is.add (is0,it_nb) v0 is_trace_tok.(t)
    | Ps_Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
        Map_is.add (is0,it_nb) v0 is_trace_nt.(ind_of_nt nt)
  in*)
  (*let is_trace = Map_is.add is0 v0 is_trace in*)
  let lhs_nb = Array.length gram_lhs in
  (*let ext_nt_nb = Array.length nt_of_ind in*)
  let stations = Array.make lhs_nb None in
  (*Printf.fprintf !log_channel "lhs_nb = %d\n" lhs_nb;*)
  (*Printf.fprintf !log_channel "ext_nt_nb = %d\n" ext_nt_nb;*)
  
  let rec map_nt n state_list =
    if n>=lhs_nb then state_list else
    (*let rule_list = match gram_lhs.(n) with
      | rn_l, None -> rn_l
      | rn_l, (Some rn) -> rn::rn_l
    in*)
    (*if n>=lhs_nb then
      let v = {
        li = Ps_Ter 0;
        items = new_item_set ();
        number = n;
        succ_states = Succ_states.empty }
      in
      let () = Printf.fprintf !log_channel "Empty station\n" in
      let () = stations.(n) <- Some v in
      map_nt (n+1) (v::state_list)
    else*)
    let is, nt_to_add =
      init_is gram_rhs (fst gram_lhs.(n)) r_L array_nt_prio lhslists
      prio_dat non_kernel_array
    in
    (match snd gram_lhs.(n) with
      | None -> ()
      | Some rn -> is.reducible <- Int_set.add rn Int_set.empty);
    (*Printf.fprintf !log_channel "\nstation: (%s,%s,%d) = (%d,%d)\n"
      str_non_ter.(nt_of_ind.(n)) prio_dat.prd_names.(prio_of_ind.(n))
      n nt_of_ind.(n) prio_of_ind.(n);
    print_item_set !log_channel is gram_rhs lhs_table nt_of_ind prio_of_ind
      str_non_ter str_ter prio_dat.prd_names;*)
    closure_v0_LR0 is gram_rhs gram_lhs nt_to_add non_kernel_array;
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
      bestowing_nt = Int_set.empty;
      succ_states = [] }
    in
    (*let is_trace,state_list =
      List.fold_left map_succ (is_trace,state_list) [v]
    in*)
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
  let state_list =
    map_nt 0 []
    (*List.fold_left map_succ (is_trace,[v0]) [v0]*)
  in
  (*Printf.fprintf !log_channel "End Stations\n";*)
  
  countst := nt_nb;
  (*Printf.fprintf !log_channel "nt_nb=%d\n" nt_nb;*)
  
  let map_succ (is_trace,state_list) v =
    map_succ gram_rhs gram_lhs gram_parnt bnt_array lhs_table nt_of_ind
    prio_of_ind str_non_ter
    str_ter regexp_array r_L prio_dat
    array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
    array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array
    non_kernel_array
    (is_trace,state_list) v
  in
  
  let is_trace,state_list =
    List.fold_left map_succ (is_trace,[]) state_list
  in
  
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
    gram_parnt bnt_array lhs_table
    nt_of_ind prio_of_ind str_non_ter str_ter regexp_array r_L prio_dat
    array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
    array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array
    non_kernel_array)
    (*(fun (is_trace, state_list) ep ->
    let lhs_ind_set =
      Prio_map.fold
      (fun p lhs_ind set -> Int_set.add lhs_ind set)
      array_nt_prio.(ep) Int_set.empty
    in
    if Int_set.cardinal lhs_ind_set = 1 then
      match stations.(Int_set.choose lhs_ind_set) with Some s ->
      (Hashtbl.add entry_points ep s;
      is_trace, state_list)
      | None -> assert false
    else
      (let is = new_item_set () in
      Int_set.iter
      (fun i ->
        match stations.(i) with Some s ->
        is.reducible <- Int_set.union is.reducible s.items.reducible;
        is.non_kernel <- merge_non_kernel is.non_kernel s.items.non_kernel non_kernel_array;
        is.predict <- Predict.union is.predict s.items.predict
        | None -> assert false)
      lhs_ind_set;
      let v = {
        li = Ps_Ter (-1);
        items = is;
        number = !countst;
        succ_states = [] }
      in
      incr countst;
      let is_trace, state_list = map_succ (is_trace,state_list) v in
      Hashtbl.add entry_points ep v;
      is_trace, v::state_list)
      (* is it really necessary to have v in state_list ? *)
    )*)
    (is_trace, state_list) entry_points_list
  in
  
  (*List.iter (fun v ->
    Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_ter prio_dat.prd_names;
    Printf.fprintf !log_channel "\n") state_list;*)
  
  (*Gc.set { (Gc.get()) with Gc.space_overhead = 80 };*)
  (*let _ = print_aut v0 gram_rhs lhs_table lhs_of_ind in*)
  is_trace, state_list, stations, entry_points




let build_automaton_LALR is_trace gram_rhs gram_lhs gram_parnt bnt_array prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind lhslists r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list regexp_array =
  (*let time1 = Sys.time () in
  compute_first first_memo gram nt_nb prio_dat array_nt_prio lhs_of_ind;
  let time2 = Sys.time () in
  Printf.printf "first computed, %.3f sec\n" (time2-.time1);*)
  (*flush_all ();*)
  let is_trace,state_list,stations, entry_points =
    build_automaton_LR0 is_trace gram_rhs
    gram_lhs gram_parnt bnt_array prio_dat it_nb array_nt_prio nt_of_ind
    prio_of_ind lhslists r_L lhs_table ist_nt_nb token_nb
    str_non_ter str_ter entry_points_list regexp_array
  in
  
  is_trace, state_list, stations, entry_points


let build_automaton is_trace gram_rhs gram_lhs (gram_parnt:(int * int) list array) bnt_array verbose prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind lhslists r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list regexp_array =
  countst := 0;
  count_trans := 0;
  let time1 = Sys.time () in
  let is_trace, state_list, stations, entry_points = (*match automaton_kind with
    | `LR0 ->*)
        let is_trace, state_list, stations, entry_points =
          build_automaton_LR0 is_trace gram_rhs gram_lhs gram_parnt bnt_array
          prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind lhslists r_L
          lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list
          regexp_array
        in is_trace, state_list, stations, entry_points
    (*| `LALR -> build_automaton_LALR is_trace
        gram_rhs gram_lhs prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind
        lhslists r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list
    | `LR1 ->
        print_endline "LR(1) automaton currently not available, `LALR(1) is chosen.";
        build_automaton_LALR is_trace
        gram_rhs gram_lhs prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind
        lhslists r_L lhs_table ist_nt_nb token_nb str_non_ter str_ter entry_points_list*)
  in
  let time2 = Sys.time () in
  if verbose>0 then
    ((*let str_aut_kind = match automaton_kind with
      `LR0 -> "LR(0)" | `LALR -> "`LALR(1)" | `LR1 -> "LR(1)" in*)
    Printf.fprintf !log_channel "LR(0) automaton built, %d states, %d transitions, %.3f sec\n"
    (*str_aut_kind*) (!countst) (!count_trans+1) (time2-.time1);
    (*let _ = print_aut v0 lhs_of_ind in*)
    flush stdout) else ();
  state_list, !countst, stations, entry_points, is_trace





(** The following functions are used to cover 2 automatons to test whether
    they are equal. *) 
(*
let eq_vertex s1 s2 =
  if s1.li<>s2.li then false else
  if (IS.compare_is s1.items s2.items)=0 then true
  else false

exception Look_for_lit_failed

let eq_next n1 v2 =
  let aux1 _ v l = v.li::l in
  let key_list = Li_map.fold aux1 n1 [] in
  let aux2 key = (Li_map.find key v2.succ_states) in
  try (true,List.map aux2 key_list) with Not_found -> (false,[])
*)

(*
let eq_aut automaton1 automaton2 =
  let n1 = number_aut automaton1 in
  let n2 = number_aut automaton2 in
  if n1<>n2 then false else
  if eq_vertex automaton1.init automaton2.init = false then false else
  let next1 = automaton1.init.succ_states in
  let b,next2 = eq_next next1 automaton2.init in
  if b = false then false else
  let rec aux n1 n2 vv b =
    if b = false then false,vv else
    match (n1,n2) with
      | ([],[]) -> true,vv
      | (_,[]) -> false,vv
      | ([],_) -> false,vv
      | (v1::t1,v2::t2) -> if State_set.mem v1 vv then aux t1 t2 vv true else
          let new_vv = State_set.add v1 vv in
          let bb = eq_vertex v1 v2 in
          if bb = false then false,new_vv else
          let next1 = v1.succ_states in
          let bb,next2 = eq_next next1 v2 in
          if bb = false then false,new_vv else
          let b,new_vv = aux next1 next2 new_vv true in
          if b = false then (false,new_vv) else aux t1 t2 new_vv true
  in
  let b,vv = aux next1 next2 (State_set.add automaton1.init State_set.empty) true in
  b
*)

