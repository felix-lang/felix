let log_channel = ref stdout
let log_stack_channel = ref stdout

let dypgen_verbose = ref 0

let list_append l1 l2 = List.rev_append (List.rev l1) l2
let (@) l1 l2 = list_append l1 l2

type token_name = int

type 'a pliteral =
  | Ter of token_name
  | Non_ter of 'a

(*module Priority = Priority_by_relation.Make(struct type priority = int end)*)
include Priority_by_relation

type ('lhs,'nt_lit) prule_bis = 'lhs * ('nt_lit pliteral array)
type 'nt_lit p_rhs = 'nt_lit pliteral array

type non_ter = int
type lit = (non_ter * non_terminal_priority) pliteral
type rule = non_ter * (lit list) * priority

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
  ('obj * 'gd * 'ld) list -> ('obj list * 'gd * 'ld)

(*let keep_all ol gd ld o _ _ = Merge (o::ol,gd,ld)
let keep_one _ gd ld o _ _ = Merge ([o],gd,ld)*)

let keep_all l = match l with
  | (_,gd,ld)::_ -> List.map (fun (o,_,_) -> o) l, gd, ld
  | [] -> assert false

let keep_one l = match l with
  | (o,gd,ld)::_ -> [o], gd, ld
  | [] -> assert false

module Ordered_int =
struct
  type t = int
  let compare = Pervasives.compare
end

module Int_map = Map.Make(Ordered_int)
module Int_set = Set.Make(Ordered_int)

(*type datadyn = ((int * int) String_map.t) * (int Int_map.t) * int*)
(* 1st map is with key : the string of the non terminal and associated values are 2 int. 1st int is the non_ter value, 2nd int is the non_ter value (an int with dypgen) of the non_terminal of the initial grammar which has the same 'type' as this non terminal.
The 2nd map associates the first int with the second int of the values of the previous map (i.e. a nt name to its 'type').
The rigthmost int is the number of non terminals in the map *)

module Dyp_special_types =
struct
  type automaton_kind = [ `LR0 | `LALR | `LR1 ]
  type datadyn = {
    nt_map : ((int * int * string) String_map.t);
      (* map from the string of the non ter to the couple
      of its number and the number of a constructor that should
      be the constructor of the values returned by this nt.
      The string is the string of this constructor. *)
    cons_map : int String_map.t;
      (* map from the string of the constructor to its number. *)
    nt_num : int; (* the number of non terminals *)
    cons_nb : int
   }
  (*type datadyn = (int String_map.t) * int*)

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
end
open Dyp_special_types

module type Automaton_parameters =
sig
  type non_terminal
  type lhs
  type lit_nt
  (*val str_token_name : int -> string
  val str_non_terminal : non_terminal -> string*)
  (*val token_epsilon : int*)
  (*val str_priority : int -> string*)
  (*val select_rule : priority_data -> lit_nt -> lhs -> bool*)
  (*val select_lhs : lit_nt -> priority_data -> (int Prio_map.t) array -> lhs list*)
  val lhs_list : lit_nt -> priority_data -> (int Prio_map.t) array -> lhs list
  type lhslists
  val lhslists_init : int -> lhslists
  val comp_lhslist : lit_nt -> lhslists -> priority_data -> (int Prio_map.t) array -> lhs list
  (*val lhsl_of_nt : non_terminal -> (int Prio_map.t) array -> lhs list*)
end

module type Non_terminal_type =
sig
  type non_terminal
  type lhs
  type lit_nt
  val nt_of_lit : lit_nt -> non_terminal
  val lit_of_nt : non_terminal -> lit_nt
  val nt_of_lhs : lhs -> non_terminal
  val fst_of_lhs : lhs -> non_ter
  val ind_of_lhs : lhs -> int
  val ind_of_nt : non_terminal -> int
  val test_array_nt : bool array -> lit_nt -> bool
  val array_nt_size : int -> int
  val clean_array_nt : bool array -> lit_nt list -> unit
  val get_lhs_prio : lhs -> priority
  val new_rhs_lit : lit_nt -> (int Prio_map.t) array -> int-> lit_nt
  val str_token_name : int -> string
  val str_non_terminal : non_terminal -> string array -> string array -> string
  val str_lhs : lhs -> string array -> string array -> string
end

module type Grammar_type =
sig
  type non_terminal
  type lhs
  type lit_nt
  val nt_of_lit : lit_nt -> non_terminal
  val lit_of_nt : non_terminal -> lit_nt
  val nt_of_lhs : lhs -> non_terminal
  val fst_of_lhs : lhs -> non_ter
  val ind_of_lhs : lhs -> int
  val ind_of_nt : non_terminal -> int
  val test_array_nt : bool array -> lit_nt -> bool
  val array_nt_size : int -> int
  val clean_array_nt : bool array -> lit_nt list -> unit
  val new_rhs_lit : lit_nt -> (int Prio_map.t) array -> int -> lit_nt
  val get_lhs_prio : lhs -> priority
  type rule_bis = (lhs,lit_nt) prule_bis
  type rhs = lit_nt p_rhs
  module Map_rhs : Map.S with type key = rhs
  type ('a,'b,'c) grammar = ((('a,'b,'c) action) list Map_rhs.t) array
    (* the index of the array is the index of the non terminal of the lhs
      i.e. the 3rd int. *)
    (* to fix: the type grammar should not be available to Automaton *)
  type item = (int * int)
  (* 1st int is rule number
     2nd int is dot position *)
  (*type item_rhs = rhs * int*)
  module Item_map : Map.S with type key=item
  (*module Irhs_map : Map.S with type key=item_rhs*)
  module TNS : Set.S with type elt=token_name
  type item_set = {
    mutable reducible : (int * TNS.t) list;
    mutable kernel_nt : (int * int) list;
    mutable kernel_t : (int * int) list;
    mutable non_kernel : int list;
    (*mutable nt_to_add : Int_set.t*)
  }
  val new_item_set : unit -> item_set
  val dummy_item_set : item_set
  val compare_is : item_set -> item_set -> int
  val str_token_name : token_name -> string
  val str_non_terminal : non_terminal -> string array -> string array -> string
  val str_lhs : lhs -> string array -> string array -> string
  type literal = lit_nt pliteral
  val str_literal : literal -> string array -> string array -> string
  val str_handle : literal array -> int -> string array -> string array -> string
  val str_token_set : TNS.t -> string
  val print_reducible : out_channel -> rhs array -> lhs array ->
    string array -> string array -> (int * TNS.t) -> unit
  val print_kernel : out_channel -> rhs array -> lhs array ->
    string array -> string array -> (int * int) -> unit
  val print_non_kernel : out_channel -> rhs array -> lhs array ->
    string array -> string array -> int -> unit
  val print_item : out_channel -> lhs array -> int ->
    string array -> string array -> (literal array * int) -> unit
end

module Grammar_struct(Ntt:Non_terminal_type) =
struct
  include Ntt
  type literal = lit_nt pliteral
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
    type t = rhs
    let compare l1 l2 =
      Pervasives.compare l1 l2
  end
  module Map_rhs = Map.Make (Ordered_rhs)
  type ('a,'b,'c) grammar = ((('a,'b,'c) action) list Map_rhs.t) array

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


  type item_set = {
    mutable reducible : (int * TNS.t) list;
    mutable kernel_nt : (int * int) list;
    mutable kernel_t : (int * int) list;
    mutable non_kernel : int list;
    (*mutable nt_to_add : Int_set.t*)
  }

  let new_item_set () = {
    reducible = [];
    kernel_nt = [];
    kernel_t = [];
    non_kernel = [];
    (*nt_to_add = Int_set.empty*)
  }
  let dummy_item_set = {
    reducible = [];
    kernel_nt = [];
    kernel_t = [];
    non_kernel = [];
    (*nt_to_add = Int_set.empty*)
  }

  let compare_is is1 is2 = Pervasives.compare is1 is2
    (*let c = Pervasives.compare is1.reducible is2.reducible in
    if c<>0 then c else
    let c = Pervasives.compare is1.kernel_t is2.kernel_t in
    if c<>0 then c else
    let c = Pervasives.compare is1.kernel_nt is2.kernel_nt in
    if c<>0 then c else
    let c = Pervasives.compare is1.non_kernel is2.non_kernel in
    if c<>0 then c else Int_set.compare is1.nt_to_add is2.nt_to_add*)

  let str_literal lit str_non_ter str_prio = match lit with
    | Ter tk -> str_token_name tk
    | Non_ter nt -> str_non_terminal (nt_of_lit nt) str_non_ter str_prio

  let str_handle litl dp str_non_ter str_prio =
    let rec aux i s =
      let s1 = if dp=i then s^"." else s in
      if i=Array.length litl then s1
      else aux (i+1) (s1^(str_literal litl.(i) str_non_ter str_prio)^" ")
    in
    aux 0 ""

    (*| [] when dp = 0 -> "."
    | [] -> ""
    | t::q -> if dp=0 then ("."^(str_literal t)^" "^(str_handle q (dp-1)))
      else ((str_literal t)^" "^(str_handle q (dp-1))) *)

  let str_token_set tns =
    let f tn str = str^(str_token_name tn)^"," in
    let str = TNS.fold f tns "" in
    if str = "" then "" else
    let string_length = (String.length str) in
    String.sub str 0 (string_length-1)

  (*let print_item ((nt,litl,length),dp) (tns:TNS.t) =
    Printf.fprintf !log_channel "   %s -> %s, (%s) ; length=%d\n" (str_lhs nt)
      (str_handle litl dp) (str_token_set tns) length*)
  let print_reducible chan gram_rhs lhs_table str_non_ter str_prio (rn,tns) =
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s, (%s)\n"
      (str_lhs lhs str_non_ter str_prio)
      (str_handle rhs (Array.length rhs) str_non_ter str_prio) (str_token_set tns)

  let print_kernel chan gram_rhs lhs_table str_non_ter str_prio (rn,dp) =
    let lhs,rhs = lhs_table.(rn), gram_rhs.(rn) in
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs lhs str_non_ter str_prio) (str_handle rhs dp str_non_ter str_prio)

  let print_non_kernel chan gram_rhs lhs_table str_non_ter str_prio rn =
    print_kernel chan gram_rhs lhs_table str_non_ter str_prio (rn,0)

  let print_item chan lhs_of_ind ind str_non_ter str_prio (rhs,dp) =
    Printf.fprintf chan "   %s -> %s\n"
      (str_lhs (lhs_of_ind.(ind)) str_non_ter str_prio)
      (str_handle rhs dp str_non_ter str_prio)

end



module Automaton_make(Gr:Grammar_type)
  (Ap:Automaton_parameters with type non_terminal=Gr.non_terminal
    and type lit_nt=Gr.lit_nt with type lhs=Gr.lhs) =
struct
open Ap
open Gr

  let token_epsilon = 0

(** this type is used to construct the automaton. Each state in the automaton
    has a field of type [lit_trans] which is the literal of transition to
    this state. The difference with the type [literal] is that there is no
    priority attached to the non terminals. *)
type lit_trans = non_terminal pliteral
(*  | Ter of token_name
  | Non_ter of non_terminal*)

let lit_trans (lit:lit_nt pliteral) = match lit with
  | Non_ter nt -> Non_ter (nt_of_lit nt)
  | Ter t -> Ter t



(** [int] is the length of the [literal] list *)
(* type rule_bis = non_terminal_name * (literal list) * priority * int *)
(* the marker rule_kind is used by the parser. The parser checks the lookahead token to know if it can reduce only in the case of classic rule because dynamic ones can add new rules which may make the lookahead token condition irrelevant *)

(*module Grammar_module = Grammar_struct(Ntt)
include Grammar_module*)



module Ordered_item_set =
struct
  type t = item_set * int (* int is the number of items *)
  let compare (is1,n1) (is2,n2) =
    if n1>n2 then 1
    else if n2>n1 then -1
    else Pervasives.compare
      (is1.reducible,is1.kernel_nt,is1.kernel_t)
      (is2.reducible,is2.kernel_nt,is2.kernel_t)
end

module Map_is = Map.Make(Ordered_item_set)


(*module OrdLi =
struct
  type t = lit_trans
  let compare = Pervasives.compare
end
module Li_map = Map.Make(OrdLi)*)


  (** [li] is the literal of transition from the previous states *)
module rec State :
sig
  type state = {
    number : int;
    li : lit_trans;
    items : item_set;
    mutable succ_states : state list;
    (*mutable pred_states : State.State_set.t Li_map.t*)
  }
  module State_set : Set.S with type elt = state
end
=
struct
  type state = {
      number : int;
      li : lit_trans;
      items : item_set;
      mutable succ_states : state list;
      (*mutable pred_states : State.State_set.t Li_map.t*)
    }
  module State_set =
  struct
    module Ordered_States =
    struct
      type t = state
      let compare s1 s2 = compare_is s1.items s2.items
    end
    module State_set_prime = Set.Make(Ordered_States)
    include (State_set_prime:Set.S with type elt = state)
  end
end

include State

(*let add_edge n1 n2 =
  n1.succ_states <- Li_map.add n2.li n2 n1.succ_states*)(*;
  let state_set = try Li_map.find n1.li n2.pred_states
    with Not_found -> State_set.empty in
  let state_set = State_set.add n1 state_set in
  n2.pred_states <- Li_map.add n1.li state_set n2.pred_states*)

(*let mem_edge s1 s2 =
  Li_map.mem s2.li s1.succ_states*)


(*module Ordered_literal =
struct
  type t = literal
  let compare = Pervasives.compare
end

module Map_lit = Map.Make(Ordered_literal)*)
(* ATTENTION : on s'en sert dans first_on_literal pour la structure
first_memo, on distingue un non terminal avec deux non_terminal_priority
alors que ça ne sert à rien. Il faut : soit ne pas distinguer les deux,
soit s'en servir pour restreindre les lookahead set en utilisant les
priorités. *)




(** Assign unique numbers to states. *)
(*let number_aut (s0:state) =
  let f s (n:int) visited_v =
    if State_set.mem s visited_v then (n,visited_v,Li_map.empty) else
      ((*s.number <- n;*)
      (n+1,State_set.add s visited_v,s.succ_states))
  in
  let rec map_succ _ state (n,visited_v) =
    let n,visited_v,v_map = f state n visited_v in
    Li_map.fold map_succ v_map (n,visited_v)
  in
  let n,visited_v,vertex_map = f s0 0 State_set.empty in
  let n,visited_v = Li_map.fold map_succ vertex_map (n,visited_v) in
  n*)


(** STRING functions used to print the states of the automaton *)


let str_literal_trans lit str_non_ter str_prio = match lit with
  | Ter tk -> str_token_name tk
  | Non_ter nt -> str_non_terminal nt str_non_ter str_prio

let rec str_tok_list ll str_non_ter = match ll with
  | [] -> ""
  | [tok] -> str_literal (Ter tok) str_non_ter [||]
  | tok::tl -> (str_literal (Ter tok) str_non_ter [||])^","^
      (str_tok_list tl str_non_ter)


let str_state_succ sl str_non_ter str_prio =
  let f str state =
    str^" ["^(string_of_int state.number)^","^
    (str_literal_trans state.li str_non_ter str_prio)^"]"
  in
  List.fold_left f "" sl

(*let str_state_pred sl =
  let f lit state_set str =
    let f2 state str =
      str^" ["^(string_of_int state.number)^","^(str_literal_trans lit)^"]"
    in
    State_set.fold f2 state_set str
  in
  Li_map.fold f sl ""*)



let print_item_set chan is gram_rhs lhs_table lhs_of_ind str_non_ter str_prio =
  Printf.fprintf chan "  nb of items: %d\n"
  ((List.length is.reducible)+(List.length is.kernel_nt)+
  (List.length is.kernel_t)+(List.length is.non_kernel));
  List.iter (print_reducible chan gram_rhs lhs_table str_non_ter str_prio)
    is.reducible;
(*   Printf.fprintf chan "test1\n"; *)
  List.iter (print_kernel chan gram_rhs lhs_table str_non_ter str_prio) is.kernel_t;
(*   Printf.fprintf chan "test2\n"; *)
  List.iter (print_kernel chan gram_rhs lhs_table str_non_ter str_prio) is.kernel_nt;
(*   Printf.fprintf chan "test3\n"; *)
  List.iter (print_non_kernel chan gram_rhs lhs_table str_non_ter str_prio)
    is.non_kernel
(*   Printf.fprintf chan "test4\n" *)

    (*;
  Printf.fprintf !log_channel "  nt to add:\n";
  let f i =
    Printf.fprintf !log_channel "   %s\n" (str_lhs lhs_of_ind.(i));
  in
  Int_set.iter f is.nt_to_add*)

let print_state s gram_rhs lhs_table lhs_of_ind str_non_ter str_prio =
  Printf.fprintf !log_channel " State %d\n" s.number;
  Printf.fprintf !log_channel "  li : %s\n"
    (str_literal_trans s.li str_non_ter str_prio);
  Printf.fprintf !log_channel "  items :\n";
  print_item_set !log_channel s.items gram_rhs lhs_table lhs_of_ind
    str_non_ter str_prio;
  Printf.fprintf !log_channel "  next states : %s\n"
    (str_state_succ s.succ_states str_non_ter str_prio)(*;
  Printf.fprintf !log_channel "  previous states : %s\n" (str_state_pred s.pred_states)*)

let rec str_lit_list litl str_non_ter str_prio = match litl with
  | [] -> ""
  | lit::tl -> (str_literal lit str_non_ter str_prio)^" "^
      (str_lit_list tl str_non_ter str_prio)

let str_rule (nt,litl,_) str_non_ter str_prio =
  (str_lhs nt str_non_ter str_prio)^" -> "^(str_lit_list litl str_non_ter str_prio)

let rec print_rule_list rl str_non_ter str_prio = match rl with
  | [] -> ()
  | r::t -> Printf.fprintf !log_channel " %s\n"
    (str_rule r str_non_ter str_prio); print_rule_list t str_non_ter str_prio


(** Used to print the content of an automaton. *)
let print_aut v0 gram_rhs lhs_table lhs_of_ind str_non_ter str_prio =
  output_string !log_channel "\n"; output_string !log_channel "\n";
  output_string !log_channel
"----------------------------------- Automaton ----------------------------------\n";
  output_string !log_channel "\n";
  let f s visited_v =
    if State_set.mem s visited_v then (visited_v,[]) else
      (print_state s gram_rhs lhs_table lhs_of_ind str_non_ter str_prio;
      print_newline ();
      (State_set.add s visited_v,s.succ_states))
  in
  let rec map_succ visited_v state  =
    let visited_v,v_map = f state visited_v in
    List.fold_left map_succ visited_v v_map
  in
  let visited_v,vertex_map = f v0 State_set.empty in
  List.fold_left map_succ visited_v vertex_map

let print_map m gram_rhs lhs_table lhs_of_ind str_non_ter str_prio =
  let f (is,_) s =
    let () = Printf.fprintf !log_channel "state %d\n" (s.number) in
    print_item_set !log_channel is gram_rhs lhs_table lhs_of_ind str_non_ter str_prio
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


let compute_first first_memo grammar nt_nb prio_dat array_nt_prio lhs_of_ind =
  let ind_of_lit nt_lit = ind_of_nt (nt_of_lit nt_lit) in

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
    let ind = ind_of_lit nt in
    first_memo.(ind) <- Some tns;
    tns

  and first_on_litl litl p la vrules =
    let rec aux i tns =
      if i=Array.length litl then match la with
        | Some tok -> First.add tok tns
        | None -> First.add_epsilon tns
      else match litl.(i) with
        | Ter ter -> (First.add ter tns)
        | Non_ter nt ->
          let tns2 =
            match first_memo.(ind_of_lit nt) with
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
    let nt = nt_of_lhs (lhs_of_ind.(i)) in
    let j = ind_of_nt nt in
    match first_memo.(j) with
      | None ->
          let nt_lit = lit_of_nt nt in
          first_memo.(j) <- Some (first_on_nt nt_lit RbS.empty)
      | _ -> ()
  done



let first litl la p first_memo =
  let ind_of_lit nt_lit = ind_of_nt (nt_of_lit nt_lit) in

  let first_on_litl litl p la vrules =
    let rec aux i tns =
      if i=Array.length litl then match la with
        | Some tok -> First.add tok tns
        | None -> tns (*TNS.add token_epsilon tns*)
      else match litl.(i) with
        | Ter ter -> (First.add ter tns)
        | Non_ter nt ->
          let tns2 =
            match first_memo.(ind_of_lit nt) with
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



(* AUTOMATON CREATION *)

let countst = ref 0
(** This ref counts the number of state creations. *)
let count_trans = ref 0
(** This ref counts the number of transitions between states. *)

(* ---------------------------- LR(1) automaton ---------------------------- *)

(*

let nonkernel_items (lit:literal) (gram:('a,'b,'c) grammar) (tns:TNS.t) prio_dat array_nt_prio nt_nb =
  match lit with
  | Non_ter nt ->
      let f lhs litl _ items_set =
        IS.add (ind_of_lhs lhs) (litl,0) tns items_set
      in
      let lhs_list = select_lhs nt prio_dat array_nt_prio in
      let f2 is lhs = Map_rhs.fold (f lhs) gram.(ind_of_lhs lhs) is in
      List.fold_left f2 (IS.make nt_nb) lhs_list
  | Ter _ -> failwith "Non terminal expected"

let closure (is:IS.t) (first_memo:First.t option array) (gram:('a,'b,'c) grammar) prio_dat array_nt_prio nt_nb =
  let aux2 _ (litl,dp) (tns:TNS.t) its =
    try
      let a1 = litl.(dp) in
      match lit_trans a1 with
        | Ter _ -> (IS.make nt_nb)
        | Non_ter a1nt ->
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
      let lit = lit_trans rhs.(dp) in
      let is =
        try Map_lit_trans.find lit lit_map
        with Not_found -> (IS.make nt_nb),0
      in
      let new_is = (IS.add ind (rhs,dp+1) tns (fst is)),((snd is)+1) in
      Map_lit_trans.add lit new_is lit_map
  in
  let lit_map = IS.fold f1 s.items Map_lit_trans.empty in
  let f2 (lit:lit_trans) (is:IS.t * int) vl =
    count_trans := !count_trans+1;
    try
      let v1 = match lit with
        | Ter t -> Map_is.find is is_trace_tok.(t)
        | Non_ter nt -> Map_is.find is is_trace_nt.(ind_of_nt nt)
      in
      add_edge s v1;
      vl
    with Not_found ->
      let () = countst := (!countst+1) in
      let is1 = closure (fst is) first_memo gram prio_dat array_nt_prio nt_nb in
      let v1 = {
        li = lit;
        items = is1;
        number = !countst;
        succ_states = Li_map.empty;
        pred_states = Li_map.empty
      } in
      add_edge s v1;
      let () = match lit with
        | Ter t -> is_trace_tok.(t) <- Map_is.add is v1 is_trace_tok.(t)
        | Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
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
    | Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
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



let closure_v0_LR0 (is:item_set) gram_rhs gram_lhs nt_to_add =
  let f ind (nk,red) =
    match gram_lhs.(ind) with
      | rn_l, None -> (rn_l@nk,red)
      | rn_l, (Some rn) -> (rn_l@nk,(rn,TNS.empty)::red)
  in
  let non_kernel,reducible = Int_set.fold f nt_to_add ([],is.reducible) in
  is.reducible <- is.reducible@reducible;
  is.non_kernel <- is.non_kernel@non_kernel(*;
  is.nt_to_add <- Int_set.empty*)


(* array_lhs is still not used, it should be ysed to avoid calling
Int_set.add too many times in f1. Then we could give up the Int_set for a list.
array_lhs should be useful for both prio in automaton and prio at runtime. *)
let move_LR0 s is_trace gram_rhs gram_lhs r_L prio_dat array_nt_prio lhslists array_lt_ter array_lt_nt array_nt array_lhs =
  let is_trace_tok,is_trace_nt = is_trace in

  let f1 lt_list (rn,dp) =
    let rhs = gram_rhs.(rn) in
    let is,lt_list = match rhs.(dp) with
      | Ter t -> (match array_lt_ter.(t) with
        | None ->
            let is = new_item_set () in
            array_lt_ter.(t) <- Some is;
            is,(lit_trans rhs.(dp))::lt_list
        | Some is -> is,lt_list)
      | Non_ter nt -> (match array_lt_nt.(ind_of_nt (nt_of_lit nt)) with
        | None ->
            let is = new_item_set () in
            array_lt_nt.(ind_of_nt (nt_of_lit nt)) <- Some is;
            is,(lit_trans rhs.(dp))::lt_list
        | Some is -> is,lt_list)
    in
    let () =
      if dp+1 = Array.length rhs then
        is.reducible <- (rn,TNS.empty)::is.reducible
      else
      match rhs.(dp+1) with
        | Ter _ -> is.kernel_t <- (rn,dp+1)::is.kernel_t
        | Non_ter nt ->
            (is.kernel_nt <- (rn,dp+1)::is.kernel_nt)
    in
    lt_list
  in

  let lt_list = List.fold_left f1 [] s.items.kernel_nt in
  let lt_list = List.fold_left f1 lt_list s.items.kernel_t in
  let f3 lt_list rn = f1 lt_list (rn,0) in
  let lt_list = List.fold_left f3 lt_list s.items.non_kernel in

  let f2 vl (lit:lit_trans) =
    let is = match lit with
      | Ter t -> (match array_lt_ter.(t) with
          Some is -> is | None -> assert false)
      | Non_ter nt -> (match array_lt_nt.(ind_of_nt nt) with
          Some is -> is | None -> assert false)
    in

    count_trans := !count_trans+1;
    let it_nb =
      (List.length is.reducible)
      + (List.length is.kernel_nt)
      + (List.length is.kernel_t)
    in
    let old_reducible = is.reducible in
    (* epsilon rules may be added to reducible by closure_LR0 *)
    try
      let v1 = match lit with
        | Ter t -> Map_is.find (is,it_nb) is_trace_tok.(t)
        | Non_ter nt -> Map_is.find (is,it_nb) is_trace_nt.(ind_of_nt nt)
      in
      s.succ_states <- v1::s.succ_states;
      vl
    with Not_found ->
      countst := (!countst+1);
      let f1 (nt_to_add,nt_list) (rn,dp) = match gram_rhs.(rn).(dp) with
        | Non_ter nt ->
            let lhs_l,nt_list =
              if test_array_nt array_nt nt then [],nt_list
              else (comp_lhslist nt lhslists prio_dat array_nt_prio),
                nt::nt_list
            in
            let g1 nt_to_add ind =
              Int_set.add ind nt_to_add
            in
            let g2 nt_to_add lhs =
              let ind_list = r_L.(ind_of_lhs lhs) in
              List.fold_left g1 nt_to_add ind_list
            in
            (List.fold_left g2 nt_to_add lhs_l), nt_list
        | Ter _ -> assert false
      in
      let nt_to_add, nt_list =
        List.fold_left f1 (Int_set.empty,[]) is.kernel_nt
      in
      clean_array_nt array_nt nt_list;
      let f ind (nk,red) = match gram_lhs.(ind) with
        | rn_l, None -> (rn_l@nk,red)
        | rn_l, (Some rn) -> (rn_l@nk,(rn,TNS.empty)::red)
      in
      let non_kernel,reducible =
        Int_set.fold f nt_to_add ([],is.reducible)
      in
      is.reducible <- reducible;
      is.non_kernel <- non_kernel;
      let v1 = {
        li = lit;
        items = is;
        number = !countst;
        succ_states = [];
        (*pred_states = Li_map.empty*)
      } in
      let old_is = { is with reducible = old_reducible } in
      let () = match lit with
        | Ter t -> is_trace_tok.(t) <-
            Map_is.add (old_is,it_nb) v1 is_trace_tok.(t)
        | Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
            Map_is.add (old_is,it_nb) v1 is_trace_nt.(ind_of_nt nt)
      in
      s.succ_states <- v1::s.succ_states;
      v1::vl
  in
  let vl = List.fold_left f2 [] lt_list in
  let clear_array lit = match lit with
    | Ter t -> array_lt_ter.(t) <- None
    | Non_ter nt -> array_lt_nt.(ind_of_nt nt) <- None
  in
  List.iter clear_array lt_list;
  vl





(* on n'a pas besoin de nt_nb, c'est la longueur de gram_lhs  *)

let build_automaton_LR0 (is0:item_set) (lit0:lit_trans) is_trace (gram_rhs:rhs array) (gram_lhs:((int list) * (int option)) array) prio_dat it_nb array_nt_prio lhs_of_ind lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio =

  let array_lt_ter = Array.make token_nb None in
  let array_lt_nt = Array.make ist_nt_nb None in

  Gc.set { (Gc.get()) with Gc.space_overhead = 100 };

  let array_nt = Array.make (array_nt_size ist_nt_nb) false in
  let array_lhs = Array.make (Array.length gram_lhs) false in
  (* May be replaced by array of int 30 times shorter *)

  let rec map_succ (is_trace,state_list) v =
    (*Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table lhs_of_ind str_non_ter str_prio;
    Printf.fprintf !log_channel "\n";
    flush_all ();*)
    (*Printf.printf "build state %d\n" v.number;
    flush_all ();*)
    let vl =
      move_LR0 v is_trace gram_rhs gram_lhs r_L prio_dat
        array_nt_prio lhslists array_lt_ter array_lt_nt
        array_nt array_lhs
    in
    List.fold_left map_succ (is_trace,v::state_list) vl
  in
  closure_v0_LR0 is0 gram_rhs gram_lhs nt_to_add;
  let v0 = {
    li = lit0;
    items = is0;
    number = !countst; (* should always be 0 *)
    succ_states = [];
    (*pred_states = Li_map.empty*)
  } in
  let is_trace_tok,is_trace_nt = is_trace in
  let () = match lit0 with
    | Ter t -> is_trace_tok.(t) <- Map_is.add (is0,it_nb) v0 is_trace_tok.(t)
    | Non_ter nt -> is_trace_nt.(ind_of_nt nt) <-
        Map_is.add (is0,it_nb) v0 is_trace_nt.(ind_of_nt nt)
  in
  (*let is_trace = Map_is.add is0 v0 is_trace in*)
  let is_trace,state_list =
    List.fold_left map_succ (is_trace,[v0]) [v0]
  in
  Gc.set { (Gc.get()) with Gc.space_overhead = 80 };
  (*let _ = print_aut v0 gram_rhs lhs_table lhs_of_ind in*)
  is_trace,state_list

let build_automaton_LALR (is0:item_set) (lit0:lit_trans) is_trace gram_rhs gram_lhs prio_dat it_nb array_nt_prio nt_of_ind lhs_of_ind lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio =
  (*let time1 = Sys.time () in
  compute_first first_memo gram nt_nb prio_dat array_nt_prio lhs_of_ind;
  let time2 = Sys.time () in
  Printf.printf "first computed, %.3f sec\n" (time2-.time1);*)
  (*flush_all ();*)
  let is_trace,state_list =
    build_automaton_LR0 is0 lit0 is_trace gram_rhs
    gram_lhs prio_dat it_nb array_nt_prio lhs_of_ind lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio
  in

  state_list


let build_automaton automaton_kind (is0:item_set) (lit0:lit_trans) is_trace gram_rhs gram_lhs verbose prio_dat it_nb array_nt_prio nt_of_ind lhs_of_ind lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio =
  countst := 0;
  count_trans := 0;
  let time1 = Sys.time () in
  let state_list = match automaton_kind with
    | `LR0 ->
        let _,state_list =
          build_automaton_LR0 is0 lit0 is_trace gram_rhs gram_lhs
            prio_dat it_nb array_nt_prio lhs_of_ind lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio
        in state_list
    | `LALR -> build_automaton_LALR is0 lit0 is_trace
        gram_rhs gram_lhs prio_dat it_nb array_nt_prio nt_of_ind lhs_of_ind
        lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio
    | `LR1 ->
        print_endline "LR(1) automaton currently not available, `LALR(1) is chosen.";
        build_automaton_LALR is0 lit0 is_trace
        gram_rhs gram_lhs prio_dat it_nb array_nt_prio nt_of_ind lhs_of_ind
        lhslists r_L lhs_table ist_nt_nb token_nb nt_to_add str_non_ter str_prio
  in
  let time2 = Sys.time () in
  if verbose>0 then
    (let str_aut_kind = match automaton_kind with
      `LR0 -> "LR(0)" | `LALR -> "`LALR(1)" | `LR1 -> "LR(1)" in
    Printf.fprintf !log_channel "%s automaton built, %d states, %d transitions, %.3f sec\n"
    str_aut_kind (!countst+1) (!count_trans+1) (time2-.time1);
    (*let _ = print_aut v0 lhs_of_ind in*)
    flush stdout) else ();
  state_list,!countst





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
end

