module type Dyp_parameters_type =
sig
  val str_token_name : int -> string
    (** Makes possible to display relevant error messages about
        tokens' names and rules *)

  val entry_points : (int * int) list
    (** These are the entry points of the grammar *)
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

let list_map f l = List.rev (List.rev_map f l)

include Automaton
open Dyp_special_types

let default_priority = 0


type ('obj,'data,'local_data) dypgen_toolbox = {
  global_data : 'data;
  local_data : 'local_data;
  last_local_data : 'local_data;
  priority_data : priority_data;
  (*mutable add_rules : (rule * (
    ('obj,'data,'local_data) dypgen_toolbox -> 'obj list -> 'obj)) list;
  mutable remove_rules : rule list;
  mutable will_shift : bool;
  mutable keep_grammar : bool;
  mutable next_state : out_channel option;
  mutable next_grammar : out_channel option;*)
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
exception Undefined_nt of string
exception Bad_constructor of (string * string * string)
exception Constructor_mismatch of (string * string)
exception Syntax_error
    (** This exception is raised by glrParse if the parser is stuck in a
    situtation where no shift and no reduction is possible. *)


(** The way this module handles GLR is documented in the following paper :
Scott McPeak, Elkhound: A fast, practical GLR parser generator, University of
California Berkeley, Report No. UCB/CSD-2-1214, December 2002 ; figures 7 and 8.
Optimizations which are specific to Elkhound are not implemented.

This implementation adds dynamic changes to the grammar at run-time during
parsing. These modifications take place in the function reduceViaPath.

When a reduction is performed the parser checks whether the action is 'classic'
(no change to the grammar) or 'dynamic' and proceeds with the modifications to
the grammar and the automaton if needed. If the parsing follows simultaneously
several paths, as it may be the case with GLR parsing, then there is a distinct
grammar and a distinct automaton for each path if needed.

Following Scott McPeak's report the stack is implemented as a graph.
*)
module Make_dyp(E:Dyp_parameters_type) =
struct
open E


let countred = ref 0
(* counts the number if reductions performed
it is only used for information *)

type counters = {
  countsn : int;
  counted : int;
  count_token : int;
  count_g : int } (* counts number of grammars *)

(* open Gs *)

let non_terminal_startprime = 0


module Ordered_urule =
struct
  type t = rule
  let compare = Pervasives.compare
end

module Urule_map = Map.Make(Ordered_urule)
type ('a,'b,'c) user_grammar = (('a,'b,'c) action) list Urule_map.t

module type Other_parameters =
sig
  type non_terminal
  type lit_nt
  type lhs
  (*val str_non_terminal : non_terminal -> string*)
  (*val select_rule : priority_data -> lit_nt -> non_terminal -> bool*)
  (*val select_lhs : lit_nt -> priority_data -> (int Prio_map.t) array -> lhs list*)
  val lhs_list : lit_nt -> priority_data -> (int Prio_map.t) array -> lhs list
  type lhslists
  val lhslists_init : int -> lhslists
  val comp_lhslist : lit_nt -> lhslists -> priority_data -> (int Prio_map.t) array -> lhs list
  (*val lhsl_of_non_ter : non_ter -> (int Prio_map.t) array -> lhs list*)
  val lhs_startprime : lhs
  val non_ter_of_nt : non_terminal -> int
  val non_ter_of_lhs : lhs -> int
  val dummy_lhs : lhs
  type ('a,'b,'c) grammar
  type rhs
  module Map_rhs : Map.S with type key = rhs
  val make_real_grammar : ('a,'b,'c) user_grammar -> priority_data ->
    string array ->
    (((('a,'b,'c) action) list Map_rhs.t) array * (int Prio_map.t) array *
     int * int * non_terminal array * int array *
     int array * lhs array * int)
  type prio_imprint
  type item_set
  val new_prio_imprint : priority_data -> prio_imprint -> item_set ->
    (non_terminal pliteral) -> priority -> rhs array -> lhs array ->
    lhs array -> string array -> string array -> (prio_imprint * bool)
  val change_rn : prio_imprint -> item_set -> int Int_map.t ->
    (prio_imprint * item_set)
  val print_prio_imp : out_channel -> rhs array -> lhs array ->
    prio_imprint -> string array -> string array -> unit
  val default_prio_imp : prio_imprint
  val prio_imp_equal : prio_imprint -> prio_imprint -> bool
  (*val prev_is : item_set -> rhs array -> item_set*)
  type rule_bis
  val entry_point_prio :
    non_ter -> prio_imprint -> rhs array ->
    (non_terminal pliteral) -> priority
  val prio_imprint_check : int -> rhs array -> prio_imprint -> bool
  val str_with_priority : priority -> string array -> string
end

module NTS = Set.Make(Ordered_non_ter)


module Tools =
struct
  let automaton_kind = ref `LR0

  type ('global_data,'local_data) data_equal = {
    global_data_equal : 'global_data -> 'global_data -> bool;
    local_data_equal : 'local_data -> 'local_data -> bool }

  let add_nt (s:string) (cons:string)
      (datadyn:(Dyp_special_types.datadyn ref)) =
    let { nt_map=nt_map; cons_map=cons_map;
      nt_num=nt_nb; cons_nb=cons_nb } = !datadyn
    in
    try
      let nt,old_cons_i,old_cons =
        String_map.find s nt_map
      in
      if cons<>old_cons
      then raise (Constructor_mismatch(old_cons,cons))
      else nt
    with Not_found ->
      (let cons_map,cons_nb,cons_i =
        try
          let cons_i = String_map.find cons cons_map in
          cons_map, cons_nb, cons_i
        with Not_found ->
          String_map.add cons cons_nb cons_map,
          cons_nb+1, cons_nb
      in
      datadyn :=
        { nt_map = String_map.add s (nt_nb,cons_i,cons) nt_map;
          cons_map = String_map.add cons cons_i cons_map;
          nt_num = nt_nb+1;
          cons_nb = cons_nb };
      nt_nb)

  let find_nt s datadyn =
    let nt,_,cons = String_map.find s datadyn.nt_map in nt,cons


  let init_datadyn ntl consl =
    let aux1 (nt_map,n) (nt,cons_i,cons) =
      (String_map.add nt (n,cons_i,cons) nt_map),(n+1)
    in
    let nt_map, nt_nb =
      List.fold_left aux1 (String_map.empty,1) ntl
    in
    let aux2 (cons_map,i) cons =
      (String_map.add cons i cons_map),(i+1)
    in
    let cons_map, cons_nb =
      List.fold_left aux2 (String_map.empty,0) consl
    in
    { nt_map = nt_map; cons_map = cons_map;
    nt_num = nt_nb; cons_nb = cons_nb }


  (*let init_merge_map (mnl:('obj merge_function * int) list) =
    let aux mm (mf,nt) = Nt_map.add nt mf mm in
    List.fold_left aux Nt_map.empty mnl*)

  let empty_datadyn = {
    nt_map = String_map.empty;
    cons_map = String_map.empty;
    nt_num = 1;
    (* there is always at least one nt : S', which is not in
    nt_map, but this is not very relevant, empty_datadyn is just
    used in pgen. *)
    cons_nb = 0 }

  (*let empty_merge_map = Nt_map.empty*)

  type ('obj,'gd,'ld) action_com = {
    ac_gd : 'gd;
    ac_ld : 'ld;
    ac_pdat : priority_data;
    add_rules : (rule * (('obj,'gd,'ld) dypgen_toolbox ->
               ('obj list -> 'obj * ('obj,'gd,'ld) dyp_action list))) list;
    remove_rules : rule list;
    will_shift : bool;
    keep_grammar : bool;
    next_state : out_channel option;
    next_grammar : out_channel option }

  let rec action_command dal da = match dal with
    | [] -> da
    | (Global_data gd)::t -> action_command t { da with ac_gd = gd }
    | (Local_data ld)::t -> action_command t { da with ac_ld = ld }
    | (Priority_data pdat)::t -> action_command t { da with ac_pdat = pdat }
    | (Add_rules ar)::t -> action_command t { da with add_rules = ar }
    | (Remove_rules rr)::t -> action_command t { da with remove_rules = rr }
    | (Will_shift ws)::t -> action_command t { da with will_shift = ws }
    | (Keep_grammar kg)::t -> action_command t { da with keep_grammar = kg }
    | (Next_state ns)::t -> action_command t { da with next_state = Some ns }
    | (Next_grammar ng)::t -> action_command t { da with next_grammar = Some ng }

  let rec transform_action a =
    Dyp_special_types.Dypgen_action(fun av_list symbol_pos
    position_list data_arg datadyn local_data_arg last_local_data_arg
    prio_data debug_infos ->
      let __dypgen_datadyn = ref datadyn in
      let dyp = {
        global_data = data_arg;
        local_data = local_data_arg;
        last_local_data = last_local_data_arg;
        priority_data = prio_data;
        symbol_start = (fun () -> (fst symbol_pos).Lexing.pos_cnum);
        symbol_start_pos = (fun () -> fst symbol_pos);
        symbol_end = (fun () -> (snd symbol_pos).Lexing.pos_cnum);
        symbol_end_pos = (fun () -> snd symbol_pos);
        rhs_start = (fun i -> (fst (List.nth position_list (i-1))).Lexing.pos_cnum);
        rhs_start_pos = (fun i -> fst (List.nth position_list (i-1)));
        rhs_end = (fun i -> (snd (List.nth position_list (i-1))).Lexing.pos_cnum);
        rhs_end_pos = (fun i -> snd (List.nth position_list (i-1)));
        add_nt = (fun nt cons -> add_nt nt cons __dypgen_datadyn);
        find_nt = (fun (s:string) -> find_nt s !__dypgen_datadyn);
        print_state =
          debug_infos.Dyp_special_types.prt_state;
        print_grammar =
          debug_infos.Dyp_special_types.prt_grammar
      }
      in
      let new_obj,dal = a dyp av_list in
      let mapfun (r,ac) = (r,(transform_action ac)) in
      let dyp_a = action_command dal {
        ac_gd = data_arg;
        ac_ld = local_data_arg;
        ac_pdat = prio_data;
        add_rules = [];
        remove_rules = [];
        will_shift = true;
        keep_grammar = false;
        next_state = None;
        next_grammar = None }
      in
      let add_rules_transformed = List.map mapfun dyp_a.add_rules in
      (new_obj,dyp_a.will_shift,dyp_a.keep_grammar,dyp_a.ac_gd,!__dypgen_datadyn,
      dyp_a.ac_ld,add_rules_transformed,dyp_a.remove_rules,
      dyp_a.ac_pdat,dyp_a.next_state,dyp_a.next_grammar))

  let keep_zero l = match l with
    | (o,gd,ld)::_ -> [], gd, ld
    | [] -> assert false

  let array_of_list =
    let rec aux a l n = match l with
      | [] -> a
      | h::t -> a.(n) <- h; aux a t (n+1)
    in
    fun l -> match l with
     | [] -> [||]
     | h::_ -> let a = Array.make (List.length l) h in aux a l 0

end

open Tools

module Parser(Gr:Grammar_type)(F:Other_parameters with type non_terminal = Gr.non_terminal and type lit_nt = Gr.lit_nt and type ('a,'b,'c) grammar = ('a,'b,'c) Gr.grammar and type lhs=Gr.lhs and type item_set = Gr.item_set and type rule_bis = Gr.rule_bis and type rhs = Gr.rhs) =
struct

open F
open Gr



module Aut_param =
struct
  type non_terminal = Gr.non_terminal
  type lit_nt = Gr.lit_nt
  type lhs = Gr.lhs
(*   let str_priority p = priority_names.(p) *)
  (*let token_epsilon = E.token_epsilon*)
  (*let select_rule = F.select_rule*)
  (*let select_lhs = F.select_lhs*)
  let lhs_list = F.lhs_list
  type lhslists = F.lhslists
  let lhslists_init = F.lhslists_init
  let comp_lhslist = F.comp_lhslist
  (*let lhsl_of_non_ter = F.lhsl_of_non_ter*)
end

module Automat = Automaton_make(Gr)(Aut_param)
open Automat
open Aut_param


type ('a,'b,'c) grammar = ('a,'b,'c) Gr.grammar

(* ******* CE QUI SUIT EST A DEPLACER DANS UN AUTRE FICHIER ****** *)




(*module Ordered_ntcouple =
struct
  type t = non_ter * non_ter
  let compare = Pervasives.compare
end
module Map_ntc = Map.Make(Ordered_ntcouple)*)

(*module OrdPrioNb =
struct
  type t = int * int
  let compare = Pervasives.compare
end
module PrioNb_set = Set.Make(OrdPrioNb)*)


type deb_rhs = rhs
type deb_lhs = lhs
type ('obj,'data,'local_data) deb_action = ('obj,'data,'local_data) action
type ('obj,'data,'local_data) deb_user_grammar = ('obj,'data,'local_data) user_grammar
type deb_item_set = item_set
type deb_lit_trans = lit_trans
type deb_lhslists = lhslists
type deb_non_terminal = non_terminal


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
  deb_datadyn : datadyn;
  deb_aut_kind : automaton_kind;
  deb_nt_nb : int;
  deb_non_ter_of_ind : int array;
  deb_prio_of_ind : int array;
  deb_nt_of_ind : deb_non_terminal array;
  deb_lhs_of_ind : deb_lhs array;
  deb_str_non_ter : string array;
  deb_cons_of_nt : int array;
  deb_str_prio : string array
}

type ('obj,'data,'local_data) parsing_device = {
  gram_lhs : ((int list) * (int option)) array;
  gram_rhs : rhs array;
  lhs_table : lhs array;
  actions : ('obj,'data,'local_data) action list array;
  g_nb : int; (* grammar number *)
  user_g : ('obj,'data,'local_data) user_grammar;
  table : int array array array;
  (* 1st index for state
  2nd index: 0 for terminals, 1 for non terminals
  3rd index to choose which terminal or nt. *)
  table_it : item_set array;
  table_lit_trans : lit_trans array;
  lhslists : lhslists;
  po : bool option array array;
    (** Patial order between non terminal name couples. A couple of non
     terminals is bound to a bool. This may be implemented alternatively
     with a set : the presence of a couple (nt1,nt2) in the set would
     denote that nt1<=nt2 is true. This would save memory space. *)
  data : 'data;
  prio : priority_data;
  local_data : 'local_data;
  datadyn : datadyn;
  (*global_merge : 'obj merge_function;*)
  aut_kind : automaton_kind;
  nt_nb : int;
    (* number of non terminals in the internal grammar
     (as opposed to the user grammar. *)
  non_ter_of_ind : int array;
  prio_of_ind : int array;
  nt_of_ind : non_terminal array;
  lhs_of_ind : lhs array;
  str_non_ter : string array;
  cons_of_nt : int array;
  str_prio : string array
}

(*type saved_parsing_device = {
  sa_init : state;
  sa_po : bool Map_ntc.t
}*)

module Ordered_rule =
struct
  type t = rule
  let compare = Pervasives.compare
end

module RS = Set.Make (Ordered_rule)

(* This function decides if the non terminal nt1 can derive the non terminal
nt2 and stores the results in po_array. If it needed to decide whether another
non terminal nt can derive nt2 then it stores it too in po_array. nt_epsilon
stores the non terminals which can derive epsilon.*)
let derive nt1 nt2 user_g po_array nt_epsilon =

  (*let rec derive_epsilon nt nt_epsion vr =*)
  (* il semble que ça marchait malgré cette faute de frappe (nt_epsion) *)
  let rec derive_epsilon nt vr =
    (*try (Nt_map.find nt nt_epsilon),nt_epsilon with Not_found ->*)
    match nt_epsilon.(nt) with Some b -> b
    | None ->
    let f r _ b =
      let (nt,l,_) = r in
      if b then true else
      if nt<>nt1 then false else
      if RS.mem r vr then false else
      match l with
        | [] -> true
        | litl -> begin
            let rec f2 litl =
              match litl with
                | (Non_ter (nt,_))::t -> nt::(f2 t)
                | _ -> []
            in
            let ntl = f2 litl in
            if (List.length ntl)<>(List.length ntl) then false
            else
            let rec f3 (ntl:non_ter list) =
              match ntl with
                | [] -> true
                | nt::t ->
                    let b =
                      let b = derive_epsilon nt (RS.add r vr) in
                      let () = nt_epsilon.(nt) <- Some b in b
                    in
                    if b then f3 t
                    else false
            in
            f3 ntl
        end
    in
    Urule_map.fold f user_g false
  in

  let rec aux nt1 nt2 vr =
    (*try (Map_ntc.find (nt1,nt2) po_array),po_array,nt_epsilon with Not_found ->*)
    match po_array.(nt1).(nt2) with Some b -> b
    | None ->
    let f r _ b =
      let (nt,l,_) = r in
      if b then true else
      if nt<>nt1 then false else
      if RS.mem r vr then false else
      match l with
        | [Non_ter (ntbis,_)] when ntbis=nt2 -> true
        | [Non_ter (ntbis,_)] ->
            let b = aux ntbis nt2 (RS.add r vr) in
            let () = (po_array.(ntbis).(nt2) <- Some b) in b
        | litl -> begin
            let rec f2 litl =
              match litl with
                | (Non_ter (nt,_))::t -> nt::(f2 t)
                | _ -> []
            in
            let ntl = f2 litl in
            if (List.length ntl)<>(List.length ntl) then false
            else
            let rec f3 n1 n2 ntl =
              match ntl with
                | [] -> true
                | nt::t ->
                    let b =
                      if n1=n2 then let b = aux nt nt2 (RS.add r vr) in
                        let () = po_array.(nt).(nt2) <- Some b in b
                      else let b = derive_epsilon nt RS.empty in
                        let () = nt_epsilon.(nt) <- Some b in b
                    in
                    if b then f3 (n1+1) n2 t
                    else false
            in
            let rec f4 n = match n with
              | 0 -> false
              | _ -> let b = f3 1 n ntl in
                  if b then true
                  else f4 (n-1)
            in
            f4 (List.length ntl)
        end
    in
    Urule_map.fold f user_g false
  in
  let b = aux nt1 nt2 RS.empty in
  let () = po_array.(nt1).(nt2) <- Some b in
  b


let print_r_L r_L =
  let f j = print_int j; print_string " " in
  for i=0 to (Array.length r_L)-1 do
    (print_int i; print_string " -L-> ";
    List.iter f r_L.(i); print_newline ())
  done


let print_rel_L rel_L width n =
  let str_bin bin =
    let rec aux c i =
      if i=30 then c else
      aux (c^(string_of_int ((bin lsr i) mod 2))) (i+1)
    in
    aux "" 0
  in
  for i=0 to width-1 do
    (for j=0 to n-1 do
      Printf.printf "%s\n" (str_bin rel_L.(i).(j))
    done;
    print_newline ())
  done





(*let compute_L_woPath gram_rhs gram_lhs lhslists prio_dat array_nt_prio =
  (* n is the number of lhs in the grammar (nt_nb) *)

  let n = Array.length gram_lhs in
  let width = (n+29)/30 in
  let ba = Array.make 30 0 in
  let all_one = ref 0 in
  for i=0 to 29 do
    (ba.(i) <- 1 lsl i;
    all_one := !all_one + ba.(i))
  done;

  (*Printf.printf "compute_L called\n";
  flush_all ();*)

  let dim = n*width in

  let rel_L = Array.make (2*dim) 0 in

  let f i rhs =
    match rhs.(0) with (* rhs should never be of length 0 here *)
      | Non_ter nt ->
          let l = comp_lhslist nt lhslists prio_dat array_nt_prio in
          let g j =
            let w1,w2 = i/30, i mod 30 in
            let k = ind_of_lhs j in
            rel_L.(dim+k+n*w1) <- rel_L.(dim+k+n*w1) lor ba.(w2)
          in
          List.iter g l
      | Ter _ -> ()
  in

  for i=0 to n-1 do
    (let w1,w2 = i/30, i mod 30 in
    rel_L.(dim+i+n*w1) <- rel_L.(dim+i+n*w1) lor ba.(w2);
    let rn_l = fst gram_lhs.(i) in
    let g rn =
      f i gram_rhs.(rn)
    in
    List.iter g rn_l)
  done;

  for k=0 to n-1 do (
    (*Printf.printf "k=%d\n" k; flush_all ();*)
    for i=0 to width-1 do
      for j=0 to n-1 do
        let l0,l1 = (k+1) mod 2, k mod 2 in
        let b =
          if (rel_L.(l0*dim+j+n*(k/30)) lsr (k mod 30)) mod 2 = 0
          then 0 else !all_one
        in
        rel_L.(l1*dim+j+n*i) <- rel_L.(l0*dim+j+n*i) lor
          (rel_L.(l0*dim+k+n*i) land b)
      done
    done)
  done;

  let l = (n-1) mod 2 in

  let r_L = Array.make (n+29) [] in

  for i=0 to width-1 do
    for j=0 to n-1 do
      for k=0 to 29 do
        if (rel_L.(l*dim+j+n*i) lsr k) mod 2 = 1
        then
          let m = i*30+k in
          r_L.(m) <- j::r_L.(m)
        else ()
      done
    done
  done;

  (rel_L,r_L)*)


let compute_L gram_rhs gram_lhs lhslists prio_dat array_nt_prio first_memo =
  (* n is the number of lhs in the grammar (nt_nb) *)

  let n = Array.length gram_lhs in
  let width = (n+29)/30 in
  let ba = Array.make 30 0 in
  let all_one = ref 0 in
  for i=0 to 29 do
    (ba.(i) <- 1 lsl i;
    all_one := !all_one + ba.(i))
  done;


  let dim = n*width in

  let rel_L = Array.make (2*dim) 0 in

  let f i rhs =
    match rhs.(0) with (* rhs should never be of length 0 here *)
      | Non_ter nt ->
          let l = comp_lhslist nt lhslists prio_dat array_nt_prio in
          let g j =
            let ind = dim+(ind_of_lhs j)+n*(i/30) in
            rel_L.(ind) <- rel_L.(ind) lor ba.(i mod 30)
          in
          List.iter g l
      | Ter _ -> ()
  in

  for i=0 to n-1 do
    (let ind = dim+i+n*(i/30) in
    rel_L.(ind) <- rel_L.(ind) lor ba.(i mod 30);
    let rn_l = fst gram_lhs.(i) in
    let g rn =
      f i gram_rhs.(rn)
    in
    List.iter g rn_l)
  done;

  for k=0 to n-1 do (
    (*Printf.printf "k=%d\n" k; flush_all ();*)
    let l0 = ((k+1) mod 2)*dim in
    let l1 = (k mod 2)*dim in
    let k0 = n*(k/30) in
    let k1 = k mod 30 in
    for i=0 to width-1 do
      let i0 = n*i in
      for j=0 to n-1 do
        let b =
          if (rel_L.(l0+j+k0) lsr k1) mod 2 = 0
          then 0 else !all_one
        in
        rel_L.(l1+j+i0) <- rel_L.(l0+j+i0) lor
          (rel_L.(l0+k+i0) land b)
      done
    done)
  done;

  let l = ((n-1) mod 2)*dim in

  let r_L = Array.make (n+29) [] in

  for i=0 to width-1 do
    let i0 = n*i in
    let i1 = i*30 in
    for j=0 to n-1 do
      for k=0 to 29 do
        if (rel_L.(l+j+i0) lsr k) mod 2 = 1
        then
          let m = i1+k in
          r_L.(m) <- j::r_L.(m)
        else ()
      done
    done
  done;

  (rel_L,r_L)


(*let compute_L gram_rhs gram_lhs lhslists prio_dat array_nt_prio first_memo =
  (* n is the number of lhs in the grammar (nt_nb) *)

  let n = Array.length gram_lhs in
  let width = (n+29)/30 in
  let ba = Array.make 30 0 in
  let all_one = ref 0 in
  for i=0 to 29 do
    (ba.(i) <- 1 lsl i;
    all_one := !all_one + ba.(i))
  done;


  let dim = n*width in

  let rel_L = Array.make (2*dim) 0 in

  let f i rhs =
    match rhs.(0) with (* rhs should never be of length 0 here *)
      | Non_ter nt ->
          let l = comp_lhslist nt lhslists prio_dat array_nt_prio in
          let g j =
            let w1,w2 = i/30, i mod 30 in
            let k = ind_of_lhs j in
            rel_L.(dim+k+n*w1) <- rel_L.(dim+k+n*w1) lor ba.(w2)
          in
          List.iter g l
      | Ter _ -> ()
  in

  for i=0 to n-1 do
    (let w1,w2 = i/30, i mod 30 in
    rel_L.(dim+i+n*w1) <- rel_L.(dim+i+n*w1) lor ba.(w2);
    let rn_l = fst gram_lhs.(i) in
    let g rn =
      f i gram_rhs.(rn)
    in
    List.iter g rn_l)
  done;

  for k=0 to n-1 do (
    (*Printf.printf "k=%d\n" k; flush_all ();*)
    for i=0 to width-1 do
      for j=0 to n-1 do
        let l0,l1 = (k+1) mod 2, k mod 2 in
        let b =
          if (rel_L.(l0*dim+j+n*(k/30)) lsr (k mod 30)) mod 2 = 0
          then 0 else !all_one
        in
        rel_L.(l1*dim+j+n*i) <- rel_L.(l0*dim+j+n*i) lor
          (rel_L.(l0*dim+k+n*i) land b)
      done
    done)
  done;

  let l = (n-1) mod 2 in

  let r_L = Array.make (n+29) [] in

  for i=0 to width-1 do
    for j=0 to n-1 do
      for k=0 to 29 do
        if (rel_L.(l*dim+j+n*i) lsr k) mod 2 = 1
        then
          let m = i*30+k in
          r_L.(m) <- j::r_L.(m)
        else ()
      done
    done
  done;

  (rel_L,r_L)*)


(*let compute_L gram_rhs gram_lhs lhslists prio_dat array_nt_prio =
  (* n is the number of lhs in the grammar (nt_nb) *)

  let n = Array.length gram_lhs in
  let width = (n+29)/30 in
  let ba = Array.make 30 0 in
  let all_one = ref 0 in
  for i=0 to 29 do
    (ba.(i) <- 1 lsl i;
    all_one := !all_one + ba.(i))
  done;

  (*Printf.printf "compute_L called\n";
  flush_all ();*)

  let rel_L =
    [|Array.init width (fun _ -> Array.make n 0);
      Array.init width (fun _ -> Array.make n 0)|]
  in

  let f i rhs =
    match rhs.(0) with (* rhs should never be of length 0 here *)
      | Non_ter nt ->
          let l = comp_lhslist nt lhslists prio_dat array_nt_prio in
          let g j =
            let w1,w2 = i/30, i mod 30 in
            let k = ind_of_lhs j in
            rel_L.(1).(w1).(k) <- rel_L.(1).(w1).(k) lor ba.(w2)
          in
          List.iter g l
      | Ter _ -> ()
  in

  for i=0 to n-1 do
    (let w1,w2 = i/30, i mod 30 in
    rel_L.(1).(w1).(i) <- rel_L.(1).(w1).(i) lor ba.(w2);
    let rn_l = fst gram_lhs.(i) in
    let g rn =
      f i gram_rhs.(rn)
    in
    List.iter g rn_l)
  done;

  for k=0 to n-1 do (
    (*Printf.printf "k=%d\n" k; flush_all ();*)
    for i=0 to width-1 do
      for j=0 to n-1 do
        let l0,l1 = (k+1) mod 2, k mod 2 in
        let b =
          if (rel_L.(l0).(k/30).(j) lsr (k mod 30)) mod 2 = 0
          then 0 else !all_one
        in
        rel_L.(l1).(i).(j) <- rel_L.(l0).(i).(j) lor
          (rel_L.(l0).(i).(k) land b)
      done
    done)
  done;

  let l = (n-1) mod 2 in

  let r_L = Array.make (n+29) [] in

  for i=0 to width-1 do
    for j=0 to n-1 do
      for k=0 to 29 do
        if (rel_L.(l).(i).(j) lsr k) mod 2 = 1
        then
          let m = i*30+k in
          r_L.(m) <- j::r_L.(m)
        else ()
      done
    done
  done;

  (rel_L,r_L)*)


let create_aut gram_rhs gram_lhs r_L init_is lit prio_dat it_nb array_nt_prio nt_of_ind lhs_of_ind ist_nt_nb lhslists lhs_table nt_to_add str_non_ter str_prio =
  let () = countst := 0 in
  let is_trace =
    (Array.make E.token_nb Map_is.empty),
    (Array.make ist_nt_nb Map_is.empty)
  in
  let state_list,n =
    build_automaton !automaton_kind init_is lit is_trace
      gram_rhs gram_lhs !dypgen_verbose prio_dat it_nb
      array_nt_prio nt_of_ind lhs_of_ind lhslists r_L lhs_table
      ist_nt_nb token_nb nt_to_add str_non_ter str_prio
  in
  state_list,n

(*let create_po user_g =
  let collect_nt (nt,_,_) _ nts = NTS.add nt nts in
  let nt_set = Urule_map.fold collect_nt user_g NTS.empty in
  let cover_nts nt1 (po_array,nt_epsilon) =
  (* nt_epsilon is used to record whether a non terminal can derive epsilon *)
    let aux nt2 (po_array,nt_epsilon) =
      if Map_ntc.mem (nt1,nt2) po_array then po_array,nt_epsilon
      else derive nt1 nt2 user_g po_array nt_epsilon
    in
    NTS.fold aux nt_set (po_array,nt_epsilon)
  in
  let po_array,_ = NTS.fold cover_nts nt_set (Map_ntc.empty,Nt_map.empty) in
  po_array*)

let create_po user_g nt_nb str_non_ter =
  let time1 = Sys.time () in
  let po_array =
    Array.init nt_nb (fun _ -> (Array.make nt_nb None))
  in
  let nt_epsilon = Array.make nt_nb None in
  let rec rhs_tokless litl = match litl with
    | (Ter _)::_ -> false
    | (Non_ter _)::tl -> rhs_tokless tl
    | [] -> true
  in
  let fold_fun r a (new_g,nts0,nts1) =
    let (nt,litl,_) = r in
    let nts0 = NTS.add nt nts0 in
    let new_g,nts1 = match litl with
      | [] -> let () = nt_epsilon.(nt) <- Some true in new_g,nts1
      | [Non_ter (nt2,_)] -> let () = po_array.(nt).(nt2) <- Some true in
          new_g,(NTS.add nt nts1)
      | _ ->
          if rhs_tokless litl then
            (Urule_map.add r a new_g),(NTS.add nt nts1)
          else (new_g,nts1)
    in
    (new_g,nts0,nts1)
  in
  let user_g, nt_set_0, nt_set_1 =
    Urule_map.fold fold_fun user_g (Urule_map.empty,NTS.empty,NTS.empty)
  in
  let nt_set_2 = NTS.diff nt_set_0 nt_set_1 in

  let cover_nts_2 nt1 =
    let aux nt2 = po_array.(nt1).(nt2) <- Some false in
    NTS.iter aux nt_set_0;
    nt_epsilon.(nt1) <- Some false
  in
  let () = NTS.iter cover_nts_2 nt_set_2 in

  let cover_nts_1 nt1 =
  (* nt_epsilon is used to record whether a non terminal can derive epsilon *)
    let aux nt2 =
      (*if Map_ntc.mem (nt1,nt2) po_array then po_array,nt_epsilon
      else let _,pm,nte = derive nt1 nt2 user_g po_array nt_epsilon in pm,nte*)
      let b = match po_array.(nt1).(nt2) with
        | None -> derive nt1 nt2 user_g po_array nt_epsilon
        | Some b -> b
        (*try (Map_ntc.find (nt1,nt2) po_array),po_array,nt_epsilon
        with Not_found -> derive nt1 nt2 user_g po_array nt_epsilon*)
      in
      if b && nt1=nt2 then
        let () = Printf.fprintf stderr
          "Error: non terminal `%s' can derive itself, cyclic grammars are not allowed\n"
          (str_non_ter.(nt1)) in
        failwith "cyclic grammar"
      else ()
    in
    NTS.iter aux nt_set_0
  in
  let () = NTS.iter cover_nts_1 nt_set_1 in

  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    (Printf.fprintf !log_channel "po_array built, %.3f sec\n" (time2-.time1);
    flush stdout) else ();
  po_array



let print_table_state chan i table table_it table_lit_trans gram_rhs lhs_table lhs_of_ind str_non_ter nt_of_ind str_prio =
  let tnb, ntnb = Array.length table.(0).(0), Array.length table.(0).(1) in
  let f tab len name_fun =
    for i=0 to len-1 do
      if tab.(i)>=0 then Printf.printf "(%s,%d) " (name_fun i) tab.(i)
    done
  in
  Printf.fprintf chan " State %d\n" i;
  Printf.fprintf chan "  li: %s\n  items:\n"
    (str_literal_trans table_lit_trans.(i) str_non_ter str_prio);
  print_item_set chan table_it.(i) gram_rhs lhs_table
    lhs_of_ind str_non_ter str_prio;
  Printf.fprintf chan "  next states (shift):\n   ";
  f table.(i).(0) tnb E.str_token_name; print_newline ();
  Printf.fprintf chan "  next states (reduction):\n   ";
  f table.(i).(1) ntnb
    (fun i -> str_non_terminal (nt_of_ind.(i)) str_non_ter str_prio);
  print_newline (); print_newline ()

let print_tables table table_it table_lit_trans gram_rhs lhs_table lhs_of_ind str_non_ter nt_of_ind str_prio =
  Printf.printf "\nTables\n\n";
  for i=0 to (Array.length table)-1 do
    print_table_state !log_channel i table table_it table_lit_trans gram_rhs
      lhs_table lhs_of_ind str_non_ter nt_of_ind str_prio
  done



let make_table state_list n ist_nt_nb gram_rhs lhs_table lhs_of_ind =
  let table = Array.init n
    (fun _ -> [|Array.make E.token_nb (-1);Array.make ist_nt_nb (-1)|])
  in
  (*Printf.printf "E.token_nb=%d, ist_nt_nb=%d\n" E.token_nb ist_nt_nb;*)
  let table_it = Array.make n dummy_item_set in
  let table_lit_trans = Array.make n (Ter 0) in

  let g num succ = match succ.li with
    | Ter t ->
        (*Printf.printf "Ter %s, t=%d\n" (str_token_name t) t;*)
        table.(num).(0).(t) <- succ.number
    | Non_ter nt ->
        (*Printf.printf "Non_ter %s\n" (str_non_terminal nt);*)
        table.(num).(1).(ind_of_nt nt) <- succ.number
  in
  let f v =
    (*Printf.printf "process state %d\n" v.number;*)
    List.iter (g v.number) v.succ_states;
    table_it.(v.number) <- v.items;
    table_lit_trans.(v.number) <- v.li
  in

  List.iter f state_list;

  (*print_tables table table_it table_lit_trans gram_rhs lhs_table lhs_of_ind;*)

  table,table_it,table_lit_trans


let create_parsing_device_with_init gram_rhs gram_lhs lhs_table
    actions r_L user_g (data:'b) datadyn (local_data:'c)
    (pcs:priority_data) init_is
    lit user_nt_nb po_array it_nb array_nt_prio nt_of_ind
    non_ter_of_ind prio_of_ind lhs_of_ind g_nb ist_nt_nb lhslists
    nt_to_add str_non_ter cons_of_nt str_prio =
  let state_list,n = create_aut gram_rhs gram_lhs r_L init_is
    lit pcs it_nb array_nt_prio nt_of_ind lhs_of_ind ist_nt_nb
    lhslists lhs_table nt_to_add str_non_ter str_prio in
  let table,table_it,table_lit_trans =
    make_table state_list (n+1) ist_nt_nb gram_rhs
      lhs_table lhs_of_ind
  in
  let parsing_device =
    { actions = actions ; gram_rhs = gram_rhs ;
      gram_lhs = gram_lhs ; lhs_table = lhs_table ;
      g_nb = g_nb; user_g = user_g ; table = table ;
      table_it = table_it ; table_lit_trans = table_lit_trans ;
      lhslists = lhslists ; po = po_array ; data = data;
      local_data = local_data ; datadyn = datadyn ;
      prio = pcs ;
      aut_kind = !automaton_kind ;
      nt_nb = Array.length gram_lhs; non_ter_of_ind=non_ter_of_ind;
      prio_of_ind=prio_of_ind; nt_of_ind=nt_of_ind;
      lhs_of_ind=lhs_of_ind; str_non_ter=str_non_ter;
      cons_of_nt=cons_of_nt;
      str_prio = str_prio }
   in
  parsing_device


(*let dummy_token_name = 0*)
let dummy_token_map =
  let aux dt_map (ep,dt) = Int_map.add ep dt dt_map in
  List.fold_left aux Int_map.empty E.entry_points


let init_is gram_rhs gram_lhs r_L array_nt_prio lhslists prio_dat array_nt_prio =
  (*let non_ter_of_lit nt_lit = non_ter_of_nt (nt_of_lit nt_lit) in*)
  let is = new_item_set () in
  let aux nt_to_add rn =
  (*print_endline ("> "^(string_of_int (Array.length gram_rhs.(rn))));
  print_endline ("> "^(str_handle gram_rhs.(rn) 0));*)
  match gram_rhs.(rn) with
    | [|Non_ter nt|] ->
        (is.non_kernel <- rn::is.non_kernel;
        let lhs_l =
          comp_lhslist nt lhslists prio_dat array_nt_prio
        in
        let g1 nt_to_add ind =
          Int_set.add ind nt_to_add
        in
        let g2 nt_to_add lhs =
          let ind_list = r_L.(ind_of_lhs lhs) in
          List.fold_left g1 nt_to_add ind_list
        in
        (List.fold_left g2 nt_to_add lhs_l))
    | _ -> assert false
  in
  is,(List.fold_left aux Int_set.empty (fst gram_lhs.(0)))



let init_lit = Ter 0

(*let create_parsing_device_bis
    (gram_lhs:((int list) * (int option)) array)
    gram_rhs lhs_table
    actions aut_kind (data:'b) (local_data:'c)
    (pcs:priority_data)
    user_nt_nb po_array user_g datadyn array_nt_prio nt_of_ind
    non_ter_of_ind prio_of_ind lhs_of_ind g_nb ist_nt_nb str_non_ter
    cons_of_nt =
    (* gram must already contain the rules S' -> entry_point *)
  automaton_kind := aut_kind;
  let lhslists = lhslists_init ist_nt_nb in

  let first_memo = Array.make ist_nt_nb None in

  (*let time1 = Sys.time () in
  compute_first first_memo gram nt_nb prio_dat array_nt_prio lhs_of_ind;
  let time2 = Sys.time () in
  Printf.printf "first computed, %.3f sec\n" (time2-.time1);*)

  let time1 = Sys.time () in
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    pcs array_nt_prio first_memo in
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
      (time2-.time1);
  let is0, nt_to_add = init_is gram_rhs gram_lhs r_L array_nt_prio
    lhslists pcs array_nt_prio
  in
  create_parsing_device_with_init gram_rhs gram_lhs
    lhs_table actions r_L user_g data
    datadyn local_data pcs
    is0 init_lit user_nt_nb
    po_array 0 array_nt_prio nt_of_ind non_ter_of_ind prio_of_ind
    lhs_of_ind g_nb ist_nt_nb lhslists nt_to_add str_non_ter cons_of_nt*)


(*let update_parsing_device_data automaton dat ldat =
  { automaton with data=dat ; local_data = ldat }*)

(*let create_saved_parsing_device gram nt_nb prio_dat =
  let v , po_array = create_a_and_po (gram:('a,'b,'c) grammar)
    (init_is gram) init_lit nt_nb prio_dat in
  { sa_init = v ; sa_po = po_array }

let complete_parsing_device sa gram aut_kind data local_data priodata
    merge_map global_merge nt_nb =
  { g = gram ; init = sa.sa_init ; po = sa.sa_po ;
  data = data ; local_data = local_data ; prio = priodata ; merge_map = merge_map;
  global_merge = global_merge ; aut_kind = aut_kind ; nt_nb = nt_nb }*)





let update_user_g ral user_g =
  let f user_g (r,a) =
    let al = try Urule_map.find r user_g with Not_found -> [] in
    Urule_map.add r (a::al) user_g
  in
  List.fold_left f user_g ral


let str_rule rn gram_rhs lhs_table str_non_ter str_prio =
    Printf.sprintf "%s -> %s"
      (str_lhs lhs_table.(rn) str_non_ter str_prio)
      (str_handle gram_rhs.(rn) (-1) str_non_ter str_prio)

let print_grammar chan gram_rhs lhs_table str_non_ter str_prio =
  for i=0 to (Array.length lhs_table)-1 do
    Printf.fprintf chan "rn:%d  %s\n" i
      (str_rule i gram_rhs lhs_table str_non_ter str_prio)
  done;
  print_newline ()


let print_gram_lhs gram_lhs lhs_of_ind str_non_ter str_prio =
  let f i rn =
    Printf.printf "lhs:%s  rn:%d\n" (str_lhs lhs_of_ind.(i) str_non_ter str_prio) rn
  in
  for i=0 to (Array.length gram_lhs)-2 do (
    (match snd gram_lhs.(i) with
    | None ->
        Printf.printf "no epsilon rule for %s\n"
          (str_lhs (lhs_of_ind.(i)) str_non_ter str_prio)
    | Some rn ->
        Printf.printf "no epsilon rule nb %d for %s\n" rn
          (str_lhs (lhs_of_ind.(i)) str_non_ter str_prio)
    );
    List.iter (f i) (fst gram_lhs.(i)) )
  done


let print_g g lhs_of_ind str_non_ter str_prio =
  let f i rhs _ =
    Printf.printf "%s -> %s\n" (str_lhs lhs_of_ind.(i) str_non_ter str_prio)
      (str_handle rhs 0 str_non_ter str_prio)
  in
  for i=0 to (Array.length g)-1 do
    F.Map_rhs.iter (f i) g.(i)
  done

let print_lhs_of_ind lhs_of_ind str_non_ter str_prio =
  for i=0 to (Array.length lhs_of_ind)-1 do
    Printf.printf "ind %d = lhs %s\n" i (str_lhs lhs_of_ind.(i) str_non_ter str_prio)
  done


let make_grams_actions g nbr array_nt_prio kernel_nt kernel_t reducible g_lhs g_rhs lhs_t actions lhs_of_ind =
  let gl_len = (Array.length g)+1 in
  let gram_lhs = Array.make gl_len ([],None) in
  (* The last cell of the array will be kept ([],None) and reserved
  for the non terminals appearing in the rhs of the "seed" items,
  i.e. kernel items of the initial state, when the automaton is
  generated for a modification of the grammar. *)

  let g1 rn_set (rn,_) = Int_set.add rn rn_set in

  let rn_set = List.fold_left g1 Int_set.empty kernel_nt in
  let rn_set = List.fold_left g1 rn_set kernel_t in

  let reducible = List.filter
    (fun (rn,_) -> Array.length g_rhs.(rn)>0) reducible
  in

  let rn_set = List.fold_left g1 rn_set reducible in

  let g2 rn (rn_map1,rn_map2,i) =
    (Int_map.add rn i rn_map1),
    (Int_map.add i rn rn_map2), i+1
    in

  let rn_map1,rn_map2,ir_nb =
    Int_set.fold g2 rn_set (Int_map.empty,Int_map.empty,0)
  in

  let gram_rhs = Array.make (ir_nb+nbr) [||] in
  let lhs_table = Array.make (ir_nb+nbr) dummy_lhs in
  let new_actions = Array.make (ir_nb+nbr)
    [Dypgen_action (fun ol _ _ d dd ld _ pd _ ->
      (List.hd ol,false,false,d,dd,ld,[],[],pd,None,None))]
  in

  let correct_rhs rhs =
    let len = Array.length rhs in
    let new_rhs = Array.make len (Ter 0) in
    for i=0 to len-1 do match rhs.(i) with
      | Ter _ -> new_rhs.(i) <- rhs.(i)
      | Non_ter nt -> new_rhs.(i) <-
          Non_ter (new_rhs_lit nt array_nt_prio (gl_len-1))
    done;
    new_rhs
  in

  for i=0 to ir_nb-1 do
    (let rn = Int_map.find i rn_map2 in
    new_actions.(i) <- actions.(rn);
    gram_rhs.(i) <- correct_rhs g_rhs.(rn);
    lhs_table.(i) <- lhs_t.(rn))
  done;

  let g3 (rn,x) = ((Int_map.find rn rn_map1),x) in

  let kernel_nt = List.map g3 kernel_nt in
  let kernel_t = List.map g3 kernel_t in
  let reducible = List.map g3 reducible in

  let rn = ref ir_nb in

  let f i rhs ac =
    gram_rhs.(!rn) <- rhs;
    new_actions.(!rn) <- ac;
    lhs_table.(!rn) <- lhs_of_ind.(i);
    let (rnl,iop) = gram_lhs.(i) in
    (if Array.length rhs > 0 then gram_lhs.(i) <- (!rn::rnl,iop)
    else gram_lhs.(i) <- (rnl,Some !rn));
    incr rn
  in
  for i=0 to (Array.length g)-1 do
    F.Map_rhs.iter (f i) g.(i)
  done;

  (*print_grammar gram_rhs lhs_table;
  print_g g lhs_of_ind;
  print_lhs_of_ind lhs_of_ind;
  print_gram_lhs gram_lhs lhs_of_ind;*)

  gram_lhs, gram_rhs, lhs_table, new_actions, kernel_nt,
  kernel_t, reducible, rn_map1


let make_grammar ral prio_dat str_non_ter =
  let user_g = update_user_g ral Urule_map.empty in
  let
   g,array_nt_prio,user_nt_nb,ist_nt_nb,
   nt_of_ind,non_ter_of_ind,prio_of_ind,lhs_of_ind,nbr
     = make_real_grammar user_g prio_dat str_non_ter
  in
  let gram_lhs,gram_rhs,lhs_table,actions,_,_,_,_ =
    make_grams_actions g nbr array_nt_prio [] [] []
    [||] [||] [||] [||] lhs_of_ind
  in
  let po_array = create_po user_g user_nt_nb str_non_ter in
  gram_lhs,gram_rhs,lhs_table,actions,user_nt_nb,ist_nt_nb,po_array,
  user_g,array_nt_prio,nt_of_ind,non_ter_of_ind,prio_of_ind,
  lhs_of_ind



let create_parsing_device rapf_list priority_data aut_kind global_data local_data datadyn str_non_ter cons_of_nt str_prio =
  let gram_lhs,gram_rhs,lhs_table,actions,user_nt_nb,ist_nt_nb,
    po_array,user_g, array_nt_prio,nt_of_ind,non_ter_of_ind,
    prio_of_ind,lhs_of_ind =
    make_grammar rapf_list priority_data str_non_ter
  in
  automaton_kind := aut_kind;
  let lhslists = lhslists_init ist_nt_nb in

  let first_memo = Array.make ist_nt_nb None in

  (*let time1 = Sys.time () in
  compute_first first_memo gram nt_nb prio_dat array_nt_prio lhs_of_ind;
  let time2 = Sys.time () in
  Printf.printf "first computed, %.3f sec\n" (time2-.time1);*)

  let time1 = Sys.time () in
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    priority_data array_nt_prio first_memo in
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
      (time2-.time1);
  let is0, nt_to_add = init_is gram_rhs gram_lhs r_L array_nt_prio
    lhslists priority_data array_nt_prio
  in
  let state_list,n = create_aut gram_rhs gram_lhs r_L is0
    init_lit priority_data 0 array_nt_prio nt_of_ind lhs_of_ind ist_nt_nb
    lhslists lhs_table nt_to_add str_non_ter str_prio in
  let table,table_it,table_lit_trans =
    make_table state_list (n+1) ist_nt_nb gram_rhs
      lhs_table lhs_of_ind
  in
  let deb_parsing_device =
    { deb_gram_rhs = gram_rhs ;
      deb_gram_lhs = gram_lhs ;
      deb_lhs_table = lhs_table ;
      deb_g_nb = 0;
      deb_table = table ;
      deb_table_it = table_it ;
      deb_table_lit_trans = table_lit_trans ;
      deb_lhslists = lhslists ;
      deb_po = po_array ;
      deb_data = global_data;
      deb_local_data = local_data ;
      deb_datadyn = datadyn ;
      deb_prio = priority_data ;
      deb_aut_kind = !automaton_kind ;
      deb_nt_nb = Array.length gram_lhs;
      deb_non_ter_of_ind=non_ter_of_ind;
      deb_prio_of_ind=prio_of_ind;
      deb_nt_of_ind=nt_of_ind;
      deb_lhs_of_ind=lhs_of_ind;
      deb_str_non_ter=str_non_ter;
      deb_cons_of_nt=cons_of_nt;
      deb_str_prio = str_prio }
  in
  deb_parsing_device
  (*create_parsing_device_bis gram_lhs gram_rhs lhs_table actions aut_kind
    global_data local_data priority_data
    user_nt_nb po_array user_g datadyn array_nt_prio nt_of_ind
    non_ter_of_ind prio_of_ind lhs_of_ind 0 ist_nt_nb str_non_ter
    cons_of_nt*)
(*{
  deb_gram_lhs = [||];
  deb_gram_rhs = [||];
  deb_lhs_table = [||];
(*   deb_actions = [||]; *)
  deb_g_nb = 0;
(*   deb_user_g = Urule_map.empty; *)
  deb_table = [||];
  deb_table_it = [||];
  deb_table_lit_trans = [||];
  deb_lhslists = lhslists_init 0;
  deb_po = [||];
  deb_data = global_data;
  deb_prio = priority_data;
  deb_local_data = local_data;
  deb_datadyn = datadyn;
  deb_aut_kind = !automaton_kind;
  deb_nt_nb = 0;
  deb_non_ter_of_ind = [||];
  deb_prio_of_ind = [||];
  deb_nt_of_ind = [||];
  deb_lhs_of_ind = [||];
  deb_str_non_ter = [||];
  deb_cons_of_nt = [||];
}*)
(*{
  gram_lhs = [||];
  gram_rhs = [||];
  lhs_table = [||];
  actions = [||];
  g_nb = 0; (* grammar number *)
  user_g = Urule_map.empty;
  table = [||];
  (* 1st index for state
  2nd index: 0 for terminals, 1 for non terminals
  3rd index to choose which terminal or nt. *)
  table_it = [||];
  table_lit_trans = [||];
  lhslists = lhslists_init 0;
  po = [||];
    (** Patial order between non terminal name couples. A couple of non
     terminals is bound to a bool. This may be implemented alternatively
     with a set : the presence of a couple (nt1,nt2) in the set would
     denote that nt1<=nt2 is true. This would save memory space. *)
  data = global_data;
  prio = priority_data;
  local_data = local_data;
  datadyn = datadyn;
  (*global_merge : 'obj merge_function;*)
  aut_kind = !automaton_kind;
  nt_nb = 0;
    (* number of non terminals in the internal grammar
     (as opposed to the user grammar. *)
  non_ter_of_ind = [||];
  prio_of_ind = [||];
  nt_of_ind = [||];
  lhs_of_ind = [||];
  str_non_ter = [||];
  cons_of_nt = [||];
}*)

(** UPDATE AUTOMATON used in reduceViaPath to change the grammar and the automaton
    when new rule_bis are added and old ones are removed. *)

let remove_from_user_g rl user_g =
  let f user_g r = Urule_map.remove r user_g in
  List.fold_left f user_g rl

let build_nt_to_add gram_rhs r_L item_list lhslists prio_dat array_nt_prio =
  let nta nt_to_add (rn,dp) = match gram_rhs.(rn).(dp) with
    | Non_ter nt ->
        let lhs_l =
          comp_lhslist nt lhslists prio_dat array_nt_prio
        in
        let g1 nt_to_add ind =
          Int_set.add ind nt_to_add
        in
        let g2 nt_to_add lhs =
          let ind_list = r_L.(ind_of_lhs lhs) in
          List.fold_left g1 nt_to_add ind_list
        in
        List.fold_left g2 nt_to_add lhs_l
    | _ -> assert false
  in
  List.fold_left nta Int_set.empty item_list


let str_user_rhs rhs =
  let rec aux s rhs = match rhs with
    | [] -> s
    | (Non_ter (nt,_))::tl -> aux (s^" "^(string_of_int nt)) tl
    | (Ter t)::tl -> aux (s^" "^(E.str_token_name t)) tl
  in
  aux "" rhs

let str_user_rule (lhs,rhs,_) =
  (string_of_int lhs)^" -> "^(str_user_rhs rhs)

let print_ral ral =
  let f (r,a) = print_endline (str_user_rule r) in
  List.iter f ral




let update_parsing_device parsing_device ral_add r_remove newdata newdatadyn
    newlocal_data newprio lit_trans g_nb kernel_nt kernel_t reducible =

  (*print_ral ral_add;*)

  let user_g = remove_from_user_g r_remove parsing_device.user_g in
  let user_g = update_user_g ral_add user_g in

  let str_non_ter = Array.make newdatadyn.nt_num "" in
  let f str_nt (i,_,_) = str_non_ter.(i) <- str_nt in
  String_map.iter f newdatadyn.nt_map;

  let
    g,array_nt_prio,user_nt_nb,ist_nt_nb,
    nt_of_ind,non_ter_of_ind,prio_of_ind,lhs_of_ind,nbr =
    make_real_grammar user_g newprio str_non_ter
  in

  (*print_endline "previous grammar:";
  print_grammar stdout parsing_device.gram_rhs
    parsing_device.lhs_table parsing_device.str_non_ter;
  print_endline "* end of previous grammar *";
  print_endline "previous lhs_of_ind:";
  for i=0 to (Array.length parsing_device.lhs_of_ind)-1 do
    Printf.printf "  %d : %s\n" i
      (str_lhs parsing_device.lhs_of_ind.(i)
       parsing_device.str_non_ter)
  done;
  print_endline "* end of previous lhs_of_ind *";*)

  let gram_lhs,gram_rhs,lhs_table,actions,kernel_nt,kernel_t,
    reducible,rn_map =
    make_grams_actions g nbr array_nt_prio kernel_nt kernel_t reducible
      parsing_device.gram_lhs parsing_device.gram_rhs
      parsing_device.lhs_table parsing_device.actions lhs_of_ind
  in

  (*print_endline "new grammar:";
  print_grammar stdout gram_rhs lhs_table str_non_ter;
  print_endline "* end of new, grammar *";
  print_endline "new lhs_of_ind:";
  for i=0 to (Array.length lhs_of_ind)-1 do
    Printf.printf "  %d : %s\n" i
      (str_lhs lhs_of_ind.(i) str_non_ter)
  done;
  print_endline "* end of new lhs_of_ind *";*)

  let po_array = create_po user_g user_nt_nb str_non_ter in

  let first_memo = Array.make ist_nt_nb None in

  (*let time1 = Sys.time () in
  compute_first first_memo gram nt_nb prio_dat array_nt_prio lhs_of_ind;
  let time2 = Sys.time () in
  Printf.printf "first computed, %.3f sec\n" (time2-.time1);*)

  let lhslists = lhslists_init ist_nt_nb in
  let time1 = Sys.time () in
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    newprio array_nt_prio first_memo
  in
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
      (time2-.time1);
  let nt_to_add =
    build_nt_to_add gram_rhs r_L kernel_nt (*(kernel_nt@kernel_t)*)
      lhslists newprio array_nt_prio
  in
  let init_is_updated_parsing_device,it_nb =
    { reducible = reducible;
      kernel_nt = kernel_nt;
      kernel_t = kernel_t;
      non_kernel = []
    },
    (List.length kernel_nt)+(List.length kernel_t)+
    (List.length reducible)
    (*let f ind irhs tns ((is:IS.t),it_nb) =
      let (_,dp) = irhs in
      if dp=0 then is,it_nb
      else
        (IS.insert ind irhs tns is),(it_nb+1)
    in
    IS.fold f s_rightSib.items (IS.make nt_nb,0)*)
  in
  (* All the kernel items are selected in the current state after
   the reduction happened. An items set is made with them and
   used as the starting state of a new automaton. *)

  let cons_of_nt = Array.make newdatadyn.nt_num 0 in
  let aux _ (i,cons_i,_) =
    cons_of_nt.(i) <- cons_i
  in
  String_map.iter aux newdatadyn.nt_map;

  (*print_endline "cons_of_nt array:";
  for i=0 to (Array.length cons_of_nt)-1 do
    print_endline (string_of_int cons_of_nt.(i))
  done;
  print_endline "end of cons_of_nt array";*)
          (*print_endline "lhs_of_ind:";
          for i=0 to (Array.length lhs_of_ind)-1 do
            Printf.printf "  %d : %s\n" i
              (str_lhs lhs_of_ind.(i)
               str_non_ter)
          done;
          print_endline "* end of lhs_of_ind *";*)

  let str_prio = Array.make newprio.prd_nb "" in
  let str_prio_update name prio = str_prio.(prio) <- name in
  String_map.iter str_prio_update newprio.prd_strmap;

  create_parsing_device_with_init gram_rhs gram_lhs lhs_table actions r_L
    user_g newdata newdatadyn newlocal_data newprio (*parsing_device.prio*)
    init_is_updated_parsing_device lit_trans user_nt_nb
    po_array it_nb array_nt_prio nt_of_ind non_ter_of_ind
    prio_of_ind lhs_of_ind g_nb ist_nt_nb lhslists nt_to_add str_non_ter
    cons_of_nt str_prio,
  rn_map

(* ******* CE QUI PRECEDE EST A DEPLACER DANS UN AUTRE FICHIER ****** *)




type ('obj,'data,'local_data) vertex = {
  state_nb : int;
  pdev : ('obj,'data,'local_data) parsing_device;
  sn_nb : int;
  last_token : int;
  lexer_pos : (Lexing.position * Lexing.position);
  prio_imprint : F.prio_imprint;
  mutable succ_edges : (('obj,'data,'local_data) edge) list;
  mutable det_depth : int; (* deterministic depth, see Elkhound TR sect. 3.1 *)
  mutable ref_count : int;  (* reference count, see Elkhound TR sect. 3.1 *)
}
and ('obj,'data,'local_data) edge = {
  mutable edge_label : 'obj list * int;
  mutable dest : ('obj,'data,'local_data) vertex;
}

let update_depth topmost f =
  let rec aux g sn =
    if f sn then match sn.succ_edges with
      | [e] when sn.ref_count=1 ->
          let h x = sn.det_depth <- x+1; g (x+1) in
          aux h e.dest
      | _ -> sn.det_depth <- 0; g 0
  in
  List.iter (aux (fun _ -> ())) topmost

let create_e v1 label v2 f topmost =
  let new_edge = { edge_label = label; (*source = v1;*) dest = v2 } in
  v1.succ_edges <- new_edge::(v1.succ_edges);
  v2.ref_count <- v2.ref_count+1;
  if v2.ref_count > 2 then
    update_depth topmost f;
  new_edge



let create_v state_nb pdev sn_nb last_token lexer_pos prio_imprint depth = {
  state_nb = state_nb;
  pdev = pdev;
  sn_nb = sn_nb;
  last_token = last_token;
  lexer_pos = lexer_pos;
  prio_imprint = prio_imprint;
  succ_edges = [];
  det_depth = depth;
  ref_count = 0; }



(*type ('obj,'data,'local_data) vertex_lab = {
  state_nb : int;
  pdev : ('obj,'data,'local_data) parsing_device;
  sn_nb : int;
  last_token : int;
  lexer_pos : (Lexing.position * Lexing.position);
  prio_imprint : F.prio_imprint;
}*)
(* sn_nb is the number of the stack node, which is useful to distinguish
the stack nodes.
first_token is the number of the first token of the part of the input corresponding to this stack node.
last_token is the number of the last token of the part of the input corresponding to this stack node.
These two fields are used to determine the order of the reductions.

prio_imprint : stack node should not be merged if there is an item i and
a non terminal nt in the rhs of i before the 'dot' such that nt was yielded
with two different priorities. This ensures that when two stack nodes are
merged, the "history of priorities" is the same. Thus, for any stack nodes
each items is either 'valid' with respect to the priorities of all paths
from this node or it is not valid for all paths. This validity is stored
in the bool. The priority list is the history of priorities.
Before being merged, two stack nodes must have the same prio_imprint,
see the function find_rightSib
*)

(* type 'obj edge_lab = 'obj list * int *)

type ('a,'b,'c) path = ('a,'b,'c) vertex * (('a,'b,'c) edge list) * int
(** vertex is for the start of the path, int is the token number, i.e.
the first token in the part of the input which would be reduced if a reduction
along this path of the graph-structured stack happens. *)

(** [find_paths] returns a list of couples ([path],[token_nb]), where a path is
    a list of edges of the graph structured stack [gs] . The returned paths
    have the following properties : they begin at stack node [sn], their length
    is [len], the nodes along each path contain states which associated
    literals are in [litl], these literals take place along the path in the
    same order as in the list [litl] (actually in reverse). These paths are the
    ones along which a reduction can be performed by a production rule which
    rhs is [litl].
    [token_nb] is the token number of the leftmost stack node of a path, i.e.
    the dest stack node of the edge which is at the end of the list of edges
    which is [path]. It is the number of the token which is the first in the
    part of the input which would be reduced by the rule given as argument.
    [find_paths] is used in [doReductions] and in [insertLimitedReductions]
    and in [insertLimitedReductions2]. *)
let find_paths (sn:('a,'b,'c) vertex) ind (rhs:rhs) rn =
  let b = prio_imprint_check rn
    sn.pdev.gram_rhs
    sn.prio_imprint in
  if b = false then [] else
  let rec aux n succ path =
    if n = 0 then match path with
      | e::_ -> let s = e.dest in [path,s.last_token]
      | [] -> [[],-1]
    else
    match succ with
      | [] -> []
      | e::t ->
          let sn2 = e.dest in
          let succ2 = sn2.succ_edges in
          (aux (n-1) succ2 (e::path))@(aux n t path)
  in
  aux (Array.length rhs) sn.succ_edges []

let stack_node_equal sn1 sn2 = sn1.sn_nb = sn2.sn_nb

let edge_equal e1 e2 = (snd e1.edge_label) = (snd e2.edge_label)
let edge_list_equal el1 el2 = List.for_all2 edge_equal el1 el2

(* revcat a b = rev a @ b *)
let rec revcat a b = match a with
  | [] -> b
  | h :: t -> revcat t (h::b)

(** Maintains a partially ordered list of couples (path,rule_bis), where a path is
    a list of edges of the graph structured stack. This function inserts the couple
    of the path [p] and the rule_bis [r] in the list [l] at its right place. The
    partial order is the field [po] of the state which is held by the source
    stack node of the first edge of the path [p].
    The partial order is implemented as a 2 dim array of type bool option.
    The need for this partial order is explained in Scott McPeak's report.
    A better data structure than a list should be used. *)
let insert_partially_ordered l (((start_node,p,token_nb):('a,'b,'c) path),(ind,rhs), (rn,tns)) =
  let parsing_device = start_node.pdev in
  let non_ter_of_ind = parsing_device.non_ter_of_ind in
  if non_ter_of_ind.(ind) = non_terminal_startprime then l (* Do not reduce with S'->S *)
  else
  let rec aux result l = match l with
    | [] -> revcat result [((start_node,p,token_nb),(ind,rhs),(tns,rn))]
    | ((start1,p1,tnb1),(ind1,rhs1),(tns1,rn1))::tl ->
        if tnb1<token_nb then (* yes, this is the good order *)
          revcat result (((start_node,p,token_nb),(ind,rhs),(tns,rn))::l)
        else
        if tnb1>token_nb
        then aux (((start1,p1,tnb1),(ind1,rhs1),(tns1,rn1))::result) tl
        else
        let parsing_device1 = start1.pdev in
        if parsing_device.g_nb <> parsing_device1.g_nb
        then aux (((start1,p1,tnb1),(ind1,rhs1),(tns1,rn1))::result) tl
        else
        (*let (unt1,_,_),(unt,_,_) = nt1,nt in*)
        (*let rel = Map_ntc.find ((non_ter_of_nt nt1),(non_ter_of_nt nt))
          parsing_device.po in*)
        let rel =
          match parsing_device.po.(non_ter_of_ind.(ind1)).(non_ter_of_ind.(ind)) with
          | Some b -> b
          | None -> assert false
        in
        if rel then revcat result
         (((start_node,p,token_nb),(ind,rhs),(tns,rn))::l)
        else if ind=ind1 && rhs=rhs1 &&
          (stack_node_equal start_node start1) && (edge_list_equal p p1)
        then revcat result l
        else aux (((start1,p1,tnb1),(ind1,rhs1),(tns1,rn1))::result) tl
  in
  aux [] l

(* The following functions are used when pathList is a queue, using
the module Path_queue. (but it seems to be slower) *)
(*let compare_path path path1 =
  let ((start_node,p,token_nb),(ind,rhs)) = path in
  let ((start1,p1,tnb1),(ind1,rhs1)) = path1 in
  if token_nb<tnb1 then -1
  else if token_nb>tnb1 then 1 else
  let parsing_device = start_node.pdev in
  let parsing_device1 = start1.pdev in
  if parsing_device.g_nb<parsing_device1.g_nb then -1
  else if parsing_device.g_nb>parsing_device1.g_nb then 1
  else
  let ntoi = parsing_device.non_ter_of_ind in
  let rel =
    match parsing_device.po.(ntoi.(ind1)).(ntoi.(ind)) with
    | Some b -> b
    | None -> assert false
  in
  if rel then -1 else
  let rel =
    match parsing_device.po.(ntoi.(ind)).(ntoi.(ind1)) with
    | Some b -> b
    | None -> assert false
  in
  if rel then 1 else
  let c = Pervasives.compare (ind,rhs) (ind1,rhs1) in
  if c<>0 then c else
  let c = Pervasives.compare start_node.sn_nb
    start1.sn_nb in
  if c<>0 then c else
  let rec aux l1 l2 = match l1,l2 with
    | [],[] -> 0
    | _,[] -> 1
    | [],_ -> -1
    | (e1::t1),(e2::t2) ->
        let c = Pervasives.compare (snd e1.edge_label)
          (snd e2.edge_label) in
        if c<>0 then c else aux t1 t2
  in aux p p1

let insert_path path path_queue =
  let (start_node,_,_),(ind,_) = path in
  let pdev = start_node.pdev in
  let ntoi = pdev.non_ter_of_ind in
  if ntoi.(ind) = non_terminal_startprime then path_queue else
  (* Do not reduce with S'->S *)
  Path_queue.insert compare_path path path_queue

let pop_path path_queue =
  Path_queue.pop compare_path path_queue*)

let print_path sn p =
  let snnb = sn.sn_nb in
  let () = Printf.fprintf !log_channel "sn:%d; " snnb in flush_all ();
  let f e =
    let _,ednb = e.edge_label in
    Printf.fprintf !log_channel "%d " ednb
  in
  let () = List.iter f p in
  Printf.fprintf !log_channel "\n"

let insertLimitedReductions pathList link t topmost dummy_token_name =
  let _,edge_nb = link.edge_label in
  let aux1 pathList sn =
    let v,pdev = sn.state_nb,sn.pdev in
    let aux2 pathList (rn,tns) =
      (*if && (!automaton_kind=LR0 || (TNS.mem t tns) ||
         (TNS.mem dummy_token_name tns)) || (rule_kind=Dynamic_rule)) then*)
        let rhs,ind = pdev.gram_rhs.(rn), ind_of_lhs pdev.lhs_table.(rn) in
        let paths = find_paths sn ind rhs rn in
        let aux3 pathList (p,tnb) =
          if List.exists (function e -> (snd e.edge_label)=edge_nb) p then
            let () = if !dypgen_verbose>2 then print_path sn p else () in
            insert_partially_ordered pathList ((sn,p,tnb),(ind,rhs),(tns,rn))
            (*insert_path ((sn,p,tnb),(ind,rhs)) pathList*)
          else pathList
        in
        List.fold_left aux3 pathList paths
      (*else pathList*)
    in
    List.fold_left aux2 pathList pdev.table_it.(v).reducible
  in
  List.fold_left aux1 pathList topmost

  (*let _,edge_nb = link.edge_label in
  let aux1 pathList sn =
    let v,aut = sn.state_nb,sn.pdev in
    let aux2 ind (rhs,dp) tns pathList =
      if dp = Array.length rhs (*&& (!automaton_kind=LR0 || (TNS.mem t tns) ||
         (TNS.mem dummy_token_name tns)) || (rule_kind=Dynamic_rule))*) then
        let paths = find_paths sn ind rhs in
        let aux3 pathList (p,tnb) =
          if List.exists (function e -> (snd e.edge_label)=edge_nb) p then
            let () = if !dypgen_verbose>2 then print_path sn p else () in
            insert_partially_ordered pathList ((sn,p,tnb),(ind,rhs))
            (*insert_path ((sn,p,tnb),(ind,rhs)) pathList*)
          else pathList
        in
        List.fold_left aux3 pathList paths
      else pathList
    in
    IS.fold aux2 v.items pathList
  in
  List.fold_left aux1 pathList topmost*)

let insertLimitedReductions2 pathList t sn dummy_token_name =
  let v,pdev = sn.state_nb,sn.pdev in
  let aux2 pathList (rn,tns) =
    (*if && (!automaton_kind=LR0 || (TNS.mem t tns) ||
         (TNS.mem dummy_token_name tns)) || (rule_kind=Dynamic_rule)) then*)
      let rhs,ind = pdev.gram_rhs.(rn), ind_of_lhs pdev.lhs_table.(rn) in
      let paths = find_paths sn ind rhs rn in
      let aux3 pathList (p,tnb) =
        let () = if !dypgen_verbose>2 then print_path sn p else () in
        insert_partially_ordered pathList ((sn,p,tnb),(ind,rhs),(tns,rn))
        (*insert_path ((sn,p,tnb),(ind,rhs)) pathList*)
      in
      List.fold_left aux3 pathList paths
    (*else pathList*)
  in
  List.fold_left aux2 pathList pdev.table_it.(v).reducible

  (*let v,pdev = sn.state_nb,sn.pdev in
  let aux2 ind (rhs,dp) tns pathList =
    if dp = Array.length rhs (*&& (!automaton_kind=LR0 || (TNS.mem t tns) ||
         (TNS.mem dummy_token_name tns)) || (rule_kind=Dynamic_rule))*) then
      let paths = find_paths sn ind rhs in
      let aux3 pathList (p,tnb) =
        let () = if !dypgen_verbose>2 then print_path sn p else () in
        insert_partially_ordered pathList ((sn,p,tnb),(ind,rhs))
        (*insert_path ((sn,p,tnb),(ind,rhs)) pathList*)
      in
      List.fold_left aux3 pathList paths
    else pathList
  in
  IS.fold aux2 v.items pathList*)






exception Find_rightSib_failed

(** [find_rightSib] is used in [reduceViaPath] and [doShift].
    Its purpose is to find in the stack nodes list [snl] a stack node which
    holds the same grammar as [g_leftSib] and which holds a state which has an
    items set equal to [is_rightSib]. *)
let find_rightSib prio_imp g_nb_rightSib st_nb datadyn_rightSib snl
  gd ld gd_equal ld_equal =
  let rec aux snl = match snl with
    | [] -> raise Find_rightSib_failed
    | sn::tl ->
        let (v_nb,parsing_device, sn_prio_imp) =
          sn.state_nb, sn.pdev,
          sn.prio_imprint
        in
        if v_nb = st_nb && parsing_device.g_nb = g_nb_rightSib &&
          parsing_device.datadyn == datadyn_rightSib &&
          gd_equal parsing_device.data gd &&
          ld_equal parsing_device.local_data ld &&
          (prio_imp_equal prio_imp sn_prio_imp)
        then sn
        else aux tl
  in
  aux snl

(*let rec last_in_list l =  match l with
  | [x] -> x
  | _::tl -> last_in_list tl
  | [] -> failwith "error last_in_list, empty list"*)

(*let replace_in_pathList pathList edge_nb new_link =
  let aux2 e = if snd e.edge_label = edge_nb then new_link else e in
  let aux1 ((sn,edl,tnb),r) = ((sn,list_map aux2 edl,tnb),r) in
  list_map aux1 pathList*)


let print_sn sn =
  let chan = !log_stack_channel in
  let { state_nb = v; pdev = parsing_device; sn_nb = snnb;
    last_token = last_token; prio_imprint = prio_imp } = sn
  in
  let lhs_of_ind = parsing_device.lhs_of_ind in
  let gram_rhs = parsing_device.gram_rhs in
  let lhs_table = parsing_device.lhs_table in
  let is = parsing_device.table_it.(v) in
  let str_non_ter = parsing_device.str_non_ter in
  let str_prio = parsing_device.str_prio in
  output_string chan "____________________________________\n";
  let () = Printf.fprintf chan "STACK NODE <%d>, last token:%d\n" snnb last_token in
  output_string chan "\n";
  (*let () = print_state v lhs_of_ind in*)
  print_item_set chan is gram_rhs lhs_table lhs_of_ind str_non_ter str_prio;
  let () = Printf.fprintf chan "  state number: %d\n" v in
  output_string chan "\n";
  output_string chan " priority imprint:\n";
  print_prio_imp chan gram_rhs lhs_table prio_imp str_non_ter str_prio;
  let f3 f4 ed =
    let _,ednb = ed.edge_label in
    let { state_nb = v; sn_nb = snnb} = f4 ed in
    Printf.fprintf chan "  sn:%d ed:%d st:%d\n" snnb ednb v
  in
  output_string chan "\n";
  output_string chan " predecessor stack nodes :\n";
  (*let () = List.iter (f3 (fun e -> e.source.vertex_label))
    sn.pred_edges in
  output_string chan "\n";*)
  output_string chan " successor stack nodes :\n";
  let () = List.iter (f3 (fun e -> e.dest)) sn.succ_edges in
  output_string chan "\n"


let check_last_token last_token vl =
  vl.last_token >= last_token

open Lexing

exception Find_link_failed

let complete_reduction (*gs*) pathList topmost leftSib pdev_rightSib
    v_rightSib new_obj t nt lexer_pos prio counters data_equal
    dummy_token_name prio_imp_lS is_lS merge_map cons_index =
  countred := !countred + 1;
(*   let _ = Printf.fprintf !log_channel "complete_reduction called\n" in *)
  let gram_rhs, lhs_table = pdev_rightSib.gram_rhs, pdev_rightSib.lhs_table in
  let prio_imp,b =
    new_prio_imprint pdev_rightSib.prio
      prio_imp_lS
      is_lS
      (Non_ter nt) prio
      gram_rhs
      lhs_table leftSib.pdev.lhs_of_ind
      leftSib.pdev.str_non_ter
      leftSib.pdev.str_prio
  in
  (*if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel "new_prio_imprint returned:\n";
     print_prio_imp !log_channel pdev_rightSib.lhs_of_ind prio_imp;
     output_string !log_channel "\n");*)
  if b=false
  then
    ((*Printf.fprintf !log_channel "complete_reduction b=false\n";
    (*Printf.fprintf !log_channel "state nb : %d\n"
      leftSib.state_nb;*)
    print_prio_imp !log_channel gram_rhs lhs_table prio_imp_lS;
    output_string !log_channel "\n";
    print_item_set !log_channel is_lS gram_rhs lhs_table
      pdev_rightSib.lhs_of_ind;
    output_string !log_channel "\n";
    print_prio_imp !log_channel gram_rhs lhs_table prio_imp_lS;
    output_string !log_channel "\n";*)
    (*gs,*)pathList,topmost,counters,merge_map)
  else
  try
    let rightSib =
      find_rightSib prio_imp pdev_rightSib.g_nb v_rightSib
        pdev_rightSib.datadyn topmost 0 0
        (fun _ _ -> true) (fun _ _ -> true)
    in
    let find_link (*gs*) rightSib leftSib =
      let rec aux el = match el with
        | [] -> raise Find_link_failed
        | e::tl -> if (stack_node_equal e.dest leftSib) then e else aux tl
      in
      aux rightSib.succ_edges
    in
    try
      let link = find_link (*gs*) rightSib leftSib in
      (*let _ = Printf.fprintf !log_channel "complete_reduction called, merge\n" in*)
      let link_label = link.edge_label in
      let old_obj_list = (fst link_label) in
      let edge_nb = snd link_label in
      if (!dypgen_verbose>2 || E.merge_warning) && old_obj_list<>[] then
        (let (start_pos,end_pos) = lexer_pos in
        let col1 = start_pos.pos_cnum - start_pos.pos_bol in
        let col2 = end_pos.pos_cnum - end_pos.pos_bol in
        Printf.fprintf !log_channel "Warning: parser merges non terminal `%s'%s\nin file \"%s\", from l:%d,c:%d to l:%d,c:%d\n"
        (str_non_terminal nt pdev_rightSib.str_non_ter pdev_rightSib.str_prio)
        (str_with_priority prio pdev_rightSib.str_prio) start_pos.pos_fname start_pos.pos_lnum
        col1 end_pos.pos_lnum col2);

      let merge_item =
        try
          let (sn,_,_,objdata_list) = Int_map.find edge_nb merge_map in
          sn,link,cons_index,
          (new_obj,rightSib.pdev.data,
            rightSib.pdev.local_data)::objdata_list
        with Not_found ->
          let old_obj = match old_obj_list with
            | [o] -> o
            | _ -> assert false
          in
          rightSib,link,cons_index,
          [(new_obj,rightSib.pdev.data,
            rightSib.pdev.local_data);
          (old_obj,pdev_rightSib.data,pdev_rightSib.local_data)]
      in
      let merge_map = Int_map.add edge_nb merge_item merge_map in

      (*let merge = merge_array.(cons_index) in
      let new_obj_list,global_data,local_data = match
        merge old_obj_list pdev_rightSib.data pdev_rightSib.local_data
        new_obj rightSib.pdev.data
        rightSib.pdev.local_data with
          | Merge merge_res -> merge_res
          | Dont_merge -> raise Find_rightSib_failed
      in
      let () = rightSib.vertex_label <- { rightSib.vertex_label with
        pdev = { rightSib.pdev with
          data = global_data ; local_data = local_data }}
      in
      let () = link.edge_label <- (new_obj_list,edge_nb) in*)
      pathList,topmost,counters,merge_map
    with
      Find_link_failed ->
      (*let _ = Printf.fprintf !log_channel
        "complete_reduction called, Find_link_failed\n" in*)
      if data_equal.global_data_equal pdev_rightSib.data
        rightSib.pdev.data &&
        data_equal.local_data_equal pdev_rightSib.local_data
        rightSib.pdev.local_data then ()
        else raise Find_rightSib_failed;
      let link = create_e rightSib ([new_obj],counters.counted) leftSib
        (check_last_token leftSib.last_token) topmost in
      if (!dypgen_verbose>2) then
        Printf.fprintf !log_stack_channel "Edge %d created from <%d> to <%d>\n"
          counters.counted rightSib.sn_nb leftSib.sn_nb;
      let counters = {counters with counted = counters.counted+1 } in
      let pathList =
        insertLimitedReductions pathList link t topmost
          dummy_token_name
      in
      pathList,topmost,counters,merge_map
  with
    Find_rightSib_failed ->
    (*let _ = Printf.fprintf !log_channel
      "complete_reduction called, Find_rightSib_failed\n" in*)
    let rightSib =
      create_v v_rightSib pdev_rightSib counters.countsn
      counters.count_token lexer_pos prio_imp (leftSib.det_depth+1)
    in
    let _ = create_e rightSib ([new_obj],counters.counted) leftSib
      (check_last_token leftSib.last_token) topmost in
    if !dypgen_verbose>2 then print_sn rightSib;
    let counters = { counters with
      countsn = counters.countsn+1;
      counted = counters.counted+1 }
    in
    (*let gs = rightSib::gs in*)
    let topmost = rightSib::topmost in
    let pathList =
      insertLimitedReductions2 pathList t rightSib
       dummy_token_name
    in
    pathList,topmost,counters,merge_map



let position_map p start_node =
  let rec aux res l = match l with
    | [] -> List.rev (start_node.lexer_pos::res)
    | e::t -> aux (e.dest.lexer_pos::res) t
  in
  try aux [] (List.tl p)
  with Failure _ -> [start_node.lexer_pos]



let left_rec_rule lhs rhs =
  try match rhs.(0) with
    | Non_ter nt -> nt_of_lit nt = nt_of_lhs lhs
    | _ -> false
  with Invalid_argument _ -> false



let reduceViaPath (((start_node:('a,'b,'c) vertex),(p:('a,'b,'c) edge list),_),(ind,rhs),(rn,tns)) (t:token_name) (*(gs:(('a,'b) vertex) list)*) pathList (topmost:('a,'b,'c) vertex list) counters data_equal dummy_token_name test_cons str_cons merge_map =

  if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel "reduceViaPath called, start_node=%d, pathList length=%d\n  "
      start_node.sn_nb (List.length pathList);
    print_path start_node p;
    flush_all ());

  (*let position_map e = e.source.lexer_pos in
  let position_list = list_map position_map p in*)
  (*let first_token = match p with
    | [] -> counters.count_token
    | [_] -> start_node.first_token
    | _::h::_ -> h.dest.first_token
  in*)
  let position_list = position_map p start_node in
  let end_node_pos = try fst (List.hd position_list)
    with Failure _ -> Lexing.dummy_pos in
  let start_node_pos = snd start_node.lexer_pos in
  let symbol_pos = (end_node_pos,start_node_pos) in
  (* ^ this is the good order actually ^ *)
  let leftSib, snd_node = match p with
    | [] -> start_node, start_node
    | [h] -> h.dest, start_node
    | h1::h2::_ -> h1.dest, h2.dest
  in
  let v,pdev_leftSib =
    leftSib.state_nb,leftSib.pdev in
  let prio_dat = pdev_leftSib.prio in
  let prio = pdev_leftSib.prio_of_ind.(ind) in
  let nt_of_ind = pdev_leftSib.nt_of_ind in
  (* collect_objs collects objects along the path p.
     The head of the list corresponds to the leftmost
     object in gs. *)
  let rec collect_objs p = match p with
    | e::tl -> let obj_ll = collect_objs tl in
        let obj_list = fst e.edge_label in
        let f1 new_obj_ll obj_l =
          let f2 new_obj_ll obj =
            (obj::obj_l)::new_obj_ll
          in
          List.fold_left f2 new_obj_ll obj_list
        in
        let obj_ll = List.fold_left f1 [] obj_ll in
        obj_ll
    | [] -> [[]]
  in
  let obj_ll = collect_objs p in

  (*let ac_l = Map_rhs.find rhs pdev_leftSib.g.(ind) in*)
  let ac_l = start_node.pdev.actions.(rn) in
  (*let succ = v.succ_states in*)
  (*let v_rightSib = Li_map.find (Non_ter nt_of_ind.(ind)) succ  in*)
  let non_ter = fst_of_lhs pdev_leftSib.lhs_of_ind.(ind) in
  let ind_1 = ind_of_nt (nt_of_lhs pdev_leftSib.lhs_of_ind.(ind)) in
  let v_rightSib = pdev_leftSib.table.(v).(1).(ind_1) in
  let last_parsing_device = start_node.pdev in

  let foldfun (pathList,topmost,will_shift,counters,merge_map) toPass =
    try
      let rec try_actions ac_l = match ac_l with
        | (Dypgen_action f)::tl_ac_l ->
           let print_s outchan =
             print_table_state outchan
               start_node.state_nb
               last_parsing_device.table
               last_parsing_device.table_it
               last_parsing_device.table_lit_trans
               last_parsing_device.gram_rhs
               last_parsing_device.lhs_table
               last_parsing_device.lhs_of_ind
               last_parsing_device.str_non_ter
               last_parsing_device.nt_of_ind
               last_parsing_device.str_prio
            in
           let print_g outchan =
             print_grammar outchan
               last_parsing_device.gram_rhs
               last_parsing_device.lhs_table
               last_parsing_device.str_non_ter
               last_parsing_device.str_prio
           in
           (try
              let x = f toPass symbol_pos position_list last_parsing_device.data
              pdev_leftSib.datadyn pdev_leftSib.local_data
              last_parsing_device.local_data prio_dat
              { prt_state = print_s;
              prt_grammar = print_g } in
              x
           with Giveup -> try_actions tl_ac_l)
        | [] -> raise Giveup
      in
      let new_obj, will_shift2, keep_gram, newdata, newdatadyn, newlocal_data,
        rapf_add, r_remove, newprio, chan_s, chan_g = try_actions ac_l
      in
      let cons_index = try pdev_leftSib.cons_of_nt.(non_ter)
        with e -> (Printf.printf "dyp.ml reduceViaPath, error cons_of_nt:%d\n"
          (Array.length pdev_leftSib.cons_of_nt); raise e)
      in
      if test_cons.(cons_index) new_obj = false then
        let sr = str_rule rn pdev_leftSib.gram_rhs
          pdev_leftSib.lhs_table pdev_leftSib.str_non_ter pdev_leftSib.str_prio
        in
        let (_,cons) = Tools.find_nt
          pdev_leftSib.str_non_ter.(non_ter) pdev_leftSib.datadyn
        in
        raise (Bad_constructor(sr,cons,(str_cons new_obj)))
      else ();
      let v_rightSib,pdev_rightSib,counters,(prio_imp_lS,is_lS) =
        if rapf_add=[] && r_remove=[] && newprio==prio_dat then
          if (TNS.mem t tns = false)&&(TNS.mem dummy_token_name tns = false)
            && !automaton_kind<>`LR0 then raise Giveup else
          match keep_gram,
            left_rec_rule pdev_leftSib.lhs_of_ind.(ind) rhs with
          | true, true
            when snd_node.pdev.g_nb
            = last_parsing_device.g_nb ->
              snd_node.state_nb,
              { snd_node.pdev
                with data = newdata ;
                local_data = newlocal_data ;
                datadyn = last_parsing_device.datadyn },
               counters,
              (leftSib.prio_imprint,
              pdev_leftSib.table_it.(v))
          | true, _ ->
              let pdev_rightSib, rn_map =
                let is = pdev_leftSib.table_it.(v_rightSib) in
                let lit_trans = pdev_leftSib.table_lit_trans.(v_rightSib) in
                (update_parsing_device
                  { last_parsing_device with
                    gram_lhs = pdev_leftSib.gram_lhs;
                    gram_rhs = pdev_leftSib.gram_rhs;
                    lhs_table = pdev_leftSib.lhs_table;
                    actions = pdev_leftSib.actions }
                  [] []
                  newdata last_parsing_device.datadyn newlocal_data
                  newprio lit_trans
                  (counters.count_g+1) is.kernel_nt is.kernel_t is.reducible)
              in
              0,pdev_rightSib,
              { counters with count_g = counters.count_g+1 },
              (change_rn leftSib.prio_imprint
              pdev_leftSib.table_it.(v) rn_map)
          | _ ,_ ->
              v_rightSib,{ pdev_leftSib with data = newdata ;
              local_data = newlocal_data },counters,
              (leftSib.prio_imprint,
              pdev_leftSib.table_it.(v))
        else
          let pdev_rightSib, rn_map =
            let is = pdev_leftSib.table_it.(v_rightSib) in
            let lit_trans = pdev_leftSib.table_lit_trans.(v_rightSib) in
              (update_parsing_device pdev_leftSib rapf_add r_remove
                newdata newdatadyn newlocal_data newprio lit_trans
                (counters.count_g+1) is.kernel_nt is.kernel_t is.reducible)
          in
          0,pdev_rightSib,
          { counters with count_g = counters.count_g+1 },
          (change_rn leftSib.prio_imprint
          pdev_leftSib.table_it.(v) rn_map)
      in
      (match chan_s with
        | None -> ()
        | Some chan ->
             print_table_state chan v_rightSib
               pdev_rightSib.table
               pdev_rightSib.table_it
               pdev_rightSib.table_lit_trans
               pdev_rightSib.gram_rhs
               pdev_rightSib.lhs_table
               pdev_rightSib.lhs_of_ind
               pdev_rightSib.str_non_ter
               pdev_rightSib.nt_of_ind
               pdev_rightSib.str_prio);
      (match chan_g with
        | None -> ()
        | Some chan ->
             print_grammar chan
               pdev_rightSib.gram_rhs
               pdev_rightSib.lhs_table
               pdev_rightSib.str_non_ter
               pdev_rightSib.str_prio);

      let (*gs,*)pathList,topmost,counters,merge_map =
        complete_reduction (*gs*) pathList topmost leftSib
        pdev_rightSib v_rightSib new_obj t (nt_of_ind.(ind))
        symbol_pos prio counters data_equal dummy_token_name
        prio_imp_lS is_lS merge_map cons_index
      in
      (*gs,*)pathList,topmost,(will_shift2 && will_shift),counters,merge_map
    with Giveup -> ((*gs,*)pathList,topmost,will_shift,counters,merge_map)
  in
  List.fold_left foldfun (pathList,topmost,true,counters,merge_map) obj_ll



let remove_edge edge_nb sn =
  let rec aux succ_edges res = match succ_edges with
    | [] -> res
    | {edge_label = (_,n); dest = _ }::t when n=edge_nb -> res@t
    | e::t -> aux t (e::res)
  in
  sn.succ_edges <- aux sn.succ_edges []



let remove_path_edge edge_nb pathList =
  let aux1 e = match e with
    | { edge_label = (_,n) ; dest = _ } when n=edge_nb -> false
    | _ -> true
  in
  let aux2 ((_,p,_),_,_) = List.for_all aux1 p in
  List.filter aux2 pathList



let do_merge merge_map merge_array topmost pathList counters t dummy_token_name =
  let f edge_nb (sn,link,cons_index,objdata_list)
  (topmost,pathList,counters) =
    let obj_list,glo_dat,loc_dat =
      merge_array.(cons_index) objdata_list
    in
    let pdev = sn.pdev in
    if List.length sn.succ_edges >1 then
      (remove_edge edge_nb sn;
      if !dypgen_verbose>2 then Printf.fprintf !log_stack_channel "Edge %d removed\n" edge_nb;
      (*Printf.fprintf !log_channel "do_merge, nb de link>1, obj_datalist length=%d\n"
        (List.length objdata_list);*)
      let pathList = remove_path_edge edge_nb pathList in
      let rightSib = create_v sn.state_nb
        {pdev with data = glo_dat; local_data = loc_dat }
        counters.countsn sn.last_token sn.lexer_pos sn.prio_imprint
        (link.dest.det_depth+1)
      in
      let _ = create_e rightSib (obj_list,counters.counted) link.dest
        (check_last_token link.dest.last_token) topmost in
      if !dypgen_verbose>2 then print_sn rightSib;
      let counters = {counters with
        countsn = counters.countsn+1;
        counted = counters.counted+1 }
      in
      let topmost = rightSib::topmost in
      let pathList =
        insertLimitedReductions2 pathList t rightSib
         dummy_token_name
      in
      topmost,pathList,counters)
    else
      ((*sn <- { sn.vertex_label with pdev = {pdev with
        data = glo_dat; local_data = loc_dat } };*)
      let new_sn = { sn with pdev = {pdev with
        data = glo_dat; local_data = loc_dat } }
      in
      let topmost = new_sn::(List.filter (fun x -> x!=sn) topmost) in
      (*Printf.fprintf !log_channel "do_merge, nb de link=1, obj_datalist length=%d\n"
        (List.length objdata_list);*)
      link.edge_label <- (obj_list,edge_nb);
      topmost,pathList,counters)
  in
  Int_map.fold f merge_map (topmost,pathList,counters)



let compare_pr (((start_node,p,token_nb):('a,'b,'c) path),(ind,rhs), (rn,tns))
    ((start1,p1,tnb1),(ind1,rhs1),(tns1,rn1)) =
  if tnb1<token_nb then -1
  else if tnb1>token_nb then 1
  else
  let parsing_device1 = start1.pdev in
  let parsing_device = start_node.pdev in
  if parsing_device.g_nb <> parsing_device1.g_nb then 0
  else
   let non_ter_of_ind = parsing_device.non_ter_of_ind in
   let rel =
    match parsing_device.po.(non_ter_of_ind.(ind)).(non_ter_of_ind.(ind1)) with
    | Some b -> b
    | None -> assert false
  in
  if rel then 1 else 0



let doReductions (t:token_name) (*gs*) (topmost:('a,'b,'c) vertex list)
    entry_point counters data_equal dummy_token_name test_cons str_cons
    merge_array =

(* let _ = Printf.fprintf !log_channel "(doReductions) longueur topmost : %d\n" (List.length topmost) in *)

  let aux1 pathList sn =
    let st_nb,pdev = sn.state_nb,sn.pdev in

    (*let aux2 ind (rhs,dp) tns pathList =*)
    let aux2 pathList (rn,tns) =
      let ind = ind_of_lhs pdev.lhs_table.(rn) in
      if
        (!automaton_kind=`LR0 || t<>dummy_token_name ||
        (pdev.non_ter_of_ind.(ind))=entry_point ||
        (t=dummy_token_name && TNS.mem t tns) )
          (*(!automaton_kind=LR0 ||
          (t<>dummy_token_name && TNS.mem t tns) ||
          (pdev.non_ter_of_ind.(ind))=entry_point ||
          (t=dummy_token_name && TNS.mem t tns) )*)
          (* commented code above: for early test of the lookahead
            token, this makes parsing 5% faster but rules would need
            to be differentiated between classic and the one
            that change the grammar, because the test must not be
            performed on the latter. *)
      then
        let rhs = pdev.gram_rhs.(rn) in
        let paths =
          list_map (function (p,tnb) -> sn,p,tnb)
            (find_paths sn ind rhs rn)
        in
        let aux3 pathList p =
          insert_partially_ordered pathList (p,(ind,rhs),(tns,rn))
          (*insert_path (p,(ind,rhs)) pathList*)
        in
        List.fold_left aux3 pathList paths
      else pathList
    in

    List.fold_left aux2 pathList pdev.table_it.(st_nb).reducible
  in

  let pathList = List.fold_left aux1 (*Path_queue.empty*) [] topmost in

  let rec aux4 pathList topmost counters last_pr merge_map =
    match pathList with
    | [] ->
        let topmost,pathList,counters =
          do_merge merge_map merge_array topmost pathList counters t dummy_token_name
        in
        (match pathList with [] -> topmost,counters
          | _ -> aux4 pathList topmost counters None Int_map.empty)
    | pr::_ ->
        let merge_map,topmost,pathList,counters = match last_pr with
          | None -> merge_map,topmost,pathList,counters
          | Some pr0 ->
              if compare_pr pr pr0 = 1 then
                (let topmost,pathList,counters =
                  do_merge merge_map merge_array topmost pathList counters t dummy_token_name
                in
                Int_map.empty,topmost,pathList,counters)
              else merge_map,topmost,pathList,counters
        in
        (match pathList with
          | pr::tl ->
            let pathList,topmost,counters,merge_map =
              let pathList,topmost,will_shift,counters,merge_map =
                reduceViaPath pr t tl topmost counters data_equal
                  dummy_token_name test_cons str_cons merge_map
              in
              if will_shift = false then
                let (sn,_,_),_,_ = pr in
                let topmost = List.filter (function x -> x!=sn) topmost in
                pathList,topmost,counters,merge_map
              else pathList,topmost,counters,merge_map
            in
            aux4 pathList topmost counters (Some pr) merge_map
          | [] -> topmost,counters)
  in
  aux4 pathList topmost counters None Int_map.empty




let doShifts tok_name tok_value (*gs*) (prevTops:('a,'b,'c) vertex list)
    lexbuf_position lexbuf counters data_equal =
(*   let _ = Printf.fprintf !log_channel "(doShift) longueur prevTops : %d\n" (List.length prevTops) in *)
  let f ((*gs,*)topmost,counters) sn =
    let state_nb,parsing_device =
      sn.state_nb,sn.pdev
    in
    let table = parsing_device.table in
    let table_it = parsing_device.table_it in
    let next_state = table.(state_nb).(0).(tok_name) in
    if next_state = -1 then (*gs,*)topmost,counters else

    let prio_imp,b =
      new_prio_imprint parsing_device.prio sn.prio_imprint
        table_it.(state_nb) (Ter tok_name) 0
        parsing_device.gram_rhs parsing_device.lhs_table
        parsing_device.lhs_of_ind parsing_device.str_non_ter
        parsing_device.str_prio
    in
    if b=false then (*gs,*)topmost,counters else
    try
      let rightSib =
        find_rightSib prio_imp parsing_device.g_nb
        next_state parsing_device.datadyn topmost
        parsing_device.data parsing_device.local_data
        data_equal.global_data_equal
        data_equal.local_data_equal
      in
      let _ = create_e rightSib ([tok_value],counters.counted) sn
        (check_last_token sn.last_token) topmost in
      if (!dypgen_verbose>2) then
        Printf.fprintf !log_stack_channel "Link %d created from <%d> to <%d>\n"
          counters.counted rightSib.sn_nb sn.sn_nb;
      let counters = {counters with counted = counters.counted+1 } in
      (*gs,*)topmost,counters
    with Find_rightSib_failed ->
      let rightSib =
        create_v next_state parsing_device counters.countsn counters.count_token
        (lexbuf_position lexbuf) prio_imp (sn.det_depth+1)
      in
      let _ = create_e rightSib ([tok_value],counters.counted) sn
        (check_last_token sn.last_token) topmost in
      if !dypgen_verbose>2 then print_sn rightSib;
      let counters = { counters with
        countsn = counters.countsn+1;
        counted = counters.counted+1 }
      in
      (*let gs = rightSib::gs in*)
      (*gs,*)(rightSib::topmost),counters

  in
  List.fold_left f ((*gs,*)[],counters) prevTops



(** This function print the content of the (graph) stack [gs], it is meant
    for debugging purpose. *)
(*let print_gs gs title =
(* Problème posé par le fait que v est maintenant un numéro d'état et non plus un état. On pourrait passer les tableaux nécesssaires mais on ne peut quand même pas avoir les états des automates générés pour des extensions (numéro d'état non pertinent). *)
  output_string !log_channel "\n"; output_string !log_channel "\n";
  output_string !log_channel title;
  output_string !log_channel "\n";
  let f sn =
    let { state_nb = v; pdev = parsing_device; sn_nb = snnb;
      token_shifted = token_nb ; prio_imprint = prio_imp } = sn.vertex_label
    in
    let lhs_of_ind = parsing_device.lhs_of_ind in
    output_string !log_channel "____________________________________\n";
    let () = Printf.fprintf !log_channel "STACK NODE <%d>, token number : %d\n" snnb token_nb in
    output_string !log_channel "\n";
    (*let () = print_state v lhs_of_ind in*)
    let () = Printf.fprintf !log_channel "state number : %d\n" v in
    output_string !log_channel "\n";
    output_string !log_channel " priority imprint :\n";
    print_prio_imp lhs_of_ind prio_imp;
    let f3 f4 ed = ()
      (*let _,ednb = ed.edge_label in
      let { aut_vertex = v; sn_nb = snnb} = f4 ed in
      Printf.fprintf !log_channel "  sn:%d ed:%d st:%d\n" snnb ednb v.number*)
    in
    output_string !log_channel "\n";
    output_string !log_channel " predecessor stack nodes :\n";
    (*let () = List.iter (f3 (fun e -> e.source.vertex_label))
      sn.pred_edges in
    output_string !log_channel "\n";*)
    output_string !log_channel " successor stack nodes :\n";
    let () = List.iter (f3 (fun e -> e.dest.vertex_label)) sn.succ_edges in
    output_string !log_channel "\n"
  in
  List.iter f gs*)




(*let non_ter_of_li li = match li with
  | Non_ter nt -> nt
  | _ -> failwith "non_ter_of_li"*)

(** Despite the use of the [merge] function there may be several final parse
    objects, because two objects won't be merged if the two currents grammars
    are not the same, and because merge may keep several objects.
    Therefore, [log_parse_forest] is used to put in a list all the final parse
    objects. *)
let log_parse_forest successful entry_point gram_rhs =
  let f l sn =
    let prio_imp = sn.prio_imprint in
    let st_nb = sn.state_nb in
    let tlt = sn.pdev.table_lit_trans in
    let li = tlt.(st_nb) in
    (*let prio_l,_ = Item_map.find (r,1) prio_imp in*)
    (*let prio = 0 in*) (*List.hd prio_l in*)
    let prio = entry_point_prio entry_point prio_imp gram_rhs li in
    let edges = sn.succ_edges in
    let f2 l e =
      let snnb = e.dest.sn_nb in
      if snnb = 0 then
        let ol = fst e.edge_label in
        let f3 o = (o,prio) in
        (list_map f3 ol)@l
      else l
    in
    List.fold_left f2 l edges
  in
  List.fold_left f [] successful



(* The same automaton is used for all entry points, so the parser
tries to match all of them and then it selects the entry point it
is looking for. This may make the parser slower and should be
addressed in the future. *)
let glrParse deb_p get_value get_name str_token
    (entry_point:non_ter) data_equal test_cons str_cons merge_array
    global_data local_data
    rapf_list priority_data str_non_ter
    (lexfun:('a -> 'token)) (lexbuf:'a)
    (lexbuf_position:'a -> (Lexing.position * Lexing.position)) =

  let counters = {
    countsn = 0;
    counted = 0;
    count_token = 0;
    count_g = 0 }
  in

  let time1 = Sys.time () in

  let _,_,_,actions,_,_,
    _,user_g, _,_,_,
    _,_ = make_grammar rapf_list priority_data str_non_ter
  in

  let parsing_device = {
    gram_rhs = deb_p.deb_gram_rhs ;
    gram_lhs = deb_p.deb_gram_lhs ;
    lhs_table = deb_p.deb_lhs_table ;
    g_nb = 0;
    table = deb_p.deb_table ;
    table_it = deb_p.deb_table_it ;
    table_lit_trans = deb_p.deb_table_lit_trans ;
    lhslists = deb_p.deb_lhslists ;
    po = deb_p.deb_po ;
    data = deb_p.deb_data;
    local_data = deb_p.deb_local_data ;
    datadyn = deb_p.deb_datadyn ;
    prio = deb_p.deb_prio ;
    aut_kind = deb_p.deb_aut_kind ;
    nt_nb = deb_p.deb_nt_nb;
    non_ter_of_ind=deb_p.deb_non_ter_of_ind;
    prio_of_ind=deb_p.deb_prio_of_ind;
    nt_of_ind=deb_p.deb_nt_of_ind;
    lhs_of_ind=deb_p.deb_lhs_of_ind;
    str_non_ter=deb_p.deb_str_non_ter;
    cons_of_nt=deb_p.deb_cons_of_nt ;
    str_prio = deb_p.deb_str_prio;
    user_g = user_g;
    actions = actions; }
  in

  (*print_endline "parsing begins";*)

  (*dypgen_verbose := 3;*)

  let log_count = ref 0 in
  if !dypgen_verbose>2 then (
    (try
      let log_count_chan = open_in "dypgen_log_count" in
      let dlc = input_line log_count_chan in
      let dlc = if dlc = "" then "0" else dlc in
      let () = log_count := int_of_string dlc in
      close_in log_count_chan
    with _ -> log_count := 0);
    let log_count_chan = open_out "dypgen_log_count" in
    output_string log_count_chan (string_of_int (!log_count+1));
    close_out log_count_chan;
    log_channel :=
      open_out ("dypgen_"^(string_of_int !log_count)^".log");
    log_stack_channel :=
      open_out ("dypgen_gs_"^(string_of_int !log_count)^".log");
    output_string !log_stack_channel
    "\n\n--------------- Graph Structured Stack ---------------\n\n");

  let parsing_device = { parsing_device with
    data = global_data;
    local_data = local_data }
  in

  let start =
    create_v 0 parsing_device counters.countsn 0
    (lexbuf_position lexbuf) default_prio_imp 0
  in
  let dummy_token_name = Int_map.find entry_point dummy_token_map in
  let counters = { counters with countsn=counters.countsn+1 } in
  (*let gs = [start] in*)
  let topmost = [start] in
  (*let title_gs = "------------------------- Graph Structured Stack -------------------------\n" in*)
  (*let title_tm =
    "-------------------------- Topmost Stack Nodes --------------------------\n"
  in*)
  let is_successful sn =
    let st_nb = sn.state_nb in
    let tlt = sn.pdev.table_lit_trans in
    let li = tlt.(st_nb) in
    match li with
      | Non_ter nt when (non_ter_of_nt nt)=entry_point -> true
      | _ -> false
  in

  let rec aux_la (*gs*) topmost t counters =
    if !dypgen_verbose>2 then
      (Printf.fprintf !log_channel "\nTOKEN : %s, size of topmost = %d\n"
        (str_token t) (List.length topmost); flush_all ());
    let (*gs,*)topmost,counters =
      doReductions (get_name t) (*gs*) topmost entry_point counters
        data_equal dummy_token_name test_cons str_cons merge_array
    in
    let counters = { counters with
      count_token = counters.count_token+1 }
    in
    let (*gs,*)topmost,counters =
      doShifts (get_name t) (get_value t) (*gs*) topmost lexbuf_position
        lexbuf counters data_equal
    in
    let (*gs,*)topmost,counters =
      doReductions dummy_token_name (*gs*) topmost entry_point counters
        data_equal dummy_token_name test_cons str_cons merge_array in
      (* tries to reduce to the entry point *)
    if topmost = []
    then ((*if !dypgen_verbose>2 then print_gs (*gs*) title_gs;*)
      raise Syntax_error)
    else
    let successful = List.filter is_successful topmost in
    if successful<>[] then (*gs,*)successful,counters else
      aux_la (*gs*) topmost (lexfun lexbuf) counters
  in

  let rec aux_LR0 (*gs*) topmost t counters =
    (*Printf.printf "token number %d\n" counters.count_token;*)
    (*Gc.minor ();*)
    if !dypgen_verbose>2 then
      (Printf.fprintf !log_channel "\nTOKEN : %s, size of topmost = %d\n"
        (str_token t) (List.length topmost); flush_all ());
    let counters =
      { counters with count_token = counters.count_token+1 }
    in
    let (*gs,*)topmost,counters =
      doShifts (get_name t) (get_value t) (*gs*) topmost lexbuf_position
        lexbuf counters data_equal
    in
    let (*gs,*)topmost,counters =
      doReductions dummy_token_name (*gs*) topmost entry_point counters
        data_equal dummy_token_name test_cons str_cons merge_array
    in
    if topmost = []
    then ((*if !dypgen_verbose>2 then print_gs (*gs*) title_gs;*)
      raise Syntax_error)
    else
    let successful = List.filter is_successful topmost in
    if successful<>[] then (*gs,*)successful,counters else
    aux_LR0 (*gs*) topmost (lexfun lexbuf) counters
  in

  (*let gs,topmost = if !automaton_kind=LR0
    then doReductions dummy_token_name gs topmost
    else gs,topmost in*)
  let ((*gs,*)successful,counters) =
    try (
      if !automaton_kind <> `LR0
      then aux_la (*gs*) topmost (lexfun lexbuf) counters
      else
        let (*gs,*)topmost,counters = (* reduces initial epsilon rules *)
          doReductions dummy_token_name (*gs*) topmost entry_point
            counters data_equal dummy_token_name test_cons str_cons
            merge_array
        in
        aux_LR0 (*gs*) topmost (lexfun lexbuf) counters)
    with Syntax_error ->
      (flush_all ();
      if !dypgen_verbose>2 then close_out !log_channel;
      raise Syntax_error)
  in

  (*if !dypgen_verbose>2 then print_gs gs title_gs;*)
  if !dypgen_verbose>1 then
    (let time2 = Sys.time () in
    Printf.fprintf !log_channel "parsing time = %.3f\n"
      (time2-.time1);
    output_string !log_channel ("number of stack nodes = "^
      (string_of_int counters.countsn)^"\n");
    output_string !log_channel ("number of edges = "^
      (string_of_int counters.counted)^"\n");
    output_string !log_channel ("number of reductions = "^
      (string_of_int !countred)^"\n");
    flush_all ());
  (*print_endline "end of parsing";*)
  if !dypgen_verbose>2 then close_out !log_channel;
  log_parse_forest successful entry_point parsing_device.gram_rhs

end






module Ntt_PrioInAutomaton =
struct
  type non_terminal = non_ter * priority * int
  type lhs = non_ter * priority * int
  type lit_nt = non_terminal
  let nt_of_lit nt = nt
  let lit_of_nt nt = nt
  let nt_of_lhs nt = nt
  let fst_of_lhs (nt,_,_) = nt
  let ind_of_lhs (_,_,i) = i
  let ind_of_nt (_,_,i) = i
  let test_array_nt array_nt (_,_,i) =
    if array_nt.(i) then true else
      (array_nt.(i) <- true; false)
  let array_nt_size i = i
  let clean_array_nt array_nt nt_list =
    List.iter (fun (_,_,i) -> array_nt.(i) <- false) nt_list
  let new_rhs_lit (nt,p,_) array_nt_prio n =
    try let ind = Prio_map.find p array_nt_prio.(nt) in (nt,p,ind)
    with Not_found -> (nt,p,n)
  let get_lhs_prio (_,p,_) = p
  let str_token_name = E.str_token_name

  let str_non_terminal (nt,p,i) str_non_ter str_prio =
    if p = 0 then
      try
      "("^(str_non_ter.(nt))^":"^(string_of_int nt)^","^(string_of_int i)^")"
      with Invalid_argument _ -> (Printf.printf "nt=%d\n" nt; failwith "str_non_terminal")
      (*str_non_ter.(nt)*)
    else
      try
      "("^(str_non_ter.(nt))^
      (try
      ","^(str_prio.(p))^ ","^(string_of_int i)^")"
      with Invalid_argument _ -> (Printf.printf "p=%d\n" p; failwith "str_non_terminal"))
      with Invalid_argument _ -> (Printf.printf "nt=%d\n" nt; failwith "str_non_terminal")

  let str_lhs = str_non_terminal
end

module Ntt_PrioAtRuntime =
struct
  type non_terminal = non_ter
  type lhs = non_ter * priority * int
  type lit_nt = non_ter * non_terminal_priority
  let nt_of_lit (nt,_) = nt
  let lit_of_nt nt = (nt,No_priority)
  let nt_of_lhs (nt,_,_) = nt
  let fst_of_lhs (nt,_,_) = nt
  let ind_of_lhs (_,_,i) = i
  let ind_of_nt i = i
  let test_array_nt _ _ = false
  let clean_array_nt array_nt nt_list = ()
  let array_nt_size _ = 0
  let new_rhs_lit nt_lit _ _ = nt_lit
  let get_lhs_prio (_,p,_) = p
  let str_token_name = E.str_token_name
  let str_non_terminal nt str_non_ter _ = str_non_ter.(nt)
  let str_lhs (nt,p,i) str_non_ter str_prio =
    if p = 0 then
      "("^(str_non_ter.(nt))^","^(string_of_int i)^")"
    else
      "("^(str_non_ter.(nt))^
      ","^(str_prio.(p))^ ","^(string_of_int i)^")"
end

module Grammar_PrioInAutomaton = Grammar_struct(Ntt_PrioInAutomaton)
module Grammar_PrioAtRuntime = Grammar_struct(Ntt_PrioAtRuntime)




module PrioInAutomaton =
struct

(*let select_rule _ _ _ = true*)
(*let select_lhs nt _ _ = [nt]*)
let lhs_list nt _ _ = [nt]
type lhslists = Dummy_lhslists
let lhslists_init _ = Dummy_lhslists
let comp_lhslist nt _ _ _ = [nt]

(*let lhsl_of_non_ter nt _ = [nt]*)
let lhs_startprime = (0,0,0)

module OrdPrioNb =
struct
  type t = int * int
  let compare = Pervasives.compare
end
module PrioNb_set = Set.Make(OrdPrioNb)
module Non_ter_set = Set.Make(Ordered_non_ter)
open Grammar_PrioInAutomaton
module Map_rhs = Grammar_PrioInAutomaton.Map_rhs
type ('a,'b,'c) grammar = ('a,'b,'c) Grammar_PrioInAutomaton.grammar
type non_terminal = Grammar_PrioInAutomaton.non_terminal
type lhs = Grammar_PrioInAutomaton.lhs
type lit_nt = Grammar_PrioInAutomaton.lit_nt
let non_ter_of_nt  (nt,_,_) = nt
let non_ter_of_lhs (nt,_,_) = nt
let dummy_lhs = (0,0,0)
type item_set = Grammar_PrioInAutomaton.item_set
type rule_bis = Grammar_PrioInAutomaton.rule_bis
type rhs = Grammar_PrioInAutomaton.rhs
let str_with_priority _ _ = ""

let nt_prio_to_prio_set p_map (ps:Prio_set.t) p = match p with
  | No_priority -> ps
  | Eq_priority pr -> Prio_set.add pr Prio_set.empty
  | Less_priority pr -> (try fst (Prio_map.find pr p_map.prd_rel)
      with Not_found -> Prio_set.empty)
  | Lesseq_priority pr -> Prio_set.add pr (try (fst (Prio_map.find pr p_map.prd_rel))
      with Not_found -> Prio_set.empty)
  | Greater_priority pr -> (try snd (Prio_map.find pr p_map.prd_rel)
      with Not_found -> Prio_set.empty)
  | Greatereq_priority pr -> Prio_set.add pr (try (snd (Prio_map.find pr p_map.prd_rel))
      with Not_found -> Prio_set.empty)
      (* this may result in pr being 2 times in the list and result in
         2 times the same rule_bis, but it will be only once
         in the new_grammar because a grammar is a map of key
         rule_bis. *)
(*let nt_prio_to_prio_set p_map (ps:Prio_set.t) p = match p with
  | No_priority -> ps
  | Less_priority pr -> (try fst (Prio_map.find pr p_map.prd_rel)
      with Not_found -> Prio_set.empty)
  | Lesseq_priority pr -> Prio_set.add pr (try (fst (Prio_map.find pr p_map.prd_rel))
      with Not_found -> Prio_set.empty)
      (* this may result in pr being 2 times in the list and result in
         2 times the same rule_bis, but it will be only once
         in the new_grammar because a grammar is a map of key
         rule_bis. *)*)

let new_rules p_map array_nt_ps array_nt_prio ps ntn litl len prio nt_nb str_non_ter =
  let aux2 tn lit_l = (Ter tn)::lit_l in
  let aux3 nt p lit_ll =
    let rec aux4 lit_ll new_lit_ll = match lit_ll with
      | lit_l::tl ->
          let prio_set = nt_prio_to_prio_set p_map ps p in
          let prio_set2 = array_nt_ps.(nt) in
          let prio_set = Prio_set.inter prio_set prio_set2 in
          let f1 pr pn_set =
            let n = try Prio_map.find pr array_nt_prio.(nt)
              with Not_found -> failwith "Not_found in new_rules rhs"
                | Invalid_argument("index out of bounds") ->
                    (Printf.fprintf !log_channel "Error: non terminal `%s' is out of bounds.\n"
                    (str_non_ter.(nt));failwith "index out of bounds in new_rules rhs")
                    (*(Printf.fprintf !log_channel "Warning: non terminal `%s' is never in a left-hand side.\n"
                    (E.str_non_terminal nt); -1)*)
            in
            PrioNb_set.add (pr,n) pn_set
          in
          let pn_set = Prio_set.fold f1 prio_set PrioNb_set.empty in
          let f (pr,n) l = ((Non_ter (nt,pr,n))::lit_l)::l in
          let lit_ll_2 = PrioNb_set.fold f pn_set [] in
          aux4 tl lit_ll_2@new_lit_ll
      | [] -> new_lit_ll
    in
    aux4 lit_ll []
  in
  let rec aux1 litl lit_ll = match litl with
    | (Ter tn)::tl -> aux1 tl (list_map (aux2 tn) lit_ll)
    | (Non_ter (nt,p))::tl -> aux1 tl (aux3 nt p lit_ll)
    | [] -> lit_ll
  in
  let lit_ll = aux1 litl [[]] in
  let array_of_list l =
    let a = Array.make len (Ter 0) in
    let _ = List.fold_left (fun i x -> a.(i) <- x; (i+1)) 0 l in
    a
  in
  let lit_ll = List.map array_of_list lit_ll in
  let nb = try Prio_map.find prio array_nt_prio.(ntn)
    with Not_found -> failwith "Not_found in new_rules lhs"
  in
  let complete_new_rule lit_l = ((ntn,prio,nb),lit_l) in
  list_map complete_new_rule lit_ll


(* ps is the set of all priorities. *)
let make_real_grammar (user_g:('a,'b,'c) user_grammar) (pmap:priority_data) str_non_ter =

  if !dypgen_verbose>1 then
    (let nbr = Urule_map.fold (fun _ _ n -> n+1) user_g 0 in
    Printf.fprintf !log_channel "size of the grammar : %d rules\n" nbr);

  let ps =
    Prio_map.fold (fun p _ pset ->
      (*output_string !log_channel
      ("add priority :"^(priority_names.(p))^"\n");*)
      Prio_set.add p pset) pmap.prd_rel Prio_set.empty
  in
  let ps = Prio_set.add default_priority ps in
  let foldfun user_g (ep,_) =
    Urule_map.add (non_terminal_startprime,[Non_ter (ep,No_priority)],0)
      [(Dypgen_action(fun x _ _ d ld _ dd prd _ ->
      (List.hd x,true,false,d,ld,dd,[],[],prd,None,None)))] user_g
  in
  let user_g = List.fold_left foldfun user_g entry_points in
  (*let foldfun3 (nt,_,_) _ non_ter_set = Non_ter_set.add nt non_ter_set in
  let non_ter_set = Urule_map.fold foldfun3 user_g Non_ter_set.empty in
  let nt_nb = Non_ter_set.cardinal non_ter_set in*)
  let f1 (nt,litl,_) _ n =
    let n = max n nt in
    let f2 n lit = match lit with
      | Non_ter (nt,_) -> max n nt
      | _ -> n
    in
    List.fold_left f2 n litl
  in
  let nt_nb = (Urule_map.fold f1 user_g 0)+1 in

  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "number of non terminals : %d\n" nt_nb;

  if E.undef_nt then
    (let check_nt_def = Array.make nt_nb (false,false) in
    let rec f4 rhs = match rhs with
      | (Non_ter (nt,_))::tl ->
          let b,_ = check_nt_def.(nt) in
          check_nt_def.(nt) <- (b,true);
          f4 tl
      | _::tl -> f4 tl
      | [] -> ()
    in
    let f3 (nt,rhs,_) _ =
      let _,b = check_nt_def.(nt) in
      check_nt_def.(nt) <- (true,b);
      f4 rhs
    in
    Urule_map.iter f3 user_g;
    for i=0 to nt_nb-1 do
      if check_nt_def.(i) = (false,true) then
        raise (Undefined_nt(str_non_ter.(i)))
    done);

  let array_nt_ps = Array.make nt_nb Prio_set.empty in

  let iterfun (nt,_,p) _ = array_nt_ps.(nt) <- Prio_set.add p array_nt_ps.(nt) in

  Urule_map.iter iterfun user_g;

  let array_nt_prio = Array.make nt_nb Prio_map.empty in
  let rec f_rec i n = if i=nt_nb then n else
    let prio_set = array_nt_ps.(i) in
    let foldfun prio (prio_map,n) = (Prio_map.add prio n prio_map),(n+1) in
    let prio_map,n = Prio_set.fold foldfun prio_set (Prio_map.empty,n) in
    let () = array_nt_prio.(i) <- prio_map in
    f_rec (i+1) n
  in
  let newnt_nb = f_rec 0 0 in

  let non_ter_of_ind = Array.make newnt_nb 0 in
  let prio_of_ind = Array.make newnt_nb 0 in
  let nt_of_ind = Array.make newnt_nb (0,0,0) in
  let f2 nt p n =
    non_ter_of_ind.(n) <- nt;
    prio_of_ind.(n) <- p;
    nt_of_ind.(n) <- (nt,p,n)
  in
  for i=0 to nt_nb-1 do
    Prio_map.iter (f2 i) array_nt_prio.(i)
  done;

  (*let rec print_array_nt_prio i =
    if i=nt_nb then () else
    (print_string ("  "^(E.str_non_terminal i)^" : ");
    Prio_map.iter (fun p _ -> print_string (priority_names.(p)^" ")) array_nt_prio.(i);
    print_newline ();
    print_array_nt_prio (i+1))
  in
  print_endline "array_nt_prio :";
  print_array_nt_prio 0;*)

  (*let fiter nt ps =
    print_string ("nt: "^(E.str_non_terminal nt));
    Prio_set.iter (fun p -> print_string (priority_names.(p)^" ")) ps;
    output_string !log_channel "\n"
  in
  Nt_map.iter fiter nt_prio_map;*)

  let g = Array.make newnt_nb Map_rhs.empty in

  let aux (ntn,litl,p) a =
    let litl = List.rev litl in
    let rl =
      new_rules pmap array_nt_ps array_nt_prio ps ntn litl
        (List.length litl) p nt_nb str_non_ter
    in
    let f ((_,_,i),litl) = g.(i) <- Map_rhs.add litl a g.(i) in
    List.iter f rl
  in
  let () = Urule_map.iter aux user_g in
  let aux _ _ i = i+1 in
  let rec f nt i = if nt=newnt_nb then i else
    f (nt+1) (Map_rhs.fold aux g.(nt) i)
  in
  let nbr = f 0 0 in
  if !dypgen_verbose>1 then
    (Printf.fprintf !log_channel "size of the new grammar : %d rules\n" nbr;
    Printf.fprintf !log_channel "number of new non terminals : %d\n" newnt_nb;
    flush stdout);
  g,array_nt_prio,nt_nb,newnt_nb,nt_of_ind,non_ter_of_ind,
  prio_of_ind,nt_of_ind,nbr


type prio_imprint = int

let new_prio_imprint _ _ _ _ _ _ _ _ _ _ = 0,true
let change_rn pi is _ = pi,is
let default_prio_imp = 0
let prio_imprint_check _ _ _ = true
let prio_imp_equal _ _ = true
let print_prio_imp _ _ _ _ _ _ = ()
(* let prev_is is _ = is *)
let entry_point_prio _ _ _ li = match li with
  | Non_ter (_,p,_) -> p
  | Ter _ -> failwith "entry_point_prio, priority in automaton"

end



module PrioAtRuntime =
struct

open Grammar_PrioAtRuntime
module Map_rhs = Grammar_PrioAtRuntime.Map_rhs
type ('a,'b,'c) grammar = ('a,'b,'c) Grammar_PrioAtRuntime.grammar
type non_terminal = Grammar_PrioAtRuntime.non_terminal
type lit_nt = Grammar_PrioAtRuntime.lit_nt
type lhs = Grammar_PrioAtRuntime.lhs
let non_ter_of_nt nt = nt
let non_ter_of_lhs (nt,_,_) = nt
let dummy_lhs = (0,0,0)
type item_set = Grammar_PrioAtRuntime.item_set
type rule_bis = Grammar_PrioAtRuntime.rule_bis
type rhs = Grammar_PrioAtRuntime.rhs
let str_with_priority p str_prio = " with priority `"^(str_prio.(p))^"'"

(*let select_rule prio_dat ntn (_,p,_) =
  let nt,nt_p = ntn in
  match nt_p with
    | No_priority -> true
    | Eq_priority q -> p=q
    | Less_priority q -> is_relation prio_dat p q
    | Lesseq_priority q -> (is_relation prio_dat p q) || (p=q)
    | Greater_priority q -> is_relation prio_dat q p
    | Greatereq_priority q -> (is_relation prio_dat q p) || (p=q) *)

(*let str_nt_lit (nt,nt_p) =
  match nt_p with
    | No_priority -> "("^(str_non_terminal nt)^",No_priority)"
    | Eq_priority q -> "("^(str_non_terminal nt)^",="^priority_names.(q)^")"
    | Less_priority q -> "("^(str_non_terminal nt)^",<"^priority_names.(q)^")"
    | Lesseq_priority q -> "("^(str_non_terminal nt)^",<="^priority_names.(q)^")"
    | Greater_priority q -> "("^(str_non_terminal nt)^",>"^priority_names.(q)^")"
    | Greatereq_priority q -> "("^(str_non_terminal nt)^",>="^priority_names.(q)^")"*)

(*let select_lhs (nt,nt_p) prio_dat array_nt_prio =
  (*print_endline ("select_lhs sur "^(str_nt_lit (nt,nt_p)));*)
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
    else f (p+1)
  in
  let l = f 0 in
  (*List.iter (fun lhs -> print_endline (str_lhs lhs)) l;*)
  l*)

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
  type t = non_terminal_priority
  let compare = Pervasives.compare
end
module Ntp_map = Map.Make(Ordered_ntp)

type lhslists = ((lhs list) Ntp_map.t) array
let lhslists_init i = Array.make i Ntp_map.empty

let comp_lhslist (nt,nt_p) lhslists prio_dat array_nt_prio =
  (*print_endline ("comp_lhslist sur "^(str_nt_lit (nt,nt_p)));*)
  try Ntp_map.find nt_p lhslists.(nt) with Not_found ->(
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
  | e -> (Printf.printf "lhslists length=%d\n"
     (Array.length lhslists); raise e)

let lhs_startprime = (0,0,0)

(*let lhsl_of_non_ter nt array_nt_prio =
  let f p ind lhs_l = (nt,p,ind)::lhs_l in
  Prio_map.fold f array_nt_prio.(nt) []*)

(*let compute_nt_nb g =
  let f1 (nt,litl,_) _ n =
    let n = max n nt in
    let f2 n lit = match lit with
      | Non_ter (nt,_) -> max n nt
      | _ -> n
    in
    List.fold_left f2 n litl
  in
  (Map_r.fold f1 g 0)+1*)
(*  let foldfun3 (nt,_,_,_,_) _ non_ter_set = NTS.add nt non_ter_set in
  let non_ter_set = Map_r.fold foldfun3 g NTS.empty in
  NTS.cardinal non_ter_set*)

(*let rule_bis_of_rule (nt,l,p) = (nt,l,p,(List.length l))*)

(*let make_real_grammar user_g _ =
  let foldfun r a g = Map_r.add (rule_bis_of_rule r) a g in
  let g = Urule_map.fold foldfun user_g Map_r.empty in
  let foldfun g ep =
    Map_r.add (non_terminal_startprime,[Non_ter (ep,No_priority)],0,1)
      [(Dypgen_action(fun x _ _ d ld dd prd -> (List.hd x,true,d,ld,dd,[],[],prd)))] g
  in
  let nt_nb = compute_nt_nb g in
  (List.fold_left foldfun g entry_points),nt_nb,nt_nb*)


let make_real_grammar (user_g:('a,'b,'c) user_grammar) (pmap:priority_data) str_non_ter =
  if !dypgen_verbose>1 then
    (let nbr = Urule_map.fold (fun _ _ n -> n+1) user_g 0 in
    Printf.fprintf !log_channel "size of the grammar : %d rules\n" nbr);
  let foldfun user_g (ep,_) =
    Urule_map.add (non_terminal_startprime,[Non_ter (ep,No_priority)],0)
      [(Dypgen_action(fun x _ _ d ld _ dd prd _ ->
          (List.hd x,true,false,d,ld,dd,[],[],prd,None,None)))] user_g
  in
  let user_g = List.fold_left foldfun user_g entry_points in
  (*let foldfun3 (nt,_,_) _ non_ter_set = Non_ter_set.add nt non_ter_set in
  let non_ter_set = Urule_map.fold foldfun3 user_g Non_ter_set.empty in
  let nt_nb = Non_ter_set.cardinal non_ter_set in*)
  let f1 (nt,litl,_) _ n =
    let n = max n nt in
    let f2 n lit = match lit with
      | Non_ter (nt,_) -> max n nt
      | _ -> n
    in
    List.fold_left f2 n litl
  in
  let nt_nb = (Urule_map.fold f1 user_g 0)+1 in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "number of non terminals : %d\n" nt_nb;

  if E.undef_nt then
    (let check_nt_def = Array.make nt_nb (false,false) in
    let rec f4 rhs = match rhs with
      | (Non_ter (nt,_))::tl ->
          let b,_ = check_nt_def.(nt) in
          check_nt_def.(nt) <- (b,true);
          f4 tl
      | _::tl -> f4 tl
      | [] -> ()
    in
    let f3 (nt,rhs,_) _ =
      let _,b = check_nt_def.(nt) in
      check_nt_def.(nt) <- (true,b);
      f4 rhs
    in
    Urule_map.iter f3 user_g;
    for i=0 to nt_nb-1 do
      if check_nt_def.(i) = (false,true) then
        raise (Undefined_nt(str_non_ter.(i)))
    done);

  let array_nt_ps = Array.make nt_nb Prio_set.empty in

  let iterfun (nt,_,p) _ = array_nt_ps.(nt) <- Prio_set.add p array_nt_ps.(nt) in

  let () = Urule_map.iter iterfun user_g in

  let array_nt_prio = Array.make nt_nb Prio_map.empty in
  let rec f_rec i n = if i=nt_nb then n else
    let prio_set = array_nt_ps.(i) in
    let foldfun prio (prio_map,n) = (Prio_map.add prio n prio_map),(n+1) in
    let prio_map,n = Prio_set.fold foldfun prio_set (Prio_map.empty,n) in
    let () = array_nt_prio.(i) <- prio_map in
    f_rec (i+1) n
  in
  let newnt_nb = f_rec 0 0 in

  let non_ter_of_ind = Array.make newnt_nb 0 in
  let prio_of_ind = Array.make newnt_nb 0 in
  let lhs_of_ind = Array.make newnt_nb (0,0,0) in
  let f2 nt p n =
    non_ter_of_ind.(n) <- nt;
    prio_of_ind.(n) <- p;
    lhs_of_ind.(n) <- (nt,p,n)
  in
  for i=0 to nt_nb-1 do
    Prio_map.iter (f2 i) array_nt_prio.(i)
  done;

  (*let rec print_array_nt_prio i =
    if i=nt_nb then () else
    (print_string ("  "^(str_non_terminal i)^" : ");
    Prio_map.iter (fun p _ -> print_string (priority_names.(p)^" ")) array_nt_prio.(i);
    print_newline ();
    print_array_nt_prio (i+1))
  in
  print_endline "array_nt_prio :";
  print_array_nt_prio 0;*)

  (*let fiter nt ps =
    print_string ("nt: "^(E.str_non_terminal nt));
    Prio_set.iter (fun p -> print_string (priority_names.(p)^" ")) ps;
    output_string !log_channel "\n"
  in
  Nt_map.iter fiter nt_prio_map;*)

  let g = Array.make newnt_nb Map_rhs.empty in

  let array_of_list l =
    let a = Array.make (List.length l) (Ter 0) in
    let _ = List.fold_left (fun i x -> a.(i) <- x; (i+1)) 0 l in
    a
  in

  let aux (ntn,litl,p) a =
    let ind = Prio_map.find p array_nt_prio.(ntn) in
    g.(ind) <- Map_rhs.add (array_of_list litl) a g.(ind)
  in
  let () = Urule_map.iter aux user_g in

  let aux _ _ i = i+1 in
  let rec f nt i = if nt=newnt_nb then i else
    f (nt+1) (Map_rhs.fold aux g.(nt) i)
  in
  let nbr = f 0 0 in
  if !dypgen_verbose>1 then
    (Printf.fprintf !log_channel "size of the new grammar : %d rules\n" nbr;
    Printf.fprintf !log_channel "number of new non terminals : %d\n" newnt_nb;
    flush stdout);
  g,array_nt_prio,nt_nb,nt_nb,non_ter_of_ind,non_ter_of_ind,prio_of_ind,lhs_of_ind,nbr


(*type prio_imprint = int*)
type prio_imprint = ((priority list) * bool) Item_map.t

let symb_of_lit lit = match lit with
  | Ter t -> Ter t
  | Non_ter (n,_) -> Non_ter n

(*let new_prio_imprint _ _ _ _ = 0,true*)

let print_prio_imp chan gram_rhs lhs_table prio_imp str_non_ter str_prio =
  let f (rn,dp) (pl,b) =
    print_kernel chan gram_rhs lhs_table str_non_ter str_prio (rn,dp);
    let f2 s p = s^(str_prio.(p))^" " in
    output_string chan ("  "^(string_of_bool b)^" : ["^(List.fold_left f2 " " pl)^"]\n")
  in
  Item_map.iter f prio_imp


let new_prio_imprint priodat prio_imp is symb prio gram_rhs lhs_table lhs_of_ind str_non_ter str_prio =
  let f (new_pi,b) (rn,dp) =
    let rhs = gram_rhs.(rn) in
    let lit = rhs.(dp) in
    if (symb_of_lit lit)<>symb then (new_pi,b) else
    let b1 = match lit with
      | Ter _ | Non_ter (_,No_priority) -> true
      | Non_ter (_,Eq_priority p) -> prio=p
      | Non_ter (_,Less_priority p) ->
          is_relation priodat prio p
      | Non_ter (_,Lesseq_priority p) ->
          (is_relation priodat prio p)||(p=prio)
      | Non_ter (_,Greater_priority p) ->
          is_relation priodat p prio
      | Non_ter (_,Greatereq_priority p) ->
          (is_relation priodat p prio)||(p=prio)
    in
    let old_prio_l,old_b =
      if dp=0 then ([],true) else
      try Item_map.find (rn,dp) prio_imp
      with Not_found ->
        (
        print_kernel !log_channel gram_rhs lhs_table str_non_ter str_prio (rn,dp);
        print_prio_imp !log_channel gram_rhs lhs_table prio_imp str_non_ter str_prio;
        failwith "new_prio_imprint")
    in
    (Item_map.add (rn,dp+1) ((prio::old_prio_l),(b1 && old_b)) new_pi),
    (b || b1)
  in
  let im,b = List.fold_left f (Item_map.empty,false) is.kernel_t in
  let im,b = List.fold_left f (im,b) is.kernel_nt in
  let g x rn = f x (rn,0) in
  List.fold_left g (im,b) is.non_kernel
  (* If the returned bool is true then the parser can complete the reduction.
  In the case of the shift it is false only if the token is not
  expected, not because of priorities. *)

let change_rn prio_imp is rn_map =
  let new_is = new_item_set () in
  let f kernel (rn,x) =
    try
      let new_rn = Int_map.find rn rn_map in
      (new_rn,x)::kernel
    with Not_found -> kernel
  in
  new_is.kernel_nt <- List.fold_left f [] is.kernel_nt;
  new_is.kernel_t <- List.fold_left f [] is.kernel_t;
  new_is.reducible <- List.fold_left f [] is.reducible;
  let g non_kernel rn =
    try
      let new_rn = Int_map.find rn rn_map in
      new_rn::non_kernel
    with Not_found -> non_kernel
  in
  new_is.non_kernel <- List.fold_left g [] is.non_kernel;

  let h (rn,dp) pi new_prio_imp =
    try
      let new_rn = Int_map.find rn rn_map in
      Item_map.add (new_rn,dp) pi new_prio_imp
    with Not_found -> new_prio_imp
  in
  let new_prio_imp = Item_map.fold h prio_imp Item_map.empty in
  new_prio_imp, new_is

let prio_imprint_check rn gram_rhs prio_imp =
  let _,b = try Item_map.find (rn,Array.length gram_rhs.(rn)) prio_imp
    with Not_found -> [],true in
  b
  (*let _,b = try Item_map.find (ind,(rhs,Array.length rhs)) prio_imp
    with Not_found -> [],true in
  b*)

let default_prio_imp = Item_map.empty
let prio_imp_equal pi1 pi2 = Item_map.equal (=) pi1 pi2

exception Prio_found of priority

let entry_point_prio entry_point (prio_imp:prio_imprint) (gram_rhs:rhs array) _ =
  let f (rn,dp) (prio_l,_) =
    if rn >= Array.length gram_rhs then (
      Printf.fprintf !log_channel "rn=%d, gram_rhs len=%d\n"
      rn (Array.length gram_rhs); failwith "entry_pp");
    if dp<>1 || Array.length gram_rhs.(rn)<>1 then () else
    match gram_rhs.(rn).(0) with
      | Non_ter (nt,_) when nt=entry_point ->
          raise (Prio_found (List.hd prio_l))
      | _ -> ()
  in
  try Item_map.iter f prio_imp;
    failwith "entry_point_prio"
  with Prio_found prio -> prio

  (*let r =
    (lhs_startprime,[Non_ter(entry_point,No_priority)],1)
  in*)
  (*let prio_l,_ = Item_map.find
    ((ind_of_lhs lhs_startprime),(([|Non_ter(entry_point,No_priority)|]),1))
    prio_imp in
  try List.hd prio_l with Failure _ -> failwith "entry_point_prio"*)

(*let prev_is is gram_rhs =
  let f (kt,knt,nk) (rn,dp) =
    if dp=1 then (kt,knt,rn::nk) else
    match gram_rhs.(rn).(dp-1) with
      | Ter _ -> ((rn,dp-1)::kt,knt,nk)
      | Non_ter _ -> (kt,(rn,dp-1)::knt,nk)
  in
  let kernel_t, kernel_nt, non_kernel =
    List.fold_left f ([],[],[]) is.kernel_t in
  let kernel_t, kernel_nt, non_kernel =
    List.fold_left f (kernel_t, kernel_nt, non_kernel) is.kernel_t
  in
  let reducible = List.map
    (fun (rn,_) -> (rn,Array.length gram_rhs.(rn)))
    is.reducible
  in
  let reducible = List.filter (fun (_,dp) -> dp>0) reducible in
  let kernel_t, kernel_nt, non_kernel =
    List.fold_left f (kernel_t, kernel_nt, non_kernel) reducible
  in
  { dummy_item_set with
  kernel_t = kernel_t;
  kernel_nt = kernel_nt;
  non_kernel = non_kernel }*)

end

module Parser_PIA = Parser(Grammar_PrioInAutomaton)(PrioInAutomaton)
module Parser_PAR = Parser(Grammar_PrioAtRuntime)(PrioAtRuntime)

module type Parser_type =
sig

type ('obj,'data,'local_data) parsing_device
    (** Abstract type of a structure which contains an parsing_device, the
    grammar associated to it and the actions associated to the grammar and
    other data. *)


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
  deb_datadyn : datadyn;
  deb_aut_kind : automaton_kind;
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
    (** Returns the parsing_device which parses strings written with the input
        grammar and assuming the relations between priority classes which
        are contained in the input priority data. *)

  (*val update_parsing_device_data : ('obj,'data,'local_data) parsing_device ->
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


end


