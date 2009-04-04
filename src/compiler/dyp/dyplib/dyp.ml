let list_map f l = List.rev (List.rev_map f l)

include Dyplex
open Dyp_special_types

let default_priority = 0

type ('token,'obj,'data,'local_data,'lexbuf) dypgen_toolbox = {
  parser_pilot : ('token,'obj,'data,'local_data,'lexbuf) parser_pilot;
  global_data : 'data;
  local_data : 'local_data;
  last_local_data : 'local_data;
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
      ('obj list -> 'obj * ('token,'obj,'gd,'ld,'l) dyp_action list))) list
  | Bind_to_cons of (string * string) list
  | Global_data of 'gd
  | Keep_grammar
  | Local_data of 'ld
  | Next_grammar of out_channel
  | Next_state of out_channel
  | Relation of string list list
  | Dont_shift

exception Giveup
exception Undefined_nt of string
exception Undefined_ter of string
exception Bad_constructor of (string * string * string)
exception Constructor_mismatch of (string * string)
exception Syntax_error
    (** This exception is raised by the function parse if the parser is
    stuck in a situtation where no shift and no reduction is possible. *)


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

let countred = ref 0
(* counts the number if reductions performed
it is only used for information *)

type counters = {
  mutable countsn : int;
  mutable counted : int;
  mutable count_token : int;
  mutable last_layout : int;
  mutable count_g : int; (* counts number of grammars *)
  mutable count_lex : int } (* counts number of lexers *)

let new_counters () = {
  countsn = 0;
  counted = 0;
  count_token = 0;
  last_layout = 0;
  count_g = 0;
  count_lex = 0 }

module Ordered_urule =
struct
  type t = nrule
  let compare = Pervasives.compare
end

module Urule_map = Map.Make(Ordered_urule)
type ('t,'a,'b,'c,'d) user_grammar = (('t,'a,'b,'c,'d) action) list Urule_map.t

module NTS = Set.Make(Ordered_non_ter)

module Tools =
struct
  type ('token,'obj,'gd,'ld,'l) action_com = {
    ac_gd : 'gd;
    ac_ld : 'ld;
    add_rules : (rule * (('token,'obj,'gd,'ld,'l) dypgen_toolbox ->
      ('obj list -> 'obj * ('token,'obj,'gd,'ld,'l) dyp_action list))) list;
    new_nt_cons : (string * string) list;
    ac_relations : string list list;
    will_shift : bool;
    keep_grammar : bool;
    next_state : out_channel option;
    next_grammar : out_channel option }

  let rec action_command dal da = match dal with
    | [] -> da
    | (Global_data gd)::t -> action_command t { da with ac_gd = gd }
    | (Local_data ld)::t -> action_command t { da with ac_ld = ld }
    | (Add_rules ar)::t ->
        action_command t { da with add_rules = ar@da.add_rules }
    | (Bind_to_cons l)::t ->
         action_command t { da with new_nt_cons = l@da.new_nt_cons }
    | (Relation l)::t ->
         action_command t { da with ac_relations = l@da.ac_relations }
    | Dont_shift::t -> action_command t { da with will_shift = false }
    | Keep_grammar::t -> action_command t { da with keep_grammar = true }
    | (Next_state ns)::t -> action_command t { da with next_state = Some ns }
    | (Next_grammar ng)::t -> action_command t { da with next_grammar = Some ng }

  let make_dyp symbol_pos position_list data_arg local_data_arg
  last_local_data_arg debug_infos parser_pilot next_lexeme =
    { parser_pilot = parser_pilot;
      global_data = data_arg;
      local_data = local_data_arg;
      last_local_data = last_local_data_arg;
      next_lexeme = next_lexeme;
      symbol_start = (fun () -> (fst symbol_pos).Lexing.pos_cnum);
      symbol_start_pos = (fun () -> fst symbol_pos);
      symbol_end = (fun () -> (snd symbol_pos).Lexing.pos_cnum);
      symbol_end_pos = (fun () -> snd symbol_pos);
      rhs_start =
        (fun i -> (fst (List.nth position_list (i-1))).Lexing.pos_cnum);
      rhs_start_pos = (fun i -> fst (List.nth position_list (i-1)));
      rhs_end = (fun i -> (snd (List.nth position_list (i-1))).Lexing.pos_cnum);
      rhs_end_pos = (fun i -> snd (List.nth position_list (i-1)));
      print_state =
        debug_infos.prt_state;
      print_grammar =
        debug_infos.prt_grammar
    }

  let rec transform_action a av_list symbol_pos position_list data_arg
  local_data_arg last_local_data_arg debug_infos parser_pilot next_lexeme =
    let dyp =
      make_dyp symbol_pos position_list data_arg local_data_arg
      last_local_data_arg debug_infos parser_pilot next_lexeme
    in
    let new_obj, dal = a dyp av_list in
    let mapfun (r, ac) = (r, (Dypgen_action
      (fun ol pos posl gd ld lld di p nl ->
      (transform_action ac) ol pos posl gd ld lld di p nl)), [])
    in
    let dyp_a = action_command dal {
      ac_gd = data_arg;
      ac_ld = local_data_arg;
      add_rules = [];
      new_nt_cons = [];
      ac_relations = [];
      will_shift = true;
      keep_grammar = false;
      next_state = None;
      next_grammar = None }
    in
    let add_rules_transformed = List.map mapfun dyp_a.add_rules in
    (new_obj,dyp_a.will_shift,dyp_a.keep_grammar,dyp_a.ac_gd,
    dyp_a.ac_ld,add_rules_transformed,dyp_a.new_nt_cons,
    dyp_a.ac_relations,dyp_a.next_state,dyp_a.next_grammar)

  let transform_inh_val a av_list symbol_pos position_list data_arg
  local_data_arg last_local_data_arg debug_infos parser_pilot next_lexeme =
    let dyp =
      make_dyp symbol_pos position_list data_arg local_data_arg
      last_local_data_arg debug_infos parser_pilot next_lexeme
    in
    a dyp av_list

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

  let hashtbl_of_array a =
    let len = Array.length a in
    if len = 0 then failwith "hashtbl_of_array" else
    let ht = Hashtbl.create len in
    for i=0 to len-1 do
      Hashtbl.add ht a.(i) i
    done;
    ht

  let make_nt_cons_map nt_cons_list =
    List.fold_left
    (fun ncm (nt_str,cons_i) -> String_map.add nt_str cons_i ncm)
    String_map.empty nt_cons_list
end

open Tools

let dummy_lhs = (0,0,0)
let str_with_priority p priority_names = " with priority `"^(priority_names.(p))^"'"

let make_real_grammar user_g (pmap:priority_data) str_non_ter nt_table ppar =
  if !dypgen_verbose>1 then
    (let nbr = Urule_map.fold (fun _ _ n -> n+1) user_g 0 in
    Printf.fprintf !log_channel "size of the grammar : %d rules\n" nbr);
  
  (*Printf.printf "nt_table:\n";
  Hashtbl.iter (fun nt_str nt_int -> Printf.printf "%s : %d\n" nt_str nt_int) nt_table;
  flush_all ();*)
  
  let nt_nb = Hashtbl.length nt_table + 1 in
  
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "number of non terminals : %d\n" nt_nb;
  
  if ppar.undef_nt then
    (let check_nt_def = Array.make nt_nb (false,false) in
    let rec f4 rhs = match rhs with
      | (Ps_Non_ter (nt,_))::tl
      | (Ps_Non_ter_NL (nt,_))::tl ->
          let b,_ = check_nt_def.(nt) in
          check_nt_def.(nt) <- (b,true);
          f4 tl
      | _::tl -> f4 tl
      | [] -> ()
    in
    let f3 (nt,rhs,_,_) _ =
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
  
  let all_prio_set =
    let rec f i set =
      if i= -1 then set else
      f (i-1) (Prio_set.add i set)
    in f (pmap.prd_nb-1) Prio_set.empty
  in
  
  let iterfun (nt,rhs,p,_) _ =
    array_nt_ps.(nt) <- Prio_set.add p array_nt_ps.(nt);
    List.iter
    (fun li -> match li with
      | Ps_Non_ter (nt,ntp) | Ps_Non_ter_NL (nt,ntp) -> (match ntp with
        | No_priority -> array_nt_ps.(nt) <- all_prio_set
        | Eq_priority p ->
            array_nt_ps.(nt) <- Prio_set.add p array_nt_ps.(nt)
        | Less_priority p ->
            array_nt_ps.(nt) <-
            Prio_set.union array_nt_ps.(nt) (fst pmap.prd_rel.(p))
        | Lesseq_priority p ->
            array_nt_ps.(nt) <-
            Prio_set.union array_nt_ps.(nt)
            (Prio_set.add p (fst pmap.prd_rel.(p)))
        | Greater_priority p ->
            array_nt_ps.(nt) <-
            Prio_set.union array_nt_ps.(nt) (snd pmap.prd_rel.(p))
        | Greatereq_priority p ->
            array_nt_ps.(nt) <-
            Prio_set.union array_nt_ps.(nt)
            (Prio_set.add p (snd pmap.prd_rel.(p))))
      | _ -> ())
    rhs
  in
  
  let () = Urule_map.iter iterfun user_g in
  
  let array_nt_prio = Array.make nt_nb Prio_map.empty in
  let rec f_rec i n = if i=nt_nb then n else
    let prio_set = array_nt_ps.(i) in
    let foldfun prio (prio_map,n) =
      (*Printf.fprintf !log_channel "add (%s,%s) (%d,%d,%d)\n" str_non_ter.(i)
      pmap.prd_names.(prio) i prio n;*)
      (Prio_map.add prio n prio_map),(n+1)
    in
    let prio_map,n = Prio_set.fold foldfun prio_set (Prio_map.empty,n) in
    let () = array_nt_prio.(i) <- prio_map in
    f_rec (i+1) n
  in
  (*Printf.fprintf !log_channel "make array_nt_prio\n";*)
  let newnt_nb = f_rec 0 0 in
  
  let non_ter_of_ind = Array.make newnt_nb 0 in
  let prio_of_ind = Array.make newnt_nb 0 in
  let f2 nt p n =
    non_ter_of_ind.(n) <- nt;
    prio_of_ind.(n) <- p;
  in
  for i=0 to nt_nb-1 do
    Prio_map.iter (f2 i) array_nt_prio.(i)
  done;
  
  let g = Array.make newnt_nb Map_rhs.empty in
  
  let array_of_list l =
    let a = Array.make (List.length l) (Ps_Ter 0) in
    let _ = List.fold_left (fun i x -> a.(i) <- x; (i+1)) 0 l in
    a
  in
  
  Urule_map.iter
    (fun (ntn, litl, p, b) a ->
      let ind = Prio_map.find p array_nt_prio.(ntn) in
      g.(ind) <- Map_rhs.add ((array_of_list litl), b) a g.(ind))
    user_g;
  
  let nbr =
    let rec f nt i =
      if nt = newnt_nb then i else
      f (nt+1) (Map_rhs.fold (fun _ _ i -> i+1) g.(nt) i) in
    f 0 0
  in
  
  if !dypgen_verbose>1 then
    (Printf.fprintf !log_channel
      "size of the extended grammar : %d rules\n" nbr;
    Printf.fprintf !log_channel
      "number of non terminals : %d\n" newnt_nb;
    flush stdout);
  
  g, array_nt_prio, nt_nb, non_ter_of_ind, prio_of_ind, nbr



module Ordered_nrule =
struct
  type t = nrule
  let compare = Pervasives.compare
end

module RS = Set.Make (Ordered_nrule)


(*let rec derive_epsilon nt1 vr nt_epsilon user_g =
  match nt_epsilon.(nt1) with (True|False) as b -> b | NA ->
  let f (r:nrule) _ b =
    if b=True then b else
    let (nt,litl,_,_) = r in
    if nt<>nt1 then False else
    if RS.mem r vr then False else
    let rec f2 litl = match litl with
      | (Ps_Non_ter (nt,_))::t | (Ps_Non_ter_NL (nt,_))::t ->
          nt::(f2 t)
      | _ -> []
    in
    let ntl = f2 litl in
    let rec f3 (ntl:non_ter list) = match ntl with
      | [] -> True
      | nt::t ->
          let b =
            let b = derive_epsilon nt (RS.add r vr) nt_epsilon user_g in
            nt_epsilon.(nt) <- b; b
          in
          if b=True then f3 t else False
    in
    f3 ntl
  in
  Urule_map.fold f user_g False*)



(* Computes the array nullable for the non trivial cases.
This array tells whether a nonterminal can derive the empty string.
The grammar is assumed to only contains rules with rhs of two or
more nonterminals and no terminals. *)
let make_nullable gram_rhs lhs_table lhslists priodata array_nt_prio sub_g nullable =
  let f rn b =
    let _,_,ind = lhs_table.(rn) in
    if nullable.(ind) then b else
    let rec all_nullable rhs = function 0 -> true | i ->
      match rhs.(i-1) with
      | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
        let lhslist = comp_lhslist nt lhslists priodata array_nt_prio in
        if List.exists (fun (nt,_,_) -> nullable.(nt)) lhslist
        then all_nullable rhs (i-1) else false
      | _ -> assert false
    in
    let rhs = gram_rhs.(rn) in
    if all_nullable rhs (Array.length rhs)
    then (nullable.(ind) <- true; true) else b
  in
  let rec aux () = if Int_set.fold f sub_g false then aux () in
  aux ()


(* This is a tail-recursive implementation of the closure algorithm
outlined in Martin Bravenboer PhD thesis Chapter 1 p.29.
When a node n1 points to a node n2 it means: set(n1) includes set(n2).
The array result is an array of Int_set.t.
The array nodes contains the list of adjacent vertices of each vertex,
when the function returns, nodes is an array of empty lists. *)
let closure_of_include (nodes:int list array) (result:Int_set.t array) =
  let node_nb = Array.length nodes in
  let color = Array.make node_nb `white in
  let root = Array.make node_nb (-1) in
  let d = Array.make node_nb (-1) in
  let rec pop res date = function
    | (w,d)::t when d >= date ->
        result.(w) <- res;
        color.(w) <- `blue;
        pop res date t
    | stack -> stack
  in
  let rec visit time stack visited v =
    if d.(v) = -1 then
      (color.(v) <- `gray;
      d.(v) <- time;
      root.(v) <- time);
    match nodes.(v) with
    | w::t ->
        nodes.(v) <- t;
        if color.(w)=`white then visit (time+1) stack (v::visited) w
        else
          (if (color.(w)=`blue || color.(w)=`black) && d.(v) >= d.(w)
          then result.(v) <- Int_set.union result.(v) result.(w);
          if color.(w) <> `blue && root.(w) < root.(v)
          then root.(v) <- root.(w);
          visit time stack visited v)
    | [] ->
        let stack =
          if root.(v) = d.(v) then
            (color.(v) <- `blue;
            pop result.(v) d.(v) stack)
          else
            (color.(v) <- `black;
            (v,time)::stack)
        in
        (match visited with
          | u::t ->
              result.(u) <- Int_set.union result.(u) result.(v);
              visit time stack t u
          | [] -> time)
  in
  let rec aux id time =
    if id < node_nb then
      let time = visit time [] [] id in
      aux (id+1) time
  in
  aux 0 0



let array_for_all f a =
  let rec aux i =
    if i=0 then true else
    if f (i-1) a.(i-1) then aux (i-1) else false
  in
  aux (Array.length a)



let make_po_array gram_rhs lhs_table lhslists priodata array_nt_prio sub_g nullable po_array cyclic_rule =
  let lhs_nb = Array.length po_array in
  let result = Array.make lhs_nb Int_set.empty in
  let nodes = Array.make lhs_nb [] in
  let check_cyclic_rule = Array.make (Array.length cyclic_rule) [] in
  let f rn =
    let _,_,ind = lhs_table.(rn) in
    let rhs = Array.map
      (fun symb -> match symb with
        | (Ps_Non_ter nt | Ps_Non_ter_NL nt) ->
            let lhs_list =
              comp_lhslist nt lhslists priodata array_nt_prio in
            lhs_list,
            List.exists (fun (_,_,i) -> nullable.(i)) lhs_list
            (* possible optimization: memoize this result *)
        | _ -> assert false)
      gram_rhs.(rn)
    in
    let g i (lhs_list,is_nullable) =
      if array_for_all (fun j x -> i=j || snd x) rhs then
        List.iter (fun (_,_,ind') ->
          if not po_array.(ind).(ind') then
            (po_array.(ind).(ind') <- true;
            nodes.(ind) <- ind'::nodes.(ind);
            result.(ind) <- Int_set.add ind' result.(ind);
            check_cyclic_rule.(rn) <- (i,ind')::check_cyclic_rule.(rn)))
        lhs_list
    in
    Array.iteri g rhs
  in
  Int_set.iter f sub_g;
  closure_of_include nodes result;
  for i=0 to lhs_nb -1 do
    Int_set.iter (fun j -> po_array.(i).(j) <- true) result.(i)
  done;
  for i=0 to (Array.length cyclic_rule) -1 do
    let ind,_,_ = lhs_table.(i) in
    cyclic_rule.(i) <- List.fold_left
      (fun res (j,ind') -> match res with
        | k::_ when k=j -> res
        | _ -> if po_array.(ind).(ind') && po_array.(ind').(ind)
            then j::res else res)
      [] check_cyclic_rule.(i)
  done



(* This function decides if the non terminal nt1 can derive the non terminal
nt2 and stores the results in po_array. If it needed to decide whether another
non terminal nt can derive nt2 then it stores it too in po_array. nt_epsilon
stores the non terminals which can derive epsilon.*)
(*let derive nt1 nt2 gram_rhs sub_gram_lhs po_array nt_epsilon =
  
  let rec aux nt1 nt2 vr =
    match po_array.(nt1).(nt2) with (True | False) as b -> b | NA ->
    let f r _ b =
      if b=True then True else
      let (nt,l,_,_) = r in
      if nt<>nt1 then False else
      if RS.mem r vr then False else
      match l with
        | ([Ps_Non_ter (ntbis,_)] | [Ps_Non_ter_NL (ntbis,_)])
          when ntbis=nt2 -> po_array.(nt1).(nt2) <- True; True
        | [Ps_Non_ter (ntbis,_)] | [Ps_Non_ter_NL (ntbis,_)] ->
            aux ntbis nt2 (RS.add r vr)
        | litl ->
            let rec f2 litl = match litl with
              | (Ps_Non_ter (nt,_))::t
              | (Ps_Non_ter_NL (nt,_))::t -> nt::(f2 t)
              | _ -> []
            in
            let ntl = f2 litl in
            let rec f3 n1 n2 ntl = match ntl with
              | [] -> True
              | nt::t ->
                  let b =
                    if n1=n2 then
                      (let b = aux nt nt2 (RS.add r vr) in
                      po_array.(nt).(nt2) <- b; b)
                    else
                      (let b = derive_epsilon nt RS.empty nt_epsilon user_g in
                      nt_epsilon.(nt) <- b; b)
                  in
                  if b=True then f3 (n1+1) n2 t else False
            in
            let rec f4 n = match n with
              | 0 -> False
              | _ -> let b = f3 1 n ntl in
                  if b=True then True else f4 (n-1)
            in
            f4 (List.length ntl)
    in
    Urule_map.fold f user_g False
  in
  aux nt1 nt2 Int_set.empty in
  po_array.(nt1).(nt2) <- b;
  b*)


let print_r_L r_L =
  let f j = print_int j; print_string " " in
  for i=0 to (Array.length r_L)-1 do
    (print_int i; print_string " -L-> ";
    List.iter f r_L.(i); print_newline ())
  done


let print_rel_L rel_L width n =
  let str_bin bin =
    let rec aux c i =
      if i=31 then c else
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


let compute_L gram_rhs gram_lhs lhslists prio_dat array_nt_prio =
  
  let n = Array.length gram_lhs in
  let nt_graph = Array.make n [] in
  let edge_matrix = Array.make (n*n) false in
    (* better: use int instead of bool *)
  let marked = Array.make n false in
  
  for i=0 to n-1 do
    let l =
      List.fold_left (fun l rn -> match gram_rhs.(rn).(0) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
            let lhs_l = comp_lhslist nt lhslists prio_dat array_nt_prio in
            List.fold_left (fun newl (nt,p,j) ->
              if marked.(j) then newl else
              (marked.(j) <- true;
              j::newl)) l lhs_l
        | _ -> l) [] (fst gram_lhs.(i))
    in
    let l2 = if marked.(i) then l else i::l in
    nt_graph.(i) <- l2;
    List.iter (fun j -> marked.(j) <- false) l
  done;
  
  let r_L = Array.make n [] in
  
  let rec traverse from_l node =
    match r_L.(node) with
    | [] ->
        List.iter (fun i ->
          if edge_matrix.(i+node*n) = false then
            (r_L.(i) <- node::r_L.(i);
            edge_matrix.(i+node*n) <- true))
        from_l;
        if marked.(node) = false then (marked.(node) <- true;
        List.iter (traverse (node::from_l)) nt_graph.(node))
    | l -> List.iter (fun i ->
        let newl = List.fold_left (fun newl j ->
          if edge_matrix.(i+j*n) = false then
            (edge_matrix.(i+j*n) <- true;
            j::newl)
          else newl) r_L.(i) l
        in
        let newl =
          if edge_matrix.(i+node*n) = false then
            (edge_matrix.(i+node*n) <- true; node::newl)
          else newl
        in
        r_L.(i) <- newl)
        from_l
  in
  
  for i=0 to n-1 do
    match r_L.(i) with [] ->
      marked.(i) <- true;
      List.iter (traverse [i]) nt_graph.(i)
    | _ -> ()
  done;
  
  (), r_L


let create_aut gram_rhs gram_lhs gram_parnt bnt_array r_L prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind ist_nt_nb lhslists lhs_table str_non_ter entry_points_list ppar token_nb str_ter regexp_array =
  let () = countst := 0 in
  let is_trace =
    ((Array.make token_nb Map_is.empty),
    (Array.make token_nb Map_is.empty)),
    ((Array.make ist_nt_nb Map_is.empty),
    (Array.make ist_nt_nb Map_is.empty))
  in
  let state_list, n, stations, entry_points, is_trace =
    build_automaton is_trace gram_rhs gram_lhs gram_parnt bnt_array
      !dypgen_verbose
      prio_dat it_nb array_nt_prio nt_of_ind prio_of_ind lhslists
      r_L lhs_table ist_nt_nb token_nb str_non_ter
      str_ter entry_points_list regexp_array
  in
  state_list, n, stations, entry_points, is_trace


let list_of_array a =
  let rec aux i l =
    if i=(-1) then l else
    aux (i-1) (a.(i)::l)
  in
  aux (Array.length a -1) []



let create_po gram_rhs lhs_table lhslists priodata array_nt_prio lhs_nb str_non_ter nt_of_ind prio_of_ind =
  
  let time1 = Sys.time () in
  let po_array =
    Array.init lhs_nb (fun _ -> (Array.make lhs_nb false))
  in
  let nullable = Array.make lhs_nb false in
  let rec rhs_tokless rhs = function 0 -> true | i ->
    match rhs.(i-1) with
    | (Ps_Ter _) | (Ps_Ter_NL _) -> false
    | (Ps_Non_ter _) | (Ps_Non_ter_NL _) -> rhs_tokless rhs (i-1)
  in
  let rnb = Array.length gram_rhs in
  let rec f i sub_g =
    if i=rnb then sub_g else
    let _,_,ind = lhs_table.(i) in
    let sub_g = match gram_rhs.(i) with
      | [||] -> nullable.(ind) <- true; sub_g
      | rhs ->
          if rhs_tokless rhs (Array.length rhs)
          then Int_set.add i sub_g else sub_g
    in
    f (i+1) sub_g
  in
  let sub_g = f 0 Int_set.empty in
  
  let cyclic_rule = Array.make (Array.length gram_rhs) [] in
  
  make_nullable gram_rhs lhs_table lhslists priodata array_nt_prio
    sub_g nullable;
  make_po_array gram_rhs lhs_table lhslists priodata array_nt_prio
    sub_g nullable po_array cyclic_rule;
  
  if !dypgen_verbose>1 then
  for i=0 to lhs_nb -1 do
    if po_array.(i).(i) then
      (Printf.printf
      "Warning: non terminal `(%s,%s)' can derive itself\n"
      str_non_ter.(nt_of_ind.(i))
      priodata.prd_names.(prio_of_ind.(i))
      (*output_string stderr "cyclic grammars are not allowed\n";
      failwith "cyclic grammar"*))
  done;
  
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    (Printf.fprintf !log_channel "po_array built, %.3f sec\n" (time2-.time1);
    flush stdout) else ();
  po_array, cyclic_rule



let print_table_state chan i table table_it table_lit_trans gram_rhs lhs_table prio_of_ind str_non_ter nt_of_ind priority_names str_ter regexp_array =
  let tnb, ntnb = Array.length table.(0).(0), Array.length table.(0).(1) in
  let f tab len name_fun =
    for i=0 to len-1 do
      Printf.printf "(%s,%d) " (name_fun i) tab.(i)
      (*List.iter (fun t -> Printf.printf "(%s,%d) " (name_fun i) t) tab.(i)*)
    done
  in
  Printf.fprintf chan " State %d\n" i;
  Printf.fprintf chan "  li: %s\n  items:\n"
    (str_literal_trans table_lit_trans.(i) str_non_ter str_ter);
  print_item_set chan table_it.(i) gram_rhs lhs_table
    nt_of_ind prio_of_ind str_non_ter str_ter priority_names regexp_array;
  Printf.fprintf chan "  next states (shift):\n   ";
  f table.(i).(0) tnb (fun i -> try str_ter.(i) with _ ->
    (Printf.sprintf "<regexp:%d>" i));
  f table.(i).(2) tnb (fun i -> try "- "^str_ter.(i) with _ ->
    (Printf.sprintf "- <regexp:%d>" i));
  print_newline ();
  Printf.fprintf chan "  next states (reduction):\n   ";
  f table.(i).(1) ntnb
    (fun i -> str_non_ter.(nt_of_ind.(i)));
  print_newline (); print_newline ()

let print_tables table table_it table_lit_trans gram_rhs lhs_table prio_of_ind str_ter str_non_ter nt_of_ind priority_names ppar regexp_array =
  Printf.printf "\nTables\n\n";
  for i=0 to (Array.length table)-1 do
    print_table_state !log_channel i table table_it table_lit_trans gram_rhs
      lhs_table prio_of_ind str_non_ter nt_of_ind priority_names str_ter
      regexp_array
  done



let make_table state_list n nt_nb gram_rhs lhs_table nt_of_ind prio_of_ind array_nt_prio ppar token_nb str_ter str_non_ter priority_names regexp_array =
  let table = Array.init n
    (fun _ -> [|Array.make token_nb (-1);Array.make nt_nb (-1);
      Array.make token_nb (-1);Array.make nt_nb (-1)|])
  in
  (*Printf.printf "E.token_nb=%d, ist_nt_nb=%d\n" E.token_nb ist_nt_nb;*)
  let table_it = Array.make n dummy_item_set in
  let state_is_mergeable = Array.make n true in
  let state_bnt = Array.make n Int_set.empty in
  let table_lit_trans = Array.make n (Ps_Ter 0) in
  (*let table_lex_trans = Array.make (n*token_nb) false in*)
  (*Printf.fprintf !log_channel "token_nb=%d\n" token_nb;*)
  let g num (succ,prio) = match succ.li with
    | Ps_Ter t ->
        (*Printf.printf "Ps_Ter %s, t=%d\n" (str_token_name t) t;*)
        table.(num).(0).(t) <- succ.number(*::table.(num).(0).(t)*)
        (*Printf.fprintf !log_channel
         "table_lex_trans(state=%d,token=%d):true\n" num t;*)
        (*table_lex_trans.(num*token_nb+t) <- true*)
    | Ps_Non_ter nt ->
        (*Printf.printf "Ps_Non_ter %s\n" (str_non_terminal nt);*)
        let ind = Prio_map.find prio array_nt_prio.(nt) in
        (*Printf.printf "num=%d, ind=%d, table length=%d, po_array length=%d\n"
          num ind (Array.length table) (Array.length po_array);
        Printf.printf "table.(num).(1) length=%d\n"
          (Array.length table.(num).(1));
        Printf.printf "po_array.(ind) length=%d\n" (Array.length po_array.(ind));*)
        table.(num).(1).(ind) <- succ.number(*::table.(num).(1).(ind)*)
    | Ps_Ter_NL t ->
        (*Printf.printf "Ps_Ter_NL %s, t=%d\n" (str_token_name t) t;*)
        table.(num).(2).(t) <- succ.number(*::table.(num).(2).(t)*)
    | Ps_Non_ter_NL nt ->
        (*Printf.printf "Ps_Non_ter %s\n" (str_non_terminal nt);*)
        let ind = Prio_map.find prio array_nt_prio.(nt) in
        table.(num).(3).(ind) <- succ.number(*::table.(num).(3).(ind)*)
  in
  let f v =
    (*Printf.printf "process state %d\n" v.number;*)
    List.iter (g v.number) v.succ_states;
    table_it.(v.number) <- v.items;
    table_lit_trans.(v.number) <- v.li;
    state_bnt.(v.number) <- v.bestowing_nt;
    (*Printf.printf "state: %d\n" v.number;
    print_bnt v.bestowing_nt;*)
    state_is_mergeable.(v.number) <- v.mergeable
  in
  
  List.iter f state_list;
  
  (*print_tables table table_it table_lit_trans gram_rhs lhs_table prio_of_ind
  str_ter str_non_ter nt_of_ind priority_names ppar regexp_array;*)
  
  table, table_it, table_lit_trans, state_bnt, state_is_mergeable



let str_rule rn gram_rhs lhs_table str_non_ter priority_names str_ter regexp_array =
    Printf.sprintf "%s -> %s"
      (str_lhs lhs_table.(rn) str_non_ter priority_names)
      (str_handle gram_rhs.(rn) (-1) str_non_ter str_ter regexp_array)

let print_grammar chan gram_rhs lhs_table str_non_ter priority_names str_ter regexp_array =
  for i=0 to (Array.length lhs_table)-1 do
    Printf.fprintf chan "rn:%d  %s\n" i
      (str_rule i gram_rhs lhs_table str_non_ter priority_names str_ter regexp_array)
  done;
  print_newline ()


let print_gram_lhs chan gram_lhs nt_of_ind prio_of_ind str_non_ter priority_names =
  let f i rn =
    Printf.fprintf chan "lhs:%s  rn:%d\n"
    (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priority_names) rn
  in
  for i=0 to (Array.length gram_lhs)-2 do (
    (match snd gram_lhs.(i) with
    | None ->
        Printf.fprintf chan "no epsilon rule for %s\n"
          (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priority_names)
    | Some rn ->
        Printf.fprintf chan "some epsilon rule nb %d for %s\n" rn
          (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priority_names)
    );
    List.iter (f i) (fst gram_lhs.(i)) )
  done


let print_g g nt_of_ind prio_of_ind str_non_ter priority_names str_ter regexp_array =
  let f i (rhs,_) _ =
    Printf.printf "%s -> %s\n"
      (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priority_names)
      (str_handle rhs 0 str_non_ter str_ter regexp_array)
  in
  for i=0 to (Array.length g)-1 do
    Map_rhs.iter (f i) g.(i)
  done

let print_lhs_of_ind nt_of_ind prio_of_ind str_non_ter priority_names =
  for i=0 to (Array.length nt_of_ind)-1 do
    Printf.printf "ind %d = lhs %s\n" i
      (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priority_names)
  done




let update_user_g ral user_g =
  let f (user_g, i) (r, a, inh_l) =
    let (al, j, inh_ll), i =
      try Urule_map.find r user_g, i
      with Not_found ->
        ([], i+1, List.map (fun (p,_) -> p, []) inh_l), i+1
    in
    let inh_ll = List.map2 (fun (p,a) (_,l) -> p, a::l) inh_l inh_ll in
    (Urule_map.add r (a::al, j, inh_ll) user_g), i
  in
  fst (List.fold_left f (user_g, -1) (List.rev ral))




let make_grams_actions g nbr array_nt_prio nt_of_ind prio_of_ind str_non_ter =
  (*let gl_len = (Array.length g)+1 in*)(*Printf.fprintf !log_channel "-state built-\n";
    print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      str_token_name prio_dat.prd_names;
    Printf.fprintf !log_channel "\n";
    flush_all ();*)
  let gram_lhs = Array.make (Array.length g) ([],None) in
  (* The last cell of the array will be kept ([],None) and reserved
  for the non terminals appearing in the rhs of the "seed" items,
  i.e. kernel items of the initial state, when the automaton is
  generated for a modification of the grammar. *)
  
  let gram_rhs = Array.make nbr [||] in
  let gram_parnt = Array.make nbr [] in
  let bnt_array = Array.make (Array.length array_nt_prio) false in
  let rule_options = Array.make nbr 3 in
  let lhs_table = Array.make nbr dummy_lhs in
  let new_actions = Array.make nbr [] in
  
  (*let rn = ref 0 in*)
  
  let inh_list = ref [] in
  let inh = ref 0 in
  
  let f i (rhs, ro) (ac, rn, inh_l) (inh, inh_list) =
    gram_rhs.(rn) <- rhs;
    rule_options.(rn) <- ro;
    new_actions.(rn) <- ac;
    lhs_table.(rn) <- (nt_of_ind.(i), prio_of_ind.(i),i);
    let rnl, iop = gram_lhs.(i) in
    let de =
      if Array.length rhs > 0 then
        (gram_lhs.(i) <- (rn::rnl, iop);
        match rhs.(0) with
        | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_)
          when str_non_ter.(nt).[0] = '0' -> true
        | _ -> false)
      else
       (gram_lhs.(i) <- (rnl, Some rn); false)
    in
    let parnt, inh, inh_list = List.fold_left
      (fun (parnt, inh, inh_list) (place, inh_val) ->
        let nt = match rhs.(place-1) with
          | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_) -> nt
          | _ -> assert false
        in
        bnt_array.(nt) <- true;
        (place, inh)::parnt, inh+1,
        (inh_val, place-1, nt_of_ind.(i), nt, de)::inh_list)
      ([], inh, inh_list) inh_l
    in
    gram_parnt.(rn) <- List.rev parnt;
    inh, inh_list
  in
  for i = 0 to Array.length g - 1 do
    let a, b = Map_rhs.fold (f i) g.(i) (!inh, !inh_list) in
    inh := a; inh_list := b
  done;
  
  let inherited = Array.make !inh ([],-1,-1,-1,false) in
  let _ = List.fold_left
    (fun i (inh_val, nbarg, lhs, nt, de) ->
      inherited.(i) <- inh_val, nbarg, lhs, nt, de; i+1)
    0 (List.rev !inh_list)
  in
  
  (*print_grammar gram_rhs lhs_table;
  print_g g lhs_of_ind;
  print_lhs_of_ind lhs_of_ind;
  print_gram_lhs gram_lhs lhs_of_ind;*)
  
  gram_lhs, gram_rhs, gram_parnt, bnt_array, lhs_table, new_actions,
  inherited, rule_options


module String_set = Set.Make(Ordered_string)



module Ordered_rule =
struct
  type t = (non_ter * priority) * rhs * int
   (* the int tells whether the rule allows layout characters
    inside or afterwards *)
  let compare = Pervasives.compare
end
module Rule_map = Map.Make(Ordered_rule)



type extra_station = {
  es_number : int;
  es_component : int }

type old_station = No_station | Some_station of state | Extra_station of extra_station



let print_stations stations gram_rhs lhs_table str_non_ter nt_of_ind prio_of_ind priodata str_ter regexp_array =
  Printf.fprintf !log_channel "\nstations: (%d)\n" (Array.length stations -1);
  for i=0 to Array.length stations -1 do
    Printf.fprintf !log_channel "\nstation: (%s,%s,%d) = (%d,%d)\n"
      str_non_ter.(nt_of_ind.(i)) priodata.prd_names.(prio_of_ind.(i))
      i nt_of_ind.(i) prio_of_ind.(i);
    match stations.(i) with None -> assert false | Some s ->
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind
      str_non_ter str_ter priodata.prd_names regexp_array
  done;
  flush_all ()








let compose_grammar pdev_list =
  
  let token_nb = (List.hd (List.tl pdev_list)).token_nb in
  
  let pnb = List.length pdev_list in
  let f2 n str ind sm =
    let l =
      try String_map.find str sm
      with Not_found -> []
    in
    String_map.add str ((n,ind)::l) sm
  in
  let f1 (nsm,n) pdev =
    (*Printf.fprintf !log_channel "\ncompose_grammar state_list %d:\n" n;
    List.iter
    (fun s ->
      Printf.fprintf !log_channel "\n";
      print_state s pdev.gram_rhs pdev.lhs_table pdev.nt_of_ind
      pdev.prio_of_ind pdev.str_non_ter E.str_token_name pdev.prio.prd_names)
    pdev.state_list;*)
    (Hashtbl.fold (f2 n) pdev.nt_table nsm), (n+1)
  in
  let nt_subst_map,_ = List.fold_left f1 (String_map.empty,0) pdev_list in
  let f3 _ _ n = n+1 in
  let new_nt_nb = String_map.fold f3 nt_subst_map 0 +1 in
  let nt_table = Hashtbl.create new_nt_nb in
  let nt_subst = Array.make pnb [||] in
  let f4 i pdev =
    nt_subst.(i) <- Array.make (Array.length pdev.str_non_ter) 0; i+1
  in
  let _ = List.fold_left f4 0 pdev_list in
  let f6 subst i (ind_pdev,ind) = subst.(ind_pdev).(ind) <- i in
  let f5 table subst str l i =
    Hashtbl.add table str i;
    List.iter (f6 subst i) l;
    i+1
  in
  let _ = String_map.fold (f5 nt_table nt_subst) nt_subst_map 1 in
  
  let f7 (psm,n) pdev =
    (Hashtbl.fold (f2 n) pdev.prio.prd_ind psm), (n+1)
  in
  let prio_subst_map,_ = List.fold_left f7 (String_map.empty,0) pdev_list in
  (*String_map.iter
    (fun str l -> Printf.printf "%s -> " str;
      List.iter (fun (a,b) -> Printf.printf "(%d,%d)" a b) l;
      print_newline ())
    prio_subst_map;*)
  let new_prio_nb = String_map.fold f3 prio_subst_map 0 in
  (*Printf.printf "new_prio_nb = %d\n" new_prio_nb;*)
  let prio_table = Hashtbl.create new_prio_nb in
  let prio_subst = Array.make pnb [||] in
  let f8 i pdev =
    prio_subst.(i) <- Array.make (Array.length pdev.prio.prd_names) 0; i+1
  in
  let _ = List.fold_left f8 0 pdev_list in
  let _ = String_map.fold (f5 prio_table prio_subst) prio_subst_map 0 in
  
  (*Hashtbl.iter (fun p_str p_int -> Printf.printf "p_str=%s, p_int=%d\n" p_str p_int) prio_table;
  print_endline "prio_subst";
  for n=0 to pnb-1 do
    Printf.printf ":%d:\n" n;
    for i=0 to Array.length prio_subst.(n) -1 do
      Printf.printf "(%d->%d) " i prio_subst.(n).(i)
    done;
    print_newline ()
  done;*)
  
  let subst_prio n p = match p with
    | No_priority -> No_priority
    | Eq_priority p -> Eq_priority prio_subst.(n).(p)
    | Less_priority p -> Less_priority prio_subst.(n).(p)
    | Lesseq_priority p -> Lesseq_priority prio_subst.(n).(p)
    | Greater_priority p -> Greater_priority prio_subst.(n).(p)
    | Greatereq_priority p -> Greatereq_priority prio_subst.(n).(p)
  in
  let subst_symb n old_symb = match old_symb with
    | Ps_Ter t -> Ps_Ter t
    | Ps_Non_ter (nt,p) -> Ps_Non_ter (nt_subst.(n).(nt), subst_prio n p)
    | Ps_Ter_NL t -> Ps_Ter_NL t
    | Ps_Non_ter_NL (nt,p) ->
        Ps_Non_ter_NL (nt_subst.(n).(nt), subst_prio n p)
  in
  let subst_rule n (nt,p,_) old_rhs b =
    (*Printf.fprintf !log_channel "subst_rule pdev=%d, nt=%d, p=%d\n" n nt p;*)
    let lhs = nt_subst.(n).(nt), prio_subst.(n).(p) in
    let len = Array.length old_rhs in
    let rhs = Array.make len (Ps_Ter 0) in
    for i=0 to (len-1) do
      rhs.(i) <- subst_symb n old_rhs.(i)
    done;
    (lhs,rhs,b)
  in
  (*let count_inh_val
  let subst_inh n l = List.map (fun (i,l) -> i, inh_subst.(n).(id)) l in*)
  let f10 n inh inh_subst lhs_table gram_rhs rule_options gram_parnt rsm i =
    let rec aux rsm inh i =
      if i= -1 then rsm, inh else
      let rule =
        subst_rule n lhs_table.(i) gram_rhs.(i) rule_options.(i) in
      let (l, parnt), inh =
        try Rule_map.find rule rsm, inh
        with Not_found ->
          let parnt, inh = List.fold_left
            (fun (parnt, inh) (place, _) ->
              (place, inh)::parnt, inh+1)
            ([], inh) gram_parnt.(i)
          in
          ([], List.rev parnt), inh
      in
      List.iter2 (fun (_,id) (_,old_id) -> inh_subst.(old_id) <- id)
        parnt gram_parnt.(i);
      aux (Rule_map.add rule (((n,i)::l), parnt) rsm) inh (i-1)
    in aux rsm inh i
  in
  
  let inherit_subst = Array.make pnb [||] in
  
  let f9 (rsm,n,inh) pdev =
    (*print_grammar stdout pdev.gram_rhs pdev.lhs_table pdev.str_non_ter
     pdev.prio.prd_names;*)
    let inh_subst = Array.make (Array.length pdev.inherited) (-1) in
    inherit_subst.(n) <- inh_subst;
    let rsm, inh =
      f10 n inh inh_subst pdev.lhs_table pdev.gram_rhs pdev.rule_options
      pdev.gram_parnt rsm (Array.length pdev.lhs_table - 1) in
    rsm, n+1, inh
  in
  let rule_subst_map, _, inh_nb =
    List.fold_left f9 (Rule_map.empty,0,0) pdev_list in
  
  let inherited = Array.make inh_nb ([],-1,-1,-1,false) in
  let _ = List.fold_left
    (fun n pdev ->
      for i=0 to Array.length pdev.inherited - 1 do
        let j = inherit_subst.(n).(i) in
        let lj, _, _, _, _ = inherited.(j) in
        let li, argnb, lhs, nt, ed = pdev.inherited.(i) in
        inherited.(j) <- li@lj, argnb, nt_subst.(n).(lhs), nt_subst.(n).(nt), ed
      done; n+1)
    0 pdev_list in
  
  let rec all_prio nt lhs_set n =
    if n= -1 then lhs_set else
    all_prio nt (Intc_set.add (nt,n) lhs_set) (n-1)
  in
  
  (*Printf.printf "new_prio_nb=%d\n" new_prio_nb;*)
  let prd_rel = Array.make new_prio_nb (Prio_set.empty,Prio_set.empty) in
  let f20 n p s = Prio_set.add prio_subst.(n).(p) s in
  let f19 n pdev =
    (*Printf.printf "Array.length pdev.prio.prd_rel=%d\n" (Array.length pdev.prio.prd_rel);*)
    for i=0 to Array.length pdev.prio.prd_rel -1 do
      let sn1,sn2 = pdev.prio.prd_rel.(i) in
      let s1,s2 = prd_rel.(prio_subst.(n).(i)) in
      let s1 = Prio_set.fold (f20 n) sn1 s1 in
      let s2 = Prio_set.fold (f20 n) sn2 s2 in
      (*Printf.printf "i=%d\n" i;*)
      prd_rel.(prio_subst.(n).(i)) <- (s1,s2)
    done;
    n+1
  in
  let _ = List.fold_left f19 0 pdev_list in
  let prd_nb = Hashtbl.length prio_table in
  let prd_names = Array.make prd_nb "" in
  let f21 str_prio int_prio = prd_names.(int_prio) <- str_prio in
  Hashtbl.iter f21 prio_table;
  let priodata = {
    prd_rel = prd_rel;
    prd_ind = prio_table;
    prd_names = prd_names;
    prd_nb = prd_nb }
  in
  
  let f11 (lhs,rhs,_) _ lhs_set =
    let rec aux i lhs_set =
      if i= -1 then lhs_set else
      let lhs_set =
      match rhs.(i) with
      | Ps_Non_ter (nt,ntp) | Ps_Non_ter_NL (nt,ntp) -> (match ntp with
        | No_priority ->
            all_prio nt lhs_set (new_prio_nb-1)
        | Eq_priority p ->
            Intc_set.add (nt,p) lhs_set
        | Less_priority p ->
            let ps = fst priodata.prd_rel.(p) in
            Prio_set.fold (fun p lhs_set ->
              Intc_set.add (nt,p) lhs_set)
              ps lhs_set
        | Lesseq_priority p ->
            let ps = Prio_set.add p (fst priodata.prd_rel.(p)) in
            Prio_set.fold (fun p lhs_set ->
              Intc_set.add (nt,p) lhs_set)
              ps lhs_set
        | Greater_priority p ->
            let ps = snd priodata.prd_rel.(p) in
            Prio_set.fold (fun p lhs_set ->
              Intc_set.add (nt,p) lhs_set)
              ps lhs_set
        | Greatereq_priority p ->
            let ps = Prio_set.add p (snd priodata.prd_rel.(p)) in
            Prio_set.fold (fun p lhs_set ->
              Intc_set.add (nt,p) lhs_set)
              ps lhs_set)
      | _ -> lhs_set
      in
      aux (i-1) lhs_set
    in
    let lhs_set = aux (Array.length rhs -1) lhs_set in
    Intc_set.add lhs lhs_set
  in
  (* lhs_set actually includes more than just the lhss, it also includes
    the couples (nt,prio) that would be accepted in some rhs but are not
    in a lhs. *)
  let lhs_set = Rule_map.fold f11 rule_subst_map Intc_set.empty in
  (*Printf.printf "new_nt_nb = %d\n" new_nt_nb;*)
  let array_nt_prio = Array.make new_nt_nb Prio_map.empty in
  let lhs_nb = Intc_set.cardinal lhs_set in
  let nt_of_ind = Array.make lhs_nb 0 in
  let prio_of_ind = Array.make lhs_nb 0 in
  let f12 (nt,p) n =
    (*Printf.printf "n = %d\n" n;*)
    array_nt_prio.(nt) <- Prio_map.add p n array_nt_prio.(nt);
    nt_of_ind.(n) <- nt;
    prio_of_ind.(n) <- p;
    n+1
  in
  let _ = Intc_set.fold f12 lhs_set 0 in
  let new_rule_nb = Rule_map.fold f3 rule_subst_map 0 in
  
  (*Printf.fprintf !log_channel "new_rule_nb = %d\n" new_rule_nb;*)
  let gram_rhs = Array.make new_rule_nb [||] in
  let gram_parnt = Array.make new_rule_nb [] in
  let bnt_array = Array.make new_nt_nb false in
  
  let _ = List.fold_left (fun n pdev ->
    for i=0 to Array.length pdev.bnt_array - 1 do
      bnt_array.(nt_subst.(n).(i)) <- pdev.bnt_array.(i)
    done; n+1) 0 pdev_list in
  
  let rule_options = Array.make new_rule_nb 3 in
  let lhs_table = Array.make new_rule_nb (0,0,0) in
  let rule_subst = Array.make pnb [||] in
  let f14 i pdev =
    rule_subst.(i) <- Array.make (Array.length pdev.gram_rhs) 0; i+1
  in
  let _ = List.fold_left f14 0 pdev_list in
  (*Printf.fprintf !log_channel "lhs_table length=%d\n" (Array.length lhs_table);
  Printf.fprintf !log_channel "array_nt_prio length=%d\n" (Array.length array_nt_prio);
  Printf.fprintf !log_channel "gram_rhs length=%d\n" (Array.length gram_rhs);*)
  let f13 ((nt,p),rhs,ro) (l,parnt) n =
    (*Printf.fprintf !log_channel "n=%d, nt=%d\n" n nt;*)
    gram_rhs.(n) <- rhs;
    gram_parnt.(n) <- parnt;
    rule_options.(n) <- ro;
    lhs_table.(n) <- (nt,p,Prio_map.find p array_nt_prio.(nt));
    List.iter (f6 rule_subst n) l;
    n+1
  in
  let _ = Rule_map.fold f13 rule_subst_map 0 in
  let gram_lhs = Array.make lhs_nb ([],None) in
  for i=0 to new_rule_nb-1 do
    let (_,_,ind) = lhs_table.(i) in
    let (l,o) = gram_lhs.(ind) in
    gram_lhs.(ind) <- match gram_rhs.(i) with
      | [||] ->  (l, Some i)
      | _ -> (i::l, o)
  done;
  let actions = Array.make new_rule_nb [] in
  let f24 n pdev =
    (*Printf.printf "pdev.actions length = %d\n" (Array.length pdev.actions);*)
    for i=0 to Array.length pdev.actions-1 do
      let ind = rule_subst.(n).(i) in
      (*Printf.printf "ind = %d\n" ind;
      Printf.printf "i = %d\n" i;*)
      actions.(ind) <- (pdev.actions.(i))@(actions.(ind))
    done;
    n+1
  in
  let _ = List.fold_left f24 0 pdev_list in
  
  let old_stations = Array.make (pnb * lhs_nb) No_station in
  let max_state_nb =
    List.fold_left
    (fun msn pdev -> max msn (Array.length pdev.table_it))
    0 pdev_list
  in
  let state_array = Array.make max_state_nb None in
  
  let lhs_station = Hashtbl.create (pnb * lhs_nb) in
  (*Printf.printf "pnb=%d, E.token_nb=%d\n" pnb E.token_nb;*)
  let is_trace = Array.init pnb (fun _ ->
    ((Array.make token_nb Map_is.empty),
    (Array.make token_nb Map_is.empty)),
    ((Array.make new_nt_nb Map_is.empty),
    (Array.make new_nt_nb Map_is.empty)))
  in
  
  let ind_subst = Array.make pnb [||] in
  let f17 n pdev =
    ind_subst.(n) <- Array.make (Array.length pdev.gram_lhs) 0;
    n+1
  in
  let _ = List.fold_left f17 0 pdev_list in
  let f25 i pdev =
    for n=0 to Array.length pdev.stations -1 do
      let nt, p =
        nt_subst.(i).(pdev.nt_of_ind.(n)),
        prio_subst.(i).(pdev.prio_of_ind.(n))
      in
      (*Printf.fprintf !log_channel "i=%d, n=%d, nt=%d, p=%d, old nt=%d, old p=%d\n"
        i n nt p pdev.nt_of_ind.(n) pdev.prio_of_ind.(n);*)
      let ind = Prio_map.find p array_nt_prio.(nt) in
      ind_subst.(i).(n) <- ind
    done;
    i+1
  in
  let _ = List.fold_left f25 0 pdev_list in
  
  let str_non_ter = Array.make new_nt_nb "" in
  let f18 str_nt ind_nt = str_non_ter.(ind_nt) <- str_nt in
  Hashtbl.iter f18 nt_table;
  
  let non_kernel_array = Array.make new_rule_nb false in
  
  let subst_rn n is =
    let aux (rn,x) set = Intc_set.add (rule_subst.(n).(rn),x) set in
    Intc_set.fold aux is Intc_set.empty
  in
  let subst_rn_nk n is =
  
    let aux nk rn =
      if non_kernel_array.(rule_subst.(n).(rn)) then nk
      else (non_kernel_array.(rule_subst.(n).(rn)) <- true;
        rule_subst.(n).(rn)::nk)
    in
    let nk = List.fold_left aux [] is in
    List.iter (fun rn -> non_kernel_array.(rn) <- false) nk;
    nk
  in
  let subst_rn_red n is =
    let aux rn set = Int_set.add rule_subst.(n).(rn) set in
    Int_set.fold aux is Int_set.empty
  in
  let subst_predict n predict =
    let aux (nt,p) set = Predict.add (nt_subst.(n).(nt),subst_prio n p) set in
    Predict.fold aux predict Predict.empty
  in
  let subst_is n is = {
    reducible = subst_rn_red n is.reducible;
    kernel_nt = subst_rn n is.kernel_nt;
    kernel_t = subst_rn n is.kernel_t;
    non_kernel = subst_rn_nk n is.non_kernel;
    predict = subst_predict n is.predict }
  in
  
  let lhslists = Array.make new_nt_nb Ntp_map.empty in
  
  let state_lists = Array.make pnb [] in
  
  let lhs_nb_array = Array.make pnb 0 in
  let _ = List.fold_left
    (fun i pdev ->
      lhs_nb_array.(i) <- Array.length pdev.stations; (i+1))
    0 pdev_list
  in
  
  let make_key is =
    let is = {
      reducible = Int_set.filter (fun rn -> Array.length gram_rhs.(rn)>0)
        is.reducible;
      kernel_nt = is.kernel_nt;
      kernel_t = is.kernel_t;
      non_kernel = [];
      predict = Predict.empty }
    in
    is,
    (Int_set.cardinal is.reducible) +
    (Intc_set.cardinal is.kernel_t) +
    (Intc_set.cardinal is.kernel_nt)
  in
  
  let copy_state i state st_nb_offset =
    match state_array.(state.number) with
    | Some s -> s, false
    | None ->
        let s = {
          number = state.number+st_nb_offset;
          li = (match state.li with
            | Ps_Ter t -> Ps_Ter t
            | Ps_Non_ter nt -> Ps_Non_ter (nt_subst.(i).(nt))
            | Ps_Ter_NL t -> Ps_Ter_NL t
            | Ps_Non_ter_NL nt -> Ps_Non_ter_NL (nt_subst.(i).(nt)));
          items = subst_is i state.items;
          mergeable = state.mergeable;
          bestowing_nt = Int_set.fold
            (fun n s -> Int_set.add inherit_subst.(i).(n) s)
            state.bestowing_nt Int_set.empty;
          succ_states = [] }
        in
        state_lists.(i) <- s::state_lists.(i);
        let key = make_key s.items in
        (match s.li with
            | Ps_Ter t ->
              (*Printf.printf "i=%d, t=%d\n" i t;
              Printf.printf "is_trace len=%d\n" (Array.length is_trace);*)
              if t<>(-1) then
              (*let _ = Printf.printf "(fst is_trace.(i)) len=%d\n"
                (Array.length (fst is_trace.(i))) in*)
              (fst (fst is_trace.(i))).(t) <-
                Map_is.add key s (fst (fst is_trace.(i))).(t)
            | Ps_Non_ter nt -> (fst (snd is_trace.(i))).(nt) <-
                Map_is.add key s (fst (snd is_trace.(i))).(nt)
            | Ps_Ter_NL t -> if t<>(-1) then
                (snd (fst is_trace.(i))).(t) <-
                Map_is.add key s (snd (fst is_trace.(i))).(t)
            | Ps_Non_ter_NL nt -> (snd (snd is_trace.(i))).(nt) <-
                Map_is.add key s (snd (snd is_trace.(i))).(nt));
        (*Printf.fprintf !log_channel "copy state %d\n" s.number;*)
        state_array.(state.number) <- Some s;
        if state.number<lhs_nb_array.(i) then
          Hashtbl.add lhs_station s.number ind_subst.(i).(state.number);
        s, true
  in
  
  let rec copy_succ_states i prev_s st_nb_offset prev_succ_states (state,p) =
    let s, b = copy_state i state st_nb_offset in
    if b then
      ((*Printf.fprintf !log_channel "-state copied-\n";
      print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        str_token_name priodata.prd_names;
      Printf.fprintf !log_channel "\n";*)
      let succ_states =
        List.fold_left (copy_succ_states i s st_nb_offset)
          [] state.succ_states
      in
      s.succ_states <- succ_states);
    match s.li with
      | Ps_Non_ter _ | Ps_Non_ter_NL _ ->
          (s,prio_subst.(i).(p))::prev_succ_states
      | Ps_Ter _ | Ps_Ter_NL _ -> (s,0)::prev_succ_states
  in
  
  let _, st_nb, fst_of_comp_l = List.fold_left
    (fun (i, st_nb_offset, fst_of_comp_l) pdev ->
      for n=0 to Array.length pdev.stations -1 do
        match pdev.stations.(n) with None -> () | Some state ->
        let s, b = copy_state i state st_nb_offset in
        (*Printf.fprintf !log_channel "old state:\n";
        print_state state pdev.gram_rhs pdev.lhs_table pdev.nt_of_ind
          pdev.prio_of_ind pdev.str_non_ter E.str_token_name pdev.prio.prd_names;
        Printf.fprintf !log_channel "new state:\n";
        print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
          E.str_token_name priodata.prd_names;*)
        old_stations.(i*lhs_nb+ind_subst.(i).(n)) <- Some_station s;
        if b then
          (let succ_states =
            List.fold_left (copy_succ_states i s st_nb_offset)
              [] state.succ_states
          in
          s.succ_states <- succ_states)
      done;
      if i<pnb-1 then
        for j=0 to Array.length pdev.table_it -1 do
          state_array.(j) <- None
        done;
      i+1, st_nb_offset+Array.length pdev.table_it,
      st_nb_offset::fst_of_comp_l)
    (0, 0, []) pdev_list
  in
  
  (*Printf.fprintf !log_channel "old_stations:\n";
  for i=0 to pnb*lhs_nb-1 do
    (*Printf.fprintf !log_channel ""*)
     Printf.fprintf !log_channel "old_stations= number: %d\n" i;
    match old_stations.(i) with Some_station s ->
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind
      str_non_ter str_token_name priodata.prd_names
    | _-> ()
  done;
  
  Printf.fprintf !log_channel "predict:\n";
  for i=0 to state_nb-1 do
    Printf.fprintf !log_channel "state %i: \n" i;
    Int_set.iter
    (fun j ->
      Printf.fprintf !log_channel "(%s:%d,%s:%d,%d)" str_non_ter.(nt_of_ind.(j))
      nt_of_ind.(j) priodata.prd_names.(prio_of_ind.(j)) prio_of_ind.(j) j)
    predict.(i);
    Printf.fprintf !log_channel "\n"
  done;*)
  
  let cons_of_nt = Array.make new_nt_nb 0 in
  let f22 n pdev =
    for i=0 to Array.length pdev.cons_of_nt-1 do
      cons_of_nt.(nt_subst.(n).(i)) <- pdev.cons_of_nt.(i)
    done;
    n+1
  in
  let _ = List.fold_left f22 0 pdev_list in
  
  let po_array, cyclic_rules = create_po gram_rhs lhs_table lhslists priodata
    array_nt_prio (Array.length gram_lhs) str_non_ter nt_of_ind prio_of_ind in
  
  (*print_grammar stdout gram_rhs lhs_table str_non_ter priodata.prd_names;*)
  
  (*for i=0 to Array.length nt_of_ind -1 do
    Printf.fprintf !log_channel "%s\n"
      (str_lhs (nt_of_ind.(i),prio_of_ind.(i),i) str_non_ter priodata.prd_names)
  done;*)
  
  nt_table, nt_of_ind, prio_of_ind, array_nt_prio, gram_rhs, gram_parnt,
  bnt_array, lhs_table,
  gram_lhs, actions, inherited, old_stations, str_non_ter, priodata,
  cons_of_nt, po_array, new_nt_nb, lhs_nb, pnb, rule_subst, nt_subst,
  lhslists, lhs_station, is_trace, state_lists, st_nb, ind_subst,
  non_kernel_array, rule_options, cyclic_rules




let nt_prio ntp prio_names = match ntp with
  | No_priority -> No_priority
  | Eq_priority p -> Eq_priority prio_names.(p)
  | Less_priority p -> Less_priority prio_names.(p)
  | Lesseq_priority p -> Lesseq_priority prio_names.(p)
  | Greater_priority p -> Greater_priority prio_names.(p)
  | Greatereq_priority p -> Greatereq_priority prio_names.(p)


let lit_of_nlit str_non_ter prio_names str_ter (regexp_array:regexp array) symb =
  match symb with
  | Ps_Ter t ->
      (try Ter (str_ter.(t))
      with _ -> Regexp (regexp_array.(t-(Array.length str_ter))))
  | Ps_Non_ter (nt,p) -> Non_ter (str_non_ter.(nt),nt_prio p prio_names)
  | Ps_Ter_NL t ->
      (try Ter_NL (str_ter.(t))
      with _ -> Regexp_NL (regexp_array.(t-(Array.length str_ter))))
  | Ps_Non_ter_NL (nt,p) -> Non_ter_NL (str_non_ter.(nt),nt_prio p prio_names)

let nl_inside = [No_layout_inside]
let nl_follows = [No_layout_follows]
let nl_inside_nl_follows = [No_layout_inside;No_layout_follows]

let make_rn_of_rule (gram_rhs:rhs array) rule_options lhs_table
str_non_ter prio_names str_ter (regexp_array:regexp array) =
  let n = Array.length gram_rhs in
  let rn_of_rule = Hashtbl.create n in
  for i=0 to n-1 do
    let nt,p,_ = lhs_table.(i) in
    let litl = List.map
      (lit_of_nlit str_non_ter prio_names str_ter regexp_array)
      (list_of_array gram_rhs.(i)) in
    let rol = match rule_options.(i) with
      | 3 -> []
      | 2 -> nl_inside
      | 1 -> nl_follows
      | 0 -> nl_inside_nl_follows
      | _ -> assert false
    in
    Hashtbl.add rn_of_rule
      (str_non_ter.(nt),litl,prio_names.(p),rol) i
  done;
  rn_of_rule




let make_grammar ral (relations:string list list) ppar ter_table str_ter regexp_fun regexp_table =
  
  let token_nb = Hashtbl.length ter_table in
  let fold_rhs (nts, ps) = function
    | (Non_ter (nt, p)) | (Non_ter_NL (nt, p)) ->
        let nts = String_set.add nt nts in
        (match p with
          | No_priority -> nts, ps
          | Eq_priority p
          | Less_priority p
          | Lesseq_priority p
          | Greater_priority p
          | Greatereq_priority p -> nts, String_set.add p ps)
    | Ter _ | Ter_NL _ | Regexp _ | Regexp_NL _ -> nts, ps
  in
  let nt_set, prio_set =
    List.fold_left
      (fun (nts, ps) ((lhs, rhs, p, _), _, _) ->
        let nts = String_set.add lhs nts in
        let ps = String_set.add p ps in
        List.fold_left fold_rhs (nts, ps) rhs)
      (String_set.empty, String_set.empty) ral
  in
  let prio_set = List.fold_left
    (fun prio_set l -> List.fold_left
      (fun prio_set p -> String_set.add p prio_set) prio_set l)
    prio_set relations
  in
  let nt_table = Hashtbl.create (String_set.cardinal nt_set) in
  let pr_nb = String_set.cardinal prio_set (*+ 1*) in
  let prio_table = Hashtbl.create pr_nb in
  let fold_ht ht str n = Hashtbl.add ht str n; (n+1) in
  let _ = String_set.fold (fold_ht nt_table) nt_set 0 in
  let _ = String_set.fold (fold_ht prio_table) prio_set 0 in
  (*let regexp_table = Hashtbl.create 10 in*)
  let re_id = ref (token_nb + (Hashtbl.length regexp_table)) in
  let regexp_list = (ref []),(ref []) in
  let regexp_actions = (ref []),(ref []) in
  let make_regexp re =
    if not ppar.use_dyplex then
      failwith "make_grammar: Regular expression not allowed in a rule when using an external lexer generator.";
    try Hashtbl.find regexp_table re
    with Not_found ->
      (Hashtbl.add regexp_table re !re_id;
      (match re with
        | RE_String _ ->
          (fst regexp_list) := re::(!(fst regexp_list));
          (fst regexp_actions) := (!re_id,regexp_fun)::
            (!(fst regexp_actions))
        | _ ->
          (snd regexp_list) := re::(!(snd regexp_list));
          (snd regexp_actions) :=
            (!re_id,regexp_fun)::(!(snd regexp_actions)) );
      incr re_id;
      !re_id-1)
  in
  let make_non_ter nt p =
    let p = match p with
      | No_priority -> No_priority
      | Eq_priority p -> Eq_priority (Hashtbl.find prio_table p)
      | Less_priority p -> Less_priority (Hashtbl.find prio_table p)
      | Lesseq_priority p -> Lesseq_priority (Hashtbl.find prio_table p)
      | Greater_priority p -> Greater_priority (Hashtbl.find prio_table p)
      | Greatereq_priority p -> Greatereq_priority
          (Hashtbl.find prio_table p)
    in
    try (Hashtbl.find nt_table nt, p)
    with Not_found ->
      failwith ("make_non_ter did not find non terminal \""^nt^"\"")
  in
  let f4 l = match l with
    | Ter t -> Ps_Ter (try Hashtbl.find ter_table t
        with Not_found -> raise (Undefined_ter t)
        (*failwith
        "make_grammar: terminal not found in table"*))
    | Regexp re -> Ps_Ter (make_regexp re)
    | Non_ter (nt,p) -> Ps_Non_ter (make_non_ter nt p)
    | Ter_NL t -> Ps_Ter_NL (try Hashtbl.find ter_table t
        with Not_found -> raise (Undefined_ter t))
    | Regexp_NL re ->(* print_endline "Regexp_NL";*) Ps_Ter_NL (make_regexp re)
    | Non_ter_NL (nt,p) -> Ps_Non_ter_NL (make_non_ter nt p)
  in
  let f3 (((lhs:string),rhs,(p:string),rol),a,inhl) =
    let lhs = Hashtbl.find nt_table lhs in
    let p = Hashtbl.find prio_table p in
    let rhs = List.map f4 rhs in
    let x = List.fold_left (fun x ro ->
     match ro with
      | No_layout_inside -> (lnot 1) land x
      | No_layout_follows -> (lnot 2) land x) 3 rol
    in
    ((lhs,rhs,p,x),a,inhl)
  in
  let ral = List.map f3 ral in
  let user_g = update_user_g ral Urule_map.empty in
  
  (*Printf.printf "regexp_table len = %d, token_nb = %d\n"
    (Hashtbl.length regexp_table) token_nb;
  Hashtbl.iter (fun re i -> Printf.printf "i=%d\n" i) regexp_table;*)
  
  let regexp_array = Array.make
    (Hashtbl.length regexp_table)
    RE_Eof_char in
  Hashtbl.iter (fun re i -> regexp_array.(i-token_nb) <- re) regexp_table;
  
  let str_non_ter = Array.make (Hashtbl.length nt_table + 1) "" in
  let f5 str n = str_non_ter.(n) <- str in
  Hashtbl.iter f5 nt_table;
  
  let prio_dat = {
    prd_rel = Array.make pr_nb (Prio_set.empty,Prio_set.empty);
    prd_ind = prio_table;
    prd_names = Array.make pr_nb "";
    prd_nb = pr_nb }
  in
  
  let f9 s = Hashtbl.find prio_table s in
  let f8 l = List.map f9 l in
  let relations = List.map f8 relations in
  let f7 l = add_list_relations prio_dat l in
  List.iter f7 relations;
  
  let f10 s i = prio_dat.prd_names.(i) <- s in
  Hashtbl.iter f10 prio_table;
  
  let g, array_nt_prio, nt_nb, nt_of_ind, prio_of_ind, nbr =
    make_real_grammar user_g prio_dat str_non_ter nt_table ppar
  in
  let gram_lhs, gram_rhs, gram_parnt, bnt_array, lhs_table, actions,
  inherited, rule_options =
    make_grams_actions g nbr array_nt_prio nt_of_ind prio_of_ind str_non_ter
    (*try Hashtbl.find nt_table "dypgen__epsilon" with Not_found -> -1*)
  in
  let rn_of_rule =
    make_rn_of_rule gram_rhs rule_options lhs_table str_non_ter
    prio_dat.prd_names str_ter regexp_array in
  
  let lhslists = Array.make nt_nb Ntp_map.empty in
  
  let po_array, cyclic_rules = create_po gram_rhs lhs_table lhslists prio_dat
    array_nt_prio (Array.length gram_lhs) str_non_ter nt_of_ind prio_of_ind in
  
  gram_lhs, gram_rhs, lhs_table, actions, inherited, nt_nb, po_array,
  user_g, array_nt_prio, nt_of_ind, prio_of_ind,
  nt_table, str_non_ter, prio_dat, rn_of_rule, regexp_table,
  ((List.rev !(fst regexp_list)), (List.rev !(snd regexp_list))),
  ((List.rev !(fst regexp_actions)), (List.rev !(snd regexp_actions))),
  regexp_array, rule_options, lhslists, cyclic_rules, gram_parnt, bnt_array





(*let create_interm_pdev rapf_list relations global_data local_data nt_cons_map entry_point_list ppar regexp_decl main_lexer aux_lexer token_nb str_ter ter_table pdev =
  let gram_lhs,gram_rhs,lhs_table,actions,user_nt_nb,ist_nt_nb,
    po_array,user_g, array_nt_prio,nt_of_ind,
    prio_of_ind, nt_table, str_non_ter, priority_data, rn_of_rule,
    regexp_table, regexp_list, regexp_actions, regexp_array, allow_layout =
    make_grammar rapf_list relations ppar ter_table str_ter ppar.regexp_fun
    (*token_nb*) pdev.regexp_table
  in
  let regexp_nb = (List.length (fst regexp_list)) +
    (List.length (snd regexp_list)) in
  let new_token_nb = token_nb + regexp_nb in
  let entry_point_list =
    List.map (fun ep -> Hashtbl.find nt_table ep) entry_point_list
  in
  let cons_of_nt = Array.make (Array.length str_non_ter) 0 in
  let f nt cons =
    try
      let i = Hashtbl.find nt_table nt in
      cons_of_nt.(i) <- cons
    with Not_found -> ()
    (* nt_cons_map may contains non terminals that are not in any rules
    in the case of addition of new rules at parsing time one uses
    create_parsing_device just for the set of new rules but one reuses
    the nt_cons_map of the previous grammar with the possible addition
    of new pairs (nt,cons) for new non terminals. *)
  in
  String_map.iter f nt_cons_map;
  let lhslists = Array.make ist_nt_nb Ntp_map.empty in
  
  let time1 = Sys.time () in
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    priority_data array_nt_prio in
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
      (time2-.time1);
  let state_list, n, stations, entry_points, is_trace =
    create_aut gram_rhs gram_lhs r_L
    priority_data 0 array_nt_prio nt_of_ind prio_of_ind ist_nt_nb
    lhslists lhs_table str_non_ter entry_point_list ppar new_token_nb
    str_ter regexp_array
  in
  
  (*print_gram_lhs gram_lhs lhs_of_ind str_non_ter;*)
  
  let table, table_it, table_lit_trans(*, table_lex_trans*) =
    make_table state_list n (Array.length gram_lhs) gram_rhs
      lhs_table nt_of_ind prio_of_ind array_nt_prio ppar new_token_nb
      str_ter str_non_ter pdev.prio.prd_names regexp_array
  in
  let rnb = Array.length gram_rhs in
  let actions = Array.make rnb [] in
  List.iter (fun (r,a) ->
    let i = Hashtbl.find rn_of_rule r in
    actions.(i) <- a::actions.(i))
  rapf_list;
  
  { gram_rhs = gram_rhs ;
    gram_lhs = gram_lhs ;
    lhs_table = lhs_table ;
    entry_points = entry_points ;
    g_nb = 0;
    lex_nb = 0;
    nt_table = nt_table ;
    stations = stations ;
    state_list = state_list ;
    is_trace = is_trace;
    array_nt_prio = array_nt_prio;
    st_nb = n;
    table = table ;
    table_it = table_it ;
    (*table_lex_trans = table_lex_trans ;*)
    table_lit_trans = table_lit_trans ;
    lhslists = lhslists ;
    r_L = r_L ;
    po = po_array ;
    data = global_data;
    loc_data = local_data ;
    prio = priority_data ;
    nt_nb = Array.length gram_lhs;
    token_nb = new_token_nb;
    prio_of_ind = prio_of_ind;
    nt_of_ind = nt_of_ind;
    str_non_ter = str_non_ter;
    cons_of_nt = cons_of_nt;
    relations = relations;
    nt_cons_map = nt_cons_map;
    rn_of_rule = rn_of_rule;
    entry_point = 0;
    actions = actions;
    regexp_decl = pdev.regexp_decl;
    main_lexer_start = pdev.main_lexer_start;
    main_lexer_table = pdev.main_lexer_table;
    main_lexer_actions = pdev.main_lexer_actions;
    aux_lexer = pdev.aux_lexer;
    str_ter = pdev.str_ter;
    ter_table = pdev.ter_table;
    layout_id = pdev.layout_id;
    regexp_table = pdev.regexp_table;
    regexp_array = pdev.regexp_array;
    allow_layout = allow_layout }*)





let create_parsing_device ra_list relations global_data local_data nt_cons_map entry_point_list ppar regexp_decl main_lexer aux_lexer token_nb str_ter ter_table =
  
  let gram_lhs, gram_rhs, lhs_table, actions, inherited, nt_nb,
    po_array, user_g, array_nt_prio,nt_of_ind,
    prio_of_ind, nt_table, str_non_ter, priority_data, rn_of_rule,
    regexp_table, regexp_list, regexp_actions, regexp_array,
    rule_options, lhslists, cyclic_rules, gram_parnt, bnt_array =
    make_grammar ra_list relations ppar ter_table str_ter ppar.regexp_fun
    (*token_nb*) (Hashtbl.create 10)
  in
  let regexp_nb = (List.length (fst regexp_list)) +
    (List.length (snd regexp_list)) in
  let new_token_nb = token_nb + regexp_nb in
  let entry_point_list =
    List.map (fun ep -> Hashtbl.find nt_table ep) entry_point_list
  in
  let cons_of_nt = Array.make (Array.length str_non_ter) 0 in
  let f nt cons =
    try
      let i = Hashtbl.find nt_table nt in
      cons_of_nt.(i) <- cons
    with Not_found -> ()
    (* nt_cons_map may contains non terminals that are not in any rules
    in the case of addition of new rules at parsing time one uses
    create_parsing_device just for the set of new rules but one reuses
    the nt_cons_map of the previous grammar with the possible addition
    of new pairs (nt,cons) for new non terminals. *)
  in
  String_map.iter f nt_cons_map;
  (*let lhslists = Array.make nt_nb Ntp_map.empty in*)
  
  let time1 = Sys.time () in
  let _, r_L =
    compute_L gram_rhs gram_lhs lhslists priority_data array_nt_prio in
  let time2 = Sys.time () in
  if !dypgen_verbose>1 then
    Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
      (time2-.time1);
  let state_list, n, stations, entry_points, is_trace =
    create_aut gram_rhs gram_lhs gram_parnt bnt_array r_L
    priority_data 0 array_nt_prio nt_of_ind prio_of_ind nt_nb
    lhslists lhs_table str_non_ter entry_point_list ppar new_token_nb
    str_ter regexp_array
  in
  
  (*print_gram_lhs gram_lhs lhs_of_ind str_non_ter;*)
  
  let table, table_it, table_lit_trans, state_bnt, state_is_mergeable =
    make_table state_list n (Array.length gram_lhs) gram_rhs
      lhs_table nt_of_ind prio_of_ind array_nt_prio ppar new_token_nb
      str_ter str_non_ter priority_data.prd_names regexp_array
  in
  (*let rnb = Array.length gram_rhs in
  let actions = Array.make rnb [] in
  List.iter (fun (r,a) ->
    let i = Hashtbl.find rn_of_rule r in
    actions.(i) <- a::actions.(i))
  ra_list;*)
  
  let regexp_decl = compile_regexp_decl regexp_decl in
  let rl = List.map (fun (_,r) -> r) (fst main_lexer) in
  let rl = (fst regexp_list)@rl@(snd regexp_list) in
  let main_table, main_start = make_lexer regexp_decl rl in
  (*let regexp_actions_array =
    Array.make regexp_nb (0,ppar.regexp_fun) in*)
  let mla_nb = (Array.length (snd main_lexer)) in
  (*Printf.printf "mla_nb=%d, regexp_nb=%d\nregexp_actions len = %d %d\nregexp_list len = %d %d\n"
    mla_nb regexp_nb
    (List.length (fst regexp_actions))
    (List.length (snd regexp_actions))
    (List.length (fst regexp_list))
    (List.length (snd regexp_list));*)
  let main_lexer_actions = Array.make
    (mla_nb+regexp_nb)
    (0,ppar.regexp_fun) in
  let i = List.fold_left
    (fun i x -> main_lexer_actions.(i) <- x; i+1) 0
    (fst regexp_actions) in
  let arr = snd main_lexer in
  for j=0 to mla_nb-1 do
    main_lexer_actions.(j+i) <- arr.(j)
  done;
  let _ = List.fold_left
    (fun i x -> main_lexer_actions.(i) <- x; i+1) (i+mla_nb)
    (snd regexp_actions) in
  let aux_nb = List.length aux_lexer in
  let aux_table = Hashtbl.create aux_nb in
  let aux_start = Hashtbl.create aux_nb in
  let aux_actions = Hashtbl.create aux_nb in
  List.iter (fun (name,(rl,al)) ->
    let table, start = make_lexer regexp_decl rl in
    Hashtbl.add aux_table name table;
    Hashtbl.add aux_start name start;
    Hashtbl.add aux_actions name (array_of_list al)) aux_lexer;
  
  { gram_rhs = gram_rhs ;
    gram_lhs = gram_lhs ;
    gram_parnt = gram_parnt ;
    bnt_array =  bnt_array ;
    (*dypgen_epsilon =
      (try Hashtbl.find nt_table "dypgen__epsilon"
      with Not_found -> -1);*)
    lhs_table = lhs_table ;
    entry_points = entry_points ;
    g_nb = 0;
    lex_nb = 0;
    nt_table = nt_table ;
    stations = stations ;
    state_list = state_list ;
    is_trace = is_trace;
    array_nt_prio = array_nt_prio;
    st_nb = n;
    table = table ;
    state_bnt = state_bnt ;
    state_is_mergeable = state_is_mergeable ;
    table_it = table_it ;
    (*table_lex_trans = table_lex_trans ;*)
    table_lit_trans = table_lit_trans ;
    lhslists = lhslists ;
    r_L = r_L ;
    po = po_array ;
    cyclic_rules = cyclic_rules ;
    data = global_data;
    loc_data = local_data ;
    prio = priority_data ;
    nt_nb = Array.length gram_lhs;
    token_nb = new_token_nb;
    prio_of_ind = prio_of_ind;
    nt_of_ind = nt_of_ind;
    str_non_ter = str_non_ter;
    cons_of_nt = cons_of_nt;
    relations = relations;
    nt_cons_map = nt_cons_map;
    rn_of_rule = rn_of_rule;
    entry_point = 0;
    actions = actions;
    inherited = inherited;
    regexp_decl = regexp_decl;
    main_lexer_start = main_start;
    main_lexer_table = main_table;
    main_lexer_actions = main_lexer_actions;
    aux_lexer = {
      aux_lexer_start = aux_start;
      aux_lexer_table = aux_table;
      aux_lexer_actions = aux_actions };
    str_ter = str_ter;
    ter_table = ter_table;
    layout_id = (try Hashtbl.find ter_table "__dypgen_layout"
      with Not_found -> -1 (*assert false*));
    regexp_table = regexp_table;
    (*regexp_actions = regexp_actions_array;*)
    regexp_array = regexp_array;
    rule_options = rule_options }


(*let array_of_list l =
  let n = List.length l in
  let a = Array.make n (List.hd l) in
  let _ = List.fold_left (fun i x -> a.(i) <- x; i+1) 0 l in
  ()*)

let make_parser
  ra_list relations global_data local_data nt_cons_map entry_point_list
  merge_warning token_nb undef_nt
  get_value get_name str_token global_data_equal local_data_equal
  test_cons str_cons cons_str cons_table merge_array lexbuf_position_fun
  regexp_decl main_lexer aux_lexer ter_string_list regexp_fun use_dyplex =
  
  (*let ra_list = List.map (fun (r,a) -> (r,a,[])) ra_list in*)
  
  let ppar = {
    merge_warning = merge_warning;
    undef_nt = undef_nt;
    get_value = get_value;
    get_name = get_name;
    str_token = str_token;
    global_data_equal = global_data_equal;
    local_data_equal = local_data_equal;
    find_rightSib_global_data_equal = (fun _ _ -> true);
    find_rightSib_local_data_equal = (fun _ _ -> true);
    test_cons = test_cons;
    str_cons = str_cons;
    cons_str = cons_str;
    cons_table = cons_table;
    merge_array = merge_array;
    lexbuf_position_fun = lexbuf_position_fun;
    regexp_fun = regexp_fun;
    use_dyplex = use_dyplex;
    use_rule_order = false;
    use_all_actions = false }
  in
  (* Here token_nb is the number of named terminals,
  in parsing device it is the sum of this number and
  the number of regexp used in rhs of parser rules. *)
  (*let main_lex_re, main_lex_act = main_lexer in
  let len, main_lex_act = match main_lex_act with
    | [] -> 0, []
    | _::t -> (List.length t), t
  in
  let main_lex_actions = Array.make len (0,regexp_fun) in
  let _ = List.fold_left (fun i x -> main_lex_actions.(i) <- x; i+1)
    0 main_lex_act in
  let main_lexer = main_lex_re, main_lex_actions in*)
  let main_lexer = match main_lexer with
    | ("dummy_entry",_)::_, _ -> [],[||]
    | a, b -> a, array_of_list b
  in
  let str_ter = Array.make token_nb "" in
  let ter_table = Hashtbl.create token_nb in
  List.iter (fun (name,id) ->
    str_ter.(id) <- name;
    Hashtbl.add ter_table name id) ter_string_list;
  let pdev = create_parsing_device
    ra_list relations global_data local_data nt_cons_map entry_point_list
    ppar regexp_decl main_lexer aux_lexer token_nb str_ter ter_table
  in
  pdev, ppar



let remove_from_user_g rl user_g =
  let f user_g r = Urule_map.remove r user_g in
  List.fold_left f user_g rl

let build_nt_to_add gram_rhs r_L item_list lhslists prio_dat array_nt_prio =
  let nta nt_to_add (rn,dp) = match gram_rhs.(rn).(dp) with
    | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
        let lhs_l =
          comp_lhslist nt lhslists prio_dat array_nt_prio
        in
        let g1 nt_to_add ind =
          Int_set.add ind nt_to_add
        in
        let g2 nt_to_add (_,_,j) =
          let ind_list = r_L.(j) in
          List.fold_left g1 nt_to_add ind_list
        in
        List.fold_left g2 nt_to_add lhs_l
    | _ -> assert false
  in
  List.fold_left nta Int_set.empty item_list


let str_user_rhs rhs str_ter =
  let rec aux s rhs = match rhs with
    | [] -> s
    | (Ps_Non_ter (nt,_))::tl -> aux (s^" "^(string_of_int nt)) tl
    | (Ps_Non_ter_NL (nt,_))::tl -> aux (s^" - "^(string_of_int nt)) tl
    | (Ps_Ter t)::tl -> aux
      (s^" "^(try str_ter.(t) with _ -> (Printf.sprintf "<regexp:%d>" t))) tl
    | (Ps_Ter_NL t)::tl -> aux
      (s^" "^(try str_ter.(t) with _ -> (Printf.sprintf "- <regexp:%d>" t))) tl
  in
  aux "" rhs

let str_user_rule (lhs,rhs,_) str_ter =
  (string_of_int lhs)^" -> "^(str_user_rhs rhs str_ter)

let print_ral ral str_ter =
  let f (r,a) = print_endline (str_user_rule r str_ter) in
  List.iter f ral




let mark source old_stations component predict lhs_nb is_station count_es lhs_station source_to_clear q1 =
  (*print_state q1 gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
  str_token_name priority_names;*)
  (*Printf.fprintf !log_channel "is_station:%s\n" (string_of_bool is_station);
  Printf.fprintf !log_channel "q1.number:%d\n" q1.number;
  Printf.fprintf !log_channel "component.(q1.number):%d\n" component.(q1.number);
  Printf.fprintf !log_channel "lhs_nb:%d\n" lhs_nb;*)
  let f1 q lhs_ind stc =
    (*Printf.fprintf !log_channel "lhs_ind:%d\n" lhs_ind;*)
    match old_stations.(component.(q1.number)*lhs_nb+lhs_ind) with
    | Some_station r -> source.(r.number) <- q.number; r.number::stc
    | Extra_station r -> source.(r.es_number) <- q.number; r.es_number::stc
    | No_station ->
        (*Printf.fprintf !log_channel "extra_station 1 pour state=%d, lhs_ind=%d\n"
          q.number lhs_ind;*)
        let r = {
          es_number = !count_es;
          es_component = component.(q1.number) }
        in
        incr count_es;
        old_stations.(component.(q1.number)*lhs_nb+lhs_ind) <- Extra_station r;
        Hashtbl.add lhs_station r.es_number lhs_ind;
        source.(r.es_number) <- q.number; r.es_number::stc
  in
  if is_station then
    (source.(q1.number) <- q1.number;
    Int_set.fold (f1 q1) predict.(q1.number) (q1.number::source_to_clear))
  else
    let f2 lhs_ind stc =
      match old_stations.(component.(q1.number)*lhs_nb+lhs_ind) with
      | Some_station q2 ->
          source.(q2.number) <- q2.number;
          Int_set.fold (f1 q2) predict.(q2.number) (q2.number::stc)
      | Extra_station es ->
          source.(es.es_number) <- es.es_number; es.es_number::stc
      | No_station ->
          (*Printf.fprintf !log_channel "extra_station 2\n";*)
          let r = {
            es_number = !count_es;
            es_component = component.(q1.number) }
          in
          incr count_es;
          Hashtbl.add lhs_station r.es_number lhs_ind;
          old_stations.(component.(q1.number)*lhs_nb+lhs_ind) <- Extra_station r;
          source.(r.es_number) <- r.es_number; r.es_number::stc
    in
    Int_set.fold f2 predict.(q1.number) source_to_clear







let rec visit v src color_black source result old_stations component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list =
  
  let comp = component.(v.number) in
  (*Printf.fprintf !log_channel "visit, comp=%d, v.number=%d\n" comp v.number;*)
  let cb_tc =
    if color_black.(v.number) then cb_tc
    else (color_black.(v.number) <- true; v.number::cb_tc)
  in
  
  let f1 lhs (cb_tc, state_list) =
    match old_stations.(comp*lhs_nb+lhs) with
    | Some_station w ->
    (*Printf.fprintf !log_channel "visit f1 Some_station\n";*)
    if (source.(w.number) = src || source.(w.number) = -1)
        && color_black.(w.number) = false then
      visit w src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
    else (cb_tc, state_list)
    | No_station ->
    (*Printf.fprintf !log_channel "visit f1 No_station\n";*)
    let es = {
      es_number = !count_es;
      es_component = comp }
    in
    incr count_es;
    Hashtbl.add lhs_station es.es_number lhs;
    old_stations.(comp*lhs_nb+lhs) <- Extra_station es;
      visit_es es src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
    | Extra_station es ->
    (*Printf.fprintf !log_channel "visit f1 Extra_station\n";*)
    if (source.(es.es_number) = src || source.(es.es_number) = -1)
        && color_black.(es.es_number) = false then
      visit_es es src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
    else (cb_tc, state_list)
  in
  let cb_tc, state_list =
    Int_set.fold f1 predict.(v.number) (cb_tc, state_list)
  in
  let lhs = Hashtbl.find lhs_station v.number in
  let rec f2 i cb_tc state_list =
    if i=comp_nb then (cb_tc, state_list) else
    if i=comp then f2 (i+1) cb_tc state_list else
    match old_stations.(i*lhs_nb+lhs) with
    | No_station ->
        (*Printf.fprintf !log_channel "visit f2 No_station\n";*)
        f2 (i+1) cb_tc state_list
    | Some_station w ->
    (*Printf.fprintf !log_channel "visit f2 Some_station\n";*)
    if source.(w.number) = -1
        && color_black.(w.number) = false then
      let cb_tc, state_list =
        visit w src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
      in
      let state_list =
        if result.(w.number) then state_list
        else (result.(w.number) <- true; w::state_list)
      in
      f2 (i+1) cb_tc state_list
    else f2 (i+1) cb_tc state_list
    | Extra_station es ->
    (*Printf.fprintf !log_channel "visit f2 Extra_station\n";*)
    if source.(es.es_number) = -1
        && color_black.(es.es_number) = false then
      let cb_tc, state_list =
        visit_es es src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
      in
      f2 (i+1) cb_tc state_list
    else f2 (i+1) cb_tc state_list
  in
  f2 0 cb_tc state_list



and visit_es es src color_black source result old_stations component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list =
  let cb_tc =
    if color_black.(es.es_number) then cb_tc
    else (color_black.(es.es_number) <- true; es.es_number::cb_tc)
  in
(*Printf.fprintf !log_channel "visit_es, comp=%d, es.es_number=%d\n"
    es.es_component es.es_number;*)
  let lhs = Hashtbl.find lhs_station es.es_number in
  let rec f2 i cb_tc state_list =
    if i=comp_nb then (cb_tc, state_list) else
    if i=es.es_component then f2 (i+1) cb_tc state_list else
    match old_stations.(i*lhs_nb+lhs) with
    | No_station ->
        (*Printf.fprintf !log_channel "visit No_station\n";*)
        f2 (i+1) cb_tc state_list
    | Some_station w ->
    (*Printf.fprintf !log_channel "visit Some_station\n";*)
    if source.(w.number) = -1
        && color_black.(w.number) = false then
      let cb_tc, state_list =
        visit w src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
      in
      let state_list =
        if result.(w.number) then state_list
        else (result.(w.number) <- true; w::state_list)
      in
      f2 (i+1) cb_tc state_list
    else f2 (i+1) cb_tc state_list
    | Extra_station es1 ->
    (*Printf.fprintf !log_channel "visit Extra_station\n";*)
    if source.(es1.es_number) = -1
        && color_black.(es1.es_number) = false then
      let cb_tc, state_list =
        visit_es es1 src color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
      in
      f2 (i+1) cb_tc state_list
    else f2 (i+1) cb_tc state_list
  in
  f2 0 cb_tc state_list




let epsilon_closure state_list old_stations component predict lhs_nb comp_nb is_station lhs_station count_es source color_black result =
  let source_to_clear =
    List.fold_left
    (mark source old_stations component predict lhs_nb is_station count_es lhs_station)
    [] state_list
  in
  
  List.iter (fun s -> result.(s.number) <- true) state_list;
  
  let maybe_visit (cb_tc, state_list) q =
    if source.(q.number) = q.number
        && color_black.(q.number)  = false then
      visit q q.number color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
    else
      (cb_tc, state_list)
  in
  
  let maybe_visit_es (cb_tc, state_list) es =
    let b =
      source.(es.es_number) = es.es_number
        && color_black.(es.es_number) = false
    in
    if b then
      visit_es es es.es_number color_black source result old_stations
        component predict lhs_nb comp_nb lhs_station count_es cb_tc state_list
    else
      (cb_tc, state_list)
  in
  
  let maybe_visit_bis (cb_tc, state_list) q =
    let comp = component.(q.number) in
    (*Printf.fprintf !log_stack_channel "predict card = %d\n"
      (Int_set.cardinal predict.(q.number));*)
    Int_set.fold
    (fun lhs (cb_tc, state_list) ->
      match old_stations.(comp*lhs_nb+lhs) with
      | Some_station s ->
          maybe_visit (cb_tc, state_list) s
      | Extra_station es ->
          maybe_visit_es (cb_tc, state_list) es
      | No_station -> assert false)
    predict.(q.number) (cb_tc, state_list)
  in
  
  let color_black_to_clear, state_list =
    if is_station then
      List.fold_left maybe_visit ([],state_list) state_list
    else
      List.fold_left maybe_visit_bis ([],state_list) state_list
  in
  state_list, source_to_clear, color_black_to_clear




let merge_states state_list li succ_states_array prd_nb non_kernel_array =
  let state = {
    number = !countst;
    li = li;
    items = new_item_set ();
    mergeable = not (list_exists (fun s -> not s.mergeable) state_list);
    bestowing_nt = List.fold_left
      (fun bnt s -> Int_set.union bnt s.bestowing_nt) Int_set.empty state_list;
    succ_states = [] }
  in
  incr countst;
  let sl = List.fold_left
    (fun sl s ->
     (*Printf.fprintf !log_stack_channel "red card=%d\n" (Int_set.cardinal s.items.reducible);
     Printf.fprintf !log_stack_channel "red card=%d\n" (Int_set.cardinal s.items.reducible);*)
      state.items.reducible <-
        Int_set.union state.items.reducible s.items.reducible;
      state.items.non_kernel <-
        merge_non_kernel state.items.non_kernel s.items.non_kernel
        non_kernel_array;
      state.items.predict <- Predict.union state.items.predict s.items.predict;
      List.fold_left (fun sl (v,p) ->
        if !succ_states_array.(v.number*prd_nb+p) then sl
        else (!succ_states_array.(v.number*prd_nb+p) <- true; (v,p)::sl))
        sl s.succ_states)
    [] state_list
  in
  state.succ_states <- sl;
  List.iter (fun (s,p) -> !succ_states_array.(s.number*prd_nb+p) <- false) sl;
  state



module Ordered_edge =
struct
  type t = lit_trans * priority
  let compare = Pervasives.compare
end
module Edge_map = Map.Make(Ordered_edge)



let make_key state_list gram_rhs =
  let is = new_item_set () in
  List.iter
    (fun s ->
      is.reducible <- Int_set.union is.reducible
        (Int_set.filter
        (fun rn -> Array.length gram_rhs.(rn)>0)
        s.items.reducible);
      is.kernel_t <- Intc_set.union is.kernel_t s.items.kernel_t;
      is.kernel_nt <- Intc_set.union is.kernel_nt s.items.kernel_nt)
    state_list;
  is,
  (Int_set.cardinal is.reducible) +
  (Intc_set.cardinal is.kernel_t) +
  (Intc_set.cardinal is.kernel_nt)




let compute_nt_to_add is gram_rhs r_L array_nt_prio lhslists prio_dat non_kernel_array =
  let aux (rn,dp) (nt_to_add,nk) =
    try (match gram_rhs.(rn).(dp) with
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
        (List.fold_left g2 nt_to_add lhs_l),
        if non_kernel_array.(rn) then nk
        else (non_kernel_array.(rn) <- true; rn::nk))
      | _ -> nt_to_add,nk)
    with Invalid_argument _ -> nt_to_add,nk
  in
  let nt_to_add,nk =
    Intc_set.fold aux is.kernel_nt (Int_set.empty,is.non_kernel)
  in
  is.non_kernel <- nk;
  List.iter (fun rn -> non_kernel_array.(rn) <- false) nk;
  nt_to_add



let consider_priorities pdev_list state_lists old_is_trace priodata array_nt_prio lhslists r_L st_nb ind_subst diff_lhslist gram_rhs gram_lhs gram_parnt bnt_array lhs_table nt_of_ind prio_of_ind str_non_ter succ_states_array non_kernel_array =
  
  let token_nb =
    List.fold_left (fun tn p -> max p.token_nb tn) 0 pdev_list in
  
  let array_lt_ter = Array.make token_nb None in
  let array_lt_ter_nl = Array.make token_nb None in
  (*let splitting_ter = Array.make token_nb false in*)
  let nt_nb = Array.length gram_lhs in
  let array_lt_nt = Array.make nt_nb None in
  let array_lt_nt_nl = Array.make nt_nb None in
  (*let splitting_nt = Array.make (Array.length str_non_ter) false in*)
  
  countst := st_nb;
  let array_lhs = Array.make nt_nb false in
  
  (*let aux_nt (is_list,lt_list) lhs =
    let (_,_,ind) = lhs in
    match array_lt_nt.(ind) with
      | None ->
          let is = new_item_set () in
          array_lt_nt.(ind) <- Some is;
          is::is_list,(Ps_Non_ter lhs)::lt_list
      | Some is -> is::is_list,lt_list
  in
  
  let aux_nt_nl (is_list,lt_list,nl_list) lhs =
    let (_,_,ind) = lhs in
    match array_lt_nt_nl.(ind) with
      | None ->
          let is = new_item_set () in
          array_lt_nt_nl.(ind) <- Some is;
          is::is_list,(Ps_Non_ter_NL lhs)::lt_list,(ind,is)::nl_list
      | Some is -> is::is_list,lt_list,nl_list
  in*)
  
  let f i pdev =
    let rec map_succ v =
      (*Printf.fprintf !log_channel "-state built-\n";
      print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        str_token_name prio_dat.prd_names;
      Printf.fprintf !log_channel "\n";
      flush_all ();*)
      (*Printf.printf "build state %d\n" v.number;
      flush_all ();*)
      let vl =
        move_LR0 v old_is_trace.(i) gram_rhs gram_lhs gram_parnt bnt_array
          r_L priodata
          array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
          array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs succ_states_array
          non_kernel_array
      in
      List.iter map_succ vl
    in
    
    let (is_trace_tok, is_trace_tok_nl), (is_trace_nt, is_trace_nt_nl) =
      old_is_trace.(i) in
    
    let g ntp_map s =
      (*Printf.fprintf !log_channel "consider_priorities s.number=%d\n" s.number;
      Printf.fprintf !log_channel "kernel_nt size=%d\n"
        (Intc_set.cardinal s.items.kernel_nt);
      print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        E.str_token_name priodata.prd_names;*)
      
      (*let spl_ter_l, spl_nt_l =
        splitting_symbol s gram_rhs bnt_array splitting_ter splitting_nt in*)
      
      let h (rn,dp) (lt_list, nl_list, ntp_map) =
        (*print_kernel !log_channel gram_rhs lhs_table str_non_ter
         E.str_token_name priodata.prd_names (rn,dp);*)
        let rhs = gram_rhs.(rn) in
        let (is_list, lt_list), nl_list, ntp_map = match rhs.(dp) with
          | Ps_Non_ter nt ->
              let lhslist, ntp_map =
                diff_lhslist nt ntp_map pdev lhslists priodata
                array_nt_prio ind_subst.(i)
              in
              List.fold_left
                (aux_move_nt array_lt_nt (*splitting_nt*))
                ([],lt_list) lhslist, nl_list, ntp_map
          | Ps_Ter _ | Ps_Ter_NL _ -> ([],lt_list), nl_list, ntp_map
          | Ps_Non_ter_NL nt ->
              let lhslist, ntp_map =
                diff_lhslist nt ntp_map pdev lhslists priodata
                array_nt_prio ind_subst.(i)
              in
              let a, b, c =
                List.fold_left
                  (aux_move_nt_nl array_lt_nt_nl (*splitting_nt*))
                  ([],lt_list, nl_list) lhslist in
              (a, b), c, ntp_map
        in
        List.iter
          (if dp+1 = Array.length rhs then
            (fun is -> is.reducible <- Int_set.add rn is.reducible)
          else
          match rhs.(dp+1) with
            | Ps_Ter _ | Ps_Ter_NL _ -> (fun is -> is.kernel_t <-
                Intc_set.add (rn,dp+1) is.kernel_t)
            | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
                (fun is -> (is.kernel_nt <- Intc_set.add (rn,dp+1) is.kernel_nt)))
          is_list;
        lt_list, nl_list, ntp_map
      in
      let lt_list, nl_list, ntp_map =
        Intc_set.fold h s.items.kernel_nt ([],[],ntp_map) in
      let h2 (lt_list, nl_list, ntp_map) rn =
        h (rn,0) (lt_list, nl_list, ntp_map) in
      let lt_list, nl_list, ntp_map =
        List.fold_left h2 (lt_list, nl_list, ntp_map) s.items.non_kernel in
      (*Intc_set.iter (print_kernel !log_channel gram_rhs lhs_table
         str_non_ter str_token_name priority_names)
         is.kernel_nt;*)
      
      (*List.iter (fun x -> splitting_ter.(x) <- false) spl_ter_l;
      List.iter (fun x -> splitting_nt.(x) <- false) spl_nt_l;*)
      
      (*let add_to_is is0 is1op = match is1op with
        | None -> ()
        | Some is1 ->
            is0.kernel_nt <- Intc_set.union is0.kernel_nt is1.kernel_nt;
            is0.kernel_t <- Intc_set.union is0.kernel_t is1.kernel_t;
            is0.reducible <- Int_set.union is0.reducible is1.reducible
      in*)
      
      add_items_to_nl_state array_lt_ter array_lt_nt nl_list;
      (*List.iter (fun (ind, is0) -> add_to_is is0 array_lt_nt.(ind)) nl_list;*)
     
      let prd_nb = priodata.prd_nb in
      List.iter (fun (s,p) -> ga_set succ_states_array (s.number*prd_nb+p) true)
        s.succ_states;
      
      let f2 (vl, succ_states_list) symb =
        
        let is_opt, prio = match symb with
          | Ps_Non_ter (_,prio,ind) -> array_lt_nt.(ind), prio
          | Ps_Non_ter_NL (_,prio,ind) -> array_lt_nt_nl.(ind), prio
          | _ -> assert false
        in
        match is_opt with Some is ->
          close_state
          is_trace_tok is_trace_tok_nl is_trace_nt is_trace_nt_nl
          succ_states_array prd_nb gram_rhs gram_lhs gram_parnt lhslists
          priodata array_nt_prio r_L non_kernel_array symb prio
          (vl, succ_states_list) is
        | None -> assert false
        (*let it_nb =
          (Int_set.cardinal is.reducible)
          + (Intc_set.cardinal is.kernel_nt)
          + (Intc_set.cardinal is.kernel_t)
        in
        let old_reducible = is.reducible in
        (* epsilon rules may be added to reducible by closure_LR0 *)
        try
          let v1 = match symb with
            | Ps_Non_ter (nt,_,_) ->
                Map_is.find (is,it_nb) (fst (snd old_is_trace.(i))).(nt)
            | Ps_Non_ter_NL (nt,_,_) ->
                Map_is.find (is,it_nb) (snd (snd old_is_trace.(i))).(nt)
            | _ -> assert false
          in
          let succ_states_list =
            if ga_get succ_states_array (v1.number*prd_nb+prio)
            then succ_states_list
            else (ga_set succ_states_array (v1.number*prd_nb+prio) true;
              (v1,prio)::succ_states_list)
          in
          vl, succ_states_list
        with Not_found ->
          let f1 (rn,dp) (nt_to_add,predict) = match gram_rhs.(rn).(dp) with
            | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
                let lhs_l =
                  comp_lhslist nt lhslists priodata array_nt_prio
                in
                let g1 nt_to_add ind =
                  Int_set.add ind nt_to_add
                in
                let g2 nt_to_add (_,_,ind) =
                  let ind_list = r_L.(ind) in
                  List.fold_left g1 nt_to_add ind_list
                in
                (List.fold_left g2 nt_to_add lhs_l), Predict.add nt predict
            | Ps_Ter _ | Ps_Ter_NL _ -> assert false
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
            bestowing_nt = make_bestowing_nt is gram_parnt gram_rhs lhslists
              priodata array_nt_prio;
            succ_states = [] }
          in
          incr countst;
          let old_is = { is with reducible = old_reducible } in
          let () = match symb with
            | Ps_Non_ter (nt,_,_) -> (fst (snd old_is_trace.(i))).(nt) <-
                Map_is.add (old_is,it_nb) v1 (fst (snd old_is_trace.(i))).(nt)
            | Ps_Non_ter_NL (nt,_,_) -> (snd (snd old_is_trace.(i))).(nt) <-
                Map_is.add (old_is,it_nb) v1 (snd (snd old_is_trace.(i))).(nt)
            | _ -> assert false
          in
          ga_set succ_states_array (v1.number*prd_nb+prio) true;
          v1::vl, (v1,prio)::succ_states_list*)
      in (* ends let f2 ... *)
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
      List.iter map_succ vl;
      ntp_map
    
    in (* ends let g ... *)
    let _ = List.fold_left g Ntp_map.empty state_lists.(i) in
    i+1
  in (* ends let f ... *)
  let _ = List.fold_left f 0 pdev_list in
  ()


let rec diff_list l1 l2 res = match l1,l2 with
  | _, [] -> res@l1
  | [], _ -> res
  | h1::t1, h2::t2 when h1=h2 -> diff_list t1 t2 res
  | h1::t1, h2::t2 ->
      if compare h1 h2 = 1 then diff_list l1 t2 res
      else diff_list t1 l2 res


exception No_prio

let pick_prio ntp = match ntp with
  | No_priority -> raise No_prio
  | Eq_priority p
  | Less_priority p
  | Lesseq_priority p
  | Greater_priority p
  | Greatereq_priority p -> p



let change_prio ntp p = match ntp with
  | No_priority -> raise No_prio
  | Eq_priority _ -> Eq_priority p
  | Less_priority _ -> Less_priority p
  | Lesseq_priority _ -> Lesseq_priority p
  | Greater_priority _ -> Greater_priority p
  | Greatereq_priority _ -> Greatereq_priority p


exception External_prio

let make_old_nt nt ntp pdev priodata str_non_ter =
  let old_nt = Hashtbl.find pdev.nt_table str_non_ter.(nt) in
  let old_ntp =
    try
      let p = pick_prio ntp in
      let old_p = Hashtbl.find pdev.prio.prd_ind priodata.prd_names.(p) in
      change_prio ntp old_p
    with No_prio -> ntp
      | Not_found -> raise External_prio
      (* This can happen because stations include items imported from
       other parse table components. *)
  in
  old_nt,old_ntp



(* to be improved: memoize the resulting list for a couple (nt,ntp) too,
instead of just the result for ntp. *)
let diff_lhslist nt_of_ind prio_of_ind str_non_ter (nt,ntp) ntp_map pdev lhslists priodata array_nt_prio ind_subst =
    (*Printf.fprintf !log_channel "diff_lhslist nt=%s ntp=%s\n"
      str_non_ter.(nt) (str_nt_prio ntp priodata.prd_names);*)
  try
    let prio_l = Ntp_map.find ntp ntp_map in
    (*Printf.fprintf !log_channel "diff_lhslist prio_l = ";
    List.iter (fun p -> Printf.fprintf !log_channel "%s " priodata.prd_names.(p)) prio_l;
    Printf.fprintf !log_channel "\n";*)
    List.map (fun p -> nt,p,Prio_map.find p array_nt_prio.(nt)) prio_l,
    ntp_map
  with Not_found ->
    let new_lhslist =
      comp_lhslist (nt,ntp) lhslists priodata array_nt_prio in
    let new_lhslist = List.sort Pervasives.compare new_lhslist in
    try
    let old_nt = make_old_nt nt ntp pdev priodata str_non_ter in
    let old_lhslist =
      comp_lhslist old_nt pdev.lhslists pdev.prio pdev.array_nt_prio in
    let old_lhslist =
      List.map (fun (nt,p,i) ->
        let i = ind_subst.(i) in
        nt_of_ind.(i),prio_of_ind.(i),i)
      old_lhslist
    in
    let old_lhslist = List.sort Pervasives.compare old_lhslist in
    let l = diff_list new_lhslist old_lhslist [] in
    let prio_l = List.map (fun (_,p,_) -> p) l in
    (*Printf.fprintf !log_channel "diff_lhslist res = ";
    List.iter (fun p -> Printf.fprintf !log_channel "%s "
      priodata.prd_names.(p)) prio_l;
    Printf.fprintf !log_channel "\n";*)
    l, Ntp_map.add ntp prio_l ntp_map
    with External_prio ->
      [], Ntp_map.add ntp [] ntp_map




let new_start_state is lit_trans subst_rn_red subst_rn nt_nb is_trace state_list gram_rhs gram_lhs gram_parnt bnt_array r_L array_nt_prio lhslists priodata non_kernel_array map_succ =

  let key_is = {
    reducible = (Int_set.filter
      (fun rn -> Array.length gram_rhs.(rn)>0)
      (subst_rn_red is.reducible));
    kernel_t = subst_rn is.kernel_t;
    kernel_nt = subst_rn is.kernel_nt;
    non_kernel = [];
    predict = Predict.empty }
  in
  let key = key_is,
    (Int_set.cardinal key_is.reducible) +
    (Intc_set.cardinal key_is.kernel_t) +
    (Intc_set.cardinal key_is.kernel_nt)
  in
    try
      let state_ind = match lit_trans with
        | Ps_Ter t -> (Map_is.find key (fst (fst is_trace)).(t)).number
        | Ps_Non_ter nt -> (Map_is.find key (fst (snd is_trace)).(nt)).number
        | Ps_Ter_NL t -> (Map_is.find key (snd (fst is_trace)).(t)).number
        | Ps_Non_ter_NL nt -> (Map_is.find key (snd (snd is_trace)).(nt)).number
      in
      (*Printf.printf "state_ind = %d\n" state_ind;*)
      state_list, (Some state_ind), is_trace
    with Not_found ->
      let nt_to_add =
        compute_nt_to_add key_is gram_rhs r_L array_nt_prio
        lhslists priodata non_kernel_array
      in
      closure_v0_LR0 key_is gram_rhs gram_lhs nt_to_add non_kernel_array;
      let v = {
        li = Ps_Ter (-1);
        items = key_is;
         (* is instead of key_is here doesn't produce any bug !? *)
        number = !countst;
        mergeable = is_mergeable is gram_parnt;
        bestowing_nt = make_bestowing_nt is gram_parnt;
        succ_states = [] }
      in
      let state_ind = !countst in
      incr countst;
      let is_trace, state_list = map_succ (is_trace,[]) v in
      state_list, (Some state_ind), is_trace



(* On utilise update_parsing_device � la place maintenant *)
(*let merge_parsing_device pdev_list newdata
    newlocal_data lit_trans g_nb is nt_cons_map relations ppar =
  
    let token_nb = (List.hd pdev_list).token_nb in
  (* to be changed when lexer is extensible *)
  
  (*let time0 = Sys.time () in*)
  
  let nt_table, nt_of_ind, prio_of_ind, array_nt_prio, gram_rhs,
      lhs_table, gram_lhs, actions, old_stations, str_non_ter,
      priodata, cons_of_nt, po_array, nt_nb, lhs_nb, pnb, (*component,*)
      rule_subst, nt_subst, (*user_g,*) lhslists, (*predict,*) lhs_station,
      old_is_trace, state_lists, st_nb, ind_subst, non_kernel_array,
      allow_layout =
    compose_grammar pdev_list
  in
  
  (*Printf.fprintf !log_channel "print_grammar:\n";
  print_grammar !log_channel gram_rhs lhs_table str_non_ter priodata.prd_names;
  Printf.fprintf !log_channel "\nprint_gram_lhs\n";
  print_gram_lhs !log_channel gram_lhs nt_of_ind prio_of_ind str_non_ter
    priodata.prd_names;*)
  
  (*Printf.fprintf !log_channel "\nstate_lists.(0):\n";
  List.iter
  (fun s ->
    Printf.fprintf !log_channel "\n";
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      E.str_token_name priodata.prd_names)
  state_lists.(0);*)
  
  (*let time2 = Sys.time () in
  Printf.printf "compose_grammar = %.3f s\n" (time2-.time0);*)
  
  (*merge_states_ref := [];*)
  
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    priodata array_nt_prio (*str_non_ter priodata.prd_names nt_of_ind prio_of_ind*) in
  
  (*let time1 = Sys.time () in
  Printf.printf "compute_L = %.3f s\n" (time1-.time2);*)
  
  let prd_nb = priodata.prd_nb in
  let succ_states_array = ref (Array.make (2*st_nb*prd_nb) false) in
  
  consider_priorities pdev_list state_lists old_is_trace priodata
    array_nt_prio lhslists r_L st_nb ind_subst
    (diff_lhslist nt_of_ind prio_of_ind str_non_ter) gram_rhs gram_lhs
    lhs_table nt_of_ind prio_of_ind str_non_ter succ_states_array
    non_kernel_array;
  
  (*let time2 = Sys.time () in
  Printf.printf "consider_priorities = %.3f s\n" (time2-.time1);*)
  
  let st_nb = !countst in
  
  let predict = Array.make st_nb Int_set.empty in
  let component = Array.make st_nb (-1) in
  
  let make_predict_array nt set =
    (*Printf.fprintf !log_channel "make_predict nt=%s, ntp=%s\n"
      str_non_ter.(fst nt) (str_nt_prio (snd nt) priodata.prd_names);*)
    let lhs_l = comp_lhslist nt lhslists priodata array_nt_prio in
    List.fold_left
      (fun set (_,_,i) ->
        (*Printf.fprintf !log_channel "add: nt=%d, p=%d, i=%d, %s\n" nt p i
        (str_lhs (nt,p,i) str_non_ter priodata.prd_names);*)
        Int_set.add i set)
      set lhs_l
  in
  
  for i=0 to pnb-1 do
    List.iter (fun s ->
      predict.(s.number) <-
        Predict.fold make_predict_array s.items.predict Int_set.empty;
      component.(s.number) <- i)
      state_lists.(i)
  done;
  
  (*let time1 = Sys.time () in
  Printf.printf "make component predict = %.3f s\n" (time1-.time2);*)
  
  (*let fst_pdev = List.hd pdev_list in
  
  if fst_pdev.ind_subst_inv <> [||] then
    for i=0 to lhs_nb-1 do
      ind_subst_inv.(i) <- fst_pdev.ind_subst_inv.(ind_subst_inv.(i))
    done*)
  
  let entry_point =
    let pdev = List.hd pdev_list in
    nt_subst.(0).(pdev.entry_point)
  in
  
  let is_trace =
    (Array.make token_nb Map_is.empty),
    (Array.make nt_nb Map_is.empty)
  in
  (*Printf.printf "lhs_nb = %d\n" lhs_nb;*)
  let stations = Array.make lhs_nb None in
  
  let subst_rn is =
    let aux (rn,x) set = Intc_set.add (rule_subst.(0).(rn),x) set in
    Intc_set.fold aux is Intc_set.empty
  in
  let subst_rn_red is =
    let aux rn set = Int_set.add rule_subst.(0).(rn) set in
    Int_set.fold aux is Int_set.empty
  in
  
  countst := 0;
  let count_es = ref (Array.length component) in
  
  (*let time3 = Sys.time () in*)
  
  let source = Array.make (st_nb+lhs_nb*pnb) (-1) in
  (* the +lhs_nb*pnb is here for the extra stations *)
  let color_black = Array.make (st_nb+lhs_nb*pnb) false in
  let result = Array.make (st_nb+lhs_nb*pnb) false in
  let symb_nb = nt_nb+token_nb in
  let edge_array = Array.make (symb_nb*prd_nb) [] in
  let new_ssa = ref (Array.make (2*st_nb*prd_nb) false) in
  
  let working_list =
    let rec g i wl =
      if i=lhs_nb then wl else
      let s_list =
        let rec f n s_list =
          if n=pnb then s_list else
          match old_stations.(n*lhs_nb+i) with
          | Some_station s -> f (n+1) (s::s_list)
          | _ -> f (n+1) s_list
        in f 0 []
      in
      let state_list_result, source_to_clear, color_black_to_clear =
        epsilon_closure s_list old_stations component predict
        lhs_nb pnb true lhs_station count_es source color_black result
      in
      List.iter (fun i -> source.(i) <- -1) source_to_clear;
      List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
      List.iter (fun i -> result.(i.number) <- false) state_list_result;
      let state = merge_states state_list_result (Ps_Ter (-1)) succ_states_array prd_nb non_kernel_array in
      stations.(i) <- Some state;
      g (i+1) (state::wl)
    in g 0 []
  in
  
  (*let time4 = Sys.time () in
  Printf.printf "working_list = %.3f s\n" (time4-.time3);*)
  
  (*let em_arg_ref = ref [] in*)
  (*let bloc_A_arg = ref [] in
  let bloc_B_arg = ref [] in*)
  (*let epsilon_closure_arg = ref [] in*)
  
  let rec process_state wl state_list =
    match wl with [] -> state_list | _ ->
    let state_list, new_wl =
      List.fold_left
      (fun (state_list, new_wl) s ->
        
        let el =
          (*em_arg_ref := s::!em_arg_ref;*)
          List.fold_left
          (fun el (t,p) -> match t.li with
            | Ps_Ter ter ->
                if edge_array.(symb_nb*p+ter) = [] then
                  (edge_array.(symb_nb*p+ter) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+ter) <- t::edge_array.(symb_nb*p+ter); el)
            | Ps_Non_ter nt ->
                if edge_array.(symb_nb*p+token_nb+nt) = [] then
                  (edge_array.(symb_nb*p+token_nb+nt) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+token_nb+nt)
                   <- t::edge_array.(symb_nb*p+token_nb+nt); el))
          [] s.succ_states
        in
        
        let (state_list, new_wl, new_ss_list) =
          List.fold_left
          (fun (state_list, new_wl, new_ss_list) (li,p) ->
            let edge_array_ind = match li with
              | Ps_Ter t -> symb_nb*p+t
              | Ps_Non_ter nt -> symb_nb*p+token_nb+nt
            in
            let s_list = edge_array.(edge_array_ind) in
            edge_array.(edge_array_ind) <- [];
            let key = make_key s_list gram_rhs in
            (*Printf.fprintf !log_stack_channel "\nkey is:\n";
            print_item_set !log_stack_channel (fst key) gram_rhs lhs_table
              nt_of_ind prio_of_ind str_non_ter str_token_name priodata.prd_names;
            Printf.fprintf !log_stack_channel "fin key is\n";*)
            try (* bloc_A_prof *)
              let succ_state = match li with
                | Ps_Ter t ->
                    (*bloc_A_arg := (key,(fst is_trace).(t),p,succ_state)::!bloc_A_arg;*)
                    Map_is.find key (fst is_trace).(t)
                | Ps_Non_ter nt ->
                    (*bloc_A_arg := (key,(snd is_trace).(nt),p,succ_state)::!bloc_A_arg;*)
                    Map_is.find key (snd is_trace).(nt)
              in
              let new_ss_list =
                if ga_get new_ssa (succ_state.number*prd_nb+p) then new_ss_list
                else (ga_set new_ssa (succ_state.number*prd_nb+p) true; (succ_state,p)::new_ss_list)
              in
              (state_list, new_wl, new_ss_list)
            with Not_found ->
              (*let li = (State_set.choose state_set).li in*)
              let state_list_result, source_to_clear, color_black_to_clear =
                epsilon_closure s_list old_stations component predict
                lhs_nb pnb false lhs_station count_es source color_black result
              in
              List.iter (fun i -> source.(i) <- -1) source_to_clear;
              List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
              List.iter (fun i -> result.(i.number) <- false) state_list_result;
              (*epsilon_closure_arg := (s_list, old_stations, component, predict,
                lhs_nb, pnb, lhs_station, count_es)::!epsilon_closure_arg;*)
              let state = merge_states state_list_result li succ_states_array prd_nb non_kernel_array in
              state.items.kernel_t <- (fst key).kernel_t;
              state.items.kernel_nt <- (fst key).kernel_nt;
              (*Printf.fprintf !log_stack_channel "\nstate is:\n";
              print_item_set !log_stack_channel state.items gram_rhs lhs_table
                nt_of_ind prio_of_ind str_non_ter str_token_name priodata.prd_names;
              Printf.fprintf !log_stack_channel "fin state is\n";*)
              (match li with
                | Ps_Ter t ->
                    (*bloc_B_arg := (key, state, (fst is_trace).(t),p, new_succ_states)::!bloc_B_arg;*)
                    (fst is_trace).(t) <- Map_is.add key state (fst is_trace).(t)
                | Ps_Non_ter nt ->
                    (*bloc_B_arg := (key, state, (snd is_trace).(nt),p, new_succ_states)::!bloc_B_arg;*)
                    (snd is_trace).(nt) <- Map_is.add key state (snd is_trace).(nt));
              ga_set new_ssa (state.number*prd_nb+p) true;
              (state::state_list,state::new_wl,(state,p)::new_ss_list))
          (state_list, new_wl, []) el
        in
        s.succ_states <- new_ss_list;
        List.iter (fun (s,p) -> !new_ssa.(s.number*prd_nb+p) <- false) new_ss_list;
        (state_list, new_wl))
      (state_list, []) wl
    in
    process_state new_wl state_list
  in
  let state_list =
    process_state working_list working_list
  in
  
  (*let time5 = Sys.time () in
  Printf.printf "process_state = %.3f s\n" (time5-.time4);*)
  
  let array_lt_ter = Array.make token_nb None in
  let array_lt_ter_nl = Array.make token_nb None in
  let array_lt_nt = Array.make nt_nb None in
  
  (*Gc.set { (Gc.get()) with Gc.space_overhead = 100 };*)
  
  let array_lhs = Array.make nt_nb false in
  
  let rec map_succ (is_trace, state_list) v =
    let vl =
      (*Printf.fprintf !log_channel "-state built-\n";
        print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        str_token_name priodata.prd_names;
      Printf.fprintf !log_channel "\n";
      flush_all ();*)
      move_LR0 v is_trace gram_rhs gram_lhs r_L priodata
        array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
        array_lhs new_ssa non_kernel_array
    in
    List.fold_left map_succ (is_trace,v::state_list) vl
  in
  
  let state_list, state_ind, is_trace =
    match is, lit_trans with
    | (Some is), (Some lt) ->
        let lt = match lt with
        | Ps_Ter t -> Ps_Ter t
        | Ps_Non_ter nt -> Ps_Non_ter nt_subst.(0).(nt)
        in
        new_start_state is lt subst_rn_red subst_rn nt_nb is_trace state_list
        gram_rhs gram_lhs r_L array_nt_prio lhslists priodata non_kernel_array
        map_succ
    | _ -> state_list, None, is_trace
  in
  
  (*let time6 = Sys.time () in
  Printf.printf "state_list = %.3f s\n" (time6-.time5);*)
  
  let entry_points_list =
    Hashtbl.fold (fun nt _ l -> nt_subst.(0).(nt)::l)
    (List.hd pdev_list).entry_points []
  in
  let entry_points = Hashtbl.create (List.length entry_points_list) in
  
  let state_list, is_trace =
    
    List.fold_left
    (fun (state_list, is_trace) ep ->
      let lhs_ind_set =
        Prio_map.fold
        (fun p lhs_ind set -> Int_set.add lhs_ind set)
        array_nt_prio.(ep) Int_set.empty
      in
      if Int_set.cardinal lhs_ind_set = 1 then
        match stations.(Int_set.choose lhs_ind_set) with Some s ->
        (Hashtbl.add entry_points ep s;
        state_list, is_trace)
        | None -> assert false
      else
        let s_list =
          Int_set.fold
            (fun i l -> match stations.(i) with Some v ->
              v::l | None -> assert false)
            lhs_ind_set []
        in
        let state_list_result, source_to_clear, color_black_to_clear =
          epsilon_closure s_list old_stations component predict
          lhs_nb pnb true lhs_station count_es source color_black result
        in
        List.iter (fun i -> source.(i) <- -1) source_to_clear;
        List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
        List.iter (fun i -> result.(i.number) <- false) state_list_result;
        let state = merge_states state_list_result (Ps_Ter (-1)) succ_states_array prd_nb non_kernel_array in
        let is_trace, state_list = map_succ (is_trace,state_list) state in
        Hashtbl.add entry_points ep state;
        state::state_list, is_trace)
        (* is it really necessary to have state in state_list ? *)
    (state_list, is_trace) entry_points_list
  in
  (*
  let time7 = Sys.time () in
  Printf.printf "entry_points_list = %.3f s\n" (time7-.time6);*)
  
  (*Printf.fprintf !log_channel "\nstate_list:\n";
  List.iter
  (fun s ->
    Printf.fprintf !log_channel "\n";
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      E.str_token_name priodata.prd_names)
  state_list;
  
  print_stations stations gram_rhs lhs_table str_non_ter nt_of_ind prio_of_ind priodata;*)
  
  let table, table_it, table_lit_trans(*, table_lex_trans*) =
    make_table state_list (!countst) (Array.length gram_lhs) gram_rhs
      lhs_table nt_of_ind prio_of_ind array_nt_prio ppar token_nb
  in
  
  let pdev = List.hd pdev_list in
  
  let rn_of_rule =
    make_rn_of_rule gram_rhs allow_layout lhs_table str_non_ter
    priodata.prd_names pdev.str_ter pdev.regexp_array in
  
  (*let time2 = Sys.time () in
  Printf.printf "table = %.3f s\n" (time2-.time7);
  Printf.printf "merge_parsing_device = %.3f s\n" (time2-.time0);*)
  
  (*let time1 = Sys.time () in
  List.iter merge_states_prof (!merge_states_ref);
  let time2 = Sys.time () in
  Printf.printf "merge_states = %.3f s\n" (time2-.time1);*)
  
  (*let time_em1 = Sys.time () in
  let em_prof _ s =
          em_arg_ref := s::!em_arg_ref;
          Succ_states.fold
          (fun (t,p) el -> match t.li with
            | Ps_Ter ter ->
                if edge_array.(symb_nb*p+ter) = [] then
                  (edge_array.(symb_nb*p+ter) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+ter) <- t::edge_array.(symb_nb*p+ter); el)
            | Ps_Non_ter nt ->
                if edge_array.(symb_nb*p+E.token_nb+nt) = [] then
                  (edge_array.(symb_nb*p+E.token_nb+nt) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+E.token_nb+nt)
                   <- t::edge_array.(symb_nb*p+E.token_nb+nt); el))
          s.succ_states []
  in
  let _ = List.fold_left em_prof [] !em_arg_ref in
  let time_em2 = Sys.time () in
  Printf.printf "em_prof = %.3f s\n"
    (time_em2-.time_em1);*)
  
  (*let epsilon_closure_prof (s_list, old_stations, component, predict,
    lhs_nb, pnb, lhs_station, count_es) =
    let state_list_result, source_to_clear, color_black_to_clear =
      epsilon_closure s_list old_stations component predict
      lhs_nb pnb false lhs_station count_es source color_black result
    in
    List.iter (fun i -> source.(i) <- -1) source_to_clear;
    List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
    List.iter (fun i -> result.(i.number) <- false) state_list_result;
  in
  let time_prof1 = Sys.time () in
  List.iter epsilon_closure_prof !epsilon_closure_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "epsilon_closure_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !epsilon_closure_arg);*)
  
  (*let block_A_prof (key,ist,p,new_succ_states) =
    try let succ_state = Map_is.find key ist in
      let _ = Succ_states.add (succ_state,p) new_succ_states in ()
    with Not_found -> ()
  in
  let time_prof1 = Sys.time () in
  List.iter block_A_prof !bloc_A_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "block_A_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !bloc_A_arg);
  
  let block_B_prof (key,state,ist,p,new_succ_states) =
    let _ = Map_is.add key state ist in
    let _ = Succ_states.add (state,p) new_succ_states in ()
  in
  let time_prof1 = Sys.time () in
  List.iter block_B_prof !bloc_B_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "block_B_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !bloc_B_arg);*)
  
  (*let dec_table, final, build_nfa_table, start = make_lexer regexp_list in*)
  
  { gram_lhs = gram_lhs;
    gram_rhs = gram_rhs;
    lhs_table = lhs_table;
    actions = actions;
    entry_point = entry_point;
    entry_points = entry_points;
    g_nb = g_nb;
    lex_nb = pdev.lex_nb;
    nt_table = nt_table;
    stations = stations;
    state_list = state_list;
    is_trace = is_trace;
    array_nt_prio = array_nt_prio;
    st_nb = !countst;
    table = table;
    table_it = table_it;
    (*table_lex_trans = table_lex_trans;*)
    table_lit_trans = table_lit_trans;
    lhslists = lhslists;
    r_L = r_L;
    po = po_array;
    data = newdata;
    prio = priodata;
    loc_data = newlocal_data;
    nt_nb = nt_nb;
    token_nb = token_nb;
    prio_of_ind = prio_of_ind;
    nt_of_ind = nt_of_ind;
    str_non_ter = str_non_ter;
    cons_of_nt = cons_of_nt;
    relations = relations;
    nt_cons_map = nt_cons_map;
    rn_of_rule = rn_of_rule;
    regexp_decl = pdev.regexp_decl;
    main_lexer_start = pdev.main_lexer_start;
    main_lexer_table = pdev.main_lexer_table;
    main_lexer_actions = pdev.main_lexer_actions;
    aux_lexer = {
      aux_lexer_start = pdev.aux_lexer.aux_lexer_start;
      aux_lexer_table = pdev.aux_lexer.aux_lexer_table;
      aux_lexer_actions = pdev.aux_lexer.aux_lexer_actions };
    str_ter = pdev.str_ter;
    ter_table = pdev.ter_table;
    layout_id = pdev.layout_id;
    regexp_table = pdev.regexp_table;
    regexp_array = pdev.regexp_array;
    allow_layout = allow_layout },
  state_ind*)



let make_key_single is0 gram_rhs =
  let is = new_item_set () in
  is.reducible <- Int_set.union is.reducible
    (Int_set.filter
    (fun rn -> Array.length gram_rhs.(rn)>0)
    is0.reducible);
  is.kernel_t <- Intc_set.union is.kernel_t is0.kernel_t;
  is.kernel_nt <- Intc_set.union is.kernel_nt is0.kernel_nt;
  is,
  (Int_set.cardinal is.reducible) +
  (Intc_set.cardinal is.kernel_t) +
  (Intc_set.cardinal is.kernel_nt)



let rule_of_nrule (gram_rhs:rhs array) rule_options lhs_table str_non_ter prio_names str_ter regexp_array rn =
  (*Printf.fprintf !log_channel "Array.length gram_rhs=%d, rn=%d\n"
  (Array.length gram_rhs) rn;*)
  let len = Array.length gram_rhs.(rn) in
  let rec f i l =
    if i= -1 then l else
    f (i-1) ((lit_of_nlit str_non_ter prio_names str_ter
      regexp_array gram_rhs.(rn).(i))::l)
  in
  let litl = f (len-1) [] in
  let nt,p,_ = lhs_table.(rn) in
  let rol = match rule_options.(rn) with
    | 3 -> []
    | 2 -> nl_inside
    | 1 -> nl_follows
    | 0 -> nl_inside_nl_follows
    | _ -> assert false
  in
  (str_non_ter.(nt),litl,prio_names.(p),rol)



let extend_parsing_device pdev pl is lit_trans ppar =
  (*if pdev.rule_subst = [||] then print_endline "pdev.rule_subst = [||]";*)
  let f =
    rule_of_nrule pl.gram_rhs pl.rule_options pl.lhs_table pl.str_non_ter
      pl.prio.prd_names pl.str_ter pl.regexp_array
  in
  is.reducible <- Int_set.fold (fun rn red ->
    let rn = Hashtbl.find pdev.rn_of_rule (f rn) in
    Int_set.add rn red) is.reducible Int_set.empty;
  is.kernel_t <- Intc_set.fold (fun (rn,x) k ->
    let rn = Hashtbl.find pdev.rn_of_rule (f rn) in
    Intc_set.add (rn,x) k) is.kernel_t Intc_set.empty;
  is.kernel_nt <- Intc_set.fold (fun (rn,x) k ->
    let rn = Hashtbl.find pdev.rn_of_rule (f rn) in
    Intc_set.add (rn,x) k) is.kernel_nt Intc_set.empty;
  let is, inb = make_key_single is pdev.gram_rhs in
  let key = is, inb in
  (*Printf.fprintf !log_channel "extend_parsing_device key is:\n";
  print_item_set !log_channel is pdev.gram_rhs pdev.lhs_table
    pdev.nt_of_ind pdev.prio_of_ind pdev.str_non_ter
    str_token_name pdev.prio.prd_names;*)
    
  let lit_trans = match lit_trans with
    | Ps_Non_ter nt ->
        Ps_Non_ter (Hashtbl.find pdev.nt_table (pl.str_non_ter.(nt)))
    | Ps_Ter t -> Ps_Ter t
    | Ps_Non_ter_NL nt ->
        Ps_Non_ter_NL (Hashtbl.find pdev.nt_table (pl.str_non_ter.(nt)))
    | Ps_Ter_NL t -> Ps_Ter_NL t
  in
  try
    (* ??? bug fst <-> snd ?
    let s = match lit_trans with
      | Ps_Non_ter nt -> Map_is.find key (fst pdev.is_trace).(nt)
      | Ps_Ter t -> Map_is.find key (snd pdev.is_trace).(t)
    in*)
    let s = match lit_trans with
      | Ps_Non_ter nt -> Map_is.find key (fst (snd pdev.is_trace)).(nt)
      | Ps_Ter t -> Map_is.find key (fst (fst pdev.is_trace)).(t)
      | Ps_Non_ter_NL nt -> Map_is.find key (snd (snd pdev.is_trace)).(nt)
      | Ps_Ter_NL t -> Map_is.find key (snd (fst pdev.is_trace)).(t)
    in
    s.number, pdev
  with Not_found ->
    let array_lt_ter = Array.make pdev.token_nb None in
    let array_lt_ter_nl = Array.make pdev.token_nb None in
    (*let splitting_ter = Array.make pdev.token_nb false in*)
    let nt_nb = Array.length pdev.gram_lhs in
    let array_lt_nt = Array.make nt_nb None in
    let array_lt_nt_nl = Array.make nt_nb None in
    (*let splitting_nt = Array.make (Array.length pdev.str_non_ter) false in*)
    
    (*Gc.set { (Gc.get()) with Gc.space_overhead = 100 };*)
    
    let array_lhs = Array.make nt_nb false in
    let succ_states_array =
      ref (Array.make (2*(Array.length pdev.table_it)*pdev.prio.prd_nb) false)
    in
    let non_kernel_array = Array.make (Array.length pdev.gram_rhs) false in
    
    let rec map_succ (is_trace,state_list) v =
      let vl =
        (*Printf.fprintf !log_channel "-state built-\n";
          print_state v pdev.gram_rhs pdev.lhs_table pdev.nt_of_ind pdev.prio_of_ind pdev.str_non_ter
          str_token_name pdev.prio.prd_names;
        Printf.fprintf !log_channel "\n";
        flush_all ();*)
        move_LR0 v is_trace pdev.gram_rhs pdev.gram_lhs pdev.gram_parnt
        pdev.bnt_array pdev.r_L pdev.prio
        pdev.array_nt_prio pdev.lhslists array_lt_ter array_lt_ter_nl
        array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs
        succ_states_array non_kernel_array
      in
      List.fold_left map_succ (is_trace,v::state_list) vl
    in
    let nt_to_add =
      let aux1 (rn,dp) (nt_to_add,nk) =
        (*is.non_kernel <- Int_set.add rn is.non_kernel;*)
        try (match pdev.gram_rhs.(rn).(dp) with
          | Ps_Non_ter nt | Ps_Non_ter_NL nt ->
              (let lhs_l =
                comp_lhslist nt pdev.lhslists pdev.prio pdev.array_nt_prio
              in
              (*Printf.fprintf !log_channel "List.length lhs_l=%d\n" (List.length lhs_l);*)
              let g1 nt_to_add ind =
                Int_set.add ind nt_to_add
              in
              let g2 nt_to_add (_,_,ind) =
              let ind_list = pdev.r_L.(ind) in
              (*Printf.fprintf !log_channel "List.length ind_list=%d\n" (List.length ind_list);
              List.iter (fun i -> Printf.fprintf !log_channel "%d " i) ind_list;
              Printf.fprintf !log_channel "\n";*)
              List.fold_left g1 nt_to_add ind_list
              in
              (List.fold_left g2 nt_to_add lhs_l),
              if non_kernel_array.(rn) then nk
              else (non_kernel_array.(rn) <- true; rn::nk))
          | _ -> assert false)
        with Invalid_argument _ -> assert false
      in
      let nt_to_add, nk =
        Intc_set.fold aux1 is.kernel_nt (Int_set.empty,[])
      in
      is.non_kernel <- nk;
      (*List.iter (fun rn -> non_kernel_array.(rn) <- false) nk;*)
      (* closure_v0_LR0 will clear the array *)
      nt_to_add
      (*let aux2 (rn,dp) nt_to_add = aux1 dp rn nt_to_add in
      let nt_to_add = Intc_set.fold aux2 is.kernel_nt Int_set.empty in
      Int_set.fold (aux1 0) is.non_kernel nt_to_add*)
    in
    
    closure_v0_LR0 is pdev.gram_rhs pdev.gram_lhs nt_to_add non_kernel_array;
    countst := pdev.st_nb+1;
    let predict =
      List.fold_left
      (fun predict rn -> try (match pdev.gram_rhs.(rn).(0) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt -> Predict.add nt predict
        | _ -> predict)
        with Invalid_argument _ -> assert false)
      Predict.empty is.non_kernel
    in
    
    let predict =
      Intc_set.fold
      (fun (rn,dp) predict -> try (match pdev.gram_rhs.(rn).(dp) with
        | Ps_Non_ter nt | Ps_Non_ter_NL nt -> Predict.add nt predict
        | _ -> assert false)
        with Invalid_argument _ -> assert false)
      is.kernel_nt predict
    in
    
    is.predict <- predict;
    let v = {
      li = lit_trans;
      items = is;
      number = !countst;
      mergeable = is_mergeable is pdev.gram_parnt;
      bestowing_nt = make_bestowing_nt is pdev.gram_parnt;
      succ_states = [] }
    in
    let v_rightSib = !countst in
    incr countst;
    let is_trace, state_list = map_succ (pdev.is_trace,pdev.state_list) v in
    let table, table_it, table_lit_trans, state_bnt, state_is_mergeable =
      make_table state_list (!countst) (Array.length pdev.gram_lhs)
      pdev.gram_rhs pdev.lhs_table pdev.nt_of_ind pdev.prio_of_ind
      pdev.array_nt_prio ppar pdev.token_nb pdev.str_ter
      pdev.str_non_ter pdev.prio.prd_names pdev.regexp_array
    in
    
    let pdev_rightSib = { pdev with
      table = table;
      table_it = table_it;
      table_lit_trans = table_lit_trans;
      state_bnt = state_bnt;
      state_is_mergeable = state_is_mergeable;
      is_trace = is_trace;
      state_list = state_list;
      st_nb = !countst }
    in
    
    v_rightSib, pdev_rightSib




let update_parsing_device ppar pdev_leftSib new_relations rapf_add newdata newlocal_data new_nt_cons counters start_is lit_trans =
  let time1 = Sys.time () in
  let new_nt_cons =
    List.map
    (fun (nt,cons) -> (nt,Hashtbl.find ppar.cons_table cons))
    new_nt_cons
  in
  let nt_cons_map =
    List.fold_left
    (fun m (nt,cons) ->
      try
        let prev_cons = String_map.find nt m in
        if prev_cons = cons then m else
        raise (Constructor_mismatch
          (ppar.cons_str.(prev_cons),ppar.cons_str.(cons)))
      with Not_found -> String_map.add nt cons m)
    pdev_leftSib.nt_cons_map new_nt_cons
  in
  let relations = new_relations@pdev_leftSib.relations in
  
  (*let interm_pdev =
    create_interm_pdev rapf_add relations
    newdata newlocal_data nt_cons_map [] ppar [] ([],[||]) []
    pdev_leftSib.token_nb pdev_leftSib.str_ter pdev_leftSib.ter_table
    pdev_leftSib
  in*)
  
  let interm_pdev =
    let rapf_list, global_data, local_data,
      entry_point_list, regexp_decl, main_lexer, aux_lexer,
      (*token_nb,*) str_ter, ter_table, pdev =
    rapf_add, newdata, newlocal_data, [],
    [], ([],[||]), [], (*pdev_leftSib.token_nb,*)
      pdev_leftSib.str_ter, pdev_leftSib.ter_table, pdev_leftSib in
    let gram_lhs, gram_rhs, lhs_table, actions, inherited, nt_nb,
      po_array, user_g, array_nt_prio, nt_of_ind,
      prio_of_ind, nt_table, str_non_ter, priority_data, rn_of_rule,
      regexp_table, regexp_list, regexp_actions, regexp_array,
      rule_options, lhslists, cyclic_rules, gram_parnt, bnt_array =
      make_grammar rapf_list relations ppar ter_table str_ter ppar.regexp_fun
      (*token_nb*) pdev_leftSib.regexp_table
    in
  
    (*Printf.printf "regexp_array len=%d\n" (Array.length regexp_array);*)
  
    let main_table, main_start, main_lexer_actions, lex_nb =
      match regexp_list with
      | [], [] ->
        pdev.main_lexer_table, pdev.main_lexer_start,
        pdev.main_lexer_actions, pdev.lex_nb
      | _ ->
      let prev_mla_nb = Array.length pdev_leftSib.main_lexer_actions in
      let main_table, main_start =
        let node_nb =
          (Array.length pdev_leftSib.main_lexer_table.tbl_trans)/257 in
        extend_lexer pdev_leftSib.main_lexer_start regexp_list
        pdev_leftSib.regexp_decl node_nb prev_mla_nb in
  
      let main_lexer_actions = Array.make
        (prev_mla_nb+(List.length (fst regexp_list))+
        (List.length (snd regexp_list)))
        (0,ppar.regexp_fun) in
      let i = List.fold_left
        (fun i x -> main_lexer_actions.(i) <- x; i+1) 0
        (fst regexp_actions) in
      let arr = pdev_leftSib.main_lexer_actions in
      for j=0 to prev_mla_nb-1 do
        main_lexer_actions.(j+i) <- arr.(j)
      done;
      let _ = List.fold_left
        (fun i x -> main_lexer_actions.(i) <- x; i+1) (i+prev_mla_nb)
        (snd regexp_actions) in
      counters.count_lex <- counters.count_lex + 1;
      main_table, main_start, main_lexer_actions, counters.count_lex
    in
  
    (*Printf.printf "main_lexer_actions:\n";
    for i=0 to Array.length main_lexer_actions -1 do
      Printf.printf "%d -> %d\n" i (fst main_lexer_actions.(i))
    done;
    print_newline ();*)
  
    (*let regexp_nb = (List.length (fst regexp_list)) +
      (List.length (snd regexp_list)) in
    let new_token_nb = token_nb + regexp_nb in*)
    let new_token_nb = (Hashtbl.length ter_table) +
      (Array.length regexp_array) in
    let entry_point_list =
      List.map (fun ep -> Hashtbl.find nt_table ep) entry_point_list
    in
    let cons_of_nt = Array.make (Array.length str_non_ter) 0 in
    let f nt cons =
      try
        let i = Hashtbl.find nt_table nt in
        cons_of_nt.(i) <- cons
      with Not_found -> ()
      (* nt_cons_map may contains non terminals that are not in any rules
      in the case of addition of new rules at parsing time one uses
      create_parsing_device just for the set of new rules but one reuses
      the nt_cons_map of the previous grammar with the possible addition
      of new pairs (nt,cons) for new non terminals. *)
    in
    String_map.iter f nt_cons_map;
    (*let lhslists = Array.make nt_nb Ntp_map.empty in*)
  
    let time1 = Sys.time () in
    let _, r_L = compute_L gram_rhs gram_lhs lhslists
      priority_data array_nt_prio in
    let time2 = Sys.time () in
    if !dypgen_verbose>1 then
      Printf.fprintf !log_channel "r_L computed, %.3f sec\n"
        (time2-.time1);
    let state_list, n, stations, entry_points, is_trace =
      create_aut gram_rhs gram_lhs gram_parnt bnt_array r_L
      priority_data 0 array_nt_prio nt_of_ind prio_of_ind nt_nb
      lhslists lhs_table str_non_ter entry_point_list ppar new_token_nb
      str_ter regexp_array
    in
  
    (*print_gram_lhs gram_lhs lhs_of_ind str_non_ter;*)
  
    let table, table_it, table_lit_trans, state_bnt, state_is_mergeable =
      make_table state_list n (Array.length gram_lhs) gram_rhs
        lhs_table nt_of_ind prio_of_ind array_nt_prio ppar new_token_nb
        str_ter str_non_ter priority_data.prd_names regexp_array
    in
    (*let rnb = Array.length gram_rhs in
    let actions = Array.make rnb [] in
    List.iter (fun (r,a) ->
      let i = Hashtbl.find rn_of_rule r in
      actions.(i) <- a::actions.(i))
    rapf_list;*)
  
  { gram_rhs = gram_rhs ;
    gram_lhs = gram_lhs ;
    gram_parnt = gram_parnt ;
    bnt_array = bnt_array ;
    (*dypgen_epsilon =
      (try Hashtbl.find nt_table "dypgen__epsilon"
      with Not_found -> -1);*)
    lhs_table = lhs_table ;
    entry_points = entry_points ;
    g_nb = 0;
    lex_nb = lex_nb;
    nt_table = nt_table ;
    stations = stations ;
    state_list = state_list ;
    is_trace = is_trace;
    array_nt_prio = array_nt_prio;
    st_nb = n;
    table = table ;
    state_bnt = state_bnt ;
    state_is_mergeable = state_is_mergeable ;
    table_it = table_it ;
    (*table_lex_trans = table_lex_trans ;*)
    table_lit_trans = table_lit_trans ;
    lhslists = lhslists ;
    r_L = r_L ;
    po = po_array ;
    cyclic_rules = cyclic_rules ;
    data = global_data;
    loc_data = local_data ;
    prio = priority_data ;
    nt_nb = Array.length gram_lhs;
    token_nb = new_token_nb;
    prio_of_ind = prio_of_ind;
    nt_of_ind = nt_of_ind;
    str_non_ter = str_non_ter;
    cons_of_nt = cons_of_nt;
    relations = relations;
    nt_cons_map = nt_cons_map;
    rn_of_rule = rn_of_rule;
    entry_point = 0;
    actions = actions;
    inherited = inherited;
    regexp_decl = pdev.regexp_decl;
    main_lexer_start = main_start;
    main_lexer_table = main_table;
    main_lexer_actions = main_lexer_actions;
    aux_lexer = pdev.aux_lexer;
    str_ter = pdev.str_ter;
    ter_table = pdev.ter_table;
    layout_id = pdev.layout_id;
    regexp_table = pdev.regexp_table;
    regexp_array = regexp_array;
    rule_options = rule_options }
  in
  
  (*let pdev_rightSib, start_state_ind =
    (*let is = pdev_leftSib.table_it.(v_rightSib) in
    let lit_trans = pdev_leftSib.table_lit_trans.(v_rightSib) in*)
    merge_parsing_device [pdev_leftSib;interm_pdev]
      newdata newlocal_data lit_trans (counters.count_g+1)
      start_is interm_pdev.nt_cons_map interm_pdev.relations ppar
  in*)
  
  let pdev_rightSib, start_state_ind =
    
    let pdev_list = [pdev_leftSib;interm_pdev] in
    (*Printf.printf "pdev_leftSib.token_nb = %d, interm_pdev.token_nb = %d\n"
      pdev_leftSib.token_nb interm_pdev.token_nb;*)
    let g_nb, is, nt_cons_map, relations =
      (counters.count_g+1), start_is, interm_pdev.nt_cons_map,
      interm_pdev.relations in
    
    let token_nb = interm_pdev.token_nb in
  
  (*let time0 = Sys.time () in*)
  
  let nt_table, nt_of_ind, prio_of_ind, array_nt_prio, gram_rhs, gram_parnt,
    bnt_array, lhs_table, gram_lhs, actions, inherited, old_stations,
    str_non_ter, priodata, cons_of_nt, po_array, nt_nb, lhs_nb, pnb,
    rule_subst, nt_subst, (*user_g,*) lhslists, (*predict,*) lhs_station,
    old_is_trace, state_lists, st_nb, ind_subst, non_kernel_array,
    rule_options, cyclic_rules =
    compose_grammar pdev_list
  in
  
  (*Printf.fprintf !log_channel "print_grammar:\n";
  print_grammar !log_channel gram_rhs lhs_table str_non_ter priodata.prd_names;
  Printf.fprintf !log_channel "\nprint_gram_lhs\n";
  print_gram_lhs !log_channel gram_lhs nt_of_ind prio_of_ind str_non_ter
    priodata.prd_names;*)
  
  (*Printf.fprintf !log_channel "\nstate_lists.(0):\n";
  List.iter
  (fun s ->
    Printf.fprintf !log_channel "\n";
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      E.str_token_name priodata.prd_names)
  state_lists.(0);*)
  
  (*let time2 = Sys.time () in
  Printf.printf "compose_grammar = %.3f s\n" (time2-.time0);*)
  
  (*merge_states_ref := [];*)
  
  let _, r_L = compute_L gram_rhs gram_lhs lhslists
    priodata array_nt_prio
     (*str_non_ter priodata.prd_names nt_of_ind prio_of_ind*)
  in
  
  (*let time1 = Sys.time () in
  Printf.printf "compute_L = %.3f s\n" (time1-.time2);*)
  
  let prd_nb = priodata.prd_nb in
  let succ_states_array = ref (Array.make (2*st_nb*prd_nb) false) in
  
  consider_priorities pdev_list state_lists old_is_trace priodata
    array_nt_prio lhslists r_L st_nb ind_subst
    (diff_lhslist nt_of_ind prio_of_ind str_non_ter) gram_rhs gram_lhs
    gram_parnt bnt_array lhs_table nt_of_ind prio_of_ind str_non_ter
    succ_states_array non_kernel_array;
  
  (*let time2 = Sys.time () in
  Printf.printf "consider_priorities = %.3f s\n" (time2-.time1);*)
  
  let st_nb = !countst in
  
  let predict = Array.make st_nb Int_set.empty in
  let component = Array.make st_nb (-1) in
  
  let make_predict_array nt set =
    (*Printf.fprintf !log_channel "make_predict nt=%s, ntp=%s\n"
      str_non_ter.(fst nt) (str_nt_prio (snd nt) priodata.prd_names);*)
    let lhs_l = comp_lhslist nt lhslists priodata array_nt_prio in
    List.fold_left
      (fun set (_,_,i) ->
        (*Printf.fprintf !log_channel "add: nt=%d, p=%d, i=%d, %s\n" nt p i
        (str_lhs (nt,p,i) str_non_ter priodata.prd_names);*)
        Int_set.add i set)
      set lhs_l
  in
  
  for i=0 to pnb-1 do
    List.iter (fun s ->
      predict.(s.number) <-
        Predict.fold make_predict_array s.items.predict Int_set.empty;
      component.(s.number) <- i)
      state_lists.(i)
  done;
  
  (*let time1 = Sys.time () in
  Printf.printf "make component predict = %.3f s\n" (time1-.time2);*)
  
  (*let fst_pdev = List.hd pdev_list in
  
  if fst_pdev.ind_subst_inv <> [||] then
    for i=0 to lhs_nb-1 do
      ind_subst_inv.(i) <- fst_pdev.ind_subst_inv.(ind_subst_inv.(i))
    done*)
  
  let entry_point =
    let pdev = List.hd pdev_list in
    nt_subst.(0).(pdev.entry_point)
  in
  
  let is_trace =
    ((Array.make token_nb Map_is.empty),
    (Array.make token_nb Map_is.empty)),
    ((Array.make nt_nb Map_is.empty),
    (Array.make nt_nb Map_is.empty))
  in
  (*Printf.printf "lhs_nb = %d\n" lhs_nb;*)
  let stations = Array.make lhs_nb None in
  
  let subst_rn is =
    let aux (rn,x) set = Intc_set.add (rule_subst.(0).(rn),x) set in
    Intc_set.fold aux is Intc_set.empty
  in
  let subst_rn_red is =
    let aux rn set = Int_set.add rule_subst.(0).(rn) set in
    Int_set.fold aux is Int_set.empty
  in
  
  countst := 0;
  let count_es = ref (Array.length component) in
  
  (*let time3 = Sys.time () in*)
  
  let source = Array.make (st_nb+lhs_nb*pnb) (-1) in
  (* the +lhs_nb*pnb is here for the extra stations *)
  let color_black = Array.make (st_nb+lhs_nb*pnb) false in
  let result = Array.make (st_nb+lhs_nb*pnb) false in
  let symb_nb = 2*nt_nb+2*token_nb in
  let edge_array = Array.make (symb_nb*prd_nb) [] in
  let new_ssa = ref (Array.make (2*st_nb*prd_nb) false) in
  
  let working_list =
    let rec g i wl =
      if i=lhs_nb then wl else
      let s_list =
        let rec f n s_list =
          if n=pnb then s_list else
          match old_stations.(n*lhs_nb+i) with
          | Some_station s -> f (n+1) (s::s_list)
          | _ -> f (n+1) s_list
        in f 0 []
      in
      let state_list_result, source_to_clear, color_black_to_clear =
        epsilon_closure s_list old_stations component predict
        lhs_nb pnb true lhs_station count_es source color_black result
      in
      List.iter (fun i -> source.(i) <- -1) source_to_clear;
      List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
      List.iter (fun i -> result.(i.number) <- false) state_list_result;
      let state =
        merge_states state_list_result (Ps_Ter (-1)) succ_states_array
        prd_nb non_kernel_array in
      stations.(i) <- Some state;
      g (i+1) (state::wl)
    in g 0 []
  in
  
  (*let time4 = Sys.time () in
  Printf.printf "working_list = %.3f s\n" (time4-.time3);*)
  
  (*let em_arg_ref = ref [] in*)
  (*let bloc_A_arg = ref [] in
  let bloc_B_arg = ref [] in*)
  (*let epsilon_closure_arg = ref [] in*)
  
  let make_succ_states li p (state_list, new_wl, new_ss_list) s_list =
            let edge_array_ind = match li with
              | Ps_Ter t -> symb_nb*p+t
              | Ps_Non_ter nt -> symb_nb*p+2*token_nb+nt
              | Ps_Ter_NL t -> symb_nb*p+token_nb+t
              | Ps_Non_ter_NL nt -> symb_nb*p+2*token_nb+nt_nb+nt
            in
            let s_list = edge_array.(edge_array_ind) in
            edge_array.(edge_array_ind) <- [];
            let key = make_key s_list gram_rhs in
            try
              let succ_state = match li with
                | Ps_Ter t ->
                    Map_is.find key (fst (fst is_trace)).(t)
                | Ps_Non_ter nt ->
                    Map_is.find key (fst (snd is_trace)).(nt)
                | Ps_Ter_NL t -> Map_is.find key (snd (fst is_trace)).(t)
                | Ps_Non_ter_NL nt ->
                    Map_is.find key (snd (snd is_trace)).(nt)
              in
              let new_ss_list =
                if ga_get new_ssa (succ_state.number*prd_nb+p) then new_ss_list
                else (ga_set new_ssa (succ_state.number*prd_nb+p) true;
                  (succ_state,p)::new_ss_list)
              in
              (state_list, new_wl, new_ss_list)
            with Not_found ->
              let state_list_result, source_to_clear, color_black_to_clear =
                epsilon_closure s_list old_stations component predict
                lhs_nb pnb false lhs_station count_es source color_black result
              in
              List.iter (fun i -> source.(i) <- -1) source_to_clear;
              List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
              List.iter (fun i -> result.(i.number) <- false) state_list_result;
              let state =
                merge_states state_list_result li succ_states_array prd_nb
                non_kernel_array in
              state.items.kernel_t <- (fst key).kernel_t;
              state.items.kernel_nt <- (fst key).kernel_nt;
              (match li with
                | Ps_Ter t ->
                    (fst (fst is_trace)).(t) <-
                     Map_is.add key state (fst (fst is_trace)).(t)
                | Ps_Non_ter nt ->
                    (fst (snd is_trace)).(nt) <-
                    Map_is.add key state (fst (snd is_trace)).(nt)
                | Ps_Ter_NL t ->
                    (snd (fst is_trace)).(t) <-
                     Map_is.add key state (snd (fst is_trace)).(t)
                | Ps_Non_ter_NL nt ->
                    (snd (snd is_trace)).(nt) <-
                    Map_is.add key state (snd (snd is_trace)).(nt));
              ga_set new_ssa (state.number*prd_nb+p) true;
              (state::state_list,state::new_wl,(state,p)::new_ss_list)
  in
  
  let rec process_state wl state_list =
    match wl with [] -> state_list | _ ->
    let state_list, new_wl =
      List.fold_left
      (fun (state_list, new_wl) s ->
        
        let el =
          (*em_arg_ref := s::!em_arg_ref;*)
          List.fold_left
          (fun el (t,p) -> match t.li with
            | Ps_Ter ter ->
                if edge_array.(symb_nb*p+ter) = [] then
                  (edge_array.(symb_nb*p+ter) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+ter) <- t::edge_array.(symb_nb*p+ter); el)
            | Ps_Non_ter nt ->
                if edge_array.(symb_nb*p+2*token_nb+nt) = [] then
                  (edge_array.(symb_nb*p+2*token_nb+nt) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+2*token_nb+nt)
                   <- t::edge_array.(symb_nb*p+2*token_nb+nt); el)
            | Ps_Ter_NL ter ->
                if edge_array.(symb_nb*p+token_nb+ter) = [] then
                  (edge_array.(symb_nb*p+token_nb+ter) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+token_nb+ter) <-
                  t::edge_array.(symb_nb*p+token_nb+ter);
                  el)
            | Ps_Non_ter_NL nt ->
                if edge_array.(symb_nb*p+2*token_nb+nt_nb+nt) = [] then
                  (edge_array.(symb_nb*p+2*token_nb+nt_nb+nt) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+2*token_nb+nt_nb+nt)
                   <- t::edge_array.(symb_nb*p+2*token_nb+nt_nb+nt); el))
          [] s.succ_states
        in
        
        let (state_list, new_wl, new_ss_list) =
          List.fold_left
          (fun (state_list, new_wl, new_ss_list) (li, p) ->
            
            let edge_array_ind = match li with
              | Ps_Ter t -> symb_nb*p+t
              | Ps_Non_ter nt -> symb_nb*p+2*token_nb+nt
              | Ps_Ter_NL t -> symb_nb*p+token_nb+t
              | Ps_Non_ter_NL nt ->
                  symb_nb*p+2*token_nb+nt_nb+nt
            in
            let s_list = edge_array.(edge_array_ind) in
            edge_array.(edge_array_ind) <- [];
            
            let s_ll =
              if list_exists
                (fun s -> not (Int_set.is_empty s.bestowing_nt)) s_list
              then List.map (fun x -> [x]) s_list else [s_list] in
            
            List.fold_left (make_succ_states li p)
              (state_list, new_wl, new_ss_list) s_ll
            
            (*let key = make_key s_list gram_rhs in
            (*Printf.fprintf !log_stack_channel "\nkey is:\n";
            print_item_set !log_stack_channel (fst key) gram_rhs lhs_table
              nt_of_ind prio_of_ind str_non_ter str_token_name priodata.prd_names;
            Printf.fprintf !log_stack_channel "fin key is\n";*)
            try (* bloc_A_prof *)
              let succ_state = match li with
                | Ps_Ter t ->
                    (*bloc_A_arg :=
                      (key,(fst is_trace).(t),p,succ_state)::!bloc_A_arg;*)
                    Map_is.find key (fst (fst is_trace)).(t)
                | Ps_Non_ter nt ->
                    (*bloc_A_arg :=
                      (key,(snd is_trace).(nt),p,succ_state)::!bloc_A_arg;*)
                    Map_is.find key (fst (snd is_trace)).(nt)
                | Ps_Ter_NL t -> Map_is.find key (snd (fst is_trace)).(t)
                | Ps_Non_ter_NL nt ->
                    Map_is.find key (snd (snd is_trace)).(nt)
              in
              let new_ss_list =
                if ga_get new_ssa (succ_state.number*prd_nb+p) then new_ss_list
                else (ga_set new_ssa (succ_state.number*prd_nb+p) true;
                  (succ_state,p)::new_ss_list)
              in
              (state_list, new_wl, new_ss_list)
            with Not_found ->
              (*let li = (State_set.choose state_set).li in*)
              let state_list_result, source_to_clear, color_black_to_clear =
                epsilon_closure s_list old_stations component predict
                lhs_nb pnb false lhs_station count_es source color_black result
              in
              List.iter (fun i -> source.(i) <- -1) source_to_clear;
              List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
              List.iter (fun i -> result.(i.number) <- false) state_list_result;
              (*epsilon_closure_arg := (s_list, old_stations, component, predict,
                lhs_nb, pnb, lhs_station, count_es)::!epsilon_closure_arg;*)
              let state =
                merge_states state_list_result li succ_states_array prd_nb
                non_kernel_array in
              state.items.kernel_t <- (fst key).kernel_t;
              state.items.kernel_nt <- (fst key).kernel_nt;
              (*Printf.fprintf !log_stack_channel "\nstate is:\n";
              print_item_set !log_stack_channel state.items gram_rhs lhs_table
                nt_of_ind prio_of_ind str_non_ter str_token_name priodata.prd_names;
              Printf.fprintf !log_stack_channel "fin state is\n";*)
              (match li with
                | Ps_Ter t ->
                    (*bloc_B_arg := (key, state, (fst is_trace).(t),p,
                     new_succ_states)::!bloc_B_arg;*)
                    (fst (fst is_trace)).(t) <-
                     Map_is.add key state (fst (fst is_trace)).(t)
                | Ps_Non_ter nt ->
                    (*bloc_B_arg := (key, state, (snd is_trace).(nt),p,
                     new_succ_states)::!bloc_B_arg;*)
                    (fst (snd is_trace)).(nt) <-
                    Map_is.add key state (fst (snd is_trace)).(nt)
                | Ps_Ter_NL t ->
                    (snd (fst is_trace)).(t) <-
                     Map_is.add key state (snd (fst is_trace)).(t)
                | Ps_Non_ter_NL nt ->
                    (snd (snd is_trace)).(nt) <-
                    Map_is.add key state (snd (snd is_trace)).(nt));
              ga_set new_ssa (state.number*prd_nb+p) true;
              (state::state_list,state::new_wl,(state,p)::new_ss_list)*))
          (state_list, new_wl, []) el
        in
        s.succ_states <- new_ss_list;
        List.iter (fun (s,p) -> !new_ssa.(s.number*prd_nb+p) <- false) 
          new_ss_list;
        (state_list, new_wl))
      (state_list, []) wl
    in
    process_state new_wl state_list
  in
  let state_list =
    process_state working_list working_list
  in
  
  (*let time5 = Sys.time () in
  Printf.printf "process_state = %.3f s\n" (time5-.time4);*)
  
  let array_lt_ter = Array.make token_nb None in
  let array_lt_ter_nl = Array.make token_nb None in
  (*let splitting_ter = Array.make token_nb false in*)
  let array_lt_nt = Array.make lhs_nb None in
  let array_lt_nt_nl = Array.make lhs_nb None in
  (*let splitting_nt = Array.make (Array.length str_non_ter) false in*)
  
  (*Gc.set { (Gc.get()) with Gc.space_overhead = 100 };*)
  
  let array_lhs = Array.make nt_nb false in
  
  let pdev = List.hd pdev_list in
  
  let rec map_succ (is_trace, state_list) v =
    let vl =
      (*Printf.fprintf !log_channel "-state built-\n";
        print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        str_token_name priodata.prd_names;
      Printf.fprintf !log_channel "\n";
      flush_all ();*)
      (*print_state v gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
        pdev.str_ter priodata.prd_names interm_pdev.regexp_array;*)
      move_LR0 v is_trace gram_rhs gram_lhs gram_parnt bnt_array r_L priodata
        array_nt_prio lhslists array_lt_ter array_lt_ter_nl array_lt_nt
        array_lt_nt_nl (*splitting_ter splitting_nt*) array_lhs new_ssa
        non_kernel_array
    in
    List.fold_left map_succ (is_trace,v::state_list) vl
  in
  
  let state_list, state_ind, is_trace =
    match is, lit_trans with
    | (Some is), (Some lt) ->
        let lt = match lt with
        | Ps_Non_ter nt -> Ps_Non_ter nt_subst.(0).(nt)
        | Ps_Ter t -> Ps_Ter t
        | Ps_Non_ter_NL nt -> Ps_Non_ter_NL nt_subst.(0).(nt)
        | Ps_Ter_NL t -> Ps_Ter_NL t
        in
        new_start_state is lt subst_rn_red subst_rn nt_nb is_trace state_list
        gram_rhs gram_lhs gram_parnt bnt_array r_L array_nt_prio lhslists priodata
        non_kernel_array map_succ
    | _ -> state_list, None, is_trace
  in
  
  (*let time6 = Sys.time () in
  Printf.printf "state_list = %.3f s\n" (time6-.time5);*)
  
  let entry_points_list =
    Hashtbl.fold (fun nt _ l -> nt_subst.(0).(nt)::l)
    (List.hd pdev_list).entry_points []
  in
  let entry_points = Hashtbl.create (List.length entry_points_list) in
  
  let state_list, is_trace =
    
    List.fold_left
    (fun (state_list, is_trace) ep ->
      let lhs_ind_set =
        Prio_map.fold
        (fun p lhs_ind set -> Int_set.add lhs_ind set)
        array_nt_prio.(ep) Int_set.empty
      in
      if Int_set.cardinal lhs_ind_set = 1 then
        match stations.(Int_set.choose lhs_ind_set) with Some s ->
        (Hashtbl.add entry_points ep s;
        state_list, is_trace)
        | None -> assert false
      else
        let s_list =
          Int_set.fold
            (fun i l -> match stations.(i) with Some v ->
              v::l | None -> assert false)
            lhs_ind_set []
        in
        let state_list_result, source_to_clear, color_black_to_clear =
          epsilon_closure s_list old_stations component predict
          lhs_nb pnb true lhs_station count_es source color_black result
        in
        List.iter (fun i -> source.(i) <- -1) source_to_clear;
        List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
        List.iter (fun i -> result.(i.number) <- false) state_list_result;
        let state =
          merge_states state_list_result (Ps_Ter (-1)) succ_states_array
          prd_nb non_kernel_array in
        let is_trace, state_list = map_succ (is_trace,state_list) state in
        Hashtbl.add entry_points ep state;
        state::state_list, is_trace)
        (* is it really necessary to have state in state_list ? *)
    (state_list, is_trace) entry_points_list
  in
  (*
  let time7 = Sys.time () in
  Printf.printf "entry_points_list = %.3f s\n" (time7-.time6);*)
  
  (*Printf.fprintf !log_channel "\nstate_list:\n";
  List.iter
  (fun s ->
    Printf.fprintf !log_channel "\n";
    print_state s gram_rhs lhs_table nt_of_ind prio_of_ind str_non_ter
      E.str_token_name priodata.prd_names)
  state_list;
  
  print_stations stations gram_rhs lhs_table str_non_ter nt_of_ind prio_of_ind priodata;*)
  
  let table, table_it, table_lit_trans, state_bnt, state_is_mergeable =
    make_table state_list (!countst) (Array.length gram_lhs) gram_rhs
      lhs_table nt_of_ind prio_of_ind array_nt_prio ppar token_nb
      interm_pdev.str_ter interm_pdev.str_non_ter
      interm_pdev.prio.prd_names interm_pdev.regexp_array
  in
  
  let rn_of_rule =
    make_rn_of_rule gram_rhs rule_options lhs_table str_non_ter
    priodata.prd_names pdev.str_ter interm_pdev.regexp_array in
  
  (*let time2 = Sys.time () in
  Printf.printf "table = %.3f s\n" (time2-.time7);
  Printf.printf "merge_parsing_device = %.3f s\n" (time2-.time0);*)
  
  (*let time1 = Sys.time () in
  List.iter merge_states_prof (!merge_states_ref);
  let time2 = Sys.time () in
  Printf.printf "merge_states = %.3f s\n" (time2-.time1);*)
  
  (*let time_em1 = Sys.time () in
  let em_prof _ s =
          em_arg_ref := s::!em_arg_ref;
          Succ_states.fold
          (fun (t,p) el -> match t.li with
            | Ps_Ter ter ->
                if edge_array.(symb_nb*p+ter) = [] then
                  (edge_array.(symb_nb*p+ter) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+ter) <- t::edge_array.(symb_nb*p+ter); el)
            | Ps_Non_ter nt ->
                if edge_array.(symb_nb*p+E.token_nb+nt) = [] then
                  (edge_array.(symb_nb*p+E.token_nb+nt) <- [t]; (t.li,p)::el)
                else
                  (edge_array.(symb_nb*p+E.token_nb+nt)
                   <- t::edge_array.(symb_nb*p+E.token_nb+nt); el))
          s.succ_states []
  in
  let _ = List.fold_left em_prof [] !em_arg_ref in
  let time_em2 = Sys.time () in
  Printf.printf "em_prof = %.3f s\n"
    (time_em2-.time_em1);*)
  
  (*let epsilon_closure_prof (s_list, old_stations, component, predict,
    lhs_nb, pnb, lhs_station, count_es) =
    let state_list_result, source_to_clear, color_black_to_clear =
      epsilon_closure s_list old_stations component predict
      lhs_nb pnb false lhs_station count_es source color_black result
    in
    List.iter (fun i -> source.(i) <- -1) source_to_clear;
    List.iter (fun i -> color_black.(i) <- false) color_black_to_clear;
    List.iter (fun i -> result.(i.number) <- false) state_list_result;
  in
  let time_prof1 = Sys.time () in
  List.iter epsilon_closure_prof !epsilon_closure_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "epsilon_closure_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !epsilon_closure_arg);*)
  
  (*let block_A_prof (key,ist,p,new_succ_states) =
    try let succ_state = Map_is.find key ist in
      let _ = Succ_states.add (succ_state,p) new_succ_states in ()
    with Not_found -> ()
  in
  let time_prof1 = Sys.time () in
  List.iter block_A_prof !bloc_A_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "block_A_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !bloc_A_arg);
  
  let block_B_prof (key,state,ist,p,new_succ_states) =
    let _ = Map_is.add key state ist in
    let _ = Succ_states.add (state,p) new_succ_states in ()
  in
  let time_prof1 = Sys.time () in
  List.iter block_B_prof !bloc_B_arg;
  let time_prof2 = Sys.time () in
  Printf.printf "block_B_prof = %.3f s, calls=%d\n"
    (time_prof2-.time_prof1) (List.length !bloc_B_arg);*)
  
  (*let dec_table, final, build_nfa_table, start = make_lexer regexp_list in*)
  
  { gram_lhs = gram_lhs;
    gram_rhs = gram_rhs;
    gram_parnt = gram_parnt;
    bnt_array = bnt_array;
    (*dypgen_epsilon =
      (try Hashtbl.find nt_table "dypgen__epsilon"
      with Not_found -> -1);*)
    lhs_table = lhs_table;
    actions = actions;
    inherited = inherited;
    entry_point = entry_point;
    entry_points = entry_points;
    g_nb = g_nb;
    lex_nb = interm_pdev.lex_nb;
    nt_table = nt_table;
    stations = stations;
    state_list = state_list;
    is_trace = is_trace;
    array_nt_prio = array_nt_prio;
    st_nb = !countst;
    table = table;
    state_bnt = state_bnt;
    state_is_mergeable = state_is_mergeable;
    table_it = table_it;
    (*table_lex_trans = table_lex_trans;*)
    table_lit_trans = table_lit_trans;
    lhslists = lhslists;
    r_L = r_L;
    po = po_array;
    cyclic_rules = cyclic_rules;
    data = newdata;
    prio = priodata;
    loc_data = newlocal_data;
    nt_nb = nt_nb;
    token_nb = token_nb;
    prio_of_ind = prio_of_ind;
    nt_of_ind = nt_of_ind;
    str_non_ter = str_non_ter;
    cons_of_nt = cons_of_nt;
    relations = relations;
    nt_cons_map = nt_cons_map;
    rn_of_rule = rn_of_rule;
    regexp_decl = pdev.regexp_decl;
    main_lexer_start = interm_pdev.main_lexer_start;
    main_lexer_table = interm_pdev.main_lexer_table;
    main_lexer_actions = interm_pdev.main_lexer_actions;
    aux_lexer = {
      aux_lexer_start = pdev.aux_lexer.aux_lexer_start;
      aux_lexer_table = pdev.aux_lexer.aux_lexer_table;
      aux_lexer_actions = pdev.aux_lexer.aux_lexer_actions };
    str_ter = pdev.str_ter;
    ter_table = pdev.ter_table;
    layout_id = interm_pdev.layout_id;
    regexp_table = interm_pdev.regexp_table;
    regexp_array = interm_pdev.regexp_array;
    rule_options = rule_options },
  state_ind
  in
  (*let pdev_rightSib, start_state_ind =
    (*let is = pdev_leftSib.table_it.(v_rightSib) in
    let lit_trans = pdev_leftSib.table_lit_trans.(v_rightSib) in*)
    merge_parsing_device [pdev_leftSib;interm_pdev]
      newdata newlocal_data lit_trans (counters.count_g+1)
      start_is interm_pdev.nt_cons_map interm_pdev.relations ppar
  in*)
  
  let time2 = Sys.time () in
  counters.count_g <- counters.count_g + 1;
  if !dypgen_verbose>0 then
    Printf.fprintf !log_channel
    "Parse table composed in %.3f sec\n" (time2-.time1);
  start_state_ind, pdev_rightSib



let update_pp (pdev, ppar) command_list =
  let dyp_a = action_command command_list {
    ac_gd = pdev.data;
    ac_ld = pdev.loc_data;
    add_rules = [];
    new_nt_cons = [];
    ac_relations = [];
    will_shift = true;
    keep_grammar = false;
    next_state = None;
    next_grammar = None }
  in
  let mapfun (r, ac) =
    (r,
    (Dypgen_action
      (fun ol pos posl gd ld lld di p nl ->
      (transform_action ac) ol pos posl gd ld lld di p nl)),
    [])
  in
  let add_rules_transformed = List.map mapfun dyp_a.add_rules in
  let counter = {
    countsn = 0;
    counted = 0;
    count_token = 0;
    last_layout = 0;
    count_g = 0;
    count_lex = 0 }
  in
  let _, new_pdev =
    update_parsing_device ppar pdev dyp_a.ac_relations add_rules_transformed
    dyp_a.ac_gd dyp_a.ac_ld dyp_a.new_nt_cons counter None None
  in
  new_pdev, ppar



(*let update_parsing_device parsing_device ral_add r_remove newdata newdatadyn
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
      non_kernel = [];
      predict = Int_set.empty
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
  
  create_parsing_device_with_init gram_rhs gram_lhs lhs_table actions r_L
    user_g newdata newdatadyn newlocal_data parsing_device.prio
    init_is_updated_parsing_device lit_trans user_nt_nb
    po_array it_nb array_nt_prio nt_of_ind non_ter_of_ind
    prio_of_ind lhs_of_ind g_nb ist_nt_nb lhslists nt_to_add str_non_ter
    cons_of_nt,
  rn_map *)

(* ******* CE QUI PRECEDE EST A DEPLACER DANS UN AUTRE FICHIER ****** *)




type ('token,'obj,'data,'local_data,'lexbuf) vertex = {
  state_nb : int;
  mutable pdev : ('token,'obj,'data,'local_data,'lexbuf) parsing_device;
  sn_nb : int;
  last_token : int;
  layout_flags : int;
  (* First bit = 1 if layout char were read just before the last
  token read before creating this stack node.
  Second bit = 1 if layout char are allowed to be read before the
  next token. *)
  lexer_pos : (Lexing.position * Lexing.position);
  mutable succ_edges : (('token,'obj,'data,'local_data,'lexbuf) edge) list;
  mutable prev_nodes_eps : ('token,'obj,'data,'local_data,'lexbuf) vertex list;
  (* stack nodes that point to this node and that have the same last_token,
  the list is made empty when the node is removed from topmost *)
  (*inherited_values : (int, 'obj list) Hashtbl.t;*)
    (* the int is the id of the associated non terminal *)
  mutable inherited_values : 'obj list;
    (* There is only one nt in one rhs that can have an inherited value
    waiting to be used. *)
  mutable det_depth : int;
  (* deterministic depth, see Elkhound TR sect. 3.1 (not used yet) *)
  mutable ref_count : int;
  (* reference count, see Elkhound TR sect. 3.1 (not used yet) *)
}
and ('token,'obj,'data,'local_data,'lexbuf) edge = {
  mutable edge_label : 'obj list;
  mutable edge_id : int;
  mutable dest : ('token,'obj,'data,'local_data,'lexbuf) vertex;
  mutable parse_tree : string
  (*mutable edge_reduced : bool;*)
  (* tells whether the edge has been used for a reduction *)
  (*mutable reduction_list :
    ('token,'obj,'data,'local_data,'lexbuf) reduction list option;*)
  (* Only useful in case of a cyclic grammar: yield-then-merge may
  happen for an edge corresponding with a nonterminal that can derive
  itself. When reducing along such an edge, one stores the path of this
  reduction (even if the action raises Giveup). If a subsequent merge
  happens on this edge then the reductions are added to pathList and
  removed from the edge.
  When the source node quits topmost the field reduction_list is set to
  []. *)
}
(*and ('token,'obj,'data,'local_data,'lexbuf) reduction =
  (('token,'obj,'data,'local_data,'lexbuf) vertex *
   ('token,'obj,'data,'local_data,'lexbuf) edge list * int) *
  (int * rhs) * int*)

(*let update_depth topmost f =
  let rec aux g sn =
    if f sn then match sn.succ_edges with
      | [e] when sn.ref_count=1 ->
          let h x = sn.det_depth <- x+1; g (x+1) in
          aux h e.dest
      | _ -> sn.det_depth <- 0; g 0
  in
  List.iter (aux (fun _ -> ())) topmost*)

let create_e v1 label id v2 f topmost pt =
  let new_edge = {
    edge_label = label; edge_id = id; dest = v2;
    parse_tree = pt
    (*edge_reduced = false;*) (*reduction_list = None*) }
  in
  v1.succ_edges <- new_edge::(v1.succ_edges);
  (*v2.ref_count <- v2.ref_count+1;
  if v2.ref_count > 2 then
    update_depth topmost f;*)
  if v1.last_token = v2.last_token then
    v2.prev_nodes_eps <- v1::v2.prev_nodes_eps;
  new_edge



let create_v state_nb pdev sn_nb last_token lexer_pos layout_flags depth = {
  state_nb = state_nb;
  pdev = pdev;
  sn_nb = sn_nb;
  last_token = last_token;
  lexer_pos = lexer_pos;
  layout_flags = layout_flags;
  succ_edges = [];
  prev_nodes_eps = [];
  inherited_values = [];
    (*Hashtbl.create (Int_set.cardinal pdev.state_bnt.(state_nb));*)
  det_depth = depth;
  ref_count = 0; }

let count_nodes topmost =
  let rec aux n idset sn =
    if Int_set.mem sn.sn_nb idset then (n,idset) else
    let idset = Int_set.add sn.sn_nb idset in
    List.fold_left (fun (n,idset) e -> aux n idset e.dest)
      (n+1,idset) sn.succ_edges
  in
  let n, _ =
    List.fold_left (fun (n,idset) sn -> aux n idset sn)
    (0,Int_set.empty) topmost
  in
  n



type ('t,'a,'b,'c,'d) path =
  ('t,'a,'b,'c,'d) vertex * (('t,'a,'b,'c,'d) edge list) * int
(** vertex is for the start of the path, int is the token number, i.e.
the first token in the part of the input which would be reduced if a reduction
along this path of the graph-structured stack happens. *)

type ('t,'o,'gd,'ld,'lb) reduction =
  ('t,'o,'gd,'ld,'lb) path * (int * rhs) * int
  (* The last int is the rule id.
  The couple (int * rhs) is not necessary and should be dropped. *)

type ('t,'o,'gd,'ld,'lb) merge_item =
  ('t,'o,'gd,'ld,'lb) vertex * ('t,'o,'gd,'ld,'lb) edge * int *
  ('o * 'gd * 'ld) list

type ('t,'o,'gd,'ld,'lb) edge_obj =
  ('t,'o,'gd,'ld,'lb) edge * 'o list

type 'a last_red = No_red | Reg_red of 'a | Rec_red of 'a

type ('t,'o,'gd,'ld,'lb) reduction_data = {
  mutable red : ('t,'o,'gd,'ld,'lb) reduction list;
  mutable recred : ('t,'o,'gd,'ld,'lb) reduction list;
  mutable recred_eps : ('t,'o,'gd,'ld,'lb) reduction list;
  mutable next_recred : ('t,'o,'gd,'ld,'lb) reduction list;
  mutable next_recred_eps : ('t,'o,'gd,'ld,'lb) reduction list;
  mutable merge_map : ('t,'o,'gd,'ld,'lb) merge_item Int_map.t;
  mutable merge_map_eps : ('t,'o,'gd,'ld,'lb) merge_item Int_map.t;
  mutable obj_recred : ('t,'o,'gd,'ld,'lb) edge_obj Int_map.t;
  mutable obj_recred_eps : ('t,'o,'gd,'ld,'lb) edge_obj Int_map.t;
  mutable old_obj_recred : ('t,'o,'gd,'ld,'lb) edge_obj Int_map.t;
  mutable old_obj_recred_eps : ('t,'o,'gd,'ld,'lb) edge_obj Int_map.t;
  mutable last_red : ('t,'o,'gd,'ld,'lb) reduction last_red;
  mutable last_red_eps : ('t,'o,'gd,'ld,'lb) reduction last_red;
  mutable parse_result : ('o * string) list; }




(** [find_paths] returns a list of couples ([path],[token_nb]),
where a path is a list of edges of the graph structured stack
[gs]. The returned paths have the following properties : they
begin at stack node [sn], their length is [len], the nodes
along each path contain states which associated literals are in
[litl], these literals take place along the path in the same
order as in the list [litl] (actually in reverse). These paths
are the ones along which a reduction can be performed by a
production rule which rhs is [litl].
[token_nb] is the token number of the leftmost stack node of a
path, i.e. the dest stack node of the edge which is at the end
of the list of edges which is [path]. It is the number of the
token which is the first in the part of the input which would be
reduced by the rule given as argument.
[find_paths] is used in [do_reductions] and in [insert_reduction]
and in [insert_reduction2]. *)
let find_paths ?(skip=0) ?(init=[]) sn (rhs:rhs) =
  let rec aux n succ path =
    if n = skip then match path with
      | e::_ -> let s = e.dest in [path,s.last_token]
      | [] -> [[],sn.last_token]
    else
    match succ with
      | e::t ->
          let sn2 = e.dest in
          let succ2 = sn2.succ_edges in
          (aux (n-1) succ2 (e::path))@(aux n t path)
      | [] -> []
  in
  aux (Array.length rhs) sn.succ_edges init

let find_paths_no_layout ?(skip=0) ?(init=[]) sn (rhs:rhs) last_layout =
  let rec aux n succ path =
    if n = skip then match path with
      | e::_ -> let s = e.dest in [path,s.last_token]
      | [] -> [[],sn.last_token]
    else
    match succ with
      | e::t ->
          let sn2 = e.dest in
          if sn2.last_token < last_layout then []
          else
          let succ2 = sn2.succ_edges in
          (aux (n-1) succ2 (e::path))@(aux n t path)
      | [] -> []
  in
  aux (Array.length rhs) sn.succ_edges init

let stack_node_equal sn1 sn2 = sn1.sn_nb = sn2.sn_nb

let edge_equal e1 e2 = e1.edge_id = e2.edge_id
let edge_list_equal el1 el2 = List.for_all2 edge_equal el1 el2

let print_path sn p =
  let snnb = sn.sn_nb in
  let f e =
    let ednb = e.edge_id in
    Printf.fprintf !log_channel "[%d]-%d-" e.dest.sn_nb ednb
  in
  List.iter f p;
  Printf.fprintf !log_channel "[%d]\n" snnb



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
    A better data structure than a list should be used, maybe a
    mutable list. *)
let insert_partially_ordered l (((start0,p0,tnb0),(ind0,rhs0),rn0) as pr0) =
  if !dypgen_verbose>2 then
    (output_string !log_channel "inserting reduction along:\n";
    print_path start0 p0);
  let pdev0 = start0.pdev in
  (*Printf.fprintf !log_channel "insert_partially_ordered\n";*)
  (* The bool b tells whether a path of the same class has already
   been seen in the list *)
  let rec aux result b l = match l with
    | [] -> revcat result [pr0]
    | (((start1,p1,tnb1),(ind1,rhs1),rn1) as pr1)::tl ->
        if tnb1<tnb0 then revcat result (pr0::l)
          (* yes, this is the good order *)
        else if tnb1>tnb0 then aux (pr1::result) false tl
        else if pdev0.g_nb <> start1.pdev.g_nb
          then aux (pr1::result) false tl
        (* Is the following test really useful ? *)
        else if rn0=rn1 && (stack_node_equal start0 start1) &&
          (edge_list_equal p0 p1) then revcat result l else
        if pdev0.po.(ind0).(ind1) then
          aux (pr1::result) pdev0.po.(ind1).(ind0) tl
        else
          if pdev0.po.(ind1).(ind0) || ind0=ind1 then
            revcat result (pr0::l)
          else
            if b then revcat result (pr0::l)
            else aux (pr1::result) false tl
  in
  aux [] false l

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

let str_symb_in_rhs_reduction str_non_ter = function
  | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_) -> str_non_ter.(nt)
  | Ps_Ter t | Ps_Ter_NL t -> "t"^(string_of_int t)


let str_rhs_reduction sn p rhs str_non_ter =
  let _, l = List.fold_left
    (fun (lt,l) e -> e.dest.last_token, (e.dest.last_token,lt)::l)
    (sn.last_token,[]) p
  in
  let rec aux i l s = if i=Array.length rhs then s else
    match l with
    | (a,b)::tl -> let s = Printf.sprintf "%s %s:%d-%d"
        s (str_symb_in_rhs_reduction str_non_ter rhs.(i)) a b in
        aux (i+1) tl s
    | _ -> assert false
  in
  (aux 0 l "")^"\n"



let insert_reduction2 rl sn counters use_rule_order =
  let v = sn.state_nb and pdev = sn.pdev in
  let aux2 rn rl =
    let rhs = pdev.gram_rhs.(rn)
    and _, _, ind = pdev.lhs_table.(rn) in
    let paths =
      if 1 land pdev.rule_options.(rn) = 1 then find_paths sn rhs
      else find_paths_no_layout sn rhs counters.last_layout
    in
    List.fold_left
      (fun rl (p,tnb) ->
        insert_partially_ordered rl ((sn,p,tnb),(ind,rhs),rn))
      rl paths
  in
  
  if use_rule_order then
    let rn =
      Int_set.fold (fun rn minrn -> min minrn rn)
      pdev.table_it.(v).reducible max_int
    in
    (if rn < max_int then aux2 rn rl else rl)
  else
  
  try Int_set.fold aux2 pdev.table_it.(v).reducible rl
  with Invalid_argument _ ->
    (Printf.fprintf stderr "v = %d, table_it len = %d\n" v
    (Array.length pdev.table_it); exit 2)


(* link is supposed to be an edge coming from sn *)
let insert_reduction_with_link rl sn link counters use_rule_order =
  if !dypgen_verbose>2 then
    output_string !log_channel "insert_reduction_with_link called\n";
  let v = sn.state_nb and pdev = sn.pdev in
  let sn1 = link.dest in
  let aux2 rn rl =
    let rhs = pdev.gram_rhs.(rn)
    and _, _, ind = pdev.lhs_table.(rn) in
    let paths =
      if rhs = [||] then [[],sn.last_token] else
      if 1 land pdev.rule_options.(rn) = 1
      then find_paths ~skip:1 ~init:[link] sn1 rhs
      else if sn1.last_token < counters.last_layout then []
      else find_paths_no_layout ~skip:1 ~init:[link] sn1 rhs
        counters.last_layout
    in
    List.fold_left
      (fun rl (p,tnb) ->
        insert_partially_ordered rl ((sn,p,tnb),(ind,rhs),rn))
      rl paths
  in
  
  if use_rule_order then
    let rn =
      Int_set.fold (fun rn minrn -> min minrn rn)
      pdev.table_it.(v).reducible max_int
    in
    (if rn < max_int then aux2 rn rl else rl)
  else
  
  try Int_set.fold aux2 pdev.table_it.(v).reducible rl
  with Invalid_argument _ ->
    (Printf.fprintf stderr "v = %d, table_it len = %d\n" v
    (Array.length pdev.table_it); exit 2)



let find_all_prev_nodes sn =
  let rec aux (n_list,n_set) curr_sn =
    if Int_set.mem curr_sn.sn_nb n_set then (n_list,n_set) else
    let n_set = Int_set.add curr_sn.sn_nb n_set in
    let n_list = curr_sn::n_list in
    List.fold_left aux (n_list,n_set) curr_sn.prev_nodes_eps
  in
  let pn, _ = aux ([],Int_set.empty) sn in
  pn



let insert_reduction rl sn link counters topmost use_rule_order =
  match sn.prev_nodes_eps with
  | [] -> insert_reduction_with_link rl sn link
      counters use_rule_order
  | _ ->
  if !dypgen_verbose>2 then
    output_string !log_channel "insert_reduction called\n";
  let prev_nodes = find_all_prev_nodes sn in
  let edge_nb = link.edge_id in
  let aux1 rl sn =
    let v = sn.state_nb and pdev = sn.pdev in
    let aux2 rn rl =
      let rhs = pdev.gram_rhs.(rn)
      and _, _, ind = pdev.lhs_table.(rn) in
      let paths =
        if 1 land pdev.rule_options.(rn) = 1 then find_paths sn rhs
        else find_paths_no_layout sn rhs counters.last_layout in
      List.fold_left
        (fun rl (p,tnb) ->
          insert_partially_ordered rl ((sn,p,tnb),(ind,rhs),rn))
        rl
        (List.filter (fun (p,_) ->
          List.exists (fun e -> e.edge_id=edge_nb) p)
          paths)
    in
    
    if use_rule_order then
      (let rn =
        Int_set.fold (fun rn minrn -> min minrn rn)
        pdev.table_it.(v).reducible max_int
      in
      if rn < max_int then aux2 rn rl else rl)
    else
    
    Int_set.fold aux2 pdev.table_it.(v).reducible rl
  in
  List.fold_left aux1 rl prev_nodes



(*let insert_reduction_old pathList _ link last_layout topmost =
  let _,edge_nb = link.edge_label in
  let aux1 pathList sn =
    let v,pdev = sn.state_nb,sn.pdev in
    let aux2 rn pathList =
      let rhs,(_,_,ind) = pdev.gram_rhs.(rn), pdev.lhs_table.(rn) in
      let paths =
        if 1 land pdev.rule_options.(rn) = 1 then find_paths sn rhs
        else find_paths_no_layout sn rhs last_layout in
      let aux3 pathList (p,tnb) =
        if List.exists (function e -> (snd e.edge_label)=edge_nb) p then
          let () = if !dypgen_verbose>2 then print_path sn p else () in
          insert_partially_ordered pathList ((sn,p,tnb),(ind,rhs),rn)
        else pathList
      in
      List.fold_left aux3 pathList paths
    in
    Int_set.fold aux2 pdev.table_it.(v).reducible pathList
  in
  List.fold_left aux1 pathList topmost*)



exception Find_rightSib_failed

(** [find_rightSib] is used in [reduceViaPath] and [doShift].
    Its purpose is to find in the stack nodes list [snl] a stack node which
    holds the same grammar as [g_leftSib] and which holds a state which has an
    items set equal to [is_rightSib]. *)
let find_rightSib g_nb_rightSib st_nb snl (gd:'gd) (ld:'ld) layout_flags (gd_equal:'gd->'gd->bool) (ld_equal:'ld->'ld->bool) =
  let rec aux snl = match snl with
    | [] -> raise Find_rightSib_failed
    | sn::tl ->
        if sn.state_nb = st_nb && sn.pdev.g_nb = g_nb_rightSib &&
          gd_equal sn.pdev.data gd &&
          ld_equal sn.pdev.loc_data ld &&
          layout_flags = sn.layout_flags &&
          sn.pdev.state_is_mergeable.(sn.state_nb)
        then sn
        else aux tl
  in
  aux snl



let print_sn sn =
  let chan = !log_stack_channel in
  let { state_nb = v; pdev = parsing_device; sn_nb = snnb;
    last_token = last_token } = sn
  in
  let nt_of_ind = parsing_device.nt_of_ind in
  let prio_of_ind = parsing_device.prio_of_ind in
  let gram_rhs = parsing_device.gram_rhs in
  let lhs_table = parsing_device.lhs_table in
  let is = parsing_device.table_it.(v) in
  let str_non_ter = parsing_device.str_non_ter in
  output_string chan "____________________________________\n";
  Printf.fprintf chan "STACK NODE <%d>, last token:%d\n" snnb last_token;
  output_string chan "\n";
  let priority_names = parsing_device.prio.prd_names in
  print_item_set chan is gram_rhs lhs_table nt_of_ind prio_of_ind
  str_non_ter sn.pdev.str_ter priority_names parsing_device.regexp_array;
  Printf.fprintf chan "  state number: %d\n" v;
  output_string chan "\n";
  let f3 f4 ed =
    let ednb = ed.edge_id in
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
  output_string chan "\n";
  flush_all ()


let check_last_token last_token vl =
  vl.last_token >= last_token

open Lexing

exception Find_link_failed



let compare_pr ((start0,p0,tnb0),(ind0,rhs0),rn0) ((start1,p1,tnb1),(ind1,rhs1),rn1) =
  if tnb1<tnb0 then -1
  else if tnb1>tnb0 then 1 else
  let pdev0 = start0.pdev in
  if pdev0.g_nb <> start1.pdev.g_nb then 0 else
  if pdev0.po.(ind0).(ind1) && not pdev0.po.(ind1).(ind0) then 1 else
  if pdev0.po.(ind1).(ind0) && not pdev0.po.(ind0).(ind1) then -1 else
  0



(*let insert_in_ml ml ind0 tnb0 rightSib link cons_index new_obj gd ld old_obj_list =
  let g_nb0 = rightSib.pdev.g_nb in
  let po = rightSib.pdev.po in
  let new_merge_item () =
    let objdatalist =
      (new_obj, gd, ld)::(List.map
      (fun o -> o,rightSib.pdev.data,rightSib.pdev.loc_data)
       old_obj_list
    in
    ind0,tnb0,g_nb0,(rightSib, link, cons_index, objdatalist)
  in
  let rec aux result b l = match l with
    | [] -> revcat result [new_merge_item ()]
    | ((ind1,tnb1,g_nb1,(gd1,e1,ld1,odl1)) as m1)::tl ->
        if tnb1<tnb0 then revcat result (new_merge_item ()::l)
          (* yes, this is the good order *)
        else if tnb1>tnb0 then aux (m1::result) false tl
        else if g_nb0 <> g_nb1 then aux (m1::result) false tl
        else if link.edge_nb = e1.edge_nb then
          let odl = (new_obj, gd, ld)::odl1 in
          let m = (ind1,tnb1,g_nb1,(gd1,e1,ld1,odl)) in
          revcat result (m::tl) else
        if po.(ind0).(ind1) then
          aux (m1::result) po.(ind1).(ind0) tl
        else
          if po.(ind1).(ind0) || ind0=ind1 then
            revcat result (new_merge_item ()::l)
          else
            if b then revcat result (new_merge_item ()::l)
            else aux (m1::result) false tl
  in
  aux [] false ml*)



let remove_path_edge edge_nb pathList =
  let aux1 e = match e with
    | { edge_id = n ; dest = _ } when n=edge_nb -> false
    | _ -> true
  in
  let aux2 ((_,p,_),_,_) = List.for_all aux1 p in
  List.filter aux2 pathList



let rec remove_sn_from_list sn_nb l res = match l with
  | [] -> res
  | sn::t when sn.sn_nb = sn_nb -> res@t
  | sn::t -> remove_sn_from_list sn_nb t (sn::res)



let remove_edge edge_nb sn =
  let rec aux succ_edges res = match succ_edges with
    | [] -> res
    | {edge_id = n; dest = sn_dest }::t when n=edge_nb ->
        if sn.last_token = sn_dest.last_token then
          sn_dest.prev_nodes_eps <-
          remove_sn_from_list sn.sn_nb sn_dest.prev_nodes_eps [];
        res@t
    | e::t -> aux t (e::res)
  in
  sn.succ_edges <- aux sn.succ_edges []



let merge_in_edge ppar counters edge_nb (sn, link, cons_index, objdata_list) (topmost, rl) =
  (*let topmost, rl =
    let _, _, _, (sn, link, cons_index, objdata_list) = m in
    let edge_nb = link.edge_nb in*)
    if !dypgen_verbose>2 then
      Printf.fprintf !log_channel "do_merge for edge: [%d]-%d-[%d]\n"
      link.dest.sn_nb edge_nb sn.sn_nb;
    let obj_list, glo_dat, loc_dat =
      ppar.merge_array.(cons_index) objdata_list in
    let pdev = sn.pdev in
    if (ppar.global_data_equal glo_dat pdev.data &&
      ppar.local_data_equal loc_dat pdev.loc_data)
    then
      (link.edge_label <- obj_list;
      link.edge_id <- edge_nb;
      if !dypgen_verbose>2 then
        Printf.fprintf !log_channel
        "do_merge updates the contents of the edge: [%d]-%d-[%d]\n"
        link.dest.sn_nb edge_nb sn.sn_nb;
      topmost, rl)
    else
    match sn.succ_edges with _::_::_ ->
      (if !dypgen_verbose>2 then
        Printf.fprintf !log_channel
        "do_merge removes the edge: [%d]-%d-[%d]\n"
        link.dest.sn_nb edge_nb sn.sn_nb;
      remove_edge edge_nb sn;
      let rl = remove_path_edge edge_nb rl in
      let rightSib, new_countsn, topmost, rS_is_new =
        try
          if not pdev.state_is_mergeable.(sn.state_nb)
          then raise Find_rightSib_failed else
          find_rightSib pdev.g_nb sn.state_nb topmost
          pdev.data pdev.loc_data sn.layout_flags
          ppar.global_data_equal ppar.local_data_equal,
          counters.countsn, topmost, false
        with Find_rightSib_failed ->
          let rightSib =
            create_v sn.state_nb
            { pdev with data = glo_dat; loc_data = loc_dat }
            counters.countsn sn.last_token sn.lexer_pos sn.layout_flags
            (link.dest.det_depth+1)
          in
          rightSib, counters.countsn+1, rightSib::topmost, true
      in
      let new_edge = create_e rightSib
        obj_list counters.counted link.dest
        (check_last_token link.dest.last_token) topmost link.parse_tree in
      if !dypgen_verbose>2 then
        (print_sn rightSib;
        Printf.fprintf !log_channel
        "do_merge creates a new edge%s:\n[%d]-%d-[%d]\n"
        (if rS_is_new then " and a new node" else "")
        link.dest.sn_nb counters.counted rightSib.sn_nb);
      counters.countsn <- new_countsn;
      counters.counted <- counters.counted + 1;
      let rl =
        if rS_is_new then
          insert_reduction2 rl rightSib counters ppar.use_rule_order
        else
          insert_reduction rl rightSib new_edge counters
            topmost ppar.use_rule_order
      in
      topmost, rl)
    | _ ->
      (sn.pdev <- { pdev with
        data = glo_dat; loc_data = loc_dat };
      link.edge_label <- obj_list;
      link.edge_id <- edge_nb;
      topmost, rl)



(* collect_objs collects objects along the path p.
   The head of the list corresponds to the leftmost
   object in gs. *)
let rec collect_objs path start_node =
  let rec aux res = function
    | e1::((e2::_) as tl)  ->
        let str_non_ter = e2.dest.pdev.str_non_ter in
        (match e2.dest.pdev.table_lit_trans.(e2.dest.state_nb) with
          | Ps_Non_ter nt when str_non_ter.(nt).[0] = '0' -> aux res tl
          | _ -> aux (e1.edge_label::res) tl)
    | [e] ->
        let str_non_ter = start_node.pdev.str_non_ter in
        (match start_node.pdev.table_lit_trans.(start_node.state_nb) with
          | Ps_Non_ter nt when str_non_ter.(nt).[0] = '0' -> res
          | _ -> e.edge_label::res)
    | [] -> []
  in
  let rec collect = function
    | obj_list::tl ->
      let obj_ll = collect tl in
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
  collect (List.rev (aux [] path))



let print_s start_node last_pdev outchan =
  print_table_state outchan
    start_node.state_nb
    last_pdev.table
    last_pdev.table_it
    last_pdev.table_lit_trans
    last_pdev.gram_rhs
    last_pdev.lhs_table
    last_pdev.prio_of_ind
    last_pdev.str_non_ter
    last_pdev.nt_of_ind
    last_pdev.prio.prd_names
    last_pdev.str_ter
    last_pdev.regexp_array

let print_g last_pdev outchan =
  print_grammar outchan
    last_pdev.gram_rhs
    last_pdev.lhs_table
    last_pdev.str_non_ter
    last_pdev.prio.prd_names
    last_pdev.str_ter
    last_pdev.regexp_array



let position_map p start_node =
  let rec aux res l = match l with
    | [] -> List.rev (start_node.lexer_pos::res)
    | e::t -> aux (e.dest.lexer_pos::res) t
  in
  try aux [] (List.tl p)
  with Failure _ -> [start_node.lexer_pos]



let rec make_path sn n res =
  if n = 0 then res else match sn.succ_edges with
  [e] -> make_path e.dest (n-1) (e::res) | _ -> assert false


let add_inherited_val sn0 sn1 argll nt =
  let nt =
    if sn0.pdev.g_nb = sn1.pdev.g_nb then nt
    else Hashtbl.find sn0.pdev.nt_table sn1.pdev.str_non_ter.(nt)
  in
  if sn0.pdev.bnt_array.(nt) then
    (*let arg0l = try Hashtbl.find sn0.inherited_values nt
      with Not_found -> assert false in*)
    let arg0l = sn0.inherited_values in
    let arglll =
      List.map (fun argl ->
        List.map (fun arg0 -> arg0::argl) arg0l)
      argll
    in
    List.flatten arglll
  else argll


let compute_inherited_values sn ppar next_lexeme =
  let state_id = sn.state_nb in
  let pdev = sn.pdev in
  let inh_set = pdev.state_bnt.(state_id) in
  let aux inh =
    let actl, argnb, lhs, nt, ed = pdev.inherited.(inh) in
    let path = make_path sn argnb [] in
    let sn0 = match path with
      | e::_ -> e.dest
      | _ -> assert false
      (* path should be at least of length 1 because dypgen__epsilon
      is inserted at te beginning of rhs when it begins with a
      parameterized nt. *)
    in
    let path = if ed then List.tl path else path in
    let argll = collect_objs path sn in
    let argll = add_inherited_val sn0 sn argll lhs in
    let position_list = position_map path sn in
    let end_node_pos = try fst (List.hd position_list)
      with Failure _ -> Lexing.dummy_pos in
    let start_node_pos = snd sn.lexer_pos in
    let symbol_pos = (end_node_pos, start_node_pos) in
    let objll =
      List.map (fun objl ->
        List.map (fun ac ->
          ac objl symbol_pos position_list pdev.data pdev.loc_data
          pdev.loc_data
          { prt_state = (print_s sn pdev); prt_grammar = (print_g pdev)}
          (pdev, ppar) next_lexeme)
        actl)
      argll
    in
    let res_l = List.flatten objll in
    (*Hashtbl.add sn.inherited_values nt res_l*)
    sn.inherited_values <- res_l
  in
  Int_set.iter aux inh_set




let complete_reduction topmost rl mm parse_res pr leftSib pdev_rightSib v_rightSib new_obj nt lexer_pos prio counters ppar cons_index layout_flags ind tnb selfderiv pt next_lexeme =
(*pathList pl_recred topmost leftSib pdev_rightSib
v_rightSib new_obj nt lexer_pos prio counters ppar merge_map cons_index layout_flags edge_map ind tnb =*)
  (*if !dypgen_verbose>2 then
    output_string !log_channel "complete_reduction called\n";*)
  countred := !countred + 1;
  try
    if not pdev_rightSib.state_is_mergeable.(v_rightSib)
    then raise Find_rightSib_failed else
    (* fixme : we should allow to merge (same rightSib and same link)
    for non mergeable state, only the case with Find_link_failed
    should make non mergeable mandatory. *)
    let rightSib =
      find_rightSib pdev_rightSib.g_nb v_rightSib topmost
      pdev_rightSib.data pdev_rightSib.loc_data layout_flags
      ppar.find_rightSib_global_data_equal
      ppar.find_rightSib_local_data_equal
    in
    let find_link rightSib leftSib =
      let rec aux el = match el with
        | [] -> raise Find_link_failed
        | e::tl -> if (stack_node_equal e.dest leftSib) then e else aux tl
      in
      aux rightSib.succ_edges
    in
    try
      let link = find_link rightSib leftSib in
      (*output_string !log_channel "complete_reduction 1\n";*)
      let old_obj_list = link.edge_label in
      let edge_nb = link.edge_id in
      if (!dypgen_verbose>2 || ppar.merge_warning) then
        (let (start_pos,end_pos) = lexer_pos in
        let col1 = start_pos.pos_cnum - start_pos.pos_bol in
        let col2 = end_pos.pos_cnum - end_pos.pos_bol in
        let priority_names = pdev_rightSib.prio.prd_names in
        Printf.fprintf !log_channel "Warning: the parser will merge the non terminal `%s'%s\nin file \"%s\", from l:%d,c:%d to l:%d,c:%d\n"
        (pdev_rightSib.str_non_ter.(nt))
        (str_with_priority prio priority_names) start_pos.pos_fname
        start_pos.pos_lnum col1 end_pos.pos_lnum col2);
      if !dypgen_verbose>2 then
        Printf.fprintf !log_channel
        "complete_reduction prepares a merge for [%d]-%d-[%d]\n"
        leftSib.sn_nb edge_nb v_rightSib;
      if tnb = counters.count_token || selfderiv then
        let objdata_list =
          List.map (fun o -> o, pdev_rightSib.data, pdev_rightSib.loc_data)
          old_obj_list
        in
        let topmost, rl =
          merge_in_edge ppar counters edge_nb
            (rightSib, link, cons_index, objdata_list) (topmost, rl)
        in
        topmost, rl, mm, parse_res
      else
      let mm =
        let merge_item =
          try
            let (sn,_,_,objdata_list) = Int_map.find edge_nb mm in
            sn, link, cons_index, (new_obj, rightSib.pdev.data,
              rightSib.pdev.loc_data)::objdata_list
          with Not_found ->
            rightSib, link, cons_index,
            match old_obj_list with
            | _::_ ->
                (new_obj, rightSib.pdev.data, rightSib.pdev.loc_data)::
                (List.map
                  (fun o -> o, pdev_rightSib.data, pdev_rightSib.loc_data)
                  old_obj_list)
            | _ -> assert false
            (*| _ -> Printf.printf "len=%d\n" (List.length old_obj_list);
               assert false*)
        in
        Int_map.add edge_nb merge_item mm
      in
      topmost, rl, mm, parse_res
    with
      Find_link_failed ->
      (*output_string !log_channel "complete_reduction 2\n";*)
      if ppar.global_data_equal pdev_rightSib.data
        rightSib.pdev.data &&
        ppar.local_data_equal pdev_rightSib.loc_data
        rightSib.pdev.loc_data then ()
        else raise Find_rightSib_failed;
      let link = create_e rightSib [new_obj] counters.counted leftSib
        (check_last_token leftSib.last_token) topmost pt in
      if !dypgen_verbose>2 then
        Printf.fprintf !log_channel
        "complete_reduction creates a new edge:\n[%d]-%d-[%d]\n"
         leftSib.sn_nb counters.counted rightSib.sn_nb;
      counters.counted <- counters.counted + 1;
      (*let pathList, pl_recred, pl_recred_eps =
        insert_reduction pathList pl_recred pl_recred_eps rightSib link
        counters.last_layout topmost ppar.use_rule_order ind tnb in*)
      let rl =
        insert_reduction rl rightSib link counters topmost
        ppar.use_rule_order
      in
      topmost, rl, mm, parse_res
  with
    Find_rightSib_failed ->
    (*output_string !log_channel "complete_reduction 3\n";*)
    let rightSib =
      create_v v_rightSib pdev_rightSib counters.countsn
      counters.count_token lexer_pos layout_flags (leftSib.det_depth+1)
    in
    let _ = create_e rightSib [new_obj] counters.counted leftSib
      (check_last_token leftSib.last_token) topmost pt in
    if !dypgen_verbose>2 then
      (print_sn rightSib;
      Printf.fprintf !log_channel
        "complete_reduction creates a new edge and a new node:\n[%d]-%d-[%d]\n"
        leftSib.sn_nb counters.counted rightSib.sn_nb);
    counters.countsn <- counters.countsn + 1;
    counters.counted <- counters.counted + 1;
    
    compute_inherited_values rightSib ppar next_lexeme;
    
    (*let pathList, pl_recred =
      insert_reduction2 pathList pl_recred pl_recred_eps rightSib
      counters.last_layout ppar.use_rule_order ind tnb in*)
    let rl =
      insert_reduction2 rl rightSib counters ppar.use_rule_order in
    rightSib::topmost, rl, mm, parse_res



let left_rec_rule (nt0,_,_) rhs =
  try match rhs.(0) with
    | Ps_Non_ter (nt,_) | Ps_Non_ter_NL (nt,_) -> nt = nt0
    | _ -> false
  with Invalid_argument _ -> false



let print_pathList pathList =
  output_string !log_channel " pathList :\n";
  List.iter (fun ((start_node,p,_),_,rn) ->
    output_string !log_channel " > ";
    print_path start_node p;
    let pdev = start_node.pdev in
    output_string !log_channel
      (" > "^(str_rule rn pdev.gram_rhs pdev.lhs_table pdev.str_non_ter
      pdev.prio.prd_names pdev.str_ter pdev.regexp_array)^"\n"))
      pathList



let rec try_actions ac_l start_node last_pdev toPass symbol_pos position_list pdev_leftSib ppar next_lexeme use_all_actions res_list =
  match ac_l with
  | (Dypgen_action f)::tl_ac_l ->
     let res_list, b =
       (try
         (f toPass symbol_pos position_list
         last_pdev.data  pdev_leftSib.loc_data last_pdev.loc_data
         { prt_state = (print_s start_node last_pdev);
           prt_grammar = (print_g last_pdev)}
         (last_pdev, ppar) next_lexeme)::res_list, true
       with Giveup -> res_list, false)
     in
     if b && not use_all_actions then res_list else
     try_actions tl_ac_l start_node last_pdev toPass symbol_pos
       position_list pdev_leftSib ppar next_lexeme use_all_actions res_list
  | [] -> res_list



let match_chan_s_chan_g v_rightSib pdev_rightSib chan_s chan_g =
  (match chan_s with
    | None -> ()
    | Some chan ->
         print_table_state chan v_rightSib
           pdev_rightSib.table
           pdev_rightSib.table_it
           pdev_rightSib.table_lit_trans
           pdev_rightSib.gram_rhs
           pdev_rightSib.lhs_table
           pdev_rightSib.prio_of_ind
           pdev_rightSib.str_non_ter
           pdev_rightSib.nt_of_ind
           pdev_rightSib.prio.prd_names
           pdev_rightSib.str_ter
           pdev_rightSib.regexp_array);
  (match chan_g with
    | None -> ()
    | Some chan ->
         print_grammar chan
           pdev_rightSib.gram_rhs
           pdev_rightSib.lhs_table
           pdev_rightSib.str_non_ter
           pdev_rightSib.prio.prd_names
           pdev_rightSib.str_ter
           pdev_rightSib.regexp_array)



let fold_rightSib_l pr counters nt_of_ind symbol_pos start_node last_pdev position_list leftSib ppar non_ter rn ind rhs snd_node ind_lS prio next_lexeme layout_flags ind tnb selfderiv pt cons_index (new_obj, will_shift2, keep_gram, newdata, newlocal_data, rapf_add, new_nt_cons, new_relations, chan_s, chan_g) (topmost, rl, mm, parse_res, will_shift) v_rightSib =
    let v_rightSib, pdev_rightSib =
      if rapf_add = [] then
        let lhs =
          last_pdev.nt_of_ind.(ind), last_pdev.nt_of_ind.(ind), ind
        in
        match keep_gram, left_rec_rule lhs rhs with
        | true, true when snd_node.pdev.g_nb = last_pdev.g_nb ->
            snd_node.state_nb,
            { snd_node.pdev
              with data = newdata ; loc_data = newlocal_data }
        | true, _ ->
            let time1 = Sys.time () in
            let v_rightSib, pdev_rightSib =
              let is = copy_item_set leftSib.pdev.table_it.(v_rightSib) in
              let lit_trans = leftSib.pdev.table_lit_trans.(v_rightSib) in
              extend_parsing_device last_pdev leftSib.pdev is lit_trans ppar
            in
            let time2 = Sys.time () in
            if !dypgen_verbose>0 then
              Printf.fprintf !log_channel
                "Parse table composed in %.3f sec\n" (time2-.time1);
            v_rightSib, pdev_rightSib
        | _ ,_ ->
            v_rightSib, { leftSib.pdev with data = newdata ;
            loc_data = newlocal_data }
      else
        let is = Some (leftSib.pdev.table_it.(v_rightSib)) in
        let lit_trans = Some (leftSib.pdev.table_lit_trans.(v_rightSib)) in
        match update_parsing_device ppar leftSib.pdev new_relations rapf_add
        newdata newlocal_data new_nt_cons counters is lit_trans with
        | (Some start_ind), pdev -> start_ind, pdev
        | _ -> assert false
    in
    
    let _ = match_chan_s_chan_g v_rightSib pdev_rightSib chan_s chan_g in
    
    let topmost, rl, mm, parse_res =
      if last_pdev.entry_point = last_pdev.nt_of_ind.(ind)
      && leftSib.last_token = 0 then
        (*(complete_reduction pathList pl_recred topmost leftSib
        pdev_rightSib v_rightSib new_obj (nt_of_ind.(ind_lS))
        symbol_pos prio counters ppar
        merge_map cons_index layout_flags edge_map ind tnb),*)
        let prio_str =
          last_pdev.prio.prd_names.(last_pdev.prio_of_ind.(ind)) in
        let parse_res = (new_obj, prio_str)::parse_res in
        complete_reduction topmost rl mm parse_res pr leftSib
          pdev_rightSib v_rightSib new_obj nt_of_ind.(ind_lS)
          symbol_pos prio counters ppar cons_index layout_flags
          ind tnb selfderiv pt next_lexeme
      else
        complete_reduction topmost rl mm parse_res pr leftSib
          pdev_rightSib v_rightSib new_obj nt_of_ind.(ind_lS)
          symbol_pos prio counters ppar cons_index layout_flags
          ind tnb selfderiv pt next_lexeme
        (*(complete_reduction pathList pl_recred topmost leftSib
        pdev_rightSib v_rightSib new_obj (nt_of_ind.(ind_lS))
        symbol_pos prio counters ppar
        merge_map cons_index layout_flags edge_map ind tnb), parse_result*)
    in
    topmost, rl, mm, parse_res, (will_shift2 && will_shift)



let reduce_with_result pr counters nt_of_ind symbol_pos start_node last_pdev position_list leftSib ppar non_ter rn v_rightSib ind rhs snd_node ind_lS prio next_lexeme layout_flags ind tnb selfderiv pt (topmost, rl, mm, parse_res, will_shift) ((new_obj, will_shift2, keep_gram, newdata, newlocal_data, rapf_add, new_nt_cons, new_relations, chan_s, chan_g) as res) =
    let cons_index = try leftSib.pdev.cons_of_nt.(non_ter)
      with e -> (Printf.printf "dyp.ml reduce_with, error cons_of_nt:%d\n"
        (Array.length leftSib.pdev.cons_of_nt); raise e)
    in
    if ppar.test_cons.(cons_index) new_obj = false then
      (let sr = str_rule rn leftSib.pdev.gram_rhs
        leftSib.pdev.lhs_table leftSib.pdev.str_non_ter
        leftSib.pdev.prio.prd_names leftSib.pdev.str_ter
        leftSib.pdev.regexp_array
      in
      let cons = ppar.cons_str.(leftSib.pdev.cons_of_nt.(non_ter)) in
      raise (Bad_constructor(sr,cons,(ppar.str_cons new_obj))));
      
    if v_rightSib = -1 then
      (if !dypgen_verbose>2 then output_string !log_channel
          "reduce_with found a new parse_result\n";
      let parse_res =
        let lpd = last_pdev in
        let prio = lpd.prio.prd_names.(lpd.prio_of_ind.(ind)) in
        (new_obj,prio)::parse_res
      in
      topmost, rl, mm, parse_res, (will_shift2 && will_shift))
    else
    
    fold_rightSib_l pr counters nt_of_ind symbol_pos start_node last_pdev
      position_list leftSib ppar non_ter rn ind rhs snd_node ind_lS prio
      next_lexeme layout_flags ind tnb selfderiv pt cons_index res
      (topmost, rl, mm, parse_res, will_shift) v_rightSib



let reduce_with_action ac_l pr counters nt_of_ind symbol_pos start_node last_pdev position_list leftSib ppar non_ter rn v_rightSib ind rhs snd_node ind_lS prio next_lexeme layout_flags ind tnb selfderiv pt (topmost, rl, mm, parse_res, will_shift) toPass =
  
  let res_list =
    try_actions ac_l start_node last_pdev toPass symbol_pos
      position_list leftSib.pdev ppar next_lexeme ppar.use_all_actions []
  in
  List.fold_left
  (reduce_with_result pr counters nt_of_ind symbol_pos start_node last_pdev
  position_list leftSib ppar non_ter rn v_rightSib ind rhs snd_node ind_lS
  prio next_lexeme layout_flags ind tnb selfderiv pt)
  (topmost, rl, mm, parse_res, will_shift) res_list



(*let next_states table state_nb symb_id layout i0 i2 state_bnt =
  if layout then table.(state_nb).(i0).(symb_id)
  else
    let ns0 = table.(state_nb).(i0).(symb_id) in
    let ns2 = table.(state_nb).(i2).(symb_id) in
    match ns0, ns2 with
      | [s0], [s2] when Int_set.is_empty state_bnt.(s0)
          && Int_set.is_empty state_bnt.(s2) -> ns2
      | _ -> ns0@ns2*)



let reduceViaPath (((start_node,p,tnb),(ind,rhs),rn) as pr) rl mm topmost parse_res ppar counters next_lexeme_precursor last_pr merge_reduce =
  
  if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel
      "\nreduceViaPath called, start_node=%d, pathList length=%d\n  "
      start_node.sn_nb (List.length rl);
    let pdev = start_node.pdev in
    output_string !log_channel
      ((str_rule rn pdev.gram_rhs pdev.lhs_table pdev.str_non_ter
      pdev.prio.prd_names pdev.str_ter pdev.regexp_array)^"\n  ");
    print_path start_node p;
    flush_all ());
  
  let position_list = position_map p start_node in
  let end_node_pos = try fst (List.hd position_list)
    with Failure _ -> Lexing.dummy_pos in
  let start_node_pos = snd start_node.lexer_pos in
  let symbol_pos = (end_node_pos, start_node_pos) in
  (* ^ this is the good order actually ^ *)
  (*add_to_reduction_list path counters.count_token;*)
  (*set_edge_reduced p;*)
  let leftSib, snd_node = match p with
    | [] -> start_node, start_node
    | [h] -> h.dest, start_node
    | h1::h2::_ -> h1.dest, h2.dest
  in
  let pt =
    if !dypgen_verbose>2 then
      (let pdev = start_node.pdev in
      let lhs_s =
        Printf.sprintf "%s:%d-%d -> "
        (str_lhs_bis pdev.lhs_table.(rn) pdev.str_non_ter pdev.prio.prd_names)
        leftSib.last_token start_node.last_token
      in
      Printf.fprintf !log_channel "  %s%s\n" lhs_s
        (str_rhs_reduction start_node p pdev.gram_rhs.(rn) pdev.str_non_ter);
      let pt_rhs = String.concat " " (List.map (fun e -> e.parse_tree) p) in
      let pt = "("^lhs_s^pt_rhs^")" in
      output_string !log_channel (pt^"\n");
      pt)
    else ""
  in
  let layout = (1 land snd_node.layout_flags = 1) && p <> [] in
  (*Printf.fprintf !log_channel "snd_node.layout_flags=%d\n"
    snd_node.layout_flags;
  Printf.fprintf !log_channel
    "start_node.pdev.rule_options.(rn)=%d start_node.layout_flags=%d\n"
    start_node.pdev.rule_options.(rn) start_node.layout_flags;*)
  let layout_flags =
    let first_flag =
      match p with [] -> 0 | _ -> 1 land snd_node.layout_flags
    in
    first_flag lor
    ((2 land start_node.pdev.rule_options.(rn)) land
    (2 land start_node.layout_flags))
  in
  (*Printf.fprintf !log_channel "layout_flags=%d\n" layout_flags;*)
  let v = leftSib.state_nb in
  let last_pdev = start_node.pdev in
  let nt_of_ind = leftSib.pdev.nt_of_ind in
  (*let dypgen_epsilon =
    if Array.length rhs > 0 then
      rhs.(0) = Ps_Non_ter (start_node.pdev.dypgen_epsilon,No_priority)
    else false
  in*)
  let obj_ll =
    collect_objs p start_node in
  let obj_ll =
    add_inherited_val leftSib start_node obj_ll last_pdev.nt_of_ind.(ind) in
  let ac_l = start_node.pdev.actions.(rn) in
  let non_ter =
    Hashtbl.find leftSib.pdev.nt_table
    last_pdev.str_non_ter.(last_pdev.nt_of_ind.(ind))
  in
  let prio =
    Hashtbl.find leftSib.pdev.prio.prd_ind
    last_pdev.prio.prd_names.(last_pdev.prio_of_ind.(ind))
  in
  let ind_lS =
    if leftSib.pdev.g_nb=last_pdev.g_nb then ind
    else Prio_map.find prio leftSib.pdev.array_nt_prio.(non_ter)
  in
  
  (*let v_rightSib_NL = leftSib.pdev.table.(v).(3).(ind_lS) in
  let v_rightSib =
    if not (layout || v_rightSib_NL = []) then v_rightSib_NL
    else leftSib.pdev.table.(v).(1).(ind_lS)
  in*)
  
  let v_rightSib =
    if layout then leftSib.pdev.table.(v).(1).(ind_lS)
    else
      let ns = leftSib.pdev.table.(v).(3).(ind_lS) in
      if ns = -1 then leftSib.pdev.table.(v).(1).(ind_lS)
      else ns
  in
    (*next_states leftSib.pdev.table v ind_lS layout 1 3
      leftSib.pdev.state_bnt in*)
  
  if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel "obj_ll length=%d\n"
      (List.length obj_ll);
    List.iter (fun ol ->
      Printf.fprintf !log_channel "obj_l length=%d\n" (List.length ol))
      obj_ll;
    Printf.fprintf !log_channel "v_rightSib = %d\n" v_rightSib;
    (*List.iter (fun v -> Printf.fprintf !log_channel "v_rightSib = %d\n" v)
      v_rightSib;*)
    print_pathList rl; flush_all ());
  
  if v_rightSib = -1 && not (last_pdev.entry_point =
    last_pdev.nt_of_ind.(ind) && leftSib.last_token = 0)
  then (topmost, parse_res) else
  
  let next_lexeme () = next_lexeme_precursor leftSib.pdev in
  let selfderiv = last_pdev.po.(ind).(ind) in
  
  let topmost, rl, mm, parse_res, will_shift =
    List.fold_left
    (reduce_with_action ac_l pr counters
      nt_of_ind symbol_pos start_node last_pdev position_list leftSib
      ppar non_ter rn v_rightSib ind rhs snd_node ind_lS prio
      next_lexeme layout_flags ind tnb selfderiv pt)
    (topmost, rl, mm, parse_res, true)
    obj_ll
  in
  let topmost =
    if not will_shift then
      (start_node.prev_nodes_eps <- [];
      List.filter (function x -> x!=start_node) topmost)
    else topmost
  in
  merge_reduce rl mm topmost parse_res
    (if tnb=counters.count_token then last_pr else Some pr)





let do_merge mm rl topmost ppar counters =
  if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel "do_merge called\n";
    flush_all ());
  Int_map.fold (merge_in_edge ppar counters) mm (topmost, rl)



let print_pr ((sn1,p1,_),_,rn1) ((sn0,p0,_),_,rn0) =
  print_path sn1 p1;
  let pdev = sn1.pdev in
  output_string !log_channel
    ((str_rule rn1 pdev.gram_rhs pdev.lhs_table pdev.str_non_ter
    pdev.prio.prd_names pdev.str_ter pdev.regexp_array)^"\n");
  print_path sn0 p0;
  let pdev = sn0.pdev in
  output_string !log_channel
    ((str_rule rn0 pdev.gram_rhs pdev.lhs_table pdev.str_non_ter
    pdev.prio.prd_names pdev.str_ter pdev.regexp_array)^"\n")


let make_reduction_list topmost counters ppar =
  let aux1 pathList sn =
    let st_nb = sn.state_nb and pdev = sn.pdev in
    
    (*Printf.fprintf !log_channel "reducible card = %d\n"
    (Int_set.cardinal pdev.table_it.(st_nb).reducible);*)
    (*print_sn sn;
    print_item_set !log_channel pdev.table_it.(st_nb) pdev.gram_rhs
      pdev.lhs_table pdev.nt_of_ind pdev.prio_of_ind pdev.str_non_ter
      str_token_name pdev.prio.prd_names;*)
    let aux2 rn pathList =
      let (_,_,ind) = pdev.lhs_table.(rn) in
      let rhs = pdev.gram_rhs.(rn) in
      let paths =
        if 1 land pdev.rule_options.(rn) = 1 then find_paths sn rhs
        else find_paths_no_layout sn rhs counters.last_layout in
      let paths = list_map (function (p,tnb) -> sn,p,tnb) paths in
      let aux3 pathList p =
        insert_partially_ordered pathList (p,(ind,rhs),rn) in
      List.fold_left aux3 pathList paths
    in
    
    if ppar.use_rule_order then
      let rn =
        Int_set.fold (fun rn minrn -> min minrn rn)
        pdev.table_it.(st_nb).reducible max_int
      in
      if rn = max_int then pathList
      else aux2 rn pathList
    else
    
    Int_set.fold aux2 pdev.table_it.(st_nb).reducible pathList
  in
  
  List.fold_left aux1 [] topmost



(*let new_reduction_data () = {
  red = [];
  recred = [];
  recred_eps = [];
  next_recred = [];
  next_recred_eps = [];
  merge_map = Int_map.empty;
  merge_map_eps = Int_map.empty;
  obj_recred = Int_map.empty;
  obj_recred_eps = Int_map.empty;
  old_obj_recred = Int_map.empty;
  old_obj_recred_eps = Int_map.empty;
  last_red = None;
  last_red_eps = None;
  parse_result = [] }



let do_reductions topmost counters ppar next_lexeme_precursor =
  
  let rd = new_reduction_data () in
  rd.red <- make_red_list topmost counters ppar;
  
  let process_reduction pr topmost counters is_rec =
    let (sn,_,tnb),_,_ = pr in
    let new_last_red, new_last_red_eps =
      (*Printf.fprintf !log_channel "tnb=%d, count_token=%d\n"
        tnb counters.count_token;*)
      if tnb < counters.count_token
      then
        (if is_rec then (Rec_red pr), No_red
        else (Reg_red pr), No_red)
      else
        (if is_rec then rd.last_red, (Rec_red pr)
        else rd.last_red, (Reg_red pr))
    in
    rd.last_red <- new_last_red;
    rd.last_red_eps <- new_last_red_eps;
    let topmost, will_shift =
      reduceViaPath pr rd topmost counters ppar is_rec
      next_lexeme_precursor
    in
    if not will_shift then
      (sn.prev_nodes_eps <- [];
      List.filter (function x -> x!=sn) topmost)
    else topmost
  in
  
  (*let process_recred rd topmost counters =
    match rd.recred with
    | (((sn,_,tnb),_,_) as pr)::tl ->
    rd.recred <- tl;
    let new_last_red, new_last_red_eps =
      if tnb < counters.count_token
      then (Some pr), None else rd.last_red, (Some pr)
    in
    rd.last_red <- new_last_red;
    rd.last_red_eps <- new_last_red_eps;
    let topmost, will_shift =
      reduceViaPath pr rd topmost counters ppar (*is_epsred*)
      next_lexeme_precursor
    in
    if not will_shift then
      (sn.prev_nodes_eps <- [];
      List.filter (function x -> x!=sn) topmost)
    else topmost
    | [] ->
  in*)
  
  let rec merge_reduce topmost =
    (*output_string !log_channel "merge_reduce called\n";*)
    
    (*(match rd.red, rd.last_red with
    | pr::_, Some pr0 ->
      Printf.fprintf !log_channel "compare_pr = %d\n"
      (compare_pr pr pr0)
    | _, None -> output_string !log_channel
      "rd.last_red matched None\n"
    | [], _ -> output_string !log_channel
      "rd.red matched []\n");*)
    
    let topmost = match rd.red, rd.last_red, rd.last_red_eps with
      | ((_,_,tnb) as pr)::_, _, Reg_red pr0
        when compare_pr pr pr0 = 1 ->
          do_merge rd topmost ppar counters true
      | pr::_, (Reg_red pr0), No_red when compare_pr pr pr0 = 1 ->
          do_merge rd topmost ppar counters false
      | [], (Reg_red pr0), No_red ->
          do_merge rd topmost ppar counters false
      | [], _, (Reg_red pr0) ->
          do_merge rd topmost ppar counters true
      | _ -> topmost
    in
    
    match rd.red, rd.recred_eps with
    | ((_,_,tnb) as pr)::tl, [] when tnb < counters.count_token ->
      (match rd.recred with
        | pr1::_ when compare_pr pr pr1 = 1 ->
            merge_reduce (process_reduction pr1 rd topmost counters true)
        | _ ->
            rd.red <- tl;
            merge_reduce (process_reduction pr topmost counters false))
    | pr::_, pr1::_ when compare_pr pr pr1 = 1 ->
        merge_reduce (process_reduction pr1 rd topmost counters true)
    | pr::tl, _ -> (* eps red here: tnb = counters.count_token *)
        rd.red <- tl;
        merge_reduce (process_reduction pr topmost counters false)
    | [], pr::_ ->
        merge_reduce (process_reduction pr rd topmost counters true)
    | _ -> (match rd.recred with
        | pr::_ -> merge_reduce (process_reduction pr rd topmost counters true)
        | _ -> topmost)
  in
  let topmost = merge_reduce topmost in
  topmost, rd.parse_result*)



let must_merge ((start0,p0,tnb0),(ind0,rhs0),rn0) (ind1,tnb1,g_nb1,(gd1,e1,ld1,odl1)) =
  if tnb1<tnb0 then false
  else if tnb1>tnb0 then true
  else if start0.pdev.po.(ind0).(ind1) then true
  else false




let do_reductions topmost counters ppar next_lexeme_precursor =
  
  let rl = make_reduction_list topmost counters ppar in
  
  let rec merge_reduce rl mm topmost parse_res last_pr =
    let topmost, rl = match rl, last_pr with
      | pr1::_, Some pr2 when compare_pr pr1 pr2 = 1 ->
          do_merge mm rl topmost ppar counters
      | [], _ ->
          do_merge mm rl topmost ppar counters
      | _ -> topmost, rl
    in
    match rl with
      | pr::tl ->
          reduceViaPath pr tl mm topmost parse_res ppar counters
          next_lexeme_precursor last_pr merge_reduce
      | [] -> topmost, parse_res
  in
  
  merge_reduce rl Int_map.empty topmost [] None



let do_shifts_for_each tok_name tok_value prevTops lexbuf counters ppar layout lexeme pt sn layout_flags parsing_device next_lexeme_precursor topmost next_state =
    try
      if not parsing_device.state_is_mergeable.(next_state)
      then raise Find_rightSib_failed else
      let rightSib =
        find_rightSib parsing_device.g_nb next_state topmost
        parsing_device.data parsing_device.loc_data layout_flags
        ppar.global_data_equal ppar.local_data_equal
      in
      let _ = create_e rightSib [tok_value] counters.counted sn
        (check_last_token sn.last_token) topmost pt in
      if (!dypgen_verbose>2) then
        Printf.fprintf !log_channel
        "do_shifts creates a new edge:\n  [%d]-%d-[%d]\n"
        sn.sn_nb counters.counted rightSib.sn_nb;
      counters.counted <- counters.counted + 1;
      topmost
    with Find_rightSib_failed ->
      let rightSib =
        create_v next_state parsing_device counters.countsn counters.count_token
        (ppar.lexbuf_position_fun lexbuf) layout_flags (sn.det_depth+1)
      in
      let _ = create_e rightSib [tok_value] counters.counted sn
        (check_last_token sn.last_token) topmost pt in
      if !dypgen_verbose>2 then
        (print_sn rightSib;
        Printf.fprintf !log_channel
        "do_shifts creates a new edge and a new node:\n  [%d]-%d-[%d]\n"
        sn.sn_nb counters.counted rightSib.sn_nb);
      counters.countsn <- counters.countsn + 1;
      counters.counted <- counters.counted + 1;
      let next_lexeme () = next_lexeme_precursor rightSib.pdev in
      compute_inherited_values rightSib ppar next_lexeme;
      rightSib::topmost



let do_shifts tok_name tok_value prevTops lexbuf counters ppar layout lexeme next_lexeme_precursor =
  let layout_flags = if layout then 3 else 2 in
  let pt =
    if !dypgen_verbose>2 then
      "\""^lexeme^"\":"^(string_of_int counters.count_token)
    else ""
  in
  let f topmost sn =
    let state_nb, parsing_device =
      sn.state_nb, sn.pdev
    in
    if layout && (2 land sn.layout_flags = 0)
    then topmost else
    let table = parsing_device.table in
    let next_state =
      if layout then table.(state_nb).(0).(tok_name)
      else
        let ns = table.(state_nb).(2).(tok_name) in
        if ns = -1 then table.(state_nb).(0).(tok_name)
        else ns
    in
      (*next_states table state_nb tok_name layout 0 2
      parsing_device.state_bnt in
    if next_state_l = [] then topmost else*)
    if next_state = -1 then topmost else
    do_shifts_for_each tok_name tok_value prevTops lexbuf counters
      ppar layout lexeme pt sn layout_flags parsing_device next_lexeme_precursor
      topmost next_state
  in
  List.fold_left f [] prevTops



let end_of_parsing_info time1 counters =
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
  if !dypgen_verbose>2 then close_out !log_channel else ()




let init_parser parser_pilot entry_point global_data local_data lexpos lexbuf keep_data use_rule_order use_all_actions =
  
  let counters = new_counters () in
  
  let parsing_device, ppar = parser_pilot in
  let entry_point =
    try Hashtbl.find parsing_device.nt_table entry_point
    with Not_found -> assert false
  in
  let parsing_device = { parsing_device with entry_point = entry_point } in
  let ppar =
    let f = match lexpos with
      | Some f -> f
      | None -> ppar.lexbuf_position_fun
    in
    let frS_global_de, frS_local_de = match keep_data with
      | `both -> ppar.global_data_equal, ppar.local_data_equal
      | `global -> ppar.global_data_equal, ppar.find_rightSib_local_data_equal
      | `local -> ppar.find_rightSib_global_data_equal, ppar.local_data_equal
      | `none -> ppar.find_rightSib_global_data_equal,
          ppar.find_rightSib_local_data_equal
    in
    { ppar with lexbuf_position_fun = f;
    find_rightSib_global_data_equal = frS_global_de;
    find_rightSib_local_data_equal = frS_local_de;
    use_rule_order = use_rule_order;
    use_all_actions = use_all_actions }
  in
  
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
    data = (match global_data with Some gd -> gd
      | None -> parsing_device.data);
    loc_data = (match local_data with Some ld -> ld
      | None -> parsing_device.loc_data) }
  in
  
  let start_state, parsing_device =
    try
      Hashtbl.find parsing_device.entry_points entry_point,
      parsing_device
    with Not_found ->
      let p = parsing_device in
      let array_lt_ter = Array.make parsing_device.token_nb None in
      let array_lt_ter_nl = Array.make parsing_device.token_nb None in
      let nt_nb = Array.length p.gram_lhs in
      let array_lt_nt = Array.make nt_nb None in
      let array_lt_nt_nl = Array.make nt_nb None in
      (*let splitting_ter = Array.make p.token_nb false in*)
      (*let splitting_nt = Array.make (Array.length p.str_non_ter) false in*)
      let succ_states_array = ref (Array.make 10000 false) in
      let non_kernel_array = Array.make (Array.length p.gram_rhs) false in
      let array_lhs = Array.make nt_nb false in
      let _ = countst := p.st_nb in
      let is_trace, state_list =
        make_entry_point_state p.entry_points p.stations p.gram_rhs
        p.gram_lhs p.gram_parnt p.bnt_array p.lhs_table p.nt_of_ind p.prio_of_ind
        p.str_non_ter p.str_ter
        p.regexp_array p.r_L p.prio p.array_nt_prio p.lhslists array_lt_ter
        array_lt_ter_nl array_lt_nt array_lt_nt_nl (*splitting_ter splitting_nt*)
        array_lhs succ_states_array
        non_kernel_array (p.is_trace, p.state_list) entry_point
      in
      let n = List.length state_list in
      let table, table_it, table_lit_trans, state_bnt, state_is_mergeable =
        make_table state_list n (Array.length p.gram_lhs) p.gram_rhs
          p.lhs_table p.nt_of_ind p.prio_of_ind p.array_nt_prio ppar
          parsing_device.token_nb parsing_device.str_ter
          parsing_device.str_non_ter parsing_device.prio.prd_names
          parsing_device.regexp_array
      in
      List.hd state_list,
      { parsing_device with is_trace = is_trace; state_list = state_list;
        table = table; table_it = table_it; table_lit_trans = table_lit_trans;
      state_bnt = state_bnt ;
      state_is_mergeable = state_is_mergeable }
  in
  let start =
    create_v start_state.number parsing_device counters.countsn 0
    (ppar.lexbuf_position_fun lexbuf) 2 0
  in
  
  if !dypgen_verbose>2 then print_sn start;
  
  { counters with countsn=counters.countsn+1 },
  [start], parsing_device, ppar



let clean_topmost topmost =
  List.iter (fun sn ->
    sn.prev_nodes_eps <- [](*;
    List.iter (fun e -> e.reduction_list <- [])
    sn.succ_edges*))
  topmost



let parse parser_pilot (entry_point:string)
    ?global_data ?local_data
    ?(match_len=`longest)
    ?(keep_data=`both)
    ?lexpos
    ?(use_rule_order=false)
    ?(use_all_actions=false)
    (lexfun:('a -> 'token)) (lexbuf:'a) =
  
  let time1 = Sys.time () in
  
  let counters, topmost, parsing_device, ppar =
    init_parser parser_pilot entry_point global_data local_data
    lexpos lexbuf keep_data use_rule_order use_all_actions
  in
  
  let next_lexeme_precursor _ =
    failwith "next_lexeme must not be called when using an external lexer"
  in
  
  let rec aux_LR0 topmost t prev_parse_result =
    (*if !dypgen_verbose>2 then
      (Printf.fprintf !log_channel "\nTOKEN : %s, size of topmost = %d\n"
        (ppar.str_token t) (List.length topmost); flush_all ());*)
    counters.count_token <- counters.count_token + 1;
    clean_topmost topmost;
    let topmost =
      do_shifts (ppar.get_name t) (ppar.get_value t) topmost
      lexbuf counters ppar true "" next_lexeme_precursor
    in
    let topmost, parse_result =
      do_reductions topmost counters ppar next_lexeme_precursor
    in
    if topmost = []
    then (
      match parse_result with _::_ -> parse_result
      | _ -> (match prev_parse_result with _::_ -> prev_parse_result
      | _ -> raise Syntax_error))
    else match match_len, parse_result with
      | `shortest, _::_ -> parse_result
      | _, _::_ -> aux_LR0 topmost (lexfun lexbuf) parse_result
      | _ -> aux_LR0 topmost (lexfun lexbuf) prev_parse_result
  in
  
  let parse_result =
    try (
      let topmost, _ = (* reduces initial epsilon rules *)
        do_reductions topmost counters ppar next_lexeme_precursor
      in
        aux_LR0 topmost (lexfun lexbuf) [])
    with Syntax_error ->
      (flush_all ();
      if !dypgen_verbose>2 then close_out !log_channel;
      raise Syntax_error)
  in
  
  end_of_parsing_info time1 counters;
  
  parse_result



let select_act_id =
  let choose_id a b = match a, b with
    | a, b when a<0 -> b
    | a, b when b<0 -> a
    | _ -> min a b
  in
  function h::t ->
    let rec aux k = function
      | p::t when p>=0 -> aux (choose_id k p) t
      | [] -> k
      |  _::t -> aux k t
    in
    aux h t
    | [] -> failwith "min_list: empty list"



let all_neg l = List.for_all (fun x -> x<0) l

let no_double = function [] -> assert false | i::t ->
  let rec aux i t l = match t with
    | j::u when j=i -> aux i u l
    | j::u -> aux j u (j::l)
    | [] -> l
  in
  aux i t [i]



(* One of the results returned by lex_engine is selected by select_token.
(lex_engine returns ids of regular expressions (and of their
corresponding action) that match the input and the corresponding
position)
The token must be expected by at least one item set. Among those which
are expected the longest are selected, then those belonging to the
most recent lexer, then the one generated by the higher regular
expression in the precedence order.
LAYOUT is always selected if it is the longest match and no other
matching of the same length is expected (for a given lexer, for instance if the most recent lexer matches LAYOUT and only LAYOUT, then LAYOUT is selected whatever the other (less recent) lexers match).
The variable layout is a bool that tells whether layout characters
have been read.
all_token is a bool that tells whether all the token of the maximal length are selected or only the first one (this is default). *)
let select_token topmost lexbuf all_token reset_start_pos select_unexpected layout =
  if !dypgen_verbose>2 then
    output_string !log_channel "select_token called\n";
  
  let topmost = List.sort (fun v1 v2 ->
    Pervasives.compare v1.pdev.lex_nb v2.pdev.lex_nb) topmost in
  (*let topmost = match topmost with
    | [_] -> topmost
    | _ -> List.sort (fun v1 v2 ->
        Pervasives.compare v1.pdev.lex_nb v2.pdev.lex_nb)
        topmost
  in*)
  let tbl_l, pdev_l, snll, snl, _ = List.fold_left
    (fun (tl,pl,snll,snl,i) sn ->
      if i=sn.pdev.lex_nb then tl,pl,snll,sn::snl,i else
      sn.pdev.main_lexer_table::tl,sn.pdev::pl,
      (match snl with [] -> snll | _ -> snl::snll),
      [sn],sn.pdev.lex_nb)
    ([],[],[],[],-1) topmost
  in
  let snll = match snl with [] -> snll | _ -> snl::snll in
  let res = lex_engine true tbl_l lexbuf.lb_lexbuf reset_start_pos in
  
  if !dypgen_verbose>2 then
    (Printf.fprintf !log_channel
     "lex_engine: res length=%d, snll len=%d, pdev_l len=%d\n"
    (List.length res)
    (List.length snll)
    (List.length pdev_l);
    List.iter (fun l -> List.iter (fun sn -> Printf.fprintf !log_channel
      "sn:%d " sn.sn_nb) l; Printf.fprintf !log_channel "\n") snll);
  
  let find_match3 pdev l snl =
    (*Printf.fprintf !log_channel "find_match3 called\n";*)
    let aux1 sn l = (* l is assumed to be sorted in increasing order *)
      (* the function f must filter the actions id of ter that are
      expected otherwise: when a layout regexp matches and another
      regexp matches but produces an unexpected ter, the layout is not
      considered and the lexer stops while it should discard the
      unexpected token and continue to lex. *)
      let f select_unexpected l = List.fold_left (fun l aid ->
        if fst pdev.main_lexer_actions.(aid) = pdev.layout_id
        then (-aid-1)::l else
        let ter_id =
          try fst pdev.main_lexer_actions.(aid)
          with _ -> assert false
        in
        if sn.pdev.table.(sn.state_nb).(0).(ter_id) <> -1 ||
          (sn.pdev.table.(sn.state_nb).(2).(ter_id) <> -1 && not layout)
          || select_unexpected
        then ((*Printf.fprintf !log_channel "aid=%d selected\n" aid;*)
          aid::l)
        else ((*Printf.fprintf !log_channel "aid=%d not selected\n" aid;*) l))
        [] l
      in
      (*Printf.printf "fm3 sn.sn_nb=%d, sn.state_nb=%d\n"
        sn.sn_nb sn.state_nb;*)
      let rec aux2 layout_matched l0 = match l0 with
        | act_id::t ->
          let ter_id =
            try fst pdev.main_lexer_actions.(act_id)
            with _ -> assert false
              (*fst pdev.regexp_actions.(act_id-
              (Array.length pdev.main_lexer_actions))*)
          in
          (*Printf.fprintf !log_channel
          "fm3 act_id=%d, ter_id=%d, token_nb=%d, layout_id=%d\n"
            act_id ter_id pdev.token_nb pdev.layout_id;
          Printf.fprintf !log_channel "ter: %s, act_id=%d\n"
            (try pdev.str_ter.(ter_id) with _ ->
            ("<"^(print_pretty_regexp
            pdev.regexp_array.(ter_id-(Array.length pdev.str_ter)))^">"))
            act_id;*)
          (*if sn.pdev.table_lex_trans.(pdev.token_nb*sn.state_nb+ter_id)*)
          if sn.pdev.table.(sn.state_nb).(0).(ter_id) <> -1 ||
            (sn.pdev.table.(sn.state_nb).(2).(ter_id) <> -1 && not layout)
          then
            ((*output_string !log_channel "selected by fm3\n";*)
            (Some (if all_token then (f select_unexpected l) else [act_id])),
              layout_matched)
          else if ter_id = pdev.layout_id then
            (*let _ = output_string !log_channel "fm3 layout matched\n" in*)
            if all_token then
              (Some (f select_unexpected l)), layout_matched
            else aux2 (Some [-act_id-1]) t (*(-1)*)
          else if select_unexpected then
            (Some (if all_token then (f true l) else [act_id])), layout_matched
          else ((*output_string !log_channel "not selected by fm3\n";*)
            aux2 layout_matched t)
        | [] -> (*Printf.fprintf !log_channel "fm3 None\n";*)
          None, layout_matched
      in
      let matched, layout_matched = aux2 None l in
        match matched with
        | Some _ -> matched
        | _ -> layout_matched
    in
    let act_id_l = List.fold_left
      (fun ail sn -> match aux1 sn l with
        | None -> ail
        | Some idl -> idl@ail)
      [] snl
    in
    match act_id_l with [] -> None
    | x ->
    let x = no_double (List.sort Pervasives.compare x) in
        (*Printf.printf "list x = %s\n"
        (String.concat " " (List.map (fun i -> string_of_int i) x));*)
        Some ((if all_token then x else [select_act_id x]), pdev)
  in
  let rec find_match2 aid_pdev_layout accu_layout = function
    | ([-1]::t1),(_::t2),(_::t3) ->
         (*Printf.fprintf !log_channel "find_match2 -1\n";*)
        find_match2 aid_pdev_layout accu_layout (t1,t2,t3)
    | (l::t1),(pdev::t2),(snl::t3) -> (match find_match3 pdev l snl with
      | None -> find_match2 aid_pdev_layout accu_layout (t1,t2,t3)
      | Some ((action_id_l, pdev) as x) ->
        if all_neg action_id_l then
          find_match2 (Some ([List.hd action_id_l],pdev))
            (snl@accu_layout) (t1,t2,t3)
        else Some (x, snl))
    | _ -> (match aid_pdev_layout with None -> None
      | Some (aid,pdev) -> Some ((aid, pdev), accu_layout))
  in
  let rec find_match1 = function
    | (p,l)::t ->
      if !dypgen_verbose>4 then
        Printf.fprintf !log_channel "pos=%d\n"
        (p - lexbuf.lb_lexbuf.lex_abs_pos);
      (match find_match2 None [] (List.rev l,pdev_l,snll) with
      | None -> find_match1 t
      | x ->
          lexbuf.lb_lexbuf.lex_curr_p <-
          { lexbuf.lb_lexbuf.lex_curr_p with Lexing.pos_cnum = p };
          lexbuf.lb_lexbuf.lex_curr_pos <-
            p - lexbuf.lb_lexbuf.lex_abs_pos;
          x)
    | [] -> None
  in
  let x = find_match1 res in
  (if !dypgen_verbose>2 then match x with
    | Some ((action_id_l, pdev), new_topmost) ->
    Printf.fprintf !log_channel
    "select_token ends, lexbuf.lb_lexbuf.lex_curr_p.pos_cnum = %d\n"
    lexbuf.lb_lexbuf.lex_curr_p.pos_cnum
    | None -> output_string !log_channel "select_token ends, no match\n");
  x



let rec lex_token topmost lexbuf layout all_token =
  let old_pos = lexbuf.lb_lexbuf.lex_curr_p.pos_cnum in
  match select_token topmost lexbuf all_token true false layout with
  | None -> None
  | Some ((action_id_l, pdev), new_topmost) ->
  let lexbuf = { lexbuf with lb_aux_lex = pdev.aux_lexer } in
  (match action_id_l with [i] when i<0 ->
    (if old_pos = lexbuf.lb_lexbuf.lex_curr_p.pos_cnum
    then failwith("lexing: empty token");
    (* regexp of layout are not allowed to match input of length = 0 *)
    if !dypgen_verbose>2 then
      output_string !log_channel "lex_token called bis\n";
    let _, action =
      try pdev.main_lexer_actions.(-i-1)
      with Invalid_argument _ -> assert false
    in
    (*Printf.printf "action_id=%d, ter=%d, layout_id=%d\n"
    action_id ter pdev.layout_id;*)
    let _ = action lexbuf in
    lex_token new_topmost lexbuf true all_token)
  | _ ->
  let ter_obj_l = List.fold_left
    (fun l action_id ->
      if action_id<0 then l else
      let ter, action =
        try pdev.main_lexer_actions.(action_id)
        with Invalid_argument _ -> assert false
      in
      (ter, action lexbuf)::l)
    [] action_id_l
  in
  Some (ter_obj_l, new_topmost, lexbuf, layout))



(* Note: next_lexeme may return incorrect result when called from an action that extends the grammar or that uses Keep_grammar. *)
let next_lexeme_precur_lb_pdev lexbuf pdev =
  let snl =
    [create_v 0 pdev 0 0 (Lexing.dummy_pos,Lexing.dummy_pos) 2 0]
  in
  let first_curr_pos = lexbuf.lb_lexbuf.lex_curr_pos in
  let rec next_token res =
    let prev_curr_pos = lexbuf.lb_lexbuf.lex_curr_pos in
    let x =
      try select_token snl lexbuf false false true false
      (* We can take false for all_token (2nd bool) because it doesn't
      change the lexeme. *)
      with Failure "lexing: empty token" -> None
    in
    match x with
    | None -> lexbuf.lb_lexbuf.lex_curr_pos <- first_curr_pos; res
    | Some ((action_id_l, pdev), _) ->
      let len = lexbuf.lb_lexbuf.lex_curr_pos - prev_curr_pos in
      let lexeme =
        String.sub lexbuf.lb_lexbuf.lex_buffer prev_curr_pos len in
      (match action_id_l with
        | [i] when i<0 ->
          (if !dypgen_verbose>2 then
            output_string !log_channel "next_lexeme called bis\n";
          next_token (lexeme::res))
        | _ -> lexbuf.lb_lexbuf.lex_curr_pos <- first_curr_pos;
          lexeme::res)
  in
  next_token []
(* The returned list contains all the layout lexemes matched and the non layout lexeme, in reverse order, i.e. the non layout lexeme first. *)



let print_regexp_array pdev =
  let toknb = Array.length pdev.str_ter in
  output_string !log_channel "regexp_array:\n";
  for i=0 to Array.length pdev.regexp_array -1 do
    Printf.fprintf !log_channel "%d:<%s>\n" (i+toknb)
    (print_pretty_regexp pdev.regexp_array.(i))
  done;
  output_string !log_channel "\n"

let print_main_lexer_actions mla =
  output_string !log_channel "main_lexer_actions:\n";
  for i=0 to Array.length mla -1 do
    Printf.fprintf !log_channel "%d -> %d\n" i (fst mla.(i))
  done;
  output_string !log_channel "\n"



let lexparse parser_pilot (entry_point:string)
    ?global_data ?local_data
    ?(match_len=`longest)
    ?(keep_data=`both)
    ?(choose_token=`first)
    ?(use_rule_order=false)
    ?(use_all_actions=false)
    lexbuf =
  
  let time1 = Sys.time () in
  
  let counters, topmost, parsing_device, ppar =
    init_parser parser_pilot entry_point global_data local_data
    None lexbuf keep_data use_rule_order use_all_actions
  in
  
  if !dypgen_verbose>4 then
    (print_dec_table parsing_device.main_lexer_table.tbl_trans;
    print_final parsing_device.main_lexer_table.tbl_final;
    print_regexp_array parsing_device;
    print_main_lexer_actions parsing_device.main_lexer_actions);
  
  let all_token = match choose_token with
    `first -> false | `all -> true in
  
  let next_lexeme_precursor pdev =
    next_lexeme_precur_lb_pdev lexbuf pdev in
  
  (*let max_nodes_count = ref 0 in*)
  
  let rec aux_LR0 lexbuf topmost prev_parse_result =
    if !dypgen_verbose>2 then
      (Printf.fprintf !log_channel "prev_parse_result length = %d\n"
      (List.length prev_parse_result); flush_all ());
    counters.count_token <- counters.count_token + 1;
    clean_topmost topmost;
    match lex_token topmost lexbuf false all_token with
    | None -> (match prev_parse_result with
      | _::_ -> prev_parse_result
      | _ -> raise Syntax_error)
    | Some (ter_obj_l, topmost, lexbuf, layout) ->
    if layout then counters.last_layout <- counters.count_token - 1;
    if !dypgen_verbose>2 then
      (List.iter (fun (ter_id,_) -> Printf.fprintf !log_channel
       "\nTOKEN : %s, ter_id=%d\n"
       (try (List.hd topmost).pdev.str_ter.(ter_id) with _ ->
         (let pdev = (List.hd topmost).pdev in
         Printf.sprintf "<regexp:%s>"
         (print_pretty_regexp pdev.regexp_array.(ter_id-
           (Array.length pdev.str_ter)))))
        ter_id) ter_obj_l;
       Printf.fprintf !log_channel
       "size of topmost = %d lexbuf.lb_curr_p.pos_cnum = %d\n"
         (List.length topmost)
        lexbuf.lb_lexbuf.lex_curr_p.pos_cnum;
        flush_all ());
    (*Printf.printf "1 topmost length=%d\n" (List.length topmost);*)
    let topmost = List.fold_left
      (fun new_topmost (ter,obj) ->
        (do_shifts ter obj topmost lexbuf counters ppar layout
          (Dyplex.lexeme lexbuf) next_lexeme_precursor)
        @new_topmost)
      [] ter_obj_l
    in
    let next_lexeme_precursor pdev =
      next_lexeme_precur_lb_pdev lexbuf pdev in
    let topmost, parse_result =
      do_reductions topmost counters ppar next_lexeme_precursor
    in
    (*let nodes_count = count_nodes topmost in
    if nodes_count > !max_nodes_count then max_nodes_count:=nodes_count;*)
    (*Printf.printf "topmost length = %d\n" (List.length topmost);
    Printf.printf "count_nodes = %d\n" nodes_count;*)
    if !dypgen_verbose>2 then
    Printf.fprintf !log_channel "parse_result length = %d\n"
      (List.length parse_result);
    if topmost = []
    then
      (match parse_result with _::_ -> parse_result
      | _ ->
        (match prev_parse_result with
         | _::_ -> prev_parse_result
         | _ -> raise Syntax_error))
    else (match match_len, parse_result with
      | `shortest, _::_ -> parse_result
      | _, _::_ -> aux_LR0 lexbuf topmost parse_result
      | _ -> aux_LR0 lexbuf topmost prev_parse_result)
  in
  
  let parse_result =
    try (
      let topmost, _ = (* reduces initial epsilon rules *)
        do_reductions topmost counters ppar next_lexeme_precursor
      in
        aux_LR0 lexbuf topmost [])
    with Syntax_error ->
      (flush_all ();
      if !dypgen_verbose>2 then close_out !log_channel;
      raise Syntax_error)
  in
  
  end_of_parsing_info time1 counters;
(*   Printf.printf "max_count_nodes = %d\n" !max_nodes_count; *)
  
  parse_result
