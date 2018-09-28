(* Extensible lexer: one can define new terminals or extend existing ones but not redefine nor remove them. *)

include Automaton

(*let soc c = let x = " " in x.[0] <- c; x*)

let any_char = (0, 255)

let interval_diff (a1,b1) (a2,b2) =
  if a1 < a2 then
    if b1 <= b2 then [a1, min (a2-1) b1]
    else [(a1, a2-1);(b2+1, b1)]
  else
    if b1 <= b2 then []
    else [max a1 (b2+1), b1]

let diff cs1 cs2 =
  let rec aux cs1 ci2 =
    List.fold_left (fun l ci1 -> (interval_diff ci1 ci2)@l) [] cs1
  in
  List.fold_left aux cs1 cs2

let interval_inter (a1,b1) (a2,b2) =
  if b1<a2 || b2<a1 then None
  else Some (max a1 a2, min b1 b2)

let regexp_of_string s =
  let rec aux i l = if i= -1 then l else
    aux (i-1) ((RE_Char s.[i])::l)
  in
  let len = String.length s in
  if len = 0 then
    failwith "Lexer generator: empty string in regular expression"
  else RE_Seq (aux (len-1) [])

let norm_cs cs = List.map
  (fun (a,b) ->
    let a, b = int_of_char a, int_of_char b in
    min a b, max a b) cs

let check_cs cs = List.map
  (fun (a,b) -> if a>b then failwith "check_cs") cs



let disjoint cs = match cs with [] | [_] -> cs | _ ->
  let cs = List.sort (fun (a,_) (b,_) -> compare a b) cs in
  let rec aux h l accu = match l with
  | [] -> h::accu
  | ((a2,b2) as h2)::t -> let a1, b1 = h in
      if b1>=a2-1 then aux (a1, max b1 b2) t accu
      else aux h2 t (h::accu) in
  match cs with h::t -> List.rev (aux h t []) | _ -> assert false



let str_int_list l =
  let l = List.map (fun i -> string_of_int i) l in
  String.concat " " l

let list_of_set s = Int_set.fold (fun x l -> x::l) s []

let str_int_set s = str_int_list (list_of_set s)

let str_intc_list l =
  String.concat " "
  (List.map (fun (a,b) ->
    (string_of_int a)^"-"^(string_of_int b)) l)

let str_trans (l,n) =
  Printf.sprintf "\n    <%d> %s" n.id (str_intc_list l)

let str_eps n =
  Printf.sprintf "\n    <%d>" n.id

let str_trans_list l =
  String.concat "" (List.map str_trans l)

let str_eps_list l =
  String.concat "" (List.map str_eps l)


let print_node s =
  Printf.fprintf !log_channel
  "State [%d]\n  trans=%s\n  eps=%s\n  matched=%s\n\n"
  s.id
  (str_trans_list s.trans)
  (str_eps_list s.eps)
  (str_int_set s.matched)

let prn n =
  Printf.fprintf !log_channel
  "Node [%d]\n  trans=%s\n  eps=%s\n  matched=%s\n\n"
  n.id
  (str_trans_list n.trans)
  (str_int_list (List.map (fun n -> n.id) n.eps))
  (str_int_set n.matched);
  n

let print_nfa s =
  let rec aux s visited =
    if not (Int_set.mem s.id visited) then
      (print_node s;
      let visited =
        List.fold_left (fun visited (_, n) -> aux n visited)
        (Int_set.add s.id visited) s.trans
      in
      List.fold_left (fun visited n -> aux n visited) visited s.eps)
    else visited
  in
  let _ = aux s Int_set.empty in
  ()


let build_nfa table =
  let rec aux = function
  | RE_Char c ->
      let c = int_of_char c in
      (fun succ id -> (*prn*) { id = id; trans = [[c,c], succ]; eps = [];
       matched = Int_set.empty }, id+1)
  | RE_Char_set cs ->
      let cs = disjoint (norm_cs cs) in
      (fun succ id -> (*prn*) { id = id; trans = [cs, succ];
       eps = []; matched = Int_set.empty }, id+1)
  | RE_Char_set_exclu cs ->
      let cs = disjoint (diff [any_char] (norm_cs cs)) in
      (fun succ id ->
      (*prn*) { id = id;
      trans = [cs, succ];
      eps = []; matched = Int_set.empty }, id+1)
  | RE_String s -> aux (regexp_of_string s)
  | RE_Alt rl ->
      let fl = aux_list rl in
      (fun succ id ->
      let nl, id = List.fold_left
        (fun (nl, id) f ->
          let n, id = f succ id in n::nl, id)
        ([], id) fl in
      (*prn*) { id = id; trans = [];
       eps = nl;
       matched = Int_set.empty }, id+1)
  | RE_Seq rl ->
      let fl = aux_list rl in
      (fun succ id -> List.fold_left
        (fun (n, id) f -> f n id) (succ, id) fl)
  | RE_Star r ->
      let f = aux r in
      (fun succ id ->
      let n = { id = id; trans = []; eps = [];
        matched = Int_set.empty } in
      let s, id1 = f n (id+1) in
      n.eps <- [s;succ];
      (*prn*) n, id1)
  | RE_Plus r ->
      let f = aux r in
      (fun succ id ->
      let n = { id = id; trans = []; eps = [];
        matched = Int_set.empty } in
      let s, id1 = f n (id+1) in
      n.eps <- [s;succ];
      (*prn n;*)
      (*prn*) s, id1)
  | RE_Option r ->
      let f = aux r in
      (fun succ id ->
      let s, id = f succ id in
      (*prn*) { id = id; trans = []; eps = [s;succ];
        matched = Int_set.empty }, id+1)
  | RE_Name s -> Hashtbl.find table s
  | RE_Eof_char ->
      (fun succ id ->
      (*prn*) { id = id; trans = [[256,256], succ]; eps = [];
        matched = Int_set.empty }, id+1)
  and aux_list rl =
    List.fold_left (fun fl r -> (aux r)::fl) [] rl in
  function r -> aux r


let make_nfa r table regexp_id id =
  let f = build_nfa r table in
  f ((*prn*) { id = id; trans = []; eps = [];
    matched = Int_set.add regexp_id Int_set.empty })
    (id+1)


let compile_regexp_decl rl =
  let table = Hashtbl.create (List.length rl) in
  (*print_endline "regexp_decl:\n";*)
  List.iter (fun (name, r) ->
    (*Printf.printf "%s : %s\n" name (print_pretty_regexp r);*)
    let f = build_nfa table r in
    (*let n, _ =
      f { id = 0; trans = []; eps = [];
      matched = Int_set.add (-1) Int_set.empty } 1
    in
    print_nfa n;*)
    Hashtbl.add table name f)
    rl;
  (*print_newline ();*)
  table


let print_tl tl =
  Printf.printf "disjoint_tl called, trans list:\n";
  List.iter (fun (cil,n) ->
    Printf.printf "node id: %d\n" n.id;
    Printf.printf "  %s\n" (str_intc_list cil))
  tl


let print_disjoint_tl_res res =
  Printf.printf "disjoint_tl result:\n";
  List.iter (fun ((a,b),nl) ->
    Printf.printf "character interval: %d-%d\n" a b;
    Printf.printf "  %s\n"
      (str_int_list (List.map (fun n -> n.id) nl)))
  res


module Ordered_node =
struct
  type t = node
  let compare n1 n2 = Pervasives.compare n1.id n2.id
end
module Node_set = Set.Make(Ordered_node)


let list_of_ns ns =
  Node_set.fold (fun n l -> n::l) ns []

let ci_begin = Array.make 257 []
let ci_end = Array.make 257 []

let disjoint_tl tl scount =
  List.iter (fun (il,n) ->
    List.iter (fun (a,b) ->
      ci_begin.(a) <- n::ci_begin.(a);
      ci_end.(b) <- n::ci_end.(b)) il) tl;
  
  let rec aux1 i res =
    if i=257 then res else
    match ci_begin.(i) with
    | [] -> aux1 (i+1) res
    | l ->
        let new_ns = List.fold_left
          (fun ns n ->
            scount.(n.id) <- scount.(n.id)+1;
            if scount.(n.id)=1 then
              Node_set.add n ns else ns) Node_set.empty l
        in
        aux2 i i new_ns res
  
  and aux2 i inf ns res =
    match ci_end.(i) with
    | [] -> aux3 (i+1) inf ns res
    | l ->
        let new_ns = List.fold_left
          (fun ns n ->
            scount.(n.id) <- scount.(n.id)-1;
            if scount.(n.id)=0 then
              Node_set.remove n ns else ns) ns l
        in
        let nl = list_of_ns ns in
        if i=256 then ((inf,i),nl)::res
        else if Node_set.is_empty new_ns then
          aux1 (i+1) (((inf,i),nl)::res)
        else aux3 (i+1) (i+1) new_ns (((inf,i),nl)::res)
  
  and aux3 i inf ns res =
    match ci_begin.(i) with
    | [] -> aux2 i inf ns res
    | l ->
        let new_ns = List.fold_left
          (fun ns n ->
            scount.(n.id) <- scount.(n.id)+1;
            if scount.(n.id)=1 then
              Node_set.add n ns else ns) ns l
        in
        if inf=i then aux2 i i new_ns res else
        aux2 i i new_ns (((inf,i-1),list_of_ns ns)::res)
  in
  
  let res = aux1 0 [] in
  
  List.iter (fun (il,n) ->
    List.iter (fun (a,b) ->
      ci_begin.(a) <- [];
      ci_end.(b) <- []) il) tl;
  
  res


module Ordered_int_set =
struct
  type t = Int_set.t
  let compare = Int_set.compare
end
module State_map = Map.Make(Ordered_int_set)



let print_node_list sl =
  Printf.fprintf !log_channel "States list:\n\n";
  List.iter print_node sl



let union_matched nl filter_matched =
  List.fold_left
  (fun s n -> Int_set.union s (filter_matched n))
  Int_set.empty nl

let make_dfa state_count_array filter_matched =
  
  let rec make_state (next,sl,sm,id) ((ci,nl),ids) =
    try
      let s = State_map.find ids sm in
      ([ci],s)::next, sl, sm, id
    with Not_found ->
      let s = {
        id = id;
        trans = [];
        eps = [];
        matched = union_matched nl filter_matched } in
      let new_next, sl, sm, id =
        let sm = State_map.add ids s sm in
        make_next nl sl sm (id+1) in
      s.trans <- new_next;
      ([ci],s)::next, s::sl, sm, id
  
  and make_next nl sl sm id =
    (*Printf.printf "make_next pour %d\n" (id-1); print_node_list nl;*)
    
    let l =
      let dtlres =
        (disjoint_tl (List.concat (List.map (fun n -> n.trans) nl))
          state_count_array)
      in
      
      (*print_disjoint_tl_res dtlres;*)
      
      List.map
      (fun (ci,nl) -> let nl, ids = epsilon_closure nl in (ci,nl), ids)
      dtlres in
    
    let res = List.fold_left make_state ([],sl,sm,id) l in
    
    (*(print_endline "make_next ends";
    let a,_,_,_ = res in
    Printf.printf "result = %s\n" (str_trans_list a));*)
    
    res
  
  and epsilon_closure nl =
    let rec aux (accu,id_set) n =
      if Int_set.mem n.id id_set then accu, id_set
      else
        List.fold_left
          aux (n::accu, Int_set.add n.id id_set) n.eps
    in
    List.fold_left aux ([],Int_set.empty) nl
  in
  
  function nfa_start_list ->
    (*Printf.printf "nfa_start_list length = %d\n"
      (List.length nfa_start_list);*)
    let nl, ids = epsilon_closure nfa_start_list in
    let matched = union_matched nl filter_matched in
    let start = {
      id = 0;
      trans = [];
      eps = [];
      matched = matched } in
    let next, sl, _, snb =
      let smap = State_map.add ids start State_map.empty in
      make_next nl [start] smap 1 in
    start.trans <- next;
    start, sl, snb



let write_interval (a,b) i dec_table next_id =
  for j=i*257+a to i*257+b do
    dec_table.(j) <- next_id
  done


let print_dec_table dt =
  Printf.fprintf !log_channel "Transition table\n";
  for i=0 to Array.length dt -1 do
    Printf.fprintf !log_channel "  (%d,%d):%d\n"
    (i/257) (i mod 257) dt.(i)
  done;
  Printf.fprintf !log_channel "\n"

let print_final f =
  Printf.fprintf !log_channel "Final table\n";
  for i=0 to Array.length f -1 do
    Printf.fprintf !log_channel "  state %d:%s\n" i
    (str_int_list f.(i))
  done;
  Printf.fprintf !log_channel "\n\n"


let make_lexer build_nfa_table =
  function [] ->
    let dummy_node = { id=0; trans=[]; eps=[]; matched=Int_set.empty } in
    { tbl_trans = [||] ;
    tbl_final = [||];
    tbl_notrans = [||] },
    dummy_node
  | rl ->
  (*Printf.printf "rl length = %d\n" (List.length rl);*)
  (*print_endline "main lexer:\n";*)
  let nfa_list,_,nfa_state_nb = List.fold_left
    (fun (nfa_l,regexp_id,node_id) regexp ->
      let nfa, node_id =
        make_nfa build_nfa_table regexp regexp_id node_id in
      (*Printf.printf "%d : %s\n" regexp_id (print_pretty_regexp regexp);
      print_nfa nfa;*)
      nfa::nfa_l,
      (regexp_id+1), node_id)
    ([],0,0) rl
  in
  let state_count_array = Array.make nfa_state_nb 0 in
  let start, sl, snb =
    make_dfa state_count_array (fun n -> n.matched) nfa_list in
  
  if !dypgen_verbose>4 then print_node_list sl;
  
  let state_nb = List.length sl in
  let dec_table = Array.make (state_nb*257) (-1) in
  (* This is the decision table, 256 indices for characters and
  the last one for eof. *)
  let final = Array.make state_nb [] in
  let notrans = Array.make state_nb false in
  
  let _ = List.iter
    (fun n ->
      (match n.trans with [] -> notrans.(n.id) <- true
      |  trans_l -> List.iter
        (function ([ci],n1) -> write_interval ci n.id dec_table n1.id
          | _ -> assert false)
        trans_l);
      final.(n.id) <- List.sort Pervasives.compare (list_of_set n.matched)
      (* Is it necessary to sort the list or doesn't
       list_of_set already do it? *))
    sl in
  
  if !dypgen_verbose>4 then
    (print_dec_table dec_table;
    print_final final);
  
  { tbl_trans = dec_table ; tbl_final = final ;
    tbl_notrans = notrans }, start


let extend_lexer main_lexer_start regexp_list build_nfa_table node_nb regexp_nb =
  
  let aux_nfa (nfa_l,regexp_id,node_id) regexp =
    let nfa, node_id =
      make_nfa build_nfa_table regexp regexp_id node_id in
    nfa::nfa_l, (regexp_id+1), node_id
  in
  
  let nfa_list, fst_regexp_id, nfa_state_nb =
    List.fold_left aux_nfa ([main_lexer_start],0,node_nb)
      (fst regexp_list) in
  let nfa_list, curr_regexp_nb, nfa_state_nb =
    List.fold_left aux_nfa
      (nfa_list,regexp_nb+fst_regexp_id,node_nb+nfa_state_nb)
      (snd regexp_list) in
  
  let state_count_array = Array.make nfa_state_nb 0 in
  let filter_matched n =
    if n.id < node_nb then
      Int_set.fold (fun i s -> Int_set.add (i+fst_regexp_id) s)
      n.matched Int_set.empty
    else n.matched
  in
  let start, sl, snb =
    make_dfa state_count_array filter_matched nfa_list in
  
  if !dypgen_verbose>4 then print_node_list sl;
  
  let state_nb = List.length sl in
  let dec_table = Array.make (state_nb*257) (-1) in
  (* This is the decision table, 256 indices for characters and
  the last one for eof. *)
  let final = Array.make state_nb [] in
  let notrans = Array.make state_nb false in
  
  let _ = List.iter
    (fun n ->
      (match n.trans with [] -> notrans.(n.id) <- true
      |  trans_l -> List.iter
        (function ([ci],n1) -> write_interval ci n.id dec_table n1.id
          | _ -> assert false)
        trans_l);
      final.(n.id) <- List.sort Pervasives.compare (list_of_set n.matched)
      (* Is it necessary to sort the list or doesn't
       list_of_set already do it? *))
    sl in
  
  if !dypgen_verbose>4 then
    (print_dec_table dec_table;
    print_final final);
  
  { tbl_trans = dec_table ; tbl_final = final ;
    tbl_notrans = notrans }, start




open Lexing

let lexeme dyplexbuf =
  Lexing.lexeme dyplexbuf.lb_lexbuf
let lexeme_char dyplexbuf i =
  Lexing.lexeme_char dyplexbuf.lb_lexbuf i
let lexeme_start dyplexbuf =
  Lexing.lexeme_start dyplexbuf.lb_lexbuf
let lexeme_end dyplexbuf =
  Lexing.lexeme_end dyplexbuf.lb_lexbuf
let lexeme_start_p dyplexbuf =
  Lexing.lexeme_start_p dyplexbuf.lb_lexbuf
let lexeme_end_p dyplexbuf =
  Lexing.lexeme_end_p dyplexbuf.lb_lexbuf
let flush_input dyplexbuf =
  Lexing.flush_input dyplexbuf.lb_lexbuf



let lex_engine is_main_lexer tbl_list (lexbuf:Lexing.lexbuf) reset_start_pos =
  (*let input_string = dyplexbuf.lb_string in*)
  if reset_start_pos then
    (lexbuf.lex_start_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_start_p <- lexbuf.lex_curr_p);
  (*let curr_pos = lexbuf.lex_curr_pos in*)
  if !dypgen_verbose>4 then
    (Printf.printf "lex_engine begins: curr_pos = %d\n"
    lexbuf.lex_curr_pos);
  let add_final_p, add_final = if is_main_lexer
    then (fun a b -> a::b), (fun a b -> a::b)
    else (fun a _ -> [[List.hd a]]), (fun (a,b) _ -> [a,[List.hd b]])
  in
  let lex_nb = List.length tbl_list in
  
  let rec aux state_list final valid_lex =
    let aux_final (final_p,valid_lex,matched) tbl state =
      if state = -1 then
        [-1]::final_p,valid_lex,matched else
      let final_p, matched = match tbl.tbl_final.(state) with
        | [] -> [-1]::final_p, matched
            (* -1 is useful as a placeholder when the list will be read
            in the function select_token *)
        | lf ->
          if !dypgen_verbose>4 then
            (Printf.fprintf !log_channel "add_final_p : %s\n"
            (str_int_list lf));
          add_final_p lf final_p, true
      in
      let valid_lex =
        if (try tbl.tbl_notrans.(state)
           with _ -> false)
        then valid_lex-1 else valid_lex
      in
      final_p, valid_lex, matched
    in
    let final_p, valid_lex, matched =
      List.fold_left2 aux_final ([], valid_lex, false)
      tbl_list state_list in
    let final =
      if matched
      then
        let abs_curr_pos = lexbuf.lex_abs_pos + lexbuf.lex_curr_pos in
        add_final (abs_curr_pos,final_p) final
      (* The position here is useful information: in lex_token
      lex_curr_p.pos_cnum will be set to the position recorded here. *)
      else final
    in
    if valid_lex = 0 then
      match final with _::_ -> final
      | [] -> failwith("lexing: empty token")
    else
    let c =
      let b = (lexbuf.lex_curr_pos = lexbuf.lex_buffer_len) in
      if b then
        lexbuf.refill_buff lexbuf;
      if b && lexbuf.lex_eof_reached then 256
      else
        let p = lexbuf.lex_curr_pos in
        lexbuf.lex_curr_pos <- p+1;
        if !dypgen_verbose>4 then
          (Printf.fprintf !log_channel
          "lex_engine reads: `%c'\n" (Bytes.get lexbuf.lex_buffer p));
        try Char.code (Bytes.get lexbuf.lex_buffer p)
        with Invalid_argument("index out of bounds")
        -> (Printf.printf "%d, %d, %s, %d, %d\n"
         lexbuf.lex_curr_pos lexbuf.lex_buffer_len
         (string_of_bool reset_start_pos)
         p (Bytes.length lexbuf.lex_buffer);
        raise (Invalid_argument("index out of bounds")))
    in
    let aux_lex (new_state_list,valid_lex) tbl state =
      if state = -1 then
        ((*Printf.printf "next_state = -1\n";*)
        (-1)::new_state_list,valid_lex) else
      let next_state = tbl.tbl_trans.(state*257+c) in
      if !dypgen_verbose>4 then
        (Printf.fprintf !log_channel "next_state = %d\n" next_state);
      let valid_lex =
        if (try (next_state = -1 && not tbl.tbl_notrans.(state))
           with _ -> false)
        then valid_lex-1 else valid_lex
      in
      next_state::new_state_list, valid_lex
    in
    let new_state_list, valid_lex =
      List.fold_left2 aux_lex ([], valid_lex)
      tbl_list state_list in
    let new_state_list = List.rev new_state_list in
    if valid_lex = 0 then
      match final with _::_ -> final
      | [] -> failwith("lexing: empty token")
    else
      aux new_state_list final valid_lex
  in
  
  let l0 = List.map (fun _ -> 0) tbl_list in
  aux l0 [] lex_nb



let lex lexer_name argl dyplexbuf =
  let table = Hashtbl.find dyplexbuf.lb_aux_lex.aux_lexer_table lexer_name in
  match lex_engine false [table] dyplexbuf.lb_lexbuf true
  with [p,[[final]]] ->
  (*dyplexbuf.lb_curr_p <- { dyplexbuf.lb_curr_p with Lexing.pos_cnum = p };*)
  dyplexbuf.lb_lexbuf.lex_curr_p <-
  { dyplexbuf.lb_lexbuf.lex_curr_p with Lexing.pos_cnum = p };
  dyplexbuf.lb_lexbuf.lex_curr_pos <-
    p - dyplexbuf.lb_lexbuf.lex_abs_pos;
  (*Printf.printf "lex: curr_pos = %d\n" p;*)
  let action = (Hashtbl.find
    dyplexbuf.lb_aux_lex.aux_lexer_actions lexer_name).(final) in
  action argl dyplexbuf
  | _ -> assert false



let zero_position = {
  Lexing.pos_fname = "" ;
  Lexing.pos_lnum = 0 ;
  Lexing.pos_bol = 0 ;
  Lexing.pos_cnum = 0 ;
}


(*let make_lexbuf pp str = {
  lb_curr_p = zero_position;
  lb_start_p = zero_position;
  lb_string = str;
  lb_aux_lex = (fst pp).aux_lexer }*)


let from_string pp str = {
  lb_lexbuf = Lexing.from_string str;
  lb_aux_lex = pp.pp_dev.aux_lexer }

let from_channel pp ic = {
  lb_lexbuf = Lexing.from_channel ic;
  lb_aux_lex = pp.pp_dev.aux_lexer }

let from_function pp f = {
  lb_lexbuf = Lexing.from_function f;
  lb_aux_lex = pp.pp_dev.aux_lexer }


let dyplex_lexbuf_position dyplexbuf =
  dyplexbuf.lb_lexbuf.lex_start_p,
  dyplexbuf.lb_lexbuf.lex_curr_p

let std_lexbuf dyplexbuf = dyplexbuf.lb_lexbuf

let set_newline dyplexbuf =
  let l = std_lexbuf dyplexbuf in
  let pos = l.lex_curr_p in
  let npos = { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum } in
  l.lex_curr_p <- npos

let set_fname dyplexbuf fname =
  let l = std_lexbuf dyplexbuf in
  let pos = l.lex_curr_p in
  let npos = { pos with pos_fname = fname } in
  l.lex_curr_p <- npos;
  let pos = l.lex_start_p in
  let npos = { pos with pos_fname = fname } in
  l.lex_start_p <- npos
