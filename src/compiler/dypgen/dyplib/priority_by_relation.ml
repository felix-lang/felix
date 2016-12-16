type priority = int

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
          automaton (see below create_automaton).
            The Toeq constructor behaves the same way except that it also 
          accepts pc1 for priority class of the substring even if we don't
          have pc1 -> pc1 in the priority set. *)

let str_ntp ntp = match ntp with
  | No_priority -> "No_priority"
  | Eq_priority p -> "="^(string_of_int p)
  | Less_priority p -> "<"^(string_of_int p)
  | Lesseq_priority p -> "<="^(string_of_int p)
  | Greater_priority p -> ">"^(string_of_int p)
  | Greatereq_priority p -> ">="^(string_of_int p)

let start_priority = No_priority

module OrdPrio =
struct
  type t = priority
  let compare = Pervasives.compare
end
module Ordered_string =
struct
  type t = string
  let compare = Pervasives.compare
end

module Prio_set = Set.Make(OrdPrio)
module Prio_map = Map.Make(OrdPrio)
module String_map = Map.Make(Ordered_string)

type priority_data = {
  prd_rel : (Prio_set.t * Prio_set.t) array;
  prd_ind : (string, int) Hashtbl.t;
  prd_names : string array;
  prd_nb : int }
(* This is a map from a priority to a couple of priority sets :
p to (ps1,ps2) where ps1 is the set of all priorities q s.a. q<p
and ps2 is the set of all priorities r s.a. p<r
The string_map maps the string of a priority to its int value.
int is the number of priorities. *)

(* REMARQUE : Puisque les priorités sont des entiers, la structure qu'il nous faut c'est un tableau à 2 entrées :
prio_dat.(p).(q) = true <=> p<q *)



(* this set p1<p2 true if b=true and false if b=false *)
let set_relation priodat b p1 p2 =
  let (ps1,ps2) = priodat.prd_rel.(p1) in
  let (ps3,ps4) = priodat.prd_rel.(p2) in
  let (ps2,ps3) =
    if b then (Prio_set.add p2 ps2),(Prio_set.add p1 ps3)
    else (Prio_set.remove p2 ps2),(Prio_set.remove p1 ps3)
  in
  priodat.prd_rel.(p1) <- (ps1,ps2);
  priodat.prd_rel.(p2) <- (ps3,ps4)
  (*let prd_rel =
    Prio_map.add p2 (ps3,ps4) (Prio_map.add p1 (ps1,ps2) priodat.prd_rel)
  in
  { priodat with prd_rel = prd_rel }*)

(*let insert_priority priodat str =
  try
    let p = Hashtbl.find priodat.prd_ind str in
    (priodat,p)
  with Not_found ->
    let p = priodat.prd_nb in
    let ind = Hashtbl.add priodat.prd_ind str p in
    let rel = Prio_map.add p (Prio_set.empty,Prio_set.empty) priodat.prd_rel in
    { prd_rel = rel ; prd_ind = ind ; prd_nb = (p+1) },p*)

let find_priority priodat str =
  Hashtbl.find priodat.prd_ind str

(*let default_priority = 0
let empty_priority_data =
{ prd_rel = Prio_map.empty ; prd_name = Hashtbl.create 30; prd_nb = 0 }
let empty_priority_data = fst (insert_priority empty_priority_data "default_priority")*)

let is_relation priodat p1 p2 =
  let (_,ps) = priodat.prd_rel.(p1) in
  Prio_set.mem p2 ps


let update_priority priodat ppbl =
  let aux (p1,p2,b) = set_relation priodat b p1 p2 in
  List.iter aux ppbl
(** update_priority ps [pc1,pc2,true]
adds the binary relation pc1 -> pc2 to ps
update_priority ps [pc1,pc2,false]
removes the relation pc1 -> pc2 from ps if it exists. *)

(* used for p1<p2<p3<...<pn *)
let add_list_relations priodat l =
  let iterfun p1 p2 = set_relation priodat true p1 p2 in
  let rec aux p1 l priodat = match l with
    | [p2] -> set_relation priodat true p1 p2
    | p2::tl ->
        List.iter (iterfun p1) l;
        aux p2 tl priodat
    | [] -> failwith "add_list_relation"
  in
  aux (List.hd l) (List.tl l) priodat



(* does the same as the previous except that there is the reflexivity *)
(*let add_list_relations_order priodat l =
  let foldfun p1 priodat p2 = set_relation priodat true p1 p2 in
  let rec aux p1 l priodat = match l with
    | [p2] -> set_relation priodat true p1 p2
    | p2::tl ->
        let priodat = List.fold_left (foldfun p1) priodat l in
        aux p2 tl priodat
    | [] -> failwith "add_list_relation"
  in
  aux (List.hd l) l priodat*)
