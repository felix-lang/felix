open Flx_btype
open Flx_kind
open Flx_bid

(* used by overload *)

(* handles typeset and type match integration somehow *)

let is_typeset tss1 =
  match List.rev tss1 with
  | [] -> false
  | (p1,v1) ::t ->
    p1.assignments = [] &&
    BidSet.cardinal p1.pattern_vars = 1 &&
    match p1.pattern,v1 with 
    | BTYP_type_var (i,KIND_type), BTYP_void
      when i = BidSet.choose p1.pattern_vars ->
      begin try 
        List.iter (fun (p,v) -> match p,v with
        | { assignments=[]; 
            pattern_vars=pvs; 
            pattern=BTYP_inst (_,[],_)
          },
          BTYP_tuple [] when BidSet.is_empty pvs -> ()
        | _ -> raise Not_found
        )
        t;
        true
      with Not_found -> false
      end
    | _ -> false

let make_typeset tss : bid_t list =
  match List.rev tss with
  | h::t -> List.map (fun x ->
    match x with 
    | {pattern=BTYP_inst (i,[],_)},_ -> i
    | _ -> assert false
    ) 
    t
  | _ -> assert false

let is_subset tss1 tss2 : bool =
  let tss1: bid_t list = make_typeset tss1
  and tss2: bid_t list = make_typeset tss2 in
  try List.iter (fun x -> if not (List.mem x tss2) then raise Not_found) tss1; true
  with Not_found -> false

(* this routine checks that the second list of cases includes the first,
 * which means the first implies the second. This means, every case
 * in the first list must be in the second list. The order must agree
 * as well, since typematches are ordered.
 *)

let rec scancases bsym_table counter tss1 tss2 = match (tss1, tss2) with
  | [],_ -> true
  | _,[] -> false
  | (p1,v1)::t1 as c1, (p2,v2)::t2  ->
    if p1.assignments = [] 
    && p2.assignments = []
    then
      if BidSet.is_empty (p1.pattern_vars)
      && BidSet.is_empty (p2.pattern_vars)
      then
        if Flx_unify.type_eq bsym_table counter p1.pattern p2.pattern
        && Flx_unify.type_eq bsym_table counter v1 v2
        then scancases bsym_table counter t1 t2 (* advance both *)
        else scancases bsym_table counter c1 t2 (* skip rhs case *)
      (* special case of wildcard, somewhat hacked *)
      else match p1.pattern,p2.pattern with
      | BTYP_type_var _, BTYP_type_var _ ->
         if Flx_unify.type_eq bsym_table counter v1 v2
         then scancases bsym_table counter t1 t2 (* advance both *)
         else scancases bsym_table counter c1 t2 (* skip rhs case *)
      | BTYP_type_var _,_ -> scancases bsym_table counter c1 t2 (* skip rhs case *)
      | _ -> false
   else false

let typematch_implies (bsym_table:Flx_bsym_table.t) counter a b = match a, b with
  | BTYP_type_match (v1,tss1), BTYP_type_match (v2,tss2) ->
     Flx_unify.type_eq bsym_table counter v1 v2 &&
     if is_typeset tss1 && is_typeset tss2 
     then is_subset tss1 tss2
     else scancases bsym_table counter tss1 tss2
  | _ -> false

let factor_implies (bsym_table:Flx_bsym_table.t) counter ls b =
  try 
    List.iter (fun a ->
      if Flx_unify.type_eq bsym_table counter a b then raise Not_found
      else if typematch_implies bsym_table counter a b then raise Not_found
    ) 
    ls;
    false
  with Not_found -> true

let terms_imply (bsym_table:Flx_bsym_table.t) counter ls1 ls2 =
  try
    List.iter (fun b ->
      if not (factor_implies bsym_table counter ls1 b) then raise Not_found
    )  
    ls2;
    true
   with Not_found -> false

let rec split_conjuncts' t =
  match t with
  | BTYP_intersect ls ->
    List.concat (List.map split_conjuncts' ls)
  | _ -> [t]

let filter_out_units ls = 
   List.filter (fun x -> x <> btyp_tuple []) ls

let split_conjuncts ls = filter_out_units (split_conjuncts' ls)

let constraint_implies (bsym_table:Flx_bsym_table.t) counter a b =
  let r = terms_imply bsym_table counter (split_conjuncts a) (split_conjuncts b) in
  r


