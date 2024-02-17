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
    | BTYP_type_var (i,_,KIND_type), BTYP_void
      when i = BidSet.choose p1.pattern_vars ->
      begin try 
        List.iter (fun (p,v) -> match p,v with
        | { assignments=[]; 
            pattern_vars=pvs; 
            pattern=BTYP_inst (_,_,_,[],_)
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
    | {pattern=BTYP_inst (_,_,i,[],_)},_ -> i
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

let rec scancases bsym_table counter tss1 tss2 = 
  match (tss1, tss2) with
  | [],_ ->  (* print_endline ("LHS OUT OF CASES, TRUE"); *) true
  | _,[] -> (* print_endline ("RHS OUT OF CASES, FALSE"); *) false
  | (p1,v1)::t1 as c1, (p2,v2)::t2  ->
    if p1.assignments = [] 
    && p2.assignments = []
    then
      if BidSet.is_empty (p1.pattern_vars)
      && BidSet.is_empty (p2.pattern_vars)
      then begin
        let pateq = Flx_unify.type_eq bsym_table counter p1.pattern p2.pattern in
        let ceq = Flx_unify.type_eq bsym_table counter v1 v2 in
(*
print_endline ("Pats eq = " ^ string_of_bool pateq);
print_endline ("Ceq = " ^ string_of_bool ceq);
*)
        if pateq && ceq
        then scancases bsym_table counter t1 t2 (* advance both *)
        else scancases bsym_table counter c1 t2 (* skip rhs case *)
      (* special case of wildcard, somewhat hacked *)
      end else match p1.pattern,p2.pattern with
      | BTYP_type_var _, BTYP_type_var _ ->
         if Flx_unify.type_eq bsym_table counter v1 v2
         then scancases bsym_table counter t1 t2 (* advance both *)
         else scancases bsym_table counter c1 t2 (* skip rhs case *)
      | BTYP_type_var _,_ -> scancases bsym_table counter c1 t2 (* skip rhs case *)
      | _ -> false
   else false

let typematch_implies (bsym_table:Flx_bsym_table.t) counter a b = match a, b with
  | BTYP_type_match (v1,tss1), BTYP_type_match (v2,tss2) ->
(*
print_endline ("Typematch implication");
*)
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
  | BTYP_typeop ("_staticbool_and", args,Flx_kind.KIND_bool) ->
    begin match args with
    | BTYP_type_tuple ls -> List.concat (List.map split_conjuncts' ls)
    | _ -> [args]
    end
  | _ -> [t]

let filter_out_units ls = 
   List.filter (fun x -> match x with | Flx_btype.BBOOL true -> false | _ -> true) ls

let split_conjuncts ls = filter_out_units (split_conjuncts' ls)

let constraint_implies (bsym_table:Flx_bsym_table.t) counter a b =

  let r = 
    (* for the moment we need to downgrade from staticbool kind for the
       implication to work
    *)
    let a = match a with 
      | BTYP_typeop ("_type_to_staticbool",t,_) -> t
      |  _ -> a
    in
    let b = match b with 
      | BTYP_typeop ("_type_to_staticbool",t,_) -> t
      |  _ -> b
    in
    try 
      let conjuncts_a = split_conjuncts a in
      let conjuncts_b = split_conjuncts a in
(*
print_endline ("Conjuncts_a = " ^ Flx_util.catmap "\n AND " Flx_btype.st conjuncts_a);
print_endline ("Conjuncts_b = " ^ Flx_util.catmap "\n AND " Flx_btype.st conjuncts_b);
*)
      terms_imply bsym_table counter conjuncts_a conjuncts_b 
    with exn -> 
      print_endline("EXCEPTION THROWN: Constraint implication:\n");
      print_endline("destaticed: LHS = " ^ Flx_print.sbt bsym_table a);
      print_endline("destaticed: RHS = " ^ Flx_print.sbt bsym_table b);
      raise exn
  in
  begin match r with
  | false  -> 
      print_endline("CONSTRAINT IMPLICATION FAILED:\n");
      print_endline("original: LHS = " ^ Flx_print.sbt bsym_table a);
      print_endline("original: RHS = " ^ Flx_print.sbt bsym_table b);
  | _-> ()
  end; 
  r


