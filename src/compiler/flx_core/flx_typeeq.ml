
open Flx_types
open Flx_btype
open Flx_util
open Flx_list
open Flx_exceptions
open Flx_bid
open Flx_btype_subst
let rec memq trail (a,b) = match trail with
  | [] -> false
  | (i,j)::t -> i == a && j == b || memq t (a,b)

let rec type_eq' sbt counter ltrail ldepth rtrail rdepth trail t1 t2 =
  (* print_endline (sbt sym_table t1 ^ " =? " ^ sbt sym_table t2); *)
  if memq trail (t1,t2) then true
  else let te a b = type_eq' sbt counter
    ((ldepth,t1)::ltrail) (ldepth+1)
    ((rdepth,t2)::rtrail) (rdepth+1)
    ((t1,t2)::trail)
    a b
  in
  (*
  let assoc i ls =
    try List.assoc i ls
    with Not_found -> raise Not_found
      (*
      print_endline (
      "trail failure, index=" ^ si i ^ ", trail="
      ^ catmap "," (fun (k,t) -> si k ^ "->" ^ sbt sym_table t) ls
      );
      failwith "Trail failure"
      *)
  in
  *)
  match t1,t2 with
  | BTYP_hole,_ (* for zippers *)
  | _,BTYP_hole -> true

  | BTYP_label,BTYP_label -> true
  | BTYP_vinst (i1,ts1,_),BTYP_vinst (i2,ts2,_)
  | BTYP_inst (i1,ts1,_),BTYP_inst (i2,ts2,_) ->
    (* kind should aqree already .. ? *)
    i1 = i2 &&
    List.length ts1 = List.length ts2 &&
    List.fold_left2
    (fun tr a b -> tr && te a b)
    true ts1 ts2

  | BTYP_unitsum i,BTYP_unitsum j -> i = j

  | BTYP_sum ts1, BTYP_sum ts2
  | BTYP_type_tuple ts1,BTYP_type_tuple ts2
  | BTYP_tuple ts1,BTYP_tuple ts2 ->
    let result =
    if List.length ts1 = List.length ts2
    then
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true ts1 ts2
    else false
    in
    (*
    print_endline ("Tuple/sum compared " ^ (if result then "TRUE" else "FALSE"));
    if List.length ts1 = List.length ts2 then
    print_endline ("Args = " ^ catmap "\n  " (fun (t1,t2) ->
      "lhs=" ^sbt sym_table t1 ^" vs rhs=" ^ sbt sym_table t2)
     (combine ts1 ts2))
    else print_endline ("unequal lengths");
    *)
    result

  | BTYP_record ([]),BTYP_tuple []
  | BTYP_tuple [],BTYP_record ([]) -> true

  | BTYP_polyrecord (t1,v1), BTYP_polyrecord (t2,v2) ->
   te (btyp_record t1) (btyp_record t2) && te v1 v2 

  | BTYP_record (t1),BTYP_record (t2) ->
    if List.length t1 = List.length t2
    then begin
      List.map fst t1 = List.map fst t2 &&
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true (List.map snd t1) (List.map snd t2)
    end else false

  | BTYP_variant [],BTYP_tuple []
  | BTYP_tuple [],BTYP_variant [] -> true

  | BTYP_variant t1,BTYP_variant t2 ->
    if List.length t1 = List.length t2
    then begin
      (* should not be needed but variants aren't implemented yet *)
      let rcmp (s1,_) (s2,_) = compare s1 s2 in
      let t1 = List.sort rcmp t1 in
      let t2 = List.sort rcmp t2 in
      List.map fst t1 = List.map fst t2 &&
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true (List.map snd t1) (List.map snd t2)
    end else false


  | BTYP_rptsum (s1,d1),BTYP_rptsum (s2,d2)
  | BTYP_array (s1,d1),BTYP_array (s2,d2)
  | BTYP_function (s1,d1),BTYP_function (s2,d2)
  | BTYP_cfunction (s1,d1),BTYP_cfunction (s2,d2)
  | BTYP_type_apply(s1,d1),BTYP_type_apply(s2,d2)
  | BTYP_type_map(s1,d1),BTYP_type_map(s2,d2)
  | BTYP_tuple_cons (s1,d1),BTYP_tuple_cons (s2,d2)
    -> te s1 s2 && te d1 d2

  | BTYP_tuple_snoc (s1,d1),BTYP_tuple_snoc (s2,d2)
    -> te s1 s2 && te d1 d2


  | BTYP_effector (s1,e1,d1),BTYP_effector (s2,e2,d2)
    -> te s1 s2 && te d1 d2 && te e1 e2

  (* order is important for lvalues .. *)
  | BTYP_array (ta,BTYP_unitsum n),BTYP_tuple ts
    when List.length ts = n ->
    List.fold_left (fun tr t -> tr && te ta t) true ts


  | BTYP_tuple ts,BTYP_array (ta,BTYP_unitsum n)
    when List.length ts = n ->
    List.fold_left (fun tr t -> tr && te t ta) true ts

  | BTYP_uniq p1,BTYP_uniq p2

  | BTYP_rref p1,BTYP_rref p2
  | BTYP_wref p1,BTYP_wref p2
  | BTYP_pointer p1,BTYP_pointer p2
    -> te p1 p2

  | BTYP_cltrref (d1,c1),BTYP_cltrref (d2,c2)
  | BTYP_cltwref (d1,c1),BTYP_cltwref (d2,c2)
  | BTYP_cltpointer (d1,c1),BTYP_cltpointer (d2,c2)
    -> te d1 d2 && te c1 c2


  | BTYP_void,BTYP_void
    -> true

  | BTYP_type_var (i,_), BTYP_type_var (j,_) ->
    let result = i = j in
    (*
    print_endline ("Type variables compared " ^ (if result then "TRUE" else "FALSE"));
    *)
    result


(*
  | BTYP_fix (0,BTYP_type 0),_ 
  | _,BTYP_fix (0,BTYP_type 0) -> true
*)

  | BTYP_fix (i,t1),BTYP_fix (j,t2) ->
    (*
    print_endline ("Check fixpoint " ^ si i ^ " vs " ^ si j);
    *)
    if i = j then begin 
      if t1 <> t2 then print_endline "[type_eq] Fix points at same level have different metatypes";
(*
      true
*)
      (* should be correct .. but something breaks , it seems to clobber the trail.
         but we're going down a level from types to meta-types, so this shouldn't
         happen
      *)
      Flx_kind.kind_eq t1 t2 
    end else (* hack ..? *)
    begin
      (*
      print_endline "Matching fixpoints";
      *)
      try
      let a = List.assoc (ldepth+i) ltrail in
      let b = List.assoc (rdepth+j) rtrail in
      type_eq' sbt counter ltrail ldepth rtrail rdepth trail a b
      with Not_found -> false
    end

  | BTYP_fix (i,mt1),t ->
    (*
    print_endline "LHS fixpoint";
    *)
    begin try
    let a = List.assoc (ldepth+i) ltrail in
    type_eq' sbt counter ltrail ldepth rtrail rdepth trail a t
    with Not_found -> false
    end

  | t,BTYP_fix (j,mt2) ->
    (*
    print_endline "RHS fixpoint";
    *)
    begin try
    let b = List.assoc (rdepth+j) rtrail in
    type_eq' sbt counter ltrail ldepth rtrail rdepth trail t b
    with Not_found -> false
    end

  | BTYP_type_function (p1,r1,b1), BTYP_type_function (p2,r2,b2) ->
    List.length p1 = List.length p2 &&
    let vs = List.map2 (fun (i1,_) (i2,t) -> i1,btyp_type_var (i2,t))  p1 p2 in
(*
    print_endline "Comparing type functions";
    print_endline ("b1 =          " ^ sbt b1);
*)
    let b1 = list_subst counter vs b1 in
(*
    print_endline ("Adjusted b1 = " ^ sbt b1);
    print_endline ("b2 =          " ^ sbt b2);
*)
    let result = te b1 b2 in
(*
    print_endline ("Compared = " ^ (if result then "TRUE" else "FALSE"));
*)
    result

  | l,r ->
(*
    print_endline ("WOOOPS .. dunno.." ^ sbt l ^" vs " ^ sbt r);
*)
    false

let type_eq sbt counter t1 t2 = (* print_endline "TYPE EQ";  *)
  type_eq' sbt counter [] 0 [] 0 [] t1 t2

let type_match sbt counter t1 t2 = (* print_endline "TYPE MATCH"; *)
  type_eq' sbt counter [] 0 [] 0 [] t1 t2


