open Flx_btype
open Flx_bid

(* Test with a type variable exists in a type expression *)

let var_i_occurs i t =
  let rec f_btype t =
    match t with
    | BTYP_type_var (j,_) when i = j -> raise Not_found
    | _ -> Flx_btype.flat_iter ~f_btype t
 in
   try
     f_btype t;
     false
   with Not_found -> true

let rec vars_in t =
  let vs = ref BidSet.empty in
  let add_var i = vs := BidSet.add i !vs in
  let rec f_btype t =
    match t with
    | BTYP_type_var (i,_) -> add_var i
    | _ -> Flx_btype.flat_iter ~f_btype t
  in
  f_btype t;
  !vs

let var_list_occurs ls t =
  let yes = ref false in
  List.iter (fun i -> yes := !yes || var_i_occurs i t) ls;
  !yes

let var_occurs bsym_table t =
  let rec aux' excl t = let aux t = aux' excl t in
    match t with
    | BTYP_type_tuple ls
    | BTYP_type_set ls
    | BTYP_type_set_intersection ls
    | BTYP_type_set_union ls
    | BTYP_compactsum ls
    | BTYP_sum ls
    | BTYP_inst (_,_,ls,_)
    | BTYP_vinst (_,ls,_)
    | BTYP_compacttuple ls 
    | BTYP_intersect ls
    | BTYP_tuple ls -> List.iter aux ls

    | BTYP_record (ls) -> List.iter (fun (s,t) -> aux t) ls
    | BTYP_polyrecord (ls,s,v) -> List.iter (fun (s,t) -> aux t) ls; aux v
    | BTYP_variant ls -> List.iter (fun (s,t) -> aux t) ls

    | BTYP_in (a, b) 
    | BTYP_compactrptsum (a,b)
    | BTYP_compactarray (a,b)
    | BTYP_rptsum (a,b)
    | BTYP_array (a,b)
    | BTYP_linearfunction (a,b) -> aux a; aux b
    | BTYP_function (a,b) -> aux a; aux b
    | BTYP_lineareffector (a,e,b) -> aux a; aux e; aux b
    | BTYP_effector (a,e,b) -> aux a; aux e; aux b

    | BTYP_cfunction (a,b) -> aux a; aux b

    | BTYP_ptr (_,t,ts)  -> aux t; List.iter aux ts
    | BTYP_rev a -> aux a

    | BTYP_uniq a -> aux a
    | BTYP_borrowed a -> aux a

    | BTYP_typeof (_, _)
    | BTYP_none
    | BBOOL _
    | BTYP_finst (_,_,_,_) (* type function instances can contain kind variables but not type variables *)
    | BTYP_instancetype _
    | BTYP_ellipsis
    | BTYP_label
    | BTYP_unitsum _
    | BTYP_void
    | BTYP_fix _ -> ()

    | BTYP_type_var (k,_) -> if not (List.mem k excl) then raise Not_found
    | BTYP_type_function (p,r,b) ->
      aux' (List.map fst p @ excl) b
    
    | BTYP_type_apply (a,b) -> aux a; aux b
    | BTYP_type_map (a,b) -> aux a; aux b
    | BTYP_tuple_cons (a,b) -> aux a; aux b
    | BTYP_tuple_snoc (a,b) -> aux a; aux b

    | BTYP_polyvariant pvs -> List.iter (function | `Ctor (_,t) -> aux t | `Base t -> aux t) pvs
       

    (* FIXME: these should be handled *)
    | BTYP_type_match (_, _)
    | BTYP_subtype_match (_, _)
    | BTYP_typeop (_, _, _)
    -> ()


 in try aux' [] t; false with Not_found -> true

