open Flx_btype
open Flx_bid

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
    | BTYP_intersect ls
    | BTYP_type_set ls
    | BTYP_type_set_intersection ls
    | BTYP_type_set_union ls
    | BTYP_sum ls
    | BTYP_inst (_,ls,_)
    | BTYP_vinst (_,ls,_)
    | BTYP_tuple ls -> List.iter aux ls

    | BTYP_record (ls) -> List.iter (fun (s,t) -> aux t) ls
    | BTYP_polyrecord (ls,v) -> List.iter (fun (s,t) -> aux t) ls; aux v
    | BTYP_variant ls -> List.iter (fun (s,t) -> aux t) ls

    | BTYP_rptsum (a,b)
    | BTYP_array (a,b)
    | BTYP_function (a,b) -> aux a; aux b
    | BTYP_effector (a,e,b) -> aux a; aux e; aux b
    | BTYP_cfunction (a,b) -> aux a; aux b

    | BTYP_pointer a  -> aux a
    | BTYP_rref a  -> aux a
    | BTYP_wref a  -> aux a
    | BTYP_rev a -> aux a

    | BTYP_cltpointer (a,b) -> aux a; aux b
    | BTYP_cltwref (a,b) -> aux a; aux b
    | BTYP_cltrref (a,b) -> aux a; aux b

    | BTYP_uniq a -> aux a

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

    | _ -> 
      print_endline ("[var_occurs] unexpected metatype " ^ Flx_print.sbt bsym_table t);
      failwith ("[var_occurs] unexpected metatype " ^ Flx_print.sbt bsym_table t)

 in try aux' [] t; false with Not_found -> true

