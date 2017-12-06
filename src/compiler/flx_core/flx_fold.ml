open Flx_btype

let type_eq bsym_table x = Flx_typeeq.type_eq (Flx_print.sbt bsym_table) x

exception Found of Flx_btype.t

let fold (bsym_table: Flx_bsym_table.t) counter t =
  let rec aux trail depth t' =
    let ax t = aux ((depth,t')::trail) (depth+1) t in
    match t' with
    | BTYP_intersect ls
    | BTYP_union ls
    | BTYP_sum ls
    | BTYP_inst (_,ls,_)
    | BTYP_vinst (_,ls,_)
    | BTYP_tuple ls -> List.iter ax ls
    | BTYP_record (ls) -> List.iter (fun (s,t) -> ax t) ls
    | BTYP_polyrecord (ls,v) -> List.iter (fun (s,t) -> ax t) ls; ax v
    | BTYP_variant ls -> List.iter (fun (s,t) -> ax t) ls
    | BTYP_polyvariant ls -> List.iter (fun k -> match k with
       | `Ctor (s,t) -> ax t
       | `Base t -> ax t) ls


    | BTYP_cltpointer (a,b)
    | BTYP_cltrref (a,b)
    | BTYP_cltwref (a,b)
    | BTYP_array (a,b)
    | BTYP_rptsum (a,b)
    | BTYP_function (a,b) -> ax a; ax b
    | BTYP_effector (a,e, b) -> ax a; ax e; ax b
    | BTYP_cfunction (a,b) -> ax a; ax b

    | BTYP_pointer a -> ax a
    | BTYP_rref a -> ax a
    | BTYP_wref a -> ax a
    | BTYP_rev a -> ax a

    | BTYP_uniq a -> ax a

    | BTYP_tuple_cons (a,b) -> ax a; ax b
    | BTYP_tuple_snoc (a,b) -> ax a; ax b

    | BTYP_hole
    | BTYP_label 
    | BTYP_none
    | BTYP_void
    | BTYP_unitsum _
    | BTYP_type_var _
    | BTYP_fix (0,_) -> ()

    | BTYP_fix (i,_) ->
      let k = depth + i in
      begin try
        let t'' = List.assoc k trail in
        if type_eq bsym_table counter t'' t then raise (Found t'')
      with Not_found -> ()
      end

    | BTYP_type_apply (a,b) -> ax a; ax b
    | BTYP_type_map(a,b) -> ax a; ax b

    | BTYP_typeof _
    | BTYP_type_set_intersection _
    | BTYP_type_set_union _
    | BTYP_type_set _
    | BTYP_type_function _
    | BTYP_type_tuple _
    | BTYP_type_match _ -> () (* assume fixpoint can't span these boundaries *)
    | BTYP_subtype_match _ -> () (* assume fixpoint can't span these boundaries *)
      (* failwith ("[fold] unexpected metatype " ^ sbt sym_table t') *)
  in
    try aux [] 0 t; t
    with Found t -> t
(* produces a unique minimal representation of a type
by folding at every node *)

let minimise bsym_table counter t =
  fold bsym_table counter (Flx_btype.map ~f_btype:(fold bsym_table counter) t)

