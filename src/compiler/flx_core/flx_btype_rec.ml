open Flx_btype
open Flx_exceptions

(* top down check for fix point not under sum or pointer *)
let rec check_rec t = match t with
   | BTYP_pointer _
   | BTYP_rref _
   | BTYP_wref _
   | BTYP_sum _
   | BTYP_function _
   | BTYP_effector _
   | BTYP_cfunction _
   | BTYP_variant _
     -> ()

   | BTYP_fix (i,_)
     -> raise Bad_recursion

   | x -> flat_iter ~f_btype:check_rec x


let is_recursive_type t = Flx_btype.is_recursive_type t 
let adjust_fixpoint t = Flx_btype.adjust_fixpoint t

let fix i t =
  let rec aux n t =
    let aux t = aux (n - 1) t in
    match t with
    | BTYP_typeof _ -> assert false
    | BTYP_hole -> assert false
    | BTYP_tuple_cons _ -> assert false
    | BTYP_tuple_snoc _ -> assert false
    | BTYP_none -> assert false
    | BTYP_type_var (k,mt) -> if k = i then btyp_fix n mt else t
    | BTYP_inst (k,ts,mt) -> btyp_inst (k, List.map aux ts,mt)
    | BTYP_vinst (k,ts,mt) -> btyp_vinst (k, List.map aux ts,mt)
    | BTYP_tuple ts -> btyp_tuple (List.map aux ts)
    | BTYP_sum ts -> btyp_sum (List.map aux ts)
    | BTYP_intersect ts -> btyp_intersect (List.map aux ts)
    | BTYP_union ts -> btyp_union (List.map aux ts)
    | BTYP_type_set ts -> btyp_type_set (List.map aux ts)
    | BTYP_function (a,b) -> btyp_function (aux a, aux b)
    | BTYP_effector (a,e,b) -> btyp_effector (aux a, aux e, aux b)
    | BTYP_cfunction (a,b) -> btyp_cfunction (aux a, aux b)

    | BTYP_pointer a -> btyp_pointer (aux a)
    | BTYP_rref a -> btyp_rref (aux a)
    | BTYP_wref a -> btyp_wref (aux a)

    | BTYP_cltpointer (d,c) -> btyp_cltpointer (aux d) (aux c)
    | BTYP_cltrref (d,c) -> btyp_cltrref (aux d) (aux c)
    | BTYP_cltwref (d,c) -> btyp_cltwref (aux d) (aux c)



    | BTYP_array (a,b) -> btyp_array (aux a, aux b)
    | BTYP_rptsum (a,b) -> btyp_rptsum (aux a, aux b)
    | BTYP_rev t -> btyp_rev (aux t)
    | BTYP_uniq t -> btyp_uniq (aux t)

    | BTYP_record (ts) ->
       btyp_record (List.map (fun (s,t) -> s, aux t) ts)

    | BTYP_polyrecord (ts,v) ->
       btyp_polyrecord (List.map (fun (s,t) -> s, aux t) ts) (aux v)

    | BTYP_variant ts ->
       btyp_variant (List.map (fun (s,t) -> s, aux t) ts)

    | BTYP_polyvariant ts ->
       btyp_polyvariant (List.map (fun k -> 
       match k with
       | `Ctor (s,t) -> `Ctor (s, aux t)
       | `Base t -> `Base (aux t)
       ) ts)


    | BTYP_label 
    | BTYP_unitsum _
    | BTYP_void
    | BTYP_fix _
    | BTYP_type_apply _
    | BTYP_type_map _
    | BTYP_type_function _
    | BTYP_type_tuple _
    | BTYP_type_match _
    | BTYP_subtype_match _
    | BTYP_type_set_union _ -> t
    | BTYP_type_set_intersection _ -> t
  in
    aux 0 t


