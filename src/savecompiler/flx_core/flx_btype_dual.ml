open Flx_btype
open Flx_exceptions

let rec dual t =
  match  t with
  | BTYP_none -> assert false

  | BTYP_rptsum (n,b) as k ->
    btyp_array (dual b,n)

  | BTYP_sum ls ->
   btyp_tuple (List.map dual ls)

  | BTYP_tuple ls ->
   btyp_sum (List.map dual ls)

  | BTYP_function (a,b) -> btyp_function (dual b,dual a)
  | BTYP_effector (a,e,b) -> btyp_effector (dual b,dual e,dual a)
  | BTYP_cfunction (a,b) -> btyp_cfunction (dual b,dual a)
  | BTYP_array (a,b) -> btyp_rptsum (b,dual a)

  | BTYP_pointer t -> btyp_pointer (dual t)
  | BTYP_void -> btyp_unit ()
  | BTYP_unitsum k -> 
    let rec aux ds k = if k = 0 then ds else aux (btyp_unit () :: ds) (k-1) in
    btyp_tuple (aux [] k)

  | BTYP_type_set ts -> btyp_intersect (List.map dual ts)
  | BTYP_intersect ts -> btyp_union (List.map dual ts)
  | BTYP_union ts -> btyp_intersect (List.map dual ts)
  | BTYP_record (ts) -> btyp_variant ts
  | t -> Flx_btype.map ~f_btype:dual t

