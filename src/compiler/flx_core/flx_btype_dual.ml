open Flx_btype
open Flx_exceptions

let rec dual t =
  match Flx_btype.map ~f_btype:dual t with
  | BTYP_none -> assert false

  | BTYP_sum ls ->
    begin match ls with
    | [t] -> t
    | ls -> btyp_tuple ls
    end

  | BTYP_tuple ls ->
    begin match ls with
    | [] -> btyp_void ()
    | [t] -> t
    | ls -> btyp_sum ls
    end

  | BTYP_function (a,b) -> btyp_function (b,a)
  | BTYP_effector (a,e,b) -> btyp_effector (b,e,a)
  | BTYP_cfunction (a,b) -> btyp_cfunction (b,a)
  | BTYP_array (a,b) -> btyp_array (b,a)

  | BTYP_pointer t -> btyp_pointer (dual t)
  | BTYP_void -> btyp_unit ()
  | BTYP_unitsum k ->
    let rec aux ds k = if k = 0 then ds else aux (btyp_unit () :: ds) (k-1) in
    btyp_tuple (aux [] k)

  | BTYP_type_set ts -> btyp_intersect (List.map dual ts)
  | BTYP_intersect ts -> btyp_union (List.map dual ts)
  | BTYP_union ts -> btyp_intersect (List.map dual ts)
  | BTYP_record (ts) -> btyp_variant ts
  | t -> t

