open Flx_btype
open Flx_bexpr

let try_bind_prod bsym_table counter (ea,ta as a)=
match ta with
| BTYP_tuple ts ->
  let ds = ref [] and cs = ref [] in 
  List.iter 
    (fun t -> match t with 
    | BTYP_function (d,c) -> 
      ds := d:: !ds; cs := c :: !cs
    | _ -> raise Flx_dot.OverloadResolutionError
    )
    ts
  ;
  let t = btyp_function (btyp_tuple (List.rev (!ds)), btyp_tuple (List.rev (!cs))) in
  bexpr_funprod t a
| BTYP_array (BTYP_function (d,c),BTYP_unitsum n) ->
  let t = btyp_function (btyp_array (d,btyp_unitsum n), btyp_array (c,btyp_unitsum n)) in
  bexpr_funprod t a
| _ -> raise Flx_dot.OverloadResolutionError



