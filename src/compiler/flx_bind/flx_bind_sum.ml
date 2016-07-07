open Flx_btype
open Flx_bexpr

let try_bind_sum bsym_table counter (ea,ta as a)=
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
  let t = btyp_function (btyp_sum (List.rev (!ds)), btyp_sum (List.rev (!cs))) in
  bexpr_funsum t a
| BTYP_array (BTYP_function (d,c),BTYP_unitsum n) ->
  let t = btyp_function (btyp_sum (Flx_list.repeat d n), btyp_sum (Flx_list.repeat c n)) in
  bexpr_funsum t a
| _ -> raise Flx_dot.OverloadResolutionError


