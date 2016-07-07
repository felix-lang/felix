open Flx_btype
open Flx_bexpr

let try_bind_lrangle bsym_table counter (ea,ta as a)=
match ta with
| BTYP_tuple ts ->
  let dt = ref None and cs = ref [] in 
  List.iter 
    (fun t -> match t with 
    | BTYP_function (d,c) -> 
      begin match !dt with
      | None -> dt := Some d
      | Some t when Flx_unify.type_eq bsym_table counter d t -> ()
      | _ -> raise Flx_dot.OverloadResolutionError
      end
      ;
      cs := c :: !cs
    | _ -> raise Flx_dot.OverloadResolutionError
    )
    ts
  ;
  let t = match !dt with 
    | Some d -> 
      btyp_function (d, btyp_tuple (List.rev (!cs))) 
    | None -> (* dunno what to do with empty case *)
      raise Flx_dot.OverloadResolutionError  
  in
  bexpr_lrangle t a

| BTYP_array (BTYP_function (d,c),BTYP_unitsum n) ->
  let t = btyp_function (d, btyp_array (c,btyp_unitsum n)) in
  bexpr_lrangle t a
| _ -> raise Flx_dot.OverloadResolutionError


