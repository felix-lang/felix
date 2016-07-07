open Flx_btype
open Flx_bexpr

let try_bind_lrbrack bsym_table counter (ea,ta as a)=
match ta with
| BTYP_tuple ts ->
  let ct = ref None and ds = ref [] in 
  List.iter 
    (fun t -> match t with 
    | BTYP_function (d,c) -> 
      begin match !ct with
      | None -> ct := Some c
      | Some t when Flx_unify.type_eq bsym_table counter c t -> ()
      | _ -> raise Flx_dot.OverloadResolutionError
      end
      ;
      ds := d :: !ds
    | _ -> raise Flx_dot.OverloadResolutionError
    )
    ts
  ;
  let t = match !ct with 
    | Some c -> 
      btyp_function (btyp_sum (List.rev (!ds)), c) 
    | None -> (* dunno what to do with empty case *)
      raise Flx_dot.OverloadResolutionError  
  in
  bexpr_lrbrack t a

| BTYP_array (BTYP_function (d,c),BTYP_unitsum n) ->
  let ds = Flx_list.repeat d n in
  let t = btyp_function (btyp_sum ds, c) in
  bexpr_lrbrack t a
| _ -> raise Flx_dot.OverloadResolutionError



