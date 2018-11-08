open Flx_btype
open Flx_bexpr
open Flx_ast

let bind_constant_slice bsym_table be bt sr f' a' ta a = 

  (* ---------------------------------------------------------- *)
  (* special case, constant slice of tuple *) 
  (* ---------------------------------------------------------- *)
  try 
    match ta with
    | BTYP_tuple ls ->
      let tmin = 0 and tmax = List.length ls - 1 in
      let smin, smax = Flx_cal_slice.cal_slice tmin tmax f' in 
      let sls = ref [] in
      for i = smin to smax do
        sls := i :: !sls
      done;
      let sls = List.rev_map (fun i -> `EXPR_get_n (sr,(i,a'))) !sls in
      be (`EXPR_tuple (sr,sls))
    | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  (* ---------------------------------------------------------- *)
  (* special case, constant slice of small linear array *) 
  (* ---------------------------------------------------------- *)
  try 
    match ta with
    | BTYP_array (base,BTYP_unitsum n) ->
      let tmin = 0 and tmax = n - 1 in
      let smin, smax = Flx_cal_slice.cal_slice tmin tmax f' in 
      if smax - smin < 20 then begin 
        let sls = ref [] in
        for i = smin to smax do
          sls := i :: !sls
        done;
        let sls = List.rev_map (fun i -> `EXPR_get_n (sr,(i,a'))) !sls in
        be (`EXPR_tuple (sr,sls))
      end else begin
(*
print_endline ("Bind slice, large slice, delegate to library");
*)
        let routine = if islinear_type bsym_table base then "compact_linear_subarray" else "subarray" in
        let first = `TYP_unitsum smin in
        let len = `TYP_unitsum (smax - smin + 1) in
        let subarray = `EXPR_name (sr,routine,[first;len]) in
        be (`EXPR_apply (sr,(subarray,a')))
      end
    | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  (* ---------------------------------------------------------- *)
  (* special case, constant slice of tuple pointer *) 
  (* ---------------------------------------------------------- *)
  try 
    match ta with
    | BTYP_pointer (BTYP_tuple ls) ->
      let tmin = 0 and tmax = List.length ls - 1 in
      let smin, smax = Flx_cal_slice.cal_slice tmin tmax f' in 
      let sls = ref [] in
      for i = smin to smax do
        sls := i :: !sls
      done;
      let sls = List.rev_map (fun i -> `EXPR_get_n (sr,(i,a'))) !sls in
      be (`EXPR_tuple (sr,sls))
    | _ -> raise Flx_dot.OverloadResolutionError
  with Flx_dot.OverloadResolutionError ->

  raise Flx_exceptions.TryNext 


