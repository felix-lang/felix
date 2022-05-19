open Flx_btype
open Flx_exceptions
open Flx_bid

let remap map i = try List.assoc i map with Not_found -> i

let rec alpha counter map t = 
  match t with
  | BTYP_type_var (i, knd) -> 
    btyp_type_var (remap map i, knd)

  | BTYP_type_function (ps,r,b) ->
    let map = List.fold_left (fun acc (i,_) -> (i, fresh_bid counter)::acc) map ps in
    let ps = List.map (fun (i,k) -> remap map i, k) ps in
    let b = alpha counter map b in
    btyp_type_function (ps, r, b)
 
  | BTYP_type_match (arg, pss) ->
    let arg = alpha counter map arg in
    let pss = 
      List.map (fun ({pattern=p; assignments=a; pattern_vars=dvars},handler) ->
        (* variables to remap are the pattern variables and assignments *)
        let map = BidSet.fold (fun i acc -> (i, fresh_bid counter)::acc) dvars map in
        let map = List.fold_left  (fun acc (i,_) -> (i, fresh_bid counter)::acc) map a in

        let p = alpha counter map p in
        let dvars = BidSet.map (remap map) dvars in
        let a = List.map (fun (i,t) -> remap map i, alpha counter map t) a in
        {pattern=p; assignments=a; pattern_vars=dvars}, alpha counter map handler
      ) 
      pss
    in 
    btyp_type_match (arg,pss)

  | BTYP_subtype_match (arg, pss) ->
    let arg = alpha counter map arg in
    let pss = 
      List.map (fun ({pattern=p; assignments=a; pattern_vars=dvars},handler) ->
        (* variables to remap are the pattern variables and assignments *)
        let map = BidSet.fold (fun i acc -> (i, fresh_bid counter)::acc) dvars map in
        let map = List.fold_left  (fun acc (i,_) -> (i, fresh_bid counter)::acc) map a in

        let p = alpha counter map p in
        let dvars = BidSet.map (remap map) dvars in
        let a = List.map (fun (i,t) -> remap map i, alpha counter map t) a in
        {pattern=p; assignments=a; pattern_vars=dvars}, alpha counter map handler
      ) 
      pss
    in 
    btyp_subtype_match (arg,pss)


  | t -> Flx_btype.map ~f_btype:(alpha counter map) t
  


let alpha_convert counter t = alpha counter [] t


