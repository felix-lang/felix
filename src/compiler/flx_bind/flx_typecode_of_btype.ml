open Flx_btype
open Flx_ast

let typecode_of_btype bsym_table counter sr t0 = 
  let rec tc depth mutrail t =
    let isrecursive = Flx_unify.is_recursive_type t in
    let mutrail = 
      if isrecursive then 
        let label = "_fix_"^string_of_int (!counter) in 
        incr counter;
        (depth,label)::mutrail
      else mutrail
    in
    let tc t = tc (depth + 1) mutrail t in
    let r = match t with
      | BTYP_record flds -> TYP_record (List.map (fun (s,t) -> s,tc t) flds) 
      | BTYP_tuple ts -> TYP_tuple (List.map tc ts)
      | BTYP_array (a,n) -> TYP_array (tc a, tc n)
      | BTYP_sum ts -> TYP_sum (List.map tc ts)
      | BTYP_unitsum n -> TYP_unitsum n
      | BTYP_pointer t -> TYP_pointer (tc t)
      | BTYP_function (d,c) -> TYP_function (tc d, tc c)
      | BTYP_cfunction (d,c) -> TYP_function (tc d, tc c)
      | BTYP_void -> TYP_void sr
      | BTYP_label -> TYP_label
      | BTYP_fix (n,_) ->
        begin try 
           let label = List.assoc (depth + n) mutrail in
           TYP_name (sr,label,[])
        with Not_found -> 
          print_endline ("Free fixpoint in typecode of btype! " ^ Flx_print.sbt bsym_table t0);
          assert false
        end

      | BTYP_inst (i,ts) ->
        let id = Flx_bsym_table.find_id bsym_table i in 
        TYP_name (sr,id, (List.map tc ts))
      | _ -> assert false
    in
    if isrecursive then 
       let label = match mutrail with (_,label)::_ -> label | _ -> assert false in
       TYP_as (r,label)
    else r
  in
  let r = tc 0 [] t0 in
  (*
  print_endline ("Translated btype " ^ Flx_print.sbt bsym_table t0 ^ " --> " ^ string_of_typecode r);
  *)
  r




