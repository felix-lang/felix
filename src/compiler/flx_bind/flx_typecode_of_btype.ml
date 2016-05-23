open Flx_btype
open Flx_ast

let typecode_of_btype bsym_table sr t = 
  let rec tc t = match t with
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
  | BTYP_fix _ -> 
    print_endline ("typecode_of_btype can't handle fixpoint yet");
    assert false (* requires trickery using TYP_as *)
  | BTYP_inst (i,ts) ->
    let id = Flx_bsym_table.find_id bsym_table i in 
    TYP_name (sr,id, (List.map tc ts))
  | _ -> assert false
  in tc t


