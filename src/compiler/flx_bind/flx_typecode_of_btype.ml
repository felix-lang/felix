open Flx_btype
open Flx_ast

let typecode_of_btype ?sym_table:(sym_table=None) bsym_table counter sr t0 = 
  let rec tc depth mutrail t =
    let isrecursive = Flx_btype_rec.is_recursive_type t in
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

      | BTYP_inst (i,ts,mt) ->
        let id = Flx_bsym_table.find_id bsym_table i in 
        TYP_name (sr,id, (List.map tc ts))

      | BTYP_type_var (i,_) -> TYP_var i
      | BTYP_uniq t -> TYP_uniq (tc t)
(*
        begin match sym_table with 
        | None -> TYP_var i 
          (* failwith ("Can't lookup type variable "^string_of_int i ^" , no unbound symbol table available"); *)
        | Some sym_table ->
          begin try 
            let id = Flx_sym_table.find_id sym_table i in 
            TYP_name (Flx_srcref.dummy_sr,id,[]) 
          with _ -> 
            TYP_var i
            (*
            failwith ("Can't find type variable " ^ string_of_int i ^ " in unbound symbol table") 
            *)
          end
        end
*)
      | _ -> failwith ("typecode_of_btype can't handle type : " ^ Flx_print.sbt bsym_table t)

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




