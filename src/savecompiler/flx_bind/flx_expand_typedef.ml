
let rec find_in_list counter t lst =
   match lst with
   | [] -> None
   | (h,i) :: tail ->
     if Flx_typeeq.type_eq (Flx_btype.st) counter h t then Some i else
     find_in_list counter t tail

let rec expand bsym_table  counter sr t =
  let rec aux trail level t =
(*
    print_endline ("Aux: " ^ string_of_int level ^ " t=" ^ Flx_btype.st t);
*)
    match find_in_list counter t trail with
    | Some i -> 
(*
      print_endline ("Fixpoint found at depth " ^ string_of_int level ^ " up to level " ^ string_of_int i ^
        "diff is " ^ string_of_int (i - level)
      );
*)
      Flx_btype.btyp_fix (i - level) Flx_kind.KIND_type (* CHECK!! *)
    | None ->
      match t with
      | Flx_btype.BTYP_inst (k,ts,mt) ->
        begin try 
          let bsym = Flx_bsym_table.find bsym_table k in
          let bbdcl = Flx_bsym.bbdcl bsym in
          begin match bbdcl with
          | Flx_bbdcl.BBDCL_structural_type_alias (bvs, alias) 
          | Flx_bbdcl.BBDCL_nominal_type_alias (bvs, alias) ->
(*
print_endline ("Found typedef " ^ Flx_bsym.id bsym ^ "<"^string_of_int k^"> alias=" ^ Flx_btype.st alias);
print_endline ("bvs/tvs= " ^ catmap ", " (fun ((s,i), t) -> s^"<" ^ string_of_int i ^ "> <-- " ^ Flx_btype.st t) (List.combine bvs ts));
*)
            let salias = Flx_btype_subst.tsubst sr bvs ts alias in
(*
print_endline ("typedef " ^ Flx_bsym.id bsym ^ " after substitution =" ^ Flx_btype.st alias);
*)
            aux ((t,level)::trail) level salias (* NO level increment *)
          | _ -> 
(*
print_endline ("Found non-typedef" ^ Flx_bsym.id bsym ^ "<" ^ string_of_int k ^ ">");
            let ts = List.map (aux trail level) ts in
            btyp_inst (k,ts) 
*)
Flx_btype.map ~f_btype:(aux trail (level+1)) t 

          end
        with Not_found -> 
          Flx_btype.map ~f_btype:(aux (trail) (level+1)) t
        
(*
print_endline ("Found unknown <" ^ string_of_int k ^ ">");
print_endline ("raw ts = " ^ catmap "," Flx_btype.st ts); 
            let ts = List.map (aux ((t, level+1)::trail) (level+1)) ts in
print_endline ("processed ts = " ^ catmap "," Flx_btype.st ts); 
            btyp_inst (k,ts) 
*)
        end
     | _ -> Flx_btype.map ~f_btype:(aux (trail) (level+1)) t
  in
(*
  print_endline ("Top level expansion of " ^ Flx_btype.st t);
*)
  let r = aux [] 0 t in
  if not (Flx_btype.complete_type r) then
    failwith ("Flx_expand_typedef.expand produced incomplete type! " ^ Flx_btype.st r)
  else begin
(*
    print_endline ("Expanded to " ^ Flx_btype.st r);
*)
    r
  end
    
