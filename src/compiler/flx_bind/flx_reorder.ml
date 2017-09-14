open Flx_bexpr
open Flx_btype 
open Flx_types

(* reorders argument components based on parameter names *)
let reorder sym_table sr be ((be1,t1) as tbe1) ((be2,t2) as tbe2) =
  match be1 with
  | BEXPR_closure (i,ts) ->
    begin match t2 with
      (* a bit of a hack .. *)
      | BTYP_record _
      | BTYP_tuple [] ->
        let rs = match t2 with
          | BTYP_record (rs) -> rs
          | BTYP_tuple [] -> []
          | _ -> assert false
        in
        let pnames =
          match Flx_lookup_state.hfind "lookup" sym_table i with
          | { Flx_sym.symdef=SYMDEF_function (ps,_,_,_,_) } ->
            List.map 
              begin fun (sr,_,name,_,d) -> name, 
                match d with 
                | None -> None 
                | Some e -> Some (be i e) 
              end 
              (fst ps)
          | _ -> assert false
        in
        let n = List.length rs in
        let rs = List.map2 (fun (name,t) j -> name,(j,t)) rs (Flx_list.nlist n) in

        (* calculate argument component reordering based on names *)
        begin try
          `Reorder 
            (List.map 
              begin fun (name,d) ->
                try
                  match List.assoc name rs with
                  | j,t -> 
(*
                    print_endline ("1:get_n arg" ^ sbe bsym_table tbe2); 
*)
                    bexpr_get_n t j tbe2
                with Not_found ->
                  match d with
                  | Some d ->d
                  | None -> raise Not_found
              end 
              pnames
            )
          with Not_found -> `None
        end
      | _ -> `None
    end
  | _ -> print_endline "WOOPS WHAT IF BE1 is NOT A CLOSURE?"; `None

