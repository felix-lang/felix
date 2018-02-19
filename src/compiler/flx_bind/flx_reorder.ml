open Flx_bexpr
open Flx_btype 
open Flx_types

type reordering_t = [
  | `None 
  | `Coerce of Flx_btype.t * Flx_btype.t 
  | `Reorder of Flx_bexpr.t list
]

let get_pnames_and_dflts be i ps : (string * Flx_bexpr.t option) list =
  List.map 
    begin fun p -> match p with
    | Flx_ast.Satom (sr,_,name,_,d) -> name, 
      (match d with 
      | None -> None 
      | Some e -> Some (be i e) 
      )
    | Flx_ast.Slist _ -> raise Not_found (* can't allow nested param tuples *)
    end 
    ps

(* reorders argument components based on parameter names *)
(* raises Not_found if can't reorder *)
let reorder' sym_table sr be ((be1,t1) as tbe1) ((be2,t2) as tbe2) : reordering_t =
  begin match be1 with
  | BEXPR_closure (i,ts) ->
    begin match t2 with
    | BTYP_record _
    | BTYP_tuple [] ->
      let rs = match t2 with
        | BTYP_record (rs) -> rs
        | BTYP_tuple [] -> []
        | _ -> assert false
      in
      begin match Flx_lookup_state.hfind "lookup" sym_table i with
      | { Flx_sym.symdef=SYMDEF_function (ps,_,_,_,_) } ->
        begin match fst ps with
        | Satom _ -> raise Not_found 
        | Slist ps ->
          let pnames = get_pnames_and_dflts be i ps in
          let n = List.length rs in
          let rs = List.map2 (fun (name,t) j -> name,(j,t)) rs (Flx_list.nlist n) in

          (* calculate argument component reordering based on names *)
          begin `Reorder 
            (List.map 
              begin fun (name,(d:Flx_bexpr.t option)) ->
              try
                let j,t = List.assoc name rs in
                (bexpr_get_n t j tbe2 : Flx_bexpr.t)
              with Not_found ->
                match d with
                | Some d ->d
                | None -> raise Not_found
              end (* fun *)
              pnames
            )
          end (* remapping *)
        end (* paramspec *)
      | _ -> raise Not_found (* not functions *)
      end (* symbol table lookup *)
    | _ -> raise Not_found  (* not record or tuple *)
    end (* try closure *)
  | _ -> print_endline "WOOPS WHAT IF BE1 is NOT A CLOSURE?"; `None
  end (* match *)


let reorder sym_table sr be tbe1 tbe2 : reordering_t =
  try
  reorder' sym_table sr be tbe1 tbe2
  with Not_found -> `None


