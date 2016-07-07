open Flx_btype
open Flx_bexpr
open Flx_ast

(* add two records, tuples, or whatever together *)

(* this works for ALL types! *)
let get_field_types t = 
  match t with 
  | BTYP_array (t,BTYP_unitsum n) -> List.map (fun _ -> "",t) (Flx_list.nlist n)
  | BTYP_tuple ts -> List.map (fun t-> "",t) ts
  | BTYP_record ts -> ts
  | BTYP_polyrecord (ts,v) -> ts
  | _ -> ["",t]

let isproduct t = 
  match t with
  | BTYP_array _
  | BTYP_tuple _
  | BTYP_record _ -> true
  | _ -> false

let ispolyproduct t = 
  match t with
  | BTYP_array _
  | BTYP_tuple _
  | BTYP_record _
  | BTYP_polyrecord _ -> true
  | _ -> false


let add (_,lt as la) (_,rt as ra) =
  (* this test is required to prevent, say, 1 + 2 making a tuple (1,2) *)
  let cando = isproduct lt || ispolyproduct rt in
  if not cando then  raise Flx_dot.OverloadResolutionError;

  let lhs = get_field_types lt in
  let lcs = 
    List.map2 (fun (s,ft) n -> 
      s,bexpr_apply ft ((bexpr_prj n lt ft), la)
    )
    lhs (Flx_list.nlist (List.length lhs))
  in
  bexpr_polyrecord lcs ra


