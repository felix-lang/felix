open Flx_btype
open Flx_bexpr
open Flx_cexpr

let sbe b e = Flx_print.string_of_bound_expression b e
let sbt b t = Flx_print.string_of_btypecode (Some b) t

let isindex bsym_table t =
  let rec aux t = match t with
  | BTYP_unitsum _ -> ()
  | BTYP_sum ls -> List.iter aux ls
  | BTYP_tuple ls -> List.iter aux ls
  (* later we should allow nominally typed structs and sums too *)
  | _ -> raise Not_found
  in try aux t; true with Not_found -> false


let rec size t = match t with
  | BTYP_unitsum n -> n
  | BTYP_tuple ls ->
    List.fold_left (fun acc elt -> acc * size elt) 1 ls
  | BTYP_sum ls ->
    List.fold_left (fun acc elt -> acc + size elt) 0 ls
  | BTYP_void -> 0
  | _ -> assert false

(* Note that this computation must be driven by the array index type not
  the type of the index.
*)
let expr e = match e with
  | BEXPR_case (i,_),_ -> `Int i
  | _ -> `Expr e

let mul a b = match a,b with
  | `Int x, `Int y -> `Int (x * y)
  | `Int 0, _ 
  | _, `Int 0 -> `Int 0
  | `Int 1, x 
  | x, `Int 1 -> x
  | _ -> `Mul (a,b)

let add a b = match a,b with
  | `Int x, `Int y -> `Int (x + y)
  | `Int 0, x
  | x, `Int 0 -> x
  | _ -> `Add (a,b)


let rec cal_symbolic_array_index idx t = 
  let cax x t = cal_symbolic_array_index x t in
  match idx,t with
  | e, BTYP_unitsum _ -> expr e
  | (BEXPR_tuple es,_), BTYP_tuple ts  -> 
    List.fold_left (fun acc (elt,t) -> add (mul acc  (`Int (size t))) (cax elt t)) (`Int 0)(List.combine es ts)

  | e, BTYP_sum ts ->
    `Case_offset e

  | _ -> assert false

let rec print_index bsym_table idx = match idx with
  | `Int n -> string_of_int n
  | `Mul (a,b) -> "(" ^ print_index bsym_table a ^ ")*(" ^ print_index bsym_table b ^")"
  | `Add (a,b) -> "(" ^ print_index bsym_table a ^ ")+(" ^ print_index bsym_table b ^")"
  | `Expr e -> "Expr (" ^ sbe bsym_table e  ^ ")"
  | `Case_offset e -> "case_offset (" ^ sbe bsym_table e ^ ")"

(* this is an auxilliary table that represents the cumulative sizes of the
   components of a sum used at run time to find the offset of a particular
   component. If the selector is 0, the offset is zero, if it is j then
   the offset is the sum of the sizes of the first j-1 components. The tables
   are appended to *.rtti by Flx_ogen.
*)
let get_array_sum_offset_table bsym_table seq array_sum_offset_table ts t =
   try 
     let name,_ = Hashtbl.find array_sum_offset_table t in 
     name
   with Not_found ->
     let n = !seq in 
     incr seq;
     let name = "_gas_"^string_of_int n in
     let values =
       List.iter
         (fun t -> print_endline ("Size of " ^ sbt bsym_table t ^ " is " ^ string_of_int  (size t)))
          ts
       ;
       let sizes = List.map size ts in
       let rec aux acc tsin tsout = 
         match tsin with
         | [] -> List.rev tsout
         | h :: t -> aux (acc + h) t (acc :: tsout)
       in
       let sizes = aux 0 sizes [] in
       List.iter (fun x -> print_endline ("Sizes = " ^ string_of_int x)) sizes;
       sizes
     in 
     Hashtbl.add array_sum_offset_table t (name,values);
     name

let rec render_index bsym_table ge' array_sum_offset_table seq idx = 
  let ri x = render_index bsym_table ge' array_sum_offset_table seq x in
  match idx with
  | `Int n -> ce_atom (string_of_int n)
  | `Mul (a,b) -> ce_infix "*" (ri a) (ri b)
  | `Add (a,b) -> ce_infix "+" (ri a) (ri b)
  | `Expr e -> ge' e
  | `Case_offset ((_,(BTYP_sum ts as t)) as e) -> 
     let index = ge' e in
     print_endline ("Index is " ^ string_of_cexpr index);
     let table_name = get_array_sum_offset_table bsym_table seq array_sum_offset_table ts t in
     ce_array (ce_atom table_name) index 

  | `Case_offset _ -> assert false



