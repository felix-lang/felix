open Flx_btype
open Flx_bexpr
open Flx_cexpr

let sbe b e = Flx_print.string_of_bound_expression b e
let sbt b t = Flx_print.string_of_btypecode (Some b) t
let catmap sep f ls = String.concat sep (List.map f ls)
let si i = string_of_int i
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

let modu a b = match a,b with
  | `Int x, `Int y -> `Int (x mod y)
  | _ -> `Mod (a,b)

let div a b = match a,b with
  | `Int x, `Int y -> `Int (x / y)
  | a, `Int 1 -> a
  | _ -> `Div (a,b)

let switch ts e = `Switch (ts,e)

let rec print_index bsym_table idx = 
  let pi x = print_index bsym_table x in
  match idx with
  | `Int n -> si n
  | `Mul (a,b) -> "(" ^ pi a ^ ")*(" ^ pi b ^")"
  | `Add (a,b) -> "(" ^ pi a ^ ")+(" ^ pi b ^")"
  | `Mod (a,b) -> "(" ^ pi a ^ ")%(" ^ pi b ^")"
  | `Div (a,b) -> "(" ^ pi a ^ ")/(" ^ pi b ^")"
  | `Expr e -> "Expr (" ^ sbe bsym_table e  ^ ")"
  | `Case_offset (ts,c) -> "case_offset ["^sbt bsym_table (Flx_btype.btyp_sum ts) ^"](" ^ pi c ^ ")"
  | `Switch (ts,e) -> "switch (" ^ pi e ^ ") { " ^ catmap "," (sbt bsym_table) ts ^ " }"


let rec cal_symbolic_array_index bsym_table (_,idxt as idx) = 
(*
print_endline ("Calsym " ^ sbe bsym_table idx^ ", type="^ sbt bsym_table idxt);
*)
  let cax x = cal_symbolic_array_index bsym_table x in
  match idx,idxt with
  | (BEXPR_tuple es,_), BTYP_tuple ts  -> 
    List.fold_left (fun acc (elt,t) -> add (mul acc  (`Int (sizeof_linear_type bsym_table t))) (cax elt)) (`Int 0)(List.combine es ts)

  | (BEXPR_tuple es,_), BTYP_array (t, BTYP_unitsum n)  -> 
    let sa = `Int (sizeof_linear_type bsym_table t) in
    List.fold_left (fun acc elt -> add (mul acc sa) (cax elt)) (`Int 0) es


  | (BEXPR_match_case (i,t'),_), BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " MATCH case tag " ^si i^ "  found");
    assert false;

  | (BEXPR_case_arg (i,t'),_), BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " MATCH case tag " ^si i^ "  found");
    assert false;


  | (BEXPR_apply ((BEXPR_case (i,its),t'),(_,bt as b)),_), BTYP_sum ts ->
(*
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " APPLY case tag "^ si i^" found");
print_endline ("Top level type is " ^ sbt bsym_table t');
print_endline ("Argument is " ^ sbe bsym_table b);
print_endline ("case type is " ^ sbt bsym_table its);
*)
    let caseno = i in 
    let ix = add (`Case_offset (ts, (`Int caseno))) (cax b) in
(*
print_endline ("Final index is " ^ print_index bsym_table ix);
*)
    ix

  | (BEXPR_case (i,t'),_), BTYP_sum ts ->
     let e' = expr idx in
     let caseno = i in 
     let caset = List.nth ts i in
     (`Case_offset (ts, (`Int caseno))) 

(*
  | (BEXPR_case (i,t'),_), BTYP_unitsum n ->
    assert false
*)

  (* this doesn't make sense! This is a decode but the
     above thing is an encode .. grrr 
  *)
  | (BEXPR_case (i,_),_), BTYP_tuple _ ->
    `Int i

  | (BEXPR_case (i,_),_), BTYP_array _ ->
    `Int i


  | (BEXPR_name _,_),_ -> 
    expr idx 

  | e, BTYP_unitsum _ -> 
    expr e

  | e, BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table e);
     let e' = expr e in
     let caseno = modu e' (`Int (List.length ts)) in
     add (`Case_offset (ts, caseno)) (switch ts e')

  | _ -> 
    print_endline ("cal_symbolic_array_index can't handle expression " ^ sbe bsym_table idx);
    assert false

(* this is an auxilliary table that represents the cumulative sizes of the
   components of a sum used at run time to find the offset of a particular
   component. If the selector is 0, the offset is zero, if it is j then
   the offset is the sum of the sizes of the first j-1 components. The tables
   are appended to *.rtti by Flx_ogen.
*)
let get_array_sum_offset_table bsym_table seq array_sum_offset_table ts =
   let t = Flx_btype.btyp_sum ts in
   try 
     let name,_ = Hashtbl.find array_sum_offset_table t in 
     name
   with Not_found ->
     let n = !seq in 
     incr seq;
     let name = "_gas_"^si n in
     let values =
(*
       List.iter
         (fun t -> print_endline ("Size of " ^ sbt bsym_table t ^ " is " ^ si  (size t)))
          ts
       ;
*)
       let sizes = List.map (sizeof_linear_type bsym_table) ts in
       let rec aux acc tsin tsout = 
         match tsin with
         | [] -> List.rev tsout
         | h :: t -> aux (acc + h) t (acc :: tsout)
       in
       let sizes = aux 0 sizes [] in
(*
       List.iter (fun x -> print_endline ("Sizes = " ^ si x)) sizes;
*)
       sizes
     in 
     Hashtbl.add array_sum_offset_table t (name,values);
     name

let get_power_table bsym_table power_table size =
  if not (Hashtbl.mem power_table size) then begin
    let name = "_ipow_"^si size in
    let values = ref [] in
     let pow = ref 1 in
     while (!pow) < 60000 do
       values := !pow :: !values;
       pow := !pow * size;
     done
     ;
     let values = List.rev (!values) in
     Hashtbl.add power_table size values;
  end
  ;
  "flx_ipow_"^si size

(* Linearise a structured index *)
let rec render_index bsym_table ge' array_sum_offset_table seq idx = 
  let ri x = render_index bsym_table ge' array_sum_offset_table seq x in
  match idx with
  | `Int n -> ce_atom (si n)
  | `Mul (a,b) -> ce_infix "*" (ri a) (ri b)
  | `Add (a,b) -> ce_infix "+" (ri a) (ri b)
  | `Mod (a,b) -> ce_infix "%" (ri a) (ri b)
  | `Div (a,b) -> ce_infix "/" (ri a) (ri b)
  | `Expr e -> ge' e
  | `Case_offset (ts, caseno) ->
     let index = ri caseno in
(*
     print_endline ("Caseno is " ^ string_of_cexpr index);
*)
     let table_name = get_array_sum_offset_table bsym_table seq array_sum_offset_table ts in
     let offset = ce_array (ce_atom table_name) index in
(*
     print_endline ("Offset is " ^ string_of_cexpr offset);
*)
     offset
  | `Switch (ts,e) ->
     let n = List.length ts in
     let rec aux i ts = 
       match ts with
       | [] -> ce_atom "-1" (* error *)
       | hd::tl -> 
         begin match hd with
         | BTYP_sum ls ->
           let m = List.length ls in
           let cond = ce_infix "==" (ce_atom (si i)) (ri (modu e (`Int m))) in
           let arg = ri (`Case_offset (ts, (div e (`Int m)))) in
           ce_cond cond arg (aux (i+1) tl)
         | _ ->
           let m = Flx_btype.ncases_of_sum bsym_table hd in
           let cond = ce_infix "==" (ce_atom (si i)) (ri (modu e (`Int m))) in
           let arg = ri (div e (`Int m)) in
           ce_cond cond arg (aux (i+1) tl)
         end
     in
     aux 0 ts
 


