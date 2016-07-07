open Flx_btype
open Flx_bexpr
open Flx_cexpr

let sbe b e = Flx_print.string_of_bound_expression b e
let sbt b t = Flx_print.string_of_btypecode (Some b) t
let catmap sep f ls = String.concat sep (List.map f ls)
let si i = string_of_int i
let siu i = if i < 0 then string_of_int i else string_of_int i ^ "u"

let nth lst idx = 
  try List.nth lst idx 
  with _ -> failwith ("[flx_ixgen] nth failed, list length=" ^ 
    string_of_int (List.length lst) ^
    ", index sought=" ^string_of_int idx)

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
  | x, `Int y -> if y < 0 then `Sub (x, `Int ( - y )) else `Add (a,b)
  | _ -> `Add (a,b)

let sub a b = match a,b with
  | `Int x, `Int y -> `Int (x - y)
  | x, `Int y -> if y <= 0 then add x (`Int ( - y )) else `Sub (a,b)
  | _, _ -> `Sub (a,b)

let modu a b = match a,b with
  | `Int x, `Int y -> `Int (x mod y)
  | _ -> `Mod (a,b)

let div a b = match a,b with
  | `Int x, `Int y -> `Int (x / y)
  | a, `Int 1 -> a
  | _ -> `Div (a,b)

let switch ts e = `Switch (ts,e)

let get_array_sum_offset_values bsym_table ts =
  let sizes = List.map (sizeof_linear_type bsym_table) ts in
  let rec aux acc tsin tsout = 
    match tsin with
    | [] -> List.rev tsout
    | h :: t -> aux (acc + h) t (acc :: tsout)
  in
    aux 0 sizes []

let case_offset bsym_table ts caseno = match caseno with
  | `Int 0 -> `Int 0
  | `Int n -> `Int (nth (get_array_sum_offset_values bsym_table ts) n)
  | _ -> `Case_offset (ts,caseno)

let rec print_index bsym_table idx = 
  let pi x = print_index bsym_table x in
  match idx with
  | `Int n -> si n
  | `Mul (a,b) -> "(" ^ pi a ^ ")*(" ^ pi b ^")"
  | `Add (a,b) -> "(" ^ pi a ^ ")+(" ^ pi b ^")"
  | `Sub (a,b) -> "(" ^ pi a ^ ")-(" ^ pi b ^")"
  | `Mod (a,b) -> "(" ^ pi a ^ ")%(" ^ pi b ^")"
  | `Div (a,b) -> "(" ^ pi a ^ ")/(" ^ pi b ^")"
  | `Lookup (t,e) -> t ^ "[" ^ pi e ^ "]"
  | `Expr e -> "Expr (" ^ sbe bsym_table e  ^ ")"
  | `Case_offset (ts,c) -> "case_offset ["^sbt bsym_table (Flx_btype.btyp_sum ts) ^"](" ^ pi c ^ ")"
  | `Switch (ts,e) -> "switch (" ^ pi e ^ ") { " ^ catmap "," (sbt bsym_table) ts ^ " }"


(* Note: this calculates a compact linear value *)
let rec cal_symbolic_compact_linear_value bsym_table (_,idxt as idx) = 
(*
print_endline ("Calsym " ^ sbe bsym_table idx^ ", type="^ sbt bsym_table idxt);
*)
  let cax x = cal_symbolic_compact_linear_value bsym_table x in
  match idx,idxt with
  | (BEXPR_tuple es,_), BTYP_tuple ts  -> 
    (*  we get  ((0 * sizeof typeof i + i) * sizeof typeof j + j ) * sizeof typeof k + k 
        which is BIG ENDIAN. The sizeof i is eliminated by multiplying by 0.
        Example 3 * 4 * 5, so i:3, j:4, k:5 -> ijk = ((0 * 3 + i) * 4 + j) * 5 + k = 20i + 5j + k
     *)
    List.fold_left (fun acc (elt,t) -> add (mul acc  (`Int (sizeof_linear_type bsym_table t))) (cax elt)) (`Int 0)(List.combine es ts)

  | (BEXPR_tuple es,_), BTYP_array (t, BTYP_unitsum n)  -> 
    let sa = `Int (sizeof_linear_type bsym_table t) in
    List.fold_left (fun acc elt -> add (mul acc sa) (cax elt)) (`Int 0) es

  | (BEXPR_tuple es,_), BTYP_array (t, BTYP_tuple [])  -> 
    let sa = `Int (sizeof_linear_type bsym_table t) in
    List.fold_left (fun acc elt -> add (mul acc sa) (cax elt)) (`Int 0) es

  | (BEXPR_match_case (i,t'),_), BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " MATCH case tag " ^si i^ "  found");
    assert false;

  | (BEXPR_case_arg (i,t'),_), BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " MATCH case tag " ^si i^ "  found");
    assert false;

  | (BEXPR_apply ((BEXPR_prj (i,its,_),t'),(_,bt as b)),_), BTYP_tuple ts -> assert false

  (* I think this cannot happen! *)
  | (BEXPR_apply ((BEXPR_prj (i,its,_),t'),(_,bt as b)),_), BTYP_sum ts ->
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " APPLY prj"^ si i^" found");
print_endline ("Top level type is " ^ sbt bsym_table t');
print_endline ("Argument is " ^ sbe bsym_table b);
print_endline ("case type is " ^ sbt bsym_table its);
    assert false;
    let caseno = i in 
    let ix = add (case_offset bsym_table ts (`Int caseno)) (cax b) in
print_endline ("Final index is " ^ print_index bsym_table ix);
    ix

  (* For an injection, the domain must be the type of one of the summands of a sum *)
  (* So, its would have to be a member of ts *)
  | (BEXPR_apply ((BEXPR_inj (i,its,_),t'),(_,bt as b)),_), BTYP_sum ts ->
(*
print_endline ("Decomposing index of sum type " ^ sbe bsym_table idx ^ " APPLY inj"^ si i^" found");
print_endline ("Top level type is " ^ sbt bsym_table t');
print_endline ("Argument is " ^ sbe bsym_table b);
print_endline ("case type is " ^ sbt bsym_table its);
*)
    let caseno = i in 
    let ix = add (case_offset bsym_table ts (`Int caseno)) (cax b) in
(*
print_endline ("Final index is " ^ print_index bsym_table ix);
*)
    ix


  | (BEXPR_case (i,t'),_), BTYP_sum ts ->
     let e' = expr idx in
     let caseno = i in 
     let caset = nth ts i in
     (case_offset bsym_table ts (`Int caseno)) 

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


  | (BEXPR_varname _,_),_ -> 
    expr idx 

  | e, BTYP_unitsum _ -> 
    expr e

  | e, BTYP_sum ts ->
(*
print_endline ("xDecomposing index of sum type " ^ sbe bsym_table e);
*)
     let e' = expr e in
     (* top level case number is e mod ncases .. ? WRONG? That puts the
        top level selector last instead of first! So if we join two arrays,
        sequential indexing would alternate between the arrays, instead
        of scanning one, then the other.

        I'm being too smart! The expression "would be" the correct value
        were the array coerced to a linear form.
     *)
     (*
     let caseno = modu e' (`Int (List.length ts)) in
     add (case_offset bsym_table ts caseno) (switch ts e')
     *)
     e'

  | e,BTYP_tuple [] -> `Int 0 

  | e,BTYP_tuple _ -> 
(*
    print_endline ("cal_symbolic_compact_linear_value can't handle expression of tuple type " ^ sbe bsym_table idx);
    print_endline ("Assume already linearised");
*)
    expr e 

  | e,BTYP_array _ -> 
(*
    print_endline ("cal_symbolic_compact_linear_value can't handle expression of array type " ^ sbe bsym_table idx);
    print_endline ("Assume already linearised");
*)
    expr e 


  | e,_ -> 
    print_endline ("cal_symbolic_compact_linear_value can't handle expression " ^ sbe bsym_table idx);
    print_endline ("type " ^ sbt bsym_table idxt);
    assert false
(* ;
    print_endline ("Assume already linearised");
    expr e 
*)

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
     Hashtbl.add array_sum_offset_table t (name, get_array_sum_offset_values bsym_table ts);
     name

(* A lookup table computing size^n for n in [0..count) *)
let get_power_table bsym_table power_table size count =
  begin
  if try
       (List.length (Hashtbl.find power_table size)) < count
     with Not_found -> true then
    let values =
      let rec ipows = function 
            | n,l when n < 1 -> l
            | n,(h::t as l) -> ipows (pred n, (size*h)::l)
            | _ -> invalid_arg "power_table ipows"
      in List.rev (ipows (pred count, [1]))
    in
      Hashtbl.replace power_table size values
  end;
  "flx_ipow_"^si size


(* Linearise a structured index *)
let rec render_compact_linear_value bsym_table ge' array_sum_offset_table seq idx : Flx_ctypes.cexpr_t = 
  let ri x = render_compact_linear_value bsym_table ge' array_sum_offset_table seq x in
  match idx with
  | `Int n -> ce_atom (siu n)
  | `Mul (a,b) -> ce_infix "*" (ri a) (ri b)
  | `Add (a,b) -> ce_infix "+" (ri a) (ri b)
  | `Sub (a,b) -> ce_infix "-" (ri a) (ri b)
  | `Mod (a,b) -> ce_infix "%" (ri a) (ri b)
  | `Div (a,b) -> ce_infix "/" (ri a) (ri b)
  | `Lookup (tbl,e) -> ce_array (ce_atom tbl) (ri e)
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
           let cond = ce_infix "==" (ce_atom (siu i)) (ri (modu e (`Int m))) in
           let arg = ri (case_offset bsym_table ts (div e (`Int m))) in
           ce_cond cond arg (aux (i+1) tl)
         | _ ->
           let m = Flx_btype.ncases_of_sum bsym_table hd in
           let cond = ce_infix "==" (ce_atom (siu i)) (ri (modu e (`Int m))) in
           let arg = ri (div e (`Int m)) in
           ce_cond cond arg (aux (i+1) tl)
         end
     in
     aux 0 ts
 

let assign_to_packed_tuple bsym_table ge' ge sr e2 n j ts t' var = 
(*
print_endline "Assign to packed tuple";
*)
    let rec aux1 ls i out = 
       match ls with [] -> assert false 
       | h :: t ->
         if i = 0 then out,h
         else aux1 t (i-1) (sizeof_linear_type bsym_table h * out)
    in 
    let lo,elt = aux1 (List.rev ts) (List.length ts - j - 1) 1 in
    let elt = sizeof_linear_type bsym_table elt in
(*
print_endline ("Type of variable is " ^ sbt bsym_table t');
print_endline ("proj = " ^ si j^ ", Size of component = " ^ si elt ^ ", size of lower bit = " ^ si lo);
*)
    let ci i = ce_atom (siu i) in
    let celt = ci elt in
    let clo = ci lo in
    let clomelt = ci (lo * elt) in
    let ad x y = ce_infix "+" x y in
    let di x y = ce_infix "/" x y in
    let mu x y = ce_infix "*" x y in
    let mo x y = ce_infix "%" x y in
    let lhs = ge' sr var in
    let rhs = ge' sr e2 in
    let nuval =  ad (mu (ad (mu (di lhs clomelt) celt) rhs) clo) (mo lhs clo) in
    let cnuval = string_of_cexpr nuval in
(*
    print_endline ("Formula = " ^ cnuval);
*)
    ge sr var ^ " = " ^ cnuval ^ "; //assign to packed tuple\n"

(* Checks LHS of assignment for projection of compact linear type *)
let projoflinear bsym_table e = match e with
  | BEXPR_apply ((BEXPR_prj _,_),(_,t')),_ 
  | BEXPR_apply ((BEXPR_aprj _,_),(_,t')),_ 
    when islinear_type bsym_table t' -> true
  | _ -> false


(* the parameter "e" here is not used, what is it for? Apart from the diagnostics .. *)
let handle_get_n (syms:Flx_mtypes2.sym_state_t) bsym_table ls rt ge' e t n ((e',t') as arg) : Flx_ctypes.cexpr_t =
  let seq = syms.Flx_mtypes2.counter in
  let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
print_endline ("Flx_ixgen:handle_get_n expr : " ^ sbe bsym_table e);
print_endline ("Flx_ixgen:handle_get_n arg  : " ^ sbe bsym_table arg);
  let sidx = cal_symbolic_compact_linear_value bsym_table arg in
        (* now calculate the projection: this is given by 
             idx / a % b
             where a = product of sizes of first n  terms (or 1 if n=0)
             and b = size of term n

             Example:

             type 3 * 4 * 5: i,j,k
             encoding is 20i + 5j + k
             decoding is:
                 k = ijk / 1 % 5
                 j = ijk / 5 % 4
                 i = ijk / 20 % 3  [the %3 is not required here]

           SO: to get the n'th projection, where n=0 is i, n=1 is j, n=2 is k,
           numbering big endian digits left to right, we have to divide by
           the product of sizes of all the terms to its right: 

               size(n+1) * size(n+2) ... (size m) 
          
           where there are m digits. This is m - n - 1 terms.
           Here n ranges from 0 to m - 1, m - n thus ranges from m - 0 = m to m - (m - 1) = 1
           and so the number of terms ranges from m - 1 to 0
        *)
  assert (0 <= n && n < List.length ls);
  let rec aux ls i out = match ls with [] -> assert false | h :: t ->
    if i = 0 then out else aux t (i-1) (sizeof_linear_type bsym_table h * out)
  in 
  let a = aux (List.rev ls) (List.length ls - n - 1) 1 in
  let b = sizeof_linear_type bsym_table (nth ls n) in
(*
        let apart = if a = 1 then cidx else ce_infix "/" cidx (ce_atom (si a)) in
        let result = if n = List.length ls - 1 then apart else ce_infix "%" apart (ce_atom (si b)) in
*)
  let result = modu (div sidx (`Int a)) (`Int b) in
  render_compact_linear_value bsym_table ge' array_sum_offset_table seq result

(* at is known to be a compact linear type *)
let handle_get_n_array_clt syms bsym_table ge' idx idxt v aixt at a =
(*
print_endline "handle_get_n_array_clt";
*)
  let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
(*
print_endline ("Array len = " ^ si array_len);
*)
  let seq = syms.Flx_mtypes2.counter in
  let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
  let power_table = syms.Flx_mtypes2.power_table in
  let ipow' base exp = match exp with
      | `Int i -> 
          let rec ipow = begin function 0 -> 1 | n -> base * (ipow (n - 1)) end in
            `Int (ipow i)
      | _ ->
          let ipow = get_power_table bsym_table power_table base array_len in
            `Lookup (ipow, exp)
  in

(*
    print_endline "Projection of linear type!";
*)
    assert (idxt = aixt);
    assert (islinear_type bsym_table v);
    let array_value_size = sizeof_linear_type bsym_table v in
(*
print_endline ("Array_value_size=" ^ si array_value_size);
*)
    let sidx = cal_symbolic_compact_linear_value bsym_table idx in
    let sarr = cal_symbolic_compact_linear_value bsym_table a in
(*
print_endline ("Symbolic index = " ^ Flx_ixgen.print_index bsym_table sidx );
print_endline ("Symbolic array = " ^ Flx_ixgen.print_index bsym_table sarr );
*)
    let sdiv = ipow' array_value_size (sub (`Int (array_len - 1)) sidx) in
    let result = (modu (div sarr sdiv) (`Int array_value_size)) in
    render_compact_linear_value bsym_table ge' array_sum_offset_table seq result

(* at is known NOT to be a compact linear type (the index is though) *)
let handle_get_n_array_nonclt syms bsym_table ge' idx idxt aixt at a =
(*
print_endline "handle_get_n_array_nonclt";
*)
  let seq = syms.Flx_mtypes2.counter in
  let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
(*
print_endline ("Get n .. array " ^ sbe bsym_table a);
print_endline ("array type " ^ sbt bsym_table at);
*)
    assert (idxt = aixt); 
(*
print_endline ("index type = " ^ sbt bsym_table idxt );
print_endline ("index value = " ^ sbe bsym_table idx );
*)
    let sidx = cal_symbolic_compact_linear_value bsym_table idx in
(*
print_endline ("Symbolic index = " ^ Flx_ixgen.print_index bsym_table sidx );
*)
    let cidx = render_compact_linear_value bsym_table ge' array_sum_offset_table seq sidx in
(*
print_endline ("rendered lineralised index .. C index = " ^ string_of_cexpr cidx);
*)
    ce_array (ce_dot (ge' a) "data") cidx 



