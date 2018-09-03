open Flx_btype
open Flx_kind

let unitsum_int t =
  match t with
  | BTYP_void -> Some 0
  | BTYP_tuple [] -> Some 1
  | BTYP_unitsum n -> Some n
  | _ -> None

let isunitsum x = match x with
  | BTYP_void
  | BTYP_tuple [] 
  | BTYP_unitsum _ -> true
  | BTYP_type_var (_,mt) 
  | BTYP_typeop (_,_,mt)
    -> (match mt with | KIND_unitsum -> true | _ -> false)
  | _ -> false
 


let rec gcd m n = 
  if m = 0  && n = 0 then 1  (* technically, infinity since all positive integers divide 0 *)
  else if n = 0 then m       (* m can't be 0 because of previous test *)
  else gcd n (m mod n)

let lcm m n = 
  let x = m * n in
  if x = 0 then 0 
  else x / (gcd m n)


let unitsum_binop mk_raw_typeop op t k eval =
  if k <> KIND_unitsum 
  then failwith ("Flx_btype: typeop " ^ op ^ " requires unitsum result kind");

  match t with
  | BTYP_type_tuple [x; y] ->
   if not (isunitsum x && isunitsum y) 
   then failwith ("Flx_btype: typeop " ^ op ^ " requires unitsum arguments");

   begin match unitsum_int x, unitsum_int y with
   | Some m, Some n -> btyp_unitsum (eval m n)
   | _ -> mk_raw_typeop op t k
   end
 | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires two unitsum arguments")

let unitsum_cmp mk_raw_typeop op t k eval =
  if k <> KIND_bool
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool result kind");

  match t with
  | BTYP_type_tuple [x; y] ->
   if not (isunitsum x && isunitsum y) 
   then failwith ("Flx_btype: typeop " ^ op ^ " requires unitsum arguments");

   begin match unitsum_int x, unitsum_int y with
   | Some m, Some n -> bbool (eval m n)
   | _ -> mk_raw_typeop op t k
   end
 | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires two unitsum arguments")


let isstaticbool x = match x with 
  | BBOOL _ -> true 
  | BTYP_type_var (_,mt) 
  | BTYP_typeop (_,_,mt)
    -> (match mt with | KIND_bool -> true | _ -> false)
  | _ -> false

let staticbool_binop mk_raw_typeop op t k eval =
  if k <> KIND_bool
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool result kind");

  match t with
  | BTYP_type_tuple [x; y] ->
    if not (isstaticbool x && isstaticbool y) 
    then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool arguments");

    begin match x, y with
    | BBOOL m, BBOOL n -> bbool (eval m n)
    | _ -> mk_raw_typeop op t k
    end
 | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires two bool arguments")

(* accepts any number of arguments including none *)
let staticbool_and mk_raw_typeop op (t:Flx_btype.t) =
(*
  print_endline ("STATICBOOL and of " ^ st t);
*)
  let conjuncts = 
    match t with
    | BTYP_type_tuple ts -> ts
    | x -> [x]
  in
  List.iter (fun x -> 
    if not (isstaticbool x)
    then begin 
      print_endline ("Flx_btype: typeop "^op^" requires staticbool arguments");
      failwith ("Flx_btype: typeop "^op^" requires staticbool arguments")
    end
  ) conjuncts;
  begin try 
   let conjuncts = List.filter 
     (fun t -> 
       match t with | BBOOL false -> raise Not_found | BBOOL true -> false | _ -> true
     )
     conjuncts
   in
   begin match conjuncts with
   | [] -> bbool true
   | [x] -> x
   | _ -> mk_raw_typeop op (btyp_type_tuple conjuncts) KIND_bool
   end

  with Not_found -> bbool false
  end

let staticbool_unop mk_raw_typeop op t k eval =
  if k <> KIND_bool
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool result kind");

  if not (isstaticbool t ) 
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool argument");

  match t with
  | BBOOL m -> bbool (eval m)
  | _ -> mk_raw_typeop op t k

let staticbool_nullop mk_raw_typeop op t k eval =
  if k <> KIND_bool
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool result kind");

  match t with
  |  BTYP_tuple [] -> bbool (eval ())
  | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires unit argument")

let rec type_to_staticbool mk_raw_typeop op t =
(*
print_endline ("CONVERTING TYPE TO STATICBOOL: " ^ st t);
*)
  match t with
  | BTYP_void -> bbool false
  | BTYP_tuple [] -> bbool true
  | BTYP_intersect ts -> 
(*
    print_endline ("FOUND INTERSECTION, converting parts to static bool then calling staticbool_and");
*)
    let ts = List.map (type_to_staticbool mk_raw_typeop op) ts in
(*
    print_endline ("FOUND INTERSECTION, converted parts to static bool, now calling staticbool_and");
*)
    staticbool_and mk_raw_typeop op (btyp_type_tuple ts)
    
  | _ -> mk_raw_typeop op t Flx_kind.KIND_bool

let eval_typeop mk_raw_typeop op t k =
  match op with
  | "_unitsum_add"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> m + n)
  | "_unitsum_diff" -> unitsum_binop mk_raw_typeop op t k (fun m n -> abs (m - n))
  | "_unitsum_mul"  -> unitsum_binop mk_raw_typeop op t k (fun m n ->  m * n)
  | "_unitsum_div"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> m / n)
  | "_unitsum_mod"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> m mod n)
  | "_unitsum_min"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> min m n)
  | "_unitsum_max"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> max m n)
  | "_unitsum_gcd"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> gcd m n)
  | "_unitsum_lcm"  -> unitsum_binop mk_raw_typeop op t k (fun m n -> lcm m n)

  | "_unitsum_lt"  -> unitsum_cmp mk_raw_typeop op t k (fun m n -> m < n)

  (* shortcut and accepts any number of arguments in type_tuple *)
  | "_staticbool_and"   -> staticbool_and  mk_raw_typeop op t 

  | "_staticbool_or"    -> staticbool_binop  mk_raw_typeop op t k (fun m n -> m || n)
  | "_staticbool_xor"   -> staticbool_binop  mk_raw_typeop op t k (fun m n -> (m || n) && not (m && n))
  | "_staticbool_not"   -> staticbool_unop   mk_raw_typeop op t k (fun m -> not m)
  | "_staticbool_true"  -> staticbool_nullop mk_raw_typeop op t k (fun () -> true)
  | "_staticbool_false" -> staticbool_nullop mk_raw_typeop op t k (fun () -> false)

  | "_type_to_staticbool" -> type_to_staticbool mk_raw_typeop op t

  | _ -> failwith ("Unknown operator " ^ op ^ ": " ^ str_of_btype t ^ " -> " ^ sk k)

let set_eval_typeop () = 
  Flx_btype.eval_typeop := Some eval_typeop

