open Flx_btype
open Flx_kind
(* checks for explicily specified as exact type, call this AFTER checking
specific types.

FIXME: this doesn't handle subkinding!
*)

(* FIXME should just use kind_ge2 and metatype! *)
let iskind k x = match x with
  | BTYP_type_var (_,mt) 
  | BTYP_typeop (_,_,mt) 
  | BTYP_type_apply (BTYP_type_function (_,mt,_),_)
  | BTYP_type_apply (BTYP_inst(_,_,KIND_function (_,mt)),_) 
  | BTYP_inst (_,_,mt) when mt = k -> true
  | BTYP_type_match _ 
  | BTYP_subtype_match _ ->
    Flx_kind.kind_ge2 k (Flx_btype_kind.metatype Flx_srcref.dummy_sr x)

  | _ -> false
 
(* ====== UNITSUM SUPPORT ================== *)

let isunitsum x = match x with
  | BTYP_void
  | BTYP_tuple [] 
  | BTYP_unitsum _ -> true
  | _ -> iskind KIND_unitsum x
 
let unitsum_int t =
  match t with
  | BTYP_void -> Some 0
  | BTYP_tuple [] -> Some 1
  | BTYP_unitsum n -> Some n
  | _ -> None

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
   then begin
     failwith ("Flx_btype: typeop " ^ op ^ " requires unitsum arguments, got:" ^
       "\nArg1 = " ^ st x ^
       "\nArg2 = " ^ st y ^
       "\n"
     );
   end;

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

(* ====== BOOL SUPPORT ================== *)

let isstaticbool x = match x with
  | BBOOL _ -> true 
  | _ -> iskind KIND_bool x

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
      print_endline ("Flx_btype: typeop "^op^" requires staticbool arguments, got " ^ Flx_btype.st x
      );
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
  | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires unit argument, got " ^ Flx_btype.st t)

let rec type_to_staticbool mk_raw_typeop op t =
(*
print_endline ("CONVERTING TYPE TO STATICBOOL: " ^ st t);
*)
  match t with
  | BTYP_void -> bbool false
  | BTYP_tuple [] -> bbool true
  | _ -> mk_raw_typeop op t Flx_kind.KIND_bool

let type_le mk_raw_typeop op t k =
  if k <> KIND_bool
  then failwith ("Flx_btype: typeop " ^ op ^ " requires staticbool result kind");

  match t with
  | BTYP_type_tuple [lhs; rhs] ->
print_endline ("type_lt, args = " ^ st lhs ^ ", " ^ st rhs);
    let d = Flx_bid.BidSet.empty in
    let eqn = `Ge, (rhs, lhs) in
    let eqns = [eqn] in
    let r = 
      try
        ignore(Flx_btype.unif eqns d);
        bbool true
      with Not_found -> bbool false

    in
    r

  | _ -> failwith ("Flx_btype: typeop " ^ op ^ " requires two type arguments")


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

  | "_type_le" -> type_le mk_raw_typeop op t k


  | _ -> failwith ("Unknown operator " ^ op ^ ": " ^ str_of_btype t ^ " -> " ^ sk k)

let set_eval_typeop () = 
  Flx_btype.eval_typeop := Some eval_typeop

