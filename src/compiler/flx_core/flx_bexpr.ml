type bexpr_t =
  | BEXPR_not of t
  | BEXPR_deref of t
  | BEXPR_varname of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_ref of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_class_new of Flx_btype.t * t
  | BEXPR_literal of Flx_literal.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_variant of string * t
  | BEXPR_closure of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_btype.t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t
  | BEXPR_compose of t * t
  | BEXPR_tuple_tail of t
  | BEXPR_tuple_head of t
  | BEXPR_tuple_cons of t * t
  | BEXPR_prj of int * Flx_btype.t * Flx_btype.t
  | BEXPR_aprj of t * Flx_btype.t * Flx_btype.t
  | BEXPR_inj of int * Flx_btype.t * Flx_btype.t
  | BEXPR_label of string
  | BEXPR_unit 
  | BEXPR_unitptr

  (* This term is the product of a tuple or array of functions,
     to be eliminated after monomorphisation because it's a heck
     of a lot easier to do it then than on binding. It's replaced
     by an ordinary function application, and the function generated
  *)
  | BEXPR_funprod of t
  | BEXPR_funsum of t
  | BEXPR_lrangle of t (* mediating morphism of a product *)
  | BEXPR_lrbrack of t (* mediating morphism of a sum *)

and t = bexpr_t * Flx_btype.t

(* -------------------------------------------------------------------------- *)

let complete_check t = 
(*
  if Flx_btype.complete_type t then t else
  let () = print_endline "WARNING: expression type has free fixpoint" in
*)
  t
let complete_check_list ts = List.map complete_check ts

let bexpr_unit = BEXPR_unit,Flx_btype.BTYP_tuple []
let bexpr_unitptr = BEXPR_unitptr, Flx_btype.BTYP_pointer (Flx_btype.BTYP_tuple [])


let bexpr_label e = BEXPR_label e, Flx_btype.btyp_label ()
let bexpr_tuple_tail t e = BEXPR_tuple_tail e, complete_check t
let bexpr_tuple_head t e = BEXPR_tuple_head e, complete_check t

let bexpr_tuple_cons t (eh,et) = BEXPR_tuple_cons (eh,et), complete_check t

let bexpr_deref t e : t = 
  match t with
  | Flx_btype.BTYP_tuple [] -> bexpr_unitptr
  | _ -> BEXPR_deref e, complete_check t

let bexpr_not (e,t) : t = BEXPR_not (e,t), complete_check t

let bexpr_varname t (bid, ts) = 
(*
  print_endline ("Creating varname term " ^ string_of_int bid);
*)
  BEXPR_varname (bid, complete_check_list ts), complete_check t

let bexpr_ref t (bid, ts) = 
  match t with 
  | Flx_btype.BTYP_tuple [] -> bexpr_unitptr 
  | _ -> BEXPR_ref (bid, complete_check_list ts), complete_check t

let bexpr_likely ((_,t) as e) = BEXPR_likely e, complete_check t

let bexpr_unlikely ((_,t) as e) = BEXPR_unlikely e, complete_check t

let bexpr_address ((_,t) as e) = 
  match t with 
  | Flx_btype.BTYP_tuple [] -> bexpr_unitptr 
  | _ -> BEXPR_address e, complete_check (Flx_btype.btyp_pointer t)

let bexpr_new ((_,t) as e) = 
  match t with 
  | Flx_btype.BTYP_tuple [] -> bexpr_unitptr 
  | _ -> BEXPR_new e, complete_check (Flx_btype.btyp_pointer t)

let bexpr_class_new cl e = BEXPR_class_new (cl,e), complete_check (Flx_btype.btyp_pointer cl)

let bexpr_literal t l = BEXPR_literal l, complete_check t

let bexpr_apply t (e1, e2) = BEXPR_apply (e1, e2), complete_check t

let bexpr_apply_prim t (bid, ts, e) = BEXPR_apply_prim (bid, complete_check_list ts, e), complete_check t

let bexpr_apply_direct t (bid, ts, e) = BEXPR_apply_direct (bid, complete_check_list ts, e), complete_check t

let bexpr_apply_stack t (bid, ts, e) = BEXPR_apply_stack (bid, complete_check_list ts, e), complete_check t

let bexpr_apply_struct t (bid, ts, e) = BEXPR_apply_struct (bid, complete_check_list ts, e), complete_check t

let bexpr_tuple t es = match es with [] -> bexpr_unit | _ -> BEXPR_tuple es, complete_check t

let bexpr_record es = 
  let cmp (s1,e1) (s2,e2) = compare s1 s2 in
  let es = List.sort cmp es in
  let ts = List.map (fun (s,(_,t)) -> s,t) es in
  BEXPR_record es, complete_check (Flx_btype.btyp_record "" ts)

let bexpr_variant t (n, e) = BEXPR_variant (n, e), complete_check t


let bexpr_aprj ix d c = BEXPR_aprj (ix,d,c),complete_check (Flx_btype.BTYP_function (d,c))
let bexpr_prj n d c = BEXPR_prj (n,d,c),complete_check (Flx_btype.BTYP_function (d,c))
let bexpr_inj n d c = BEXPR_inj (n,d,c),complete_check (Flx_btype.BTYP_function (d,c))
let bexpr_get_n c n (e,d) =  BEXPR_apply ( bexpr_prj n d c, (e,d) ), complete_check c


let bexpr_closure t (bid, ts) = 
(*
  print_endline ("Creating closure term " ^ string_of_int bid);
*)
  BEXPR_closure (bid, complete_check_list ts), complete_check t

let bexpr_const_case (i, t) = BEXPR_case (i, t), complete_check t

let bexpr_nonconst_case argt (i, sumt) = BEXPR_inj (i, argt, sumt), complete_check (Flx_btype.btyp_function (argt,sumt))

let bexpr_match_case (i, e) = BEXPR_match_case (i, e), Flx_btype.btyp_unitsum 2

let bexpr_case_arg t (i, e) = BEXPR_case_arg (i, e), complete_check t

let bexpr_case_index t e = BEXPR_case_index e, complete_check t

let bexpr_expr (s, t) = BEXPR_expr (s, t), complete_check t

let bexpr_range_check t (e1, e2, e3) = BEXPR_range_check (e1, e2, e3), complete_check t

let bexpr_coerce (e, t) = BEXPR_coerce (e, t), complete_check t

let bexpr_compose t (e1, e2) = BEXPR_compose (e1, e2), complete_check t

let bexpr_unitsum_case i j =
  let case_type = Flx_btype.btyp_unitsum j in
  bexpr_const_case (i, case_type)

let bexpr_funprod t e = BEXPR_funprod e,t
let bexpr_funsum t e = BEXPR_funsum e,t
let bexpr_lrangle t e = BEXPR_lrangle e,t
let bexpr_lrbrack t e = BEXPR_lrbrack e,t

(* -------------------------------------------------------------------------- *)

(** Extract the type arguments of a bound expression. *)
let get_ts (e,_) =
  match e with
  | BEXPR_varname (_, ts)
  | BEXPR_closure (_, ts)
  | BEXPR_ref (_, ts)
  | BEXPR_apply_prim (_, ts, _)
  | BEXPR_apply_direct (_, ts, _)
  | BEXPR_apply_struct (_, ts, _) -> ts
  | _ -> []


(** Return whether or not one bound expression is equivalent with another bound
 * expression. *)
let rec cmp ((a,_) as xa) ((b,_) as xb) =
  (* Note that we don't bother comparing the type subterm: this had better be
   * equal for equal expressions: the value is merely the cached result of a
   * synthetic context independent type calculation *)
  match a,b with
  | BEXPR_label lab, BEXPR_label lab' -> lab = lab'
  | BEXPR_coerce (e,t),BEXPR_coerce (e',t') ->
    (* not really right .. *)
    cmp e e'

  | BEXPR_record ts,BEXPR_record ts' ->
    List.length ts = List.length ts' &&
    List.map fst ts = List.map fst ts' &&
    List.fold_left2 (fun r a b -> r && a = b)
      true (List.map snd ts) (List.map snd ts')

  | BEXPR_variant (s,e),BEXPR_variant (s',e') ->
    s = s' && cmp e e'

  | BEXPR_not (e),BEXPR_not (e') 
  | BEXPR_deref e,BEXPR_deref e' -> cmp e e'

  | BEXPR_varname (i,ts),BEXPR_varname (i',ts')
  | BEXPR_ref (i,ts),BEXPR_ref (i',ts')
  | BEXPR_closure (i,ts),BEXPR_closure (i',ts') ->
     i = i' && List.fold_left2 (fun r a b -> r && a = b) true ts ts'

  (* Note any two distinct new expressions are distinct ...
   * not sure what is really needed here *)
  | BEXPR_new e1,BEXPR_new e2 -> false
  | BEXPR_class_new _,BEXPR_class_new _ -> false

  | _,BEXPR_likely e2
  | _,BEXPR_unlikely e2 -> cmp xa e2

  | BEXPR_likely e1,_
  | BEXPR_unlikely e1,_ -> cmp e1 xb

  | BEXPR_literal a,BEXPR_literal a' -> a == a'

  | BEXPR_apply (a,b),BEXPR_apply (a',b') -> cmp a a' && cmp b b'

  | BEXPR_apply_prim (i,ts,b),BEXPR_apply_prim (i',ts',b')
  | BEXPR_apply_direct (i,ts,b),BEXPR_apply_direct (i',ts',b')
  | BEXPR_apply_struct (i,ts,b),BEXPR_apply_struct (i',ts',b')
  | BEXPR_apply_stack (i,ts,b),BEXPR_apply_stack (i',ts',b') ->
     i = i' &&
     List.fold_left2 (fun r a b -> r && a = b) true ts ts' &&
     cmp b b'

  | BEXPR_tuple [], BEXPR_unit
  | BEXPR_unit,BEXPR_tuple [] -> true

  | BEXPR_tuple ls,BEXPR_tuple ls' ->
     List.fold_left2 (fun r a b -> r && cmp a b) true ls ls'

  | BEXPR_case_arg (i,e),BEXPR_case_arg (i',e')
  | BEXPR_match_case (i,e),BEXPR_match_case (i',e') ->
    i = i' && cmp e e'

  | BEXPR_case_index e,BEXPR_case_index e' -> cmp e e'

  | BEXPR_case (i,t),BEXPR_case (i',t') -> i = i' && t = t'
  | BEXPR_expr (s,t),BEXPR_expr (s',t') -> s = s' && t = t'
  | BEXPR_range_check (e1,e2,e3), BEXPR_range_check (e1',e2',e3') ->
    cmp e1 e1' && cmp e2 e2' && cmp e3 e3'

  | BEXPR_tuple_head e1, BEXPR_tuple_head e2
  | BEXPR_tuple_tail e1, BEXPR_tuple_tail e2 ->
    cmp e1 e2

  | BEXPR_tuple_cons (eh,et), BEXPR_tuple_cons (eh',et') ->
    cmp eh eh' && cmp et et'

  | BEXPR_aprj (ix,d,c), BEXPR_aprj (ix',d',c') ->
    d = d' && c = c' && cmp ix ix'

  | BEXPR_prj (n,d,c), BEXPR_prj (n',d',c') ->
    d = d' && c = c' && n = n'

  | BEXPR_inj (n,d,c), BEXPR_inj (n',d',c') ->
    d = d' && c = c' && n = n'
  | BEXPR_funprod e, BEXPR_funprod e' ->
    cmp e e'
  | BEXPR_funsum e, BEXPR_funsum e' ->
    cmp e e'
  | BEXPR_lrangle e, BEXPR_lrangle e' ->
    cmp e e'
  | BEXPR_lrbrack e, BEXPR_lrbrack e' ->
    cmp e e'

  | _ -> false

(* -------------------------------------------------------------------------- *)

(* this routine applies arguments HOFs to SUB components only, not to the actual
 * argument. It isn't recursive, so the argument HOF can be. *)
let flat_iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ?(f_label=fun _ -> ())
  ((x,t) as e) =
  match x with
  | BEXPR_label s -> f_label s
  | BEXPR_not e -> f_bexpr e
  | BEXPR_deref e -> f_bexpr e
  | BEXPR_ref (i,ts) ->
      f_bid i;
      List.iter f_btype ts
  | BEXPR_likely e -> f_bexpr e
  | BEXPR_unlikely e -> f_bexpr e
  | BEXPR_address e -> f_bexpr e
  | BEXPR_new e -> f_bexpr e
  | BEXPR_class_new (t,e) -> f_btype t; f_bexpr e
  | BEXPR_apply (e1,e2) ->
      f_bexpr e1;
      f_bexpr e2
  | BEXPR_compose (e1,e2) ->
      f_bexpr e1;
      f_bexpr e2
  | BEXPR_apply_prim (i,ts,e2) ->
      f_bid i;
      List.iter f_btype ts;
      f_bexpr e2
  | BEXPR_apply_direct (i,ts,e2) ->
      f_bid i;
      List.iter f_btype ts;
      f_bexpr e2
  | BEXPR_apply_struct (i,ts,e2) ->
      f_bid i;
      List.iter f_btype ts;
      f_bexpr e2
  | BEXPR_apply_stack (i,ts,e2) ->
      f_bid i;
      List.iter f_btype ts;
      f_bexpr e2
  | BEXPR_tuple es -> List.iter f_bexpr es
  | BEXPR_record es -> List.iter (fun (s,e) -> f_bexpr e) es
  | BEXPR_variant (s,e) -> f_bexpr e
  | BEXPR_closure (i,ts) ->
      f_bid i;
      List.iter f_btype ts
  | BEXPR_varname (i,ts) ->
      f_bid i;
      List.iter f_btype ts
  | BEXPR_case (i,t') -> f_btype t'
  | BEXPR_match_case (i,e) -> f_bexpr e
  | BEXPR_case_arg (i,e) -> f_bexpr e
  | BEXPR_case_index e -> f_bexpr e
  | BEXPR_literal x -> f_btype t
  | BEXPR_expr (s,t1) -> f_btype t1
  | BEXPR_range_check (e1,e2,e3) ->
      f_bexpr e1;
      f_bexpr e2;
      f_bexpr e3
  | BEXPR_coerce (e,t) ->
      f_bexpr e;
      f_btype t
  | BEXPR_tuple_tail e -> f_bexpr e
  | BEXPR_tuple_head e -> f_bexpr e
  | BEXPR_tuple_cons (eh,et) -> f_bexpr eh; f_bexpr et
  | BEXPR_aprj (ix,d,c) -> f_bexpr ix; f_btype d; f_btype c
  | BEXPR_prj (n,d,c) -> f_btype d; f_btype c
  | BEXPR_inj (n,d,c) -> f_btype d; f_btype c
  | BEXPR_funprod e -> f_bexpr e
  | BEXPR_funsum e -> f_bexpr e
  | BEXPR_lrangle e -> f_bexpr e
  | BEXPR_lrbrack e -> f_bexpr e
  | BEXPR_unit -> ()
  | BEXPR_unitptr -> ()

(* this is a self-recursing version of the above routine: the argument to this
 * routine must NOT recursively apply itself! *)
let rec iter
  ?f_bid
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ?(f_label=fun _ -> ())
  ((x,t) as e)
=
  f_bexpr e;
  f_btype t;
  let f_bexpr e = iter ?f_bid ~f_btype ~f_bexpr ~f_label e in
  flat_iter ?f_bid ~f_btype ~f_bexpr ~f_label e


let map
  ?(f_bid=fun i -> i)
  ?(f_btype=fun t -> t)
  ?(f_bexpr=fun e -> e)
  ?(f_label=fun s -> s)
  e
=
  match e with
  | BEXPR_label s,t -> BEXPR_label (f_label s),f_btype t
  | BEXPR_not e,t -> BEXPR_not (f_bexpr e), f_btype t
  | BEXPR_deref e,t -> BEXPR_deref (f_bexpr e), f_btype t
  | BEXPR_ref (i,ts),t -> BEXPR_ref (f_bid i, List.map f_btype ts), f_btype t
  | BEXPR_new e,t -> BEXPR_new (f_bexpr e), f_btype t
  | BEXPR_class_new (cl,e),t -> BEXPR_class_new (f_btype cl, f_bexpr e), f_btype t
  | BEXPR_address e,t -> BEXPR_address (f_bexpr e), f_btype t
  | BEXPR_likely e,t -> BEXPR_likely (f_bexpr e), f_btype t
  | BEXPR_unlikely e,t -> BEXPR_unlikely (f_bexpr e), f_btype t
  | BEXPR_apply (e1,e2),t -> BEXPR_apply (f_bexpr e1, f_bexpr e2), f_btype t
  | BEXPR_compose (e1,e2),t -> BEXPR_compose (f_bexpr e1, f_bexpr e2), f_btype t
  | BEXPR_apply_prim (i,ts,e2),t ->
      BEXPR_apply_prim (f_bid i, List.map f_btype ts, f_bexpr e2),f_btype t
  | BEXPR_apply_direct (i,ts,e2),t ->
      BEXPR_apply_direct (f_bid i, List.map f_btype ts, f_bexpr e2),f_btype t
  | BEXPR_apply_struct (i,ts,e2),t ->
      BEXPR_apply_struct (f_bid i, List.map f_btype ts, f_bexpr e2),f_btype t
  | BEXPR_apply_stack (i,ts,e2),t ->
      BEXPR_apply_stack (f_bid i, List.map f_btype ts, f_bexpr e2),f_btype t
  | BEXPR_tuple  es,t -> BEXPR_tuple (List.map f_bexpr es),f_btype t
  | BEXPR_record es,t ->
      BEXPR_record (List.map (fun (s,e) -> s, f_bexpr e) es),f_btype t
  | BEXPR_variant (s,e),t -> BEXPR_variant (s, f_bexpr e),f_btype t
  | BEXPR_closure (i,ts),t ->
      BEXPR_closure (f_bid i, List.map f_btype ts),f_btype t
  | BEXPR_varname (i,ts),t -> BEXPR_varname (f_bid i, List.map f_btype ts), f_btype t
  | BEXPR_case (i,t'),t -> BEXPR_case (i, f_btype t'),f_btype t
  | BEXPR_match_case (i,e),t -> BEXPR_match_case (i, f_bexpr e),f_btype t
  | BEXPR_case_arg (i,e),t -> BEXPR_case_arg (i, f_bexpr e),f_btype t
  | BEXPR_case_index e,t -> BEXPR_case_index (f_bexpr e),f_btype t
  | BEXPR_literal x,t -> BEXPR_literal x, f_btype t
  | BEXPR_expr (s,t1),t2 -> BEXPR_expr (s, f_btype t1), f_btype t2
  | BEXPR_range_check (e1,e2,e3),t ->
      BEXPR_range_check (f_bexpr e1, f_bexpr e2, f_bexpr e3), f_btype t
  | BEXPR_coerce (e,t'),t -> BEXPR_coerce (f_bexpr e, f_btype t'), f_btype t
  | BEXPR_tuple_tail e,t -> BEXPR_tuple_tail (f_bexpr e), f_btype t
  | BEXPR_tuple_head e,t -> BEXPR_tuple_head (f_bexpr e), f_btype t
  | BEXPR_tuple_cons (eh,et),t -> BEXPR_tuple_cons (f_bexpr eh, f_bexpr et), f_btype t
  | BEXPR_aprj (ix,d,c),t -> BEXPR_aprj (f_bexpr ix, f_btype d, f_btype c), f_btype t
  | BEXPR_prj (n,d,c),t -> BEXPR_prj (n, f_btype d, f_btype c), f_btype t
  | BEXPR_inj (n,d,c),t -> BEXPR_inj (n, f_btype d, f_btype c), f_btype t
  | BEXPR_funprod e, t -> BEXPR_funprod (f_bexpr e), f_btype t
  | BEXPR_funsum e, t -> BEXPR_funsum (f_bexpr e), f_btype t
  | BEXPR_lrangle e, t -> BEXPR_lrangle (f_bexpr e), f_btype t
  | BEXPR_lrbrack e, t -> BEXPR_lrbrack (f_bexpr e), f_btype t
  | BEXPR_unit,t -> BEXPR_unit, f_btype t
  | BEXPR_unitptr,t -> BEXPR_unitptr, f_btype t

(* -------------------------------------------------------------------------- *)

(** Simplify the bound expression. *)
let rec reduce e =
  let rec f_bexpr e =
    match map ~f_bexpr:f_bexpr e with
    (*
    | BEXPR_apply ((BEXPR_closure (i,ts),_),a),t ->
        BEXPR_apply_direct (i,ts,a),t
    *)
    | _,Flx_btype.BTYP_tuple [] -> bexpr_unit
    | _, Flx_btype.BTYP_pointer (Flx_btype.BTYP_tuple []) -> bexpr_unitptr
    | BEXPR_not (BEXPR_not (e,t1),t2),t3 when t1 = t2 && t2 = t3 -> e,t1 (* had better be bool! *)
    | BEXPR_apply ((BEXPR_prj (n, _,_),_),((BEXPR_tuple ls),_)),_ -> List.nth ls n
    | BEXPR_deref (BEXPR_ref (i,ts),_),t -> BEXPR_varname (i,ts),t
    | BEXPR_deref (BEXPR_address (e,t),_),_ -> (e,t)
    | BEXPR_address (BEXPR_deref (e,t),_),_ -> (e,t)
    | BEXPR_apply 
      (
       (BEXPR_compose( (_,Flx_btype.BTYP_function (_,b) as f1), f2),_),
       e
      ),t ->
        (* print_endline "Eliminating composition"; *)
        BEXPR_apply(f2,(BEXPR_apply(f1,e),b)),t
    | BEXPR_apply((BEXPR_compose _,_),_),_ -> print_endline "Bugged composition"; assert false
    | x -> x
  in f_bexpr e

