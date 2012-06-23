type bexpr_t =
  | BEXPR_not of t
  | BEXPR_deref of t
  | BEXPR_name of Flx_types.bid_t * Flx_btype.t list
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
  | BEXPR_get_n of t * t (* tuple projection, index first then value *)
  | BEXPR_closure of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_btype.t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t
  | BEXPR_compose of t * t

and t = bexpr_t * Flx_btype.t

(* -------------------------------------------------------------------------- *)

let bexpr_deref t e : t = BEXPR_deref e, t

let bexpr_not (e,t) : t = BEXPR_not (e,t), t

let bexpr_name t (bid, ts) = BEXPR_name (bid, ts), t

let bexpr_ref t (bid, ts) = BEXPR_ref (bid, ts), t

let bexpr_likely ((_,t) as e) = BEXPR_likely e, t

let bexpr_unlikely ((_,t) as e) = BEXPR_unlikely e, t

let bexpr_address ((_,t) as e) = BEXPR_address e, (Flx_btype.btyp_pointer t)

let bexpr_new ((_,t) as e) = BEXPR_new e, (Flx_btype.btyp_pointer t)

let bexpr_class_new cl e = BEXPR_class_new (cl,e), (Flx_btype.btyp_pointer cl)

let bexpr_literal t l = BEXPR_literal l, t

let bexpr_apply t (e1, e2) = BEXPR_apply (e1, e2), t

let bexpr_apply_prim t (bid, ts, e) = BEXPR_apply_prim (bid, ts, e), t

let bexpr_apply_direct t (bid, ts, e) = BEXPR_apply_direct (bid, ts, e), t

let bexpr_apply_stack t (bid, ts, e) = BEXPR_apply_stack (bid, ts, e), t

let bexpr_apply_struct t (bid, ts, e) = BEXPR_apply_struct (bid, ts, e), t

let bexpr_tuple t es = BEXPR_tuple es, t

let bexpr_record t es = BEXPR_record es, t

let bexpr_variant t (n, e) = BEXPR_variant (n, e), t

let bexpr_get_n t (n, e) = BEXPR_get_n (n, e), t

let bexpr_closure t (bid, ts) = BEXPR_closure (bid, ts), t

let bexpr_case t (i, e) = BEXPR_case (i, e), t

let bexpr_match_case t (i, e) = BEXPR_match_case (i, e), t

let bexpr_case_arg t (i, e) = BEXPR_case_arg (i, e), t

let bexpr_case_index t e = BEXPR_case_index e, t

let bexpr_expr (s, t) = BEXPR_expr (s, t), t

let bexpr_range_check t (e1, e2, e3) = BEXPR_range_check (e1, e2, e3), t

let bexpr_coerce (e, t) = BEXPR_coerce (e, t), t

let bexpr_compose t (e1, e2) = BEXPR_compose (e1, e2), t

let bexpr_unitsum_case i j =
  let case_type = Flx_btype.btyp_unitsum j in
  bexpr_case case_type (i, case_type)

(* -------------------------------------------------------------------------- *)

(** Extract the type arguments of a bound expression. *)
let get_ts (e,_) =
  match e with
  | BEXPR_name (_, ts)
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
  | BEXPR_coerce (e,t),BEXPR_coerce (e',t') ->
    (* not really right .. *)
    cmp e e'

  | BEXPR_record ts,BEXPR_record ts' ->
    List.length ts = List.length ts' &&
    let rcmp (s,t) (s',t') = compare s s' in
    let ts = List.sort rcmp ts in
    let ts' = List.sort rcmp ts' in
    List.map fst ts = List.map fst ts' &&
    List.fold_left2 (fun r a b -> r && a = b)
      true (List.map snd ts) (List.map snd ts')

  | BEXPR_variant (s,e),BEXPR_variant (s',e') ->
    s = s' && cmp e e'

  | BEXPR_not (e),BEXPR_not (e') 
  | BEXPR_deref e,BEXPR_deref e' -> cmp e e'

  | BEXPR_name (i,ts),BEXPR_name (i',ts')
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

  | BEXPR_tuple ls,BEXPR_tuple ls' ->
     List.fold_left2 (fun r a b -> r && cmp a b) true ls ls'

  | BEXPR_case_arg (i,e),BEXPR_case_arg (i',e')
  | BEXPR_match_case (i,e),BEXPR_match_case (i',e') ->
    i = i' && cmp e e'

  | BEXPR_get_n (i,e),BEXPR_get_n (i',e') ->
    cmp i i' && cmp e e'

  | BEXPR_case_index e,BEXPR_case_index e' -> cmp e e'

  | BEXPR_case (i,t),BEXPR_case (i',t') -> i = i' && t = t'
  | BEXPR_expr (s,t),BEXPR_expr (s',t') -> s = s' && t = t'
  | BEXPR_range_check (e1,e2,e3), BEXPR_range_check (e1',e2',e3') ->
    cmp e1 e1' && cmp e2 e2' && cmp e3 e3'

  | _ -> false

(* -------------------------------------------------------------------------- *)

(* this routine applies arguments HOFs to SUB components only, not to the actual
 * argument. It isn't recursive, so the argument HOF can be. *)
let flat_iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ((x,t) as e) =
  match x with
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
  | BEXPR_get_n (i,e) -> f_bexpr i; f_bexpr e
  | BEXPR_closure (i,ts) ->
      f_bid i;
      List.iter f_btype ts
  | BEXPR_name (i,ts) ->
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

(* this is a self-recursing version of the above routine: the argument to this
 * routine must NOT recursively apply itself! *)
let rec iter
  ?f_bid
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ((x,t) as e)
=
  f_bexpr e;
  f_btype t;
  let f_bexpr e = iter ?f_bid ~f_btype ~f_bexpr e in
  flat_iter ?f_bid ~f_btype ~f_bexpr e


let map
  ?(f_bid=fun i -> i)
  ?(f_btype=fun t -> t)
  ?(f_bexpr=fun e -> e)
  e
=
  match e with
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
  | BEXPR_get_n (i,e),t -> BEXPR_get_n (f_bexpr i, f_bexpr e),f_btype t
  | BEXPR_closure (i,ts),t ->
      BEXPR_closure (f_bid i, List.map f_btype ts),f_btype t
  | BEXPR_name (i,ts),t -> BEXPR_name (f_bid i, List.map f_btype ts), f_btype t
  | BEXPR_case (i,t'),t -> BEXPR_case (i, f_btype t'),f_btype t
  | BEXPR_match_case (i,e),t -> BEXPR_match_case (i, f_bexpr e),f_btype t
  | BEXPR_case_arg (i,e),t -> BEXPR_case_arg (i, f_bexpr e),f_btype t
  | BEXPR_case_index e,t -> BEXPR_case_index (f_bexpr e),f_btype t
  | BEXPR_literal x,t -> BEXPR_literal x, f_btype t
  | BEXPR_expr (s,t1),t2 -> BEXPR_expr (s, f_btype t1), f_btype t2
  | BEXPR_range_check (e1,e2,e3),t ->
      BEXPR_range_check (f_bexpr e1, f_bexpr e2, f_bexpr e3), f_btype t
  | BEXPR_coerce (e,t'),t -> BEXPR_coerce (f_bexpr e, f_btype t'), f_btype t

(* -------------------------------------------------------------------------- *)

(** Simplify the bound expression. *)
let rec reduce e =
  let rec f_bexpr e =
    match map ~f_bexpr:f_bexpr e with
    (*
    | BEXPR_apply ((BEXPR_closure (i,ts),_),a),t ->
        BEXPR_apply_direct (i,ts,a),t
    *)
    | BEXPR_not (BEXPR_not (e,t1),t2),t3 when t1 = t2 && t2 = t3 -> e,t1 (* had better be bool! *)
    | BEXPR_get_n ((BEXPR_case (n, _),_),((BEXPR_tuple ls),_)),_ -> List.nth ls n
    | BEXPR_deref (BEXPR_ref (i,ts),_),t -> BEXPR_name (i,ts),t
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

