type bexpr_t =
  | BEXPR_deref of t
  | BEXPR_name of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_ref of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_literal of Flx_ast.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_btype.t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_variant of string * t
  | BEXPR_get_n of int * t (* tuple projection *)
  | BEXPR_closure of Flx_types.bid_t * Flx_btype.t list
  | BEXPR_case of int * Flx_btype.t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_btype.t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_btype.t

and t = bexpr_t * Flx_btype.t

(* -------------------------------------------------------------------------- *)

let bexpr_deref t e : t = BEXPR_deref e, t

let bexpr_name t (bid, ts) = BEXPR_name (bid, ts), t

let bexpr_ref t (bid, ts) = BEXPR_ref (bid, ts), t

let bexpr_likely t e = BEXPR_likely e, t

let bexpr_unlikely t e = BEXPR_unlikely e, t

let bexpr_address t e = BEXPR_address e, t

let bexpr_new t e = BEXPR_new e, t

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

let bexpr_expr t (s, e) = BEXPR_expr (s, e), t

let bexpr_range_check t (e1, e2, e3) = BEXPR_range_check (e1, e2, e3), t

let bexpr_coerce t (e1, e2) = BEXPR_coerce (e1, e2), t

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

  | BEXPR_deref e,BEXPR_deref e' -> cmp e e'

  | BEXPR_name (i,ts),BEXPR_name (i',ts')
  | BEXPR_ref (i,ts),BEXPR_ref (i',ts')
  | BEXPR_closure (i,ts),BEXPR_closure (i',ts') ->
     i = i' && List.fold_left2 (fun r a b -> r && a = b) true ts ts'

  (* Note any two distinct new expressions are distinct ...
   * not sure what is really needed here *)
  | BEXPR_new e1,BEXPR_new e2 -> false

  | _,BEXPR_likely e2
  | _,BEXPR_unlikely e2 -> cmp xa e2

  | BEXPR_likely e1,_
  | BEXPR_unlikely e1,_ -> cmp e1 xb

  | BEXPR_literal a,BEXPR_literal a' -> Flx_typing.cmp_literal a a'

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

  | BEXPR_match_case (i,e),BEXPR_match_case (i',e')
  | BEXPR_get_n (i,e),BEXPR_get_n (i',e') ->
    i = i' && cmp e e'

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
  ?(fi=fun _ -> ())
  ?(ft=fun _ -> ())
  ?(fe=fun _ -> ())
  ((x,t) as e) =
  match x with
  | BEXPR_deref e -> fe e
  | BEXPR_ref (i,ts) ->
      fi i;
      List.iter ft ts
  | BEXPR_likely e -> fe e
  | BEXPR_unlikely e -> fe e
  | BEXPR_address e -> fe e
  | BEXPR_new e -> fe e
  | BEXPR_apply (e1,e2) ->
      fe e1;
      fe e2
  | BEXPR_apply_prim (i,ts,e2) ->
      fi i;
      List.iter ft ts;
      fe e2
  | BEXPR_apply_direct (i,ts,e2) ->
      fi i;
      List.iter ft ts;
      fe e2
  | BEXPR_apply_struct (i,ts,e2) ->
      fi i;
      List.iter ft ts;
      fe e2
  | BEXPR_apply_stack (i,ts,e2) ->
      fi i;
      List.iter ft ts;
      fe e2
  | BEXPR_tuple es -> List.iter fe es
  | BEXPR_record es -> List.iter (fun (s,e) -> fe e) es
  | BEXPR_variant (s,e) -> fe e
  | BEXPR_get_n (i,e) -> fe e
  | BEXPR_closure (i,ts) ->
      fi i;
      List.iter ft ts
  | BEXPR_name (i,ts) ->
      fi i;
      List.iter ft ts
  | BEXPR_case (i,t') -> ft t'
  | BEXPR_match_case (i,e) -> fe e
  | BEXPR_case_arg (i,e) -> fe e
  | BEXPR_case_index e -> fe e
  | BEXPR_literal x -> ft t
  | BEXPR_expr (s,t1) -> ft t1
  | BEXPR_range_check (e1,e2,e3) ->
      fe e1;
      fe e2;
      fe e3
  | BEXPR_coerce (e,t) ->
      fe e;
      ft t

(* this is a self-recursing version of the above routine: the argument to this
 * routine must NOT recursively apply itself! *)
let rec iter ?fi ?(ft=fun _ -> ()) ?(fe=fun _ -> ()) ((x,t) as e) =
  fe e;
  ft t;
  let fe e = iter ?fi ~ft ~fe e in
  flat_iter ?fi ~ft ~fe e


let map ?(fi=fun i -> i) ?(ft=fun t -> t) ?(fe= fun e -> e) e =
  match e with
  | BEXPR_deref e,t -> BEXPR_deref (fe e), ft t
  | BEXPR_ref (i,ts),t -> BEXPR_ref (fi i, List.map ft ts), ft t
  | BEXPR_new e,t -> BEXPR_new (fe e), ft t
  | BEXPR_address e,t -> BEXPR_address (fe e), ft t
  | BEXPR_likely e,t -> BEXPR_likely (fe e), ft t
  | BEXPR_unlikely e,t -> BEXPR_unlikely (fe e), ft t
  | BEXPR_apply (e1,e2),t -> BEXPR_apply (fe e1, fe e2), ft t
  | BEXPR_apply_prim (i,ts,e2),t ->
      BEXPR_apply_prim (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_direct (i,ts,e2),t ->
      BEXPR_apply_direct (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_struct (i,ts,e2),t ->
      BEXPR_apply_struct (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_apply_stack (i,ts,e2),t ->
      BEXPR_apply_stack (fi i, List.map ft ts, fe e2),ft t
  | BEXPR_tuple  es,t -> BEXPR_tuple (List.map fe es),ft t
  | BEXPR_record es,t ->
      BEXPR_record (List.map (fun (s,e) -> s, fe e) es),ft t
  | BEXPR_variant (s,e),t -> BEXPR_variant (s, fe e),ft t
  | BEXPR_get_n (i,e),t -> BEXPR_get_n (i, fe e),ft t
  | BEXPR_closure (i,ts),t -> BEXPR_closure (fi i, List.map ft ts),ft t
  | BEXPR_name (i,ts),t -> BEXPR_name (fi i, List.map ft ts), ft t
  | BEXPR_case (i,t'),t -> BEXPR_case (i, ft t'),ft t
  | BEXPR_match_case (i,e),t -> BEXPR_match_case (i, fe e),ft t
  | BEXPR_case_arg (i,e),t -> BEXPR_case_arg (i, fe e),ft t
  | BEXPR_case_index e,t -> BEXPR_case_index (fe e),ft t
  | BEXPR_literal x,t -> BEXPR_literal x, ft t
  | BEXPR_expr (s,t1),t2 -> BEXPR_expr (s, ft t1), ft t2
  | BEXPR_range_check (e1,e2,e3),t ->
      BEXPR_range_check (fe e1,fe e2, fe e3), ft t
  | BEXPR_coerce (e,t'),t -> BEXPR_coerce (fe e, ft t'), ft t

(* -------------------------------------------------------------------------- *)

(** Simplify the bound expression. *)
let reduce e =
  let rec fe e =
    match map ~fe e with
    | BEXPR_apply ((BEXPR_closure (i,ts),_),a),t ->
        BEXPR_apply_direct (i,ts,a),t
    | BEXPR_get_n (n,((BEXPR_tuple ls),_)),_ -> List.nth ls n
    | BEXPR_deref (BEXPR_ref (i,ts),_),t -> BEXPR_name (i,ts),t
    | BEXPR_deref (BEXPR_address (e,t),_),_ -> (e,t)
    | BEXPR_address (BEXPR_deref (e,t),_),_ -> (e,t)
    | x -> x
  in fe e

(* -------------------------------------------------------------------------- *)

let rec print_bexpr f = function
  | BEXPR_deref e ->
      Flx_format.print_variant1 f "BEXPR_deref" print e
  | BEXPR_name (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_name"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
  | BEXPR_ref (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_ref"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
  | BEXPR_likely e ->
      Flx_format.print_variant1 f "BEXPR_likely" print e
  | BEXPR_unlikely e ->
      Flx_format.print_variant1 f "BEXPR_unlikely" print e
  | BEXPR_address e ->
      Flx_format.print_variant1 f "BEXPR_address" print e
  | BEXPR_new e ->
      Flx_format.print_variant1 f "BEXPR_new" print e
  | BEXPR_literal l ->
      Flx_format.print_variant1 f "BEXPR_literal"
        Flx_ast.print_literal l
  | BEXPR_apply (e1, e2) ->
      Flx_format.print_variant2 f "BEXPR_apply" print e1 print e2
  | BEXPR_apply_prim (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_prim"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
        print e
  | BEXPR_apply_direct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_direct"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
        print e
  | BEXPR_apply_stack (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_stack"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
        print e
  | BEXPR_apply_struct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_struct"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
        print e
  | BEXPR_tuple es ->
      Flx_format.print_variant1 f "BEXPR_tuple" (Flx_list.print print) es
  | BEXPR_record es ->
      Flx_format.print_variant1 f "BEXPR_record"
        (Flx_list.print begin fun f (s, e) ->
          Flx_format.print_tuple2 f Flx_format.print_string s print e
        end)
        es
  | BEXPR_variant (s, e) ->
      Flx_format.print_variant2 f "BEXPR_variant"
        Flx_format.print_string s
        print e
  | BEXPR_get_n (i, e) ->
      Flx_format.print_variant2 f "BEXPR_get_n"
        Format.pp_print_int i
        print e
  | BEXPR_closure (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_closure"
        Flx_types.print_bid bid
        (Flx_list.print Flx_btype.print) ts
  | BEXPR_case (i, t) ->
      Flx_format.print_variant2 f "BEXPR_match_case"
        Format.pp_print_int i
        Flx_btype.print t
  | BEXPR_match_case (i, e) ->
      Flx_format.print_variant2 f "BEXPR_match_case"
        Format.pp_print_int i
        print e
  | BEXPR_case_arg (i, e) ->
      Flx_format.print_variant2 f "BEXPR_case_arg"
        Format.pp_print_int i
        print e
  | BEXPR_case_index e ->
      Flx_format.print_variant1 f "BEXPR_case_index" print e
  | BEXPR_expr (s, t) ->
      Flx_format.print_variant2 f "BEXPR_closure"
        Flx_format.print_string s
        Flx_btype.print t
  | BEXPR_range_check (e1, e2, e3) ->
      Flx_format.print_variant3 f "BEXPR_range_check"
        print e1
        print e2
        print e3
  | BEXPR_coerce (e, t) ->
      Flx_format.print_variant2 f "BEXPR_coerce"
        print e
        Flx_btype.print t

and print f (e, t) =
  Flx_format.print_tuple2 f
    print_bexpr e
    Flx_btype.print t
