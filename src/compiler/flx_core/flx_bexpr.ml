type bexpr_t =
  | BEXPR_deref of t
  | BEXPR_name of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_ref of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_likely of t
  | BEXPR_unlikely of t
  | BEXPR_address of t
  | BEXPR_new of t
  | BEXPR_literal of Flx_ast.literal_t
  | BEXPR_apply of t * t
  | BEXPR_apply_prim of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_direct of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_stack of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_apply_struct of Flx_types.bid_t * Flx_types.btypecode_t list * t
  | BEXPR_tuple of t list
  | BEXPR_record of (string * t) list
  | BEXPR_variant of string * t
  | BEXPR_get_n of int * t (* tuple projection *)
  | BEXPR_closure of Flx_types.bid_t * Flx_types.btypecode_t list
  | BEXPR_case of int * Flx_types.btypecode_t
  | BEXPR_match_case of int * t
  | BEXPR_case_arg of int * t
  | BEXPR_case_index of t
  | BEXPR_expr of string * Flx_types.btypecode_t
  | BEXPR_range_check of t * t * t
  | BEXPR_coerce of t * Flx_types.btypecode_t

and t = bexpr_t * Flx_types.btypecode_t

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

let rec print_bexpr f = function
  | BEXPR_deref e ->
      Flx_format.print_variant1 f "BEXPR_deref" print e
  | BEXPR_name (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_name"
        Flx_types.print_bid bid
        Flx_types.print_btypes ts
  | BEXPR_ref (bid, ts) ->
      Flx_format.print_variant2 f "BEXPR_ref"
        Flx_types.print_bid bid
        Flx_types.print_btypes ts
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
        Flx_types.print_btypes ts
        print e
  | BEXPR_apply_direct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_direct"
        Flx_types.print_bid bid
        Flx_types.print_btypes ts
        print e
  | BEXPR_apply_stack (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_stack"
        Flx_types.print_bid bid
        Flx_types.print_btypes ts
        print e
  | BEXPR_apply_struct (bid, ts, e) ->
      Flx_format.print_variant3 f "BEXPR_apply_struct"
        Flx_types.print_bid bid
        Flx_types.print_btypes ts
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
        Flx_types.print_btypes ts
  | BEXPR_case (i, t) ->
      Flx_format.print_variant2 f "BEXPR_match_case"
        Format.pp_print_int i
        Flx_types.print_btype t
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
        Flx_types.print_btype t
  | BEXPR_range_check (e1, e2, e3) ->
      Flx_format.print_variant3 f "BEXPR_range_check"
        print e1
        print e2
        print e3
  | BEXPR_coerce (e, t) ->
      Flx_format.print_variant2 f "BEXPR_coerce"
        print e
        Flx_types.print_btype t

and print f (e, t) =
  Flx_format.print_tuple2 f
    print_bexpr e
    Flx_types.print_btype t
