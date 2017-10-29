open Flx_ast
open Flx_print
open Flx_typing
open List
open Flx_exceptions

(* These routine just check that the shape of a list
  of patterns match the pattern class indicated by their names.

  These routines are used for class based desugaring.
  Note that type correctness isn't checked, since
  type binding isn't done yet.
*)

let rec check_match_literal pats =
  let rec check pat =
    match pat with
    | PAT_any _
    | PAT_setform_any _
    | PAT_literal _
    | PAT_range _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check pat
    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:30: E344] " sr "Literal pattern expected"
  in
  List.iter check pats

let rec check_match_range pats =
  let rec check pat =
    match pat with
    | PAT_any _
    | PAT_setform_any _
    | PAT_literal _
    | PAT_range _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:50: E345] " sr "Literal range pattern expected"
  in
  List.iter check pats


and check_match_record pats =
  let rec check pat =
    match pat with
    | PAT_record _
    | PAT_polyrecord _
    | PAT_any _
    | PAT_setform_any _
    | PAT_name _ -> ()

    | PAT_as (_,pat,_)
    | PAT_coercion (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:71: E346] " sr "Record pattern expected"
  in
  List.iter check pats

and check_match_tuple n pats =
  let rec check n pat =
    match pat with
    | PAT_any _
    | PAT_setform_any _
    | PAT_name _ -> ()

    | PAT_tuple (sr,pats) ->
        if List.length pats = n then () else
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:85: E347] " sr "Tuple pattern wrong length"

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check n pat

    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:94: E348] " sr "Tuple pattern expected"
  in

  List.iter (check n) pats;

  let rec match_split pat =
    match pat with
    | PAT_any _ -> []
    | PAT_setform_any _ -> []
    | PAT_name _ -> []
    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_) 
    | PAT_when (_,pat,_) -> match_split pat 
    | PAT_tuple (_,ps) -> ps
    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:111: E349] " sr "Tuple pattern expected"
  in
  let tpats =
    try
      Flx_list.transpose (List.filter
        (function | [] -> false | _ -> true)
        (List.map match_split pats))
    with _ ->
      failwith "Transpose failed"
  in

  List.iter begin fun pats ->
    if List.length pats = 0
    then failwith "Null list?"
    else find_match_type (List.hd pats) pats
  end tpats

and check_match_tuple_cons pats =
(* This should really do the transpose trick tuple does but I'm too lazy *)
  let rec check pat =
    match pat with
    | PAT_any _ 
    | PAT_setform_any _ 
    | PAT_name _ -> ()
    | PAT_tuple_cons (_,p1,p2) -> check p2
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_coercion (_,pat,_) -> check pat
    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:142: E350] " sr
        (
          Flx_srcref.short_string_of_src (src_of_pat pat) ^
          ": tuple cons pattern (,,) expected, got " ^ string_of_pattern pat
        )

  in
  List.iter check pats

and check_match_tuple_snoc pats =
(* This should really do the transpose trick tuple does but I'm too lazy *)
  let rec check pat =
    match pat with
    | PAT_any _ 
    | PAT_setform_any _ 
    | PAT_name _ -> ()
    | PAT_tuple_snoc (_,p1,p2) -> check p1
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_coercion (_,pat,_) -> check pat
    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:142: E350] " sr
        (
          Flx_srcref.short_string_of_src (src_of_pat pat) ^
          ": tuple snoc pattern (<,,>) expected, got " ^ string_of_pattern pat
        )

  in
  List.iter check pats


and check_match_union pats =
  let rec check pat =
    match pat with
    | PAT_any  _
    | PAT_setform_any  _
    | PAT_nonconst_ctor _
    | PAT_ho_ctor _
    | PAT_const_ctor _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:168: E351] " sr
        (
          Flx_srcref.short_string_of_src (src_of_pat pat) ^
          ": union pattern expected, got " ^ string_of_pattern pat
        )
  in
  List.iter check pats


and check_match_variant pats =
  let rec check pat =
    match pat with
    | PAT_subtype _
    | PAT_any  _
    | PAT_setform_any  _
    | PAT_nonconst_variant _
    | PAT_const_variant  _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_with (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierrx "[flx_desugar/flx_pat.ml:193: E352] " sr
        (
          Flx_srcref.short_string_of_src (src_of_pat pat) ^
          ": variant pattern expected, got " ^ string_of_pattern pat
        )
  in
  List.iter check pats

and renaming pats = ()
and check_match_alts pats = () (* SKIP for now cause I'm confused! *)

(* This routine finds the checker routine for given
   pattern. Note that 'renaming' checks nothing:
   if this kind is the head of a match list,
   the following matches will never be executed.
   [They should be checked for correctness anyhow ..
    but instead, we consider this an error temporarily
   ]
*)
and find_match_type pat =
  match pat with
  | PAT_alt _ -> assert false
  | PAT_none _ -> print_endline ("Find match type none"); assert false
  | PAT_literal _ -> check_match_literal

  (* ranges *)
  | PAT_range _ -> check_match_range

  (* other *)
  | PAT_name _ -> renaming
  | PAT_tuple (_,pats) -> check_match_tuple (List.length pats)
  | PAT_tuple_cons (_,p1,p2) -> check_match_tuple_cons 
  | PAT_tuple_snoc (_,p1,p2) -> check_match_tuple_snoc
  | PAT_any _ -> renaming
  | PAT_setform_any _ -> renaming
  | PAT_const_ctor _ -> check_match_union
  | PAT_nonconst_ctor _ -> check_match_union
  | PAT_ho_ctor _ -> check_match_union

  | PAT_subtype _
  | PAT_const_variant _ -> check_match_variant
  | PAT_nonconst_variant _ -> check_match_variant
  | PAT_record (_,_) -> check_match_record
  | PAT_polyrecord (_,_,_) -> check_match_record

  | PAT_expr _ -> assert false
  | PAT_as (_,pat,_)
  | PAT_when (_,pat,_)
  | PAT_with (_,pat,_)
  | PAT_coercion (_,pat,_) -> find_match_type pat

let rec is_irrefutable pat =
  let irf pat = is_irrefutable pat in
  match pat with
  | PAT_alt _ -> assert false
  | PAT_none _ -> assert false
  | PAT_literal _ -> false

  (* ranges *)
  | PAT_range _ -> false

  (* other *)
  | PAT_name _ -> true
  | PAT_tuple (_,pats) -> fold_left (fun acc v -> acc && irf v) true pats
  | PAT_tuple_cons (_,p1,p2) -> irf p1 && irf p2
  | PAT_tuple_snoc (_,p1,p2) -> irf p1 && irf p2
  | PAT_any _ -> true
  | PAT_setform_any _ -> true
  | PAT_const_ctor _ -> false
  | PAT_nonconst_ctor _ -> false
  | PAT_ho_ctor _ -> false
  | PAT_subtype _ -> false
  | PAT_const_variant _ -> false
  | PAT_nonconst_variant _ -> false
  | PAT_record (_,rpats) 
  | PAT_polyrecord (_,rpats,_) -> fold_left (fun acc (_,p) -> acc && irf p) true rpats

  | PAT_expr _ -> assert false
  | PAT_as (_,pat,_) -> irf pat
  | PAT_with (_,pat,_) -> irf pat
  | PAT_when (_,pat,_) -> false
  | PAT_coercion (_,pat,_) -> irf pat

let rec check_ir pats =
  match pats with
  | h1 :: PAT_setform_any _ :: [] -> ()
  | h1 :: h2 :: tail -> if is_irrefutable h1 then
    begin
      print_endline ("[flx_pat] WARNING: Irrefutable pattern " ^ string_of_pattern h1 ^ " not last in");
      print_endline (Flx_srcref.long_string_of_src (src_of_pat h1))
    end;
    check_ir (h2 :: tail)

  | h :: [] -> () 
  | [] -> ()

let validate_patterns pats =
  if List.length pats = 0 then failwith "Empty pattern list";

  let hpat = List.hd pats in
  let checker = find_match_type hpat in
  checker pats;

  check_ir pats;

  List.iter begin fun x ->
    match x with
    | PAT_none sr -> assert false
    | _ -> ()
  end (List.tl pats)



