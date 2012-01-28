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

let rec check_match_int pats =
  let rec check pat =
    match pat with
    | PAT_any _
    | PAT_int _
    | PAT_int_range _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr "Integer pattern expected"
  in
  List.iter check pats

and check_match_string pats =
  let rec check pat =
    match pat with
    | PAT_any _
    | PAT_string _
    | PAT_string_range _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr "String pattern expected"
  in
  List.iter check pats

and check_match_float pats =
  let rec check pat =
    match pat with
    | PAT_none _ -> assert false

    | PAT_nan _
    | PAT_any _
    | PAT_float_range _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr "Float pattern expected"
  in
  List.iter check pats

and check_match_record pats =
  let rec check pat =
    match pat with
    | PAT_record _
    | PAT_any _
    | PAT_name _ -> ()

    | PAT_as (_,pat,_)
    | PAT_coercion (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr "Record pattern expected"
  in
  List.iter check pats

and check_match_tuple n pats =
  let rec check n pat =
    match pat with
    | PAT_any _
    | PAT_name _ -> ()

    | PAT_tuple (sr,pats) ->
        if List.length pats = n then () else
        let sr = src_of_pat pat in
        clierr sr "Tuple pattern wrong length"

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> check n pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr "Tuple pattern expected"
  in

  List.iter (check n) pats;

  let rec match_split pat =
    match pat with
    | PAT_any _ -> []
    | PAT_name _ -> []
    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> match_split pat
    | PAT_tuple (_,ps) -> ps
    | _ ->
        let sr = src_of_pat pat in
        clierr sr "Tuple pattern expected"
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

and check_match_union pats =
  let rec check pat =
    match pat with
    | PAT_any  _
    | PAT_nonconst_ctor _
    | PAT_const_ctor _
    | PAT_name _ -> ()

    | PAT_coercion (_,pat,_)
    | PAT_as (_,pat,_)
    | PAT_when (_,pat,_) -> check pat

    | _ ->
        let sr = src_of_pat pat in
        clierr sr
        (
          Flx_srcref.short_string_of_src (src_of_pat pat) ^
          ": union pattern expected, got " ^ string_of_pattern pat
        )
  in
  List.iter check pats

and renaming pats = ()

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
  | PAT_none _ -> assert false
  | PAT_nan _ -> check_match_float
  | PAT_int _ -> check_match_int
  | PAT_string _ -> check_match_string

  (* ranges *)
  | PAT_int_range _ -> check_match_int
  | PAT_string_range _ -> check_match_string
  | PAT_float_range _ -> check_match_float

  (* other *)
  | PAT_name _ -> renaming
  | PAT_tuple (_,pats) -> check_match_tuple (List.length pats)
  | PAT_any _ -> renaming
  | PAT_const_ctor _ -> check_match_union
  | PAT_nonconst_ctor _ -> check_match_union
  | PAT_record (_,_) -> check_match_record

  | PAT_expr _ -> assert false
  | PAT_as (_,pat,_)
  | PAT_when (_,pat,_)
  | PAT_coercion (_,pat,_) -> find_match_type pat

(* This routine is used to check all but the last
   pattern match isn't a match all
*)

let rec is_universal pat =
  match pat with
  | PAT_any _
  | PAT_name (_,_)
  | PAT_float_range (_, Float_minus_inf, Float_inf) -> true

  | PAT_as (_,pat,_) -> is_universal pat
  | PAT_coercion (_,pat,_) -> is_universal pat
  | PAT_tuple (_,ps) -> fold_left (fun a p -> a && is_universal p) true ps

  | _ -> false

let rec check_terminal pat =
  match pat with
  | PAT_any sr ->
      failwith
      (
        "'Any' pattern '_' must be last in match in " ^
        Flx_srcref.short_string_of_src sr
      )

  | PAT_name (sr,x) ->
      failwith
      (
        "'Name' pattern '"^x^"' must be last in match in " ^
        Flx_srcref.short_string_of_src sr
      )

  | PAT_float_range (sr, Float_minus_inf, Float_inf) ->
      failwith
      (
        "Whole floating range must be last in match in " ^
        Flx_srcref.short_string_of_src sr
      )

  | PAT_as (_,pat,_) -> check_terminal pat
  | PAT_coercion (_,pat,_) -> check_terminal pat
  | _ -> ()

let validate_patterns pats =
  if List.length pats = 0 then failwith "Empty pattern list";

  let hpat = List.hd pats in
  let checker = find_match_type hpat in
  checker pats;

  List.iter check_terminal (List.tl (List.rev pats));

  List.iter begin fun x ->
    match x with
    | PAT_none sr -> assert false
    | PAT_nan sr ->
        failwith
        (
          "NaN test must be first in match in " ^
          Flx_srcref.short_string_of_src sr
        )
    | _ -> ()
  end (List.tl pats)
