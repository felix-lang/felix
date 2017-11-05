open Flx_ast
open Flx_types
open Flx_btype
open Flx_set
open Flx_mtypes2
open Flx_tpat
open Flx_exceptions
open List
open Flx_print
open Flx_list
open Flx_bid

let debug = false

(* A type constraint written in a vs list is a simplification.
   The form

   v:p

   is short for

   typematch v with | p -> 1 | _ -> 0 endmatch

   BUT ONLY IF IT ISN'T INSTEAD AN ACTUAL METATYPE SPECIFICATION!
   (in which case it is constraint that v be a member of the
   metatype p, without other constraints!)
*)


let build_constraint_element counter bt sr i p1 =
(*
print_endline ("Build type constraints for type variable " ^string_of_int i ^": " ^ str_of_kindcode p1);
*)
  match p1 with
  | KND_generic (* treated as ordinary type variable here *)
  | KND_type
  | KND_function _ 
  | KND_tuple _ -> btyp_tuple []
  | KND_tpattern p1 ->
  begin
  (* special case, no constraint, represent by just 'true' (unit type) *)
  match p1 with
(*
  | TYP_generic _ -> (* print_endline ("constraint generic .. "); *) btyp_tuple []
*)
(*
  | TYP_tuple _
*)
  | TYP_patany _
(*
  | TYP_type
*)
  | TYP_function _ -> btyp_tuple []
  | _ ->

  (* more general cases *)
  (*
  print_endline ("Build constraint " ^ string_of_typecode p1);
  *)
  let p1,explicit_vars1,any_vars1, as_vars1, eqns1 = type_of_tpattern counter p1 in

  (* check the pattern doesn't introduce any named variables *)
  (* later we may allow them as additional 'vs' variables .. but
    it is tricky because they'd have to be introduced 'in scope':
  *)
  (*
  if eqns1 <> [] then clierrx "[flx_bind/flx_tconstraint.ml:46: E260] " sr
    "Type variable constraint may not have 'as' terms"
  ;
  if explicit_vars1 <> [] then clierrx "[flx_bind/flx_tconstraint.ml:49: E261] " sr
    "Type variable constraint may not have named pattern variables"
  ;
  *)
  let varset1 =
    fold_left (fun s i -> BidSet.add i s)
    BidSet.empty any_vars1
  in
    let varset1 =
    fold_left (fun s (i,_) -> BidSet.add i s)
    varset1 as_vars1
  in
  let varset1 =
    fold_left (fun s (i,_) -> BidSet.add i s)
    varset1 explicit_vars1
  in
  let un = btyp_tuple [] in (* the 'true' value of the type system *)
  let elt = btyp_type_var (i, btyp_type 0) in
  let p1 = bt p1 in
  let rec fe t = match t with
  | BTYP_type_set ls
  | BTYP_type_set_union ls ->
     uniq_list (concat (map fe ls))

  | t -> [t]
  in
  let tyset ls =
    let e = BidSet.empty in
    let un = btyp_tuple [] in
    let lss = rev_map (fun t -> {pattern=t; pattern_vars=e; assignments=[]},un) ls in
    let fresh = fresh_bid counter in
    let dflt =
      {
        pattern = btyp_type_var (fresh, btyp_type 0);
        pattern_vars = BidSet.singleton fresh;
        assignments=[]
      },
      btyp_void ()
    in
    let lss = rev (dflt :: lss) in
    btyp_type_match (elt, lss)
  in
    let tm = tyset (fe p1) in
    (* print_endline ("Bound typematch is " ^ sbt counter.sym_table tm); *)
    tm
  end
  | _ -> assert false (* unexpected kind *)

let build_type_constraints counter bsym_table bt name sr vs =
(*
print_endline ("building type constraints for " ^ name);
*)
  let type_constraints =
    map (fun (s,i,tp) ->
(*
if name = "accumulate" then print_endline ("type variable " ^ s ^ " constraint = " ^ string_of_typecode tp);
*)
      let tp = build_constraint_element counter bt sr i tp in
      (*
      if tp <> btyp_tuple [] then
        print_endline (
        " vs entry " ^ s ^ ", var " ^ si i ^
        " constraint " ^ sbt counter.sym_table tp)
      ;
      *)
      tp
    )
    vs
  in
    let tc = btyp_intersect type_constraints in
    let tc = Flx_beta.beta_reduce "build type constraints" counter bsym_table sr tc in
(*
    print_endline ("Flx_tconstraint: intersected individual type constraints = " ^ sbt bsym_table tc);
*)
    tc

