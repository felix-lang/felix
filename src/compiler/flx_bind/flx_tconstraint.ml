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


let build_constraint_element counter bsym_table bt sr i p1 =
(*
print_endline ("Build type constraints for type variable " ^string_of_int i ^": " ^ str_of_kindcode p1);
*)
  match p1 with
  | KND_generic (* treated as ordinary type variable here *)
  | KND_type
  | KND_linear
  | KND_borrowed
  | KND_unitsum (* well this is wrong, it IS a constraint! *) 
  | KND_compactlinear  
  | KND_view
  | KND_function _ 
  | KND_tuple _ -> bbool true

  | KND_tpattern p1 -> 
  begin
  (* special case, no constraint, represent by just 'true' (unit type) *)
  match p1 with
(*
  | `TYP_generic _ -> (* print_endline ("constraint generic .. "); *) btyp_tuple []
*)
(*
  | `TYP_tuple _
*)
  | `TYP_patany _
(*
  | `TYP_type
*)
  | `TYP_function _ -> bbool true
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
  let un = bbool true in (* the 'true' value of the type system *)
  let elt = btyp_type_var (i, Flx_kind.kind_type) in
  let p1 = bt p1 in
  let rec fe t = match t with
  | BTYP_type_set ls
  | BTYP_type_set_union ls ->
     uniq_list (concat (map fe ls))
  | BTYP_inst (`Alias,index,ts,mt) ->
    begin try 
       let bsym = Flx_bsym_table.find bsym_table index in
       let bbdcl = Flx_bsym.bbdcl bsym in
       begin match bbdcl with
       | Flx_bbdcl.BBDCL_type_alias (bvs, alias) ->
         let alias = Flx_btype_subst.tsubst sr bvs ts alias in
         let alias = Flx_beta.beta_reduce "Tconstraint" counter bsym_table bsym.Flx_bsym.sr alias in
         fe alias
       | _ ->  assert false
       end
     with Not_found -> 
       print_endline ("Flx_tconstraint: Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
       assert false
    end
  | t -> [t]
  in
  let tyset ls =
(*
print_endline ("Generating type match for typeset " ^ Flx_util.catmap ", " Flx_btype.st ls);
*)
    let e = BidSet.empty in
    let un = bbool true in
    let lss = rev_map (fun t -> {pattern=t; pattern_vars=e; assignments=[]},un) ls in
    let fresh = fresh_bid counter in
    let dflt =
      {
        pattern = btyp_type_var (fresh, Flx_kind.kind_type);
        pattern_vars = BidSet.singleton fresh;
        assignments=[]
      },
      bbool false
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
  let type_constraints =
    map (fun (s,i,tp) ->
      let tp = build_constraint_element counter bsym_table bt sr i tp in
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
  (* let type_constraints =List.map (fun t -> Flx_beta.beta_reduce "build type constraints" counter bsym_table sr t) type_constraints in *)
  let tc = List.fold_left (fun acc t -> 
    match t with 
    | BTYP_tuple [] -> assert false; acc 
    | _ -> 
      (* let traint = btyp_typeop "_type_to_staticbool" t Flx_kind.KIND_bool in *)
      let traint = t in
      btyp_typeop "_staticbool_and" (btyp_type_tuple [acc; traint]) Flx_kind.kind_bool
   )
   (bbool true)
   type_constraints
  in
(*
  print_endline ("Flx_tconstraint: `"^name^"`  type constraint = " ^ sbt bsym_table tc);
*)
  tc

