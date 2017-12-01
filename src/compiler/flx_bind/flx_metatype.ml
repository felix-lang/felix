(** Meta typing. *)

open Flx_print
open Flx_types
open Flx_btype
open Flx_exceptions
open Flx_kind


(* NOTE: This function uses the bound type and the UNBOUND
symbol table entry for nominal types together to calculate
the meta type of a type term
*)

let rec metatype sym_table bsym_table rs sr term : kind =
  (*
  print_endline ("Find Metatype  of: " ^
    string_of_btypecode bsym_table term);
  *)
  let t = metatype' sym_table bsym_table rs sr term in
  (*
  print_endline ("Metatype  of: " ^ string_of_btypecode bsym_table term ^
    " is " ^ sbt bsym_table t);
  print_endline "Done";
  *)
  t

and metatype' sym_table bsym_table rs sr term : kind =
  let st t = sbt bsym_table t in
  let mt t = metatype' sym_table bsym_table rs sr t in
(*
print_endline ("Metatyping term " ^ st term);
*)
  match term with
  | BTYP_hole -> assert false
  | BTYP_type_function (a,b,c) ->
    let ps = List.map snd a in
    let argt =
      match ps with
      | [x] -> x
      | _ -> kind_tuple ps
    in
      let rt = metatype sym_table bsym_table rs sr c in
      if b<>rt
      then
        clierrx "[flx_bind/flx_metatype.ml:39: E239] " sr
        (
          "In abstraction\n" ^
          st term ^
          "\nFunction body metatype \n"^
          sk rt^
          "\ndoesn't agree with declared kind \n" ^
          sk b
        )
      else kind_function (argt,b)

  | BTYP_type_tuple ts ->
    kind_tuple (List.map mt ts)

  (* this is a hack, but should be ok for now: the type of a map
     of a type function over a tuple is a tuple of the mapped types,
     which is an ordinary type.
  *)
  | BTYP_type_map (_,_) -> kind_type

  | BTYP_type_apply (a,b) ->
    begin
      let ta = mt a
      and tb = mt b
      in match ta with
      | KIND_function (x,y) ->
        if x = tb then y
        else
          clierrx "[flx_bind/flx_metatype.ml:61: E240] " sr (
            "Metatype error: type term " ^
             st term ^
            "\nfunction argument wrong metatype, expected:\n" ^
            sk  x ^
            "\nbut got:\n" ^
            sk tb
          )

      | _ -> clierrx "[flx_bind/flx_metatype.ml:70: E241] " sr
        (
          "Metatype error: function required for LHS of application:\n"^
          sbt bsym_table term ^
          ", got metatype:\n" ^
          sk ta
        )
    end
  | BTYP_type_var (i,mt) ->
(*
    print_endline ("Type variable " ^ string_of_int i^ " has encoded meta type " ^
      sbt bsym_table mt);
    (
      try
        let symdef = Flx_sym_table.find sym_table i in begin match symdef with
        | {symdef=SYMDEF_typevar mt} ->
            print_endline ("Table shows metatype is " ^ string_of_typecode mt);
        | _ -> print_endline "Type variable isn't a type variable?"
        end
      with Not_found ->
        print_endline "Cannot find type variable in symbol table"
    );
*)
    mt

  | BTYP_vinst (index,ts,k) -> k
  | BTYP_inst (index,ts,k) -> k
(*
      let sym =
        try Flx_sym_table.find sym_table index with Not_found ->
          failwith ("[metatype'] can't find type instance index " ^
            string_of_bid index)
      in

      (* this is hacked: we should really bind the types and take the metatype
       * of them but we don't have access to the bind type routine due to module
       * factoring. we could pass in the bind-type routine as an argument.
       * yuck.  *)
      begin match sym.Flx_sym.symdef with
      | SYMDEF_nonconst_ctor (_,ut,_,_,argt) -> kind_type
(* Wrong, constructors are functions which are kind_type, they're not TYPE->TYPE maps
          btyp_function (kind_type,kind_type)
*)
      | SYMDEF_const_ctor (_,t,_,_) -> kind_type
      | SYMDEF_abs _ -> kind_type
      | SYMDEF_newtype _ -> kind_type 
(*
          clierrx "[flx_bind/flx_metatype.ml:114: E242] " sr ("Unexpected argument to metatype, newtype : " ^
            sbt bsym_table term)
*)
      | SYMDEF_struct _ 
      | SYMDEF_cstruct _ -> kind_type
(*
          clierrx "[flx_bind/flx_metatype.ml:120: E243] " sr ("Unexpected argument to metatype, struct or cstruct : " ^
            sbt bsym_table term)
*)
      | SYMDEF_type_alias t ->
        begin try
          let bsym = Flx_bsym_table.find bsym_table index in
          let bbdcl = Flx_bsym.bbdcl bsym in
          match bbdcl with
          | Flx_bbdcl.BBDCL_nominal_type_alias (bvs, alias) 
          | Flx_bbdcl.BBDCL_structural_type_alias (bvs, alias) -> 
            let salias = Flx_btype_subst.tsubst sr bvs ts alias in
            metatype' sym_table bsym_table rs sr salias
          | _ -> 
            clierrx "[flx_bind/flx_metatype.ml:124: E244] " sr ("Flx_meta-type.Expected type alias in bsym table, got " ^
            Flx_print.string_of_bbdcl bsym_table bbdcl index);
        with _ -> 
          Flx_guess_meta_type.guess_metatype sym.Flx_sym.sr t
       end 
      | _ ->
          clierrx "[flx_bind/flx_metatype.ml:128: E245] " sr ("Unexpected argument to metatype: " ^
            sbt bsym_table term)
      end
*)

  (* Ordinary type expressions *)
  | BTYP_typeof _
  | BTYP_cfunction _
  | BTYP_function _
  | BTYP_effector _
  | BTYP_cltpointer _
  | BTYP_cltrref _
  | BTYP_cltwref _
  | BTYP_pointer _
  | BTYP_rref _
  | BTYP_wref _
  | BTYP_variant _
  | BTYP_polyvariant _
  | BTYP_record _
  | BTYP_sum _
  | BTYP_array _
  | BTYP_tuple _
  | BTYP_void
  | BTYP_rev _
  | BTYP_uniq _

  | BTYP_intersect _
  | BTYP_union _
  | BTYP_polyrecord (_, _)
  | BTYP_type_match (_, _)
  | BTYP_subtype_match (_, _)
  | BTYP_tuple_cons (_, _)
  | BTYP_tuple_snoc (_, _)
  | BTYP_unitsum _ -> kind_type
  | BTYP_fix (i,mt) -> 
(*
    let si i = string_of_int i in
    let catmap sep f ls = String.concat sep (List.map f ls) in
    print_endline ("Meta type of fix point " ^ si i ^ ", type-alias-fixlist = ");
    print_endline (catmap ","  (fun (idx,level) -> "Index=" ^ si idx ^ " level = " ^ si level) (rs.type_alias_fixlist));
    print_endline "Idx fixlist is:";
    print_endline (catmap ","  (fun (idx) -> "Index=" ^ si idx ) (rs.idx_fixlist));
    print_endline "Expr fixlist is:";
    print_endline (catmap ","  (fun (e,d) -> "Expr=" ^ string_of_expr e ^ " depth=" ^ si d ) (rs.expr_fixlist));
    print_endline "As fixlist is:";
    print_endline (catmap ","  (fun (s,level) -> "as variable =" ^ s ^ " level = " ^ si level) (rs.as_fixlist));
*)
    mt
(*
  | _ ->
(*
    print_endline ("Questionable meta typing of term: " ^
      sbt bsym_table term);
*)
    kind_type (* THIS ISN'T RIGHT *)

*)
  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    -> kind_type (* WRONG but lets see what happens ! *)

  | BTYP_label
  | BTYP_none
    ->
    clierrx "[flx_bind/flx_metatype.ml:180: E246] " sr ("No meta type for type-like term " ^ sbt bsym_table term)



