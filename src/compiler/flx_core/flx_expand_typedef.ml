
let rec find_in_list counter t lst =
   match lst with
   | [] -> None
   | (h,i) :: tail ->
     if Flx_typeeq.type_eq (Flx_btype.st) counter h t then Some i else
     find_in_list counter t tail

let rec expand bsym_table  counter sr (t:Flx_btype.t):Flx_btype.t = 
(*
print_endline ("EXPAND " ^ Flx_print.sbt bsym_table t);
*)
  let rec aux trail level t =
(*
    print_endline ("Aux: " ^ string_of_int level ^ " t=" ^ Flx_btype.st t);
*)
    match find_in_list counter t trail with
    | Some i -> 
(*
      print_endline ("Fixpoint found at depth " ^ string_of_int level ^ " up to level " ^ string_of_int i ^
        "diff is " ^ string_of_int (i - level)
      );
*)
(*
print_endline ("Flx_expand_typedef: fixpoint, set metatype");
print_endline ("Type is " ^ Flx_print.sbt bsym_table t);
*)
      let mt = Flx_btype_kind.metatype sr t in
(*
print_endline ("KIND is " ^ Flx_kind.sk mt);
*)
      Flx_btype.btyp_fix (i - level) mt 

    | None ->
      match t with
      | Flx_btype.BTYP_inst (`Alias,index,ts,mt) ->
        begin try 
          let bsym = Flx_bsym_table.find bsym_table index in
          let bbdcl = Flx_bsym.bbdcl bsym in
          begin match bbdcl with
          | Flx_bbdcl.BBDCL_structural_type_alias (bvs, alias) 
          | Flx_bbdcl.BBDCL_nominal_type_alias (bvs, alias) ->
(*
print_endline ("Found typedef " ^ Flx_bsym.id bsym ^ "<"^string_of_int index^"> alias=" ^ Flx_btype.st alias);
print_endline ("bvs/tvs= " ^ catmap ", " (fun ((s,i), t) -> s^"<" ^ string_of_int i ^ "> <-- " ^ Flx_btype.st t) (List.combine bvs ts));
*)
            let salias = Flx_btype_subst.tsubst sr bvs ts alias in
(*
print_endline ("typedef " ^ Flx_bsym.id bsym ^ " after substitution =" ^ Flx_btype.st alias);
*)
            aux ((t,level)::trail) level salias (* NO level increment *)
          | _ ->  assert false
(*
print_endline ("Found non-typedef" ^ Flx_bsym.id bsym ^ "<" ^ string_of_int k ^ ">");
            let ts = List.map (aux trail level) ts in
            btyp_inst (k,ts) 
*)
            (* Flx_btype.map ~f_btype:(aux trail (level+1)) t  *)

          end
        with Not_found -> 
print_endline ("Flx_expand_typedef: Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
          assert false
          (* Flx_btype.map ~f_btype:(aux (trail) (level+1)) t *)
        
(*
print_endline ("Found unknown <" ^ string_of_int k ^ ">");
print_endline ("raw ts = " ^ catmap "," Flx_btype.st ts); 
            let ts = List.map (aux ((t, level+1)::trail) (level+1)) ts in
print_endline ("processed ts = " ^ catmap "," Flx_btype.st ts); 
            btyp_inst (k,ts) 
*)
        end

      (* FIXME: Here, we must handle a type function application, without
         actually beta-reducing, to allow the schema indices to be deduced
      *)
      | BTYP_type_apply (BTYP_finst (index, ks, dom, cod), arg) -> 
(*
print_endline ("Type function unification " ^ Flx_print.sbt bsym_table t);
*)
        begin try
          let bsym = Flx_bsym_table.find bsym_table index in
          let bbdcl = Flx_bsym.bbdcl bsym in
          begin match bbdcl with
          | Flx_bbdcl.BBDCL_type_function (bks, alias)  ->
            let eqns =
              let n = min (List.length ks) (List.length bks) in
              let eqns1 = List.map2 (fun (name,_,_) k -> Flx_kind.kind_var name,k) (Flx_list.list_prefix bks n) (Flx_list.list_prefix ks n) in
              let arg_k = Flx_btype_kind.metatype sr arg in 
              (dom,arg_k) :: eqns1
            in 
            let mgu = 
              try
(*
print_endline ("Trying to solve");
List.iter (fun (k1, k2) -> print_endline ("   " ^ Flx_kind.sk k1 ^ " >= " ^ Flx_kind.sk k2)) eqns;
*)
                Flx_kind.kind_unif eqns 
              with Not_found ->
               print_endline ("Flx_expand_typedef: unification failure ...");
               assert false
            in
            if List.length mgu <> List.length bks then begin
              print_endline ("Unable to deduce all " ^ string_of_int (List.length bks) ^ " kind variables");
              print_endline ("mgu = ");
              List.iter (fun (name, k) -> print_endline ("    " ^ name ^ " <- " ^ Flx_kind.sk k)) mgu;
              assert false
            end else begin
              let ksubst (knd:Flx_kind.kind):Flx_kind.kind = Flx_kind.ksubst_eqns sr mgu knd in
              let rec f_btype t = Flx_btype.map ~f_btype ~f_kind:ksubst t in
              let alias = f_btype alias in
              let t' = Flx_btype.btyp_type_apply (alias, arg) in
              aux ((t,level)::trail) level t' (* NO level increment *)
            end
          | _ -> 
            print_endline ("Flx_expand_typedef. Cannot find type function instance " ^ string_of_int index ^ " in bound symbol table");
            assert false
          end
        with _ -> assert false
        end 
      
      (* NOTE: if a type function instance is used *outside* an application,
        then ALL the kind arguments buts be given, in order to fully
        eliminate kind variables. This is necessary because there is
        no way to pass the bindings on, in case the function is later
        used in am application. In that case, we can still do unification only
        for error checking, but in fact it is only a kind equality check.
      *)
      | BTYP_finst (index,ks,dom,cod) -> 
        begin try
          let bsym = Flx_bsym_table.find bsym_table index in
          let bbdcl = Flx_bsym.bbdcl bsym in
          begin match bbdcl with
          | Flx_bbdcl.BBDCL_type_function (bks, alias)  ->
(*
        print_endline ("EXPANDING FUNCTION "^bsym.id^" INSTANCE");
*)
            let ksubst (knd:Flx_kind.kind):Flx_kind.kind = Flx_kind.ksubst sr bks ks knd in
            let rec f_btype t = Flx_btype.map ~f_btype ~f_kind:ksubst t in
            let alias = f_btype alias in
(*
print_endline ("Flx_expand_typedef: expanded type function with kind subs  =\n **** " ^ Flx_print.sbt bsym_table t);
*)
            begin match alias with
            | BTYP_type_function _ ->
              aux ((t,level)::trail) level alias (* NO level increment *)
            | _ -> assert false
            end
          | _ -> assert false
          end
        with Not_found -> assert false
        end
      | _ -> Flx_btype.map ~f_btype:(aux (trail) (level+1)) t
  in
(*
  print_endline ("Top level expansion of " ^ Flx_btype.st t);
*)
  let r = aux [] 0 t in
(*
  if not (Flx_btype.complete_type r) then
    failwith ("Flx_expand_typedef.expand produced incomplete type! " ^ Flx_btype.st r)
  else begin
(*
    print_endline ("Expanded to " ^ Flx_btype.st r);
*)
*)
    r
(*
  end
*) 
