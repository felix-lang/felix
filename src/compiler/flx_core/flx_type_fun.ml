open Flx_util
open Flx_types
open Flx_btype
open Flx_mtypes2

open Flx_print
open Flx_typing
open Flx_unify
open Flx_maps
open Flx_btype_subst
open Flx_kind

let adjust bsym_table t = Flx_btype_rec.adjust_fixpoint t

let rec type_apply br beta_reduce' calltag counter bsym_table sr depth (termlist: (Flx_btype.t * int) list) t f arg = 
  match f with 
  | BTYP_finst (index, ks, dom, cod) ->
    begin try
      let bsym = Flx_bsym_table.find bsym_table index in
      let bbdcl = Flx_bsym.bbdcl bsym in
      match bbdcl with
      | Flx_bbdcl.BBDCL_type_function (bks, alias)  ->
        let f =
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
            alias
          end
        in 
        type_apply br beta_reduce' calltag counter bsym_table sr depth termlist t f arg
      | _ ->
        print_endline ("Expexted finst to refer to type function");
        assert false
    with exn -> 
      print_endline ("Some error " ^ Printexc.to_string exn);
      assert false
    end (* finst *)

    (* Ordinary typedef *)
    | Flx_btype.BTYP_inst (`Alias,index,ts,mt) ->
      begin try 
       let bsym = Flx_bsym_table.find bsym_table index in
       let bbdcl = Flx_bsym.bbdcl bsym in
       begin match bbdcl with
       | Flx_bbdcl.BBDCL_structural_type_alias (bvs, alias) 
       | Flx_bbdcl.BBDCL_nominal_type_alias (bvs, alias) ->
         let salias = Flx_btype_subst.tsubst sr bvs ts alias in
         type_apply br beta_reduce' calltag counter bsym_table sr depth termlist t alias arg
       | _ ->  assert false
       end
     with Not_found -> 
       print_endline ("Flx_type_function : Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
       assert false
     end

  | BTYP_type_function (ps,r,body) ->
    let params' =
      match ps with
      | [] -> []
      | [i,_] -> [i,arg]
      | _ ->
        let ts = match br arg with
        | BTYP_type_tuple ts -> ts
        | _ -> 
          print_endline ("Expected Argument  to type function to be type tuple, got " ^ Flx_print.sbt bsym_table arg);
          assert false
        in
        if List.length ps <> List.length ts
        then failwith "Wrong number of arguments to typefun"
        else List.map2 (fun (i,_) t -> i, t) ps ts
    in
    let t' = list_subst counter params' body in
    let t' = beta_reduce' calltag counter bsym_table sr (depth-2) termlist t' in
    t'

  | _ ->
    let t = btyp_type_apply (f,arg) in
    begin match f with 
    | BTYP_type_var _ -> () (* apply type variable .. delay until type variable set *)
    | _ -> print_endline ("Flx_beta: type apply nonfunction .. can't beta reduce, keep as " ^ Flx_btype.st t);
    end;
    t


