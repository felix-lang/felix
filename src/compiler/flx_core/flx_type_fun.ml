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

let rec type_apply beta_reduce' calltag counter bsym_table sr depth (termlist: (Flx_btype.t * int) list) f arg = 
  match f with 

(* TYPEFUN REFERENCE *)
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
        type_apply beta_reduce' calltag counter bsym_table sr depth termlist f arg
      | _ ->
        print_endline ("Expexted finst to refer to type function");
        assert false
    with exn -> 
      print_endline ("Some error " ^ Printexc.to_string exn);
      assert false
    end (* finst *)

(* TYPEDEF REFERENCE *)
  | Flx_btype.BTYP_inst (`Alias,index,ts,mt) ->
    begin try 
      let bsym = Flx_bsym_table.find bsym_table index in
      let bbdcl = Flx_bsym.bbdcl bsym in
      begin match bbdcl with
      | Flx_bbdcl.BBDCL_structural_type_alias (bvs, alias) 
      | Flx_bbdcl.BBDCL_nominal_type_alias (bvs, alias) ->
       let salias = Flx_btype_subst.tsubst sr bvs ts alias in
       type_apply beta_reduce' calltag counter bsym_table sr depth termlist alias arg
      | _ ->  assert false
      end
    with Not_found -> 
      print_endline ("Flx_type_function : Unable to find symbol " ^ string_of_int index ^ " in bound symbol table!");
      assert false
    end

(* TYPE LAMBDA *)
  | BTYP_type_function (ps,r,body) ->
    (* Fixpoint handling here *)
    let appl = Flx_btype.btyp_type_apply (f, arg) in
    begin match Flx_type_list_index.type_list_index counter bsym_table termlist appl with
    | Some j -> 
      let t = btyp_fix (j-depth) r in
(*
print_endline ("Flx_type_fun: installing fixpoint " ^ Flx_btype.st t);
*)
      t
    | None -> 
(* NOTE: the typefun term MUST be alpha converted here so the substitution does not lead to false captures in NESTED
typefun terms. Note, there cannot be a capture of any of the current paraneters because they're all being replaced,
and the replacement does not rescan the replacement. However the body of the type function can contain another
type term and we are substituting "under the lambda" and the replacement may contain a free variable that is
a parameter of THAT nested lambda.

Unfortunately this means the trail comparison must use alpha equivalance, not equality of the term encoding.

NOTE: alpha equiv is easy, we alph convert both terms using the SAME initial counter then 
do normal comparison .. but I think maybe the type equality routine should do this.
*)
      let f = Flx_alpha.alpha_convert counter f in
      (* I think this is wrong but it is essential in some case if the argument
         must be a type tuple, and is actually an application that produces one.
      *)
      let arg = beta_reduce' calltag counter bsym_table sr (depth+1) termlist arg in
      begin match f with 
      | BTYP_type_function (ps,r,body) ->
        let params' =
          match ps with
          | [] -> []
          | [i,_] -> [i,arg]
          | _ ->
            match  arg with
            | BTYP_type_tuple ts ->
              if List.length ps <> List.length ts
              then  begin
                print_endline ("Flx_beta:"^calltag ^": Expected "^ string_of_int (List.length ps) ^
                   " arguments to type function got " ^ Flx_print.sbt bsym_table arg);
                Flx_exceptions.clierr sr ("Flx_beta:"^calltag ^": Expected "^ string_of_int (List.length ps) ^
                   " arguments to type function got " ^ Flx_print.sbt bsym_table arg);
              end else List.map2 (fun (i,_) t -> i, t) ps ts
            | _ -> 
              print_endline ("Flx_beta:"^calltag ^": Expected Argument to type function to be type tuple, got " ^ Flx_print.sbt bsym_table arg);
              Flx_exceptions.clierr sr ("Flx_beta:"^calltag ^": Expected Argument to type function to be type tuple, got " ^ Flx_print.sbt bsym_table arg)
        in
        let t' = list_subst counter params' body in
        let t' = beta_reduce' calltag counter bsym_table sr depth ((appl,depth)::termlist) t' in
        t'
      | _ -> assert false (* alpha convert can't fail here *)
      end
    end (* type function *)

(* HIGHER ORDER FUNCTION APPLICATION *)
  | BTYP_type_apply (f',t') ->
    let origappl = btyp_type_apply (f,arg) in
    let f = type_apply beta_reduce' calltag counter bsym_table sr (depth+1) ((origappl,depth)::termlist) f' t' in
    type_apply beta_reduce' calltag counter bsym_table sr depth termlist f arg 
    
(* IRREDUCIBLE *)
  | _ -> 
    let t = btyp_type_apply (f,arg) in
    begin match f with 
    | BTYP_type_var _ -> () (* apply type variable .. delay until type variable set *)
    | _ -> print_endline ("Flx_beta: type apply nonfunction .. can't beta reduce, keep as " ^ Flx_btype.st t);
    end;
    t


