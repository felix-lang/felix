open Flx_ast
open Flx_btype

let apl2 (sr:Flx_srcref.t) (fn : string) (tup:expr_t list) =
  EXPR_apply
  (
    sr,
    (
      EXPR_name (sr,fn,[]),
      EXPR_tuple (sr,tup)
    )
  )

let strr' bsym_table sym_table counter be rs sr a =
    let mks s = EXPR_literal (sr, 
      { Flx_literal.felix_type="string"; internal_value=s; c_value= "::std::string(" ^ Flx_string.c_quote_of_string s ^ ")" } )
    in
    let intlit i = EXPR_literal (sr,
      { Flx_literal.felix_type="int"; internal_value=string_of_int i; c_value=string_of_int i } )
    in
    let apl a b = EXPR_apply (sr,(a,b)) in
    let apls a b = EXPR_apply (sr,(EXPR_name (sr,a,[]),b)) in
    let cats a b =  apl2 sr "+" [a;b] in
    let catss ls = List.fold_left (fun acc e -> apl2 sr "+" [acc;e]) (List.hd ls) (List.tl ls) in
    let prj fld a = apls fld a in
    let rprj fld seq a = EXPR_rnprj (sr,fld,seq,a) in
    let str x = apls "_strr" x in
    let repr x = apls "repr" x in
    let strf fld a = str (prj fld a) in
    let rstrf fld seq a = str (rprj fld seq a) in
    let stri fld a = str (apl (intlit fld) a) in
    let fldrep1 fld a = cats (mks (fld^"=")) (strf fld a) in
    let fldrep2 fld a = cats (mks (","^fld^"=")) (strf fld a) in
    let rfldrep1 fld seq a = cats (mks (fld^"=")) (rstrf fld seq a) in
    let rfldrep2 fld seq a = cats (mks (","^fld^"=")) (rstrf fld seq a) in
    let vrep1 ix a = (stri ix a) in
    let vrep2 ix a = cats (mks (",")) (stri ix a) in
    let qn name = `AST_name (sr,name,[]) in 
    let (_,t) as ba = be rs a in
(*
print_endline ("strr unbound arg expression= " ^ Flx_print.string_of_expr a); 
print_endline ("strr bound arg type= " ^ Flx_print.sbt bsym_table t); 
print_endline ("strr bound arg type= " ^ Flx_btype.st t); 
print_endline ("strr bound arg expression = " ^ Flx_print.sbe bsym_table ba); 
*)
    begin match Flx_btype.unfold "_strr" t with
    | BTYP_type_var _ -> 
      print_endline "Type variable?"; 
      be rs (cats (mks "typevar?:") (repr a))
 
    | BTYP_record ls ->
(*
print_endline ("Generating _strr for record type " ^ Flx_print.sbt bsym_table t);
*)
      let first = ref true in
      let ctrl_fld = ref "" in
      let seq = ref 0 in
      let e = cats (
        List.fold_left (fun acc (s,_) -> 
          if !first then ctrl_fld := s else
          if s = !ctrl_fld then incr seq else begin seq := 0; ctrl_fld := s end;
          let res = if !first then rfldrep1 s (!seq) a else rfldrep2 s (!seq) a in
          first:=false;
          cats acc res
        )
        (mks "(")
        ls
        ) (mks ")") 
      in 
      be rs e

    | BTYP_array (vl,BTYP_unitsum n) when n < 20 -> 
      let count = ref 0 in
      let e = cats (
        List.fold_left (fun acc _ -> 
          let res = if (!count) = 0 then vrep1 (!count) a else vrep2 (!count) a in
          incr count;
          cats acc res
        )
        (mks "(")
        (Flx_list.nlist n) 
        ) (mks ")") 
      in 
      be rs e

      
    | BTYP_tuple ls ->
      let count = ref 0 in
      let e = cats (
        List.fold_left (fun acc _ -> 
          let res = if (!count) = 0 then vrep1 (!count) a else vrep2 (!count) a in
          incr count;
          cats acc res
        )
        (mks "(")
        ls
        ) (mks ")") 
      in 
      be rs e

    | BTYP_unitsum 2 ->
      be rs (EXPR_cond (sr,(a,mks "false",mks "true")))

    | BTYP_unitsum n ->
      let e = catss [(mks "case "); (apls "str" (EXPR_case_index (sr,a)));
        (mks " of "); (mks (string_of_int n))] 
      in
      be rs e 

    | BTYP_sum ls ->
      let limit = rs.Flx_types.strr_limit - 1 in
      if limit = 0 then be rs (mks "...") else
      let rs = { rs with Flx_types.strr_limit = limit } in
      let urep index t =  
        match t with
        | BTYP_void ->
          mks ("case " ^ string_of_int index^" VOID")

        | BTYP_tuple _ ->
          let arg = EXPR_case_arg (sr, (index,a)) in
          let strarg = str arg in
          cats (mks ("case " ^ string_of_int index^" ")) strarg

        | _ ->
          let arg = EXPR_case_arg (sr, (index,a)) in
          let strarg = str arg in
          cats (cats (mks ("case "^ string_of_int index^" (")) strarg) (mks ")")
      in 
      let condu index t other =
        let cond = EXPR_match_case (sr, (index,a)) in
        let u = urep index t in
        EXPR_cond (sr, (cond,u,other)) 
      in 
      let e = 
        List.fold_left (fun acc (index,t) -> 
          condu index t acc 
        )
        (mks "MATCHFAILURE")
        (List.combine (Flx_list.nlist (List.length ls)) ls)
      in 
      be rs e

    | BTYP_variant ls ->
(*
print_endline ("_strr Variant type " ^ Flx_print.sbt bsym_table t);
*)
      let limit = rs.Flx_types.strr_limit - 1 in
      if limit = 0 then be rs (mks "...") else
      let rs = { rs with Flx_types.strr_limit = limit } in
      let urep cname t hashcode =  
        match t with
        | BTYP_void ->
          mks cname

        | BTYP_tuple [] ->
          mks ("#`" ^ cname)


        | BTYP_tuple _ ->
          let arg = EXPR_case_arg (sr, (hashcode,a)) in
          let strarg = str arg in
          cats (mks ("`"^ cname^" ")) strarg

        | _ ->
          let arg = EXPR_case_arg (sr, (hashcode,a)) in
          let strarg = str arg in
          cats (cats (mks ("`"^ cname^" (")) strarg) (mks ")")
      in 
      let condu cname t other =
        let hashcode = vhash (cname,t) in
        let cond = EXPR_match_case (sr, (hashcode,a)) in
        let u = urep cname t hashcode in
        EXPR_cond (sr, (cond,u,other)) 
      in 
      let e = 
        List.fold_left (fun acc (cname,t) -> 
          condu cname t acc 
        )
        (mks "MATCHFAILURE")
        ls
      in 
      be rs e

 
    | BTYP_inst (i,ts,mt) ->
(*
print_endline ("Strr on nominal type");
*)
      begin match Flx_lookup_state.hfind "lookup:_strr" sym_table i with
      | { Flx_sym.id=name; Flx_sym.vs=(vs,_); Flx_sym.symdef=Flx_types.SYMDEF_struct ls } -> 
        let first = ref true in
        let e = cats (
          List.fold_left (fun acc (s,_) -> 
            let res = if !first then fldrep1 s a else fldrep2 s a in
            first:=false;
            cats acc res
          )
          (mks (name^" {"))
          ls
          ) (mks "}") 
        in 
        be rs e
      | { Flx_sym.id=name; Flx_sym.vs=(vs,_); Flx_sym.symdef=Flx_types.SYMDEF_union ls } -> 
(*
print_endline ("Strr on union " ^ name);
*)
        let limit = rs.Flx_types.strr_limit - 1 in
        if limit = 0 then be rs (mks "...") else
        let rs = { rs with Flx_types.strr_limit = limit } in
        let urep cname t =  
          match t with
          | TYP_void _ ->
(*
print_endline ("Constant ctor " ^ cname);
*)
            let result =  mks cname in
(*
print_endline (Flx_print.string_of_expr result);
*)
            result

          | TYP_tuple _ ->
(*
print_endline ("Tuple ctor " ^ cname);
*)
            let arg = EXPR_ctor_arg (sr, (qn cname,a)) in
            let strarg = str arg in
            cats (mks (cname^" ")) strarg

          | _ ->
(*
print_endline ("Other ctor " ^ cname);
*)
            let arg = EXPR_ctor_arg (sr, (qn cname,a)) in
            let strarg = str arg in
            cats (cats (mks (cname^" (")) strarg) (mks ")")
        in 
        let condu cname t other =
          let cond = EXPR_match_ctor (sr, (qn cname,a)) in
          let u = urep cname t in
          EXPR_cond (sr, (cond,u,other)) 
        in 
        let e = 
          List.fold_left (fun acc (cname,ix,vs,d,c,gadt) -> 
(*
print_endline ("Processing for constructor " ^ cname);
*)
            condu cname d acc 
          )
          (mks "MATCHFAILURE")
          ls
        in 
        let result = 
           try be rs e 
           with exn -> print_endline ("Binding failed"); raise exn
        in
(*
print_endline ("Bound _strr!");
*)
        result

      | _ -> 
(*
       print_endline ("_strr: Cant handle1 nominal type " ^ Flx_btype.st t ^ ": using repr"); 
*)
       be rs (repr a) 
      end

    | _ ->
(*
       print_endline ("_strr: Cant handle2 structural type " ^ Flx_btype.st t ^ ": using repr"); 
*)
      be rs (repr a)
    end

let strr bsym_table sym_table counter be rs sr a =
  strr' bsym_table sym_table counter be rs sr a



