open Flx_ast
open Flx_btype

let apl2 (sri:Flx_srcref.t) (fn : string) (tup:expr_t list) =
  (** get range from first and last expressions *)
  let rsexpr a b = Flx_srcref.rsrange (src_of_expr a) (src_of_expr b) in

  (** get source range of non-empty list of expressions *)
  let rslist lst = rsexpr (List.hd lst) (Flx_list.list_last lst) in

  let sr = rslist tup in
  EXPR_apply
  (
    sr,
    (
      EXPR_name (sri,fn,[]),
      EXPR_tuple (sr,tup)
    )
  )

let strr bsym_table sym_table counter be sr a =

    let mks s = EXPR_literal (sr, 
      { Flx_literal.felix_type="string"; internal_value=s; c_value= Flx_string.c_quote_of_string s } )
    in
    let intlit i = EXPR_literal (sr,
      { Flx_literal.felix_type="int"; internal_value=string_of_int i; c_value=string_of_int i } )
    in
    let apl a b = EXPR_apply (sr,(a,EXPR_tuple (sr,[b]))) in
    let cats a b =  apl2 sr "+" [a;b] in
    let prj fld a = apl2 sr fld [a] in
    let str x = apl2 sr "_strr" [x] in
    let strf fld a = str (prj fld a) in
    let stri fld a = str (apl (intlit fld) a) in
    let fldrep1 fld a = cats (mks (fld^"=")) (strf fld a) in
    let fldrep2 fld a = cats (mks (","^fld^"=")) (strf fld a) in
    let vrep1 ix a = (stri ix a) in
    let vrep2 ix a = cats (mks (",")) (stri ix a) in
    let qn name = `AST_name (sr,name,[]) in 
    let (_,t) = be a in
    begin match t with
    | BTYP_record ls ->
      let first = ref true in
      let e = cats (
        List.fold_left (fun acc (s,_) -> 
          let res = if !first then fldrep1 s a else fldrep2 s a in
          first:=false;
          cats acc res
        )
        (mks "(")
        ls
        ) (mks ")") 
      in 
      be e
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
      be e

    | BTYP_inst (i,[]) ->
      begin match Flx_lookup_state.hfind "lookup:_strr" sym_table i with
      | { Flx_sym.id=name; Flx_sym.symdef=Flx_types.SYMDEF_struct ls } -> 
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
        be e
      | { Flx_sym.id=name; Flx_sym.symdef=Flx_types.SYMDEF_union ls } -> 
        let urep cname t =  
          match t with
          | TYP_void _ ->
            mks cname

          | _ ->
            let arg = EXPR_ctor_arg (sr, (qn cname,a)) in
            let strarg = apl2 sr "_strr" [arg] in
            apl2 sr "+" [mks (cname^" "); strarg]
        in 
        let condu cname t other =
          let cond = EXPR_match_ctor (sr, (qn cname,a)) in
          let u = urep cname t in
          EXPR_cond (sr, (cond,u,other)) 
        in 
        let e = 
          List.fold_left (fun acc (cname,ix,vs,t) -> 
            condu cname t acc 
          )
          (mks "MATCHFAILURE")
          ls
        in 
        be e

      | _ -> be (apl2 sr "str" [a]) 
      end

    | _ -> be (apl2 sr "str" [a])
    end


