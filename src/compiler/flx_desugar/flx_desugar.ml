open Flx_version
open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_typing2
open Flx_pat
open Flx_exceptions

module CS = Flx_code_spec

let generated = Flx_srcref.make_dummy "[flx_desugar] generated"

(* model binary operator as procedure call *)
let assign sr op l r =
  match op with
  | "_set" ->  STMT_cassign (sr,l,r)
  | "_pset" -> 
    let deref = EXPR_name (sr, "deref", []) in
    let deref_l = EXPR_apply (sr, (deref, l)) in
    STMT_cassign (sr,deref_l,r)
  | _ ->
  STMT_call
  (
    sr,
    EXPR_name (sr, op,[]),
    EXPR_tuple (sr, [EXPR_ref (sr,l); r])
  )


let gen_call_init sr name' =
  let mname = EXPR_name (sr,name',[]) in
  let pname = `AST_lookup (sr, (mname, "_init_", [])) in
  let sname = EXPR_suffix (sr, (pname, TYP_tuple [])) in
  let unitt = EXPR_tuple (generated,[]) in
  Exe
  (
    sr,
    EXE_call (sname, unitt)
  )

let rec rst state name access (parent_vs:vs_list_t) (st:statement_t) : asm_t list =
  (* construct an anonymous name *)
  let parent_ts sr : typecode_t list =
    List.map (fun (s,tp)-> TYP_name (sr,s,[])) (fst parent_vs)
  in

  (* this is the name of the parent's root requirement tag, if the parent
   * is a module X, this will be "_rqs_X".
   *)
  let rqname' sr = `AST_name (sr, "_rqs_" ^ name, parent_ts sr) in

  (* Add a root to child named 'n'.
     All root requirements in the child go to this symbol,
     and it requires our root in turn.

     parent_vs is the vs list required for us,
     it is always empty for a function.
  *)

  let bridge sr n = Flx_reqs.bridge sr n parent_vs rqname' name in

  (* rename _root headers *)

  let rex x = Flx_desugar_expr.rex rst (Flx_reqs.mkreqs state access parent_ts) (Flx_reqs.map_reqs rqname') state name x in
  let rsts name vs access sts = List.concat (List.map
    (rst state name access vs) sts)
  in
  let seq () = state.Flx_desugar_expr.fresh_bid () in
  (* add _root headers and bodies as requirements for all
    bindings defined in this entity
  *)
  match st with
  | STMT_try sr -> [Exe (sr,EXE_try)]
  | STMT_endtry sr -> [Exe (sr,EXE_endtry)]
  | STMT_catch (sr,s,t) -> [
     Dcl (sr,s,None, `Private,dfltvs, DCL_const ([],t,Flx_code_spec.Str s, NREQ_true));
     Exe (sr,EXE_catch (s,t))
   ]

  | STMT_seq _ -> assert false
  | STMT_private (sr,st) -> rst state name `Private parent_vs st
  | STMT_include (sr,inspec) ->
      state.Flx_desugar_expr.include_file_cache <- inspec :: state.Flx_desugar_expr.include_file_cache;
      []
  | STMT_label (sr,s) -> [Exe (sr,EXE_label s)]
  | STMT_proc_return sr -> [Exe (sr,EXE_proc_return)]
  | STMT_proc_return_from (sr,s) -> [Exe (sr,EXE_proc_return_from s)]
  | STMT_halt (sr,s) -> [Exe (sr,EXE_halt s)]
  | STMT_trace (sr,v,s) -> [Exe (sr,EXE_trace (v,s))]
  | STMT_goto (sr,s) -> [Exe (sr,EXE_goto s)]
  | STMT_open (sr,(vs,aux),name) ->
    let vs = List.map (fun (n,t)->let i = seq() in n,i,t) vs in
    [Dir (sr,DIR_open ((vs,aux),name))]
  | STMT_inject_module (sr,(vs,aux),name) -> 
    let vs = List.map (fun (n,t)->let i = seq() in n,i,t) vs in
    [Dir (sr,DIR_inject_module ((vs,aux),name))]

  | STMT_use (sr,n,qn) -> [Dir (sr,DIR_use (n,qn))]
  | STMT_comment (sr,s) -> [Exe (sr,EXE_comment s)]

  (* objects *)
  | STMT_export_python_fun (sr,name,cpp_name) ->
    [Iface (sr, IFACE_export_python_fun (name, cpp_name))]

  | STMT_export_fun (sr,name,cpp_name) ->
    [Iface (sr, IFACE_export_fun (name, cpp_name))]

  | STMT_export_cfun (sr,name,cpp_name) ->
    [Iface (sr, IFACE_export_cfun (name, cpp_name))]

  | STMT_export_type (sr,typ,cpp_name) ->
    [Iface (sr, IFACE_export_type (typ, cpp_name))]

  | STMT_var_decl (sr,name,vs,typ,expr) ->
    let vs_exprs = List.map (fun (s,_)->TYP_name (sr,s,[])) (fst vs) in
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Var));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (
          sr,name,None,access,vs,DCL_value (TYP_typeof x, `Var));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | Some t,None ->
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Var))]
    | None,None -> failwith "Expected variable to have type or initialiser"
    end

  | STMT_val_decl (sr,name,vs,typ,expr) ->
    let vs_exprs = List.map (fun (s,_)->TYP_name (sr,s,[])) (fst vs) in
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Val));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x, `Val));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | Some t, None ->
        (* allowed in interfaces *)
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Val))]
    | None,None -> failwith "Expected value to have type or initialiser"
    end

  | STMT_ref_decl (sr,name,vs,typ,expr) ->
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Ref));
        Exe (sr,EXE_init (name,EXPR_ref (sr,x)))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x, `Ref));
        Exe (sr,EXE_init (name,EXPR_ref (sr,x)))]
    | _,None -> failwith "Expected ref to have initialiser"
    end


  | STMT_lazy_decl (sr,name,vs,typ,expr) ->
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [Dcl (sr,name,None,access,vs,DCL_value (t,`Lazy x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x,`Lazy x))]
    | _,None -> failwith "Expected lazy value to have initialiser"
    end

  | STMT_const_decl (sr,name, vs,typ, s, reqs) ->
    let _,props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,None,access,vs,DCL_const (props,typ,s, Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls

  (* types *)
  | STMT_abs_decl (sr,name,vs,quals,s, reqs) ->
    let quals',props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,None,access,vs,DCL_abs (quals' @ quals,s,Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls

  | STMT_newtype (sr,name,vs,t) ->
    [Dcl (sr,name,None,access,vs,DCL_newtype t)]

  | STMT_union (sr,name, vs, components) -> [Dcl (sr,name,None,access,vs,DCL_union (components))]
  | STMT_struct (sr,name, vs, components) ->  [Dcl (sr,name,None,access,vs,DCL_struct (components))]
  | STMT_cstruct (sr,name, vs, components, reqs) ->  
    let _,props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,None,access,vs,DCL_cstruct (components, Flx_reqs.map_reqs rqname' sr reqs)) :: dcls

  | STMT_typeclass (sr,name, vs, sts) ->
    let asms = rsts name (Flx_merge_vs.merge_vs parent_vs vs) `Public sts in
    let asms = bridge name sr :: asms in
    let mdcl = [ Dcl (sr,name,None,access,vs, DCL_typeclass asms) ] in
    (* hack: add _init_ function to typeclass to init any variables,
       but only if it is not polymorphic
    *)
    if vs = dfltvs then gen_call_init sr name :: mdcl else mdcl

  (* typeclasses and modules are basically the same thing now .. *)
  | STMT_untyped_module (sr,name', vs', sts) ->
    let asms = rsts name' (Flx_merge_vs.merge_vs parent_vs vs') `Public sts in
    let asms = bridge name' sr :: asms in
    let mdcl =
      [ Dcl (sr,name',None,access,vs', DCL_module asms) ]
    in
    (* HACK !!!! Actually, it's wrong: there are no polymorphic modules
       or polymorphic variables .. *)
    if vs' = dfltvs then gen_call_init sr name' :: mdcl else mdcl


  | STMT_instance (sr, vs, name, sts) ->
    let name',ts = match name with
    | `AST_lookup (_,(_,name,ts)) -> name,ts
    | `AST_name (_,name,ts) -> name,ts
    | _ -> syserr sr "Instance name has wrong form, qualified name required"
    in
    let asms = rsts name' dfltvs `Public sts in
    let asms = bridge name' sr :: asms in
    let mdcl =
      [ Dcl (sr,name',None,access,vs, DCL_instance (name,asms)) ]
    in
    mdcl

  | STMT_type_alias (sr,name,vs,typ) -> 
    [Dcl (sr,name,None,access,vs,DCL_type_alias (typ))]

  | STMT_inherit (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit qn)]
  | STMT_inherit_fun (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit_fun qn)]

  | STMT_curry (sr,name',vs,pps,ret,kind,adjs,sts) ->
    let fdef = rst state name access parent_vs 
      (Flx_desugar_expr.mkcurry seq sr name' vs pps ret kind sts adjs) 
    in
    let export_name = ref name' in
    let doexport = ref false in
    List.iter 
      (
        function 
        | `Export -> doexport := true 
        | `NamedExport s -> doexport := true; export_name := s 
        | _ -> ()
      )
      adjs
    ;
    if (!doexport) then
      let domain = 
        match pps with
        | (ps,_)::_ -> paramtype ps
        | [] -> TYP_tuple []
      in
      let qn = `AST_name (sr,name',[]) in
      let sname = `AST_suffix (sr,(qn,domain)) in
      match kind with
      | `Function
      | `GeneratedInlineProcedure
      | `GeneratedInlineFunction
      | `InlineFunction
      | `NoInlineFunction
      | `Virtual
      | `Ctor
      | `Generator
      | `GeneratorMethod
      | `Method
      | `Object ->
        print_endline ("Export fun " ^ string_of_suffixed_name sname ^ " as '" ^ (!export_name) ^ "'");
        Iface (sr, IFACE_export_fun (sname, name')) :: fdef
      | `CFunction ->
        print_endline ("Export cfun " ^ string_of_suffixed_name sname ^ " as '" ^ (!export_name)  ^ "'");
        Iface (sr, IFACE_export_cfun (sname, name')) :: fdef
    else fdef
    

  (* functions *)
  | STMT_reduce (sr,name,vs,params, rsrc,rdst) ->
    [ Dcl (sr,name,None,access,vs,DCL_reduce (params,rsrc,rdst)) ]

  | STMT_axiom (sr,name,vs,params, rsrc) ->
    [ Dcl (sr,name,None,access,vs,DCL_axiom (params,rsrc)) ]

  | STMT_lemma (sr,name,vs,params, rsrc) ->
    [ Dcl (sr,name,None,access,vs,DCL_lemma (params,rsrc)) ]

  | STMT_function (sr,name', vs, params, (res,postcondition), props, sts) ->
    (*
    print_endline (string_of_statement 0 st);
    *)
    let ps,traint = params in
    begin match traint,postcondition with
    | None,None ->
      let vs',params = Flx_desugar_expr.fix_params sr seq params in
      let vs = Flx_merge_vs.merge_vs vs (vs',dfltvs_aux)  in
      let asms = rsts name' dfltvs `Public sts in
      let asms = bridge name' sr :: asms in
      [
        Dcl (sr,name',None,access,vs,
          DCL_function (params, res, props, asms)
        )
      ]
    | pre,post ->
      let name'' = "_wrap_" ^ name' in
      let inner = EXPR_name (sr,name'',[]) in
      let un = EXPR_tuple (sr,[]) in
      let sts =
        (match pre with
        | None -> []
        | Some x -> [STMT_assert (src_of_expr x,x)]
        )
        @
        [
          STMT_function (sr,name'', dfltvs,([],None),(res,None),props,sts);
        ]
        @
        begin match res with
        | TYP_void _ ->
           [STMT_call (sr,inner,un) ] @
           begin match post with
           | None -> []
           | Some y -> [STMT_assert (src_of_expr y,y)]
           end
          | _ ->
            let retval:expr_t = EXPR_apply (sr,(inner,un)) in
            begin match post with
            | None ->
              [STMT_fun_return (sr,retval)]
            | Some y ->
              [
                STMT_val_decl (sr,"result",dfltvs,None,Some retval);
                STMT_assert (src_of_expr y,y);
                STMT_fun_return (sr,EXPR_name (sr,"result",[]))
              ]
            end
        end
      in
      let st =
        STMT_function (sr,name',vs,(ps,None),(res,None),props,sts)
      in
      rst state name access parent_vs st
    end

  | STMT_fun_decl (sr,name',vs,args,result,code, reqs,prec) ->
    (*
    print_endline (string_of_statement 0 st);
    *)
    let vs,con = vs in
    let _,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    (* hackery *)
    let vs,args = List.fold_left begin fun (vs,args) arg ->
      match arg with
      | TYP_apply (TYP_name (_,"!",[]), TYP_name (sr,name,[])) ->
          let n = seq() in
          let var = "T" ^ string_of_bid n in
          (*
          print_endline ("Implicit var " ^ var);
          *)
          (*
          let v = var,TPAT_name (name,[]) in
          *)
          let v = var, TYP_name (sr,name,[]) in
          let arg = TYP_name (sr,var,[]) in
          v::vs, arg::args
      | x -> vs, x::args
    end (List.rev vs, []) args
    in
    Dcl (sr, name', None, access, (List.rev vs, con),
      DCL_fun (props, List.rev args, result, code, Flx_reqs.map_reqs rqname' sr reqs, prec))
    :: dcls

  | STMT_callback_decl (sr,name',args,result,reqs) ->
    let _,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name',None,access,dfltvs,
      DCL_callback (props,args,result,Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls

  (* misc *)
  | STMT_insert (sr,name',vs,s,kind,reqs) ->
    let _,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    (* SPECIAL case: insertion requires insertion use filo order *)
    dcls @ [
      Dcl (sr,Flx_reqs.map_req name name',None,access,vs,DCL_insert (s, kind, Flx_reqs.map_reqs rqname' sr reqs))
    ]

  (* executable *)
  | STMT_fun_return (sr,e) ->
    let d,x = rex e in d @ [Exe (sr,EXE_fun_return x)]

  | STMT_yield (sr,e) ->
    let d,x = rex e in d @ [Exe (sr,EXE_yield x)]

  | STMT_assert (sr,e) ->
    let d,x = rex e in d @ [Exe (sr,EXE_assert x)]

  | STMT_nop _ -> []

  | STMT_cassign (sr,l,r) ->
     let l1,x1 = rex l in
     let l2,x2 = rex r in
     l1 @ l2 @ [Exe (sr,EXE_assign (x1,x2))]

  (* STMT_assign translates to many things, including
     STMT_cassign (via the function assign) but never
     to itself. STMT_cassign eventually becomes EXE_assign
  *)
  | STMT_assign (sr,fid,l,r) ->
    let rec aux (l,t) r =
      match l with
      | `Expr (sr,e) ->
        begin match e with
        | EXPR_tuple (_,ls) ->
          let n = seq() in
          let vn = "_ds1_" ^ string_of_bid n in
          let sts = ref [] in
          let count = ref 0 in
          List.iter
          (fun l ->
            let r' = EXPR_get_n (sr,(!count,EXPR_name (sr,vn,[]))) in
            let l' = `Expr (sr,l),None in
            let asg = aux l' r' in
            sts := !sts @ asg;
            incr count
          )
          ls
          ;
          STMT_val_decl (sr,vn,dfltvs,t,Some r) :: !sts
        | _ ->
          if fid = "_init"
          then
            match e with
            | EXPR_coercion (_,(EXPR_name (_,n,[]),t')) ->
              let t = match t with
                | None -> Some t'
                | t -> t
              in
              [STMT_val_decl (sr,n,dfltvs,t,Some r)]

            | EXPR_name (_,n,[]) ->
              [STMT_val_decl (sr,n,dfltvs,t,Some r)]
            | x -> clierr sr ("identifier required in val init, got " ^ string_of_expr x)
          else
            [assign sr fid e r]
        end
      | `Val (sr,n) ->
          [STMT_val_decl (sr,n,dfltvs,t,Some r)]
      | `Var (sr,n) ->
          [STMT_var_decl (sr,n,dfltvs,t,Some r)]
      | `Skip (sr) ->  []
      | `Name (sr,n) ->
        let n = EXPR_name(sr,n,[]) in
          [assign sr fid n r]
      | `List ls ->
          let n = seq() in
          let vn = "_ds2_" ^ string_of_bid n in
          let sts = ref [] in
          let count = ref 0 in
          List.iter
          (fun l ->
            let r' = EXPR_get_n (sr,(!count,EXPR_name (sr,vn,[]))) in
            let asg = aux l r' in
            sts := !sts @ asg;
            incr count
          )
          ls
          ;
          STMT_val_decl (sr,vn,dfltvs,t,Some r) :: !sts
    in
      let sts = aux l r in
      rsts name parent_vs access sts

  | STMT_call (sr,proc, arg) ->
    let d1,x1 = rex proc in
    let d2,x2 = rex arg in
    d1 @ d2 @ [Exe (sr,EXE_call (x1,x2))]

  | STMT_init (sr,v,e) ->
    let d,x = rex e in
    d @ [Exe (sr,EXE_init (v,e))]

  | STMT_jump (sr,proc, arg) ->
    let d1,x1 = rex proc in
    let d2,x2 = rex arg in
    d1 @ d2 @ [Exe (sr,EXE_jump (x1,x2))]

  | STMT_loop (sr,proc, arg) ->
    let d2,x2 = rex arg in
    d2 @ [Exe (sr,EXE_loop (proc,x2))]

  | STMT_ifgoto (sr,e,lab)->
    let d,x = rex e in
    d @ [Exe (sr,EXE_ifgoto (x,lab))]


  | STMT_svc (sr,name) ->  [Exe (sr,EXE_svc name)]
  | STMT_code (sr,s) -> [Exe (sr,EXE_code s)]
  | STMT_noreturn_code (sr,s) -> [Exe (sr,EXE_noreturn_code s)]

  | STMT_stmt_match (sr,(e,pss)) -> 
    Flx_match.gen_stmt_match seq rex rsts name parent_vs access sr e pss

  (* split into multiple declarations *)

  | STMT_ctypes _
  | STMT_ifdo _
  | STMT_ifreturn _
  | STMT_macro_val _
  | STMT_macro_forall _
  | STMT_scheme_string _
    -> assert false

(** Desugar all the statements in a compilation unit. *)
let rec desugar_stmts state curpath stmts =
  let stmts = match stmts with
    | [] -> [STMT_nop (generated, "empty module")]
    | _ -> stmts
  in
(*
  print_endline ("   PRE Macro expand counter = " ^ 
    string_of_int (Flx_macro.get_macro_seq (state.Flx_desugar_expr.macro_state))); 
*)
  let stmts = Flx_macro.expand_macros state.Flx_desugar_expr.macro_state stmts in
(*
  print_endline ("   POST Macro expand counter = " ^ 
     string_of_int (Flx_macro.get_macro_seq (state.Flx_desugar_expr.macro_state))); 
*)
  let asms = List.concat (List.map
    (rst state state.Flx_desugar_expr.name `Public dfltvs)
    stmts)
  in
(*
  print_endline ("   POST DESUGAR counter = " ^ 
     string_of_int (Flx_macro.get_macro_seq (state.Flx_desugar_expr.macro_state))); 
*)
  (* Clear the include file cache. *)
  let include_files = state.Flx_desugar_expr.include_file_cache in
  state.Flx_desugar_expr.include_file_cache <- [];

  include_files, asms

