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

(* -------------------------------------------------------------------------- *)
(* model binary operator as procedure call *)
let assign sr op l r =
  match op with
  | "_set" ->  STMT_cassign (sr,l,r)
 (* | "_pset" -> STMT_storeat (sr,l,r)  *)
  | "storeat" ->
  STMT_call
  (
    sr,
    EXPR_name (sr, op,[]),
    EXPR_tuple (sr, [l; r])
  )

  | _ ->
  STMT_call
  (
    sr,
    EXPR_name (sr, op,[]),
    EXPR_tuple (sr, [EXPR_ref (sr,l); r])
  )
(* -------------------------------------------------------------------------- *)
(* SHOULD BE UNUSED NOW ... *)
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

(* -------------------------------------------------------------------------- *)
(** Recursive statement desugerer *)
let rec rst_with_ret state name access (parent_vs:vs_list_t) rettype (st:statement_t): asm_t list =
  (* construct an anonymous name *)
  let parent_ts sr : typecode_t list =
    List.map 
      (fun (s,tp)-> TYP_name (sr,s,[])) 
      (fst parent_vs)
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


  let rst_with_ret name access parent_vs rettype st = rst_with_ret state name access parent_vs rettype st in 
  let rst name access parent_vs st = rst_with_ret name access parent_vs TYP_none st in 

  let rsts_with_ret name vs access rettype sts = 
    List.concat 
      (List.map
        (rst_with_ret name access vs rettype) 
        sts)
  in
  let rsts name vs acces sts = rsts_with_ret name vs access TYP_none sts in 

  (* rename _root headers *)
  let rex_with_ret x rettype = 
    Flx_desugar_expr.rex 
      rst_with_ret 
      (Flx_reqs.mkreqs state access parent_ts) 
      (Flx_reqs.map_reqs rqname') 
      state 
      name 
      x 
      rettype
  in
  let rex x = rex_with_ret x TYP_none in

  let seq () = state.Flx_desugar_expr.fresh_bid () in

  (* add _root headers and bodies as requirements for all
    bindings defined in this entity
  *)
  match st with

  | STMT_circuit (sr,cs) -> 
    [Exe (sr,(EXE_circuit (cs)))]

  | STMT_type_error (sr,stmt) ->
    let asms = rst name access parent_vs stmt in
    let result = List.fold_left (fun acc exe -> 
      match exe with
      | Exe (sr,asm) -> Exe (sr, EXE_type_error asm) :: acc 
      | a -> clierrx "[flx_desugar/flx_desugar.ml:106: E323] " sr ("type-error statement must be purely executable got " ^ string_of_asm 0 a)
    ) [] asms
    in  
    let result = List.rev result in
    result
  | STMT_type_assert (sr,stmt) ->
    let asms = rst name access parent_vs stmt in
    let result = List.fold_left (fun acc exe -> 
      match exe with
      | Exe (sr,asm) -> Exe (sr, EXE_type_assert asm) :: acc 
      | a -> clierrx "[flx_desugar/flx_desugar.ml:106: E323] " sr ("type-assert statement must be purely executable got " ^ string_of_asm 0 a)
    ) [] asms
    in  
    let result = List.rev result in
    result



  | STMT_try sr -> [Exe (sr,EXE_try)]
  | STMT_endtry sr -> [Exe (sr,EXE_endtry)]
  | STMT_catch (sr,s,t) -> [
     Dcl (sr,s,None, `Private,dfltvs, DCL_const ([],t,Flx_code_spec.Str s, NREQ_true));
     Exe (sr,EXE_catch (s,t))
   ]

  | STMT_seq _ -> assert false
  | STMT_private (sr,st) -> rst name `Private parent_vs st
  | STMT_include (sr,inspec) ->
      state.Flx_desugar_expr.include_file_cache <- (sr, inspec) :: state.Flx_desugar_expr.include_file_cache;
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
    print_endline ("Export Python fun " ^ string_of_suffixed_name name ^ " as '" ^ cpp_name^ "'");
    [Iface (sr, IFACE_export_python_fun (name, cpp_name))]

  | STMT_export_fun (sr,name,cpp_name) ->
    print_endline ("Export fun/proc " ^ string_of_suffixed_name name ^ " as '" ^ cpp_name^ "'");
    [Iface (sr, IFACE_export_fun (name, cpp_name))]

  | STMT_export_cfun (sr,name,cpp_name) ->
    print_endline ("Export cfun/cproc " ^ string_of_suffixed_name name ^ " as '" ^ cpp_name^ "'");
    [Iface (sr, IFACE_export_cfun (name, cpp_name))]

  | STMT_export_type (sr,typ,cpp_name) ->
    print_endline ("Export type " ^ string_of_typecode typ ^ " as '" ^ cpp_name^ "'");
    [Iface (sr, IFACE_export_type (typ, cpp_name))]

  | STMT_export_struct (sr,name) ->
    print_endline ("Export struct " ^ name);
    [Iface (sr, IFACE_export_struct (name))]

  | STMT_export_union (sr,fname,cname) ->
    print_endline ("Export union " ^ string_of_suffixed_name fname^ " as '" ^ cname ^ "'");
    [Iface (sr, IFACE_export_union (fname,cname))]

  | STMT_export_requirement (sr,reqs) ->
    print_endline ("Export requirement " ^ string_of_raw_reqs reqs);
    let _,_,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    dcls @ [Iface (sr, IFACE_export_requirement (reqs))]

  | STMT_var_decl (sr,name,vs,typ,expr) ->
    (* 
    let vs_exprs = List.map (fun (s,_)->TYP_name (sr,s,[])) (fst vs) in
    *)
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex_with_ret e t in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Var));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_init (name,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (
          sr,name,None,access,vs,DCL_value (TYP_typeof x, `Var));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_init (name,x))]
    | Some t,None ->
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Var))]
    | None,None -> failwith "Expected variable to have type or initialiser"
    end

  | STMT_val_decl (sr,name,vs,typ,expr) ->
    (*
    let vs_exprs = List.map (fun (s,_)->TYP_name (sr,s,[])) (fst vs) in
    *)
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex_with_ret e t in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Val));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_init ( name,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x, `Val));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_init ( name,x))]
    | Some t, None ->
        (* allowed in interfaces *)
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Val))]
    | None,None -> failwith "Expected value to have type or initialiser"
    end

  | STMT_once_decl (sr,name,vs,typ,expr) ->
print_endline ("Once decl detected, deprecated");
    let vs_exprs = List.map (fun (s,_)->TYP_name (sr,s,[])) (fst vs) in
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Once));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x, `Once));
        (* Exe (sr,EXE_init (name,x))] *)
        Exe (sr,EXE_assign ( EXPR_name (sr,name,vs_exprs) ,x))]
    | Some t, None ->
        (* allowed in interfaces *)
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Once))]
    | None,None -> failwith "Expected value to have type or initialiser"
    end

  | STMT_ref_decl (sr,name,vs,typ,expr) ->
print_endline ("Ref decl detected, deprecated");
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
print_endline ("Translating Lazy Declaration " ^ name);
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
    let index, _,props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,index,access,vs,DCL_const (props,typ,s, Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls

  (* types *)
  | STMT_abs_decl (sr,name,vs,quals,s, reqs) ->
    let index,quals',props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,index,access,vs,DCL_abs (quals' @ quals,s,Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls
   
  | STMT_virtual_type (sr,name) ->
    [Dcl (sr,name,None,access,dfltvs,DCL_virtual_type)]

  | STMT_newtype (sr,name,vs,t) ->
    [Dcl (sr,name,None,access,vs,DCL_newtype t)]

  | STMT_instance_type (sr,name,vs,t) ->
    [Dcl (sr,name,None,access,vs,DCL_instance_type t)]


  | STMT_union (sr,name, vs, components) -> [Dcl (sr,name,None,access,vs,DCL_union (components))]
  | STMT_struct (sr,name, vs, components) ->  [Dcl (sr,name,None,access,vs,DCL_struct (components))]
  | STMT_cstruct (sr,name, vs, components, reqs) ->  
    let index,_,props,dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name,index,access,vs,DCL_cstruct (components, Flx_reqs.map_reqs rqname' sr reqs)) :: dcls

  | STMT_typeclass (sr,name, vs, sts) ->
    let asms = rsts name (Flx_merge_vs.merge_vs parent_vs vs) `Public sts in
    let asms = bridge name sr :: asms in
    let mdcl = [ Dcl (sr,name,None,access,vs, DCL_typeclass asms) ] in
    (* hack: add _init_ function to typeclass to init any variables,
       but only if it is not polymorphic
    *)
(*
    if vs = dfltvs then gen_call_init sr name :: mdcl else 
*)
    mdcl

  | STMT_begin_typeclass _ -> assert false

  (* typeclasses and modules are basically the same thing now .. *)
  | STMT_untyped_module (sr,name', vs', sts) ->
    let asms = rsts name' (Flx_merge_vs.merge_vs parent_vs vs') `Public sts in
    let asms = bridge name' sr :: asms in
    let mdcl =
      [ Dcl (sr,name',None,access,vs', DCL_module asms) ]
    in
    (* HACK !!!! Actually, it's wrong: there are no polymorphic modules
       or polymorphic variables .. *)
(*
    if vs' = dfltvs then gen_call_init sr name' :: mdcl else 
*)
    mdcl

  | STMT_library (sr,name', sts) ->
    let asms = rsts name' (parent_vs) `Public sts in
    let asms = bridge name' sr :: asms in
    let mdcl =
      [ Dcl (sr,name',None,access,dfltvs, DCL_library asms) ]
    in
    mdcl

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
    let asms,typ = Flx_desugar_expr.rett rex typ sr in
    (*
      print_endline (string_of_desugared asms);
      print_endline ("------------");
    *)
    asms @ [Dcl (sr,name,None,access,vs,DCL_type_alias (typ))]

  | STMT_inherit (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit qn)]
  | STMT_inherit_fun (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit_fun qn)]

  | STMT_curry (sr,name',vs,pps,ret,effects,kind,adjs,sts) ->
(*
print_endline ("STMT_curry " ^ name' ^ ", rettype=" ^ string_of_typecode ret);
*)
    (* construct a function-like STMT from the given statements, etc. *)
    let curr = (Flx_curry.mkcurry seq sr name' vs pps ret effects kind sts adjs) in

    (* The final construction we want to return and possibly export. *)
    let fdef = rst name access parent_vs curr in

    (* Determine if we should export this function *)
    let export_name = ref name' in
    let doexport = ref false in
    List.iter 
      (
        function 
        | `Export -> doexport := true 
        | `NamedExport s -> doexport := true; export_name := s 
        | _ -> ()
      )
      adjs;

    (* Handle exporting process *)
    if (!doexport) then
      let domain = 
        match pps with
        | (ps,_)::_ -> typeof_paramspec_t ps
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
        Iface (sr, IFACE_export_fun (sname, (!export_name))) :: fdef
      | `CFunction ->
        print_endline ("Export cfun " ^ string_of_suffixed_name sname ^ " as '" ^ (!export_name)  ^ "'");
        Iface (sr, IFACE_export_cfun (sname, (!export_name))) :: fdef
    else fdef
    

  (* functions *)
  | STMT_reduce (sr,name,reds) ->
    [ Dcl (sr,name,None,access,dfltvs,DCL_reduce (reds)) ]

  | STMT_axiom (sr,name,vs,params, rsrc) ->
    [ Dcl (sr,name,None,access,vs,DCL_axiom (params,rsrc)) ]

  | STMT_lemma (sr,name,vs,params, rsrc) ->
    [ Dcl (sr,name,None,access,vs,DCL_lemma (params,rsrc)) ]

  | STMT_function (sr,name', vs, params, (res,postcondition), effects, props, sts) ->
(*
    begin match sts with
    | [STMT_fun_return (sr,e)] -> 
      print_endline ("Desugar: SOLO RETURN STMT_function "^name'^
      " to DCL_function " ^ Flx_srcref.short_string_of_src sr ^ ", rettype=" ^ string_of_typecode res);
    | _ -> ()
    end
    ;
*)
    (*
    print_endline ("Desugar: STMT_function "^name'^" to DCL_function " ^ Flx_srcref.short_string_of_src sr);
    print_endline (string_of_statement 0 st);
    *)
    let ps,traint = params in
(*
print_endline ("Desugar params=");
List.iter  (fun (sr,kind,name,typ,init) -> print_endline (name ^ " at " ^ Flx_srcref.short_string_of_src sr))
ps;
*)
    begin match traint,postcondition with
    | None,None ->

      (* Check for unbound type parameters and make them bindable. Also, return
        them for introduction *)
      let vs',params = Flx_curry.fix_param sr seq params in

      (* Merge new type variables with existing *)
      let vs = Flx_merge_vs.merge_vs vs (vs',dfltvs_aux)  in
      let asms = rsts_with_ret name' dfltvs `Public res sts in
      let asms = bridge name' sr :: asms in

      (* Fix params with type level lifting *)
      let asms',ps = Flx_desugar_expr.rett_fixparams rex ps in
(*
print_endline ("Fixed params=");
List.iter  (fun (sr,kind,name,typ,init) -> print_endline (name ^ " at " ^ Flx_srcref.short_string_of_src sr))
ps;
*)
      let params = ps,traint in
      let asms = asms' @ asms in

      (* Build the function declaration *)
      [
        Dcl (sr,name',None,access,vs,
          DCL_function (params, res, effects, props, asms)
        )
      ]

    | pre,post ->
    (*
      print_endline ("This post case (inside "^name'^") s reached.");
      print_endline ("Postconditions: " ^
        (match post with
        | Some e -> (string_of_expr e)
        | None -> "no post condition"));
      *)

      let name'' = "_wrap_" ^ name' in
      let inner = EXPR_name (sr,name'',[]) in
      let un = EXPR_tuple (sr,[]) in

      let pre_assert = (match pre with
        | None -> []
        | Some x -> [STMT_assert (src_of_expr x,x)]) in

      let sts =
        pre_assert @
        [ STMT_function (sr,name'', dfltvs,(Slist [],None),(res,None),effects,props,sts); ] @

        begin match res with

        (* For procedures *)
        | TYP_void _ ->
          [STMT_call (sr,inner,un) ] @
          begin match post with
          | None -> []
          | Some y -> [STMT_assert (src_of_expr y,y)]
          end

        (* For functions *)
        | _ ->
          let retval = EXPR_apply (sr,(inner,un)) in
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

      (* Fix params with type level lifting *)
      let asms,ps = Flx_desugar_expr.rett_fixparams rex ps in

      let st =
        STMT_function (sr,name',vs,(ps,None),(res,None),effects,props,sts)
      in
      asms @ (rst_with_ret name access parent_vs res st) 
    end

  | STMT_fun_decl (sr,name',vs,args,result,code, reqs,prec) ->
    (*
    print_endline (string_of_statement 0 st);
    *)
    let vs,con = vs in
    let index,_,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    (* hackery *)
    let vs,args = List.fold_left begin fun (vs,args) arg ->
      match arg with
      | TYP_apply (TYP_name (_,"!",[]), TYP_name (sr,name,[])) ->
(*
print_endline ("Flx_desugar found ref to typeset " ^ name);
*)
          let n = seq() in
          let var = "T" ^ string_of_bid n in
          (*
          print_endline ("Implicit var " ^ var);
          *)
          (*
          let v = var,TPAT_name (name,[]) in
          *)
(*
          let v = var, KND_typeset name in
*)

          let v = var, KND_tpattern (TYP_name (sr,name,[])) in
          let arg = TYP_name (sr,var,[]) in
          v::vs, arg::args
      | x -> vs, x::args
    end (List.rev vs, []) args
    in
    Dcl (sr, name', index, access, (List.rev vs, con),
      DCL_fun (props, List.rev args, result, code, Flx_reqs.map_reqs rqname' sr reqs, prec))
    :: dcls

  | STMT_callback_decl (sr,name',args,result,reqs) ->
    let index,_,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    Dcl (sr,name',index,access,dfltvs,
      DCL_callback (props,args,result,Flx_reqs.map_reqs rqname' sr reqs))
    :: dcls

  (* misc *)
  | STMT_insert (sr,name',vs,s,kind,reqs) ->
    let index,_,props, dcls, reqs = Flx_reqs.mkreqs state access parent_ts sr reqs in
    (* SPECIAL case: insertion requires insertion use filo order *)
    dcls @ [
      Dcl (sr,Flx_reqs.map_req name name',index,access,vs,DCL_insert (s, kind, Flx_reqs.map_reqs rqname' sr reqs))
    ]

  (* executable *)
  | STMT_cgoto (sr,e) -> 
   let d,x = rex e in d @ [Exe (sr,EXE_cgoto x)]

  | STMT_ifcgoto (sr,e1,e2) -> 
   let d1,x1 = rex e1 in 
   let d2,x2 = rex e2 in 
   d1 @ d2 @ [Exe (sr,EXE_ifcgoto (x1,x2))]


  | STMT_fun_return (sr,e) ->
(*
print_endline ("STMT_fun_return " ^ string_of_expr e^ ", rettype=" ^ string_of_typecode rettype);
*)
    let d,x = rex_with_ret e rettype in 
    let x = if rettype = TYP_none then x else EXPR_coercion (sr,(x, rettype)) in
    d @ [Exe (sr,EXE_fun_return x)]

  | STMT_yield (sr,e) ->
    let d,x = rex_with_ret e rettype in d @ [Exe (sr,EXE_yield x)]

  | STMT_assert (sr,e) ->
    let d,x = rex e in d @ [Exe (sr,EXE_assert x)]

  | STMT_nop _ -> []

  | STMT_cassign (sr,l,r) ->
     let l1,x1 = rex l in
     let l2,x2 = rex r in
     l1 @ l2 @ [Exe (sr,EXE_assign (x1,x2))]

  | STMT_storeat (sr,l,r) ->
     let l1,x1 = rex l in
     let l2,x2 = rex r in
     l1 @ l2 @ [Exe (sr,EXE_storeat (x1,x2))]


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
            | x -> clierrx "[flx_desugar/flx_desugar.ml:568: E324] " sr ("identifier required in val init, got " ^ string_of_expr x)
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

      (* singleton case *)
      | `List [`Val (sr,name),t] ->
         [STMT_val_decl (sr,name,dfltvs,t,Some r)]

      (* singleton case *)
      | `List [`Var (sr,name),t] ->
         [STMT_var_decl (sr,name,dfltvs,t,Some r)]

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

  | STMT_call_with_trap (sr,proc, arg) ->
    let d1,x1 = rex proc in
    let d2,x2 = rex arg in
    d1 @ d2 @ [Exe (sr,EXE_call_with_trap (x1,x2))]


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
  | STMT_code (sr,s,e) -> 
    let d,x = rex e in
    d @ [Exe (sr,EXE_code (s,x))]

  | STMT_noreturn_code (sr,s,e) -> 
    let d,x = rex e in
    d @ [Exe (sr,EXE_noreturn_code (s,x))]

  | STMT_stmt_match (sr,(e,pss)) -> 
    Flx_match.gen_stmt_match seq rex_with_ret (rsts_with_ret name parent_vs access) name sr e pss rettype

  (* split into multiple declarations *)

  | STMT_ctypes _
  | STMT_ifdo _
  | STMT_ifreturn _
  | STMT_macro_val _
  | STMT_macro_forall _
  | STMT_scheme_string _
    -> assert false

  | STMT_invariant (sr, _) -> clierrx "[flx_desugar/flx_desugar.ml:653: E325] " sr "'invariant' not valid outside an object definition"

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
    (rst_with_ret state state.Flx_desugar_expr.name `Public dfltvs TYP_none) (* should be void? *)
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


