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

type desugar_state_t = {
  name: string;
  macro_state: Flx_macro.macro_state_t;
  fresh_bid: unit -> bid_t;
  mutable include_file_cache: string list;
}

let generated = Flx_srcref.make_dummy "[flx_desugar] generated"

let block sr body :statement_t =
  let e = EXPR_lambda (sr,(dfltvs,[[],None],TYP_void sr,body)) in
  STMT_call (sr,e,EXPR_tuple(sr,[]))

let fix_params sr seq (ps:params_t):plain_vs_list_t * params_t =
  let rec aux (ps:parameter_t list) :plain_vs_list_t * parameter_t list =
    match ps with
    | (k,x,TYP_none,d) :: t ->
      let v = "_v" ^ string_of_bid (seq()) in
      let vt: typecode_t = TYP_name (generated,v,[]) in
      let vs,ps = aux t in
      (*
      ((v,TPAT_any)::vs),((k,x,vt,d)::ps) (* a bit HACKY *)
      *)
      ((v,TYP_patany sr)::vs),((k,x,vt,d)::ps) (* a bit HACKY *)

    | h :: t ->
      let vs,ps = aux t in
      vs, (h::ps)
    | [] -> [],[]
  in
  let ps, traint = ps in
  let vs,ps = aux ps in
  vs,(ps,traint)

let arglist x =
  match x with
  | EXPR_tuple (_,ts) -> ts
  | _ -> [x]

let cal_props = function
  | `CFunction -> `Cfun::[]
  | `InlineFunction -> `Inline::[]
  | `NoInlineFunction -> `NoInline::[]
  | `Ctor -> `Ctor::[]
  | `Generator -> (* `NoInline:: *) `Generator::[]
  | `Virtual -> `Virtual::[]
  | _ -> []

let mkcurry seq sr name (vs:vs_list_t) (args:params_t list) return_type (kind:funkind_t) body props =
  let vs, tcon = vs in
  let return_type, postcondition = return_type in
  let vss',(args:params_t list)= List.split (List.map (fix_params sr seq) args) in
  let vs = List.concat (vs :: vss') in
  let vs : vs_list_t = vs,tcon in
  let mkfuntyp d c = TYP_function (d,c)
  and typeoflist lst = match lst with
    | [x] -> x
    | _ -> TYP_tuple lst
  in
  let mkret arg ret = mkfuntyp (typeoflist (List.map (fun(x,y,z,d)->z) (fst arg))) ret in
  let arity = List.length args in
  let rettype args =
    match return_type with
    | TYP_none -> TYP_none
    | _ -> List.fold_right mkret args return_type
  in

  let rec aux (args:params_t list) (vs:vs_list_t) props =
    let n = List.length args in
    let name n =
      if n = arity
      then name
      else name^"'" ^ si (arity-n+1)
    in
    match args with
    | [] ->
        begin match return_type with
        | TYP_void _ ->
          STMT_function (sr, name n, vs, ([],None), (return_type,postcondition), props, body)
        | _ ->
          (* allow functions with no arguments now .. *)
          begin match body with
          | [STMT_fun_return (_,e)] ->
            let rt = match return_type with
            | TYP_none -> None
            | x -> Some x
            in
            STMT_lazy_decl (sr, name n, vs, rt, Some e)
          | _ ->
          clierr sr "Function with no arguments"
          end
        end

    | h :: [] -> (* bottom level *)
        STMT_function (sr, name n, vs, h, (return_type,postcondition), props, body)
    | h :: t ->
      let argt =
        let hdt = List.hd t in
        let xargs,traint = hdt in
        typeoflist (List.map (fun (x,y,z,d) -> z) xargs)
      in
      let m = List.length args in
      let body =
        [
          aux t dfltvs [];
          STMT_fun_return
          (
            sr,
            EXPR_suffix
            (
              sr,
              (
                `AST_name (sr,name (m-1),[]),argt
              )
            )
          )
        ]
      in
        STMT_function (sr, name m, vs, h, (rettype t,None), `Generated "curry"::props, body)
   in aux args vs (cal_props kind @ props)

(* model binary operator as procedure call *)
let assign sr op l r =
  match op with
  | "_set" -> STMT_cassign (sr,l,r)
  | "_pset" -> STMT_cassign (sr,EXPR_deref (sr,l),r)
  | _ ->
  STMT_call
  (
    sr,
    EXPR_name (sr, op,[]),
    EXPR_tuple (sr, [EXPR_ref (sr,l); r])
  )

(* split lambdas out. Each lambda is replaced by a
   reference to a synthesised name in the original
   statement, which is prefixed by the definition.

   Blocks are replaced by a procedure definition
   and a call.

   The match statement requires all case bodies
   be replaced by calls as well.

   Actual lambdas in expressions are replaced
   by a reference and function or procedure definition.

   Attempt handler bodies are requires all handlers
   to be replaced by a call as well.
*)

(* convert an expression into a list of assembly instructions,
   plus an expression: basically, this means removing lambdas
*)

(*
  ARGGG! rex guarrantees to lift lambdas out of expressions,
  but the lifted lambda declarations also have bodies
  which might contain expression containing lambdas,
  so we have to apply rsts to these bodies..
*)

let rec rex mkreqs map_reqs state name (e:expr_t) : asm_t list * expr_t =
  let rex e = rex mkreqs map_reqs state name e in
  let rsts sts = List.concat (List.map (rst state name `Private dfltvs) sts) in
  let sr = src_of_expr e in
  let seq () = state.fresh_bid () in
  match e with

  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_match_ctor _
  | EXPR_match_case _
  | EXPR_case_arg _
  | EXPR_void _
  | EXPR_arrow _
  | EXPR_longarrow _
  | EXPR_superscript _
  | EXPR_as _
  | EXPR_product _
  | EXPR_sum _
  | EXPR_andlist _
  | EXPR_orlist _
  | EXPR_ellipsis _
  | EXPR_setunion  _
  | EXPR_intersect _
  | EXPR_isin _
  | EXPR_setintersection _
    ->
    clierr sr ("[rex] Unexpected " ^ string_of_expr e)


  (* This term works like: EXPR_ctor_arg (sr, (Some, Some 1)) -> 1, that is,
   * it returns the argument of the given constructor in the expression,
   * which expression must be precisely that constructor applied to an argument
   *)
  | EXPR_ctor_arg (sr,(qn,e)) -> 
    let l1,x1 = rex e in 
    l1,EXPR_ctor_arg (sr,(qn,x1))

  | EXPR_type_match _ -> [],e

  | EXPR_noexpand (_,e) -> rex e
  | EXPR_name (sr,name,_) -> [],e

  | EXPR_deref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_deref (sr,x1)

  | EXPR_ref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_ref (sr,x1)

  | EXPR_likely (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_likely (sr,x1)

  | EXPR_unlikely (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_unlikely (sr,x1)

  | EXPR_new (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_new (sr,x1)

  | EXPR_suffix _ -> [],e  (* ?? *)
  | EXPR_callback _ -> [],e  (* ?? *)

  | EXPR_index (_,_,_) -> [],e

  | EXPR_lookup (sr,(e,id,ts)) ->
    let l1,x1 = rex e in
    l1, EXPR_lookup (sr,(x1,id,ts))

  | EXPR_case_tag _ -> [],e
  | EXPR_typed_case _ -> [],e
  | EXPR_literal _ -> [],e

  | EXPR_expr _ -> [],e

  | EXPR_vsprintf (sr,s) ->
    let ix = seq () in
    let id = "_fmt_" ^ string_of_bid ix in
    let str = TYP_name (sr,"string",[]) in
    let fmt,its = Flx_cformat.types_of_cformat_string sr s in
    let args = catmap ","
      (fun (i,s) -> match s with
      | TYP_name (_,"string",[]) -> "$" ^ si i ^ ".data()"
      | _ ->  "$" ^ si i
      )
      its
    in
    let ss = Flx_print.string_of_string fmt in
    let fs = "flx::rtl::strutil::flx_asprintf("^ss^","^args^")" in
    let rreq = RREQ_atom (Package_req (CS_str "flx_strutil")) in
    let props, dcls, req = mkreqs sr rreq in
    assert (props = []);
    let ts =
      let n = List.fold_left (fun n (i,_) -> max n i) 0 its in
      let a = Array.make n TYP_none in
      List.iter
      (fun (i,s) ->
        if a.(i-1) = TYP_none then a.(i-1) <-s
        else if a.(i-1) = s then ()
        else clierr sr ("Conflicting types for argument " ^ si i)
      )
      its
      ;
      for i = 1 to n do
        if a.(i-1) = TYP_none then
          clierr sr ("Missing format for argument " ^ si i)
      done
      ;
      Array.to_list a
    in
    let f = DCL_fun([], ts, str, CS_str_template fs, map_reqs sr req, "primary") in
    let x = EXPR_index (sr,id,ix) in
    Dcl (sr, id, Some ix, `Private, dfltvs, f) :: dcls, x

  | EXPR_cond (sr,(e,b1,b2)) ->
     rex
     (
       EXPR_match
       (
         sr,
         (
           e,
           [
             PAT_const_ctor (sr,`AST_case_tag (sr,1)),b1; (* true *)
             PAT_any sr,b2 (* false *)
           ]
         )
       )
     )

  (* we have to lift lambdas out of typeof exprs,
     even though they're never called,
     so the typing works correctly
  *)
  | EXPR_typeof (sr,e') ->
    let l1,x1 = rex e' in
    l1, EXPR_typeof (sr,(x1))

  | EXPR_get_n (sr,(n,e')) ->
    let l1,x1 = rex e' in
    l1, EXPR_get_n (sr,(n,x1))

  | EXPR_get_named_variable (sr,(n,e')) ->
    let l1,x1 = rex e' in
    l1, EXPR_get_named_variable (sr,(n,x1))

  | EXPR_case_index (sr,e) ->
    let l,x = rex e in
    l,EXPR_case_index (sr,x)

  | EXPR_apply (sr,(fn,arg)) ->
    let l1,x1 = rex fn in
    let l2,x2 = rex arg in
    l1 @ l2, EXPR_apply (sr,(x1,x2))

  | EXPR_map (sr,fn,arg) ->
    let l1,x1 = rex fn in
    let l2,x2 = rex arg in
    l1 @ l2, EXPR_map (sr,x1,x2)

  | EXPR_tuple (sr,t) ->
    let lss,xs = List.split (List.map rex t) in
    List.concat lss,EXPR_tuple (sr,xs)

  | EXPR_record (sr,es) ->
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    List.concat lss,EXPR_record (sr, List.combine ss xs)

  | EXPR_record_type _ -> assert false

  | EXPR_variant (sr,(s,e)) ->
    let l,x = rex e in
    l,EXPR_variant (sr,(s,x))

  | EXPR_variant_type _ -> assert false

  | EXPR_arrayof (sr,t) ->
    let lss,xs = List.split (List.map rex t) in
    List.concat lss,EXPR_arrayof(sr,xs)

  | EXPR_lambda (sr,(vs,pps,ret,sts)) ->
    let kind = `InlineFunction in
    let n = seq() in
    let name' = "_lam_" ^ string_of_bid n in
    let access = `Private in
    let sts = rst
      state
      name
      access
      dfltvs
      (mkcurry seq sr name' vs pps (ret,None) kind sts [`Generated "lambda"])
    in
    if List.length pps = 0 then syserr sr "[rex] Lambda with no arguments?" else
    let t = type_of_argtypes (List.map (fun(x,y,z,d)->z) (fst (List.hd pps))) in
    let e =
      EXPR_suffix
      (
        sr,
        (
          `AST_name (sr,name',[]), t
        )
      )
    in
    sts,e

  | EXPR_dot (sr,(a,b)) ->
    let l1,x1 = rex a in
    let l2,x2 = rex b in
    l1@l2 , EXPR_dot (sr,(x1,x2))

  | EXPR_coercion (sr,(e,t)) ->
    let l1,x1 = rex e in
    l1, EXPR_coercion (sr,(x1,t))

  | EXPR_letin (sr,(pat,e1,e2)) ->
    rex (EXPR_match (sr,(e1,[pat,e2])))

  (* MATCH HANDLING NEEDS TO BE REWORKED, THE SWITCHING SHOULD BE
     DELAYED TO ALLOW TYPE BASED OPTIMISATION WHERE THE TOP
     LEVEL MATCH ON A UNION CAN USE A SWITCH.

     ALSO, TO ALLOW MULTIPLE PATTERNS WITH ONE HANDLER,
     GIVE THE HANDLER PARAMETERS, AND HAVE THE TOP LEVEL
     MATCH HANDLERS FOR EACH CASE FOR THAT CODE CALL IT:

     eg:

     match x with | A x | B x => x endmatch
  *)


  | EXPR_match (sr,(e,pss)) ->
    if List.length pss = 0 then clierr sr "Empty Pattern";

    (* step 1: evaluate e *)
    let d,x = rex e in
    let match_function_index = seq() in
    let match_var_index = seq() in
    (*
    print_endline ("Match function index = " ^ string_of_bid match_function_index );
    print_endline ("Match variable index = " ^ string_of_bid match_var_index );
    *)

    let match_var_name = name ^ "_mv_" ^ string_of_bid match_function_index in
    let match_function_id =
      name ^ "_mf_" ^ string_of_bid match_function_index
    in
    let match_function =
      EXPR_index (sr,match_function_id,match_function_index)
    in
    let match_seq = ref (seq()) in

    let expr_src = src_of_expr e in

    (* WOE. The expr may contain a lambda, which stuffs up
       bind_expression which is called by bind_type ..
    *)
    let evl =
      [
        Dcl (
          expr_src,
          match_var_name,
          Some match_var_index,
          `Private,
          dfltvs,
          DCL_value (TYP_typeof x, `Val));
        Exe (expr_src,EXE_iinit ((match_var_name,match_var_index),x))
      ]
    in
    let pats,_ = List.split pss in
    Flx_pat.validate_patterns pats
    ;
    let ematch_seq = seq() in
    (*
    let end_match_label = "_em" ^ string_of_bid ematch_seq in
    *)
    let matches = ref [Exe (generated,EXE_comment "begin match")] in
    let match_caseno = ref 1 in
    let iswild = ref false in
    List.iter
    (fun (pat,e) ->
      let n1 = !match_seq in
      let n2 = seq() in
      let mh_idx = seq () in
      let mc_idx = seq () in
      if !iswild then
        print_endline "WARNING, matches after wildcard ignored"
      else begin
        iswild := is_universal pat;
        let patsrc = src_of_pat pat in
        let expr_src = src_of_expr e in
        let match_checker_id = name ^ "_mc" ^ string_of_bid n1 in
        let match_handler_id = name ^ "_mh" ^ string_of_bid n1 in
        let match_checker = EXPR_index (patsrc,match_checker_id,mc_idx) in
        let match_handler = EXPR_index (expr_src,match_handler_id,mh_idx) in
        (*
        print_endline ("Match checker index = " ^ string_of_bid mc_idx);
        print_endline ("Match handler index = " ^ string_of_bid mh_idx);
        *)
        let sts,result_expr = rex e in
        let body =
          sts @
          [Exe (expr_src,EXE_fun_return (result_expr))]
        in
        matches := !matches @
        [
          Dcl (patsrc,match_checker_id,Some mc_idx,`Private,dfltvs,
          DCL_match_check (pat,(match_var_name,match_var_index)));
          Dcl
          (
            expr_src,
            match_handler_id,Some mh_idx,
            `Private,
            dfltvs,
            DCL_match_handler
            (
              pat,
              (match_var_name,match_var_index),
              body
            )
          )
        ]
        @
        [
        Exe (patsrc,EXE_comment ("match case " ^ si !match_caseno^":" ^ string_of_pattern pat))
        ]
        @
        (
        (* we dont need a label for the first case *)
        if !match_caseno <> 1 then
        [
        Exe (patsrc,EXE_label ("_ml" ^ string_of_bid n1))
        ]
        else []
        )
        @

        (* This code checks the match condition, it can be
           elided if the match is wildcard
        *)
        (if !iswild then [] else
        [
          Exe
          (
            patsrc,
            EXE_ifgoto
            (
              EXPR_apply
              (
                patsrc,
                (
                  EXPR_name (patsrc,"lnot",[]),
                  EXPR_apply
                  (
                    patsrc,
                    (
                      match_checker,
                      EXPR_tuple (patsrc,[])
                    )
                  )
                )
              ),
              "_ml" ^ string_of_bid n2
            )
          )
        ]
        )
        @
        [
        Exe
        (
          patsrc,
          EXE_fun_return
          (
            EXPR_apply
            (
              patsrc,
              (
                match_handler,
                EXPR_tuple (patsrc,[])
              )
            )
          )
        )
        (*
        ;
        Exe (patsrc,EXE_goto end_match_label)
        *)
        ]
        ;
        incr match_caseno;
        match_seq := n2
      end
    )
    pss
    ;
    let failure_label = "_ml" ^ string_of_bid !match_seq in

    let match_function_body =
    d
    @
    evl
    @
    !matches
    @
    (if !iswild then [] else
      let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
      [
        Exe (sr, EXE_comment "match failure");
        Exe (sr, EXE_label failure_label);
        Exe (sr, EXE_noreturn_code (
          CS_str ("      FLX_MATCH_FAILURE(" ^ s ^ ");\n")));
      ]
    )
    in
    [
      Dcl
      (
        sr,
        match_function_id,Some match_function_index,
        `Private,
        dfltvs,
        DCL_function
        (
          ([],None),
          TYP_none,
          [`Inline;`Generated "desugar:match fun"],
          match_function_body
        )
      )
    ]
    ,
    EXPR_apply
    (
      sr,
      (
        match_function,
        EXPR_tuple (sr,[])
      )
    )

(* remove blocks *)
(* parent vs is containing module vs .. only for modules *)

and merge_vs
  (vs1,{raw_type_constraint=con1; raw_typeclass_reqs=rtcr1})
  (vs2,{raw_type_constraint=con2; raw_typeclass_reqs=rtcr2})
:vs_list_t =
  let t =
    match con1,con2 with
    | TYP_tuple[],TYP_tuple[] -> TYP_tuple[]
    | TYP_tuple[],b -> b
    | a,TYP_tuple[] -> a
    | TYP_intersect a, TYP_intersect b -> TYP_intersect (a@b)
    | TYP_intersect a, b -> TYP_intersect (a @[b])
    | a,TYP_intersect b -> TYP_intersect (a::b)
    | a,b -> TYP_intersect [a;b]
  and
    rtcr = uniq_list (rtcr1 @ rtcr2)
  in
  vs1 @ vs2,
  { raw_type_constraint=t; raw_typeclass_reqs=rtcr}

and gen_call_init sr name' =
  let mname = EXPR_name (sr,name',[]) in
  let pname = `AST_lookup (sr, (mname, "_init_", [])) in
  let sname = EXPR_suffix (sr, (pname, TYP_tuple [])) in
  let unitt = EXPR_tuple (generated,[]) in
  Exe
  (
    sr,
    EXE_call (sname, unitt)
  )

(* Here, "name" is the name of the containing parent, eg if the statement being
 * processed is in a module X, then name will be "X". The name always exists.
 * If it is a top level thing, the name is a munged version of the program filename.
 *
 * The idea here is: if you write "requires fred" in a module X, then "_rqs_X"
 * will be an empty insertion with requirement "fred". We then make every symbol
 * in X depend on "_rqs_X" and thus propagate the dependency on "fred".
 *
 * If a module Y is nested in a module X, then "_rqs_Y" will have a requirement
 * on "_rqs_X", so the symbols in a nested module will inherit any requirements
 * of the parent of the module which is their parent.
 *
 * Adding the dependency of Y on X is called making a bridge.
 *
 * BUG: TO BE FIXED: The top level "module" never gets an insertion!
 * So the bridges built to that module fail. This only happens if
 * we're processing a nested scope for which a bridge is generated,
 * some some of our regression tests pass, but any with a function in them
 * fail (since function have scopes and therefore generate bridges)
 *
 * The root rqs thing has to be manually inserted by the top level caller
 * of desugar, which is the one inventing the top level module name from
 * the program
 *)
and rst state name access (parent_vs:vs_list_t) (st:statement_t) : asm_t list =
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
  let bridge n sr : asm_t =
(*
    print_endline ("Making bridge for " ^ n ^ " -> " ^ name ^ Flx_print.string_of_vs parent_vs);
*)
    let ts = List.map (fun (s,_)-> TYP_name (sr,s,[])) (fst parent_vs) in
    let us = NREQ_atom (`AST_name (sr, "_rqs_" ^ name, ts)) in
    let body = DCL_insert (CS_str "", `Body, us) in
    Dcl (sr, "_rqs_" ^ n, None, `Public, dfltvs, body)
  in

  (* rename _root requirements *)
  let map_reqs sr (reqs : named_req_expr_t) : named_req_expr_t =
    NREQ_and (NREQ_atom (rqname' sr), reqs)
  in

  (* name literal requirements *)
  let mkprop sr s = match s with
    | "needs_gc" -> `Uses_gc
    | "needs_ptf" -> `Requires_ptf
    | "pure" -> `Pure
    | "generator" -> `Generator
    | "virtual" -> `Virtual
    | x -> clierr sr ("Unknown property " ^ x)
  in
  let mkreqs sr (rqs :raw_req_expr_t) : property_t list * asm_t list * named_req_expr_t =
    let ix = None in
    let props = ref [] in
    let decls = ref [] in
    let mkreq s kind =
      let n = state.fresh_bid () in
      let n = "_req_" ^ string_of_bid n in
      let dcl = Dcl (sr,n,ix,access,dfltvs,DCL_insert (s,kind,NREQ_true)) in
      decls := dcl :: !decls;
      NREQ_atom (`AST_name (sr,n,parent_ts sr))
    in
    let rec aux rqs = match rqs with
    | RREQ_or (a,b) -> NREQ_or (aux a, aux b)
    | RREQ_and (a,b) -> NREQ_and (aux a, aux b)
    | RREQ_true -> NREQ_true
    | RREQ_false -> NREQ_false
    | RREQ_atom x -> match x with
      | Body_req s -> mkreq s `Body
      | Header_req s -> mkreq s `Header
      | Package_req s -> mkreq s `Package

      | Named_req n -> NREQ_atom n

      | Property_req "generator" ->
        props := `Generator :: !props;
        NREQ_true

      | Property_req "virtual" ->
        props := `Virtual:: !props;
        NREQ_true

      | Property_req "lvalue" ->
        props := `Lvalue:: !props;
        NREQ_true

      | Property_req s ->
        props := mkprop sr s :: !props;
        NREQ_true
    in
    let r = aux rqs in
    !props, !decls, r
  in

  (* rename _root headers *)
  let map_req n = if n = "_root" then "_rqs_" ^ name else n in

  let rex x = rex mkreqs map_reqs state name x in
  let rsts name vs access sts = List.concat (List.map
    (rst state name access vs) sts)
  in
  let seq () = state.fresh_bid () in
  (* add _root headers and bodies as requirements for all
    bindings defined in this entity
  *)
  match st with
  | STMT_seq _ -> assert false
  | STMT_private (sr,st) -> rst state name `Private parent_vs st
  | STMT_include (sr,inspec) ->
      state.include_file_cache <- inspec :: state.include_file_cache;
      []
  | STMT_label (sr,s) -> [Exe (sr,EXE_label s)]
  | STMT_proc_return sr -> [Exe (sr,EXE_proc_return)]
  | STMT_halt (sr,s) -> [Exe (sr,EXE_halt s)]
  | STMT_trace (sr,v,s) -> [Exe (sr,EXE_trace (v,s))]
  | STMT_goto (sr,s) -> [Exe (sr,EXE_goto s)]
  | STMT_open (sr,(vs,aux),name) ->
    let vs = List.map (fun (n,t)->let i = seq() in n,i,t) vs in
    [Dir (sr,DIR_open ((vs,aux),name))]
  | STMT_inject_module (sr,name) -> [Dir (sr,DIR_inject_module name)]
  | STMT_use (sr,n,qn) -> [Dir (sr,DIR_use (n,qn))]
  | STMT_comment (sr,s) -> [Exe (sr,EXE_comment s)]

  (* objects *)
  | STMT_export_python_fun (sr,name,cpp_name) ->
    [Iface (sr, IFACE_export_python_fun (name, cpp_name))]

  | STMT_export_fun (sr,name,cpp_name) ->
    [Iface (sr, IFACE_export_fun (name, cpp_name))]

  | STMT_export_type (sr,typ,cpp_name) ->
    [Iface (sr, IFACE_export_type (typ, cpp_name))]

  | STMT_var_decl (sr,name,vs,typ,expr) ->
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Var));
        Exe (sr,EXE_init (name,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (
          sr,name,None,access,vs,DCL_value (TYP_typeof x, `Var));
        Exe (sr,EXE_init (name,x))]
    | Some t,None ->
        [Dcl (sr,name,None,access,vs,DCL_value (t, `Var))]
    | None,None -> failwith "Expected variable to have type or initialiser"
    end

  | STMT_val_decl (sr,name,vs,typ,expr) ->
    begin match typ,expr with
    | Some t, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (t, `Val));
        Exe (sr,EXE_init (name,x))]
    | None, Some e ->
      let d,x = rex e in
      d @ [
        Dcl (sr,name,None,access,vs,DCL_value (TYP_typeof x, `Val));
        Exe (sr,EXE_init (name,x))]
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
    let props,dcls, reqs = mkreqs sr reqs in
    Dcl (sr,name,None,access,vs,DCL_const (props,typ,s, map_reqs sr reqs))
    :: dcls

  (* types *)
  | STMT_abs_decl (sr,name,vs,quals,s, reqs) ->
    let props,dcls, reqs = mkreqs sr reqs in
    Dcl (sr,name,None,access,vs,DCL_abs (quals,s,map_reqs sr reqs))
    :: dcls

  | STMT_newtype (sr,name,vs,t) ->
    [Dcl (sr,name,None,access,vs,DCL_newtype t)]

  | STMT_union (sr,name, vs, components) -> [Dcl (sr,name,None,access,vs,DCL_union (components))]
  | STMT_struct (sr,name, vs, components) ->  [Dcl (sr,name,None,access,vs,DCL_struct (components))]
  | STMT_cstruct (sr,name, vs, components, reqs) ->  
    let props,dcls, reqs = mkreqs sr reqs in
    Dcl (sr,name,None,access,vs,DCL_cstruct (components, map_reqs sr reqs)) :: dcls

  | STMT_typeclass (sr,name, vs, sts) ->
    let asms = rsts name (merge_vs parent_vs vs) `Public sts in
    let asms = bridge name sr :: asms in
    [ Dcl (sr,name,None,access,vs, DCL_typeclass asms) ]

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

  | STMT_type_alias (sr,name,vs,typ) -> [Dcl (sr,name,None,access,vs,DCL_type_alias (typ))]
  | STMT_inherit (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit qn)]
  | STMT_inherit_fun (sr,name,vs,qn) -> [Dcl (sr,name,None,access,vs,DCL_inherit_fun qn)]

  | STMT_curry (sr,name',vs,pps,ret,kind,sts) ->
    rst state name access parent_vs (mkcurry seq sr name' vs pps ret kind sts [])

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
      let vs',params = fix_params sr seq params in
      let vs = merge_vs vs (vs',dfltvs_aux)  in
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
    let props, dcls, reqs = mkreqs sr reqs in
    (* hackery *)
    let vs,args = List.fold_left begin fun (vs,args) arg ->
      match arg with
      | TYP_apply (TYP_name (_,"excl",[]), TYP_name (sr,name,[])) ->
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
      DCL_fun (props, List.rev args, result, code, map_reqs sr reqs, prec))
    :: dcls

  | STMT_callback_decl (sr,name',args,result,reqs) ->
    let props, dcls, reqs = mkreqs sr reqs in
    Dcl (sr,name',None,access,dfltvs,
      DCL_callback (props,args,result,map_reqs sr reqs))
    :: dcls

  (* misc *)
  | STMT_untyped_module (sr,name', vs', sts) ->
    let asms = rsts name' (merge_vs parent_vs vs') `Public sts in
    let asms = bridge name' sr :: asms in
    let mdcl =
      [ Dcl (sr,name',None,access,vs', DCL_module asms) ]
    in
    (* HACK !!!! Actually, it's wrong: there are no polymorphic modules
       or polymorphic variables .. *)
    if vs' = dfltvs then gen_call_init sr name' :: mdcl else mdcl

  | STMT_insert (sr,name',vs,s,kind,reqs) ->
    let props, dcls, reqs = mkreqs sr reqs in
    (* SPECIAL case: insertion requires insertion use filo order *)
    dcls @ [
      Dcl (sr,map_req name',None,access,vs,DCL_insert (s, kind, map_reqs sr reqs))
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

  | STMT_assign (sr,fid,l,r) ->
    let rec aux (l,t) r =
      match l with
      | `Expr (sr,e) ->
        begin match e with
        | EXPR_tuple (_,ls) ->
          let n = seq() in
          let vn = "_" ^ string_of_bid n in
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
          let vn = "_" ^ string_of_bid n in
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
    if List.length pss = 0 then clierr sr "Empty Pattern";

    (* step 1: evaluate e *)
    let d,x = rex e in
    let match_index : bid_t = seq () in

    let match_var_name = name^ "_mv_" ^ string_of_bid match_index in
    let match_id = name^ "_mf_" ^ string_of_bid match_index in
    let end_match_label = "_em" ^ string_of_bid match_index in

    let expr_src = src_of_expr e in

    (* WOE. The expr may contain a lambda, which stuffs up
       bind_expression which is called by bind_type ..
    *)
    let evl =
      [
        Dcl (
          expr_src,
          match_var_name,
          Some match_index,
          `Private,
          dfltvs,
          DCL_value (TYP_typeof x, `Val));
        Exe (expr_src,EXE_iinit ((match_var_name,match_index),x))
      ]
    in
    let pats,_ = List.split pss in
    Flx_pat.validate_patterns pats
    ;
    let matches = ref [Exe (generated,EXE_comment "begin match")] in
    let match_caseno = ref 1 in
    let iswild = ref false in
    let n2 = ref (seq()) in (* the next case *)
    let need_final_label = ref false in
    List.iter
    (fun (pat,sts) ->
      let n1 = !n2 in (* this case *)
      n2 := seq(); (* the next case *)
      iswild := is_universal pat;
      let patsrc = src_of_pat pat in
      let match_checker_id = name ^ "_mc" ^ string_of_bid n1 in
      let match_checker = EXPR_index (patsrc,match_checker_id,n1) in
      let vars = Hashtbl.create 97 in
      Flx_mbind.get_pattern_vars vars pat [];
          (*
          print_endline ("PATTERN IS " ^ string_of_pattern pat ^ ", VARIABLE=" ^ mvname);
          print_endline "VARIABLES ARE";
          Hashtbl.iter (fun vname (sr,extractor) ->
            let component =
              Flx_mbind.gen_extractor extractor (EXPR_index (sr,mvname,match_var_index))
            in
            print_endline ("  " ^ vname ^ " := " ^ string_of_expr component);
          ) vars;
          *)

      let new_sts = ref sts in
      Hashtbl.iter
          (fun vname (sr,extractor) ->
            let component =
              Flx_mbind.gen_extractor extractor
              (EXPR_index (sr,match_var_name,match_index))
            in
            let dcl = STMT_val_decl (sr,vname,dfltvs,None,Some component) in
            new_sts := dcl :: !new_sts;
          )
      vars;
      let body = 
(*
        rsts name parent_vs access [block sr !new_sts]
*)
        rsts name parent_vs access !new_sts
      in
      (* hacky attempt to elide useless jumps at the end of each case
       * doesn't account for non-returning calls, trailing comments or non
       * executable statements, or complicated statements (such as nested matches)
       *)
      let returns = 
        let rec aux body =
          match List.rev (List.filter (fun x -> match x with Exe x -> true | _ -> false) body) with 
          | Exe (_,h) ::_ -> 
            begin match h with
            | EXE_noreturn_code _ 
            | EXE_goto _ 
            | EXE_jump _ 
            | EXE_loop _ 
            | EXE_fun_return _ 
            | EXE_proc_return _ 
            | EXE_halt _ 
              -> true
            | _ -> false
            end
          | _ -> false
        in aux body
      in
      if not returns then need_final_label := true;
      matches := !matches @
        [
          Dcl (patsrc,match_checker_id,Some n1,`Private,dfltvs,
          DCL_match_check (pat,(match_var_name,match_index)));
        ]
        @
        [
        Exe (patsrc,EXE_comment ("match case " ^ si !match_caseno^":" ^ string_of_pattern pat))
        ]
        @
        (if !iswild then [] else
        [
          Exe
          (
            patsrc,
            EXE_ifgoto
            (
              EXPR_apply
              (
                patsrc,
                (
                  EXPR_name (patsrc,"lnot",[]),
                  EXPR_apply
                  (
                    patsrc,
                    (
                      match_checker,
                      EXPR_tuple (patsrc,[])
                    )
                  )
                )
              ),
              "_ml" ^ string_of_bid (!n2)
            )
          )
        ]
        )
        @
        body
        @
        (if not returns then [Exe (patsrc,EXE_goto end_match_label) ] else [])
        @
        [
        Exe (patsrc,EXE_label ("_ml" ^ string_of_bid (!n2)))
        ]
      ;
      incr match_caseno
    )
    pss
    ;

    let match_function_body =
    d
    @
    evl
    @
    !matches
    @
    (if !iswild then [] else
      let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
      let s = Flx_print.string_of_string f ^"," ^
        si sl ^ "," ^ si sc ^ "," ^
        si el ^ "," ^ si ec
      in
      [
        Exe (sr,EXE_comment "match failure");
        Exe (sr,EXE_noreturn_code (CS_str
          ("      FLX_MATCH_FAILURE(" ^ s ^ ");\n")));
      ]
    )
    @
    (if !need_final_label then [ Exe (sr,EXE_label end_match_label) ] else [])
    in
    match_function_body


  (* split into multiple declarations *)

  | STMT_user_statement _
  | STMT_ctypes _
  | STMT_ifdo _
  | STMT_ifreturn _
  | STMT_macro_val _
  | STMT_macro_vfor _
  | STMT_scheme_string _
    -> assert false

(** Construct a desugar state value needed for desugaring. *)
let make_desugar_state name fresh_bid = {
  name = name;
  fresh_bid = fresh_bid;
  macro_state = Flx_macro.make_macro_state name;
  include_file_cache = [];
}

(** Desugar all the statements in a compilation unit. *)
let rec desugar_stmts state curpath stmts =
  let stmts = match stmts with
    | [] -> [STMT_nop (generated, "empty module")]
    | _ -> stmts
  in
  let stmts = Flx_macro.expand_macros state.macro_state stmts in

  let asms = List.concat (List.map
    (rst state state.name `Public dfltvs)
    stmts)
  in

  (* Clear the include file cache. *)
  let include_files = state.include_file_cache in
  state.include_file_cache <- [];

  include_files, asms

(* We're changing the implementation model now, to not recursively
   desugar include files. Instead, just return the desugared statements
   and a list of include files.
*)

(*
  (* Bind all the asms in reverse order. *)
  let asms =
    List.fold_left begin fun asms file ->
      let curpath, stmts = Flx_colns.include_file state.syms curpath file in
      desugar_stmts state curpath stmts :: asms
    end [asms] include_files
  in

  (* And finally, concatenate all the asms together. *)
  List.concat asms
*)
 

(** Desugar a statement. *)
let desugar_statement state handle_asm init stmt =
  (* First we must expand all the macros in the statement *)
  Flx_macro.expand_macros_in_statement
    state.macro_state
    begin fun init stmt ->
      (* For each macro-expanded statement, desugar it into a series of
       * assemblies *)
      let asms = rst state state.name `Public dfltvs stmt in

      (* Finally, call the fold function over the assemblies *)
      List.fold_left handle_asm init asms
    end
    init
    stmt
