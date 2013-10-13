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

type desugar_state_t = {
  name: string;
  macro_state: Flx_macro.macro_state_t;
  fresh_bid: unit -> bid_t;
  mutable include_file_cache: string list;
}

(** Construct a desugar state value needed for desugaring. *)
let make_desugar_state name seq = {
  name = name;
  fresh_bid = (fun () -> let n = !seq in incr seq; n);
  macro_state = Flx_macro.make_macro_state name seq;
  include_file_cache = [];
}


let generated = Flx_srcref.make_dummy "[flx_desugar_expr] generated"

let block sr body :statement_t =
  let e = EXPR_lambda (sr,(`GeneratedInlineProcedure,dfltvs,[[],None],TYP_void sr,body)) in
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

let cal_props = function
  | `CFunction -> `Cfun::[]
  | `InlineFunction -> `Inline::[]
  | `GeneratedInlineProcedure-> `GeneratedInline::[]
  | `GeneratedInlineFunction -> `GeneratedInline::[]
  | `NoInlineFunction -> `NoInline::[]
  | `Ctor -> `Ctor::[]
  | `Generator -> (* `NoInline:: *) `Generator::[]
  | `GeneratorMethod -> (* `NoInline:: *) `Generator::[]
  | `Virtual -> `Virtual::[]
  | _ -> []

let mkcurry seq sr (name:string) (vs:vs_list_t) (args:params_t list) return_type (kind:funkind_t) body props =
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

  let isobject = kind = `Object in
  let rec aux (args:params_t list) (vs:vs_list_t) props =
    let n = List.length args in
    let synthname n =
      if n = arity
      then name
      else name^"'" ^ si (arity-n+1)
    in
    match args with
    | [] ->
        begin match return_type with
        | TYP_void _ ->
          let body = 
            let reved = List.rev body in
            List.rev (STMT_label (sr,"_endof_" ^ synthname n) ::
              match reved with
              | STMT_proc_return _ :: _ ->  reved
              | _ -> STMT_proc_return sr :: reved
            )
          in
          STMT_function (sr, synthname n, vs, ([],None), (return_type,postcondition), props, body)
        | _ ->
          (* allow functions with no arguments now .. *)
          begin match body with
          | [STMT_fun_return (_,e)] ->
            let rt = match return_type with
            | TYP_none -> None
            | x -> Some x
            in
            STMT_lazy_decl (sr, synthname n, vs, rt, Some e)
          | _ ->
          clierr sr "Function with no arguments"
          end
        end

    | h :: [] -> (* bottom level *)
      if isobject then begin
        (*
        print_endline "Found an object, scanning for methods and bogus returns";
        *)
        let methods = ref [] in
        List.iter (fun st ->
          (*
          print_endline ("Statement " ^ Flx_print.string_of_statement 2 st);
          *)
          match st with
          | STMT_fun_return _ -> clierr sr "FOUND function RETURN in Object";
          | STMT_proc_return _ -> clierr sr "FOUND procedure RETURN in Object";
          | STMT_curry (_,name, vs, pss, (res,traint) , kind, ss) when kind = `Method || kind = `GeneratorMethod->
               methods := name :: !methods;
          | _ -> ()
        )
        body
        ;
        let mkfield s = s,EXPR_name (sr,s,[]) in
        let record = EXPR_record (sr, List.map mkfield (!methods)) in
        let retstatement = STMT_fun_return (sr, record) in
        let object_body = List.rev (retstatement :: List.rev body) in
        STMT_function (sr, synthname n, vs, h, (return_type,postcondition), props, object_body)
      end else 
        let body = 
          match return_type with 
          | TYP_void _  ->
(*
            print_endline ("(args) Name = " ^ name ^ "synthname n = " ^ synthname n);
*)
            let reved = List.rev body in
            List.rev (STMT_label (sr,"_endof_" ^ synthname n) ::
              match reved with
              | STMT_proc_return _ :: _ ->  reved
              | _ -> STMT_proc_return sr :: reved
            )
          | _ -> body
        in
        STMT_function (sr, synthname n, vs, h, (return_type,postcondition), props, body)
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
                `AST_name (sr,synthname (m-1),[]),argt
              )
            )
          )
        ]
      in
        STMT_function (sr, synthname m, vs, h, (rettype t,None), `Generated "curry"::props, body)
   in aux args vs (cal_props kind @ props)

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

let rec rex rst mkreqs map_reqs (state:desugar_state_t) name (e:expr_t) : asm_t list * expr_t =
  let rex e = rex rst mkreqs map_reqs state name e in
(*
  let rsts sts = List.concat (List.map (rst state name `Private dfltvs) sts) in
*)
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
  | EXPR_product _
  | EXPR_sum _
  | EXPR_andlist _
  | EXPR_orlist _
  | EXPR_ellipsis _
  | EXPR_intersect _
  | EXPR_isin _
    ->
    clierr sr ("[rex] Unexpected " ^ string_of_expr e)


  (* This term works like: EXPR_ctor_arg (sr, (Some, Some 1)) -> 1, that is,
   * it returns the argument of the given constructor in the expression,
   * which expression must be precisely that constructor applied to an argument
   *)
  | EXPR_ctor_arg (sr,(qn,e)) -> 
    let l1,x1 = rex e in 
    l1,EXPR_ctor_arg (sr,(qn,x1))

  | EXPR_get_tuple_tail (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_tail (sr,x1)

  | EXPR_get_tuple_head (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_head (sr,x1)

  | EXPR_as_var (sr,(e,name)) ->
    let l1,x1 = rex e in
    let dcl = Dcl (sr, name, None, `Private, dfltvs, DCL_value (TYP_typeof x1,`Var)) in
    let asgn = Exe (sr,EXE_init (name,x1)) in
    l1 @ [dcl] @ [asgn], EXPR_name (sr,name,[])


  | EXPR_as (sr,(e,name)) ->
    let l1,x1 = rex e in
    let dcl = Dcl (sr, name, None, `Private, dfltvs, DCL_value (TYP_typeof x1,`Val)) in
    let asgn = Exe (sr,EXE_init (name,x1)) in
    l1 @ [dcl] @ [asgn], EXPR_name (sr,name,[])


  | EXPR_type_match _ -> [],e

  | EXPR_noexpand (_,e) -> rex e
  | EXPR_name (sr,name,_) -> [],e

  | EXPR_deref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_deref (sr,x1)

  | EXPR_ref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_ref (sr,x1)

  | EXPR_not (sr,e) ->
    let l1, x1 = rex e in
    l1, EXPR_not (sr,x1)

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

  | EXPR_range_check (sr, mi, v, mx) ->
    let l1,x1 = rex mi in
    let l2,x2 = rex v in 
    let l3,x3 = rex mx in
    l1 @ l2 @ l3, EXPR_range_check (sr,x1, x2, x3)

  | EXPR_index (_,_,_) -> [],e

  | EXPR_lookup (sr,(e,id,ts)) ->
    let l1,x1 = rex e in
    l1, EXPR_lookup (sr,(x1,id,ts))

  | EXPR_case_tag _ -> [],e
  | EXPR_typed_case _ -> [],e
  | EXPR_literal _ -> [],e

  | EXPR_expr _ -> [],e

  | EXPR_interpolate (sr,s) -> 
    let outstr = ref "" in
    let outexprs = ref [] in
    let outexpr = ref "" in
    let n = String.length s in
    let mode = ref `Char in
    let i = ref 0 in
    while !i < n do
      match !mode with
      | `Char  ->
        if s.[!i] <> '$' 
        then
          begin
            outstr := !outstr ^ String.sub s (!i) 1;
            incr i
          end
        else 
          begin
            assert (s.[!i] = '$');
            if !i + 1 < n then 
              begin 
                if s.[!i+1] = '$' then 
                  begin (* $$ *)
                    outstr := !outstr ^ String.sub s (!i) 1;
                    incr i; 
                    incr i
                  end 
                else if s.[!i+1] <> '(' then 
                  begin
                    clierr sr ("In q'" ^ s   ^"' require ( after $ at pos " ^ si (!i))
                  end
                else 
                  begin (* $( *)
                    incr i;
                    assert (s.[!i] = '(');
                    outstr := !outstr ^ "%S";
                    outexpr := "(";
                    mode := `Expr 1;
                    incr i
                  end
              end             
            else
              begin
                clierr sr ("In q'" ^ s   ^"' require ( after $ , got eos")
              end
          end
      | `Expr 0 ->
        outexprs := !outexpr :: !outexprs;
        outexpr := "";
        mode := `Char

      | `Expr k ->
        outexpr := !outexpr ^ String.sub s (!i) 1;
        if s.[!i] = '(' then mode := `Expr (k+1)
        else if s.[!i] = ')' then mode := `Expr (k-1)
        ;
        incr i
    done
    ;
    begin match !mode with
    | `Expr 0 ->
      outexprs := !outexpr :: !outexprs;
      outexpr := "";
      mode := `Char
    | `Expr k ->
      clierr sr ("In q'" ^ s   ^"' require closing ) after $expr , got eos at level " ^ si k)
    | `Char -> ()
    end
    ;
    (* print_endline ("outstr=" ^ !outstr); *)
    let outexprs = List.rev_map 
      (fun x ->  
        let n = String.length x in
        if n < 3 then clierr sr ("in q'" ^ s ^ "', require $(ident)");
        String.sub x 1 (n-2)
      )
      (!outexprs) 
    in
    (* List.iter (fun s -> print_endline ("Expr = " ^ s)) outexprs; *)
    let xs = List.map (fun x -> EXPR_name (sr, x, [])) outexprs in
    let str = EXPR_name (sr, "str",[]) in
    let xs = List.map (fun x -> EXPR_apply (sr, (str,x) )) xs in
    let res = EXPR_apply (sr, (EXPR_vsprintf (sr, !outstr), EXPR_tuple (sr,xs)))  in
    rex res

  | EXPR_vsprintf (sr,s) ->
    let ix = seq () in
    let id = "_fmt_" ^ string_of_bid ix in
    let str = TYP_name (sr,"string",[]) in
    let fmt,its = Flx_cformat.types_of_cformat_string sr s in
    let args = catmap ","
      (fun (i,s) -> match s with
      | TYP_name (_,"string",[]) -> "$" ^ si i ^ ".c_str()"
      | _ ->  "$" ^ si i
      )
      its
    in
    let ss = Flx_print.string_of_string fmt in
    let fs = "::flx::rtl::strutil::flx_asprintf("^ss^","^args^")" in
    let rreq = RREQ_atom (Package_req (CS.Str "flx_strutil")) in
    let _,props, dcls, req = mkreqs sr rreq in
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
    let f = DCL_fun (
      [],
      ts,
      str,
      CS.Str_template fs,
      map_reqs sr req,
      "primary")
    in
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

  | EXPR_tuple_cons (sr,eh,et) ->
    let l1,x1 = rex eh in
    let l2,x2 = rex et in
    l1 @ l2, EXPR_tuple_cons (sr,x1,x2)


  | EXPR_record (sr,es) ->
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    List.concat lss,EXPR_record (sr, List.combine ss xs)

  | EXPR_extension (sr,es,e) -> 
    let lss,xs = List.split (List.map rex es) in
    let l,x = rex e in
    l @ List.concat lss,EXPR_extension (sr, xs, x)

  | EXPR_record_type _ -> assert false

  | EXPR_variant (sr,(s,e)) ->
    let l,x = rex e in
    l,EXPR_variant (sr,(s,x))

  | EXPR_variant_type _ -> assert false

  | EXPR_arrayof (sr,t) ->
    let lss,xs = List.split (List.map rex t) in
    List.concat lss,EXPR_arrayof(sr,xs)

  | EXPR_lambda (sr,(kind,vs,pps,ret,sts)) ->
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
    Flx_match.gen_match rex seq name sr e pss

(* remove blocks *)
(* parent vs is containing module vs .. only for modules *)

