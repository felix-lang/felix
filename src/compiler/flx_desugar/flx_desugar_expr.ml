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
open Flx_curry
open Flx_bid

module CS = Flx_code_spec

type desugar_state_t = {
  name: string;
  macro_state: Flx_macro.macro_state_t;
  fresh_bid: unit -> bid_t;
  mutable include_file_cache: (Flx_srcref.t * string) list;
}

(** Construct a desugar state value needed for desugaring. *)
let make_desugar_state name seq = {
  name = name;
  fresh_bid = (fun () -> let n = !seq in incr seq; n);
  macro_state = Flx_macro.make_macro_state name seq;
  include_file_cache = [];
}


let block sr body :statement_t =
  let e = EXPR_lambda (sr,(`GeneratedInlineProcedure,dfltvs,[Slist [],None],TYP_void sr,body)) in
  STMT_call (sr,e,EXPR_tuple(sr,[]))

(* This function has to lift lambdas out of types, 
  this mainly (exclusively?) applies to TYP_typeof (e) term
*)
let rec rett rex typ sr =
  match typ with

  (* Lift lambdas inside typeof() *)
  | TYP_typeof _ ->
    let decls,use_t = rex (expr_of_typecode sr typ) in
    let typ = typecode_of_expr(use_t) in
    decls,typ

  | _ ->
    [],typ


(* Take one param and try to lift expressions out of the type *)
let rett_fixparam rex (sr,kind,id,typ,expr) = 
  let decls,typ' = rett rex typ sr in    (* try to lift lambdas from type expressions *)
  let param' = (sr,kind, id, typ', expr) in (* rebuild param (with new type) for return *)
  decls,param'
    

(* Iterate over params and lift lambdas out of types. 
   Returns a tuple: (list of declarations, list of params) *)
let rec rett_fixparams rex ps = 
  match ps with
  
  (* Inductive case: destructure head element and fix it *)
  | Satom param ->
    let decls,param' = rett_fixparam rex param in
    decls, Satom param'

  | Slist ps ->
    let rs = List.map (rett_fixparams rex) ps in
    let decls = List.map fst rs in
    let pss = List.map snd rs in
    List.concat decls, Slist pss
      
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

let rec rex rst_with_ret mkreqs map_reqs (state:desugar_state_t) name (e:expr_t) rettype : asm_t list * expr_t =
  let rex_with_ret e rettype = rex rst_with_ret mkreqs map_reqs state name e rettype in
  let rex e = rex_with_ret e rettype in

  let rst st = rst_with_ret name `Private dfltvs rettype st in

  let rsts_with_ret rettype sts = List.concat (List.map (rst_with_ret name `Private dfltvs rettype) sts) in
  let rsts sts = rsts_with_ret rettype sts in

  let sr = src_of_expr e in
  let seq () = state.fresh_bid () in
  match e with

  | EXPR_rptsum_type _
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_match_case _
  | EXPR_case_arg _
  | EXPR_arrow _
  | EXPR_effector _
  | EXPR_ellipsis _
  | EXPR_intersect _
  | EXPR_union _
  | EXPR_isin _
    ->
    clierrx "[flx_desugar/flx_desugar_expr.ml:127: E326] " sr ("[rex] Unexpected " ^ string_of_expr e)


  | EXPR_void (x) -> [], EXPR_void x
  | EXPR_longarrow (sr,x) -> [], EXPR_longarrow (sr,x)

  | EXPR_superscript (sr,(e1,e2)) -> 
    let l1,x1 = rex e1 in
    let l2,x2 = rex e2 in
    l1 @ l2, EXPR_superscript (sr, (x1, x2))
 
  | EXPR_product (sr,ls) ->
    let lss,xs = List.split (List.map rex ls) in
    List.concat lss,EXPR_product (sr,xs)

  | EXPR_sum (sr,ls) ->
    let lss,xs = List.split (List.map rex ls) in
    List.concat lss,EXPR_sum (sr,xs)

  | EXPR_andlist (sr,ls) ->
    let lss,xs = List.split (List.map rex ls) in
    List.concat lss,EXPR_andlist (sr,xs)

  | EXPR_orlist (sr,ls) ->
    let lss,xs = List.split (List.map rex ls) in
    List.concat lss,EXPR_orlist (sr,xs)

  | EXPR_match_ctor (sr,(name,arg)) ->
    let l1,x1 = rex arg in
    l1, EXPR_match_ctor (sr,(name,x1))

  (* lambda lifts out of patterns are suspect: premature evaluation! *)
  | EXPR_match_variant_subtype (sr, (e,t)) ->
    let l1, x1 = rex e in
    l1, EXPR_match_variant_subtype (sr, (x1, t))

  | EXPR_match_ho_ctor (sr,(name,es)) ->
    let lxs = List.map rex es in
    let ls,xs = List.split lxs in
    let ls = List.concat ls in
    ls, EXPR_match_ho_ctor (sr,(name,xs))

  (* lambda lifts out of patterns are suspect: premature evaluation! *)
  | EXPR_match_variant (sr,(name,arg)) ->
    let l1,x1 = rex arg in
    l1, EXPR_match_variant (sr,(name,x1))


  (* This term works like: EXPR_ctor_arg (sr, (Some, Some 1)) -> 1, that is,
   * it returns the argument of the given constructor in the expression,
   * which expression must be precisely that constructor applied to an argument
   *)
  | EXPR_ctor_arg (sr,(qn,e)) -> 
    let l1,x1 = rex e in 
    l1,EXPR_ctor_arg (sr,(qn,x1))

  | EXPR_ho_ctor_arg (sr,(qn,es)) -> 
    let lxs = List.map rex es in
    let ls,xs = List.split lxs in
    let ls = List.concat ls in
    ls,EXPR_ho_ctor_arg (sr,(qn,xs))

  | EXPR_variant_arg (sr,(s,e)) -> 
    let l1,x1 = rex e in 
    l1,EXPR_variant_arg (sr,(s,x1))

  | EXPR_get_tuple_tail (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_tail (sr,x1)

  | EXPR_get_tuple_head (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_head (sr,x1)

  | EXPR_get_tuple_body (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_body (sr,x1)

  | EXPR_get_tuple_last (sr, e) ->
    let l1,x1 = rex e in 
    l1,EXPR_get_tuple_last (sr,x1)


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

  | EXPR_typecase_match (sr,(t, es)) ->
    let ts,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    List.concat lss,
    EXPR_typecase_match (sr, (t, List.combine ts xs))

  | EXPR_type_match _ -> [],e
  | EXPR_subtype_match _ -> [],e

  | EXPR_noexpand (_,e) -> rex e
  | EXPR_name (sr,name,_) -> [],e
  | EXPR_label (sr,name) -> [],e

  | EXPR_deref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_deref (sr,x1)

  | EXPR_ref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_ref (sr,x1)

  | EXPR_rref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_rref (sr,x1)

  | EXPR_wref (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_wref (sr,x1)

  | EXPR_ainj (sr,e,t) ->
    let l1,x1 = rex e in
    l1, EXPR_ainj (sr,x1, t)




  | EXPR_uniq (sr,e) ->
    let l1,x1 = rex e in
    l1, EXPR_uniq (sr,x1)

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
  | EXPR_projection _ -> [],e

  | EXPR_literal _ -> [],e

  | EXPR_expr (sr,sc,t,e) -> 
    let d,x = rex e in
    d , EXPR_expr (sr,sc,t,x)


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
                    clierrx "[flx_desugar/flx_desugar_expr.ml:289: E327] " sr ("In q'" ^ s   ^"' require ( after $ at pos " ^ si (!i))
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
                clierrx "[flx_desugar/flx_desugar_expr.ml:303: E328] " sr ("In q'" ^ s   ^"' require ( after $ , got eos")
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
      clierrx "[flx_desugar/flx_desugar_expr.ml:325: E329] " sr ("In q'" ^ s   ^"' require closing ) after $expr , got eos at level " ^ si k)
    | `Char -> ()
    end
    ;
    (* print_endline ("outstr=" ^ !outstr); *)
    let outexprs = List.rev_map 
      (fun x ->  
        let n = String.length x in
        if n < 3 then clierrx "[flx_desugar/flx_desugar_expr.ml:333: E330] " sr ("in q'" ^ s ^ "', require $(ident)");
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
    let _,_,props, dcls, req = mkreqs sr rreq in
    assert (props = []);
    let ts =
      let n = List.fold_left (fun n (i,_) -> max n i) 0 its in
      let a = Array.make n TYP_none in
      List.iter
      (fun (i,s) ->
        if a.(i-1) = TYP_none then a.(i-1) <-s
        else if a.(i-1) = s then ()
        else clierrx "[flx_desugar/flx_desugar_expr.ml:369: E331] " sr ("Conflicting types for argument " ^ si i)
      )
      its
      ;
      for i = 1 to n do
        if a.(i-1) = TYP_none then
          clierrx "[flx_desugar/flx_desugar_expr.ml:375: E332] " sr ("Missing format for argument " ^ si i)
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

  | EXPR_rptsum_arg (sr,e) ->
    let l,x = rex e in
    l,EXPR_rptsum_arg (sr,x)


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

  | EXPR_tuple_snoc (sr,eh,et) ->
    let l1,x1 = rex eh in
    let l2,x2 = rex et in
    l1 @ l2, EXPR_tuple_snoc (sr,x1,x2)

  | EXPR_record (sr,es) ->
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    List.concat lss,EXPR_record (sr, List.combine ss xs)

  | EXPR_polyrecord(sr,es,e) -> 
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    let l,x = rex e in
    l @ List.concat lss,EXPR_polyrecord(sr, List.combine ss xs, x)

  | EXPR_replace_fields (sr, e, es) ->
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    let l,x = rex e in
    l @ List.concat lss,EXPR_replace_fields (sr, x, List.combine ss xs)

  | EXPR_remove_fields (sr,e,ss) ->
    let l,x = rex e in
    l,EXPR_remove_fields(sr,x,ss)

  | EXPR_extension (sr,es,e) -> 
    let lss,xs = List.split (List.map rex es) in
    let l,x = rex e in
    l @ List.concat lss,EXPR_extension (sr, xs, x)

  | EXPR_pclt_type (sr,a,b) -> [],e (* I'm lazy *)


  | EXPR_record_type (sr,ts) ->
    let to_expr (id,t) = (id, (expr_of_typecode sr t)) in
    let to_type (id,e) = (id, (typecode_of_expr e)) in
    let es = List.map to_expr ts in
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    List.concat lss,EXPR_record_type (sr, (List.map to_type (List.combine ss xs)))

  | EXPR_polyrecord_type (sr,ts,t) ->
    let to_expr (id,t) = (id, (expr_of_typecode sr t)) in
    let to_type (id,e) = (id, (typecode_of_expr e)) in
    let es = List.map to_expr ts in
    let e = expr_of_typecode sr t in
    let ss,es = List.split es in
    let lss,xs = List.split (List.map rex es) in
    let l,x = rex e in
    l @ List.concat 
          lss,
          EXPR_polyrecord_type (sr, 
            (List.map to_type (List.combine ss xs)), 
            (typecode_of_expr x))

  | EXPR_rnprj (sr,name,seq,e) -> 
    let l,x = rex e in
    l,EXPR_rnprj (sr,name,seq,x)


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
    let sts = rst_with_ret
      name
      access
      dfltvs
      ret
      (mkcurry seq sr name' vs pps (ret,None) Flx_typing.flx_unit kind sts [`Generated "lambda"])
    in
    if List.length pps = 0 then syserr sr "[rex] Lambda with no arguments?" else
    let t = typeof_paramspec_t (fst (List.hd pps)) in
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
    begin match t with
    | TYP_none -> rex e (* allow system to coerce expression to unknown type as nop *)
    | _ ->
      let l1,x1 = rex e in
      l1, EXPR_coercion (sr,(x1,t))
    end

  | EXPR_variant_subtype_match_coercion (sr,(e,t)) ->
    let l1,x1 = rex e in
    l1, EXPR_variant_subtype_match_coercion (sr,(x1,t))

  | EXPR_letin (sr,(pat,e1,e2)) -> assert false

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
    Flx_match.gen_match rex_with_ret rsts_with_ret seq name sr e pss rettype

(* remove blocks *)
(* parent vs is containing module vs .. only for modules *)


