open Flx_ast
open Flx_mtypes2
open Flx_print
open Flx_exceptions
open Flx_constfld
open Flx_typing2
open Flx_util

let dyphack (ls : ( 'a * string) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

exception Macro_return

let truthof x = match x with
  | EXPR_typed_case (_,0,TYP_unitsum 2) -> Some false
  | EXPR_typed_case (_,1,TYP_unitsum 2) -> Some true
  | _ -> None

(*
 There are no type macros: use typedef facility.
*)

type macro_t =
 | MVar of expr_t ref
 | MVal of expr_t
 | MVals of expr_t list
 | MExpr of macro_parameter_t list * expr_t
 | MStmt of macro_parameter_t list * statement_t list
 | MName of id_t
 | MNames of id_t list

type macro_dfn_t = id_t * macro_t

type macro_state_t = {
  recursion_limit: int;
  local_prefix: string;
  seq: int ref;
  reachable: bool ref;
  ref_macros: macro_dfn_t list ref;
  macros: macro_dfn_t list;
}

let string_of_statements sts =
  String.concat "\n" (List.map (string_of_statement 1) sts)

let print_mpar (id,t) =
  id ^ ":" ^
  (
    match t with
    | Expr -> "fun"
    | Stmt -> "proc"
    | Ident -> "ident"
  )

let print_mpars x =
  "(" ^ String.concat ", " (List.map print_mpar x) ^ ")"

let print_macro (id,t) =
 match t with
 | MVar v -> "MVar " ^ id ^ " = " ^ string_of_expr !v
 | MVal v -> "MVal " ^ id ^ " = " ^ string_of_expr v
 | MVals vs -> "MVals " ^ id ^ " = " ^ catmap "," string_of_expr vs
 | MExpr (ps,e) ->
   "MExpr " ^ id ^
   print_mpars ps ^
   " = " ^
   string_of_expr e

 | MStmt (ps,sts) ->
   "MStmt " ^ id ^
   print_mpars ps ^
   " = " ^
   String.concat "\n" (List.map (string_of_statement 1) sts)

 | MName id' -> "MName " ^ id ^ " = " ^ id'
 | MNames ids -> "MNames " ^ id ^ " = " ^ cat "," ids

let upper =  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let lower = "abcdefghijklmnopqrstuvwxyz"
let digits = "0123456789"

let idstart = upper ^ lower ^ "_"
let idmore = idstart ^ digits ^ "'"
let quotes =  "\"'`"

let starts_id ch = String.contains idstart ch
let continues_id ch = String.contains idmore ch
let is_quote ch = String.contains quotes ch

let string_of_macro_env x = String.concat "\n" (List.map print_macro x)

(* ident expansion: guarranteed to terminate,
  expansion of x given x -> x is just x
*)
let rec expand_ident sr macros noexpand id =
  try
    if List.mem id noexpand then id else
    match List.assoc id macros with
    | MName id2 -> expand_ident sr macros (id::noexpand) id2
    | _ -> id
  with Not_found -> id

(* Find variable names in patterns so as to protect them *)
let rec get_pattern_vars pat =
  match pat with
  | PAT_name (_,v) -> [v]
  | PAT_as (_,p,v) -> v :: get_pattern_vars p
  | PAT_when (_,p,_) -> get_pattern_vars p
  | PAT_nonconst_ctor (_,_,p) -> get_pattern_vars p
  | PAT_tuple (_,ps) -> List.concat (List.map get_pattern_vars ps)
  | PAT_record (_,ps) -> List.concat(List.map get_pattern_vars (List.map snd ps))
  | _ -> []

let alpha_pat local_prefix seq fast_remap remap expand_expr pat = 
  let ren v = List.assoc v fast_remap in
  let rexp e = expand_expr 50 local_prefix seq remap e in 
  let rec aux pat = match pat with
  | PAT_name (sr,v) -> PAT_name (sr, ren v)
  | PAT_as (sr,p,v) -> PAT_as (sr,aux p, ren v)
  | PAT_when (sr,p,e) -> PAT_when (sr,aux p, rexp e)
  | PAT_nonconst_ctor (sr,n,p) -> PAT_nonconst_ctor (sr, n, aux p)
  | PAT_tuple (sr,ps) -> PAT_tuple (sr, List.map aux ps)
  | PAT_record (sr, ps) -> PAT_record (sr, List.map (fun (id,p) -> id, aux p) ps)
  | p -> p
  in aux pat

(* protect parameter names, to prevent gratuitous substitions *)
let protect sr (ps:id_t list) : macro_dfn_t list =
  let rec aux t macs =
    match t with
    | [] -> macs
    | h :: t ->
      let mac = h, MVal (EXPR_noexpand (sr,EXPR_name (sr,h,[]))) in
      aux t (mac::macs)
  in
    aux ps []

let build_args sr ps args =
  List.map2
  (fun (p,t) a ->
    match t with
    | Ident ->
      begin match a with
      | EXPR_name (_,name,[]) -> (p,MName name)
      | _ ->
        clierr sr
        (
          "[build_args] Wrong argument type, expected Identifier, got:\n" ^
          string_of_expr a
        )
      end

    | Expr -> (p,MVal a)
    | Stmt ->
      begin match a with
      | EXPR_lambda (_,(dfltvs,[[],_],TYP_none,sts)) -> (p,MStmt ([],sts))
      | EXPR_name(_,name,[]) ->(p,MVal a)
      | _ ->
        clierr sr
        (
          "[build_args] Wrong argument type, expected {} enclosed statement list or macro procedure name, got\n" ^
          string_of_expr a
        )
      end
  )
  ps args

(* alpha convert parameter names *)
let rec alpha_expr sr local_prefix seq ps e =
  let psn, pst = List.split ps in
  let psn' =  (* new parameter names *)
    List.map
    (fun _ -> let b = !seq in incr seq; "_" ^ string_of_int b)
    psn
  in
  let remap =
    List.map2
    (fun x y -> (x,MName y))
    psn psn'
  in
    let e = expand_expr 50 local_prefix seq remap e in
    let ps = List.combine psn' pst in
    ps,e

and alpha_stmts sr local_prefix seq ps sts =
  let psn, pst = List.split ps in
  let psn' =  (* new parameter names *)
    List.map
    (fun _ -> let b = !seq in incr seq; "_" ^ local_prefix ^ "_" ^ string_of_int b)
    psn
  in
  let remap =
    List.map2
    (fun x y -> (x,MName y))
    psn psn'
  in
    let sts = subst_statements 50 local_prefix seq (ref true) remap sts in
    let ps = List.combine psn' pst in
    ps,sts

(* Syntax extension stuffs *)
and eval_arg ses recursion_limit local_prefix seq reachable sr (id:string) (h:ast_term_t) : macro_dfn_t option =
  let wrap_stmts ss = EXPR_macro_statements (sr,ss) in
  match h with
  | Expression_term  e -> Some (id,MVal e)
  | Identifier_term s -> Some (id,MName s)
  (*
  | Statement_term s -> Some (id,MStmt ([],[s]))
  | Statements_term ss -> Some (id,MStmt ([],ss))
  *)
  | Statement_term s -> Some (id,MVal (wrap_stmts [s]))
  | Statements_term ss -> Some (id,MVal (wrap_stmts ss))
  | Keyword_term _ ->
    (*
    print_endline ("[substitute statement terms] Keyword arg dropped " ^ id);
    *)
    None
  | Apply_term (body,args) ->
    let body = eval_apply ses recursion_limit local_prefix seq reachable sr body args in
    eval_arg ses recursion_limit local_prefix seq reachable sr id body

and eval_args ses recursion_limit local_prefix seq reachable sr (ts: ast_term_t list) : macro_dfn_t list =
  let rec aux terms res count =
    let id = "_" ^ si count in
    match terms with
    | h :: t ->
      let mac = eval_arg ses recursion_limit local_prefix seq reachable sr id h in
      begin match mac with
      | Some m -> aux t (m::res) (count+1)
      | None -> aux t res (count+1)
      end
    | [] -> res
  in aux ts [] 1

and eval_apply ses recursion_limit local_prefix seq reachable sr (body:ast_term_t) (args:ast_term_t list) : ast_term_t =
  (*
  print_endline "Processing Application .. evaluating args";
  *)
  let args = eval_args ses recursion_limit local_prefix seq reachable sr args in
  (*
  print_endline "[apply] Got arguments ..";
  print_endline (string_of_macro_env args);
  print_endline "[apply] WE SHOULD EXPAND THE ARGS BUT AREN'T AT THE MOMENT";
  print_endline ("[apply] Body is " ^ string_of_ast_term 0 body);
  print_endline "[apply] APPLYING TERM TO EVALUATED ARGUMENTS ";
  *)
  let term = eval_term_apply ses recursion_limit local_prefix seq reachable sr body args in
  (*
  print_endline ("Term after evaluation is " ^ string_of_ast_term 0 term);
  *)
  term

and eval_term_apply ses recursion_limit local_prefix seq reachable sr (body:ast_term_t) (args:macro_dfn_t list) : ast_term_t =
  match body with
  | Expression_term e ->
    (*
    print_endline ("EXPANDING EXPRESSION " ^ string_of_expr e);
    *)
    let e = expand_expr (recursion_limit-1) local_prefix seq args e in
    Expression_term e

  | Identifier_term id ->
    let id = expand_ident sr args [] id in
    Identifier_term id

  | Statement_term s ->
    let ss = subst_statements recursion_limit local_prefix seq reachable args [s] in
    (*
    print_endline ("[apply:statement] Body after substitution is" ^ string_of_statements ss);
    print_endline "[apply:statement] EXECUTING STATEMENTS NOW";
    *)
    let ss = ses ss in
    Statements_term ss

  | Statements_term ss ->
    let ss = subst_statements recursion_limit local_prefix seq reachable args ss in
    (*
    print_endline ("[apply:statements] Body after substitution is " ^ string_of_statements ss);
    print_endline "[apply:statements] EXECUTING STATEMENTS NOW";
    *)
    let ss = ses ss in
    Statements_term ss

  | Keyword_term _ -> body
  | Apply_term (body',args') ->
    (*
    print_endline "[apply] Inner application";
    *)
    (* Inner application -- substitute into its arguments first *)
    let args' =
      List.map begin fun body ->
        eval_term_apply
          ses
          recursion_limit
          local_prefix
          seq
          reachable
          sr
          body
          args
      end args'
    in
    eval_apply ses recursion_limit local_prefix seq reachable sr body' args'

and expand_type_expr sr recursion_limit local_prefix seq (macros:macro_dfn_t list) (t:typecode_t):typecode_t=
  if recursion_limit < 1
  then failwith "Recursion limit exceeded expanding macros";
  let recursion_limit = recursion_limit - 1 in
  let me e = expand_expr recursion_limit local_prefix seq macros e in
  let mt t : typecode_t = expand_type_expr sr recursion_limit local_prefix seq macros t in
  let mi sr i =
    let out = expand_ident sr macros [] i in
    out
  in
  match Flx_maps.map_type mt t with

  (* Name expansion *)
  | TYP_name (sr, name,[]) as t ->
    begin try
      match List.assoc name macros with
      | MVar b -> typecode_of_expr (me !b)
      | MVal b -> typecode_of_expr (me b)
      | MExpr(ps,b) -> t
      | MName _ -> TYP_name (sr,mi sr name,[])
      | MStmt (ps,b) -> t
      | MVals xs -> t
      | MNames idts -> t
    with
    | Not_found -> t
    end

  | TYP_name (sr, name, ts) as t ->
    let ts = List.map mt ts in
    begin try
      match List.assoc name macros with
      | MName _ -> TYP_name (sr,mi sr name,ts)
      | _ -> TYP_name (sr,name,ts)
    with
    | Not_found -> t
    end

  | TYP_typeof e -> TYP_typeof (me e)

  | x -> x

(* expand expression *)
and expand_expr recursion_limit local_prefix seq (macros:macro_dfn_t list) (e:expr_t):expr_t =
  (*
  print_endline ("expand expr " ^ string_of_expr e);
  *)
  if recursion_limit < 1
  then failwith "Recursion limit exceeded expanding macros";
  let recursion_limit = recursion_limit - 1 in
  let me e = expand_expr recursion_limit local_prefix seq macros e in
  let mt sr e = expand_type_expr sr recursion_limit local_prefix seq macros e in
  let mi sr i =
    let out = expand_ident sr macros [] i in
    out
  in
  let cf e = const_fold e in
  let e = cf e in
  match e with

  (* This CAN happen: typecase is an ordinary expression
    with no meaning except as a proxy for a type, however
    at a macro level, it is an ordinary expression .. hmm
  *)
  | EXPR_patvar _
  | EXPR_patany _ -> print_endline "HACK.. AST_pat thing in expr"; e

  (* Expansion block: don't even fold constants *)
  | EXPR_noexpand _ -> e
  | EXPR_vsprintf _ -> e

  (* and desugaring: x and y and z and ... *)
  | EXPR_andlist (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty and list"
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"land",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* or desugaring: x or y or z or ... *)
  | EXPR_orlist (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty alternative list"
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"lor",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* Sum desugaring: x+y+z+ ... *)
  | EXPR_sum (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty addition"
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"add",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* Product desugaring: x*y*z* ... *)
  | EXPR_product (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty multiply"
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"mul",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* Setunion desugaring: x || y || z || ... *)
  | EXPR_setunion (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty setunion "
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"setunion",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* Setintersection desugaring: x && y && z && ... *)
  | EXPR_setintersection (sr, es) ->
    begin match es with
    | [] -> failwith "Unexpected empty set intersection"
    | h::t ->
      List.fold_left
      (fun x y ->
        me
        (
          EXPR_apply
          (
            sr,
            (
              EXPR_name ( sr,"setintersect",[]),
              EXPR_tuple (sr,[me x; me y])
            )
          )
        )
      )
      h t
    end

  (* Name expansion *)
  | EXPR_name (sr, name,[]) ->
    (*
    print_endline ("EXPANDING NAME " ^ name);
    *)
    let mac = try Some (List.assoc name macros) with Not_found -> None in
    begin match mac with
    | None -> e
    | Some mac -> match mac with
    | MVar b -> me !b
    | MVal b -> me b
    | MVals bs -> EXPR_tuple (sr,(List.map me bs))
    | MExpr(ps,b) ->
     (*
     clierr sr ("Name "^name^" expands to unapplied macro function");
     *)
     e

    | MName _ -> EXPR_name (sr,mi sr name,[])
    | MNames _ -> clierr sr "Cannot use macro name list here"
    | MStmt (ps,b) ->
     (*
     clierr sr ("Name "^name^" expands to unapplied macro procedure");
     *)
     e
    end

  | EXPR_name (sr, name,ts) ->
    let ts = List.map (mt sr) ts in
    begin try
      match List.assoc name macros with
      | MName _ -> EXPR_name (sr,mi sr name,ts)
      | _ -> EXPR_name (sr,name,ts)
    with
    | Not_found -> e
    end


   (* artificially make singleton tuple *)
  | EXPR_apply (sr,(EXPR_name(_,"_tuple",[]),x)) ->
     (*
     print_endline "Making singleton tuple";
     *)
     EXPR_tuple (sr,[me x])

  | EXPR_apply (sr,(EXPR_name(_,"_str",[]),x)) ->
     let x = me x in
     let x = string_of_expr x in
     EXPR_literal (sr, AST_string x)

   (* _tuple_cons (a,t) ->
     a,t if t is not a tuple
     tuple t with a prepended otherwise

     NOTE .. not sure if this should be done
     before or after expansion ..
   *)
  | EXPR_apply (sr,
       (
         EXPR_name(_,"_tuple_cons",[]),
         EXPR_tuple (_,[h;t])
       )
     ) ->
     begin match me t with
     | EXPR_tuple (_,tail) ->
       (*
       print_endline "Packing tuple";
       *)
       EXPR_tuple (sr,me h :: tail)
     | tail ->
       (*
       print_endline "Making pair";
       *)
       EXPR_tuple (sr, [me h; tail])
     end

   (* Name application *)
   (* NOTE: Felix doesn't support shortcut applications
      for executable expressions, however these
      ARE available for macro expansion: this is in
      fact completely basic: the expression
        id
      is indeed expanded and is of course
      equivalent to
        id ()
   *)
  | EXPR_apply (sr, (e1', e2')) ->
    let
      e1 = me e1' and
      e2 = me e2'
    in
      begin match e1 with
      | EXPR_name(srn,name,[]) ->
        begin try
          match List.assoc name macros with
          | MName _
          | MNames _
          | MVar _
          | MVal _
          | MVals _ -> assert false

          | MExpr(ps,b) ->
            let args =
              match e2 with
              | EXPR_tuple (_,ls) -> ls
              | x -> [x]
            in
            let np = List.length ps and na = List.length args in
            if na = np
            then
              begin
                let args = List.map me args in
                let args = build_args sr ps args in
                let b = expand_expr recursion_limit local_prefix (ref 0) args b in
                me b
              end
            else
              clierr sr
              (
                "[expand_expr:apply] In application:\n" ^
                "  fun = " ^string_of_expr e1'^" --> "^string_of_expr e1^"\n"^
                "  arg = " ^string_of_expr e2'^" --> "^string_of_expr e2^"\n"^
                "Macro "^name^
                " requires "^string_of_int np^" arguments," ^
                " got " ^ string_of_int na
              )
          | MStmt (ps,b) ->
            (* replace the application with a lambda wrapping
              of the corresponding procedure call
            *)
            let sts = [STMT_call (sr,e1, e2)] in
            let sts = expand_statements recursion_limit local_prefix seq (ref true) macros sts in
            EXPR_lambda(sr,(dfltvs,[[],None],TYP_none,sts))
            (*
            clierr sr
            (
              "[expand_expr:apply] In application:\n" ^
              "  fun = " ^string_of_expr e1'^" --> "^string_of_expr e1^"\n"^
              "  arg = " ^string_of_expr e2'^" --> "^string_of_expr e2^"\n"^
              "Macro "^name^
              " is a procedure macro"
            )
            *)
        with
        | Not_found ->
          cf (EXPR_apply(sr,(e1, e2)))
        end
      | _ ->
        EXPR_apply(sr,(e1, e2))
      end

  | EXPR_cond (sr, (e1, e2, e3)) ->
    let cond = me e1 in
    begin match cond with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c=1 then me e2 else me e3
    | _ ->
      EXPR_cond (sr,(cond,me e2,me e3))
    end

  | EXPR_expr (sr,s,t) -> EXPR_expr (sr,s,t)

  (* Lambda hook *)
  | EXPR_lambda (sr, (vs,pss, t, sts)) ->
    let pr = List.concat (
      List.map (List.map (fun(x,y,z,d)->y)) (List.map fst pss))
    in
    let pr = protect sr pr in
    let sts =
      expand_statements recursion_limit local_prefix seq (ref true)
      (pr @ macros) sts
    in
    EXPR_lambda (sr, (vs,pss, t, sts))

  (* Name lookup *)
  | EXPR_the (sr, qn) ->
    let qn =
      match qualified_name_of_expr (me (expr_of_qualified_name qn)) with
      | Some x -> x
      | None -> assert false
    in
    EXPR_the (sr,qn)

  (* the name here is just for diagnostics *)
  | EXPR_index (sr, n, i) -> EXPR_index (sr,n,i)
  | EXPR_intersect (sr, es) -> EXPR_intersect (sr, List.map me es)
  | EXPR_isin (sr,(a,b)) -> EXPR_isin (sr, (me a, me b))

  | EXPR_lookup (sr, (e1, name,ts)) ->
      EXPR_lookup (sr,(me e1, mi sr name, List.map (mt sr) ts))

  | EXPR_case_tag (sr, i) -> e
  | EXPR_typed_case (sr, i, t) -> e
  | EXPR_case_index (sr,e) -> EXPR_case_index (sr,me e)

  | EXPR_macro_ctor (sr,(name,e)) -> EXPR_macro_ctor (sr,(name,me e))
  | EXPR_macro_statements (sr,sts) ->
     let sts =
      expand_statements recursion_limit local_prefix seq (ref true)
      macros sts
     in
     EXPR_macro_statements (sr,sts)

  | EXPR_tuple (sr, es) -> EXPR_tuple (sr, List.map me es)
  | EXPR_record (sr, es) ->
    EXPR_record (sr, List.map (fun (s,e)-> s, me e) es)

  | EXPR_variant (sr, (s,e)) ->
    EXPR_variant (sr, ( s, me e))

  | EXPR_record_type (sr,ts)
  | EXPR_variant_type (sr,ts) ->
     clierr sr "Anonymous struct or record type cannot be used as an expression"

  | EXPR_arrayof (sr, es) -> EXPR_arrayof (sr, List.map me es)
  | EXPR_coercion (sr, (e1, t)) -> EXPR_coercion (sr, (me e1,mt sr t))
  | EXPR_suffix (sr, (qn, t)) ->
    let qn =
      match qualified_name_of_expr (me (expr_of_qualified_name qn)) with
      | Some x -> x
      | None -> assert false
    in
    EXPR_suffix (sr, (qn,t))

  | EXPR_callback (sr,qn) ->
    let qn =
      match qualified_name_of_expr (me (expr_of_qualified_name qn)) with
      | Some x -> x
      | None -> assert false
    in
    EXPR_callback (sr, qn)

  | EXPR_arrow (sr, (e1, e2)) ->  EXPR_arrow (sr,(me e1, me e2))
  | EXPR_longarrow (sr, (e1, e2)) ->  EXPR_longarrow (sr,(me e1, me e2))
  | EXPR_superscript (sr, (e1, e2)) ->  EXPR_superscript (sr,(me e1, me e2))

  | EXPR_literal (sr, literal) ->  e
  | EXPR_map (sr, f, e) -> EXPR_map (sr, me f, me e)
  | EXPR_deref (sr, e1) -> EXPR_deref (sr, me e1)
  | EXPR_ref (sr, e1) ->  EXPR_ref (sr, me e1)
  | EXPR_likely (sr, e1) ->  EXPR_likely (sr, me e1)
  | EXPR_unlikely (sr, e1) ->  EXPR_unlikely (sr, me e1)
  | EXPR_new (sr, e1) ->  EXPR_new (sr, me e1)
  | EXPR_dot (sr, (e1, e2)) ->  EXPR_dot (sr,(me e1, me e2))
  | EXPR_match_ctor (sr, (qn, e1)) -> EXPR_match_ctor (sr,(qn,me e1))
  | EXPR_match_case (sr, (i, e1)) ->  EXPR_match_case (sr,(i, me e1))
  | EXPR_ctor_arg (sr, (qn, e1)) -> EXPR_ctor_arg (sr,(qn, me e1))
  | EXPR_case_arg (sr, (i, e1)) ->  EXPR_case_arg (sr,(i,me e1))
  | EXPR_letin (sr, (pat, e1, e2)) -> EXPR_letin (sr, (pat, me e1, me e2))

  | EXPR_get_n (sr, (i, e1)) ->  EXPR_get_n (sr,(i,me e1))
  | EXPR_get_named_variable (sr, (i, e1)) ->  EXPR_get_named_variable (sr,(i,me e1))
  | EXPR_as (sr, (e1, id)) ->  EXPR_as (sr,(me e1, mi sr id))

  | EXPR_match (sr, (e1, pes)) ->
    let pes =
      List.map
      (fun (pat,e) ->
        pat,
        let pvs = get_pattern_vars pat in
        let pr = protect sr pvs in
        expand_expr recursion_limit local_prefix seq (pr @ macros) e
      )
      pes
    in
    EXPR_match (sr,(me e1, pes))

  | EXPR_type_match (sr, (e,ps)) ->
    let ps = List.map (fun (pat,e) -> pat, mt sr e) ps in
    EXPR_type_match (sr,(mt sr e,ps))

  | EXPR_ellipsis _
  | EXPR_void _ -> e

  | EXPR_typeof (sr,e) -> EXPR_typeof (sr, me e)

  | EXPR_user_expr (sr,name,term) ->
    (* HACKS ARGGGHH .. *)
    let ref_macros = ref [] in
    let ses ss =
      special_expand_statements recursion_limit local_prefix seq (ref true) ref_macros macros ss
    in
    let reachable = ref true in
    (* END HACKS ARGGGHH .. *)

    (*
    print_endline ("Replacing into user expression " ^ name);
    *)
    let rec aux term = match term with
      | Statement_term s -> clierr sr ( "User expr: expected expression ")
      | Statements_term ss -> clierr sr ( "User expr: expected expression ")
      | Expression_term e -> Expression_term (me e)
      | Identifier_term s -> clierr sr ( "User expr : expected expression got identifier " ^ s)

      (* ONLY SUBSTITUTE INTO PARAMETERS? *)
      | Apply_term (t,ts) -> Apply_term (t, List.map aux ts)

      (* invariant -- for the moment *)
      | Keyword_term _ -> term
    in
    let e = aux term in
    (*
    print_endline ("Expanding expression " ^ name);
    *)
    let substitute_expr_terms sr e ts =
      let args = eval_args ses recursion_limit local_prefix seq reachable sr ts in
      let e = expand_expr recursion_limit local_prefix seq args e in
      e
    in
    (*
    print_endline ("Expand Statement: Processing user defined statement " ^ name);
    *)
    let aux term = match term with
      | Statement_term s -> clierr sr ( "User expr: expected expression ")
      | Statements_term ss -> clierr sr ( "User expr: expected expression ")
      | Identifier_term s -> clierr sr ( "User expr : expected expression got identifier " ^ s)
      | Keyword_term s -> clierr sr ( "User expr : expected statement got keyword " ^ s)

      | Expression_term e -> e
      | Apply_term (t,ts) ->
        begin match t with
        | Expression_term e ->
          substitute_expr_terms sr e ts

        | _ ->
          clierr sr
          (
            "User statement: In application, expected statement "
          )
        end
    in aux term

  (*
    -> syserr (Flx_srcref.src_of_expr e) ("Expand expr: expected expresssion, got type: " ^ string_of_expr e)
  *)

(* ---------------------------------------------------------------------
  do the common work of both subst_statement and expand_statement,
  recursion to the appropriate one as indicated by the argument 'recurse'

  The flag 'reachable' is set to false on exit if the instruction
  does not drop through. The flag may be true or false on entry.
  Whilst the flag is false, no code is generated. Once the flag
  is false, a label at the low level can cause subsequent code to become
  reachble.
*)
and rqmap me reqs =
  let r req = rqmap me req in
  match reqs with
  | RREQ_or (a,b) -> RREQ_or (r a, r b)
  | RREQ_and (a,b) -> RREQ_and (r a, r b)
  | RREQ_true -> RREQ_true
  | RREQ_false -> RREQ_false
  | RREQ_atom x ->
      match x with
      | Named_req qn ->
          let qn =
            match qualified_name_of_expr (me (expr_of_qualified_name qn)) with
            | Some x -> x
            | None -> assert false
          in
          RREQ_atom (Named_req qn)
      | x -> RREQ_atom x

and subst_or_expand recurse recursion_limit local_prefix seq reachable macros (st:statement_t):statement_t list =
  (*
  print_endline ("Subst or expand: " ^ string_of_statement 0 st);
  *)
  let recursion_limit = recursion_limit - 1 in
  let mt sr e = expand_type_expr sr recursion_limit local_prefix seq macros e in
  let me e = expand_expr recursion_limit local_prefix seq macros e in
  let meopt e = match e with | None -> None | Some x -> Some (me x) in
  let mps sr ps = List.map (fun (k,id,t,d) -> k,id,mt sr t,meopt d) ps in
  let mpsp sr (ps,pre) = mps sr ps,meopt pre in
  let rqmap req = rqmap me req in
  let ms s = recurse recursion_limit local_prefix seq (ref true) macros s in
  let ms' reachable s = recurse recursion_limit local_prefix seq reachable macros s in
  let msp sr ps ss =
    let pr = protect sr ps in
    recurse recursion_limit local_prefix seq (ref true) (pr @ macros) ss
  in
  let mi sr id = expand_ident sr macros [] id in
  let mq qn =  match qn with
    | `AST_lookup (sr, (e1, name,ts)) ->
      `AST_lookup (sr,(me e1, mi sr name, List.map (mt sr) ts))
    | `AST_name (sr, name, ts) ->
      `AST_name (sr, mi sr name, List.map (mt sr) ts)
    | x -> x
  in
  let result = ref [] in
  let tack x = result := x :: !result in
  let ctack x = if !reachable then tack x in
  let cf e = const_fold e in

  begin match st with
  (* cheat for now and ignore public and private decls *)
  (*
  | STMT_public (_,_,st) -> List.iter tack (ms [st])
  *)
  | STMT_private (sr,st) ->
    List.iter (fun st -> tack (STMT_private (sr,st))) (ms [st])

  | STMT_seq (_,sts) ->
    List.iter tack (ms sts)

  | STMT_include (sr, s) -> tack st

  (* FIX TO SUPPORT IDENTIFIER RENAMING *)
  | STMT_open (sr, vs, qn) ->
    tack (STMT_open (sr, vs, mq qn))

  | STMT_inject_module (sr, qn) -> tack st

  (* FIX TO SUPPORT IDENTIFIER RENAMING *)
  | STMT_use (sr, id, qn) -> tack (STMT_use (sr,mi sr id,qn))

  | STMT_cassign (sr,l,r) -> tack (STMT_cassign (sr, me l, me r))

  | STMT_assign (sr,name,l,r) ->
    let l = match l with
      | `Expr (sr,e),t -> `Expr (sr,me e),t
      | l -> l
    in
    tack (STMT_assign (sr, name, l, me r))

  | STMT_comment _  ->  tack st

  | STMT_union (sr, id, vs, idts ) ->
    let idts = List.map (fun (id,v,vs,t) -> id,v,vs,mt sr t) idts in
    tack (STMT_union (sr, mi sr id, vs, idts))

  | STMT_struct (sr, id, vs, idts) ->
    let idts = List.map (fun (id,t) -> id,mt sr t) idts in
    tack (STMT_struct (sr, mi sr id, vs, idts))

  | STMT_cstruct (sr, id, vs, idts, reqs) ->
    let idts = List.map (fun (id,t) -> id,mt sr t) idts in
    tack (STMT_cstruct (sr, mi sr id, vs, idts, rqmap reqs))

  | STMT_typeclass (sr, id, vs, sts) ->
    tack (STMT_typeclass (sr, mi sr id, vs, ms sts))

  | STMT_type_alias (sr, id, vs, t) ->
    tack (STMT_type_alias (sr,mi sr id,vs, mt sr t))

  | STMT_inherit (sr, id, vs, t) ->  tack st
  | STMT_inherit_fun (sr, id, vs, t) ->  tack st

  | STMT_ctypes (sr, ids, qs, reqs) ->
    List.iter
    (fun (sr,id) ->
      let id = mi sr id in
      let st = STMT_abs_decl (sr,id, dfltvs, qs, CS_str id, rqmap reqs) in
      tack st
    )
    ids

  | STMT_abs_decl (sr,id,vs,typs,v,rqs) ->
    tack (STMT_abs_decl (sr,mi sr id,vs,typs,v, rqmap rqs))

  | STMT_newtype (sr,id,vs,t) ->
    tack (STMT_newtype (sr,mi sr id,vs,mt sr t))

  | STMT_callback_decl (sr,id,args,ret,rqs) ->
    tack (STMT_callback_decl (sr,mi sr id, List.map (mt sr) args,mt sr ret,rqmap rqs))

  | STMT_const_decl (sr, id, vs, t, c, reqs) ->
     tack (STMT_const_decl (sr, mi sr id, vs, mt sr t, c, rqmap reqs))

  | STMT_fun_decl (sr, id, vs, ts, t, c, reqs,prec) ->
    tack (STMT_fun_decl (sr, mi sr id, vs, List.map (mt sr) ts, mt sr t, c, rqmap reqs,prec))

  | STMT_insert (sr, n, vs, s, ikind, reqs) ->
    tack (STMT_insert (sr,n,vs,s, ikind, rqmap reqs))

    (*
      NOTE: c code is embedded even  though it isn't
      reachable because it might contain declarations or
      even labels
    *)
  | STMT_code (sr, s) ->
    tack st;
    reachable := true

  | STMT_noreturn_code (sr, s) ->
    tack st;
    reachable := false

  (* IDENTIFIER RENAMING NOT SUPPORTED IN EXPORT *)
  | STMT_export_python_fun (sr, sn, s) ->  tack st
  | STMT_export_fun (sr, sn, s) ->  tack st
  | STMT_export_type (sr, sn, s) ->  tack st

  | STMT_label (sr, id) ->
    reachable:=true;
    tack (STMT_label (sr, mi sr id))

  | STMT_goto (sr, id) ->
    ctack (STMT_goto (sr, mi sr id));
    reachable := false

  | STMT_svc (sr, id) ->  ctack (STMT_svc (sr, mi sr id))
  | STMT_proc_return (sr)  ->  ctack st; reachable := false
  | STMT_halt (sr,s)  ->  ctack st; reachable := false
  | STMT_trace (sr,v,s)  ->  ctack st
  | STMT_nop (sr, s) ->  ()

  | STMT_reduce (sr, id, vs, ps, e1, e2) ->
    let ps = List.map (fun (s,t)-> s,mt sr t) ps in
    tack(STMT_reduce (sr, mi sr id, vs, ps, me e1, me e2))

  | STMT_axiom (sr, id, vs, psp, e1) ->
    let e1 = match e1 with
      | Predicate e -> Predicate (me e)
      | Equation (l,r) -> Equation (me l, me r)
    in
    tack(STMT_axiom (sr, mi sr id, vs, mpsp sr psp, e1))

  | STMT_lemma (sr, id, vs, psp, e1) ->
    let e1 = match e1 with
      | Predicate e -> Predicate (me e)
      | Equation (l,r) -> Equation (me l, me r)
    in
    tack(STMT_lemma (sr, mi sr id, vs, mpsp sr psp, e1))

  | STMT_function (sr, id, vs, psp, (t,post), props, sts ) ->
    let pr = List.map (fun (x,y,z,d)->y) (fst psp) in
    let post = meopt post in
    tack(STMT_function (sr, mi sr id, vs, mpsp sr psp, (mt sr t, post), props, msp sr pr sts ))

  | STMT_curry (sr,id,vs,pss,(ret,post),kind,sts) ->
    let pr = List.map (fun(x,y,z,d)->y) (List.concat (List.map fst pss)) in
    let post = match post with | None -> None | Some x -> Some (me x) in
    let pss = List.map (fun psp -> mpsp sr psp) pss in
    tack(STMT_curry(sr, mi sr id, vs, pss, (ret,post),kind, msp sr pr sts ))

  | STMT_val_decl (sr, id, vs, optt, opte) ->
    let opte = match opte with
    | Some x -> Some (me x)
        (*
          this *will be* an error if unreachable,
          provided the containing function is used
        *)
    | None -> None
        (* this is actually a syntax error in a module,
          but not in an interface: unfortunately,
          we can't tell the difference here
        *)
    in
    let optt = match optt with
    | Some t -> Some (mt sr t)
    | None -> None
    in
      tack (STMT_val_decl (sr, mi sr id, vs, optt, opte))

  | STMT_ref_decl (sr, id, vs, optt, opte) ->
    let opte = match opte with
    | Some x -> Some (me x)
        (*
          this *will be* an error if unreachable,
          provided the containing function is used
        *)
    | None -> None
        (* this is actually a syntax error in a module,
          but not in an interface: unfortunately,
          we can't tell the difference here
        *)
    in
    let optt = match optt with
    | Some t -> Some (mt sr t)
    | None -> None
    in
      tack (STMT_ref_decl (sr, mi sr id, vs, optt, opte))

  | STMT_lazy_decl (sr, id, vs, optt, opte) ->
    let opte = match opte with
    | Some x -> Some (me x)
        (*
          this *will be* an error if unreachable,
          provided the containing function is used
        *)
    | None -> None
        (* this is actually a syntax error in a module,
          but not in an interface: unfortunately,
          we can't tell the difference here
        *)
    in
    let optt = match optt with
    | Some t -> Some (mt sr t)
    | None -> None
    in
      tack (STMT_lazy_decl (sr, mi sr id, vs, optt, opte))

  | STMT_var_decl (sr, id, vs, optt, opte) ->
    let opte =
      match opte with
      | Some x -> Some (me x)
        (* unreachable var initialisations are legal *)

      | None -> None
        (* vars don't have to be initialised *)
    in
    let optt = match optt with
    | Some t -> Some (mt sr t)
    | None -> None
    in
      tack (STMT_var_decl (sr, mi sr id, vs, optt, opte))

  | STMT_untyped_module (sr, id, vs, sts) ->
    tack (STMT_untyped_module (sr, mi sr id, vs, ms sts))

  (* this gets called twice, pointlessly *)
  | STMT_stmt_match (sr, (e, pss)) ->
    (*
    print_endline "Handling statement match";
    *)
    let pss = List.map (fun (pat,sts) ->
      let pvs = get_pattern_vars pat in
      let pvs' =  (* new parameter names *)
        List.map
        (fun _ -> let b = !seq in incr seq; "_" ^ string_of_int b)
        pvs
      in
      let fast_remap = List.combine pvs pvs' in
      let remap = 
        List.map2
        (fun x y -> (x,MName y))
        pvs pvs'
      in
      (* alpha convert pattern variable names *)
      let pat' = alpha_pat local_prefix seq fast_remap remap expand_expr pat in
      (* alpha convert statements *)
      let sts' = subst_statements 50 local_prefix seq (ref true) remap sts in
      (*
      print_endline ("Statement match, original pattern: " ^ string_of_pattern pat);
      print_endline ("Statement match, original statements: " ^ string_of_statements sts);
      print_endline ("Statement match, new pattern: " ^ string_of_pattern pat');
      print_endline ("Statement match, new statements: " ^ string_of_statements sts');
      *)
      pat', ms sts' (* no need for protection because pat vars are fresh *)
      )
      pss 
    in
    tack (STMT_stmt_match (sr, (me e, pss)))
    
  | STMT_instance (sr, vs, qn, sts) ->
    tack (STMT_instance (sr, vs, mq qn, ms sts))

  | STMT_ifgoto (sr, e , id) ->
    let e = me e in
    let e = cf e in
    begin match e with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c = 1 then
      (
        ctack (STMT_goto (sr,mi sr id));
        reachable := false
      )
    | _ ->
      ctack (STMT_ifgoto (sr, e, mi sr id))
    end

  | STMT_init (sr,v,e) ->
    ctack (STMT_init (sr, mi sr v, me e))

  | STMT_assert (sr,e) ->
    let e = me e in
    begin match e with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c = 1 (* assertion proven true *)
      then ()
      else (* assertion proven false *)
        begin
          reachable := false;
          ctack (STMT_assert (sr,e))
        end

    | _ -> (* check at run time *)
        ctack (STMT_assert (sr,e))
    end

  | STMT_ifreturn (sr, e) ->
    let e = me e in
    begin match e with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c = 1 then
      (
        ctack (STMT_proc_return sr);
        reachable := false
      )
    | _ ->
      let n = !seq in incr seq;
      let lab = "_ifret_" ^ string_of_int n in
      ctack (STMT_ifgoto (sr, EXPR_apply(sr,(EXPR_name (sr,"lnot",[]), e)), lab));
      ctack (STMT_proc_return sr);
      ctack (STMT_label (sr,lab))
    end

  | STMT_ifdo (sr, e, sts1, sts2) ->
    let e = me e in
    let e = cf e in
    begin match e with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c = 1 then
        List.iter ctack (ms sts1)
      else
        List.iter ctack (ms sts2)

    | _ ->
      let n1 = !seq in incr seq;
      let n2 = !seq in incr seq;
      let lab1 = "_ifdoend_" ^ string_of_int n1 in
      let lab2 = "_ifdoelse_" ^ string_of_int n2 in
      (*
      print_endline ("Assigned labels " ^ lab1 ^ " and " ^ lab2);
      *)

      (* each branch has the initial reachability we start with.
         NOTE! Labels are allowed inside primitive conditionals!
         So even if the initial condition is 'unreachable',
         the end of a branch can still be reachable!!

         So we must tack, not ctack, the code of the inner
         compound statements, they're NOT blocks.
      *)
      ctack (STMT_ifgoto (sr, EXPR_apply (sr,(EXPR_name (sr,"lnot",[]),e)), lab1));
      let r1 = ref !reachable in
      List.iter tack (ms' r1 sts1);
      if !r1 then tack (STMT_goto (sr,lab2));

      (* this is a ctack, because it can only be targetted by prior ifnotgoto *)
      ctack (STMT_label (sr,lab1));
      let r2 = ref !reachable in
      List.iter tack (ms' r2 sts2);
      if !r1 then tack (STMT_label (sr,lab2));
      reachable := !r1 or !r2
    end


  | STMT_jump (sr, e1, e2) ->
    ctack (STMT_jump (sr, me e1, me e2));
    reachable := false

  | STMT_loop (sr, id, e2) ->
    ctack (STMT_loop (sr, mi sr id, me e2));
    reachable := false

  | STMT_fun_return (sr, e)  ->
    ctack (STMT_fun_return (sr, me e));
    reachable := false

  | STMT_yield (sr, e)  ->
    ctack (STMT_yield (sr, me e))

  | st -> failwith ("[subst_or_expand] Unhandled case " ^ string_of_statement 0 st)
  end
  ;
  List.rev !result


(* ---------------------------------------------------------------------
  expand, without defining new macros
  this routine is used to replace parameters
  in statement macros with already expanded arguments
  prior to expansion, therefore neither the arguments
  nor context in which they're used need any expansion
*)
and subst_statement recursion_limit local_prefix seq reachable macros (st:statement_t):statement_t list =
  (*
  print_endline ("subst statement " ^ string_of_statement 0 st);
  print_endline ("Macro context length " ^ si (List.length macros));
  print_endline (string_of_macro_env macros);
  *)
  if recursion_limit < 1
  then failwith "Recursion limit exceeded expanding macros";
  let recursion_limit = recursion_limit - 1 in
  let me e = expand_expr recursion_limit local_prefix seq macros e in
  let ms ss = subst_statement recursion_limit local_prefix seq (ref true) macros ss in
  let mss ss = subst_statements recursion_limit local_prefix seq (ref true) macros ss in
  let mi sr id =
    let out = expand_ident sr macros [] id in
    out
  in
  let result = ref [] in
  let tack x = result := x :: !result in
  let ctack x = if !reachable then tack x in
  let cf e = const_fold e in

  begin match st with
  | STMT_expr_macro (sr, id, ps, e) -> failwith ("unexpected macro thing");
    let ps,e = alpha_expr sr local_prefix seq ps e in
    tack (STMT_expr_macro (sr, mi sr id, ps, me e))

  | STMT_stmt_macro (sr, id, ps, sts) -> failwith ("unexpected macro thing");
    let ps,sts = alpha_stmts sr local_prefix seq ps sts in
    let sts = expand_statements recursion_limit local_prefix seq (ref true) macros sts in
    tack (STMT_stmt_macro (sr,id,ps,sts))

  | STMT_macro_block (sr, sts) -> failwith ("unexpected macro thing");
    (*
    let sts = expand_statements recursion_limit local_prefix seq (ref true) macros sts in
    *)
    let sts = mss sts in
    tack (STMT_macro_block (sr,sts))

  | STMT_macro_name (sr, id1, id2) -> failwith ("unexpected macro thing");
    (* IN THIS SPECIAL CASE THE LHS NAME IS NOT MAPPED *)
    tack (STMT_macro_name (sr, id1, mi sr id2))

  | STMT_macro_names (sr, id1, id2) -> failwith ("unexpected macro thing");
    (* IN THIS SPECIAL CASE THE LHS NAME IS NOT MAPPED *)
    tack (STMT_macro_names (sr, id1, List.map (mi sr) id2))

  | STMT_macro_val (sr, ids, e) ->
    tack (STMT_macro_val (sr, List.map (mi sr) ids, me e))

  | STMT_macro_vals (sr, id, e) -> failwith ("unexpected macro thing");
    tack (STMT_macro_vals (sr,mi sr id, List.map me e))

  | STMT_macro_var (sr, ids, e) -> failwith ("unexpected macro thing");
    tack (STMT_macro_var (sr, List.map (mi sr) ids, me e))

  | STMT_macro_assign (sr, ids, e) -> failwith ("unexpected macro thing");
    tack (STMT_macro_assign (sr, List.map (mi sr) ids, me e))

  | STMT_macro_ifor (sr,id,ids,sts) -> failwith ("unexpected macro thing");
    (* IN THIS SPECIAL CASE THE LHS NAME IS NOT MAPPED *)
    tack (STMT_macro_ifor (sr,id, List.map (mi sr) ids,mss sts))

  | STMT_macro_vfor (sr,ids,e,sts) ->
    tack (STMT_macro_vfor (sr, List.map (mi sr) ids,me e,mss sts))

  (* during parameter replacement,
    we don't know if a call is executable or not,
    so we can't elide it even if unreachable:
    it might expand to declarations or macros
  *)
  | STMT_call (sr, (EXPR_name(srn,name,[]) as e1), e2) -> failwith ("unexpected macro thing");
    (* let e1 = EXPR_name(srn, name,[]) in *)
    begin try
      match List.assoc name macros with
      | MStmt ([],b) ->
(*
        print_endline ("EXPANDING call to macro " ^ name);
*)
        List.iter tack (mss b)
      | _ ->
        tack (STMT_call (sr, me e1, me e2))
    with Not_found ->
      tack (STMT_call (sr, me e1, me e2))
    end

  | STMT_call (sr, e1, e2) ->
    tack (STMT_call (sr, me e1, me e2))

  | STMT_user_statement (sr,name,term) -> failwith ("unexpected macro thing");
    (*
    print_endline ("Replacing into user statement call " ^ name);
    *)
    let rec aux term = match term with
      | Statement_term s -> Statements_term (ms s)
      | Statements_term ss -> Statements_term (mss ss)
      | Expression_term e -> Expression_term (me e)
      | Identifier_term s -> Identifier_term (mi sr s)

      (* ONLY SUBSTITUTE INTO PARAMETERS? *)
      | Apply_term (t,ts) -> Apply_term (t, List.map aux ts)

      (* invariant -- for the moment *)
      | Keyword_term _ -> term
    in
    tack (STMT_user_statement (sr,name,aux term))

  | STMT_macro_ifgoto (sr,e,id) -> failwith ("unexpected macro thing");
    (*
    print_endline ("Substituting if/goto " ^ string_of_expr e);
    *)
    tack (STMT_macro_ifgoto (sr, cf (me e), mi sr id))

  | STMT_macro_label _
  | STMT_macro_goto _
  | STMT_macro_proc_return _
  | STMT_macro_forget _
    -> failwith ("unexpected macro thing"); tack st
 
  | st ->
    List.iter tack
    (
      subst_or_expand subst_statements recursion_limit local_prefix seq reachable macros st
    )
  end
  ;
  List.rev !result

and subst_statements recursion_limit local_prefix seq reachable macros (ss:statement_t list) =
  List.concat (List.map (subst_statement recursion_limit local_prefix seq reachable macros) ss)

(* ---------------------------------------------------------------------
  expand statement : process macros
*)
and expand_statement recursion_limit local_prefix seq reachable ref_macros macros (st:statement_t) =
  (*
  print_endline ("Expand statement " ^ string_of_statement 0 st);
  print_endline ("Macro context length " ^ si (List.length macros));
  print_endline (string_of_macro_env macros);
  *)
  if recursion_limit < 1
  then failwith "Recursion limit exceeded expanding macros";
  let recursion_limit = recursion_limit - 1 in
  let me e = expand_expr recursion_limit local_prefix seq (!ref_macros @ macros) e in
  let ms ss = expand_statements recursion_limit local_prefix seq (ref true) (!ref_macros @ macros) ss in
  let mi sr id =
    let out = expand_ident sr (!ref_macros @ macros) [] id  in
    out
  in
  let result = ref [] in
  let tack x = result := x :: !result in
  let ctack x = if !reachable then tack x in
  let ses ss =
    special_expand_statements recursion_limit local_prefix seq (ref true) ref_macros macros ss
  in
  let rec expand_names sr (names:string list):string list =
    List.concat
    (
      List.map
      (fun name ->
        let name = mi sr name in
        let d =
          try Some (List.assoc name (!ref_macros @ macros))
          with Not_found -> None
        in
        match d with
        | Some (MNames es) -> expand_names sr es
        | Some (MName x) -> [x]
        | Some(_) -> [name] (* clierr sr "Name list required" *)
        | None -> [name]
      )
      names
    )
  in
  let rec expand_exprs sr (exprs: expr_t list):expr_t list =
    (*
    print_endline ("Expand exprs: [" ^ catmap ", " string_of_expr exprs ^ "]");
    *)
    List.concat
    (
      List.map
      (fun expr -> match expr with
      | EXPR_name (sr',name,[]) ->
        print_endline ("Name " ^ name);
        let name = mi sr name in
        let d =
          try Some (List.assoc name (!ref_macros @ macros))
          with Not_found -> None
        in
        begin match d with
        | Some (MNames es) ->
          expand_exprs sr
          (List.map (fun name -> EXPR_name (sr,name,[])) es)

        | Some (MName x) ->
          expand_exprs sr [EXPR_name(sr,x,[])]

        | Some(MVals xs) -> xs
        | Some(_) -> [expr]
        | None -> [expr]
        end

      | EXPR_tuple (sr',xs) -> List.map me xs
      | x -> [me x]
      )
      exprs
    )
  in
  begin match st with
  | STMT_macro_forget (sr,ids) -> failwith ("Unexpected macro thing");
    begin
      match ids with
      | [] -> ref_macros := []
      | _ ->
        ref_macros := List.filter (fun (x,_) -> not (List.mem x ids)) !ref_macros
    end

  | STMT_expr_macro (sr, id, ps, e) -> failwith ("Unexpected macro thing"); 
    let ps,e = alpha_expr sr local_prefix seq ps e in
    ref_macros := (id,MExpr (ps, e)) :: !ref_macros

  | STMT_macro_val (sr, ids, e) ->
    let e = me e in
    let n = List.length ids in
    if n = 1 then
      ref_macros := (List.hd ids, MVal e) :: !ref_macros
    else begin
      let vs =
        match e with
        | EXPR_tuple (_,ls) -> ls
        | _ -> clierr sr "Unpack non-tuple"
      in
      let m = List.length vs in
      if m <> n then
        clierr sr
        (
          "Tuple is wrong length, got " ^
          si n ^ " variables, only " ^
          si m ^ " values"
        )
      else
      let ides = List.combine ids vs in
      List.iter (fun (id,v) ->
        ref_macros := (id,MVal v) :: !ref_macros
      )
      ides
    end

  | STMT_macro_vals (sr, id, es) ->  failwith ("Unexpected macro thing");
    ref_macros := (id,MVals (List.map me es)) :: !ref_macros

  | STMT_macro_var (sr, ids, e) -> failwith ("Unexpected macro thing"); 
    let e = me e in
    let n = List.length ids in
    if n = 1 then
      ref_macros := (List.hd ids,MVar (ref e)) :: !ref_macros
    else begin
      let vs =
        match e with
        | EXPR_tuple (_,ls) -> ls
        | _ -> clierr sr "Unpack non-tuple"
      in
      let m = List.length vs in
      if m <> n then
        clierr sr
        (
          "Tuple is wrong length, got " ^
          si n ^ " variables, only " ^
          si m ^ " values"
        )
      else
      let ides = List.combine ids vs in
      List.iter (fun (id,v) ->
        ref_macros := (id,MVar (ref v)) :: !ref_macros
      )
      ides
    end

  | STMT_macro_assign (sr, ids, e) -> failwith ("Unexpected macro thing");
    let assign id e =
      try
        let r = List.assoc id (!ref_macros @ macros) in
        match r with
        | MVar p -> p := e
        | _ -> clierr sr "Assignment to wrong kind of macro"
      with Not_found -> clierr sr "Assignment requires macro var"
    in
    let e = me e in
    let n = List.length ids in
    if n = 1 then assign (List.hd ids) e
    else begin
      let vs =
        match e with
        | EXPR_tuple (_,ls) -> ls
        | _ -> clierr sr "Unpack non-tuple"
      in
      let m = List.length vs in
      if m <> n then
        clierr sr
        (
          "Tuple is wrong length, got " ^
          si n ^ " variables, only " ^
          si m ^ " values"
        )
      else
      let ides = List.combine ids vs in
      List.iter (fun (id,v) -> assign id v) ides
    end

  | STMT_macro_ifor (sr, id, names, sts) -> failwith ("Unexpected macro thing");
    let names = expand_names sr names in
    List.iter (fun name ->
      let saved_macros = !ref_macros in
      ref_macros := (id,MName name) :: saved_macros;
      List.iter tack (ms sts);
      ref_macros := saved_macros
    ) names

  | STMT_macro_vfor (sr, ids, e, sts) ->
    (*
    print_endline "Expanding vfor";
    *)
    let e = me e in
    let vals = match e with
      | EXPR_tuple (_,vals) -> vals
      | x -> [x]
    in
    List.iter (fun e ->
      let saved_macros = !ref_macros in
      begin
        let n = List.length ids in
        if n = 1 then begin
          (*
          print_endline ("Setting " ^ List.hd ids ^ " to " ^ string_of_expr e);
          *)
          ref_macros := (List.hd ids, MVal e) :: !ref_macros
        end else begin
          let vs =
            match e with
            | EXPR_tuple (_,ls) -> ls
            | _ -> clierr sr ("Unpack non-tuple " ^ string_of_expr e)
          in
          let m = List.length vs in
          if m <> n then
            clierr sr
            (
              "Tuple is wrong length, got " ^
              si n ^ " variables, only " ^
              si m ^ " values"
            )
          else
          let ides = List.combine ids vs in
          List.iter (fun (id,v) ->
            (*
            print_endline ("Setting " ^ id ^ " to " ^ string_of_expr v);
            *)
            ref_macros := (id,MVal v) :: !ref_macros
          )
          ides
        end
      end
      ;
      List.iter tack (ms sts);
      ref_macros := saved_macros
    ) vals

  | STMT_stmt_macro (sr, id, ps, sts) -> failwith ("Unexpected macro thing");
    let ps,sts = alpha_stmts sr local_prefix seq ps sts in
    ref_macros := (id, MStmt (ps,sts)) :: !ref_macros

  | STMT_macro_name (sr, id1, id2) -> failwith ("Unexpected macro thing");
    let id2 = mi sr id2 in
    let id2 =
      match id2 with
      | "" ->
        let n = !seq in incr seq;
        "_" ^ local_prefix^ "_" ^ string_of_int n
      | _ -> id2
    in
    ref_macros := (id1,MName id2) :: !ref_macros

  | STMT_macro_names (sr, id, ids) -> failwith ("Unexpected macro thing");
    let ids = List.map (mi sr) ids in
    ref_macros := (id,MNames ids) :: !ref_macros

  | STMT_macro_block (sr,sts) -> failwith ("Unexpected macro thing");
    let b = subst_statements recursion_limit local_prefix seq reachable [] sts in
    (* NOTE SPECIAL HACK -- ANY MACROS DEFINED IN A MACRO BLOCK ARE LOST *)
    let ses ss =
      special_expand_statements recursion_limit local_prefix seq (ref true) (ref []) macros ss
    in
    let b = ses b in
    List.iter ctack b

  | STMT_call (sr, EXPR_macro_statements (srs,sts), arg) -> failwith ("Unexpected macro thing");
    begin match arg with
    | EXPR_tuple (_,[]) ->
      let sts = ms sts in
      List.iter ctack sts

    | _ -> clierr sr "Apply statements requires unit arg"
    end

  | STMT_call (sr,
      EXPR_name(srn,"_scheme", []),
      EXPR_literal (srl, AST_string s)
    ) -> failwith ("Unexpected macro thing");
    print_endline "DETECTED STATEMENT ENCODED AS SCHEME";
    let get_port = function
      | Ocs_types.Sport p -> p
      | _ -> failwith "expected port"
    in
    let env = Ocs_top.make_env () in
    let th = Ocs_top.make_thread () in
    let inp = Ocs_port.string_input_port s in
    let outp = get_port th.Ocs_types.th_stdout in
    let lex = Ocs_lex.make_lexer inp "" in
    let term = ref None in
    begin try
      match Ocs_read.read_expr lex with
      | Ocs_types.Seof -> print_endline "END OF FILE?"
      | v ->
         let c = Ocs_compile.compile env v in
         print_endline "COMPILED";
         Ocs_eval.eval th (function
           | Ocs_types.Sunspec -> print_endline "UNSPECIFIED"
           | r ->
             print_endline "EVALUATED";
             Ocs_print.print outp false r;
             Ocs_port.puts outp "\n";
             Ocs_port.flush outp;
             term := Some r
         ) c
    with
      | Ocs_error.Error err
      | Ocs_error.ErrorL (_,err)
      ->
        print_endline ("Error " ^ err)
    end
    ;
    begin match !term with
    | None -> failwith "Scheme term not returned!"
    | Some r ->
        let sex = Ocs2sex.ocs2sex r in
        print_endline "OCS scheme term converted to s-expression:";
        Sex_print.sex_print sex;
        let flx = Flx_sex2flx.xstatement_t sr sex in
        print_endline "s-expression converted to Felix statement!";
        print_endline (string_of_statement 0 flx);
        ctack flx
    end

  | STMT_call (sr, e1', e2') ->
    let
      e1 = me e1' and
      e2 = me e2'
    in
      begin match e1 with
      | EXPR_name(srn,name,[]) ->
        begin try
          match List.assoc name (!ref_macros @ macros) with
          | MName _
            -> failwith ("Unexpected MName " ^ name)
          | MNames _
            -> failwith ("Unexpected MNames " ^ name)
          | MVar _
            -> failwith ("Unexpected MVar " ^ name)
          | MVal _
            ->
            failwith
            (
              "Unexpected MVal " ^ name ^ " expansion\n" ^
              string_of_expr e1' ^ " --> " ^ string_of_expr e1
            )

          | MVals _
            ->
            failwith
            (
              "Unexpected MVals " ^ name ^ " expansion\n" ^
              string_of_expr e1' ^ " --> " ^ string_of_expr e1
            )


          (*
            The executable syntax allows the statement

            <atom>;

            to mean

            call <atom> ();

            which means <atom> here must be a procedure
            of type unit->void. The case:

            <atom1> <atom2>;

            however requires <atom1> to be a procedure,
            it can't be a function even if the application

            <atom1> <atom2>

            would return a procedure: the insertion of the
            trailing () is purely syntactic.

            This isn't the case for the macro processor,
            since it does 'type' analysis. We can allow
            <atom1> to be a function which when applied
            to <atom2> returns an expression denoting
            a procedure, and apply it to ().
          *)

          | MExpr (ps,b) ->
            (*
            print_endline ("Expanding statement, MExpr " ^ name);
            *)
            let result = me (EXPR_apply (sr,(e1,e2))) in
            let u = EXPR_tuple (sr,[]) in
            List.iter tack (ms [STMT_call(sr,result,u)])

          | MStmt(ps,b) ->
            (*
            print_endline ("Expanding statement, MStmt " ^ name);
            *)
            let args =
              match e2 with
              | EXPR_tuple (_,ls) -> ls
              | x -> [x]
            in
            let np = List.length ps and na = List.length args in
            if na = np
            then
              begin
                let args= List.map me args in
                let args = build_args sr ps args in
                let b = subst_statements recursion_limit local_prefix seq reachable args b in
                let b = ses b in
                (* ?? ctack ?? *)
                List.iter ctack b
              end
            else
              clierr sr
              (
                "[expand_expr:call] Statement Macro "^name^
                " requires "^string_of_int np^" arguments," ^
                " got " ^ string_of_int na
              )
        with
        | Not_found ->
          ctack (STMT_call (sr, e1, e2))
        end

      | _ -> ctack (STMT_call (sr,e1,e2))
      end

  | STMT_user_statement (sr,name,term) -> failwith ("Unexpected macro thing");
    (*
    print_endline ("Expanding statement " ^ name);
    *)
    let substitute_statement_terms sr ss ts =
      (*
      print_endline "[statement] Substitute statements terms!";
      print_endline "[statement] Original argument term list (the parse tree) is";
      List.iter (fun term -> print_endline (string_of_ast_term 0 term)) ts;
      *)
      let args = eval_args ses recursion_limit local_prefix seq reachable sr ts in
      (*
      print_endline "[statement] Got arguments ..";
      print_endline (string_of_macro_env args);
      *)
      (*
      print_endline "[statement] WE SHOULD EXPAND THE ARGS BUT AREN'T AT THE MOMENT";
      print_endline ("[statement] Body is " ^ string_of_statements ss);
      print_endline "[statement] SUBSTITUTING";
      *)
      let ss = subst_statements recursion_limit local_prefix seq reachable args ss in
      (*
      print_endline ("[statement] Body after substitution is" ^ string_of_statements ss);
      print_endline "[statement] EXECUTING STATEMENTS NOW";
      *)
      let ss = ses ss in
      (*
      print_endline ("[statement] Body after execution is" ^ string_of_statements ss);
      *)
      List.iter ctack ss
    in
    (*
    print_endline ("Expand Statement: Processing user defined statement " ^ name);
    *)
    let aux term = match term with
      | Statement_term s -> ctack s
      | Statements_term ss -> List.iter ctack ss (* reverse order is correct *)
      | Expression_term e -> clierr sr ( "User statement: expected statement got expression " ^ string_of_expr e)
      | Identifier_term s -> clierr sr ( "User statement: expected statement got identifier " ^ s)
      | Keyword_term s -> clierr sr ( "User statement: expected statement got keyword " ^ s)
      | Apply_term (t,ts) ->
        begin match t with
        | Statement_term s ->
          substitute_statement_terms sr [s] ts

        | Statements_term ss ->
          substitute_statement_terms sr ss ts

        | _ ->
          clierr sr
          (
            "User statement: In application, expected statement "
          )
        end
    in aux term


  | st ->
    List.iter tack
    (
      subst_or_expand expand_statements recursion_limit local_prefix seq reachable (!ref_macros @ macros) st
    )
  end
  ;
  List.rev !result

and expand_statements recursion_limit local_prefix seq reachable macros (ss:statement_t list) =
  let ref_macros = ref [] in
  special_expand_statements recursion_limit local_prefix seq reachable ref_macros macros ss

and special_expand_statements recursion_limit local_prefix seq
  reachable ref_macros macros ss
=
  (*
  List.iter (fun st -> print_endline (string_of_statement 0 st)) ss;
  *)
  if ss = [] then []
  else
  let sr =
    Flx_srcref.rsrange
    (src_of_stmt (List.hd ss))
    (src_of_stmt (Flx_list.list_last ss))
  in

  let cf e = const_fold e in
  let expansion = ref [] in
  let tack x = expansion := x :: !expansion in
  let tacks xs = List.iter tack xs in
  let pc = ref 0 in
  let label_map = Hashtbl.create 23 in
  let count =
    List.fold_left
    (fun count x ->
      match x with
      | STMT_macro_label (sr,s) ->
        Hashtbl.add label_map s (sr,count) ; count
      | _ -> count+1
    )
    0
    ss
  in
  let program =
    Array.of_list
    (
      List.filter
      (function | STMT_macro_label _ -> false | _ -> true)
      ss
    )
  in
  assert (count = Array.length program);
  try
    for i = 1 to 100000 do
      let st =
        if !pc >=0 && !pc < Array.length program
        then program.(!pc)
        else syserr sr
        (
          "Program counter "^si !pc^
          " out of range 0.." ^
          si (Array.length program - 1)
        )
      in
      begin match st with
      | STMT_macro_goto (sr,label) -> failwith ("Unexpected macro thing");
        begin
          try
            pc := snd (Hashtbl.find label_map label)
          with
          | Not_found ->
            clierr sr ("Undefined macro label " ^ label)
        end

      | STMT_macro_proc_return _ -> failwith ("Unexpected macro thing"); raise Macro_return

      | STMT_macro_ifgoto (sr,e,label) -> failwith ("Unexpected macro thing");
        (*
        print_endline ("Expanding if/goto " ^ string_of_expr e);
        *)
        let result =
          expand_expr
            recursion_limit
            local_prefix
            seq
            (!ref_macros @ macros)
            e
        in
        let result = cf result in
          begin match truthof result with
          | Some false -> incr pc
          | Some true ->
            begin
              try
                pc := snd (Hashtbl.find label_map label);
              with
              | Not_found ->
                clierr sr ("Undefined macro label " ^ label)
            end

          | None ->
            clierr sr
            ("Constant expression required, got " ^ string_of_expr e)
          end

      | st ->
         let sts =
           expand_statement
             recursion_limit
             local_prefix
             seq
             reachable
             ref_macros
             macros
             st
         in
           tacks sts;
           incr pc
      end
      ;
      if !pc = count then raise Macro_return
    done;
    clierr sr "macro execution step limit exceeded"
  with
    Macro_return -> List.rev !expansion

let expand_macros macro_state stmts =
  expand_statements
    macro_state.recursion_limit
    macro_state.local_prefix
    macro_state.seq
    macro_state.reachable
    macro_state.macros
    stmts

let expand_macros_in_statement macro_state handle_stmt init stmt =
  let stmts =
    expand_statement
      macro_state.recursion_limit
      macro_state.local_prefix
      macro_state.seq
      macro_state.reachable
      macro_state.ref_macros
      macro_state.macros
      stmt
  in
  List.fold_left handle_stmt init stmts

let make_macro_state ?(recursion_limit=5000) local_prefix =
  {
    recursion_limit = recursion_limit;
    local_prefix = local_prefix;
    seq = ref 1;
    reachable = ref true;
    ref_macros = ref [];
    macros = [];
  }
