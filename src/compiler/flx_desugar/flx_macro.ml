(* This is the old macro processor, gradually being cut down to something
 * saner and more manageable.
 *
 * It currently supports several special expansions including products and sums,
 * conversion of expressions to type expressions, constant folding,
 * val and foreach/val macros, and some weird specials including term generation
 * via Scheme, translation of expressions to strings for debugging, and some 
 * special code for managing tuples.
 *)

open Flx_ast
open Flx_mtypes2
open Flx_print
open Flx_exceptions
open Flx_constfld
open Flx_typing2
open Flx_util
open List

let mkstring sr x = 
  EXPR_literal (sr, {Flx_literal.felix_type="string"; internal_value=x; 
   c_value="::std::string(" ^ Flx_string.c_quote_of_string x ^ ")" })

let dyphack (ls : ( 'a * string) list) : 'a =
  match ls with
  | [x,_] -> x
  | _ -> failwith "Dypgen parser failed"

exception Macro_return

let rec truthof x = match x with
  | EXPR_typed_case (_,0,TYP_unitsum 2) -> Some false
  | EXPR_typed_case (_,1,TYP_unitsum 2) -> Some true
  | EXPR_likely (_,x) -> truthof x
  | EXPR_unlikely (_,x) -> truthof x
  | EXPR_not (_,x) -> 
    begin match truthof x with
    | Some true -> Some false
    | Some false -> Some true
    | None -> None
    end
  | _ -> None

(*
 There are no type macros: use typedef facility.
*)

type macro_t =
 | MVal of expr_t
 | MName of Flx_id.t

type macro_dfn_t = Flx_id.t * macro_t

let print_macros macros = 
  List.iter  (fun (id,x) -> print_endline (
   id ^ " = " ^ (match x with
   | MVal e -> string_of_expr e 
   | MName id -> id
   ) ^ ":"))
  macros

type macro_state_t = {
  recursion_limit: int;
  local_prefix: string;
  seq: int ref;
  reachable: bool ref;
  ref_macros: macro_dfn_t list ref;
  macros: macro_dfn_t list;
}

let get_macro_seq x = !(x.seq)

let string_of_statements sts =
  String.concat "\n" (List.map (string_of_statement 1) sts)

let scheme_eval s =
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
    match !term with
    | None -> failwith "Scheme term not returned!"
    | Some r ->
        let sex = Ocs2sex.ocs2sex r in
        print_endline "OCS scheme term converted to s-expression:";
        Sex_print.sex_print sex;
        sex

let upper =  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let lower = "abcdefghijklmnopqrstuvwxyz"
let digits = "0123456789"

let idstart = upper ^ lower ^ "_"
let idmore = idstart ^ digits ^ "'"
let quotes =  "\"'`"

let starts_id ch = String.contains idstart ch
let continues_id ch = String.contains idmore ch
let is_quote ch = String.contains quotes ch

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

let fix_pattern counter pat =
  let rec aux p = match p with
  | PAT_none _

  | PAT_literal _
  | PAT_range _

  | PAT_name _
  | PAT_any _
  | PAT_setform_any _
  | PAT_const_ctor _ 
  | PAT_const_variant _ 
    -> p

  | PAT_coercion (sr, p, t) -> PAT_coercion (sr, aux p, t)
  | PAT_tuple (sr,ps) -> PAT_tuple (sr,List.map aux ps)
  | PAT_tuple_cons (sr,a,b) -> PAT_tuple_cons (sr,aux a,aux b)
  | PAT_tuple_snoc (sr,a,b) -> PAT_tuple_snoc (sr,aux a,aux b)
  | PAT_nonconst_ctor (sr,qn,p) -> PAT_nonconst_ctor (sr,qn,aux p)
  | PAT_subtype (sr,typ,id) -> PAT_subtype (sr, typ, id)

  | PAT_ho_ctor (sr,qn,es,p) -> PAT_ho_ctor (sr,qn,es,aux p)
  | PAT_nonconst_variant (sr,s,p) -> PAT_nonconst_variant (sr,s,aux p)
  | PAT_as (sr,p,i) -> PAT_as (sr,aux p,i)
  | PAT_when (sr,p,e) -> PAT_when (sr,aux p,e)
  | PAT_with (sr,p,es) -> PAT_with (sr, aux p, es)
  | PAT_record (sr, ps) -> PAT_record (sr, List.map (fun (i,p) -> i,aux p) ps)
  | PAT_polyrecord (sr, ps,r) -> PAT_polyrecord (sr, List.map (fun (i,p) -> i,aux p) ps, r)

  | PAT_expr (sr,e) -> 
    let n = "_sypv_" ^ (string_of_int !counter) in 
    let v = EXPR_name (sr,n,[]) in
    incr counter;
    let eq = EXPR_name (sr,"==",[]) in
    let args = EXPR_tuple (sr,[v;e]) in
    let test = EXPR_apply (sr, (eq,args)) in
    PAT_when (sr,PAT_name (sr,n),test)

  | PAT_alt _ -> assert false
  in aux pat

(* Find variable names in patterns so as to protect them *)
let rec mac_get_pattern_vars pat =
  match pat with
  | PAT_name (_,v) -> [v]
  | PAT_as (_,p,v) -> v :: mac_get_pattern_vars p
  | PAT_when (_,p,_) -> mac_get_pattern_vars p
  | PAT_nonconst_ctor (_,_,p) -> mac_get_pattern_vars p
  | PAT_ho_ctor (_,_,_,p) -> mac_get_pattern_vars p
  | PAT_nonconst_variant (_,_,p) -> mac_get_pattern_vars p
  | PAT_tuple (_,ps) -> List.concat (List.map mac_get_pattern_vars ps)
  | PAT_tuple_cons (sr,a,b) -> mac_get_pattern_vars a @ mac_get_pattern_vars b
  | PAT_tuple_snoc (sr,a,b) -> mac_get_pattern_vars a @ mac_get_pattern_vars b
  | PAT_record (_,ps) -> List.concat(List.map mac_get_pattern_vars (List.map snd ps))
  | PAT_polyrecord (_,ps,r) -> r :: List.concat(List.map mac_get_pattern_vars (List.map snd ps))
  | PAT_alt _ -> assert false
  | PAT_with (_,p,asgns) -> List.map fst asgns @ mac_get_pattern_vars p
  | PAT_subtype (_,_,v) -> [v] 
  | _ -> []

(* cartesian product of two lists N x M is a single list of N x M pairs *)

let cart2j (join:'a ->'b -> 'c) (ps: 'a list) (qs:'b list) : 'c list =
  fold_left (fun acc a -> acc @ (map (fun b-> join a b) qs)) [] ps

let cart2 (ps: 'a list) (qs:'b list) : ('a * 'b) list =  
  cart2j (fun a b -> a,b) ps qs

type field_t = string * pattern_t
type record_t = field_t list
type field_pats = string * pattern_t list

(* let us suppose we have a list of record with all but the last
field to be added, and a list of field_pats to add to them 
Then for each pat, for each record we make a new record with
that field added.
*)

let add_fields_to_records (ps: field_pats) (rs: record_t list) =
  let field_name, pats = ps in
  concat (map (fun pat -> map (fun r -> (field_name,pat)::r) rs) pats)

(* to process a whole list, we just fold over it *)
let cartr (ps: field_pats list) : record_t list =
  fold_right add_fields_to_records  ps [[]]

type component_t = pattern_t
type tuple_t = component_t list
type component_pats = pattern_t list

let add_components_to_tuples (ps: component_pats) (rs: tuple_t list) =
  let pats = ps in
  concat (map (fun pat -> map (fun r -> (pat)::r) rs) pats)

let cartt (ps: component_pats list) : tuple_t list =
  fold_right add_components_to_tuples  ps [[]]

let expand_pattern_branches pes =
  let rec aux p = match p with
    | PAT_none _

    | PAT_literal _
    | PAT_range _

    | PAT_name _
    | PAT_any _
    | PAT_setform_any _
    | PAT_const_ctor _ 
    | PAT_const_variant _ 
    | PAT_expr _
    | PAT_subtype _
      -> [p]

    | PAT_coercion (sr, p, t) -> map (fun p-> PAT_coercion (sr, p, t)) (aux p)


    | PAT_tuple_cons (sr,a,b) ->  
      map (fun (a,b) -> PAT_tuple_cons (sr,a,b)) (cart2 (aux a) (aux b))

    | PAT_tuple_snoc (sr,a,b) ->  
      map (fun (a,b) -> PAT_tuple_snoc (sr,a,b)) (cart2 (aux a) (aux b))


    | PAT_nonconst_ctor (sr,qn,p) -> map (fun p->PAT_nonconst_ctor (sr,qn,p)) (aux p)
    | PAT_ho_ctor (sr,qn,es,p) -> map (fun p->PAT_ho_ctor (sr,qn,es,p)) (aux p)
    | PAT_nonconst_variant (sr,s,p) -> map (fun p->PAT_nonconst_variant (sr,s,p)) (aux p)
    | PAT_as (sr,p,i) -> map (fun p->PAT_as (sr,p,i)) (aux p)
    | PAT_when (sr,p,e) -> map (fun p-> PAT_when (sr,p,e)) (aux p)

    | PAT_with (sr,p, asgns) -> map (fun p-> PAT_with (sr,p,asgns)) (aux p)

    | PAT_tuple (sr,ps) -> 
      let pss = map aux ps in
      map (fun (p) -> PAT_tuple (sr, (p))) (cartt pss)

    | PAT_record (sr, ps) -> 
      let pss = map (fun (s,p) -> s,aux p) ps in
      map (fun rs -> PAT_record (sr, rs)) (cartr pss)

    | PAT_polyrecord (sr, ps,r) ->
      let pss = map (fun (s,p) -> s,aux p) ps in
      map (fun rs -> PAT_polyrecord (sr, rs, r)) (cartr pss)
   
    | PAT_alt (sr,ps) -> ps

  in 
  let pss= map (fun (p,e) -> map (fun p ->p,e) (aux p)) pes in
  concat pss

  

let alpha_pat local_prefix seq fast_remap remap expand_expr pat = 
  let ren v = List.assoc v fast_remap in
  let rexp e = expand_expr 50 local_prefix seq remap e in 
  let rec aux pat = match pat with
  | PAT_name (sr,v) -> PAT_name (sr, ren v)
  | PAT_with (sr,p,es) ->
    let es = List.map (fun (s,e) -> ren s, rexp e) es in
    PAT_with (sr, aux p, es)
  | PAT_as (sr,p,v) -> PAT_as (sr,aux p, ren v)
  | PAT_when (sr,p,e) -> PAT_when (sr,aux p, rexp e)
  | PAT_nonconst_ctor (sr,n,p) -> PAT_nonconst_ctor (sr, n, aux p)
  | PAT_ho_ctor (sr,n,es,p) -> 
    let es = List.map rexp es in
    PAT_ho_ctor (sr, n, es, aux p)

  | PAT_nonconst_variant (sr,n,p) -> PAT_nonconst_variant (sr, n, aux p)
  | PAT_tuple (sr,ps) -> PAT_tuple (sr, List.map aux ps)
  | PAT_tuple_cons (sr,a,b) -> PAT_tuple_cons (sr, aux a, aux b)
  | PAT_tuple_snoc (sr,a,b) -> PAT_tuple_snoc (sr, aux a, aux b)
  | PAT_record (sr, ps) -> PAT_record (sr, List.map (fun (id,p) -> id, aux p) ps)
  | PAT_polyrecord (sr, ps, r) -> PAT_polyrecord (sr, List.map (fun (id,p) -> id, aux p) ps, ren r)
  | PAT_subtype (sr, t, id) -> PAT_subtype (sr,t, ren id)
  | p -> p
  in aux pat

(* protect parameter names, to prevent gratuitous substitions *)
let protect sr ps =
  let rec aux t macs =
    match t with
    | [] -> macs
    | h :: t ->
      let mac = h, MVal (EXPR_noexpand (sr,EXPR_name (sr,h,[]))) in
      aux t (mac::macs)
  in
    aux ps []

(* alpha convert parameter names *)
let rec alpha_expr sr local_prefix seq ps e =
  let psn, pst = List.split ps in
  let psn' =  (* new parameter names *)
    List.map begin fun _ ->
      let b = !seq in
      incr seq;
      Flx_id.of_string ("_eparam_"^local_prefix^"_" ^ string_of_int b)
    end psn
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
    List.map begin fun _ ->
      let b = !seq in
      incr seq;
      Flx_id.of_string ("_xparam_" ^ local_prefix ^ "_" ^ string_of_int b)
    end psn
  in
  let remap =
    List.map2
    (fun x y -> (x,MName y))
    psn psn'
  in
    let sts = subst_statements 500 local_prefix seq (ref true) remap sts in
    let ps = List.combine psn' pst in
    ps,sts

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
      | MVal b -> typecode_of_expr (me b)
      | MName _ -> TYP_name (sr,mi sr name,[])
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
  | EXPR_interpolate _ -> e

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
              EXPR_name ( sr,"+",[]),
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
              EXPR_name ( sr,"*",[]),
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
    | MVal b -> me b
    | MName _ -> EXPR_name (sr,mi sr name,[])
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


   (* builting function like things *)

   (* convert arbitrary expression to string for debugging
    * _str expr -> "expr"
    *)
  | EXPR_apply (sr, (
      EXPR_name (_,"range_check",[]),
      EXPR_tuple (_,[e1;e2;e3])
    )) ->
    EXPR_range_check (sr,me e1, me e2, me e3)

  | EXPR_apply (sr,(EXPR_name(_,"_str",[]),x)) ->
    let x = me x in
    let x = string_of_expr x in mkstring sr x

   (* artificially make singleton tuple 
    *   _tuple x 
    *)
  | EXPR_apply (sr,(EXPR_name(_,"_tuple",[]),x)) ->
    EXPR_tuple (sr,[me x])

  (* _scheme string conversion to expression term *)
  | EXPR_apply (sr,
      (EXPR_name(srn,"_scheme", []),
      EXPR_literal (srl, 
      {
        Flx_literal.felix_type=felix_type; 
        internal_value=s
      }))) -> 
    print_endline "DETECTED EXPR ENCODED AS SCHEME";
    let sex = scheme_eval s in
    let flx = Flx_sex2flx.xexpr_t sr sex in
    print_endline "s-expression converted to Felix!";
    print_endline (string_of_expr flx);
    flx

  (* general application *)
  | EXPR_apply (sr, (e1, e2)) -> cf (EXPR_apply (sr, (me e1, me e2)))

  (* optimise conditional with constant condition *)
  | EXPR_cond (sr, (e1, e2, e3)) ->
    let cond = me e1 in
    begin match cond with
    | EXPR_typed_case (_,c,TYP_unitsum 2) ->
      if c=1 then me e2 else me e3
    | _ ->
      EXPR_cond (sr,(cond,me e2,me e3))
    end

  | EXPR_expr (sr,s,t,e) -> EXPR_expr (sr,s,t,me e)

  (* Lambda hook *)
  | EXPR_lambda (sr, (kind,vs,pss, t, sts)) ->
    let rec aux ps : string list =
      match ps with
      | Satom (sr,x,name,z,d)->[name] 
      | Slist ps -> List.concat (List.map aux ps)
    in
       
    let pr = List.concat (List.map (fun pc -> aux (fst pc)) pss) in
    let pr = protect sr pr in
    let sts =
      expand_statements recursion_limit local_prefix seq (ref true)
      (pr @ macros) sts
    in
    EXPR_lambda (sr, (kind,vs,pss, t, sts))

  (* the name here is just for diagnostics *)
  | EXPR_index (sr, n, i) -> EXPR_index (sr,n,i)
  | EXPR_intersect (sr, es) -> EXPR_intersect (sr, List.map me es)
  | EXPR_union (sr, es) -> EXPR_union (sr, List.map me es)
  | EXPR_isin (sr,(a,b)) -> EXPR_isin (sr, (me a, me b))

  | EXPR_get_tuple_tail (sr, e) -> EXPR_get_tuple_tail (sr, me e)
  | EXPR_get_tuple_head (sr, e) -> EXPR_get_tuple_head (sr, me e)
  | EXPR_get_tuple_body (sr, e) -> EXPR_get_tuple_body (sr, me e)
  | EXPR_get_tuple_last (sr, e) -> EXPR_get_tuple_last (sr, me e)

  | EXPR_lookup (sr, (e1, name,ts)) ->
      EXPR_lookup (sr,(me e1, mi sr name, List.map (mt sr) ts))

  | EXPR_case_tag (sr, i) -> e
  | EXPR_typed_case (sr, i, t) ->EXPR_typed_case (sr,i,mt sr t) 
  | EXPR_projection (sr, i, t) -> EXPR_projection (sr, i, mt sr t)
  | EXPR_ainj (sr, e, t) -> EXPR_ainj (sr, me e, mt sr t)
  | EXPR_rnprj (sr,name,seq,e) -> EXPR_rnprj (sr,name,seq, me e)
  | EXPR_case_index (sr,e) -> EXPR_case_index (sr,me e)
  | EXPR_rptsum_arg (sr,e) -> EXPR_rptsum_arg (sr,me e)

  | EXPR_tuple (sr, es) -> EXPR_tuple (sr, List.map me es)
  | EXPR_tuple_cons (sr, eh, et) -> EXPR_tuple_cons (sr, me eh, me et)
  | EXPR_tuple_snoc (sr, eh, et) -> EXPR_tuple_snoc (sr, me eh, me et)

  | EXPR_record (sr, es) ->
    let all_blank = fold_left (fun acc (s,_) -> acc && s = "") true es in
    if all_blank then EXPR_tuple (sr, List.map snd es) 
    else EXPR_record (sr, List.map (fun (s,e)-> s, me e) es)

  | EXPR_polyrecord (sr, es,e) ->
    EXPR_polyrecord (sr, List.map (fun (s,e)-> s, me e) es, me e)

  | EXPR_replace_fields (sr, e, es) ->
    EXPR_replace_fields (sr, me e, List.map (fun (s,e) -> s, me e) es)

  | EXPR_remove_fields (sr,e,ss) ->
    EXPR_remove_fields (sr, me e, ss)

  | EXPR_variant (sr, (s,e)) ->
    EXPR_variant (sr, ( s, me e))

  | EXPR_extension (sr, es,e) -> EXPR_extension (sr, List.map me es, me e)

  | EXPR_rptsum_type (sr,_,_)
  | EXPR_pclt_type (sr,_,_)
  | EXPR_record_type (sr,_)
  | EXPR_polyrecord_type (sr,_,_)
  | EXPR_variant_type (sr,_) ->
     clierrx "[flx_desugar/flx_macro.ml:613: E333] " sr 
     ("Record, polyrecord, variant or pointer to compact linear type types\n" ^
     "cannot be used as an expression")

(*
  | EXPR_record_type (sr,flds) -> EXPR_record_type (sr, List.map (fun (s,t) -> s, mt sr t) flds)
  | EXPR_polyrecord_type (sr,flds,v) -> EXPR_polyrecord_type (sr, List.map (fun (s,t) -> s, mt sr t) flds, mt sr v)
  | EXPR_variant_type (sr,flds) -> EXPR_variant_type (sr, List.map (fun (s,t) -> s, mt sr t) flds)
*)
  | EXPR_arrayof (sr, es) -> EXPR_arrayof (sr, List.map me es)
  | EXPR_coercion (sr, (e1, t)) -> EXPR_coercion (sr, (me e1,mt sr t))
  | EXPR_variant_subtype_match_coercion (sr, (e1, t)) -> EXPR_variant_subtype_match_coercion (sr, (me e1,mt sr t))
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
  | EXPR_effector (sr, (e1, e2, e3)) ->  EXPR_effector (sr,(me e1, me e2, me e3))
  | EXPR_longarrow (sr, (e1, e2)) ->  EXPR_longarrow (sr,(me e1, me e2))
  | EXPR_superscript (sr, (e1, e2)) ->  EXPR_superscript (sr,(me e1, me e2))

  | EXPR_literal (sr, literal) ->  e
  | EXPR_map (sr, f, e) -> EXPR_map (sr, me f, me e)
  | EXPR_deref (sr, e1) -> EXPR_deref (sr, me e1)
  | EXPR_ref (sr, e1) ->  EXPR_ref (sr, me e1)
  | EXPR_rref (sr, e1) ->  EXPR_rref (sr, me e1)
  | EXPR_wref (sr, e1) ->  EXPR_wref (sr, me e1)
  | EXPR_uniq (sr, e1) ->  EXPR_uniq (sr, me e1)
  | EXPR_likely (sr, e1) ->  EXPR_likely (sr, me e1)
  | EXPR_unlikely (sr, e1) ->  EXPR_unlikely (sr, me e1)
  | EXPR_new (sr, e1) ->  EXPR_new (sr, me e1)
  | EXPR_match_ctor (sr, (qn, e1)) -> EXPR_match_ctor (sr,(qn,me e1))
  | EXPR_match_variant_subtype (sr, (e, t)) -> 
      EXPR_match_variant_subtype (sr, (me e, mt sr t))

  | EXPR_match_ho_ctor (sr, (qn, e1)) -> EXPR_match_ho_ctor (sr,(qn,map me e1))
  | EXPR_match_variant (sr, (s, e1)) -> EXPR_match_variant (sr,(s,me e1))
  | EXPR_match_case (sr, (i, e1)) ->  EXPR_match_case (sr,(i, me e1))
  | EXPR_ctor_arg (sr, (qn, e1)) -> EXPR_ctor_arg (sr,(qn, me e1))
  | EXPR_ho_ctor_arg (sr, (qn, e1)) -> EXPR_ho_ctor_arg (sr,(qn, map me e1))
  | EXPR_variant_arg (sr, (s, e1)) -> EXPR_variant_arg (sr,(s, me e1))
  | EXPR_case_arg (sr, (i, e1)) ->  EXPR_case_arg (sr,(i,me e1))
  | EXPR_letin (sr, (pat, e1, e2)) -> 
    let pes = [pat, e2] in
    let pes = expand_pattern_branches pes in
    let pes =
      List.map
      (fun (pat,e) ->
        let pat = fix_pattern seq pat in
        let pvs = mac_get_pattern_vars pat in
        let pvs' =  (* new parameter names *)
          List.map
          (fun s -> let b = !seq in incr seq; s^"_param_" ^ local_prefix ^ "_" ^ string_of_int b)
          pvs
        in
        let fast_remap = List.combine pvs pvs' in
        let remap = 
          List.map2
          (fun x y -> (x,MName y))
          pvs pvs'
        in
        (* alpha convert pattern variable names *)
        let pat' = alpha_pat local_prefix seq fast_remap (remap @ macros) expand_expr pat in
        (* let pr = protect sr pvs in *)
        let e' = expand_expr recursion_limit local_prefix seq (remap @ macros) e in
        pat',e'
      )
      pes
    in
    EXPR_match (sr,(me e1, pes))


  | EXPR_get_n (sr, (i, e1)) ->  EXPR_get_n (sr,(i,me e1))
  | EXPR_get_named_variable (sr, (i, e1)) ->  EXPR_get_named_variable (sr,(i,me e1))
  | EXPR_as (sr, (e1, id)) ->  EXPR_as (sr,(me e1, mi sr id))
  | EXPR_as_var (sr, (e1, id)) ->  EXPR_as_var (sr,(me e1, mi sr id))

  | EXPR_match (sr, (e1, pes)) ->
    let pes = expand_pattern_branches pes in
    let pes =
      List.map
      (fun (pat,e) ->
        let pat = fix_pattern seq pat in
        let pvs = mac_get_pattern_vars pat in
        let pvs' =  (* new parameter names *)
          List.map
          (fun s -> let b = !seq in incr seq; s^"_param_" ^ local_prefix ^ "_" ^ string_of_int b)
          pvs
        in
        let fast_remap = List.combine pvs pvs' in
        let remap = 
          List.map2
          (fun x y -> (x,MName y))
          pvs pvs'
        in
        (* alpha convert pattern variable names *)
        let pat' = alpha_pat local_prefix seq fast_remap (remap @ macros) expand_expr pat in
        (* let pr = protect sr pvs in *)
        let e' = expand_expr recursion_limit local_prefix seq (remap @ macros) e in
        pat',e'
      )
      pes
    in
    EXPR_match (sr,(me e1, pes))

  | EXPR_type_match (sr, (e,ps)) ->
    let ps = List.map (fun (pat,e) -> pat, mt sr e) ps in
    EXPR_type_match (sr,(mt sr e,ps))

  | EXPR_subtype_match (sr, (e,ps)) ->
    let ps = List.map (fun (pat,e) -> pat, mt sr e) ps in
    EXPR_subtype_match (sr,(mt sr e,ps))

  | EXPR_typecase_match (sr, (t,ps)) ->
    let ps = List.map (fun (t,e) -> mt sr t, me e) ps in
    EXPR_typecase_match (sr,(mt sr t,ps))

  | EXPR_ellipsis _
  | EXPR_void _ -> e

  | EXPR_typeof (sr,e) -> EXPR_typeof (sr, me e)
  | EXPR_range_check (sr, mi, v, mx) -> EXPR_range_check (sr, me mi, me v, me mx)
  | EXPR_not (sr,e) -> EXPR_not (sr, me e)
  | EXPR_label (sr,s) -> EXPR_label (sr, mi sr s)

and rqmap me sr reqs =
  let r req = rqmap me sr req in
  match reqs with
  | RREQ_or (a,b) -> RREQ_or (r a, r b)
  | RREQ_and (RREQ_true,b) -> r b
  | RREQ_and (a,RREQ_true) -> r a
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
      | Named_index_req s ->
        let x = me (EXPR_name (sr,s,[])) in
(*
print_endline ("named req " ^ s ^ " expanded to " ^ string_of_expr x);
*)
        begin try
          match x with
          | EXPR_literal (_,{Flx_literal.internal_value=v}) ->   
            let n = int_of_string v in
            RREQ_atom (Index_req n) 
          | _ -> raise Not_found
        with _ ->
        let err = "[Flx_reqs] rqmap: Named index requirement " ^ s ^ " not defined\n" ^
          "A macro with that name defined as an integer\n" ^
          "is required for the concordance which allows the compiler\n"  ^
          "to refer directly to symbols defined in the library\n"
        in 
          clierr sr err
        end


      | x -> RREQ_atom x

(* ---------------------------------------------------------------------
  do the common work of both subst_statement and expand_statement,
  recursion to the appropriate one as indicated by the argument 'recurse'

  The flag 'reachable' is set to false on exit if the instruction
  does not drop through. The flag may be true or false on entry.
  Whilst the flag is false, no code is generated. Once the flag
  is false, a label at the low level can cause subsequent code to become
  reachble.
*)

and subst_or_expand recurse recursion_limit local_prefix seq reachable macros (st:statement_t):statement_t list =
  (*
  print_endline ("Subst or expand: " ^ string_of_statement 0 st);
  *)
  (* NOTE: skips increment of recursion limit, only used unpacking if do elif .. chains *)
  let ms' reachable s = recurse recursion_limit local_prefix seq reachable macros s in
  let recursion_limit = recursion_limit - 1 in
  let mt sr e = expand_type_expr sr recursion_limit local_prefix seq macros e in
  let me e = expand_expr recursion_limit local_prefix seq macros e in
  let meopt e = match e with | None -> None | Some x -> Some (me x) in
  let mps sr ps =
     let rec aux ps = match ps with
     | Satom (sr,k,id,t,d) -> Satom (sr,k,id,mt sr t,meopt d)
     | Slist ps -> Slist (map aux ps)
     in
     aux ps 
  in
  let mpsp sr (ps,pre) = mps sr ps,meopt pre in
  let rqmap sr req = rqmap me sr req in
  let ms s = recurse recursion_limit local_prefix seq (ref true) macros s in
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
  | STMT_virtual_type (sr, name) -> tack (STMT_virtual_type (sr, mi sr name))

  | STMT_circuit (sr,cs) -> tack st
    
  | STMT_try _ -> tack st
  | STMT_type_error _ -> tack st
  | STMT_type_assert _ -> tack st

  | STMT_endtry _ -> 
    reachable := true;
    tack st

  | STMT_catch (sr, s, t) -> 
    reachable := true;
    tack (STMT_catch (sr, s, mt sr t))

  | STMT_private (sr,st) ->
    List.iter (fun st -> tack (STMT_private (sr,st))) (ms [st])

  | STMT_seq (_,sts) ->
    List.iter tack (ms sts)

  | STMT_include (sr, s) -> tack st

  (* FIX TO SUPPORT IDENTIFIER RENAMING *)
  | STMT_open (sr, vs, qn) ->
    tack (STMT_open (sr, vs, mq qn))

  | STMT_inject_module (sr, vs, qn) -> 
    tack (STMT_inject_module (sr, vs, mq qn))

  (* FIX TO SUPPORT IDENTIFIER RENAMING *)
  | STMT_use (sr, id, qn) -> tack (STMT_use (sr,mi sr id,qn))

  | STMT_cassign (sr,l,r) -> tack (STMT_cassign (sr, me l, me r))
  | STMT_storeat (sr,l,r) -> tack (STMT_storeat (sr, me l, me r))

  | STMT_assign (sr,name,l,r) ->
    let l = match l with
      | `Expr (sr,e),t -> `Expr (sr,me e),t
      | l -> l
    in
    tack (STMT_assign (sr, name, l, me r))

  | STMT_comment _  ->  tack st

  | STMT_union (sr, id, vs, idts ) ->
    let idts = List.map (fun (id,v,vs,d,c) -> 
      id,v,vs,mt sr d, (match c with | None-> None | Some c -> Some (mt sr c))) 
      idts 
    in
    tack (STMT_union (sr, mi sr id, vs, idts))

  | STMT_struct (sr, id, vs, idts) ->
    let idts = List.map (fun (id,t) -> id,mt sr t) idts in
    tack (STMT_struct (sr, mi sr id, vs, idts))

  | STMT_cstruct (sr, id, vs, idts, reqs) ->
    let idts = List.map (fun (id,t) -> id,mt sr t) idts in
    tack (STMT_cstruct (sr, mi sr id, vs, idts, rqmap sr reqs))

  | STMT_typeclass (sr, id, vs, sts) ->
    tack (STMT_typeclass (sr, mi sr id, vs, ms sts))

  | STMT_begin_typeclass _ -> assert false

  | STMT_type_alias (sr, id, vs, t) ->
    tack (STMT_type_alias (sr,mi sr id,vs, mt sr t))

  | STMT_inherit (sr, id, vs, t) ->  tack st
  | STMT_inherit_fun (sr, id, vs, t) ->  tack st

  | STMT_ctypes (sr, ids, qs, reqs) ->
    List.iter
    (fun (sr,id) ->
      let id = mi sr id in
      let st = STMT_abs_decl (
        sr,
        id,
        dfltvs,
        qs,
        Flx_code_spec.Str ("::"^id),
        rqmap sr reqs)
      in
      tack st
    )
    ids

  | STMT_abs_decl (sr,id,vs,typs,v,rqs) ->
    tack (STMT_abs_decl (sr,mi sr id,vs,typs,v, rqmap sr rqs))

  | STMT_newtype (sr,id,vs,t) ->
    tack (STMT_newtype (sr,mi sr id,vs,mt sr t))

  | STMT_instance_type (sr,id,vs,t) ->
    tack (STMT_instance_type (sr,mi sr id,vs,mt sr t))

  | STMT_callback_decl (sr,id,args,ret,rqs) ->
    tack (STMT_callback_decl (sr,mi sr id, List.map (mt sr) args,mt sr ret,rqmap sr rqs))

  | STMT_const_decl (sr, id, vs, t, c, reqs) ->
     tack (STMT_const_decl (sr, mi sr id, vs, mt sr t, c, rqmap sr reqs))

  | STMT_fun_decl (sr, id, vs, ts, t, c, reqs,prec) ->
    tack (STMT_fun_decl (sr, mi sr id, vs, List.map (mt sr) ts, mt sr t, c, rqmap sr reqs,prec))

  | STMT_insert (sr, n, vs, s, ikind, reqs) ->
    tack (STMT_insert (sr,n,vs,s, ikind, rqmap sr reqs))

    (*
      NOTE: c code is embedded even  though it isn't
      reachable because it might contain declarations or
      even labels
    *)
  | STMT_code (sr, s, e) ->
    tack (STMT_code (sr,s,me e));
    reachable := true

  | STMT_noreturn_code (sr, s, e) ->
    tack (STMT_noreturn_code (sr,s,me e));
    reachable := false

  (* IDENTIFIER RENAMING NOT SUPPORTED IN EXPORT *)
  | STMT_export_python_fun (sr, sn, s) ->  tack st
  | STMT_export_fun (sr, sn, s) ->  tack st
  | STMT_export_cfun (sr, sn, s) ->  tack st
  | STMT_export_type (sr, sn, s) ->  tack st
  | STMT_export_struct (sr, s) ->  tack st
  | STMT_export_union (sr, sn, s) ->  tack st
  | STMT_export_requirement (sr,reqs) -> tack st
  | STMT_label (sr, id) ->
    reachable:=true;
    tack (STMT_label (sr, mi sr id))

  | STMT_goto (sr, id) ->
    ctack (STMT_goto (sr, mi sr id));
    reachable := false

  | STMT_cgoto (sr, e) ->
    ctack (STMT_cgoto (sr, me e));
    reachable := false

  | STMT_svc (sr, id) ->  ctack (STMT_svc (sr, mi sr id))
  | STMT_proc_return (sr)  ->  ctack st; reachable := false
  | STMT_proc_return_from (sr,s)  ->  ctack st; reachable := false
  | STMT_halt (sr,s)  ->  ctack st; reachable := false
  | STMT_trace (sr,v,s)  ->  ctack st
  | STMT_nop (sr, s) ->  ()

  | STMT_reduce (sr, id, reds) ->
    let reds = map (fun (vs, ps, e1, e2) -> 
      let ps = List.map (fun (s,t)-> s,mt sr t) ps in
      vs,ps,me e1, me e2)
    reds 
    in 
    tack(STMT_reduce (sr, mi sr id, reds))

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

  | STMT_function (sr, id, vs, psp, (t,post), effects, props, sts ) ->
    let rec aux ps : string list =
      match ps with
      | Satom (sr,x,name,z,d)->[name] 
      | Slist ps -> List.concat (List.map aux ps)
    in
    let pr = aux (fst psp) in       
    let post = meopt post in
    tack(STMT_function (sr, mi sr id, vs, mpsp sr psp, (mt sr t, post), mt sr effects, props, msp sr pr sts ))

  | STMT_curry (sr,id,vs,pss,(ret,post),effects,kind,adjs,sts) ->
    let rec aux ps : string list =
      match ps with
      | Satom (sr,x,name,z,d)->[name] 
      | Slist ps -> List.concat (List.map aux ps)
    in
       
    let pr = List.concat (List.map (fun pc -> aux (fst pc)) pss) in
    let post = match post with | None -> None | Some x -> Some (me x) in
    let pss = List.map (fun psp -> mpsp sr psp) pss in
    tack(STMT_curry(sr, mi sr id, vs, pss, (ret,post),effects,kind, adjs, msp sr pr sts ))

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

  | STMT_once_decl (sr, id, vs, optt, opte) ->
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
      tack (STMT_once_decl (sr, mi sr id, vs, optt, opte))


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

  | STMT_library (sr, id, sts) ->
    tack (STMT_library (sr, mi sr id, ms sts))


  (* this gets called twice, pointlessly *)
  | STMT_stmt_match (sr, (e, pss)) ->
    (*
    print_endline "Handling statement match";
    *)
    (* as with other conditionals, it is possible to jump into the middle
     * of a branch, so even if the whole statement is unreachable,
     * some of the branch handling code may be if it contains a label.
     * The end of the statement is reachable if the end of any branch
     * is reachable.
     *
     * So, special handling: if the statement isn't reachable,
     * we can drop the matching entirely, and just emit the reachable
     * parts of each branch (from the first label). The inner call to subst_statements
     * should already have elided the unreachable heads of the branches.
     *)

    let start_reachable = !reachable in
    let case_end_reachable = ref false in
    let end_label = "_degen_stmt_match_end_" ^ local_prefix ^ "_" ^ string_of_int (let n = !seq in incr seq; n) in
    let pss = expand_pattern_branches pss in
    let pss = List.map (fun (pat,sts) ->
      let pat = fix_pattern seq pat in
      let pvs = mac_get_pattern_vars pat in
      let pvs' =  (* new parameter names *)
        List.map
        (fun s -> let b = !seq in incr seq; s^"_param_" ^ local_prefix ^ "_" ^ string_of_int b)
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
      let branch_reachable = ref start_reachable in
      let sts' = subst_statements 50 local_prefix seq branch_reachable remap sts in
      case_end_reachable := !branch_reachable || !case_end_reachable;
      (*
      print_endline ("Statement match, original pattern: " ^ string_of_pattern pat);
      print_endline ("Statement match, original statements: " ^ string_of_statements sts);
      print_endline ("Statement match, new pattern: " ^ string_of_pattern pat');
      print_endline ("Statement match, new statements: " ^ string_of_statements sts');
      *)
      !branch_reachable, (pat', ms sts') (* no need for protection because pat vars are fresh *)
      )
      pss 
    in
    if start_reachable then
      tack (STMT_stmt_match (sr, (me e, List.map snd pss)))
    else begin
      List.iter (fun (r,(_,ss)) ->
        (List.iter tack ss);
        if r then tack (STMT_goto (sr,end_label))
      )
      pss
      ;
      if !case_end_reachable then tack (STMT_label (sr,end_label))
    end
    ;
    reachable := !case_end_reachable 
    
  | STMT_instance (sr, vs, qn, sts) ->
    tack (STMT_instance (sr, vs, mq qn, ms sts))

  | STMT_ifcgoto (sr, e1 , e2) ->
    let e1 = me e1 in
    let e1 = cf e1 in
    let e2 = me e2 in
    let e2 = cf e2 in
    begin match truthof e1 with
    | Some true ->
      ctack (STMT_cgoto (sr,e2));
      reachable := false

    | Some false -> ()
    | None ->
      ctack (STMT_ifcgoto (sr, e1, e2))
    end

  | STMT_ifgoto (sr, e , id) ->
    let e = me e in
    let e = cf e in
    begin match truthof e with
    | Some true ->
      ctack (STMT_goto (sr,mi sr id));
      reachable := false

    | Some false -> ()
    | None ->
      ctack (STMT_ifgoto (sr, e, mi sr id))
    end

  | STMT_init (sr,v,e) ->
    ctack (STMT_init (sr, mi sr v, me e))

  | STMT_assert (sr,e) ->
    let e = me e in
    begin match truthof e with
    | Some true -> () 
    (* check at run time: even if always fails, assert triggers only if control flows thru *)
    | _ -> 
      ctack (STMT_assert (sr,e))
    end

  | STMT_ifreturn (sr, e) ->
    let e = me e in
    begin match truthof e with
    | Some true ->
      ctack (STMT_proc_return sr);
      reachable := false
    | Some false -> ()
    | None ->
      let n = !seq in incr seq;
      let lab = "_ifret_" ^ local_prefix ^ "_" ^ string_of_int n in
      ctack (STMT_ifgoto (sr, EXPR_not (sr,e) , lab));
      ctack (STMT_proc_return sr);
      ctack (STMT_label (sr,lab))
    end

  | STMT_invariant (sr, e) -> ctack (STMT_invariant (sr, me e))

  | STMT_ifdo (sr, e, sts1, sts2) ->
    let e = me e in
    let e = cf e in
    begin match truthof e with
    | Some true ->
      List.iter ctack (ms sts1)
    | Some false ->
      List.iter ctack (ms sts2)
    | None ->
      let n1 = !seq in incr seq;
      let n2 = !seq in incr seq;
      let lab1 = "_ifdoend_" ^ local_prefix ^ "_" ^ string_of_int n1 in
      let lab2 = "_ifdoelse_" ^ local_prefix ^ "_" ^ string_of_int n2 in
      (*
      print_endline ("Assigned labels " ^ lab1 ^ " and " ^ lab2);
      *)

      (* each branch has the initial reachability we start with.
         NOTE! Labels are allowed inside primitive conditionals!
         So even if the initial condition is 'unreachable',
         the end of a branch can still be reachable!!

         So we must tack, not ctack, the code of the inner
         compound statements, they're NOT blocks.

         BUT NOTE EXCEPTION IF THE EXPRESSION IS CONSTANT!
      *)
      begin match e with
      | EXPR_not (_,e') ->
        ctack (STMT_ifgoto (sr, e', lab1))
      | _ ->
        ctack (STMT_ifgoto (sr, EXPR_not (sr,e), lab1))
      end
      ;
      let r1 = ref !reachable in
      List.iter tack (ms' r1 sts1);
      if !r1 then tack (STMT_goto (sr,lab2));

      (* this is a ctack, because it can only be targetted by prior ifnotgoto *)
      ctack (STMT_label (sr,lab1));
      let r2 = ref !reachable in
      List.iter tack (ms' r2 sts2);
      if !r1 then tack (STMT_label (sr,lab2));
      reachable := !r1 || !r2
    end


  | STMT_call_with_trap (sr, e1, e2) ->
    ctack (STMT_call_with_trap (sr, me e1, me e2));

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

  | STMT_scheme_string _
  | STMT_call _
  | STMT_macro_forall _
  | STMT_macro_val _  -> assert false

  (*
  | st -> failwith ("[subst_or_expand] Unhandled case " ^ string_of_statement 0 st)
  *)
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
  | STMT_macro_val (sr, ids, e) ->
    tack (STMT_macro_val (sr, List.map (mi sr) ids, me e))

  | STMT_macro_forall (sr,ids,e,sts) ->
    tack (STMT_macro_forall (sr, List.map (mi sr) ids,me e,mss sts))

  | STMT_call (sr, e1, e2) ->
    tack (STMT_call (sr, me e1, me e2))

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
        | Some (MName x) -> [x]
        | Some(_) -> [name] (* clierrx "[flx_desugar/flx_macro.ml:1291: E334] " sr "Name list required" *)
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
        | Some (MName x) ->
          expand_exprs sr [EXPR_name(sr,x,[])]

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
  | STMT_macro_val (sr, ids, e) ->
    let e = me e in
    let n = List.length ids in
    if n = 1 then
      ref_macros := (List.hd ids, MVal e) :: !ref_macros
    else begin
      let vs =
        match e with
        | EXPR_tuple (_,ls) -> ls
        | _ -> clierrx "[flx_desugar/flx_macro.ml:1336: E335] " sr "Unpack non-tuple"
      in
      let m = List.length vs in
      if m <> n then
        clierrx "[flx_desugar/flx_macro.ml:1340: E336] " sr
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

  | STMT_macro_forall (sr, ids, e, sts) ->
    (*
    print_endline "Expanding forall";
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
            | _ -> clierrx "[flx_desugar/flx_macro.ml:1376: E337] " sr ("Unpack non-tuple " ^ string_of_expr e)
          in
          let m = List.length vs in
          if m <> n then
            clierrx "[flx_desugar/flx_macro.ml:1380: E338] " sr
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

  (* _scheme "(blah blah)" translates to an AST 
   * by compiling and evaluating the string argument as
   * scheme and then translating the resulting s-expression
   * into Felix using ocs2sex and then sex2flx, the same as 
   * the action code of a parse is handled.
   *)

  | STMT_call (sr,
      EXPR_name(srn,"_scheme", []),
      EXPR_literal (srl, {Flx_literal.felix_type="string"; internal_value=s})
    ) -> 
    print_endline "DETECTED STATEMENT ENCODED AS SCHEME";
    let sex = scheme_eval s in
    let flx = Flx_sex2flx.xstatement_t sr sex in
    print_endline "s-expression converted to Felix!";
    print_endline (string_of_statement 0 flx);
    ctack flx

  | STMT_call (sr, e1, e2) -> ctack (STMT_call (sr, me e1, me e2))

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
  List.rev
    (List.fold_left 
      (fun acc st ->
        let sts = expand_statement recursion_limit local_prefix seq reachable ref_macros macros st in
        List.fold_left (fun acc st -> st::acc) acc sts
      )
      []
      ss
    )
  
let expand_macros macro_state stmts =
  (* translate class X; ... into class X { ...  *)
  let rec grab stmts res =
    match stmts with
    | [] -> List.rev res,[]
    | STMT_begin_typeclass _ :: _ -> List.rev res, stmts
    | h :: t -> grab t (h :: res) 
  in
  let rec aux stmts res =
    match stmts with
    | [] -> List.rev res
    | STMT_begin_typeclass (sr,name,vs) :: stmts ->
      let nested,rest = grab stmts [] in
      aux rest (STMT_typeclass (sr,name,vs,nested) :: res)

    | h :: t ->
      aux t (h::res)
  in 
  let stmts = aux stmts [] in

  expand_statements
    macro_state.recursion_limit
    macro_state.local_prefix
    macro_state.seq
    macro_state.reachable
    macro_state.macros
    stmts


let make_macro_state ?(recursion_limit=5000) local_prefix seq =
  {
    recursion_limit = recursion_limit;
    local_prefix = local_prefix;
    seq = seq;
    reachable = ref true;
    ref_macros = ref [];
    macros = [];
  }

