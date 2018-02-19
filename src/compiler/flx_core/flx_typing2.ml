open Flx_ast
open Flx_types
open Flx_print
open Flx_exceptions
open List

let qualified_name_of_expr e =
  match e with
  | EXPR_void sr -> Some (`AST_void sr)
  | EXPR_name (sr,name,ts) -> Some (`AST_name (sr,name,ts))
  | EXPR_case_tag (sr,v) -> Some (`AST_case_tag (sr,v))
  | EXPR_typed_case (sr,v,t) -> Some (`AST_typed_case (sr,v,t))
  | EXPR_lookup (sr,(e,name,ts)) -> Some (`AST_lookup (sr,(e,name,ts)))
  | EXPR_index (sr,name,index) -> Some (`AST_index (sr,name,index))
  | EXPR_callback (sr,name) -> Some (`AST_callback (sr,name))
  | _ -> None

let suffixed_name_of_expr e =
  match e with
  | EXPR_suffix (sr,(name,ts)) -> Some (`AST_suffix (sr,(name,ts)))
  | _ -> qualified_name_of_expr e

let expr_of_qualified_name e =
  match e with
  | `AST_void sr -> EXPR_void sr
  | `AST_name (sr,name,ts) -> EXPR_name (sr,name,ts)
  | `AST_case_tag (sr,v) -> EXPR_case_tag (sr,v)
  | `AST_typed_case (sr,v,t) -> EXPR_typed_case (sr,v,t)
  | `AST_lookup (sr,(e,name,ts)) -> EXPR_lookup (sr,(e,name,ts))
  | `AST_index (sr,name,index) -> EXPR_index (sr,name,index)
  | `AST_callback (sr,name) -> EXPR_callback (sr,name)

let expr_of_suffixed_name e =
  match e with
  | `AST_void sr -> EXPR_void sr
  | `AST_name (sr,name,ts) -> EXPR_name (sr,name,ts)
  | `AST_case_tag (sr,v) -> EXPR_case_tag (sr,v)
  | `AST_typed_case (sr,v,t) -> EXPR_typed_case (sr,v,t)
  | `AST_lookup (sr,(e,name,ts)) -> EXPR_lookup (sr,(e,name,ts))
  | `AST_index (sr,name,index) -> EXPR_index (sr,name,index)
  | `AST_callback (sr,name) -> EXPR_callback (sr,name)
  | `AST_suffix (sr,(name,ts)) -> EXPR_suffix (sr,(name,ts))

let type_of_list = function
  | [x] -> x
  | x -> TYP_tuple x

let kind_of_list = function
  | [x] -> x
  | x -> KND_tuple x

let all_tunits ts =
  try
    iter
    (fun t ->
      if t <> TYP_tuple []
      then raise Not_found
    )
    ts;
    true
  with Not_found -> false

let rec kindcode_of_expr (e:expr_t) :kindcode_t =
  let te e = kindcode_of_expr e in
  match e with
  | EXPR_name (_,"TYPE",[]) -> KND_type
  | EXPR_name (_,"GENERIC",[]) -> KND_generic
  | EXPR_tuple (sr,ls) ->
    begin match ls with
    | [] -> KND_tuple [] (* HACK!! *)
    | [x] -> failwith "Unexpected one element tuple converting to type tuple"
    | _ -> KND_tuple (map te ls)
    end
  | EXPR_arrow (_,(a,b)) -> KND_function (te a, te b)
  | _ -> assert false

let rec kindcode_of_typecode (t:typecode_t) : kindcode_t =
  let kt t = kindcode_of_typecode t in
  match t with
  | TYP_name (_,"GENERIC",[]) -> KND_generic 
  | TYP_name (_,"TYPE",[]) -> KND_type
  | TYP_type_tuple ts -> KND_tuple (List.map kt ts) 
  | TYP_function (d,c) -> KND_function (kt d, kt c) 
  | _ -> 
   failwith ("Typecode can't convert to kindcode: " ^ string_of_typecode t)

let rec typecode_of_expr (e:expr_t) :typecode_t =
  let te e = typecode_of_expr e in
  match e with
  | EXPR_rptsum_type (sr,n,t) -> TYP_rptsum (n,t)
  | EXPR_pclt_type (_,d,c) -> TYP_pclt (d,c)
  | EXPR_name (_,"LABEL",[]) -> TYP_label
  | EXPR_name (sr,"DEFER",[]) -> TYP_defer (sr,ref None)
  | EXPR_name (sr,"_",[]) -> TYP_patany sr
  | EXPR_ellipsis _ -> TYP_ellipsis
  | EXPR_void sr -> TYP_void sr
  | EXPR_name (sr,name,ts) -> TYP_name (sr,name,ts)
  | EXPR_case_tag (sr,v) -> TYP_case_tag (sr,v)
  | EXPR_typed_case (sr,v,t) -> TYP_typed_case (sr,v,t)
  | EXPR_lookup (sr,(e,name,ts)) -> TYP_lookup (sr,(e,name,ts))
  | EXPR_index (sr,name,index) -> TYP_index (sr,name,index)
  | EXPR_callback (sr,name) -> TYP_callback (sr,name)
  | EXPR_suffix (sr,(name,suffix)) -> TYP_suffix (sr,(name,suffix))
  | EXPR_tuple (sr,ls) ->
    begin match ls with
    | [] -> TYP_tuple [] (* HACK!! *)
    | [x] -> failwith "Unexpected one element tuple converting to type tuple"
    | _ -> TYP_type_tuple (map te ls)
    end
  | EXPR_record_type (sr,es) -> 
    let all_blank = fold_left (fun acc (s,_) -> acc && s = "") true es in
    if all_blank then TYP_tuple (List.map snd es) 
    else TYP_record es

  | EXPR_polyrecord_type (sr,es,e) -> TYP_polyrecord (es,e)
  | EXPR_variant_type (sr,es) -> TYP_variant es

  | EXPR_product (_,ts) -> TYP_tuple (map te ts)
  | EXPR_intersect (_,ts) -> TYP_intersect (map te ts)
  | EXPR_union (_,ts) -> TYP_union (map te ts)
  | EXPR_isin (_,(a,b)) -> TYP_isin (te a, te b)
  | EXPR_arrow (_,(a,b)) -> TYP_function (te a, te b)
  | EXPR_effector (_,(a,e,b)) -> TYP_effector (te a, te e, te b)
  | EXPR_longarrow (_,(a,b)) -> TYP_cfunction (te a, te b)
  | EXPR_superscript (_,(a,b)) -> TYP_array (te a, te b)
(*  | EXPR_lvalue (sr,e) -> TYP_lvalue (te e) *)
  | EXPR_ref (sr,e) -> TYP_pointer (te e)
  | EXPR_rref (sr,e) -> TYP_rref (te e)
  | EXPR_wref (sr,e) -> TYP_wref (te e)
  | EXPR_uniq (sr,e) -> TYP_uniq (te e)

  | EXPR_sum (_,ts) ->
    let ts = map te ts in
    if all_tunits ts then
      TYP_unitsum (length ts)
    else
      TYP_sum ts

  | EXPR_orlist (sr,ts) ->
    begin match ts with
    | [] -> assert false
    | [x] -> assert false
    | h :: t ->
      let llor = TYP_name (sr,"lor",[]) in
      fold_left (fun sum t -> TYP_apply (llor,TYP_type_tuple[sum; te t])) (te h) t
    end

  | EXPR_andlist (sr,ts) ->
    begin match ts with
    | [] -> assert false
    | [x] -> assert false
    | h :: t ->
      let lland = TYP_name (sr,"land",[]) in
      fold_left (fun sum t -> TYP_apply (lland,TYP_type_tuple [sum; te t])) (te h) t
    end

  | EXPR_not (sr,e) ->
    let lnot = TYP_name (sr,"lnot",[]) in
    TYP_apply (lnot, TYP_type_tuple [te e])

  | EXPR_typeof (_,e) -> TYP_typeof e
  | EXPR_as (sr,(t,x)) -> TYP_as (te t,x)

  | EXPR_literal (sr, ({Flx_literal.felix_type=t; internal_value=v} as l) ) ->
    if t <> "int"
    then
      clierrx "[flx_core/flx_typing2.ml:140: E264] " sr
      (
        "Only plain integer can be used as a type, got '" ^
       string_of_literal l ^
        "'"
      )
    else
    let v = ref
      begin try int_of_string v
      with _ -> clierrx "[flx_core/flx_typing2.ml:149: E265] " sr "Integer used as type out of range"
      end
    in
      if !v <0 then clierrx "[flx_core/flx_typing2.ml:152: E266] " sr "Negative int not allowed as type"
      else if !v = 0 then TYP_void sr
      else if !v = 1 then TYP_tuple []
      else TYP_unitsum !v

  (* NOTE SPECIAL NAME HANDLING HACKS!! *)
  | EXPR_apply (sr, (e1, e2)) ->
      begin match e1 with
      | EXPR_name (_, "\\in", []) ->
          begin match typecode_of_expr e2 with
          | TYP_type_tuple [memt; sett] -> TYP_isin (memt, sett)
          | _ ->
              (* this can be fixed by taking projections but I can't be bothered
               * atm *)
              failwith (
                "Implementation limitation, 'in' operator requires two " ^
                "explicit arguments")
          end
      | EXPR_name (_, "~", []) -> TYP_dual (typecode_of_expr e2)
      | EXPR_name (_, "typeof", []) -> TYP_typeof e2
      | EXPR_name (sr, "pow", []) -> 
          begin match e2 with
          | EXPR_tuple (_,[s1;s2]) -> TYP_tuple_cons ( sr, typecode_of_expr s1, typecode_of_expr s2)
          | _ -> assert false
          end

      | EXPR_name (sr, "tuple_snoc", []) -> 
          begin match e2 with
          | EXPR_tuple (_,[s1;s2]) -> TYP_tuple_snoc ( sr, typecode_of_expr s1, typecode_of_expr s2)
          | _ -> assert false
          end


      | EXPR_name (_, "\\cap", []) -> 
          begin match e2 with
          | EXPR_tuple (_,[s1;s2]) -> TYP_setintersection[typecode_of_expr s1; typecode_of_expr s2]
          | _ -> assert false
          end
      | EXPR_name (_, "\\cup", []) -> 
          begin match e2 with
          | EXPR_tuple (_, [s1;s2]) -> TYP_setunion [typecode_of_expr s1; typecode_of_expr s2]
          | _ -> assert false
          end
      | EXPR_name (_, "typesetof", []) ->
          begin match typecode_of_expr e2 with
          | TYP_type_tuple ls -> TYP_typeset ls
          | x -> TYP_typeset [x]
          end

      | EXPR_name (_, "\\&", []) -> 
          begin match e2 with
          | EXPR_tuple (_,[s1;s2]) -> TYP_intersect[typecode_of_expr s1; typecode_of_expr s2]
          | _ -> assert false
          end
      | EXPR_name (_, "\\|", []) -> 
          begin match e2 with
          | EXPR_tuple (_, [s1;s2]) -> TYP_union [typecode_of_expr s1; typecode_of_expr s2]
          | _ -> assert false
          end
      | _ ->
          TYP_apply (typecode_of_expr e1, typecode_of_expr e2)
      end

  | EXPR_lambda (sr,(kind,vs,paramss,ret,body)) ->
     begin match paramss with
     | [params,traint] ->
       (* constraint is ignored for now!! *)
       begin match body with
       | [STMT_fun_return (_,e)] ->
         begin
           try
             let t = typecode_of_expr e in
             match paramss,ret with

             (* special case, allows {t} to mean 1 -> t *)
             | [Slist [],None],TYP_none ->
               TYP_typefun ([],KND_type, t)

             | _ ->
               begin match params with
               | Satom (_,_,name,typ,_) ->
                 TYP_typefun ([name,kindcode_of_typecode typ], kindcode_of_typecode ret, t)
               | Slist ps ->
                 let kps  = List.map 
                 (
                   fun p -> match p with 
                   | Satom (_,_,name,typ,_) -> name,kindcode_of_typecode typ 
                   | Slist _ -> clierr sr "Type functions require non-nested parameter list"
                 )
                 ps  
                 in
                 TYP_typefun (kps, kindcode_of_typecode ret, t)
               end
           with _ ->
             clierrx "[flx_core/flx_typing2.ml:216: E267] " sr
             "Type lambda must return type expression"
         end

       | _ ->
         clierrx "[flx_core/flx_typing2.ml:221: E268] " sr
         "Type lambda must just be 'return type_expr'"
       end
     | _ ->
       clierrx "[flx_core/flx_typing2.ml:225: E269] " sr
       "Type lambda only allowed one argument (arity=1)"
     end

  | EXPR_type_match (sr,(e,ps)) ->
    TYP_type_match (e,ps)

  | EXPR_subtype_match (sr,(e,ps)) ->
    TYP_subtype_match (e,ps)


  | EXPR_noexpand (sr,e) -> te e

  | EXPR_patvar (sr,s) -> TYP_patvar (sr,s)
  | EXPR_patany sr -> TYP_patany sr

  | EXPR_extension (sr, bases, extension) ->
    TYP_type_extension (sr,List.map te bases, te extension)

  | _ ->
    let sr = src_of_expr e in
    clierrx "[flx_core/flx_typing2.ml:242: E270] " sr ("Type expression expected, got " ^ string_of_expr e)

(** Conversion function from TYP expression to EXPR expression *)
(* passing in a default source location (dsr) can fill in some missing info *)
let rec expr_of_typecode (dsr:Flx_srcref.t) (t:typecode_t) = 
  match t with 

  (* The following cannot be converted. There's no analagous expression in EXPR. *)
  | TYP_pclt (a,b) -> EXPR_pclt_type (dsr, a, b)
  | TYP_rptsum (a,b) -> EXPR_rptsum_type (dsr, a, b)

  | TYP_label -> clierrx "[flx_core/flx_typing2.ml:250: E271] " dsr ("expr_of_typecode: TYP_label")
  | TYP_none -> clierrx "[flx_core/flx_typing2.ml:251: E272] " dsr ("expr_of_typecode: TYP_none")
  | TYP_var _ -> clierrx "[flx_core/flx_typing2.ml:254: E275] " dsr ("expr_of_typecode: TYP_var")
  | TYP_defer _ -> clierrx "[flx_core/flx_typing2.ml:255: E276] " dsr ("expr_of_typecode: TYP_defer")
  | TYP_dual _ -> clierrx "[flx_core/flx_typing2.ml:256: E277] " dsr ("expr_of_typecode: TYP_dual")

  | TYP_cfunction (t1,t2) -> 
      let e1 = (expr_of_typecode dsr t1) in
      let e2 = (expr_of_typecode dsr t2) in
      EXPR_longarrow (dsr,(e1,e2))

  | TYP_array (t1,t2) -> 
      let e1 = (expr_of_typecode dsr t1) in
      let e2 = (expr_of_typecode dsr t2) in
      EXPR_superscript (dsr,(e1,e2))

  | TYP_unitsum (len) -> 
      EXPR_literal (dsr, ({Flx_literal.felix_type="int"; 
                           internal_value=(string_of_int len);
                           c_value=(string_of_int len)}))

  | TYP_type_tuple ls -> EXPR_tuple (dsr, List.map (expr_of_typecode dsr) ls)
  | TYP_type_match (e,ps) -> EXPR_type_match (dsr,(e,ps))
  | TYP_subtype_match (e,ps) -> EXPR_subtype_match (dsr,(e,ps))

  | TYP_type_extension (sr, bases, extension) -> 
      EXPR_extension (sr,
        List.map (expr_of_typecode dsr) bases, 
        (expr_of_typecode dsr extension))

  | TYP_variant _ ->
      clierrx "[flx_core/flx_typing2.ml:282: E278] " dsr ("Unable to convert " 
        ^ (string_of_typecode t)  
        ^ " to an expression. Seems incompatible.")

  (* This is a hack, we're trying to build a function out of nothing. *)
  | TYP_function (param,t) -> 
      EXPR_lambda (dsr,
        (`GeneratedInlineFunction,
        dfltvs,
        [Slist [],None],
        TYP_none,
        [STMT_fun_return (dsr,(expr_of_typecode dsr t))]))

  | TYP_typefun _ -> assert false
(*
  (* Also a hack. Treat with caution. *)
  | TYP_typefun (params, ret, t) -> 

      let params = map (fun (y,z)-> (dsr,`PVal,y,z,None)) params in
      EXPR_lambda (dsr,
        (`GeneratedInlineFunction,
        dfltvs,
        [params,None],
        ret,
        [STMT_fun_return (dsr,(expr_of_typecode dsr t))]))
*)

  (* The following can be converted *)
  | TYP_pointer t -> EXPR_ref (dsr,(expr_of_typecode dsr t))
  | TYP_rref t -> EXPR_rref (dsr,(expr_of_typecode dsr t))
  | TYP_wref t -> EXPR_wref (dsr,(expr_of_typecode dsr t))
  | TYP_uniq t -> EXPR_uniq (dsr, (expr_of_typecode dsr t))

  | TYP_void (sr) -> EXPR_void (sr)
  | TYP_name (sr, id, ts) -> EXPR_name (sr, id, ts)
  | TYP_case_tag (sr, i) -> EXPR_case_tag (sr, i)
  | TYP_typed_case (sr, i, ts) -> EXPR_typed_case (sr, i, ts)
  | TYP_lookup (sr, (ex,id,ts)) -> EXPR_lookup (sr, (ex,id,ts))
  | TYP_index (sr, name, index) -> EXPR_index (sr, name, index)
  | TYP_callback (sr, qn) -> EXPR_callback (sr, qn)
  | TYP_suffix (sr, (qn, t)) -> EXPR_suffix (sr, (qn, t))
  | TYP_patvar (sr,id) -> EXPR_patvar (sr,id)
  | TYP_patany (sr) -> EXPR_patany (sr)
  | TYP_typeof (e) -> EXPR_typeof (dsr, e)
  | TYP_ellipsis -> EXPR_ellipsis (dsr)

  | TYP_as (t, id) -> 
      let e = (expr_of_typecode dsr t) in
      EXPR_as (dsr, (e, id))

  | TYP_tuple (ts) -> 
      let exprs = (List.map (expr_of_typecode dsr) ts) in
      EXPR_tuple (dsr, exprs)

  | TYP_sum (ts) -> 
      let exprs = (List.map (expr_of_typecode dsr) ts) in
      EXPR_sum (dsr, exprs)

  | TYP_intersect (ts) -> 
      let exprs = (List.map (expr_of_typecode dsr) ts) in
      EXPR_intersect (dsr, exprs)

   | TYP_union (ts) -> 
      let exprs = (List.map (expr_of_typecode dsr) ts) in
      EXPR_union (dsr, exprs)
  
  | TYP_record (ids_and_ts) -> 
      EXPR_record_type (dsr, ids_and_ts)

  | TYP_polyrecord (ids_and_ts, t2) -> 
      let ids_and_es = 
          (List.map 
            (fun (id,t) -> (id, (expr_of_typecode dsr t))) 
            ids_and_ts) 
      in
      let e2 = (expr_of_typecode dsr t2) in
      EXPR_polyrecord (dsr, ids_and_es, e2)

  | TYP_effector (t1,t2,t3) -> 
      let e1= (expr_of_typecode dsr t1) in
      let e2 = (expr_of_typecode dsr t2) in
      let e3 = (expr_of_typecode dsr t3) in
      EXPR_effector (dsr, (e1, e2, e3))
    
  | TYP_isin (t1, t2) -> 
      let e1 = (expr_of_typecode dsr t1) in
      let e2 = (expr_of_typecode dsr t2) in
      EXPR_isin (dsr, (e1, e2))

  | TYP_apply (t1, t2) -> 
      let e1 = (expr_of_typecode dsr t1) in
      let e2 = (expr_of_typecode dsr t2) in
      EXPR_apply (dsr, (e1, e2))

  | TYP_tuple_cons (sr, t1, t2) -> 
      let e1 = EXPR_name (dsr, "pow", []) in
      let e2 = EXPR_tuple (dsr, 
        [(expr_of_typecode dsr t1);
         (expr_of_typecode dsr t2)]) 
      in
      EXPR_apply (dsr, (e1, e2))

  | TYP_tuple_snoc (sr, t1, t2) -> 
      let e1 = EXPR_name (dsr, "tuple_snoc", []) in
      let e2 = EXPR_tuple (dsr, 
        [(expr_of_typecode dsr t1);
         (expr_of_typecode dsr t2)]) 
      in
      EXPR_apply (dsr, (e1, e2))

  | TYP_setunion [t1;t2] -> 
      let e1 = EXPR_name (dsr, "\\cup", []) in
      let e2 = EXPR_tuple (dsr, 
        [(expr_of_typecode dsr t1);
         (expr_of_typecode dsr t2)]) 
      in
      EXPR_apply (dsr, (e1, e2))

  | TYP_setintersection [t1;t2] ->
      let e1 = EXPR_name (dsr, "\\cap", []) in
      let e2 = EXPR_tuple (dsr, 
        [(expr_of_typecode dsr t1);
         (expr_of_typecode dsr t2)]) 
      in
      EXPR_apply (dsr, (e1, e2))

  | TYP_typeset [x] -> 
      let e1 = EXPR_name (dsr, "typesetof", []) in
      let e2 = (expr_of_typecode dsr x) in
      EXPR_apply (dsr, (e1, e2))

  | TYP_typeset ls -> 
      let e1 = EXPR_name (dsr, "typesetof", []) in
      let e2 = EXPR_tuple (dsr, (List.map (expr_of_typecode dsr) ls)) in
      EXPR_apply (dsr, (e1, e2))

  | TYP_setintersection _
  | TYP_setunion _ -> 
      clierrx "[flx_core/flx_typing2.ml:401: E279] " dsr "expr_of_typecode: ignoring this case for now."
      
let string_of_type_name (t:typecode_t) = match t with
  | TYP_rptsum _ -> "TYP_rptsum"
  | TYP_pclt _ -> "TYP_pclt"
  | TYP_label -> "TYP_label"
  | TYP_none -> " TYP_none"
  | TYP_ellipsis -> "TYP_ellipsis"
  | TYP_void _ -> "TYP_void"
  | TYP_name _ -> " TYP_name"
  | TYP_case_tag _ -> " TYP_case_tag"
  | TYP_typed_case _ -> " TYP_typed_case"
  | TYP_lookup _ -> " TYP_lookup"
  | TYP_index _ -> " TYP_index"
  | TYP_callback _ -> " TYP_callback"
  | TYP_suffix _ -> " TYP_suffix"
  | TYP_patvar _ -> " TYP_patvar"
  | TYP_patany _ -> " TYP_patany"
  | TYP_tuple _ -> "TYP_tuple"
  | TYP_unitsum _ -> "TYP_unitsum"
  | TYP_sum _ -> "TYP_sum"
  | TYP_intersect _ -> "TYP_intersect"
  | TYP_union _ -> "TYP_union"
  | TYP_record _ -> "TYP_record"
  | TYP_polyrecord _ -> "TYP_polyrecord"
  | TYP_variant _ -> "TYP_variant"
  | TYP_function _ -> "TYP_function"
  | TYP_effector _ -> "TYP_effector"
  | TYP_cfunction _ -> "TYP_cfunction"
  | TYP_pointer _ -> "TYP_pointer"
  | TYP_rref _ -> "TYP_rref"
  | TYP_wref _ -> "TYP_wref"
  | TYP_uniq _-> "TYP_uniq"
  | TYP_array _ -> "TYP_array"
  | TYP_as _ -> "TYP_as"
  | TYP_var _ -> "TYP_var"
  | TYP_isin _ -> "TYP_isin"
  | TYP_defer _ -> "TYP_defer"
  | TYP_typeset _ -> "TYP_typeset"
  | TYP_setunion _ -> "TYP_setunion"
  | TYP_setintersection _ -> "TYP_setintersection"
  | TYP_dual _ -> "TYP_dual"
  | TYP_apply _ -> "TYP_apply"
  | TYP_typefun _ -> "TYP_typefun"
  | TYP_type_tuple _ -> "TYP_type_tuple"
  | TYP_type_match _ -> "TYP_type_match"
  | TYP_subtype_match _ -> "TYP_subtype_match"
  | TYP_type_extension _ -> "TYP_type_extension"
  | TYP_tuple_cons _ -> "TYP_tuple_cons"
  | TYP_tuple_snoc _ -> "TYP_tuple_snoc"
  | TYP_typeof _ -> "TYP_typeof"

