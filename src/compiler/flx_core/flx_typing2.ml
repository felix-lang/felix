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

let paramtype (params : parameter_t list) =
  let typlist params =
    map (fun (k,_,t,_) -> t) params
  in
  type_of_list (typlist params)

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

let rec typecode_of_expr (e:expr_t) :typecode_t =
  let te e = typecode_of_expr e in
  match e with
  | EXPR_name (_,"TYPE",[]) -> TYP_type
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
  | EXPR_record_type (sr,es) -> TYP_record es
  | EXPR_polyrecord_type (sr,es,e) -> TYP_polyrecord (es,e)
  | EXPR_variant_type (sr,es) -> TYP_variant es

  | EXPR_product (_,ts) -> TYP_tuple (map te ts)
  | EXPR_intersect (_,ts) -> TYP_intersect (map te ts)
  | EXPR_isin (_,(a,b)) -> TYP_isin (te a, te b)
  | EXPR_arrow (_,(a,b)) -> TYP_function (te a, te b)
  | EXPR_longarrow (_,(a,b)) -> TYP_cfunction (te a, te b)
  | EXPR_superscript (_,(a,b)) -> TYP_array (te a, te b)
(*  | EXPR_lvalue (sr,e) -> TYP_lvalue (te e) *)
  | EXPR_ref (sr,e) -> TYP_pointer (te e)
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

  | EXPR_typeof (_,e) -> TYP_typeof e
  | EXPR_as (sr,(t,x)) -> TYP_as (te t,x)

  | EXPR_literal (sr, ({Flx_literal.felix_type=t; internal_value=v} as l) ) ->
    if t <> "int"
    then
      clierr sr
      (
        "Only plain integer can be used as a type, got '" ^
       string_of_literal l ^
        "'"
      )
    else
    let v = ref
      begin try int_of_string v
      with _ -> clierr sr "Integer used as type out of range"
      end
    in
      if !v <0 then clierr sr "Negative int not allowed as type"
      else if !v = 0 then TYP_void sr
      else if !v = 1 then TYP_tuple []
      else TYP_unitsum !v

  (* NOTE SPECIAL NAME HANDLING HACKS!! *)
  | EXPR_apply (sr, (e1, e2)) ->
      begin match e1 with
      | EXPR_name (_, "_isin", []) ->
          begin match typecode_of_expr e2 with
          | TYP_type_tuple [memt; sett] -> TYP_isin (memt, sett)
          | _ ->
              (* this can be fixed by taking projections but I can't be bothered
               * atm *)
              failwith (
                "Implementation limitation, 'isin' operator requires two " ^
                "explicit arguments")
          end
      | EXPR_name (_, "~", []) -> TYP_dual (typecode_of_expr e2)
      | EXPR_name (_, "typeof", []) -> TYP_typeof e2
      | EXPR_name (sr, "pow", []) -> 
          begin match e2 with
          | EXPR_tuple (_,[s1;s2]) -> TYP_tuple_cons ( sr, typecode_of_expr s1, typecode_of_expr s2)
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
             | [[],None],TYP_none ->
              TYP_function (TYP_tuple [],t)
             | _ ->
             let params = map (fun (x,y,z,d)-> y,z) params in
             TYP_typefun
             (
               params,
               ret,
               t
             )
           with _ ->
             clierr sr
             "Type lambda must return type expression"
         end

       | _ ->
         clierr sr
         "Type lambda must just be 'return type_expr'"
       end
     | _ ->
       clierr sr
       "Type lambda only allowed one argument (arity=1)"
     end

  | EXPR_type_match (sr,(e,ps)) ->
    TYP_type_match (e,ps)

  | EXPR_noexpand (sr,e) -> te e

  | EXPR_patvar (sr,s) -> TYP_patvar (sr,s)
  | EXPR_patany sr -> TYP_patany sr

  | EXPR_extension (sr, bases, extension) ->
    TYP_type_extension (sr,List.map te bases, te extension)

  | _ ->
    let sr = src_of_expr e in
    clierr sr ("Type expression expected, got " ^ string_of_expr e)
