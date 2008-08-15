open Flx_ast
open Flx_types
open Flx_print
open Flx_srcref
open Flx_exceptions
open List

let qualified_name_of_expr e =
  match e with
  | #qualified_name_t as x -> x
  | _ ->
    failwith
    (
      "Qualified name expected in\n" ^
      short_string_of_src (src_of_expr e) ^
      "\nGot " ^ Flx_print.string_of_expr e
    )


let typeof_list = function
  | [x] -> x
  | x -> `TYP_tuple x

let paramtype params =
  let typlist params =
    map
    (fun (k,_,t,_) ->
      match k with
      | `PRef -> `TYP_pointer t
      | `PFun -> `TYP_function (`TYP_tuple [],t)
      | _ -> t
    )
    params
  in
  typeof_list (typlist params)

let all_tunits ts =
  try
    iter
    (fun t ->
      if t <> `TYP_tuple []
      then raise Not_found
    )
    ts;
    true
  with Not_found -> false

let rec typecode_of_expr (e:expr_t) :typecode_t =
  let te e = typecode_of_expr e in
  match e with
  | `AST_case (sr,e1,ls,e2) -> `TYP_case (te e1, ls, te e2)
  | `AST_name (_,"TYPE",[]) -> `TYP_type
  | `AST_name (sr,"_",[]) -> `AST_patany sr
  | `AST_ellipsis _ -> `TYP_ellipsis
  | #suffixed_name_t as x -> (x:>typecode_t)
  | `AST_tuple (sr,ls) ->
    begin match ls with
    | [] -> `TYP_tuple [] (* HACK!! *)
    | [x] -> failwith "Unexpected one element tuple converting to type tuple"
    | _ -> `TYP_type_tuple (map te ls)
    end
  | `AST_record_type (sr,es) -> `TYP_record es
  | `AST_variant_type (sr,es) -> `TYP_variant es

  | `AST_product (_,ts) -> `TYP_tuple (map te ts)
  | `AST_intersect (_,ts) -> `TYP_intersect (map te ts)
  | `AST_isin (_,(a,b)) -> `TYP_isin (te a, te b)
  | `AST_setintersection (_,ts) -> `TYP_setintersection (map te ts)
  | `AST_setunion (_,ts) -> `TYP_setunion (map te ts)
  | `AST_arrow (_,(a,b)) -> `TYP_function (te a, te b)
  | `AST_longarrow (_,(a,b)) -> `TYP_cfunction (te a, te b)
  | `AST_superscript (_,(a,b)) -> `TYP_array (te a, te b)
(*  | `AST_lvalue (sr,e) -> `TYP_lvalue (te e) *)
  | `AST_ref (sr,e) -> `TYP_pointer (te e)
  | `AST_sum (_,ts) ->
    let ts = map te ts in
    if all_tunits ts then
      `TYP_unitsum (length ts)
    else
      `TYP_sum ts

  | `AST_lift (sr,e) -> `TYP_lift (te e)

  | `AST_orlist (sr,ts) ->
    begin match ts with
    | [] -> assert false
    | [x] -> assert false
    | h :: t ->
      let llor = `AST_name (sr,"lor",[]) in
      fold_left (fun sum t -> `TYP_apply (llor,`TYP_type_tuple[sum; te t])) (te h) t
    end

  | `AST_andlist (sr,ts) ->
    begin match ts with
    | [] -> assert false
    | [x] -> assert false
    | h :: t ->
      let lland = `AST_name (sr,"land",[]) in
      fold_left (fun sum t -> `TYP_apply (lland,`TYP_type_tuple [sum; te t])) (te h) t
    end

  | `AST_typeof (_,e) -> `TYP_typeof e
  | `AST_as (sr,(t,x)) -> `TYP_as (te t,x)

  | `AST_literal (sr,`AST_int (enc,v)) ->
    if enc <> "int"
    then
      clierr sr
      (
        "Only plain integer can be used as a type, code= '" ^
        enc ^
        "'"
      )
    else
    let v = ref
      begin try Big_int.int_of_big_int v
      with _ -> clierr sr "Integer used as type out of range"
      end
    in
      if !v <0 then clierr sr "Negative int not allowed as type"
      else if !v = 0 then ((`AST_void sr) :> typecode_t)
      else if !v = 1 then `TYP_tuple[]
      else `TYP_unitsum !v

  (* NOTE SPECIAL NAME HANDLING HACKS!! *)
  | `AST_apply(sr,(e1,e2)) ->
    begin match e1 with
    | `AST_name (_,name,[]) ->
      let name' = name ^ "          " (* 10 chars *) in
      if name = "typeof" then `TYP_typeof e2
      else let arg = typecode_of_expr e2 in
      if name = "_isin" then
      begin
        match arg with
        | `TYP_type_tuple [memt; sett] ->
           `TYP_isin (memt, sett)
        | _ ->
          (* this can be fixed by taking projections but I can't be bothered atm *)
          failwith
           "Implementation limitation, 'isin' operator requires two explicit arguments"
      end
      else if name = "typesetof" then
      begin
        match arg with
        | `TYP_type_tuple ls -> `TYP_typeset ls
        | x -> `TYP_typeset [x]
      end
      else if name = "bnot" then `TYP_dual arg
      else if String.sub name' 0 5 = "proj_"
      then
        begin
          let acc = ref 0 in
          for i = 5 to String.length name - 1 do
          if name.[i] <= '9' && name.[i] >='0'
          then acc := 10 * !acc + Char.code (name.[i]) - Char.code '0'
          else
            clierr sr
            (
              "Digits expected in name '" ^ name ^ "' in\n" ^
              short_string_of_src sr
            )
          done;
          `TYP_proj (!acc, arg)
         end

      else if String.sub name' 0 9 = "case_arg_"
      then
        begin
          let acc = ref 0 in
          for i = 9 to String.length name - 1 do
          if name.[i] <= '9' && name.[i] >='0'
          then acc := 10 * !acc + Char.code (name.[i]) - Char.code '0'
          else
            clierr sr
            (
              "Digits expected in name '" ^ name ^ "' in\n" ^
              short_string_of_src sr
            )
          done;
          `TYP_case_arg (!acc, arg)
         end
      else
        `TYP_apply (typecode_of_expr e1,arg)

    | _ ->
      `TYP_apply (typecode_of_expr e1,typecode_of_expr e2)
    end

  | `AST_lambda (sr,(vs,paramss,ret,body)) ->
     begin match paramss with
     | [params,traint] ->
       (* constraint is ignored for now!! *)
       begin match body with
       | [`AST_fun_return (_,e)] ->
         begin
           try
             let t = typecode_of_expr e in
             match paramss,ret with
             (* special case, allows {t} to mean 1 -> t *)
             | [[],None],`TYP_none ->
              `TYP_function (`TYP_tuple [],t)
             | _ ->
             let params = map (fun (x,y,z,d)-> y,z) params in
             `TYP_typefun
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

  | `AST_type_match (sr,(e,ps)) ->
    `TYP_type_match (e,ps)

  | `AST_noexpand (sr,e) -> te e

  | `AST_patvar _ as e -> e
  | `AST_patany _ as e -> e
  | #expr_t ->
    let sr = src_of_expr e in
    clierr sr ("Type expression expected, got " ^ string_of_expr e)
