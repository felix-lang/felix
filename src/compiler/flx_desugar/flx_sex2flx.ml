
open Flx_ast
open Sex_types
open Flx_typing2
open List

exception Not_type
exception Not_kind

let prefix p s =
  let n = String.length p in
  n <= String.length s && p = String.sub s 0 n

exception Sex2FlxTypeError of string * sexp_t

let err x s =
  print_string ("[sex2flx] ERROR in " ^ s ^ " in " ^ Sex_print.string_of_sex x);
  raise (Sex2FlxTypeError (s,x))

let serr sr x s =
  print_endline ("[sex2flx] ERROR in " ^ s ^ " in " ^ Sex_print.string_of_sex x);
  print_endline ("Location: " ^ Flx_srcref.long_string_of_src sr);
  raise (Sex2FlxTypeError (s,x))


let qne ex s e' =
  let e = ex e' in
  match qualified_name_of_expr e with
  | Some x -> x
  | None -> err e' (s ^ " -- qualified name required")

let opt s (f:sexp_t->'a) x : 'a option = match x with
  | Id "none" -> None
  | Lst [Id "some"; e] -> Some (f e)
  | x -> err x (s^" option")

let lst s (f:sexp_t->'a) x : 'a list = match x with
  | Lst ls -> map f ls
  | x -> err x (s^ " list")

let rec xsr x : Flx_srcref.t =
  let ii i = int_of_string i in
  match x with
  | Lst [Str fn; Int fl; Int fc; Int ll; Int lc] ->
      Flx_srcref.make (fn,ii fl,ii fc,ii ll,ii lc)
  | x -> err x "Invalid source reference"

and kind_of_sex sr x : kindcode_t =
  let ki k = kind_of_sex sr k in
  match x with
  | Lst [Id "ast_name"; sr; Str "LINEAR"; Lst []] ->  KND_linear
  | Lst [Id "ast_name"; sr; Str "TYPE"; Lst []] ->  KND_type
  | Lst [Id "ast_name"; sr; Str "UNITSUM"; Lst []] ->  KND_unitsum
  | Lst [Id "ast_name"; sr; Str "BOOL"; Lst []] ->  KND_bool
  | Lst [Id "ast_name"; sr; Str "NAT"; Lst []] ->  KND_nat
  | Lst [Id "ast_name"; sr; Str "COMPACTLINEAR"; Lst []] ->  KND_compactlinear
  | Lst [Id "ast_name"; sr; Str "GENERIC"; Lst []] ->  KND_generic

  | Lst [Id "typ_arrow"; Lst[dom; cod]] -> KND_function (ki dom, ki cod)
  | Lst [Id "typ_tuple"; sr; Lst ts] -> KND_tuple (List.map ki ts)
  | _ ->
    print_endline ("Unexpected kind term"); 
    print_endline ( Sex_print.string_of_sex x );
    raise Not_kind


(*
  | KND_typeset of string (* excl mark *)
  | KND_tpattern of typecode_t
  | KND_special of string
*)


and type_of_sex sr w : typecode_t = xtype_t sr w

and xtype_t sr x : typecode_t = 
(*
print_endline ("sex2flx:type] " ^ Sex_print.string_of_sex x);
*)
  let ti t = xtype_t sr t in
  let ex e = xexpr_t sr e in
  match x with
 | Lst [Id "ast_literal";  sr; Str "int"; Str internal_value; Str c_value ] ->
   let v = 
     begin 
       try int_of_string internal_value 
       with _ -> 
         err x "Translating integer literal to type"
     end
   in
   `TYP_unitsum v

  | Lst [Id "ast_name"; sr; Str "LABEL"; Lst []] -> `TYP_label
  | Lst [Id "ast_name"; sr; Str "TRUE"; Lst []] -> `TYP_bool true
  | Lst [Id "ast_name"; sr; Str "FALSE"; Lst []] -> `TYP_bool false

  | Id "typ_none" -> 
    `TYP_none

  | Lst [Id "ast_name"; sr; id; Lst ts] -> 
    let ts = map ti ts in
    let id = xid id in
    `TYP_name (xsr sr, id, ts)

  | Lst [Id "ast_lookup";  Lst [e; Str s; Lst ts]] -> 
   (* this is a hack should just process qualified names .. *)
    `TYP_lookup (sr,(ex e, s,map ti ts))
  | Lst [Id "typ_typeset"; sr; Lst ts] ->
    `TYP_typeset (List.map ti ts)

  | Lst [Id "typ_as"; sr; Lst[t;id]] -> 
    let sr = xsr sr in
    `TYP_as (ti t, xid id)

  | Lst [Id "typ_apply";  sr; Lst [e1; e2]] -> `TYP_apply(ti e1, ti e2)
  | Lst [Id "typ_type_tuple";  sr; Lst es] -> `TYP_type_tuple (map ti es)
  | Lst [Id "typ_typeop"; sr; Str name; ty; kd] ->
    `TYP_typeop  (xsr sr, name, ti ty, kind_of_sex (xsr sr) kd)

 | Lst [Id "typ_implies"; sr; t1; t2] -> `TYP_typeop (xsr sr, "_staticbool_implies", `TYP_type_tuple [ti t1; ti t2], KND_bool)
 | Lst [Id "typ_and"; sr; t1; t2] -> 
   (* print_endline ("sex2flx makes staticbool_and from typ_and"); *)
   `TYP_typeop (xsr sr, "_staticbool_and", `TYP_type_tuple [ti t1; ti t2], KND_bool)

 | Lst [Id "typ_or"; sr; t1; t2] -> `TYP_typeop (xsr sr, "_staticbool_or", `TYP_type_tuple [ti t1; ti t2], KND_bool)
 | Lst [Id "typ_not"; sr; t] -> `TYP_typeop (xsr sr, "_staticbool_not", ti t, KND_bool)
 | Lst [Id "typ_true"; sr; t] -> `TYP_typeop (xsr sr, "_staticbool_true", `TYP_type_tuple [], KND_bool)
 | Lst [Id "typ_false"; sr; t] -> `TYP_typeop (xsr sr, "_staticbool_false", `TYP_type_tuple [], KND_bool)

 (* special constant folding here just to clean up common cases *)
 | Lst [Id "typ_andchain"; Lst es] -> 
  (* print_endline ("sex2flx makes staticbool_and from typ_andchain"); *)
  let tis = map ti es in
  (* print_endline ("  ** AND Components = "  ^ Flx_util.catmap "," Flx_print.string_of_typecode tis); *)
  begin try
    let tis = 
       filter (fun x -> 
         match x with 
         | `TYP_bool true -> false 
         | `TYP_bool false -> raise Not_found 
         | _ -> true
       ) 
       tis
    in 
    match tis with
    | [] -> `TYP_bool true
    | [x] -> x
    | tis -> `TYP_typeop (sr,"_staticbool_and", `TYP_type_tuple tis, KND_bool)
  with Not_found -> `TYP_bool false
  end

 | Lst [Id "ast_record_type"; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, ti e
     | x -> err x "Error in AST_record_type"
     )
     rs
   in `TYP_record (rs)

 | Lst [Id "ast_polyrecord_type"; Lst rs; Lst [Str  s; e]] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, ti e
     | x -> err x "Error in AST_polyrecord_type"
     )
     rs
   in `TYP_polyrecord (rs, s, ti e)

 | Lst [Id "ast_variant_type"; Lst rs] ->
   let rs =
     map (function
     | Lst [Id "ctor"; Str s; e] -> `Ctor (s, ti e)
     | Lst [Id "base"; e] -> `Base (ti e)
     | x -> err x "Error in AST_variant_type"
     )
     rs
   in `TYP_variant (rs)


 | Lst [Id "typ_isin"; Lst [a; b]] -> `TYP_isin (ti a, ti b)

 | Lst [Id "typ_patvar";  sr; Str s] -> `TYP_patvar (xsr sr, s)
 | Lst [Id "typ_patany"; sr] -> `TYP_patany (xsr sr)
 | Lst [Id "ast_void"; sr] -> `TYP_void (xsr sr)
 | Lst [Id "typ_ellipsis"; sr] -> `TYP_ellipsis
 | Lst [Id "typ_arrow";  Lst [e1; e2]] -> `TYP_function (ti e1, ti e2)
 | Lst [Id "typ_effector";  Lst [e1; e2; e3]] -> `TYP_effector (ti e1, ti e2, ti e3)
 | Lst [Id "typ_lineararrow";  Lst [e1; e2]] -> `TYP_linearfunction (ti e1, ti e2)
 | Lst [Id "typ_lineareffector";  Lst [e1; e2; e3]] -> `TYP_lineareffector (ti e1, ti e2, ti e3)
 | Lst [Id "typ_longarrow";  Lst [e1; e2]] -> `TYP_cfunction (ti e1, ti e2)
 | Lst [Id "typ_uniq"; sr; e] -> `TYP_uniq (ti e)

 (* pointers *)
 | Lst [Id "typ_ref"; sr; e] -> `TYP_pointer (ti e)
 | Lst [Id "typ_rref"; sr; e] -> `TYP_rref (ti e)
 | Lst [Id "typ_wref"; sr; e] -> `TYP_wref (ti e)
 | Lst [Id "typ_pclt"; sr; d; c] -> `TYP_pclt (ti d, ti c)
 | Lst [Id "typ_rpclt"; sr; d; c] -> `TYP_rpclt (ti d, ti c)
 | Lst [Id "typ_wpclt"; sr; d; c] -> `TYP_wpclt (ti d, ti c)

 | Lst [Id "typ_dual"; sr; e] -> `TYP_dual (ti e)

 | Lst [Id "typ_array"; t1; t2] -> `TYP_array (ti t1, ti t2)
 | Lst [Id "typ_compactarray"; t1; t2] -> `TYP_compactarray (ti t1, ti t2)
 | Lst [Id "typ_tuple"; sr; Lst es] -> `TYP_tuple (map ti es)
 | Lst [Id "typ_compacttuple"; sr; Lst es] -> `TYP_compacttuple (map ti es)
 | Lst [Id "typ_sum";  sr; Lst es] -> `TYP_sum (map ti es)
 | Lst [Id "typ_compactsum";  sr; Lst es] -> `TYP_compactsum (map ti es)
 | Lst [Id "typ_rptsum"; sr; d; c] -> `TYP_rptsum (ti d, ti c)
 | Lst [Id "typ_compactrptsum"; sr; d; c] -> `TYP_compactrptsum (ti d, ti c)
 | Lst [Id "typ_typeof";  sr; e] -> 
   `TYP_typeof (ex e)

 | Lst [Id "typ_tuple_cons"; sr; t1; t2] -> `TYP_tuple_cons (xsr sr, ti t1, ti t2)
 | Lst [Id "typ_tuple_snoc"; sr; t1; t2] -> `TYP_tuple_snoc (xsr sr, ti t1, ti t2)

 | Lst [Id "ast_type_match";  sr; Lst [t; Lst ts]] ->
   let ts =
     map (fun x -> 
       match x with
       | Lst [t1; t2] -> 
         let t1 = ti t1 in
         let t2 = ti t2 in
         t1, t2
       | x -> 
         err x "ast_typematch typerrror"
     )
     ts
   in 
   `TYP_type_match (ti t, ts)
   (* in `EXPR_type_match (xsr sr,(ti t, ts)) *)

 | Lst [Id "ast_subtype_match";  sr; Lst [t; Lst ts]] ->
   let ts =
     map (function
       | Lst [t1; t2] -> ti t1, ti t2
       | x -> err x "ast_subtypematch typerrror"
     )
     ts
   in `TYP_subtype_match (ti t, ts)
   (* in `EXPR_subtype_match (xsr sr,(ti t, ts)) *)

  | Lst [Id "typ_type_extension"; sr; Lst bases; extension] ->
    let bases = List.map ti bases in
    `TYP_type_extension (xsr sr, bases, ti extension)

  | Lst[Id "typ_typesetunion"; sr; t1; t2] -> `TYP_setunion [ti t1; ti t2]
  | Lst[Id "typ_typesetintersection"; sr; t1; t2] -> `TYP_setintersection [ti t1; ti t2]

  | Lst [Id "typ_typefun"; sr; argss; ret; body] -> let sr = xsr sr in 
    let fixarg  arg = match arg with
    | Lst [id; t] -> xid id, kind_of_sex sr t
    | x -> err x "mktypefun:unpack args1"
    in
    let fixargs args = match args with
    | Lst args -> map fixarg args
    | x -> err x "mktypefun:unpack args2"
    in
    let argss = match argss with
    | Lst args -> map fixargs args
    | x -> err x "mktypefun:unpack args3"
    in
    Flx_typing.mktypelambda sr argss (kind_of_sex sr ret) (xtype_t sr body)

  | Lst ls ->  
    print_endline ("flx_sex2flx]: Type expected: Unexpected sexp list: not type term"); 
    print_endline ("elt=" ^ Flx_util.catmap "\nelt= " Sex_print.string_of_sex ls);
    raise Not_type

  | _ -> 
    print_endline ("[flx_sex2flx]: Type expected: Unexpected sexp, not type term:"); 
    print_endline ("ex= " ^ Sex_print.string_of_sex x );
    raise Not_type



and xid x = match x with
  | Str id | Id id -> Flx_id.of_string id
  | x -> err x "identifier"

and xexpr_t sr x =
(*
print_endline ("sex2flx:expr] " ^ Sex_print.string_of_sex x);
*)
  let xc sr x = xcode_spec_t sr x in
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let ii i = int_of_string i in
  let xq m qn = qne ex m qn in
  let xp x = xpattern_t x in
  let xps pdef x =  xparams_t pdef sr x in
  let xvs x = xvs_list_t sr x in
  let xs x = xstatement_t sr x in
  let xsts x =  lst "statement" xs x in
  match x with
  | Str s ->
      print_endline ("Deprecated Scheme string '" ^ s ^ "' as Felix string");
      `EXPR_literal (sr, {Flx_literal.felix_type="string"; internal_value=s; c_value=Flx_string.c_quote_of_string s})

  | Lst [] -> `EXPR_tuple (sr,[])
  | Lst [x] -> ex x

  | Lst [Id "ast_here"; sr] -> 
    let sr = xsr sr in
    let dmod = `EXPR_name (sr,"Debug",[]) in
    let ctor = `EXPR_lookup (sr,(dmod,"_ctor_flx_location_t",[])) in
    let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
    let mkint i = 
      `EXPR_literal (sr,{Flx_literal.felix_type="int"; internal_value=string_of_int i; c_value=string_of_int i}) 
    in
    let mkcharp s = 
      `EXPR_literal (sr,{Flx_literal.felix_type="cstring"; internal_value="c\""^s^"\""; c_value="\""^s^"\""}) 
    in
    let arg = `EXPR_tuple (sr, [mkcharp f;mkint sl;mkint sc;mkint el;mkint ec]) in
    `EXPR_apply (sr,(ctor, arg))

  (* type slice *)
  | Lst [Id "ast_type_slice"; sr; t] -> `EXPR_name (xsr sr,"Slice_all", [ti t])

  (* this term comes from the hard coded parser! *)
  | Lst [Id "ast_vsprintf";  sr; Str s] -> `EXPR_vsprintf (xsr sr, s)
  | Lst [Id "ast_interpolate";  sr; Str s] -> `EXPR_interpolate (xsr sr, s)
  | Lst [Id "ast_noexpand"; sr; e] -> `EXPR_noexpand (xsr sr,ex e)
  | Lst [Id "ast_name"; sr; id; Lst ts] -> 
(*
print_endline ("Processing ast_name "^xid id^" in xexpr");
*)
    let ts = map ti ts in
    let id = xid id in
    `EXPR_name (xsr sr, id, ts)

  | Lst [Id "ast_case_tag";  sr; Int i] -> `EXPR_case_tag (xsr sr,ii i)
  | Lst [Id "ast_ainj"; sr; ix; tp] -> 
    `EXPR_ainj (xsr sr, ex ix, ti tp) 

  | Lst [Id "ast_false"; sr] -> `EXPR_typed_case (xsr sr,0,`TYP_unitsum 2)
  | Lst [Id "ast_true"; sr ] -> `EXPR_typed_case (xsr sr,1,`TYP_unitsum 2)
  | Lst [Id "ast_unitsum_literal";  sr; Int i; t] -> `EXPR_typed_case (xsr sr,ii i,ti t)
  
  | Lst [Id "ast_projection";  sr; Int i; t] -> `EXPR_projection (xsr sr,ii i,ti t)
  | Lst [Id "ast_identity_function"; sr; t] -> `EXPR_identity_function (xsr sr, ti t)
  | Lst [Id "ast_array_projection";  sr; index; t] -> `EXPR_array_projection (xsr sr,xexpr_t (xsr sr) index,ti t)
  | Lst [Id "ast_lookup";  Lst [e; Str s; Lst ts]] -> `EXPR_lookup (sr,(ex e, s,map ti ts))

  | Lst [Id "ast_apply";  sr; Lst [e1; e2]] -> 
    let sr = xsr sr in
    let e1 = xexpr_t sr e1 in
    let e2 = xexpr_t sr e2 in
    begin match e1 with
    | `EXPR_name (_,"!",_) -> print_endline ("Woops got ! in expression application"); assert false
    | _ -> ()
    end;
    `EXPR_apply(sr,(e1,e2))

 
  | Lst [Id "ast_tuple";  sr; Lst es] -> `EXPR_tuple (xsr sr,map (xexpr_t (xsr sr)) es)
  | Lst [Id "ast_compacttuple";  sr; Lst es] -> `EXPR_compacttuple (xsr sr,map (xexpr_t (xsr sr)) es)


  | Lst [Id "ast_tuple_cons";  sr; eh; et] -> `EXPR_tuple_cons (xsr sr, xexpr_t (xsr sr) eh, xexpr_t (xsr sr) et)
  | Lst [Id "ast_tuple_snoc";  sr; eh; et] -> `EXPR_tuple_snoc (xsr sr, xexpr_t (xsr sr) eh, xexpr_t (xsr sr) et)
  | Lst [Id "ast_record";  sr; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, xexpr_t (xsr sr) e
     | x -> err x "Error in AST_record"
     )
     rs
   in `EXPR_record (xsr sr,rs)

  | Lst [Id "ast_polyrecord";  sr; Lst rs; e] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, xexpr_t (xsr sr) e
     | x -> err x "Error in AST_polyrecord"
     )
     rs
   in `EXPR_polyrecord (xsr sr,rs, ex e)

  | Lst [Id "ast_replace_fields";  sr; e; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, xexpr_t (xsr sr) e
     | x -> err x "Error in AST_replace_fields"
     )
     rs
   in `EXPR_replace_fields (xsr sr, ex e, rs)

  | Lst [Id "ast_remove_fields";  sr; e; Lst ss] ->
   let ss =
     map (function
     | Str s -> s
     | x -> err x "Error in AST_remove_fields"
     )
     ss
   in `EXPR_remove_fields (xsr sr,ex e, ss)

 | Lst [Id "ast_getall_field"; sr; e; Str s] ->
   `EXPR_getall_field (xsr sr, ex e, s)

 | Lst [Id "ast_variant";  Lst [Str s; e]] -> `EXPR_variant (sr,(s, ex e))

 | Lst [Id "ast_arrayof";  sr; Lst es] -> `EXPR_arrayof (xsr sr, map ex es)
 | Lst [Id "ast_coercion";  sr; Lst [e; t]] ->  `EXPR_coercion (xsr sr,(ex e, ti t))

 | Lst [Id "ast_suffix";  Lst [qn;t]] -> `EXPR_suffix (sr,(xq "ast_suffix" qn,ti t))

 | Lst [Id "ast_patvar";  sr; Str s] -> `EXPR_patvar (xsr sr, s)

 | Lst [Id "ast_patany"; sr] -> `EXPR_patany (xsr sr)


 | Lst [Id "ast_intersect"; Lst es] -> `EXPR_intersect (sr, map ex es)
 | Lst [Id "ast_isin"; Lst [a; b]] -> `EXPR_isin (sr, (ex a, ex b))
 | Lst [Id "ast_not"; sr; e] -> `EXPR_not (xsr sr, ex e)
(*
 | Lst [Id "ast_arrow";  Lst [e1; e2]] -> `EXPR_arrow (sr,(ex e1, ex e2))
*)
(*
 | Lst [Id "ast_effector";  Lst [e1; e2; e3]] -> `EXPR_effector (sr,(ex e1, ex e2, ex e3))
*)
(*
 | Lst [Id "ast_longarrow";  Lst [e1; e2]] -> `EXPR_longarrow (sr,(ex e1, ex e2))
*)

 | Lst [Id "ast_superscript";  Lst [e1; e2]] -> `EXPR_superscript (sr,(ex e1, ex e2))
 | Lst [Id "ast_literal";  sr; Str felix_type; Str internal_value; Str c_value ] ->
   `EXPR_literal (xsr sr, 
     {
       Flx_literal.felix_type=felix_type; 
       internal_value=internal_value; 
       c_value=c_value
     } 
   )

 | Lst [Id "ast_deref"; sr; e] -> 
   `EXPR_deref (xsr sr,ex e)
 | Lst [Id "ast_ref"; sr; e] -> `EXPR_ref (xsr sr,ex e)

 | Lst [Id "ast_uniq"; sr; e] -> `EXPR_uniq (xsr sr, ex e)
 | Lst [Id "ast_rref"; sr; e] -> `EXPR_rref(xsr sr, ex e)
 | Lst [Id "ast_wref"; sr; e] -> `EXPR_wref(xsr sr, ex e)

 | Lst [Id "ast_label_ref"; sr; id] -> `EXPR_label (xsr sr,xid id)
 | Lst [Id "ast_new"; sr; e] -> `EXPR_new (xsr sr,ex e)
 | Lst [Id "ast_likely"; sr; e] -> `EXPR_likely (xsr sr,ex e)
 | Lst [Id "ast_unlikely"; sr; e] -> `EXPR_unlikely (xsr sr,ex e)
 | Lst [Id "ast_callback";  sr; qn] -> `EXPR_callback (xsr sr,xq "ast_callback" qn)
 | Lst [Id "ast_lambda";  sr; Lst [vs; Lst pss; t; sts]] -> 
   let rt = ti t in
   let pdef = (* match rt with `TYP_void _ -> `PVar | _ -> *) `PVal in 
   `EXPR_lambda  (xsr sr, (`GeneratedInlineFunction,xvs vs, map (xps pdef) pss, rt, xsts sts))

 | Lst [Id "ast_linearlambda";  sr; Lst [vs; Lst pss; t; sts]] -> 
   let rt = ti t in
   let pdef = (* match rt with `TYP_void _ -> `PVar | _ -> *) `PVal in 
   `EXPR_lambda  (xsr sr, (`LinearFunction,xvs vs, map (xps pdef) pss, rt, xsts sts))

 | Lst [Id "ast_object";  sr; Lst [vs; Lst pss; t; sts]] -> 

(*
print_endline ("ast_object, type = " ^ Sex_print.string_of_sex t);
*)
   let rt = ti t in
   let pdef = (* match rt with `TYP_void _ -> `PVar | _ -> *) `PVal in 
   `EXPR_lambda (xsr sr, (`Object,xvs vs, map (xps pdef) pss, rt, xsts sts))

 | Lst [Id "ast_generator";  sr; Lst [vs; Lst pss; t; sts]] -> 
   let rt = ti t in
   let pdef = `PVar in 
   `EXPR_lambda (xsr sr, (`Generator,xvs vs, map (xps pdef) pss, rt, xsts sts))

 (* can't occur in user code
 | Lst [Id "ast_match_ctor";  Lst [qn; e]] -> `EXPR_match_ctor(sr,(xq "ast_match_ctor" qn,ex e))
 | Lst [Id "ast_match_case";  Lst [Int i; e]]-> `EXPR_match_case (sr,(ii i, ex e))
 | Lst [Id "ast_ctor_arg";  Lst [qn; e]] -> `EXPR_ctor_arg (sr,(xq "ast_ctor_arg" qn, ex e))
 | Lst [Id "ast_case_arg"; Lst [Int i; e]] -> `EXPR_case_arg (sr,(ii i, ex e))
 *)
 | Lst [Id "ast_case_index";  sr; e] -> `EXPR_case_index (xsr sr, ex e)
 | Lst [Id "ast_rptsum_arg";  sr; e] -> `EXPR_rptsum_arg (xsr sr, ex e)
 | Lst [Id "ast_letin";  sr; Lst [p; e1; e2]] -> `EXPR_letin (xsr sr,(xp p, ex e1, ex e2))

(*
 | Lst [Id "ast_get_n";  sr;  Lst [Int i; e]] -> `EXPR_get_n(xsr sr,(ii i, ex e))
*)
 (* extractor for record components, can't occur in user code
 | Lst [Id "ast_get_named_variable";  Lst [Str s;e]]-> `EXPR_get_named_variable (sr, (s, ex e))
 *)
 | Lst [Id "ast_as";  sr; Lst [e; Str s]] -> `EXPR_as (xsr sr,(ex e, s))
 | Lst [Id "ast_as_var";  sr; Lst [e; Str s]] -> `EXPR_as_var (xsr sr,(ex e, s))
 | Lst [Id "ast_match";  sr; Lst [e; Lst pes]]->
   let pes = map (function
     | Lst [p;e] -> xp p, ex e
     | x -> err x "ast_match syntax"
     )
     pes
   in
   `EXPR_match (xsr sr, (ex e,pes))

 | Lst [Id "ast_cond"; sr;  Lst [e1;e2;e3]] -> `EXPR_cond (xsr sr,(ex e1, ex e2, ex e3))
 | Lst [Id "ast_expr"; sr; s; t; e] -> `EXPR_expr (xsr sr, xc (xsr sr) s, ti t, ex e)

 | Lst [Id "ast_typecase_match";  sr; Lst [t; Lst ts]] ->
   let ts =
     map (function
       | Lst [t1; e2] -> ti t1, ex e2
       | x -> err x "ast_typecase_match typerrror"
     )
     ts
   in `EXPR_typecase_match (xsr sr,(ti t, ts))


  | Lst [Id "ast_extension"; sr; Lst bases; extension] ->
    let bases = List.map ex bases in
(*
print_endline ("EXPR: Extension of bases = " ^ Flx_util.catmap "," Flx_print.string_of_expr bases);
*)
    `EXPR_extension (xsr sr, bases, ex extension)


  | Lst ls -> 
    print_endline ("[sex2flx] Unexpected list! processing expression"); 
    print_endline ("(" ^ Flx_util.catmap "\n" Sex_print.string_of_sex ls ^ ")");
    assert false; 

  | Id id ->
      (*
      print_endline ("Unexpected ID=" ^ Flx_id.to_string id);
      *)
      `EXPR_name (sr, Flx_id.of_string id, [])
  | Int i ->
    `EXPR_literal (sr, {Flx_literal.felix_type="int"; internal_value=i; c_value=i})

  | x ->
    err x "expression"

and xlit x =
  match x with
  | Lst [Id "ast_literal";  sr; Str felix_type; Str internal_value; Str c_value ] ->
     {
       Flx_literal.felix_type=felix_type; 
       internal_value=internal_value; 
       c_value=c_value
     } 
  | x -> err x "literal"


and xpattern_t x =
  let xp x = xpattern_t x in
  let ti sr x = type_of_sex sr x in
  let xq sr m qn = qne (xexpr_t (xsr sr)) m qn in
  match x with
  | Lst [Id "pat_expr"; sr; e] -> PAT_expr (xsr sr, xexpr_t (xsr sr) e)
  | Lst [Id "pat_none"; sr] -> PAT_none (xsr sr)

  (* constants *)
  | Lst [Id "pat_literal"; sr; lit] -> 
    PAT_literal (xsr sr, xlit lit) 

  | Lst [Id "pat_range"; sr; lit1; lit2] -> 
    PAT_range (xsr sr, xlit lit1, xlit lit2) 

  (* other *)
  | Lst [Id "pat_coercion"; sr; p; t] ->
      PAT_coercion (xsr sr, xp p, ti (xsr sr) t)

  (* match all the cases of a subset of polymorphic variant cases *)
  | Lst[Id "pat_subtype"; sr; typ; id]  ->
    PAT_subtype (xsr sr, ti (xsr sr) typ, xid id)


  | Lst [Id "pat_name"; sr; id] -> PAT_name (xsr sr, xid id)
  | Lst [Id "pat_tuple"; sr; Lst ps] -> PAT_tuple (xsr sr, map xp ps)
  | Lst [Id "pat_tuple_cons"; sr; a; b] -> PAT_tuple_cons (xsr sr, xp a, xp b)
  | Lst [Id "pat_tuple_snoc"; sr; a; b] -> PAT_tuple_snoc (xsr sr, xp a, xp b)

  | Lst [Id "pat_any"; sr] -> PAT_any (xsr sr)
  | Lst [Id "pat_setform_any"; sr] -> PAT_setform_any (xsr sr)

  | Lst [Id "pat_const_ctor"; sr; qn] ->
      PAT_const_ctor (xsr sr, xq sr "pat_const_ctor" qn)

  | Lst [Id "pat_nonconst_ctor"; sr; qn; p] ->
      PAT_nonconst_ctor (xsr sr, xq sr "pat_nonconst_ctor" qn, xp p)

  | Lst [Id "pat_ho_ctor"; sr; qn; Lst es; p] ->
      let es = map (xexpr_t (xsr sr)) es in
      PAT_ho_ctor (xsr sr, xq sr "pat_ho_ctor" qn, es, xp p)

  | Lst [Id "pat_const_variant"; sr; Str s] ->
      PAT_const_variant (xsr sr, s)
  | Lst [Id "pat_nonconst_variant"; sr; Str s; p] ->
      PAT_nonconst_variant (xsr sr, s, xp p)

  | Lst [Id "pat_as"; sr; p; id] -> PAT_as (xsr sr, xp p, xid id)
  | Lst [Id "pat_when"; sr; p; e] -> PAT_when (xsr sr, xp p, xexpr_t (xsr sr) e)

  | Lst [Id "pat_with"; sr; p; Lst es] -> 
    let sr = xsr sr in
    let es = List.map begin function
      | Lst [id;e] -> xid id,xexpr_t sr e
      | x -> err x "pat_with syntax"
      end es
    in
    let p = PAT_with(sr, xp p, es) in
(*
print_endline ("sex2flx: pat_with=" ^ Flx_print.string_of_pattern p);
print_endline (String.concat ", " (List.map (fun (s,e) -> s ^ "=" ^ Flx_print.string_of_expr e) es));
*)
    p

  | Lst [Id "pat_record"; sr; Lst ips] ->
      let ips =
        List.map begin function
          | Lst [id; p] -> xid id, xp p
          | x -> err x "pat_record syntax"
        end ips
      in
      PAT_record (xsr sr, ips)

  | Lst [Id "pat_polyrecord"; sr; Lst ips; r] ->
      let ips =
        List.map begin function
          | Lst [id; p] -> xid id, xp p
          | x -> err x "pat_record syntax"
        end ips
      in
      PAT_polyrecord (xsr sr, ips, xid r)

  | Lst [Id "pat_alt"; sr; Lst ps] ->
      PAT_alt (xsr sr, map xp ps) 

  | x ->
    err x "pattern"

(*
and xcharset_t sr x =
  let cs x = xcharset_t sr x in
  match x with
  | Lst [Id "charset_of_string"; Str s] -> Flx_charset.charset_of_string s
  | Lst [Id "charset_of_int_range"; Int i; Int j] ->
    Flx_charset.charset_of_int_range (int_of_string i) (int_of_string j)

  | Lst [Id "charset_of_range"; Str i; Str j] ->
    Flx_charset.charset_of_range i j

  | Lst [Id "charset_union"; x; y] ->
    Flx_charset.charset_union (cs x) (cs y)

  | Lst [Id "charset_inv"; x] ->
    Flx_charset.charset_inv (cs x)

  | x ->
    err x "charset"
*)
and xraw_typeclass_insts_t sr x =
  let ex x = xexpr_t sr x in
  let xq m qn = qne ex m qn in
  match x with
  | Lst tcs -> map (xq "raw_typeclass_insts_t") tcs
  | x -> err x "raw_typeclass_insts_t"

and xvs_aux_t sr x : vs_aux_t =
(*
print_endline ("Translating auxilliary vs data" ^ Sex_print.string_of_sex x);
*)
  let ex x = xexpr_t sr x in
  let ti x = xtype_t sr x in
  let xrtc x = xraw_typeclass_insts_t sr x in
  match x with
  | Lst [ct; tcr] -> 
(*
    print_endline ("Raw type constraint = " ^ Sex_print.string_of_sex ct);
    let xx = try ti ct with _ -> print_endline "ERROR translating type constraint"; assert false in
    print_endline ("Translated type constraint = " ^ Flx_print.string_of_typecode (ti ct));
    begin match ti ct with
    | `TYP_tuple [] -> print_endline "BUGGED constraint, got unit!"; assert false
    | _ -> ()
    end;
*)
   { raw_type_constraint=ti ct; raw_typeclass_reqs=xrtc tcr }
  | x -> err x "xvs_aux_t"

and xsimple_parameter_list sr x : simple_parameter_t list =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  match x with
  | Lst its -> map (function
    | Lst [id; t] -> xid id, ti t
    | x -> err x "xsimple_parameter_list"
    ) its
  | x -> err x "xsimple_parameter_list"


and xplain_vs_list_t sr x : plain_vs_list_t =
  let ex x = xexpr_t sr x in
  let ki x = kind_of_sex sr x in
  match x with
  | Lst its -> map (function
    | Lst [id; t] -> xid id, ki t
    | x -> err x "xplain_vs_list"
    ) its
  | x -> err x "xplain_vs_list"

and xvs_list_t sr x : vs_list_t =
(*
print_endline ("Translating vs list " ^ Sex_print.string_of_sex x);
*)
  let xpvs x = xplain_vs_list_t sr x in
  let xaux x = xvs_aux_t sr x in
  match x with
  | Lst [pvs; aux] -> xpvs pvs, xaux aux
  | x -> err x "xvs_list_t"

and xaxiom_method_t sr x : axiom_method_t =
  let ex x = xexpr_t sr x in
  match x with
  | Lst [Id "Predicate"; e] -> Predicate (ex e)
  | Lst [Id "Equation"; e1; e2] -> Equation (ex e1, ex e2)
  | x -> err x "axiom_method_t"

and xparam_kind_t def_val sr x : param_kind_t =
  match x with
  | Id "PVal" -> `PVal
  | Id "PVar" -> `PVar
  | Id "POnce" -> `POnce
  | Id "PDef" -> def_val
  | x -> err x "param_kind_t"

and xparameter_t sr def_val x : parameter_t =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let xpk x = xparam_kind_t def_val sr x in
  match x with
  | Lst [sr; pk; id; t; e] -> 
    let sr = xsr sr in
    sr, xpk pk, xid id, ti t, opt "dflt_arg" ex e
  | x -> serr sr x "parameter_t"


(* Temporarily, the parser isn't being upgraded to handle nested paramspecs,
   first get the unbound term handling to work!
*)
and old_xparamspec_t sr def_val ps : paramspec_t = 
  let xpa x = xparameter_t sr def_val x in
  match ps with
  | [p] -> Satom  (xpa p)
  | _ -> Slist (map (fun p -> Satom (xpa p)) ps)

and xparamspec_t sr def_val ps : paramspec_t = 
  let xps x = xparamspec_t sr def_val x in
  let xpa x = xparameter_t sr def_val x in
  let base = match ps with
    | Lst [Id "Satom"; p] -> Satom  (xpa p)
    | Lst [Id "Slist"; Lst ps]  -> Slist (map xps ps)

    (* anachronism in parser *)
    | Lst [p] -> 
      print_endline ("Parser: warning: sole parameter should have 'Satom'");
      print_endline ("Location: " ^ Flx_srcref.long_string_of_src sr);
      Satom (xpa p) (* FIXME: Hack for old parser, typically single parameter *)

    (* anachronism in parser *)
    | Lst [] -> 
      print_endline ("Parser: warning: unit parameter should have 'Slist'");
      print_endline ("Location: " ^ Flx_srcref.long_string_of_src sr);
      Slist []       (* FIXME: Hack for old parser, typically default empty parameter ((),none) *)
    | x -> serr sr x "xparamspec_t"
  in
  (* Replace Slist of one entry with that entry *)
  let rec aux x = match x with
  | Slist [x] -> aux x
  | x -> x
  in aux base

(* Temporarily, the parser isn't being upgraded to handle nested paramspecs,
   first get the unbound term handling to work!
*)
and old_xparams_t def_val sr x : params_t =
  let ex x = xexpr_t sr x in
  match x with
  | Lst [Lst ps; eo] -> old_xparamspec_t sr def_val ps, opt "params" ex eo
  | x -> err x "params_t"

and xparams_t def_val sr x : params_t =
  let ex x = xexpr_t sr x in
  match x with
  | Lst [ps; eo] -> xparamspec_t sr def_val ps, opt "params" ex eo
  | x -> serr sr x "xparams_t"


and xret_t sr x : typecode_t * expr_t option =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  match x with
  | Lst [t; e] -> ti t, opt "return" ex e
  | x -> err x "return encoding"

and xfunkind_t sr x : funkind_t =
  match x with
  | Id "Function" -> `Function
  | Id "CFunction" -> `CFunction
  | Id "InlineFunction" -> `InlineFunction
  | Id "NoInlineFunction" -> `NoInlineFunction
  | Id "Virtual" -> `Virtual
  | Id "Ctor" -> `Ctor
  | Id "Generator" -> `Generator
  | Id "GeneratorMethod" -> `GeneratorMethod
  | Id "Method" -> `Method
  | Id "Object" -> `Object
  | x -> err x "funkind_t"

and xadjective_t x : property_t =
  match x with
  | Id "LinearFunction" -> `LinearFunction
  | Id "InlineFunction" -> `Inline
  | Id "NoInlineFunction" -> `NoInline
  | Id "Virtual" -> `Virtual 
  | Id "Impure"  -> `ImPure
  | Id "Pure"  -> `Pure
  | Id "Strict"  -> `Strict
  | Id "NonStrict"  -> `NonStrict
  | Id "Partial"  -> `Partial
  | Id "Method"  -> `Tag "method"
  | Id "Total"  -> `Total
  | Id "Export" -> `Export
  | Id "Subtype" -> `Subtype
  | Lst [Id "NamedExport"; Str name]  -> `NamedExport name
  | x -> err x "xadjective_t"

 
and xcode_spec_t sr x : Flx_code_spec.t =
  let module CS = Flx_code_spec in
  match x with
  | Lst [Id "StrTemplate"; Str s] -> CS.Str_template (s)
  | Lst [Id "Str"; Str s] -> CS.Str (s)
  | Id "Virtual" -> CS.Virtual
  | Id "Identity" -> CS.Identity
  | x -> err x "c_t"

and xlvalue_t sr x : lvalue_t =
  let ex x = xexpr_t sr x in
  let xtlv x = xtlvalue_t sr x in
  match x with
  | Lst [Id "Val"; sr; id] -> `Val (xsr sr, xid id)
  | Lst [Id "Var"; sr; id] -> `Var (xsr sr, xid id)
  | Lst [Id "Name"; sr; id] -> `Name (xsr sr, xid id)
  | Lst [Id "Skip"; sr]  -> `Skip (xsr sr)
  | Lst [Id "List"; tl] -> `List (lst "lvalue_t" xtlv tl)
  | Lst [Id "Expr"; sr; e] -> `Expr (xsr sr,ex e)
  | x -> err x "lvalue_t"

and xtlvalue_t sr x : tlvalue_t =
  let xlv x = xlvalue_t sr x in
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let xot x = opt "typecode" ti x in
  match x with
  | Lst [lv; ot] -> xlv lv, xot ot
  | x -> err x "tlvalue_t"

and xtype_qual_t sr x : type_qual_t =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  match x with
  | Id "Incomplete" -> `Incomplete
  | Id "Uncopyable" -> `Uncopyable
  | Id "Pod" -> `Pod
  | Id "GC_pointer" -> `GC_pointer
  | Lst [Id "Raw_needs_shape"; t] -> `Raw_needs_shape (ti t)
  | x -> err x "typequal_t"

and xrequirement_t sr x : requirement_t =
  let ex x = xexpr_t sr x in
  let xq m qn = qne ex m qn in
  let xct x = xcode_spec_t sr x in
  match x with
  | Lst [Id "Body_req"; ct] -> Body_req (xct ct)
  | Lst [Id "Header_req"; ct] -> Header_req (xct ct)
  | Lst [Id "Named_req"; qn] -> Named_req (xq "Named_req" qn)
  | Lst [Id "Property_req"; Str s] -> Property_req (s)
  | Lst [Id "Package_req"; ct] -> Package_req (xct ct)
  | Lst [Id "Scanner_req"; ct] -> Scanner_req (xct ct)
  | Lst [Id "Finaliser_req"; ct] -> Finaliser_req (xct ct)
  | Lst [Id "Encoder_req"; ct] -> Encoder_req (xct ct)
  | Lst [Id "Decoder_req"; ct] -> Decoder_req (xct ct)
  | Lst [Id "Index_req"; Int indx] -> Index_req (int_of_string indx)
  | Lst [Id "Named_index_req"; Str sindx] -> Named_index_req (sindx)
  | Lst [Id "Subtype_req"] -> Subtype_req
  | x -> err x "requirement_t"

and xraw_req_expr_t sr x : raw_req_expr_t =
  let xr x = xrequirement_t sr x in
  let xrr x = xraw_req_expr_t sr x in
  match x with
  | Lst [Id "rreq_atom"; r] -> RREQ_atom (xr r)
  | Lst [Id "rreq_or"; r1; r2] -> RREQ_or (xrr r1, xrr r2)
  | Lst [Id "rreq_and"; r1; r2] -> RREQ_and (xrr r1, xrr r2)
  | Id "rreq_true"-> RREQ_true
  | Id "rreq_false"-> RREQ_false
  | Lst [] -> RREQ_true
  | x -> err x "raw_req_expr_t"


and xunion_component sr x =
  let xvs x = xvs_list_t sr x in
  let ii i = int_of_string i in
  let xi = function | Int i -> ii i | x -> err x "int" in
  let ti x = type_of_sex sr x in
  match x with
  | Lst [id; io; vs; d; c] -> xid id, opt "union component" xi io, xvs vs, ti d, Some (ti c)
  | Lst [id; io; vs; d] -> xid id, opt "union component" xi io, xvs vs, ti d, None
  | x -> err x "union component"

and xstruct_component sr = function
  | Lst [id; t] -> xid id, type_of_sex sr t
  | x -> err x "struct component"

and xpin_t = function
  | Lst[d;r] -> xid d, xid r
  | x -> err x "pin specification"

and xconnection_t sr = function
  | Lst[Id kind; data] ->
    begin match kind with
    | "connect" ->  
      begin match data with 
      | Lst pins -> Connect (map xpin_t pins)
      | x -> err x "connect specification"
      end
    | "wire" -> 
      begin match data with
      | Lst[e;rd;rp] ->
        Wire  (xexpr_t sr e, (xid rd, xid rp))
      | x -> err x "wire specification"
      end
    | _ -> assert false
    end
  | x -> err x "connection specification"

and xstatement_t sr x : statement_t =
(*
print_endline ("sex2flx:stmt] " ^ Sex_print.string_of_sex x);
*)
  let pdef = `PVal in
  let xspl sr x = xsimple_parameter_list sr x in 
  let xpvs sr x = xplain_vs_list_t sr x in
  let xs sr x = xstatement_t sr x in
  let ex sr x = xexpr_t sr x in
  let xq sr m qn = qne (ex sr) m qn in
  let xvs sr x = xvs_list_t sr x in
  let xam sr x =  xaxiom_method_t sr x in
  let xps pdef sr x =  xparams_t pdef sr x in
  let xret sr x =  xret_t sr x in
  let xsts sr x =  lst "statement" (xs sr) x in
  let xfk sr x = xfunkind_t sr x in
  let ti sr x = type_of_sex sr x in
  let ki sr x = kind_of_sex sr x in
  let ii i = int_of_string i in
  let xi = function | Int i -> ii i | x -> err x "int" in
  let xtlv sr x = xtlvalue_t sr x in
  let xtq sr x = xtype_qual_t sr x in
  let xtqs sr x = lst "typ_equal_t" (xtq sr) x in
  let xc sr x = xcode_spec_t sr x in
  let xrr sr x = xraw_req_expr_t sr x in
  let xucmp sr x = xunion_component sr x in
  let xscmp sr x = xstruct_component sr x in
  let xp x = xpattern_t x in
  let xred sr x = match x with
  | Lst [vs; spl; e1; e2] -> xvs sr vs, xspl sr spl, ex sr e1, ex sr e2 
  | _ -> err x "reduction format" 
  in

  let lnot sr x = `EXPR_not (sr, x) in
  match x with
  | Lst [Id "ast_static_assert"; sr; t] -> 
    let sr = xsr sr in
    let t = xtype_t sr t in
    STMT_static_assert (sr,t)

  | Lst [Id "ast_circuit"; sr; Lst cs] -> let sr = xsr sr in
    let cs = map (xconnection_t sr) cs in
    STMT_circuit (sr,cs)

  | Lst [] -> STMT_nop(Flx_srcref.dummy_sr, "null")

  | Lst [Id "ast_seq"; sr; sts] -> let sr = xsr sr in 
      STMT_seq (sr,xsts sr sts)

  | Lst [Id "ast_include"; sr; Str s] -> let sr = xsr sr in STMT_include (sr, s)
  | Lst [Id "ast_open"; sr; vs; qn] -> let sr = xsr sr in STMT_open (sr, xvs sr vs, xq sr "ast_open" qn)
  | Lst [Id "ast_inject_module"; sr; vs; qn] -> let sr = xsr sr in STMT_inject_module (sr, xvs sr vs, xq sr "ast_inject_module" qn)
  | Lst [Id "ast_use"; sr; id; qn] -> let sr = xsr sr in STMT_use (sr, xid id, xq sr "ast_use" qn)
  | Lst [Id "ast_comment"; sr; Str s] -> let sr = xsr sr in STMT_comment (sr, s)
  | Lst [Id "ast_private"; sr; x] -> let sr = xsr sr in STMT_private (sr, xs sr x)

  | Lst [Id "ast_reduce"; sr; id; Lst reductions]  -> let sr = xsr sr in 
    let stmt = 
      STMT_reduce (
        sr,
        xid id,
        map (xred sr) reductions 
        )
    in
(*
    print_endline ("Flx_sex2flx: Reduction: " ^ Flx_print.string_of_statement 0 stmt); 
*)
    stmt

  | Lst [Id "ast_axiom"; sr; id; vs; ps; axm] -> let sr = xsr sr in 
    STMT_axiom (
      sr,
      xid id,
      xvs sr vs,
      xps `PVal sr ps,
      xam sr axm)

  | Lst [Id "ast_lemma"; sr; id; vs; ps; axm] -> let sr = xsr sr in 
    STMT_lemma(
      sr,
      xid id,
      xvs sr vs,
      xps `PVal sr ps,
      xam sr axm)

  | Lst [Id "ast_curry"; sr; id; vs; Lst pss; ret; fk; Lst props; sts] -> let sr = xsr sr in 
(*
print_endline ("sex2flx: ast_curry (function def) " ^ xid id ^ ", sr=" ^ Flx_srcref.short_string_of_src sr);
*)
    let fret = xret sr ret in
    let rett, _ = fret in 
    let pdef = (*  match rett with `TYP_void _ -> `PVar | _ ->*)  `PVal in
    let sts = xsts sr sts in
    let result = 
    STMT_curry(
      sr,
      xid id,
      xvs sr vs,
      map (xps pdef sr) pss,
      fret,
      Flx_typing.flx_unit,
      xfk sr fk,
      map xadjective_t props,
      sts)
    in
(*
    print_endline "ast_curry done";
*)
    result

  | Lst [Id "ast_curry_effects"; sr; id; vs; Lst pss; ret; effects; fk; Lst props; sts] -> let sr = xsr sr in 
(*
print_endline ("sex2flx: ast_curry_effects (function def) " ^ xid id ^ " sr=" ^ Flx_srcref.short_string_of_src sr);
*)
(*
print_endline ("ast_curry effects " ^ xid id ^ ", effects=" ^ Flx_print.string_of_typecode (ti sr effects));
*)
    let fret = xret sr ret in
    let rett, _ = fret in 
    let pdef = (*  match rett with `TYP_void _ -> `PVar | _ ->*)  `PVal in
    STMT_curry(
      sr,
      xid id,
      xvs sr vs,
      map (xps pdef sr) pss,
      fret,
      ti sr effects,
      xfk sr fk,
      map xadjective_t props,
      xsts sr sts)

  (* promotion from subtype(arg) to supertype(param) <: is backwards from
     order used in unification engine
  *)
  | Lst [Id "ast_macro_val"; sr; ids; v] -> let sr = xsr sr in 
      STMT_macro_val (sr, lst "ast_macro_val" xid ids, ex sr v)

  | Lst [Id "ast_macro_forall"; sr; ids; e; sts] -> let sr = xsr sr in 
      STMT_macro_forall (sr,lst "ast_macro_forall" xid ids, ex sr e, xsts sr sts)

  | Lst [Id "ast_union"; sr; id; vs; ucmp] -> let sr = xsr sr in 
      let ucmp = lst "union component" (xucmp sr) ucmp in
      STMT_union (sr, xid id, xvs sr vs, ucmp)

  | Lst [Id "ast_struct"; sr; id; vs; ucmp] -> let sr = xsr sr in 
      let ucmp = lst "struct component" (xscmp sr) ucmp in
      STMT_struct (sr, xid id, xvs sr vs, ucmp)

  | Lst [Id "ast_cstruct"; sr; id; vs; ucmp; reqs] -> let sr = xsr sr in 
      let ucmp = lst "cstruct component" (xscmp sr) ucmp in
      STMT_cstruct (sr, xid id, xvs sr vs, ucmp, xrr sr reqs)

  | Lst [Id "ast_type_alias"; sr; id; vs; t] -> let sr = xsr sr in 
(*
print_endline ("type alias " ^ xid id ^ " sexpr = " ^ Sex_print.string_of_sex t);
print_endline ("Type alias " ^ xid id ^ " flx   = " ^ Flx_print. string_of_typecode (ti sr t));
*)
      STMT_type_alias (sr, xid id, xvs sr vs, ti sr t)

  | Lst [Id "mktypefun"; sr; id; vs; argss; ret; body] -> let sr = xsr sr in 
    let fixarg  arg = match arg with
    | Lst [id; t] -> xid id, ki sr t
    | x -> err x "mktypefun:unpack args1"
    in
    let fixargs args = match args with
    | Lst args -> map fixarg args
    | x -> err x "mktypefun:unpack args2"
    in
    let argss = match argss with
    | Lst args -> map fixargs args
    | x -> err x "mktypefun:unpack args3"
    in
    Flx_typing.mktypefun (sr) (xid id) (xvs sr vs) argss (ki sr ret) (ti sr body)

  | Lst [Id "ast_inherit"; sr; id; vs; qn] -> let sr = xsr sr in 
      STMT_inherit (sr, xid id, xvs sr vs, xq sr "ast_inherit" qn)

  | Lst [Id "ast_inherit_fun"; sr; id; vs; qn] -> let sr = xsr sr in 
      STMT_inherit_fun (sr, xid id, xvs sr vs, xq sr "ast_inherit_fun" qn)

  | Lst [Id "ast_val_decl"; sr; id; vs; ot; oe] -> let sr = xsr sr in 
      STMT_val_decl (
        sr, 
        xid id, 
        xvs sr vs, 
        opt "val_decl" (ti sr) ot, 
        opt "val_decl" (ex sr) oe)

  | Lst [Id "ast_once_decl"; sr; id; vs; ot; oe] -> let sr = xsr sr in 
      STMT_once_decl (
        sr, 
        xid id, 
        xvs sr vs, 
        opt "once_decl" (ti sr) ot, 
        opt "once_decl" (ex sr) oe)


  | Lst [Id "ast_lazy_decl"; sr; id; vs; ot; oe] -> let sr = xsr sr in 
      STMT_lazy_decl (
        sr,
        xid id,
        xvs sr vs,
        opt "lazy_decl" (ti sr) ot,
        opt "lazy_decl" (ex sr) oe)

  | Lst [Id "ast_var_decl"; sr; id; vs; ot; oe] -> let sr = xsr sr in 
      STMT_var_decl (
        sr,
        xid id,
        xvs sr vs,
        opt "var_decl" (ti sr) ot, 
        opt "var_decl" (ex sr) oe)

  | Lst [Id "ast_ref_decl"; sr; id; vs; ot; oe] -> let sr = xsr sr in 
      STMT_ref_decl (
        sr,
        xid id,
        xvs sr vs,
        opt "ref_decl" (ti sr) ot,
        opt "ref_decl" (ex sr) oe)

  | Lst [Id "ast_untyped_module"; sr; id; vs; sts] -> let sr = xsr sr in 
      STMT_untyped_module (sr, xid id, xvs sr vs, xsts sr sts)

  | Lst [Id "ast_library"; sr; id; sts] -> 
      let sr = xsr sr in 
      STMT_library (sr, xid id, xsts sr sts)
  
  | Lst [Id "ast_typeclass"; sr; id; vs; sts] -> let sr = xsr sr in 
      STMT_typeclass (sr, xid id, xvs sr vs, xsts sr sts)

  | Lst [Id "ast_begin_typeclass"; sr; id; vs] -> let sr = xsr sr in 
      STMT_begin_typeclass (sr, xid id, xvs sr vs)


  | Lst [Id "ast_instance"; sr; vs; qn; sts] -> let sr = xsr sr in 
    (*
    print_endline "Ast instance sts=";
    begin match sts with Lst sts->
    iter
    (fun s -> print_endline ("Stmt=" ^ Sex_print.string_of_sex s))
    sts
    | _ -> err sts "[ast_instance: Bad statement list]"
    end
    ;
    *)
    STMT_instance (sr, xvs sr vs, xq sr "ast_instance" qn, xsts sr sts)

  | Lst [Id "ast_type_error"; sr; stmt] -> let sr = xsr sr in STMT_type_error (sr, xs sr stmt)
  | Lst [Id "ast_type_assert"; sr; stmt] -> let sr = xsr sr in STMT_type_assert(sr, xs sr stmt)

  | Lst [Id "ast_label"; sr; id] -> let sr = xsr sr in STMT_label (sr, xid id)
  | Lst [Id "ast_try"; sr] -> let sr = xsr sr in STMT_try (sr)
  | Lst [Id "ast_endtry"; sr] -> let sr = xsr sr in STMT_endtry (sr)
  | Lst [Id "ast_catch"; sr; Str id; t] -> let sr = xsr sr in STMT_catch (sr, id, ti sr t)

  | Lst [Id "ast_goto_indirect"; sr; e] -> let sr = xsr sr in STMT_cgoto (sr, ex sr e)
  | Lst [Id "ast_goto"; sr; id] -> let sr = xsr sr in STMT_goto (sr, xid id)
  | Lst [Id "ast_ifgoto"; sr; e; id] -> let sr = xsr sr in STMT_ifgoto (sr,ex sr e, xid id)
  | Lst [Id "ast_ifgoto_indirect"; sr; e1; e2] -> let sr = xsr sr in STMT_ifcgoto (sr,ex sr e1, ex sr e2)
  | Lst [Id "ast_likely_ifgoto"; sr; e; id] -> let sr = xsr sr in 
      STMT_ifgoto (sr, `EXPR_likely (sr,ex sr e), xid id)

  | Lst [Id "ast_unlikely_ifgoto"; sr; e; id] -> let sr = xsr sr in 
      STMT_ifgoto (sr, `EXPR_unlikely (sr,ex sr e), xid id)

  | Lst [Id "ast_ifnotgoto"; sr; e; id] -> let sr = xsr sr in 
      STMT_ifgoto (sr, lnot (sr) (ex sr e), xid id)

  | Lst [Id "ast_likely_ifnotgoto"; sr; e; id] -> let sr = xsr sr in 
      STMT_ifgoto (
        sr,
        `EXPR_likely (sr, lnot (sr) (ex sr e)),
        xid id)

  | Lst [Id "ast_unlikely_ifnotgoto"; sr; e; id] -> let sr = xsr sr in 
      STMT_ifgoto (
        sr,
        `EXPR_unlikely (sr, lnot (sr) (ex sr e)),
        xid id)

  | Lst [Id "ast_ifreturn"; sr; e] -> let sr = xsr sr in STMT_ifreturn (sr,ex sr e)
  | Lst [Id "ast_invariant"; sr; e] -> let sr = xsr sr in STMT_invariant (sr,ex sr e)

  | Lst [Id "ast_ifdo"; sr; e; sts1; sts2] -> let sr = xsr sr in STMT_ifdo (sr,ex sr e, xsts sr sts1, xsts sr sts2)
  | Lst [Id "ast_call"; sr; f; a] -> let sr = xsr sr in STMT_call (sr,ex sr f,ex sr a)
  | Lst [Id "ast_call_with_trap"; sr; f; a] -> let sr = xsr sr in STMT_call_with_trap (sr,ex sr f,ex sr a)
  | Lst [Id "ast_assign"; sr; id; tlv; a] -> let sr = xsr sr in 
      STMT_assign (sr, xid id, xtlv sr tlv, ex sr a)
  | Lst [Id "ast_cassign"; sr; e1; e2] -> let sr = xsr sr in STMT_cassign (sr,ex sr e1, ex sr e2)
  | Lst [Id "ast_jump"; sr; e1; e2] -> let sr = xsr sr in STMT_jump (sr,ex sr e1, ex sr e2)
  | Lst [Id "ast_loop"; sr; id; e2] -> let sr = xsr sr in 
      STMT_loop (sr, xid id, ex sr e2)
  | Lst [Id "ast_svc"; sr; id] -> let sr = xsr sr in STMT_svc (sr, xid id)
  | Lst [Id "ast_fun_return"; sr; e] -> let sr = xsr sr in STMT_fun_return (sr,ex sr e)

  | Lst [Id "ast_yield"; sr; e] -> let sr = xsr sr in STMT_yield (sr,ex sr e)
  | Lst [Id "ast_proc_return"; sr]  -> let sr = xsr sr in STMT_proc_return (sr)
  | Lst [Id "ast_proc_return_from"; sr; id] -> let sr = xsr sr in STMT_proc_return_from (sr, xid id)
  | Lst [Id "ast_halt"; sr; Str s] -> let sr = xsr sr in STMT_halt (sr, s)
  | Lst [Id "ast_trace"; sr; id; Str s] -> let sr = xsr sr in STMT_trace (sr, xid id, s)
  | Lst [Id "ast_nop"; sr; Str s] -> let sr = xsr sr in STMT_nop (sr,s)
  | Lst [Id "ast_assert"; sr; e] -> let sr = xsr sr in STMT_assert (sr,ex sr e)
  | Lst [Id "ast_init"; sr; id; e] -> let sr = xsr sr in STMT_init (sr, xid id, ex sr e)

  | Lst [Id "ast_newtype"; sr; id; vs; t] -> let sr = xsr sr in 
      STMT_newtype (sr, xid id, xvs sr vs, ti sr t)

  | Lst [Id "ast_instance_type"; sr; id; vs; t] -> let sr = xsr sr in 
      STMT_instance_type (sr, xid id, xvs sr vs, ti sr t)

  | Lst [Id "ast_virtual_type"; sr; id] -> let sr = xsr sr in 
      STMT_virtual_type (sr, xid id)

  | Lst [Id "ast_abs_decl"; sr; id; vs; tqs; ct; req] -> let sr = xsr sr in 
      STMT_abs_decl (sr, xid id, xvs sr vs, xtqs sr tqs, xc sr ct, xrr sr req)

  | Lst [Id "ast_ctypes"; sr; Lst ids; tqs; req] -> let sr = xsr sr in 
      let ids = map (fun id -> sr, xid id) ids in
      STMT_ctypes (sr, ids, xtqs sr tqs, xrr sr req)

  | Lst [Id "ast_const_decl"; sr; id; vs; t; ct; req] -> let sr = xsr sr in 
      STMT_const_decl (sr, xid id, xvs sr vs, ti sr t, xc sr ct, xrr sr req)

  | Lst [Id "ast_fun_decl"; sr; id; vs; Lst ps; t; ct; req; Str prec] -> let sr = xsr sr in 
(*
print_endline ("processig fun decl " ^ xid id);
print_endline ("vs list as sex = " ^ Sex_print.string_of_sex vs);
print_endline ("vs = " ^ Flx_print.string_of_vs (xvs sr vs));
print_endline ("Argtypes = " ^ Flx_util.catmap ", " Flx_print.string_of_typecode (List.map (ti sr) ps));
*)
      STMT_fun_decl (
        sr,
        xid id,
        xvs sr vs,
        List.map (ti sr) ps,
        ti sr t,
        xc sr ct,
        xrr sr req,
        prec)
  | Lst [Id "ast_callback_decl"; sr; id; Lst ps; t; req] -> let sr = xsr sr in 
      STMT_callback_decl (sr, xid id, map (ti sr) ps, ti sr t, xrr sr req)

  | Lst [Id "ast_insert"; sr; id; vs; ct; ik; req] -> 
      let sr = xsr sr in 
      let xik = function
        | Id "header" -> `Header
        | Id "body" -> `Body
        | Id "package" -> `Package
        | x -> err x "ikind_t"
      in
      STMT_insert (sr, xid id, xvs sr vs, xc sr ct, xik ik, xrr sr req)

  | Lst [Id "ast_code"; sr; ct; arg] -> 
    let sr = xsr sr in
    STMT_code (sr, xc sr ct, ex sr arg)

  | Lst [Id "ast_noreturn_code"; sr; ct; arg] -> 
    let sr = xsr sr in 
    STMT_noreturn_code (sr, xc sr ct, ex sr arg)

  | Lst [Id "ast_export_fun"; sr; sn; Str s] -> let sr = xsr sr in 
    let xsn x = match suffixed_name_of_expr (ex sr x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_fun  (sr, xsn sn, s)

  | Lst [Id "ast_export_cfun"; sr; sn; Str s] -> let sr = xsr sr in 
    let xsn x = match suffixed_name_of_expr (ex sr x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_cfun  (sr, xsn sn, s)

  | Lst [Id "ast_export_python_fun"; sr; sn; Str s] -> let sr = xsr sr in 
    let xsn x = match suffixed_name_of_expr (ex sr x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_python_fun  (sr, xsn sn, s)

  | Lst [Id "ast_export_type"; sr; t; Str s] -> let sr = xsr sr in 
    STMT_export_type (sr, ti sr t, s)

  | Lst [Id "ast_export_struct"; sr; Str s] -> let sr = xsr sr in 
    STMT_export_struct (sr, s)

  | Lst [Id "ast_export_union"; sr; sn; Str s] -> let sr = xsr sr in 
    let xsn x = match suffixed_name_of_expr (ex sr x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_union (sr, xsn sn, s)

  | Lst [Id "ast_export_requirement"; sr; req] -> 
    let sr = xsr sr in 
    let req = xrr sr req in
    STMT_export_requirement (sr, req)

 
  | Lst [Id "ast_stmt_match";  Lst [sr; e; Lst pss]] -> let sr = xsr sr in 
    let pss = map (function
      | Lst [p;stmts] ->  xp p, xsts sr stmts
      | x -> err x "ast_stmt_match syntax"
      )
    pss
    in
    STMT_stmt_match (sr, (ex sr e,pss))

  | Lst [Id "ast_stmt_chainmatch";  sr; Lst links] as x -> 
    if List.length links = 0 then err x "invalid chain match, no cases";
    let sr = xsr sr in 
    let camel ps : pattern_t * statement_t list = 
      match ps with
      | Lst [p;stmts] -> xp p, xsts sr stmts
      | x -> err x "ast_stmt_match, case clause"
    in
    let calmatch e pss = STMT_stmt_match (sr, (e,pss)) in
    let wild = PAT_any sr in
    let append_wild cases stmts : (pattern_t * statement_t list)list = cases @ [wild, stmts] in
    let revlinks = List.rev links in
    List.fold_left (fun acc link ->
      match link with
      | Lst [e; Lst pss] -> calmatch (ex sr e) (append_wild (map camel pss) [acc])
      | x -> err x "ast_stmt_chainmatch chain syntax"
    ) 
    (match List.hd revlinks with
      | Lst [e; Lst pss] -> calmatch (ex sr e) (map camel pss)
      | x -> err x "ast_stmt_chainmatch chain syntax last case"
    )
    (List.tl revlinks)

  | Lst[Id "objc_bind_class_interface"; sr; stuff; reqs] -> bind_objc_class_interface sr stuff reqs
  | Lst[Id "objc_bind_protocol_interface"; sr; stuff; reqs] -> bind_objc_protocol_interface sr stuff reqs


  | x -> err x "statement"

(* ------- OBJC STUFF ------------*)

and decode_keyword_selector sr kws : string list * string * Flx_ast.typecode_t list * Flx_ast.typecode_t =
    print_endline ("Objc keyword selector ");
    let names, types = List.fold_left (fun (names,types) kwp -> match kwp with
      | Lst [Id "objc_keyword_declarator"; Str name; typ] ->
        let param_type = xtype_t sr typ in
        print_endline ("  param=" ^ name ^ ":(" ^ Flx_print.string_of_typecode param_type  ^")");
        
        name :: names, param_type :: types
      | x -> err x "objc keyword param"
      )
      ([],[])
      kws
    in
    let types = List.rev types in
    let name = fold_left (fun name part -> part ^ "'" ^ name) "" names in
    print_endline ("Method name = " ^ name);
    let typ = `TYP_tuple types in
    print_endline ("Method argument type = " ^ Flx_print.string_of_typecode typ);
    List.rev names,name,types,typ


(* calculate paramater tuple of method, excludes object or class, empty Slist if unit typed argument *)
and cal_params sr types : parameter_t sexpr_t = 
  let n = List.length types in
  let mkparam j : parameter_t = sr, `PVal,  "_" ^ string_of_int (j+2), List.nth types j, None in
  let param2_list = List.rev (List.fold_left (fun acc j -> mkparam j :: acc) [] (Flx_list.nlist n)) in
  let paramspec2 : parameter_t sexpr_t = 
    match param2_list with  
      | [] -> Slist [] (* unit *)
      | [p] -> Satom p (* one argument *)
      | _ -> Slist (List.map (fun p -> Satom p) param2_list) (* multiple arguments *)
  in 
  paramspec2


and cal_method_decl sr name paramss return_type body =
    let fun_decl = STMT_curry (sr, name, dfltvs, paramss,(return_type,None), `TYP_tuple[],`Generator,[`Generated "ObjCMethod"],body) in
    print_endline ("  -->> Method binding = " ^ Flx_print.string_of_statement 0 fun_decl);
    fun_decl 

and decode_property_spec classname sr attr var =
  let readonly = 
    match attr with
    | Lst [Lst attrs] -> 
       List.fold_left (fun result attr -> match attr with | Str "readonly" -> true | _ -> result) false attrs
    | Lst [] -> false
    | x -> err x "property attributes"
  in
  match var with 
  | Lst [Id "Pval"; Str name; typ] -> 
    let typ = xtype_t sr typ in

    (* get method *)
    let paramspec1 : parameter_t sexpr_t = Satom (sr, `PVal, "_1", `TYP_name (sr,classname,[]), None) in 
    let paramss = [paramspec1, None] in
    let obj = `EXPR_name (sr, "_1",[]) in
    let argument =  obj in
    let code_string = "[$1 "^name^"]" in
    let code_spec = Flx_code_spec.Str_template code_string in
    let body = [STMT_fun_return (sr,`EXPR_expr (sr, code_spec, typ, argument))] in
    let get_method = cal_method_decl sr name paramss typ body in
    if readonly then
      get_method
    else
      let paramspec2: parameter_t sexpr_t = Satom (sr, `PVal, "_2", typ, None) in
      let paramss : params_t list = [paramspec1, None; paramspec2, None] in
      let argument = `EXPR_tuple (sr, [obj ; `EXPR_name (sr, "_2",[])]) in 
      let code_string = "[$1 set" ^ name ^ ": $2]" in
      let code_spec = Flx_code_spec.Str_template code_string in
      let body = [STMT_code (sr, code_spec, argument)] in
      let set_method = cal_method_decl sr ("set"^name^"'") paramss (`TYP_void sr) body in
      STMT_seq (sr, [get_method; set_method])

  | x -> err x "property name and type"

and decode_instance_method_spec objt sr return_spec selector =
  print_endline ("instance method declaration");
  let return_type = xtype_t sr return_spec in 
  print_endline ("  Return type " ^ Flx_print.string_of_typecode return_type);

  begin match selector with
  | Lst [Id "objc_method_selector_name"; Str name] ->
    print_endline ("No argument method named " ^ name);
    let paramspec1 : parameter_t sexpr_t = Satom (sr, `PVal, "_1", `TYP_name (sr,objt,[]), None) in 
    let paramss = [paramspec1, None] in
    let obj = `EXPR_name (sr, "_1",[]) in
    let argument =  obj in
    let code_string = "[$1 "^name^"]" in
    let code_spec = Flx_code_spec.Str_template code_string in
    let body = 
      match return_type with
      | `TYP_name (_,"void",_) -> [STMT_code (sr, code_spec, argument)]
      | _ -> [STMT_fun_return (sr,`EXPR_expr (sr, code_spec, return_type, argument))] 
    in
    let binding = cal_method_decl sr name paramss return_type body in
    binding

  | Lst [Id "objc_keyword_selector"; Lst kws] ->
    let names, name, types, typ = decode_keyword_selector sr kws in
    let paramspec2 = cal_params sr types in

    let paramspec1 : parameter_t sexpr_t = Satom (sr, `PVal, "_1", `TYP_name (sr,objt,[]), None) in 
    let paramss : params_t list = [paramspec1,None; paramspec2,None] in
    let obj = `EXPR_name (sr, "_1",[]) in
    let n = List.length names in
    let argument =  
      `EXPR_tuple (sr,obj :: List.map (fun i -> `EXPR_name (sr, "_"^ string_of_int (i+2), [])) (Flx_list.nlist n)) 
    in
    let code_string = "[$1 "^(String.concat "" (List.map2 (fun i kw -> kw^":$" ^ string_of_int (i+2)^" ") (Flx_list.nlist n) names))^ "]" in
    let code_spec = Flx_code_spec.Str_template code_string in
    let body = 
      match return_type with
      | `TYP_name (_,"void",_) -> [STMT_code (sr, code_spec, argument)]
      | _ -> [STMT_fun_return (sr,`EXPR_expr (sr, code_spec, return_type, argument))] 
    in
    let binding = cal_method_decl sr name paramss return_type body in
    binding 

  | Lst [Id "objc_keyword_selector_ellipsis"; stuff] as x ->
    print_endline ("Objc keyword selector ellipsis");
    err x "Method with ellipsis not implemented yet"

  | x -> err x "Method Argument types"
  end

and decode_class_method_spec classname sr return_spec selector = 
  print_endline ("Class method declaration");
  let return_type = xtype_t sr return_spec in 
  print_endline ("  Return type " ^ Flx_print.string_of_typecode return_type);

  begin match selector with
  | Lst [Id "objc_method_selector_name"; Str name] ->
    print_endline ("No argument method named " ^ name);
    let paramspec2 = Slist [] in
    let paramss = [paramspec2,None] in
    let obj = `EXPR_name (sr, classname,[]) in
    let argument =  obj in
    let code_string = "["^classname ^" "^name^"]" in
    let code_spec = Flx_code_spec.Str_template code_string in
    let argument = `EXPR_tuple (sr,[]) in
    let body = 
      match return_type with
      | `TYP_name (_,"void",_) -> [STMT_code (sr, code_spec, argument)]
      | _ -> [STMT_fun_return (sr,`EXPR_expr (sr, code_spec, return_type, argument))] 
    in
    let binding = cal_method_decl sr (classname^"'"^name) paramss return_type body in
    binding

  | Lst [Id "objc_keyword_selector"; Lst kws] ->
    let names, name, types, typ = decode_keyword_selector sr kws in
    let paramspec2 = cal_params sr types in

    let paramss : params_t list = [paramspec2,None] in
    let obj = `EXPR_name (sr, classname,[]) in
    let n = List.length names in
    let argument =  
      `EXPR_tuple (sr,obj :: List.map (fun i -> `EXPR_name (sr, "_"^ string_of_int (i+1), [])) (Flx_list.nlist n)) 
    in
    let code_string = "["^classname^ "'" ^(String.concat "" (List.map2 (fun i kw -> kw^":$" ^ string_of_int (i+1)^" ") (Flx_list.nlist n) names))^ "]" in
    let code_spec = Flx_code_spec.Str_template code_string in
    let body = 
      match return_type with
      | `TYP_name (_,"void",_) -> [STMT_code (sr, code_spec, argument)]
      | _ -> [STMT_fun_return (sr,`EXPR_expr (sr, code_spec, return_type, argument))] 
    in
    let binding = cal_method_decl sr (classname^"'"^name) paramss return_type body in
    binding 

  | Lst [Id "objc_keyword_selector_ellipsis"; stuff] as x ->
    print_endline ("Objc keyword selector ellipsis");
    err x "Method with ellipsis not implemented yet"

  | x -> err x "Method Argument types"
  end

and supertype_specification sr super sub =
  let fname = "_supertype_"^super in
  let argt = `TYP_name (sr, sub, []) in
  let ret = `TYP_name (sr,super,[]) in
  let body = CS.Str_template "$1" in
  let reqs = RREQ_atom Subtype_req in
  let prec = "" in
  let supercoercion = STMT_fun_decl (sr,fname,dfltvs,[argt],ret,body,reqs,prec) in
  print_endline ("Supercoercion function = " ^ Flx_print.string_of_statement 0 supercoercion);
  supercoercion

and bind_objc_class_interface sr stuff reqs :Flx_ast.statement_t = 
  let sr = xsr sr in 
  let reqs = xraw_req_expr_t sr reqs  in
  match stuff with
  | Lst [Id "objc_class_interface"; Str classname; super; protocol_reference_list; instance_variables; interface_declaration] ->

    (* FIND SUPERTYPE *)
    let super = 
       match super with
       | Lst [] -> "NSObject" 
       | Lst [Lst [Str ":"; Str name]] -> name
       | x -> err x "obj super class"
    in
    print_endline ("Objc Class " ^ classname ^ (if super = "" then "" else ": " ^ super));

    (* BIND CLASS TYPE *)
    let proxy_name = "_derefproxy_" ^ classname in
    let class_type = STMT_abs_decl (sr, classname, Flx_ast.dfltvs, [], Str (classname^ "*"), reqs) in 

    (* because stupid Felix gives a typedef .. even though the type is never used .. we give it a real type name *)
    let derefproxy_type = STMT_abs_decl (sr, proxy_name, Flx_ast.dfltvs, [], Str ("void"), reqs) in 

    (* OVERLOAD DEREF OPERATOR *)
    let deref_operator = STMT_fun_decl (sr,"deref",dfltvs,[`TYP_name (sr, classname, []) ],`TYP_name (sr, proxy_name,[]),CS.Str_template "$1",reqs,"") in

    let protocols = 
      match protocol_reference_list with
      | Lst [Lst names] ->
          List.map (fun super -> let super = xid super in supertype_specification sr super classname ) names
      | Lst [] -> []
      | x -> err x "protocol list"
    in

    (* CONSTRUCT IVAR PROJECTIONS *)
    let ivar_projections = 
      match instance_variables with
      | Lst [Lst ivspecs] ->
        print_endline ("Got instance variable spec list");
        List.map (fun ivspec -> match ivspec with
          | Lst [Id "Pval"; Str ivar; typ] -> 
            let typ = xtype_t sr typ in
            print_endline ("Instance variable " ^ ivar ^ ", type=" ^ Flx_print.string_of_typecode typ);

            (* Pointer projection: our type is an abstract pointer! *)
            let fname = ivar in
            let argt = `TYP_name (sr, classname, []) in
            let ret = `TYP_pointer typ in
            let body = CS.Str_template ("&($1->"^ivar ^")") in (* MACHINE POINTER! *)
            let reqs = RREQ_true in
            let prec = "" in
            let pointer_projection = STMT_fun_decl (sr,fname,dfltvs,[argt],ret,body,reqs,prec) in
            print_endline ("ivar pointer projection = " ^ Flx_print.string_of_statement 0 pointer_projection);

            (* value projection using phantom proxy type *)
            let argt = `TYP_name (sr, proxy_name,[]) in
            let ret =  typ in
            let body = CS.Str_template ("$1->"^ivar) in
            let value_projection = STMT_fun_decl (sr,fname,dfltvs,[argt],ret,body,reqs,prec) in
            print_endline ("ivar value projection = " ^ Flx_print.string_of_statement 0 value_projection);
            STMT_seq (sr, [value_projection; pointer_projection])
   
          | x -> err x "Objc instance variable spec, expected ivar decl or vis spec"
        ) 
        ivspecs

      | Lst [] -> print_endline "Optional instance variable spec list omitted"; []
      | x -> err x "objc instance variables"
    in

    (* BIND METHODS *)
    let iface_decl = 
      match interface_declaration with
      | Lst [] -> 
        print_endline ("Optional interface spec ommitted");
        STMT_nop (sr, "Omitted optional interface spec")

      | Lst ifaces ->
        print_endline ("Got interface spec");
        let methods = 
          List.map (fun ispec -> match ispec with
          | Lst [Id "objc_class_method_declaration"; sr; typ; selector] ->  
            print_endline ("Class method declaration");
            let sr = xsr sr in
            decode_class_method_spec classname sr typ selector

          | Lst [Id "objc_instance_method_declaration"; sr; typ; selector ] ->
            print_endline ("Instance method declaration");
            let sr = xsr sr in
            decode_instance_method_spec classname sr typ selector

          | Lst [Id "objc_property"; sr; property_attributes; var] -> 
            let sr = xsr sr in
            decode_property_spec classname sr property_attributes var 

          | x -> err x "Objc interface element"
        )
        ifaces
        in
        let supercoercion = supertype_specification sr super classname in
        STMT_seq (sr,class_type :: supercoercion :: derefproxy_type :: deref_operator ::  protocols @ ivar_projections @ methods)

      | x -> err x "objc interface"
   in
   iface_decl

  | x -> err x "objc bind class interface"

and bind_objc_protocol_interface sr stuff reqs :Flx_ast.statement_t = 
  let sr = xsr sr in 
  let reqs = xraw_req_expr_t sr reqs  in
  match stuff with
  | Lst [Id "objc_protocol_interface"; Str classname; protocol_reference_list; interface_declaration] ->
    print_endline ("Objc Protocol " ^ classname);

    (* A protocol is an incomplete type, so we just use C struct tag for the binding *)
    let class_type = STMT_abs_decl (sr, classname, Flx_ast.dfltvs, [], Str ("struct " ^ classname^ "*"), reqs) in  
    let protocols = 
      match protocol_reference_list with
      | Lst [Lst names] ->
          List.map (fun super -> let super = xid super in supertype_specification sr super classname ) names
      | Lst [] -> []
      | x -> err x "protocol list"
    in
    let iface_decl = 
      match interface_declaration with
      | Lst [] -> 
        print_endline ("Optional interface spec ommitted");
        STMT_nop (sr, "Omitted optional interface spec")

      | Lst ifaces ->
        print_endline ("Got interface spec");
        let methods = 
          List.map (fun ispec -> match ispec with
          | Lst [Id "objc_class_method_declaration"; sr; typ; selector] ->  
            print_endline ("Class method declaration");
            let sr = xsr sr in
            decode_class_method_spec classname sr typ selector

          | Lst [Id "objc_instance_method_declaration"; sr; typ; selector ] ->
            print_endline ("Instance method declaration");
            let sr = xsr sr in
            decode_instance_method_spec classname sr typ selector

          | Lst [Id "objc_property"; property_attribues; var] -> 
            print_endline ("Property declaration not implemented");
            STMT_nop (sr, "property decl not implemented yet")

          | x -> err x "Objc interface element"
        )
        ifaces
        in
        STMT_seq (sr,class_type :: protocols @ methods)

      | x -> err x "objc interface"
   in
   iface_decl

  | x -> err x "objc bind protocol interface"


