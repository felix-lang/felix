open Flx_ast
open Sex_types
open Flx_typing2

(*
open Flx_types
open Flx_typing
*)
open List

exception Sex2FlxTypeError of string * sexp_t

let err x s =
  print_string ("[sex2flx] ERROR in " ^ s ^ " in " ^ Sex_print.string_of_sex x);
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

let xsr x : Flx_srcref.t =
  let ii i = int_of_string i in
  match x with
  | Lst [Str fn; Int fl; Int fc; Int ll; Int lc] ->
      Flx_srcref.make (fn,ii fl,ii fc,ii ll,ii lc)
  | x -> err x "Invalid source reference"

let rec xliteral_t sr x =
  match x with
  | Lst [Id "ast_int"; Str s; Int i] -> AST_int (s, i)
  | Lst [Id "ast_int"; Str s; Str i] -> AST_int (s, i)
  | Lst [Id "ast_string"; Str s] -> AST_string (s)
  | Lst [Id "ast_cstring"; Str s] -> AST_cstring (s)
  | Lst [Id "ast_wstring"; Str s] -> AST_wstring (s)
  | Lst [Id "ast_ustring"; Str s] -> AST_ustring (s)
  | Lst [Id "ast_float"; Str s1; Str s2] -> AST_float (s1, s2)
  | x -> err x "invalid literal"


and xast_term_t sr x =
  let ex x = xexpr_t sr x in
  let xs x = xstatement_t sr x in
  let xsts x =  lst "statement" xs x in
  let xterm x = xast_term_t sr x in
  match x with
  | Lst [Id "Expression_term"; e] -> Expression_term (ex e)
  | Lst [Id "Statement_term"; st] -> Statement_term (xs st)
  | Lst [Id "Statements_term"; sts] -> Statements_term (xsts sts)
  | Lst [Id "Identifier_term"; Str s] -> Identifier_term s
  | Lst [Id "Keyword_term"; Str s] -> Keyword_term s
  | Lst [Id "Apply_term"; fn; args] ->
    Apply_term (xterm fn, lst "ast_term" xterm args)
  | x -> err x "invalid ast_term_t"

and type_of_sex sr w =
  (*
  print_endline ("Converting sexp " ^ Sex_print.string_of_sex w ^ " to a type");
  *)
  let x = xexpr_t sr w in
  (*
  print_endline ("Felix expression is " ^ Flx_print.string_of_expr x);
  *)
  let y =
    match x with
    | EXPR_tuple (_,[]) -> TYP_tuple []
    | EXPR_name (_,"none",[]) -> TYP_none
    | EXPR_name (_,"typ_none",[]) -> TYP_none
    | x ->
      try typecode_of_expr x
      with xn ->
        print_endline ("Converting sexp " ^ Sex_print.string_of_sex w ^ " to a type");
        print_endline ("Felix expression is " ^ Flx_print.string_of_expr x);
        print_endline ("Got error: " ^ Printexc.to_string xn);
        raise xn
  in
  (*
  print_endline ("Felix type is " ^ Flx_print.string_of_typecode y);
  *)
  y

and xexpr_t sr x =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let ii i = int_of_string i in
  let xq m qn = qne ex m qn in
  let xp x = xpattern_t x in
  let xps x =  xparams_t sr x in
  let xvs x = xvs_list_t sr x in
  let xs x = xstatement_t sr x in
  let xsts x =  lst "statement" xs x in
  let xterm x = xast_term_t sr x in
  match x with
  | Str s -> print_endline ("Deprecated Scheme string "^ s ^"' as Felix string"); EXPR_literal (sr, (AST_string s))
  | Lst [] -> EXPR_tuple (sr,[])
  | Lst [x] -> ex x
  (* this term comes from the hard coded parser! *)
  | Lst [Id "ast_vsprintf";  Str s] -> EXPR_vsprintf (sr,s)
  | Lst [Id "ast_noexpand"; sr; e] -> EXPR_noexpand (xsr sr,ex e)
  | Lst [Id "ast_name"; sr; Id s ; Lst ts] -> EXPR_name (xsr sr,s,map ti ts)
  | Lst [Id "ast_name"; sr; Str s ; Lst ts] -> EXPR_name (xsr sr,s,map ti ts)
  | Lst [Id "ast_the";  sr; e] -> EXPR_the (xsr sr, xq "ast_the" e) 
  (* can't occur in user code
  | Lst [Id "ast_index";  Str s ; Int i] -> EXPR_index (sr,s,ii i)
  *)

  | Lst [Id "ast_case_tag";  sr; Int i] -> EXPR_case_tag (xsr sr,ii i)
  | Lst [Id "ast_typed_case";  Int i; t] -> EXPR_typed_case (sr,ii i,ti t)
  | Lst [Id "ast_lookup";  Lst [e; Str s; Lst ts]] -> EXPR_lookup (sr,(ex e, s,map ti ts))
  | Lst [Id "ast_apply";  sr; Lst [e1; e2]] -> EXPR_apply(xsr sr,(xexpr_t (xsr sr) e1, xexpr_t (xsr sr) e2))
  | Lst [Id "ast_tuple";  sr; Lst es] -> EXPR_tuple (xsr sr,map (xexpr_t (xsr sr)) es)
  | Lst [Id "ast_record";  sr; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, xexpr_t (xsr sr) e
     | x -> err x "Error in AST_record"
     )
     rs
   in EXPR_record (xsr sr,rs)

 | Lst [Id "ast_record_type"; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, ti e
     | x -> err x "Error in AST_record_type"
     )
     rs
   in EXPR_record_type (sr,rs)

 | Lst [Id "ast_variant";  Lst [Str s;e]] -> EXPR_variant (sr,(s, ex e))

 | Lst [Id "ast_variant_type"; Lst rs] ->
   let rs =
     map (function
     | Lst [Str s; e] -> s, ti e
     | x -> err x "Error in AST_variant_type"
     )
     rs
   in EXPR_variant_type (sr,rs)


 | Lst [Id "ast_arrayof";  sr; Lst es] -> EXPR_arrayof (xsr sr, map ex es)
 | Lst [Id "ast_coercion";  sr; Lst [e; t]] ->  EXPR_coercion (xsr sr,(ex e, ti t))

 | Lst [Id "ast_suffix";  Lst [qn;t]] -> EXPR_suffix (sr,(xq "ast_suffix" qn,ti t))

 | Lst [Id "ast_patvar";  sr; Str s] -> EXPR_patvar (xsr sr, s)
 | Lst [Id "ast_patany"; sr] -> EXPR_patany (xsr sr)
 | Lst [Id "ast_void"; sr] -> EXPR_void (xsr sr)
 | Lst [Id "ast_ellipsis"; sr] -> EXPR_ellipsis (xsr sr)

 | Lst [Id "ast_product"; sr; Lst es] -> EXPR_product (xsr sr, map (xexpr_t (xsr sr)) es)
 | Lst [Id "ast_sum";  sr; Lst es] -> EXPR_sum (xsr sr,map (xexpr_t (xsr sr)) es)
 | Lst [Id "ast_intersect"; Lst es] -> EXPR_intersect (sr, map ex es)
 | Lst [Id "ast_isin"; Lst [a; b]] -> EXPR_isin (sr, (ex a, ex b))
 | Lst [Id "ast_setintersection"; sr; Lst es] -> EXPR_setintersection (xsr sr, map ex es)
 | Lst [Id "ast_setunion"; sr; Lst es] -> EXPR_setunion (xsr sr, map (xexpr_t (xsr sr)) es)
 | Lst [Id "ast_orlist"; sr; Lst es] -> EXPR_orlist (xsr sr, map (xexpr_t (xsr sr)) es)
 | Lst [Id "ast_andlist"; sr; Lst es] -> EXPR_andlist (xsr sr, map (xexpr_t (xsr sr)) es)
 | Lst [Id "ast_arrow";  Lst [e1; e2]] -> EXPR_arrow (sr,(ex e1, ex e2))
 | Lst [Id "ast_longarrow";  Lst [e1; e2]] -> EXPR_longarrow (sr,(ex e1, ex e2))
 | Lst [Id "ast_superscript";  Lst [e1; e2]] -> EXPR_superscript (sr,(ex e1, ex e2))
 | Lst [Id "ast_literal";  sr; lit] -> EXPR_literal (xsr sr, xliteral_t sr lit)
 | Lst [Id "ast_deref"; sr; e] -> EXPR_deref (xsr sr,ex e)
 | Lst [Id "ast_ref"; sr; e] -> EXPR_ref (xsr sr,ex e)
 | Lst [Id "ast_new"; sr; e] -> EXPR_new (xsr sr,ex e)
 | Lst [Id "ast_likely"; sr; e] -> EXPR_likely (xsr sr,ex e)
 | Lst [Id "ast_unlikely"; sr; e] -> EXPR_unlikely (xsr sr,ex e)
 | Lst [Id "ast_callback";  sr; qn] -> EXPR_callback (xsr sr,xq "ast_callback" qn)
 | Lst [Id "ast_dot"; sr; Lst [e1; e2]] -> EXPR_dot (xsr sr,(xexpr_t (xsr sr) e1, xexpr_t (xsr sr) e2))
 | Lst [Id "ast_lambda";  sr; Lst [vs; Lst pss; t; sts]] -> EXPR_lambda  (xsr sr, (xvs vs, map xps pss, ti t, xsts sts))

 (* can't occur in user code
 | Lst [Id "ast_match_ctor";  Lst [qn; e]] -> EXPR_match_ctor(sr,(xq "ast_match_ctor" qn,ex e))
 | Lst [Id "ast_match_case";  Lst [Int i; e]]-> EXPR_match_case (sr,(ii i, ex e))
 | Lst [Id "ast_ctor_arg";  Lst [qn; e]] -> EXPR_ctor_arg (sr,(xq "ast_ctor_arg" qn, ex e))
 | Lst [Id "ast_case_arg"; Lst [Int i; e]] -> EXPR_case_arg (sr,(ii i, ex e))
 *)
 | Lst [Id "ast_case_index";  sr; e] -> EXPR_case_index (xsr sr, ex e)
 | Lst [Id "ast_letin";  sr; Lst [p; e1; e2]] -> EXPR_letin (xsr sr,(xp p, ex e1, ex e2))

 | Lst [Id "ast_get_n";  sr;  Lst [Int i; e]] -> EXPR_get_n(xsr sr,(ii i, ex e))
 (* extractor for record components, can't occur in user code
 | Lst [Id "ast_get_named_variable";  Lst [Str s;e]]-> EXPR_get_named_variable (sr, (s, ex e))
 *)
 | Lst [Id "ast_as";  sr; Lst [e; Str s]] -> EXPR_as (xsr sr,(ex e, s))
 | Lst [Id "ast_match";  sr; Lst [e; Lst pes]]->
   let pes = map (function
     | Lst [p;e] -> xp p, ex e
     | x -> err x "ast_match syntax"
     )
     pes
   in
   EXPR_match (xsr sr, (ex e,pes))

 (* handled in flx_typing2 now 
 | Lst [Id "ast_typeof";  e] -> EXPR_typeof (sr, ex e)
 *)

 | Lst [Id "ast_cond"; sr;  Lst [e1;e2;e3]] -> EXPR_cond (xsr sr,(ex e1, ex e2, ex e3))
 | Lst [Id "ast_expr"; sr; Str s; t] -> EXPR_expr (xsr sr, s, ti t)

 | Lst [Id "ast_type_match";  sr; Lst [t; Lst ts]] ->
   let ts =
     map (function
       | Lst [t1; t2] -> ti t1, ti t2
       | x -> err x "ast_typematch typerrror"
     )
     ts
   in EXPR_type_match (xsr sr,(ti t, ts))

  | Lst ls -> (* print_endline ("Unexpected literal tuple"); *) EXPR_tuple (sr, map ex ls)

  | Id y -> print_endline ("Unexpected ID=" ^ y); EXPR_name (sr,y,[])
  | Int i ->
    EXPR_literal (sr, AST_int ("int",i))

  | x ->
    err x "expression"

and xfloat_pat x =
  match x with
  | Lst [Id "Float_plus"; Str ty; Str vl] -> Float_plus (ty, vl)
  | Lst [Id "Float_minus"; Str ty; Str vl] -> Float_minus (ty, vl)
  | Id "Float_inf" -> Float_inf
  | Id "Float_minus_inf" -> Float_minus_inf
  | x -> err x "Float_pat syntax error"

and xpattern_t x =
  let xp x = xpattern_t x in
  let ti sr x = type_of_sex sr x in
  let xq sr m qn = qne (xexpr_t (xsr sr)) m qn in
  match x with
  | Lst [Id "pat_nan"; sr] -> PAT_nan (xsr sr)
  | Lst [Id "pat_none"; sr] -> PAT_none (xsr sr)

  (* constants *)
  | Lst [Id "pat_int"; sr; Str s; Int i] -> PAT_int (xsr sr,s, i)
  | Lst [Id "pat_string"; sr; Str s] -> PAT_string (xsr sr,s)

  (* ranges *)
  | Lst [Id "pat_int_range"; sr; Str s1; Int i1; Str s2; Int i2] ->
    PAT_int_range (xsr sr,s1, i1, s2, i2)

  | Lst [Id "pat_string_range"; sr; Str s1; Str s2] ->
    PAT_string_range (xsr sr,s1, s2)
  | Lst [Id "pat_float_range"; sr; p1; p2] ->
    PAT_float_range (xsr sr, xfloat_pat p1, xfloat_pat p2)

  (* other *)
  | Lst [Id "pat_coercion"; sr; p; t] ->
   PAT_coercion (xsr sr, xp p, ti (xsr sr) t)

  | Lst [Id "pat_name"; sr; Id x] -> PAT_name (xsr sr, x)
  | Lst [Id "pat_name"; sr; Str x] -> PAT_name (xsr sr, x)
  | Lst [Id "pat_tuple"; sr; Lst ps] -> PAT_tuple (xsr sr, map xp ps)

  | Lst [Id "pat_any"; sr] -> PAT_any (xsr sr)
  | Lst [Id "pat_const_ctor"; sr; qn] -> PAT_const_ctor (xsr sr, xq sr "pat_const_ctor" qn)
  | Lst [Id "pat_nonconst_ctor"; sr; qn; p] -> PAT_nonconst_ctor (xsr sr, xq sr "pat_nonconst_ctor" qn, xp p)

  | Lst [Id "pat_as"; sr; p; Id s] -> PAT_as (xsr sr, xp p, s)
  | Lst [Id "pat_as"; sr; p; Str s] -> PAT_as (xsr sr, xp p, s)
  | Lst [Id "pat_when"; sr; p; e] -> PAT_when (xsr sr, xp p, xexpr_t (xsr sr) e)

  | Lst [Id "pat_record"; sr; Lst ips] ->
    let ips = map (function
      | Lst [Id id; p] -> id,xp p
      | Lst [Str id; p] -> id,xp p
      | x -> err x "pat_record syntax"
      )
      ips
    in
    PAT_record (xsr sr, ips)
  | x ->
    err x "pattern"

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

and xraw_typeclass_insts_t sr x =
  let ex x = xexpr_t sr x in
  let xq m qn = qne ex m qn in
  match x with
  | Lst tcs -> map (xq "raw_typeclass_insts_t") tcs
  | x -> err x "raw_typeclass_insts_t"

and xvs_aux_t sr x : vs_aux_t =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let xrtc x = xraw_typeclass_insts_t sr x in
  match x with
  | Lst [ct; tcr] -> { raw_type_constraint=ti ct; raw_typeclass_reqs=xrtc tcr }
  | x -> err x "xvs_aux_t"

and xplain_vs_list_t sr x : plain_vs_list_t =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  match x with
  | Lst its -> map (function
    | Lst [Id s; t] -> s,ti t
    | Lst [Str s; t] -> s,ti t
    | x -> err x "xplain_vs_list"
    ) its
  | x -> err x "xplain_vs_list"

and xvs_list_t sr x : vs_list_t =
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

and xparam_kind_t sr x : param_kind_t =
  match x with
  | Id "PVal" -> `PVal
  | Id "PVar" -> `PVar
  | Id "PFun" -> `PFun
  | Id "PRef" -> `PRef
  | x -> err x "param_kind_t"

and xparameter_t sr x : parameter_t =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  let xpk x = xparam_kind_t sr x in
  match x with
  | Lst [pk; Id s; t; e] -> xpk pk, s, ti t,opt "dflt_arg" ex e
  | Lst [pk; Str s; t; e] -> xpk pk,s, ti t,opt "dflt_arg" ex e
  | x -> err x "parameter_t"

and xparams_t sr x : params_t =
  let ex x = xexpr_t sr x in
  let xpa x = xparameter_t sr x in
  match x with
  | Lst [Lst ps; eo] -> map xpa ps, opt "params" ex eo
  | x -> err x "params_t"

and xret_t sr x : typecode_t * expr_t option =
  let ex x = xexpr_t sr x in
  let ti x = type_of_sex sr x in
  match x with
  | Lst [t; e] -> ti t, opt "return" ex e
  | x -> err x "return encoding"

and xproperty_t sr x : property_t =
  match x with
  | Id "Recursive" -> `Recursive
  | Id "Inline" -> `Inline
  | Id "NoInline" -> `NoInline
  | Id "Inlining_started" -> `Inlining_started
  | Id "Inlining_complete" -> `Inlining_complete
  | Lst [Id "Generated"; Str s] -> `Generated (s)

  | Id "Heap_closure" -> `Heap_closure        (* a heaped closure is formed *)
  | Id "Explicit_closure" -> `Explicit_closure    (* explicit closure expression *)
  | Id "Stackable" -> `Stackable           (* closure can be created on stack *)
  | Id "Stack_closure" -> `Stack_closure       (* a stacked closure is formed *)
  | Id "Unstackable" -> `Unstackable         (* closure cannot be created on stack *)
  | Id "Pure" -> `Pure                (* closure not required by self *)
  | Id "Uses_global_var" -> `Uses_global_var     (* a global variable is explicitly used *)
  | Id "Ctor" -> `Ctor                (* Class constructor procedure *)
  | Id "Generator" -> `Generator           (* Generator: fun with internal state *)
  | Id "Yields" -> `Yields              (* Yielding generator *)
  | Id "Cfun" -> `Cfun                (* C function *)
  | Id "Lvalue" -> `Lvalue                (* primitive returning lvalue *)

  (* one of the below must be set before code generation *)
  | Id "Requires_ptf" -> `Requires_ptf        (* a pointer to thread frame is needed *)
  | Id "Not_requires_ptf" -> `Not_requires_ptf    (* no pointer to thread frame is needed *)

  | Id "Uses_gc" -> `Uses_gc             (* requires gc locally *)
  | Id "Virtual" -> `Virtual             (* interface in a typeclass *)
  | x -> err x "property_t"

and xfunkind_t sr x : funkind_t =
  match x with
  | Id "Function" -> `Function
  | Id "CFunction" -> `CFunction
  | Id "InlineFunction" -> `InlineFunction
  | Id "NoInlineFunction" -> `NoInlineFunction
  | Id "Virtual" -> `Virtual
  | Id "Ctor" -> `Ctor
  | Id "Generator" -> `Generator
  | x -> err x "funkind_t"

and xcode_spec_t sr x : code_spec_t =
  match x with
  | Lst [Id "StrTemplate"; Str s] -> CS_str_template (s)
  | Lst [Id "Str"; Str s] -> CS_str (s)
  | Id "Virtual" -> CS_virtual
  | Id "Identity" -> CS_identity
  | x -> err x "c_t"

and xlvalue_t sr x : lvalue_t =
  let ex x = xexpr_t sr x in
  let xtlv x = xtlvalue_t sr x in
  match x with
  | Lst [Id "Val"; sr; Str s] -> `Val (xsr sr,s)
  | Lst [Id "Var"; sr; Str s] -> `Var (xsr sr,s)
  | Lst [Id "Name"; sr; Str s] -> `Name (xsr sr,s)
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
  | Lst [Id c; io; vs; t] -> c,opt "union component" xi io,xvs vs, ti t
  | Lst [Str c; io; vs; t] -> c,opt "union component" xi io,xvs vs, ti t
  | x -> err x "union component"

and xstatement_t sr x : statement_t =
  let xpvs x = xplain_vs_list_t sr x in
  let xs x = xstatement_t sr x in
  let ex x = xexpr_t sr x in
  let ex' sr x = xexpr_t (xsr sr) x in
  let xq m qn = qne ex m qn in
  let xvs x = xvs_list_t sr x in
  let xam x =  xaxiom_method_t sr x in
  let xps x =  xparams_t sr x in
  let xret x =  xret_t sr x in
  let xsts x =  lst "statement" xs x in
  let xsts' sr x =  lst "statement" (xstatement_t (xsr sr)) x in
  let xprops x =  lst "property" (xproperty_t sr) x in
  let xfk x = xfunkind_t sr x in
  let ti x = type_of_sex sr x in
  let xid = function | Str n -> n | x -> err x "id" in
  let ii i = int_of_string i in
  let xi = function | Int i -> ii i | x -> err x "int" in
  let xtlv x = xtlvalue_t sr x in
  let xtq x = xtype_qual_t sr x in
  let xtqs x = lst "typ_equal_t" xtq x in
  let xc x = xcode_spec_t sr x in
  let xrr x = xraw_req_expr_t sr x in
  let xucmp x = xunion_component sr x in
  let xp x = xpattern_t x in
  let lnot sr x = EXPR_apply (sr,(EXPR_name (sr,"lnot",[]),x)) in
  match x with
  | Lst [] -> STMT_nop(sr,"null")
  | Lst [Id "ast_include"; sr; Str s] -> STMT_include (xsr sr, s)
  | Lst [Id "ast_open"; sr; vs; qn] -> STMT_open (xsr sr, xvs vs, xq "ast_open" qn)
  | Lst [Id "ast_inject_module"; sr; qn] -> STMT_inject_module (xsr sr, xq "ast_inject_module" qn)
  | Lst [Id "ast_use"; sr; Str s; qn] -> STMT_use (xsr sr, s, xq "ast_use" qn)
  | Lst [Id "ast_comment"; sr; Str s] -> STMT_comment(xsr sr, s)
  | Lst [Id "ast_private"; x] -> STMT_private (sr, xs x)
  | Lst [Id "ast_reduce"; sr; Str s; vs; spl; e1; e2] ->
    STMT_reduce (xsr sr,s,xvs vs, xpvs spl, ex' sr e1, ex' sr e2)
  | Lst [Id "ast_axiom"; sr; Str s; vs; ps; axm] ->
    STMT_axiom (xsr sr,s,xvs vs, xps ps,xam axm)
  | Lst [Id "ast_lemma"; sr; Str s; vs; ps; axm] ->
    STMT_lemma(xsr sr,s,xvs vs, xps ps,xam axm)
  | Lst [Id "ast_function"; Str s; vs; ps; ret; props; sts] ->
    STMT_function(sr,s,xvs vs, xps ps,xret ret, xprops props, xsts sts)
  | Lst [Id "ast_curry"; sr; Str s; vs; Lst pss; ret; fk; sts] ->
    STMT_curry(xsr sr,s,xvs vs, map xps pss,xret ret, xfk fk, xsts' sr sts)

  | Lst [Id "ast_macro_val"; ids; v] -> STMT_macro_val (sr, lst "ast_macro_val" xid ids, ex v)
  | Lst [Id "ast_macro_vfor";ids; e; sts] -> STMT_macro_vfor (sr,lst "ast_macro_vfor" xid ids, ex e, xsts sts)
  | Lst [Id "ast_seq"; sr; sts] -> STMT_seq (xsr sr,xsts' sr sts)

  | Lst [Id "ast_union"; sr; Str n; vs; ucmp] ->
    let ucmp = lst "union component" xucmp ucmp in
    STMT_union (xsr sr,n, xvs vs, ucmp)

  | Lst [Id "ast_struct"; sr; Str n; vs; ucmp] ->
    let xscmp = function
      | Lst [Id c; t] -> c, ti t
      | Lst [Str c; t] -> c, ti t
      | x -> err x "struct component"
    in
    let ucmp = lst "struct component" xscmp ucmp in
    STMT_struct (xsr sr,n, xvs vs, ucmp)


  | Lst [Id "ast_cstruct"; sr; Str n; vs; ucmp; reqs] ->
    let xscmp = function
      | Lst [Id c; t] -> c, ti t
      | Lst [Str c; t] -> c, ti t
      | x -> err x "cstruct component"
    in
    let ucmp = lst "cstruct component" xscmp ucmp in
    STMT_cstruct (xsr sr,n, xvs vs, ucmp, xrr reqs)

  | Lst [Id "ast_type_alias"; sr; Str n; vs; t] ->
    STMT_type_alias (xsr sr,n, xvs vs, ti t)

  | Lst [Id "mktypefun"; sr; Str name; vs; argss; ret; body] ->
    let fixarg  arg = match arg with
    | Lst [Str n; t] -> n,ti t
    | Lst [Id n; t] -> n,ti t
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
    Flx_typing.mktypefun (xsr sr) name (xvs vs) argss (ti ret) (ti body)

  | Lst [Id "ast_inherit"; sr; Str n; vs; qn] ->
    STMT_inherit (xsr sr,n, xvs vs, xq "ast_inherit" qn)

  | Lst [Id "ast_inherit_fun"; sr; Str n; vs; qn] ->
    STMT_inherit_fun (xsr sr,n, xvs vs, xq "ast_inherit_fun" qn)

  | Lst [Id "ast_val_decl"; sr; Str n; vs; ot; oe] ->
    STMT_val_decl (xsr sr,n, xvs vs, opt "val_decl" ti ot, 
      opt "val_decl" (ex' sr) oe)

  | Lst [Id "ast_lazy_decl"; Str n; vs; ot; oe] ->
    STMT_lazy_decl (sr,n, xvs vs, opt "lazy_decl" ti ot, opt "lazy_decl" ex oe)

  | Lst [Id "ast_var_decl"; sr; Str n; vs; ot; oe] ->
    STMT_var_decl (xsr sr,n, xvs vs, opt "var_decl" ti ot, 
    opt "var_decl" (ex' sr) oe)

  | Lst [Id "ast_ref_decl"; Str n; vs; ot; oe] ->
    STMT_ref_decl (sr,n, xvs vs, opt "ref_decl" ti ot, opt "ref_decl" ex oe)

  | Lst [Id "ast_untyped_module"; sr; Str n; vs; sts] ->
    STMT_untyped_module (xsr sr, n, xvs vs, xsts' sr sts)

  | Lst [Id "ast_typeclass"; sr; Str n; vs; sts] ->
    STMT_typeclass(xsr sr, n, xvs vs, xsts' sr sts)

  | Lst [Id "ast_instance"; sr; vs; qn; sts] ->
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
    STMT_instance(xsr sr, xvs vs, xq "ast_instance" qn, xsts' sr sts)

  | Lst [Id "ast_label"; sr; Str n] -> STMT_label(xsr sr,n)

  | Lst [Id "ast_goto"; sr; Str n] -> STMT_goto(xsr sr,n)
  | Lst [Id "ast_ifgoto"; sr; e; Str n] -> STMT_ifgoto(xsr sr,ex' sr e,n)
  | Lst [Id "ast_likely_ifgoto"; sr; e; Str n] ->
    STMT_ifgoto(xsr sr,EXPR_likely (xsr sr,ex' sr e),n)

  | Lst [Id "ast_unlikely_ifgoto"; sr; e; Str n] ->
    STMT_ifgoto(xsr sr,EXPR_unlikely (xsr sr,ex' sr e),n)

  | Lst [Id "ast_ifnotgoto"; sr; e; Str n] ->
    STMT_ifgoto(xsr sr,lnot (xsr sr) (ex' sr e),n)

  | Lst [Id "ast_likely_ifnotgoto"; sr; e; Str n] ->
    STMT_ifgoto(xsr sr,EXPR_likely (xsr sr,lnot (xsr sr) (ex' sr e)),n)

  | Lst [Id "ast_unlikely_ifnotgoto"; sr; e; Str n] ->
    STMT_ifgoto(xsr sr,EXPR_unlikely(xsr sr,lnot (xsr sr) (ex' sr e)),n)

  | Lst [Id "ast_ifreturn"; sr; e] -> STMT_ifreturn(xsr sr,ex' sr e)
  | Lst [Id "ast_ifdo"; sr; e; sts1; sts2] -> STMT_ifdo(xsr sr,ex' sr e, xsts' sr sts1, xsts' sr sts2)
  | Lst [Id "ast_call"; sr; f; a] -> STMT_call(xsr sr,ex' sr f,ex' sr a)
  | Lst [Id "ast_assign"; sr; Id v; tlv; a] -> STMT_assign(xsr sr,v,xtlv tlv,ex' sr a)
  | Lst [Id "ast_cassign"; sr; e1; e2] -> STMT_cassign(xsr sr,ex' sr e1, ex' sr e2)
  | Lst [Id "ast_jump"; sr; e1; e2] -> STMT_jump(xsr sr,ex' sr e1, ex' sr e2)
  | Lst [Id "ast_loop"; sr; Str n; e2] -> STMT_loop(xsr sr,n, ex' sr e2)
  | Lst [Id "ast_svc"; sr; Str n] -> STMT_svc(xsr sr,n)
  | Lst [Id "ast_fun_return"; sr; e] -> STMT_fun_return(xsr sr,ex' sr e)

  | Lst [Id "ast_yield"; sr; e] -> STMT_yield(xsr sr,ex' sr e)
  | Lst [Id "ast_proc_return"; sr]  -> STMT_proc_return(xsr sr)
  | Lst [Id "ast_halt"; sr; Str s] -> STMT_halt(xsr sr, s)
  | Lst [Id "ast_trace"; sr; Str n; Str s] -> STMT_trace(xsr sr, n, s)
  | Lst [Id "ast_nop"; sr; Str s] -> STMT_nop(xsr sr,s)
  | Lst [Id "ast_assert"; sr; e] -> STMT_assert(xsr sr,ex' sr e)
  | Lst [Id "ast_init"; sr; Str n; e] -> STMT_init(xsr sr,n,ex' sr e)
  | Lst [Id "ast_newtype"; sr; Str n; vs; t] -> STMT_newtype(xsr sr,n,xvs vs, ti t)
  | Lst [Id "ast_abs_decl"; sr; Str n; vs; tqs; ct; req] ->
    STMT_abs_decl (xsr sr,n,xvs vs, xtqs tqs, xc ct, xrr req)

  | Lst [Id "ast_ctypes"; sr; Lst ids; tqs; req] ->
    let ids = map (function
      | Str n -> Flx_srcref.dummy_sr,n
      | x -> err x "ast_ctypes"
    ) ids
    in
    STMT_ctypes (xsr sr,ids, xtqs tqs, xrr req)

  | Lst [Id "ast_const_decl"; sr; Str n; vs; t; ct; req] ->
    STMT_const_decl (xsr sr, n, xvs vs, ti t, xc ct, xrr req)

  | Lst [Id "ast_fun_decl"; sr; Str n; vs; Lst ps; t; ct; req; Str prec] ->
    STMT_fun_decl (xsr sr, n, xvs vs, map ti ps, ti t, xc ct, xrr req, prec)

  | Lst [Id "ast_callback_decl"; sr; Str n; Lst ps; t; req] ->
    STMT_callback_decl (xsr sr, n, map ti ps, ti t, xrr req)

  | Lst [Id "ast_insert"; sr; Str n; vs; ct; ik; req] ->
    let xik = function
     | Id "header" -> `Header
     | Id "body" -> `Body
     | Id "package" -> `Package
     | x -> err x "ikind_t"
   in
    STMT_insert (xsr sr, n, xvs vs, xc ct, xik ik, xrr req)

  | Lst [Id "ast_code"; sr; ct] -> STMT_code (xsr sr, xc ct)
  | Lst [Id "ast_noreturn_code"; sr; ct] -> STMT_noreturn_code (xsr sr, xc ct)
  | Lst [Id "ast_export_fun"; sr; sn; Str s] ->
    let xsn x = match suffixed_name_of_expr (ex x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_fun  (xsr sr, xsn sn, s)

  | Lst [Id "ast_export_python_fun"; sr; sn; Str s] ->
    let xsn x = match suffixed_name_of_expr (ex x) with
    | Some x -> x
    | None -> err x "suffixed_name_t"
    in
    STMT_export_python_fun  (xsr sr, xsn sn, s)

  | Lst [Id "ast_export_type"; sr; t; Str s] ->
    STMT_export_type (xsr sr, ti t, s)

  
  | Lst [Id "ast_stmt_match";  Lst [sr; e; Lst pss]]->
    let pss = map (function
      | Lst [p;stmts] -> xp p, xsts' sr stmts
      | x -> err x "ast_stmt_match syntax"
      )
     pss
   in
   STMT_stmt_match (xsr sr, (ex' sr e,pss))

  | x -> err x "statement"
