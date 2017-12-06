open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debug = false

exception Tfound of Flx_btype.t

let hack_name qn = match qn with
| `AST_name (sr,name,ts) -> `AST_name (sr,"_inst_"^name,ts)
| `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,"_inst_"^name,ts))
| _ -> failwith "expected qn .."

let grab_name qn = match qn with
| `AST_name (sr,name,ts) -> name
| `AST_lookup (sr,(e,name,ts)) -> name
| _ -> failwith "expected qn .."

let base_type_of_literal sr {Flx_literal.felix_type=t } = TYP_name (sr,t,[])

let type_of_literal inner_bind_type state bsym_table env sr v =
  let _,_,root,_,_ = List.hd (List.rev env) in
  let t = base_type_of_literal sr v in
  let bt = inner_bind_type state bsym_table env sr rsground t in
  bt

let handle_map sr (f,ft) (a,at) =
    let t =
      match ft with
      | BTYP_function (d,c) ->
        begin match at with
        | BTYP_inst (i,[t],k) ->
          if t <> d
          then clierrx "[flx_bind/flx_lookup.ml:3801: E161] " sr
            ("map type of data structure index " ^
            "must agree with function domain")
          else
            btyp_inst (i,[c],k)
        | _ -> clierrx "[flx_bind/flx_lookup.ml:3806: E162] " sr "map requires instance"
        end
      | _ -> clierrx "[flx_bind/flx_lookup.ml:3808: E163] " sr "map non-function"
    in
      (* actually this part is easy, it's just
      applies ((map[i] f) a) where map[i] denotes
      the map function generated for data structure i
      *)
      failwith "MAP NOT IMPLEMENTED"


let rec bind_expression' 
  build_env
  bind_type'
  inner_type_of_index_with_ts
  lookup_label_in_env
  lookup_qn_in_env2'
  lookup_name_with_sig
  inner_lookup_name_in_env
  resolve_overload
  eval_module_expr
  get_pub_tables
  lookup_qn_with_sig'
  inner_bind_type
  koenig_lookup
  cal_apply
  state bsym_table env (rs:recstop) e args 
=
  let bind_expr_orig state bsym_table env rs e args =
    bind_expression'
    build_env
    bind_type'
    inner_type_of_index_with_ts
    lookup_label_in_env
    lookup_qn_in_env2'
    lookup_name_with_sig
    inner_lookup_name_in_env
    resolve_overload
    eval_module_expr
    get_pub_tables
    lookup_qn_with_sig'
    inner_bind_type
    koenig_lookup
    cal_apply
    state bsym_table env (rs:recstop) e args
  in
  let bind_expr env rs e args = bind_expr_orig state bsym_table env rs e args in

  let sr = src_of_expr e in
(*
  print_endline ("[bind_expression'] " ^ string_of_expr e);
  print_endline ("expr_fixlist is " ^
    catmap ","
    (fun (e,d) -> string_of_expr e ^ " [depth " ^si d^"]")
    rs.expr_fixlist
  );
*)
  if List.mem_assq e rs.expr_fixlist
  then raise (Expr_recursion e)
  ;
  let rs = { rs with expr_fixlist=(e,rs.depth)::rs.expr_fixlist } in
  let be e' = bind_expr env { rs with depth=rs.depth+1} e' [] in
  let bea e' = bind_expr env { rs with depth=rs.depth+1} e' args in
  let mkenv i = build_env state bsym_table (Some i) in
  let bt sr t =
    (* we're really wanting to call bind type and propagate depth ? *)
    let t = bind_type' state bsym_table env { rs with depth=rs.depth +1 } sr t [] mkenv in
    let t = beta_reduce "flx_lookup: bind_expression'(1)" state.counter bsym_table sr t in
    t
  in
  let ti sr i ts =
    inner_type_of_index_with_ts
      state
      bsym_table
      { rs with depth = rs.depth + 1 }
      sr
      i
      ts
  in
  (*
  print_endline ("Binding expression " ^ string_of_expr e ^ " depth=" ^ string_of_int rs.depth);
  print_endline ("environment is:");
  print_env env;
  print_endline "==";
  *)
  let rt t = beta_reduce "flx_lookup: bind_expression'(2)" state.counter bsym_table sr t in
  let sr = src_of_expr e in
  match e with
  | EXPR_rptsum_type _
  | EXPR_pclt_type _
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_vsprintf _
  | EXPR_interpolate _
  | EXPR_type_match _
  | EXPR_subtype_match _
  | EXPR_noexpand _
  | EXPR_letin _
  | EXPR_typeof _
  | EXPR_as _
  | EXPR_as_var _
  | EXPR_void _
  | EXPR_arrow _
  | EXPR_effector _
  | EXPR_longarrow _
  | EXPR_ellipsis _
  | EXPR_intersect _
  | EXPR_union _
  | EXPR_isin _
    ->
      clierrx "[flx_bind/flx_lookup.ml:3897: E166] " sr
     ("[bind_expression] Expected expression, got " ^ string_of_expr e)

  (* Use a trick! convert the casematch to a typematch
    which returns a unit sum, then use that to select
    the expression to bind
  *)
  | EXPR_typecase_match (sr,(uba,ms)) -> 
    let argt = bt sr uba in
(*
    let uba = Flx_typecode_of_btype.typecode_of_btype bsym_table state.counter sr argt in
*)
    let tpats,es = List.split ms in
    let n = List.length ms in
    let il = Flx_list.nlist n in
    let ps = List.map2 (fun t u-> t,TYP_unitsum u) tpats il in
    let ubt = TYP_type_match (uba,ps) in
    let selector =
      let btp t params = bind_type' state bsym_table env
        {rs with depth = rs.depth+1}
        sr t params mkenv
      in
      bind_type_match bsym_table state.counter (bt sr) btp [] sr argt ps ubt  
    in

(*
print_endline ("TYPEMATCH = " ^ sbt bsym_table selector);
*)
    let ix = beta_reduce "flx_lookup:typecase" state.counter bsym_table sr selector in 
(*
print_endline ("REDUCED = " ^ sbt bsym_table ix);
*)
    let index = match ix with
    | BTYP_void -> 0
    | BTYP_tuple [] -> 1
    | BTYP_unitsum n -> n
    | _ -> clierrx "[flx_bind/flx_lookup.ml:3933: E167] " sr ("Unable to match typecase argument type " ^ sbt bsym_table argt ^
      " with any case in\n" ^ sbt bsym_table selector)
    in
(*
print_endline ("Case number " ^ si index);
*)
    let x = List.nth es index in
    be x

  | EXPR_cond (sr,(c,t,f)) ->
    bexpr_cond (be c) (be t) (be f)

  | EXPR_uniq (sr,e) -> bexpr_uniq (be e)

  | EXPR_label (sr,label) -> 
    let maybe_index = lookup_label_in_env state bsym_table env sr label in
    begin match maybe_index with
    | Some index -> bexpr_label index
    | None ->
      clierrx "[flx_bind/flx_lookup.ml:3950: E168] " sr ("Flx_lookup: Cannot find label " ^ label ^ " in environment");
    end

  | EXPR_range_check (sr, mi, v, mx) ->
    let (x,t) as v' = be v in
    bexpr_range_check t (be mi, v', be mx)

  | EXPR_callback (sr,qn) ->
    let es,ts = lookup_qn_in_env2' state bsym_table env rs qn in
    begin match es with
    | FunctionEntry [index] ->
       print_endline "Callback closure ..";
       let ts = List.map (bt sr) ts in
if debug then
print_endline ("flx_lookup.EXPR_callback.bexpr_closure");
       bexpr_closure (ti sr (sye index) ts) (sye index, ts)
    | NonFunctionEntry  _
    | _ -> clierrx "[flx_bind/flx_lookup.ml:3965: E169] " sr
      "'callback' expression denotes non-singleton function set"
    end

  | EXPR_expr (sr,s,t,e) ->
    let t = bt sr t in
    let e = be e in
    bexpr_expr (s,t,e)

  | EXPR_andlist (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "land" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3978: E170] " sri "Not expecting empty and list"
    end

  | EXPR_orlist (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "lor" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3985: E171] " sri "Not expecting empty or list"
    end

  | EXPR_sum (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "+" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3992: E172] " sri "Not expecting empty product (unit)"
    end

  | EXPR_product (sri,ls) ->
    begin let mkprod a b = Flx_strr.apl2 sri "*" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mkprod h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3999: E173] " sri "Not expecting empty sum (void)"
    end

  | EXPR_superscript (sri,(a,b)) ->
    be (Flx_strr.apl2 sri "pow" [a; b])

  | EXPR_coercion (sr,(x,t)) ->
    let (e',t') as x' = be x in
    let t'' = bt sr t in
    Flx_coerce.coerce state bsym_table sr x' t''

  | EXPR_tuple_cons (_, eh, et) ->
    let _,eht' as xh' = be eh in
    let _,ett' as xt' = be et in
    bexpr_tuple_cons (xh',xt') 

  | EXPR_tuple_snoc (_, eh, et) ->
    let _,eht' as xh' = be eh in
    let _,ett' as xt' = be et in
    let t = btyp_tuple_snoc eht' ett' in
(*
    print_endline ("Type of tuple cons is " ^ sbt bsym_table t);
*)
    let _,t as x = bexpr_tuple_snoc t (xh',xt') in
(*
print_endline ("Bound tuple cons " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x


  | EXPR_get_tuple_tail (sr,e) ->
(*
print_endline "Binding tuple tail";
*)
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> x'
    | BTYP_tuple ts ->
      let n = List.length ts in
      let ts' = List.tl ts in
      let counter = ref 0 in
      let es' = List.map (fun t-> 
        incr counter; 
(*
        print_endline ("2:get_n arg" ^ sbe bsym_table x');         
*)
        bexpr_get_n t (!counter) x'
      ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x
    | BTYP_array (at,BTYP_unitsum n) ->
      if n>20 then begin
        print_endline ("Array type " ^ sbt bsym_table t' ^ " size " ^ string_of_int n ^ 
          " too long to get tuple tail"); 
        assert false
      end else begin
        let ts' = repeat at (n-1) in
        let counter = ref 0 in
        let es' = List.map (fun t-> 
          incr counter; 
          bexpr_get_n t (!counter) x'
        ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
      x
      end

    | BTYP_tuple_cons (t1,t2) -> 
      let _,t as x = bexpr_tuple_tail t2 x' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple tail of type " ^ sbt bsym_table t'); assert false
    end

  | EXPR_get_tuple_body (sr,e) ->
(*
print_endline "Binding tuple tail";
*)
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> x'
    | BTYP_tuple ts ->
      let n = List.length ts in
      let ts' = List.rev (List.tl (List.rev ts)) in
      let counter = ref 0 in
      let es' = List.map (fun t-> 
        let x = bexpr_get_n t (!counter) x' in
        incr counter; 
        x
      ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | BTYP_array (at,BTYP_unitsum n) ->
      if n>20 then begin
        print_endline ("Array type " ^ sbt bsym_table t' ^ " size " ^ string_of_int n ^ 
          " too long to get tuple body"); 
        assert false
      end else begin
        let ts' = repeat at (n-1) in
        let counter = ref 0 in
        let es' = List.map (fun t-> 
          let x = bexpr_get_n t (!counter) x' in
          incr counter; 
          x
        ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
      x
      end

    | BTYP_tuple_snoc (t1,t2) -> 
      let _,t as x = bexpr_tuple_body t1 x' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple body of type " ^ sbt bsym_table t'); assert false
    end


  | EXPR_get_tuple_head (sr,e) ->
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> assert false
    | BTYP_tuple ts ->
      let ht = List.hd ts in
      let _,t as x  = 
(*
        print_endline ("3:get_n arg" ^ sbe bsym_table x');         
*)
        bexpr_get_n ht 0 x'  
      in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | BTYP_array (at,BTYP_unitsum n) ->
      let _,t as x  = 
        bexpr_get_n at 0 x'  
      in
      x


    | BTYP_tuple_cons (t1,t2) -> 
      let _,t as x = bexpr_tuple_head t1 x' in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple head of type " ^ sbt bsym_table t'); assert false
    end

  | EXPR_get_tuple_last (sr,e) ->
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> assert false
    | BTYP_tuple ts ->
      let ht = List.hd (List.rev ts) in
      let _,t as x  = 
(*
        print_endline ("3:get_n arg" ^ sbe bsym_table x');         
*)
        bexpr_get_n ht (List.length ts - 1) x'  
      in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | BTYP_array (at,BTYP_unitsum n) ->
      let _,t as x  = 
        bexpr_get_n at (n-1) x'  
      in
      x


    | BTYP_tuple_snoc (t1,t2) -> 
      let _,t as x = bexpr_tuple_last t1 x' in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple last of type " ^ sbt bsym_table t'); assert false
    end


  | EXPR_get_n (sr,(n,e')) ->
    let expr,typ = be e' in
    let ctyp,k = match unfold "flx_lookup" typ with
    | BTYP_array (t,BTYP_unitsum len)  ->
      let n = if n = -1 then n + len else n in
      if n<0 || n>len-1
      then clierrx "[flx_bind/flx_lookup.ml:4114: E174] " sr
        (
          "[bind_expression] Tuple index " ^
          string_of_int n ^
          " out of range 0.." ^
          string_of_int (len-1)
        )
      else t,len

    | BTYP_tuple ts
      ->
      let len = List.length ts in
      let n = if n = -1 then n + len else n in
      if n<0 || n>len-1
      then clierrx "[flx_bind/flx_lookup.ml:4127: E175] " sr
        (
          "[bind_expression] Tuple index " ^
          string_of_int n ^
          " out of range 0.." ^
          string_of_int (len-1)
        )
      else List.nth ts n,len

    | BTYP_tuple_cons (t1,_) ->
      if n = 0 then t1,2 (* HACK! We dunno the length of the tuple! *)
      else
      clierrx "[flx_bind/flx_lookup.ml:4139: E176] " sr
      (
        "[bind_expression] Expected tuple " ^
        string_of_expr e' ^
        " to have tuple type, got tuple cons " ^
        sbt bsym_table typ ^ " with non-zero projection " ^ si n
      )

    | BTYP_tuple_snoc (_,t1) ->
      if n = -1 then t1,2 (* HACK! We dunno the length of the tuple! *)
      else
      clierrx "[flx_bind/flx_lookup.ml:4139: E176] " sr
      (
        "[bind_expression] Expected tuple " ^
        string_of_expr e' ^
        " to have tuple type, got tuple snoc " ^
        sbt bsym_table typ ^ " with non-minus-one projection " ^ si n
      )


    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4148: E177] " sr
      (
        "[bind_expression] Expected tuple " ^
        string_of_expr e' ^
        " to have tuple type, got " ^
        sbt bsym_table typ
      )
    in
      let a = expr,typ in
(*
      print_endline ("4:get_n arg" ^ sbe bsym_table a);         
*)
      bexpr_get_n ctyp n a

  | EXPR_get_named_variable (sr,(name,e')) ->
(*
print_endline ("Find field name " ^ name ^ " of " ^ string_of_expr e');
*)
    let e'',t'' as x2 = be e' in
    begin match t'' with
    | BTYP_polyrecord (es,_) ->
      begin try 
        let ct = List.assoc name es in
        let prj = bexpr_rprj name t'' ct in
        bexpr_apply ct (prj,x2)
      with 
        Not_found ->
          clierrx "[flx_bind/flx_lookup.ml:4175: E178] " sr ("Field " ^ name ^ " is not a field of " ^ sbt bsym_table t'')
      end

    | BTYP_record (es)
      ->
      let k = List.length es in
      let field_name = name in
      begin match list_index (List.map fst es) field_name with
      | Some n -> 
        let t = List.assoc field_name es in
(*
        print_endline ("5:get_n arg" ^ sbe bsym_table x2);         
*)
        bexpr_get_n t n x2
      | None -> clierrx "[flx_bind/flx_lookup.ml:4189: E179] " sr
         (
           "Field " ^ field_name ^
           " is not a member of record " ^
           sbt bsym_table t''
          )
      end

    | t -> 
     clierrx "[flx_bind/flx_lookup.ml:4198: E180] " sr ("[bind_expression] Projection requires record or polyrecord instance, got type:\n" ^ sbt bsym_table t)
    end
  | EXPR_rptsum_arg (sr,e) ->
    let (_,t) as e'  = be e in
    begin match t with
    | BTYP_rptsum (n,argt) -> bexpr_rptsum_arg e'
    | _ -> clierr sr ("Flx_bind_expression: casearg can only be applied to value of repeated sum type (coarray)" ^
      "\ngot: " ^ Flx_btype.st t )
    end

  | EXPR_case_index (sr,e) ->
    let (e',t) as e  = be e in
    begin match t with
    | BTYP_unitsum _ -> ()
    | BTYP_rptsum _ -> ()
    | BTYP_sum _ -> ()
    | BTYP_variant _ -> ()
    | BTYP_type_var _ -> ()
    | BTYP_inst (i,_,_) ->
      begin match hfind "lookup" state.sym_table i with
      | { Flx_sym.symdef=SYMDEF_union _} -> ()
      | { Flx_sym.id=id} -> clierrx "[Flx_bind_expression:593: E181] " sr ("Argument of caseno must be sum or union type, got abstract type " ^ id)
      end
    | _ -> clierrx "[Flx_bind_expression:595: E182] " sr ("Argument of caseno must be sum or union type, got " ^ sbt bsym_table t)
    end
    ;
    let int_t = bt sr (TYP_name (sr,"int",[])) in
    begin match e' with
    | BEXPR_case (i,_) ->
      bexpr_literal int_t {Flx_literal.felix_type="int"; internal_value=string_of_int i; c_value=string_of_int i}
    | _ -> bexpr_case_index int_t e
    end

  | EXPR_case_tag (sr,v) ->
     clierrx "[flx_bind/flx_lookup.ml:4223: E183] " sr "plain case tag not allowed in expression (only in pattern)"

  | EXPR_variant (sr,(s,e)) ->
    let (_,t) as e = be e in
(*
print_endline ("binding variant " ^ s ^ " argument " ^ sbe bsym_table e ^ ", argt=" ^ 
sbt bsym_table t);
*)
    bexpr_variant (btyp_variant [s,t]) (s,e)

  | EXPR_ainj (sr, v, sumt) ->
    let bsumt = bt sr sumt in
    let (_,idxt) as e = be v in
    begin match bsumt with
    | BTYP_rptsum (sidxt, baset) ->
      if type_eq bsym_table state.counter sidxt idxt  then
        bexpr_ainj e baset bsumt
      else
        clierr sr ("Flx_bind_expression: Coarray injection aint requires " ^
         "index expression type " ^ Flx_btype.st idxt ^ " to equal " ^
         "repeated sum repetition type " ^ Flx_btype.st sidxt)
    | _ ->
      clierr sr ("Flx_bind_expression: Coarrray injection ainj requires " ^
        "repeated sum type,\ngot: " ^ Flx_btype.st bsumt) 
    end

  | EXPR_projection (sr,v,t) -> 
    let t = bt sr t in
    begin match t with
    | BTYP_tuple ts ->
      let n = List.length ts in
      if v < 0 || v >= n then
        clierrx "[flx_bind/flx_lookup.ml:4235: E184] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
          " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
      else
        let c = List.nth ts v in
        bexpr_prj v t c
 
    | BTYP_array (BTYP_unitsum n,base) ->
      if v < 0 || v >= n then
        clierrx "[flx_bind/flx_lookup.ml:4243: E185] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
          " negative or >= " ^ si n ^ "for array type " ^ sbt bsym_table t)
      else
        bexpr_prj v t base

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4249: E186] " sr ("[Flx_lookup.bind_expression] projection requires tuple or array type, got " ^ sbt bsym_table t);
    end

  | EXPR_typed_case (sr,v,t) ->
(*
print_endline ("Evaluating EXPPR_typed_case index=" ^ si v ^ " type=" ^ string_of_typecode t);
*)
    let t = bt sr t in
    ignore (try unfold "flx_lookup" t with _ -> failwith "AST_typed_case unfold screwd");
    begin match unfold "flx_lookup" t with
    | BTYP_unitsum k ->
      if v<0 || v>= k
      then clierrx "[flx_bind/flx_lookup.ml:4261: E187] " sr "Case index out of range of sum"
      else begin
(*
        print_endline "unitsum case";
*)
        bexpr_unitsum_case v k  (* const ctor *)
      end
    | BTYP_sum ls ->
      if v<0 || v>= List.length ls
      then clierrx "[flx_bind/flx_lookup.ml:4270: E188] " sr "Case index out of range of sum"
      else let vt = List.nth ls v in
      begin match vt with
      | BTYP_tuple [] -> 
        let x = bexpr_const_case (v,t) in
(*
        print_endline ( "const case " ^ sbe bsym_table x); 
*)
        x
      | _ -> 
        let x = bexpr_nonconst_case vt (v,t) in
(*
        print_endline ("Nonconst case " ^ sbe bsym_table x);
*)
        x
      end
    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4287: E189] " sr
      (
        "[bind_expression] Type of case must be sum, got " ^
        sbt bsym_table t
      )
    end

  | EXPR_name (sr,"_decoder",ts) ->
    Flx_decoder.gen_decoder state bsym_table bt lookup_name_with_sig env rs sr ts

  | EXPR_name (sr,"_encoder",ts) ->
    Flx_encoder.gen_encoder state bsym_table bt lookup_name_with_sig env rs sr ts

  | EXPR_name (sr,name,ts) ->

(*
if name = "hhhhh" then 
print_endline ("In bind_expression: Lookup name hhhhh");
*)
    if name = "_felix_type_name" then
       let sname = catmap "," string_of_typecode ts in
       let x = EXPR_literal (sr, {Flx_literal.felix_type="string"; internal_value=sname; c_value=Flx_string.c_quote_of_string sname}) in
       be x
    else
    let ts = List.map (bt sr) ts in
    let lookup_result = 
      try inner_lookup_name_in_env state bsym_table env rs sr name 
      with exn -> 
(*
        print_endline ("[bind_expression'] lookup of name "^name^" FAILED with "^ Printexc.to_string exn); 
*)
        raise exn
    in
    begin match lookup_result with
    | NonFunctionEntry { base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts } ->
      (*
      let index = sye index in
      let ts = adjust_ts state.sym_table sr index ts in
      *)
      (*
      print_endline ("NAME lookup finds index " ^ string_of_bid index);
      print_endline ("spec_vs=" ^ catmap "," (fun (s,j)->s^"<"^si j^">") spec_vs);
      print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
      print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
      begin match hfind "lookup" state.sym_table index with
        | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
          print_endline (id ^ " is a typevariable, vs=" ^
            catmap "," (fun (s,j,_)->s^"<"^si j^">") (fst vs)
          )
        | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
      end;
      *)
      (* should be a client error not an assertion *)
      if List.length spec_vs <> List.length ts then begin
        (*
        print_endline ("bind_expression'; Expr_name: NonFunctionEntry: BINDING NAME " ^ name);
        begin match hfind "lookup" state.sym_table index with
          | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
            print_endline (id ^ " is a typevariable, vs=" ^
              catmap ","
                (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">")
                (fst vs)
            )
          | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
        end;
        print_endline ("NAME lookup finds index " ^ string_of_bid index);
        print_endline ("spec_vs=" ^
          catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") spec_vs);
        print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        *)
        clierrx "[flx_bind/flx_lookup.ml:4347: E190] " sr "[lookup,AST_name] ts/vs mismatch"
      end;

      let ts = List.map (tsubst sr spec_vs ts) sub_ts in
      let ts = adjust_ts state.sym_table bsym_table sr index ts in
      let t = ti sr index ts in

      (* If we have a bound symbol for this index, return it's type. Otherwise,
       * try to figure out the type of the unbound symbol. *)
      begin match
        try Some (Flx_bsym_table.find bsym_table index) with Not_found -> None
      with
      | Some bsym ->
          (* We got a bound symbol, so this should be easy. We now just have to
           * handle reference types directly, which we'll automatically convert
           * that into dereferencing the name. *)
          begin match Flx_bsym.bbdcl bsym with
          | BBDCL_val (_,_,`Ref) ->
              (* We've got a reference, so make sure the type is a pointer. *)
              let t' = 
                match t with 
                | BTYP_pointer t' -> t' 
                | _ ->
                  failwith ("[lookup, AST_name] expected ref " ^ name ^
                  " to have pointer type")
              in
(*
print_endline ("LOOKUP 4: varname " ^ si index);
*)
              bexpr_deref t' (bexpr_varname t (index, ts))

          | BBDCL_struct _
          | BBDCL_cstruct _
            ->
if debug then
print_endline ("flx_lookup.BBDCL_struct.bexpr_closure");
            bexpr_closure t (index, ts)

(* DEPRECATED *)
          | BBDCL_nonconst_ctor _ ->
if debug then
print_endline ("flx_lookup.BBDCL_nonconst_ctor.bexpr_closure");
            bexpr_closure t (index, ts)


          | BBDCL_external_const _ 
          | BBDCL_val _ 
            -> 
(*
print_endline ("LOOKUP 5: varname " ^ si index);
*)
if debug then
print_endline ("flx_lookup.BBDCL_val.bexpr_varname");
            bexpr_varname t (index, ts)

(* DEPRECATED!! *)
          | BBDCL_const_ctor _  ->
if debug then
print_endline ("flx_lookup.BBDCL_const_ctor.bexpr_varname");
            bexpr_varname t (index, ts)

          | BBDCL_label _ ->
            bexpr_label index

          | BBDCL_fun _ 
            ->
            clierrx "[flx_bind/flx_lookup.ml:4405: E191] " sr ("Flx_lookup: bind_expression: EXPR_name] Nonfunction entry: Expected name "^name^ 
            " of struct, cstruct, constructor, const, or variable, got function!")
 
          | _ ->
            clierrx "[flx_bind/flx_lookup.ml:4409: E192] " sr ("Flx_lookup: bind_expression: EXPR_name] Nonfunction entry: Expected name "^name^ 
            " of struct, cstruct, constructor, const, or variable")
 
          end
      | None ->
          (* We haven't bound this symbol yet. We need to specially handle
           * reference types, as I mentioned above. *)
          begin match hfind "lookup:ref-check" state.sym_table index with
          | { Flx_sym.symdef=SYMDEF_ref _ } ->
              (* We've got a reference, so make sure the type is a pointer. *)
              let t' = 
                match t with 
                | BTYP_pointer t' -> t' 
                | _ ->
                failwith ("[lookup, AST_name] expected ref " ^ name ^
                  " to have pointer type")
              in
(*
print_endline ("LOOKUP 6: varname " ^ si index);
*)
if debug then
print_endline ("flx_lookup.indexed_name(ref).bexpr_varname");
              bexpr_deref t' (bexpr_varname t (index, ts))

          (* these should have function entries *)
          | { Flx_sym.symdef=SYMDEF_fun _ }
          | { Flx_sym.symdef=SYMDEF_function _ } -> assert false

          | { Flx_sym.symdef=SYMDEF_struct _ }
          | { Flx_sym.symdef=SYMDEF_cstruct _ }
            ->
            (*
            print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to closure");
            *)
if debug then
print_endline ("flx_lookup.indexed_name(function).bexpr_closure");
            bexpr_closure t (index,ts)

(* DEPRECATED *)
          | { Flx_sym.symdef=SYMDEF_nonconst_ctor _ } ->
if debug then
print_endline ("flx_lookup.indexed_name(nonfunction).bexpr_closure");
            bexpr_closure t (index,ts)

          | { Flx_sym.symdef=SYMDEF_const  _ }
          | { Flx_sym.symdef=SYMDEF_var _ }
          | { Flx_sym.symdef=SYMDEF_val _ }
          | { Flx_sym.symdef=SYMDEF_once _ }
          | { Flx_sym.symdef=SYMDEF_parameter _ }
            ->
            (*
            print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to variable");
            *)
(*
print_endline ("LOOKUP 7: varname " ^ si index);
*)
            bexpr_varname t (index,ts)

(* DEPRECATED *)
          | { Flx_sym.symdef=SYMDEF_const_ctor  _ } ->
            bexpr_varname t (index,ts)

          | { Flx_sym.symdef=SYMDEF_label _ } ->
            bexpr_label index


          | _ -> 
            clierrx "[flx_bind/flx_lookup.ml:4469: E193] " sr ("[Flx_lookup.bind_expression: EXPR_name]: Nonfunction entry: Binding " ^ 
              name ^ "<"^si index^">"^ " requires closure or variable")
          end
      end

    | FunctionEntry [{base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts} as f]
    ->
(*
if name = "hhhhh" then
print_endline ("Found solo function entry for index " ^ si index);
*)
      (* should be a client error not an assertion *)
      if List.length spec_vs <> List.length ts then begin
        (*
        print_endline ("bind_expression'; Expr_name: FunctionEntry: BINDING NAME " ^ name);
        begin match hfind "lookup" state.sym_table index with
          | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
            print_endline (id ^ " is a typevariable, vs=" ^
              catmap "," (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">") (fst vs)
            )
          | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
        end;
        print_endline ("NAME lookup finds index " ^ string_of_bid index);
        print_endline ("spec_vs=" ^
          catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") spec_vs);
        print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        *)
        clierrx "[flx_bind/flx_lookup.ml:4493: E194] " sr ( "[lookup,AST_name] ts/vs mismatch binding " ^ string_of_expr e ^ "\nName " ^ name ^
                    " is bound to " ^
                    (full_string_of_entry_kind state.sym_table bsym_table f) )
      end;

if debug then
print_endline ("Flx_looup.nonfunction) ts= " ^ catmap "," (sbt bsym_table) ts);
if debug then
print_endline ("Flx_looup.nonfunction) sub_ts= " ^ catmap "," (sbt bsym_table) sub_ts);
      let ts = List.map (tsubst sr spec_vs ts) sub_ts in
if debug then
print_endline ("Flx_looup.nonfunction) After sub: ts= " ^ catmap "," (sbt bsym_table) ts);
      let ts = adjust_ts state.sym_table bsym_table sr index ts in
if debug then
print_endline ("Flx_looup.nonfunction) After adjust: ts= " ^ catmap "," (sbt bsym_table) ts);
      let t = ti sr index ts in
if debug then
print_endline ("Flx_looup.nonfunction) indexed type t= " ^sbt bsym_table t);
(*
if name = "hhhhh" then
print_endline ("Returning closure type " ^ Flx_btype.st t);
*)
if debug then
print_endline ("flx_lookup.nonfunction).bexpr_closure");
let _ = Flx_bexpr.complete_check "Flx_lookup.nonfunction).bexpr_closiure" t in
      bexpr_closure t (index,ts)


    | FunctionEntry fs ->
      assert (List.length fs > 1);
(*
print_endline ("lookup_name_in_table_dirs_with_sig found functions " ^ name);
*)
      begin match args with
      | [] ->
(*
        print_endline
        (
          "ERROR! [bind_expression] Simple name " ^ name ^
          " binds to function set in\n" ^
          Flx_srcref.short_string_of_src sr ^
          "\nCandidates are\n: " ^ catmap "\n" (full_string_of_entry_kind state.sym_table bsym_table) fs 
        );
*)
        clierrx "[flx_bind/flx_lookup.ml:4520: E195] " sr
        (
          "[bind_expression] Simple name " ^ name ^
          " binds to function set in\n" ^
          Flx_srcref.short_string_of_src sr ^
          "\nCandidates are\n: " ^ catmap "\n" (full_string_of_entry_kind state.sym_table bsym_table) fs 
        )
      | args ->

        let sufs = List.map snd args in
        let ro = resolve_overload state bsym_table env rs sr fs name sufs ts in
        begin match ro with
         | Some (index, dom,ret,mgu,ts) ->
           (*
           print_endline "OK, overload resolved!!";
           *)
if debug then
print_endline ("flx_lookup.function).bexpr_closure");
           bexpr_closure (ti sr index ts) (index,ts)

         | None -> clierrx "[flx_bind/flx_lookup.ml:4538: E196] " sr "Cannot resolve overload .."
        end
      end
    end

  | EXPR_index (_,name,index) as x ->
    (*
    print_endline ("[bind expression] AST_index " ^ string_of_qualified_name x);
    *)
    let ts = adjust_ts state.sym_table bsym_table sr index [] in
    (*
    print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
    *)
    let t = ti sr index ts in
    (*
    print_endline ("Type is " ^ sbt bsym_table t);
    *)
    begin match hfind "lookup" state.sym_table index with
    | { Flx_sym.symdef=SYMDEF_fun _ }
    | { Flx_sym.symdef=SYMDEF_function _ }
    | { Flx_sym.symdef=SYMDEF_struct _ }
    | { Flx_sym.symdef=SYMDEF_cstruct _ } ->

      (*
      print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to closure");
      *)
if debug then
print_endline ("flx_lookup.EXPR_index1.bexpr_closure");
      bexpr_closure t (index,ts)

(* DEPRECATED *)
    | { Flx_sym.symdef=SYMDEF_nonconst_ctor _ }
      ->
if debug then
print_endline ("flx_lookup.EXPR_index2.bexpr_closure");
      bexpr_closure t (index,ts)

    | { Flx_sym.symdef=SYMDEF_const  _ }
    | { Flx_sym.symdef=SYMDEF_var _ }
    | { Flx_sym.symdef=SYMDEF_val _ }
    | { Flx_sym.symdef=SYMDEF_once _ }
    | { Flx_sym.symdef=SYMDEF_parameter _ }
      ->
      (*
      print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to variable");
      *)
(*
print_endline ("LOOKUP 8: varname " ^ si index);
*)
if debug then
print_endline ("flx_lookup.EXPR_index3.bexpr_varname");
      bexpr_varname t (index,ts)

(* DEPRECATED *)
    | { Flx_sym.symdef=SYMDEF_const_ctor  _ } ->
      bexpr_varname t (index,ts)


    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4590: E197] " sr ("[Flx_lookup.bind_expression: EXPR_index]: Indexed name: Binding " ^ 
        name ^ "<"^si index^">"^ " requires closure or variable")
      (* 
      bexpr_varname t (index,ts)
      *)
    end

  | (EXPR_lookup (sr,(e,name,ts))) as qn ->
    (*
    print_endline ("Handling qn " ^ string_of_qualified_name qn);
    *)
    let ts = List.map (bt sr) ts in
    let entry =
      match
        eval_module_expr
        state
        bsym_table
        env
        e
      with
      | (Flx_bind_deferred.Simple_module (impl, ts, htab,dirs)) ->
        let env' = Flx_name_lookup.mk_bare_env state.sym_table impl in
        let tables = get_pub_tables state bsym_table env' rs dirs in
        let result = Flx_name_lookup.lookup_name_in_table_dirs htab tables sr name in
        result

    in
      begin match entry with
      | Some entry ->
        begin match entry with
        | NonFunctionEntry (i) ->
          let i = sye i in
          begin match hfind "lookup" state.sym_table i with
          | { Flx_sym.sr=srn; symdef=SYMDEF_inherit qn} 
            -> 
            be (expr_of_qualified_name qn)

          (* these should have function entries *)
          | { Flx_sym.symdef=SYMDEF_fun _ }
          | { Flx_sym.symdef=SYMDEF_function _ } -> assert false


          | { Flx_sym.sr=srn; symdef=SYMDEF_struct _ } 
          | { Flx_sym.sr=srn; symdef=SYMDEF_cstruct _ } 
            ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
            bexpr_closure (ti sr i ts) (i,ts)

(* DEPRECATED *)
          | { Flx_sym.sr=srn; symdef=SYMDEF_nonconst_ctor  _ } ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
            bexpr_closure (ti sr i ts) (i,ts)

          | { Flx_sym.sr=srn; symdef=SYMDEF_var _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_val _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_once _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_parameter _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_const _} 
            ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
(*
print_endline ("LOOKUP 9: varname " ^ si i);
*)
            bexpr_varname (ti sr i ts) (i,ts)


(* DEPRECATED *)
          | { Flx_sym.sr=srn; symdef=SYMDEF_const_ctor _}  ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
(*
print_endline ("LOOKUP 9A: varname " ^ si i);
*)
            bexpr_varname (ti sr i ts) (i,ts)


          | _ ->
            clierrx "[flx_bind/flx_lookup.ml:4665: E198] " sr ("[Flx_lookup.bind_expression: EXPR_lookup] Non function entry "^name^
            " must be const, struct, cstruct, constructor or variable  ")

          end

        | FunctionEntry [f] when args = []  ->
            let sufs = List.map snd args in
            let ro = resolve_overload state bsym_table env rs sr [f] name sufs ts in
            begin match ro with
             | Some (index, dom,ret,mgu,ts) ->
               (*
               print_endline "OK, overload resolved!!";
               *)
               bexpr_closure (ti sr index ts) (index,ts)

            | None ->
              clierrx "[flx_bind/flx_lookup.ml:4681: E199] " sr "Overload resolution failed .. "
            end

        | FunctionEntry fs ->
          begin match args with
          | [] ->
            clierrx "[flx_bind/flx_lookup.ml:4687: E200] " sr
            (
              "[bind_expression] Qualified name " ^
              string_of_expr qn ^
              " binds to function set in" ^
              Flx_srcref.short_string_of_src sr ^
              ", Candidates are: " ^ catmap "," string_of_entry_kind fs
            )

          | args ->
            let sufs = List.map snd args in
            let ro = resolve_overload state bsym_table env rs sr fs name sufs ts in
            begin match ro with
             | Some (index, dom,ret,mgu,ts) ->
               (*
               print_endline "OK, overload resolved!!";
               *)
               bexpr_closure (ti sr index ts) (index,ts)

            | None ->
              clierrx "[flx_bind/flx_lookup.ml:4707: E201] " sr "Overload resolution failed .. "
            end
          end
        end

      | None ->
        clierrx "[flx_bind/flx_lookup.ml:4713: E202] " sr
        (
          "Can't find " ^ name
        )
      end

  | EXPR_suffix (sr,(f,suf)) ->
    let sign = bt sr suf in
    let srn = src_of_qualified_name f in
    lookup_qn_with_sig' state bsym_table sr srn env rs f [sign]

  | EXPR_likely (srr,e) -> bexpr_likely (be e)
  | EXPR_unlikely (srr,e) -> bexpr_unlikely (be e)
  | EXPR_not (sr,e) -> 
    let x = Flx_strr.apl2 sr "lnot" [e]  in
    be x

  | EXPR_ref (_,(EXPR_deref (_,e))) -> be e

  | EXPR_ref (srr,e) ->
      (* Helper function to look up a property in a symbol. *)
      let has_property bid property =
        (* If the bound symbol has the bid, check if that symbol has the
         * property. *)
        match
          try Some (Flx_bsym_table.find bsym_table bid) with Not_found -> None
        with
        | Some bsym ->
            begin match Flx_bsym.bbdcl bsym with
            | BBDCL_fun (properties,_,_,_,_,_) 
            | BBDCL_external_fun (properties,_,_,_,_,_,_) ->
                List.mem property properties
            | _ -> false
            end
        | None ->
            begin match (get_data state.sym_table bid).Flx_sym.symdef with
            | SYMDEF_function (_,_,_,properties,_) -> List.mem property properties
            | SYMDEF_fun (properties,_,_,_,_,_) -> List.mem property properties
            | _ -> false
            end
      in
      let e = be e in
      begin match e with
      | _,t when Flx_btype.istriv t -> bexpr_address e
      | BEXPR_deref e,_ -> e

      | BEXPR_varname (index,ts),_ ->
          (* Look up the type of the name, and make sure it's addressable. *)
          begin match
            try Some (Flx_bsym_table.find bsym_table index)
            with Not_found -> None
          with
          | Some bsym ->
              (* We found a bound symbol, check if it's an addressable symbol.
               * Otherwise, error out. *)
              begin match Flx_bsym.bbdcl bsym with
              | BBDCL_val (_,_,(`Var | `Ref)) ->
                  let vtype = inner_type_of_index_with_ts
                    state
                    bsym_table
                    { rs with depth = rs.depth + 1 }
                    (Flx_bsym.sr bsym)
                    index
                    ts
                  in
                  bexpr_ref (btyp_pointer vtype) (index, ts)

              | BBDCL_val (_,_,(`Val | `Tmp)) ->
                  clierr2 srr (Flx_bsym.sr bsym) ("[bind_expression] " ^
                    "Can't address a value " ^ Flx_bsym.id bsym)

              | _ ->
                  clierr2 srr (Flx_bsym.sr bsym) ("[bind_expression] " ^
                  "[1]Address non variable " ^ Flx_bsym.id bsym)
              end

          | None ->
              (* Otherwise, look up the name in the sym_table. *)
              let sym = get_data state.sym_table index in
              begin match sym.Flx_sym.symdef with
              | SYMDEF_inherit _ ->
                  clierrx "[flx_bind/flx_lookup.ml:4793: E203] " srr "Woops, bindexpr yielded inherit"
              | SYMDEF_inherit_fun _ ->
                  clierrx "[flx_bind/flx_lookup.ml:4795: E204] " srr "Woops, bindexpr yielded inherit fun"
              | SYMDEF_ref _
              | SYMDEF_var _
              | SYMDEF_parameter (`PVar,_) ->
                  let vtype = inner_type_of_index_with_ts
                    state
                    bsym_table
                    { rs with depth = rs.depth + 1 }
                    sym.Flx_sym.sr
                    index
                    ts
                  in
                  bexpr_ref (btyp_pointer vtype) (index, ts)

              | SYMDEF_parameter _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] [2]Address " ^
                    "value parameter " ^ sym.Flx_sym.id)
              | SYMDEF_const _
              | SYMDEF_once _
              | SYMDEF_val _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] " ^
                    "Can't address a value or const " ^ sym.Flx_sym.id)
              | _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] [3]Address non " ^
                    "variable " ^ sym.Flx_sym.id)
              end
          end

      | BEXPR_apply ((BEXPR_closure (i,ts),_),a),_  ->
          let bsym = try Some (Flx_bsym_table.find bsym_table i) with Not_found -> None in
          let bsym = match bsym with | Some bsym -> bsym 
            | None -> failwith ("bind_expression': BEXPR_apply: Cannot find symbol index " ^ si i);
          in
          let name = Flx_bsym.id bsym in
          let sr2 = Flx_bsym.sr bsym in
          clierrx "[flx_bind/flx_lookup.ml:4832: E205] " srr ("[bind_expression] [4]Address application of function " ^
            name ^ " in " ^ sbe bsym_table e ^ 
            "\ndefined here:\n" ^
            Flx_srcref.long_string_of_src sr2
          )

      | _ ->
          clierrx "[flx_bind/flx_lookup.ml:4839: E206] " srr ("[bind_expression] [5]Address non variable " ^
            sbe bsym_table e)
      end

  | EXPR_rref (_,e) -> 
(*
    print_endline ("Binding rref");
*)
    let x = be e in
    begin match x with
    | BEXPR_varname (index,ts),vt ->
      let pt = btyp_rref vt in
      bexpr_rref pt (index, ts)  
    | _ -> clierr sr ("Read pointer requires argument be variable")
    end

  | EXPR_wref (_,e) -> 
(*
    print_endline ("Binding rref");
*)
    let x = be e in
    begin match x with
    | BEXPR_varname (index,ts),vt ->
      let pt = btyp_wref vt in
      bexpr_wref pt (index, ts)  
    | _ -> clierr sr ("Write pointer reference requires argument be variable")
    end

  | EXPR_deref (_,(EXPR_ref (sr,e) as x)) ->
    begin 
      try ignore (be x) 
      with err -> 
      print_endline ("WARNING: binding address of expression " ^ string_of_expr x ^ 
      " gave error: \n" ^ Printexc.to_string err  ^ "\n" ^ Flx_srcref.long_string_of_src sr )
    end;
    be e

  | EXPR_deref (sr,e') ->
(*
print_endline ("Binding _deref .. " ^ string_of_expr e);
*)
    let e,t = be e' in
    begin match unfold "flx_lookup" t with
    | BTYP_rref t' 
    | BTYP_pointer t' -> bexpr_deref t' (e,t)
    | _ -> clierrx "[flx_bind/flx_lookup.ml:4856: E207] " sr 
     ("[bind_expression'] Dereference non pointer, type " ^ sbt bsym_table t)
    end

  (* this is a bit hacky at the moment, try to cheat by using
   * plain old new (T a) where T turns out to be a type
   * down the track we have totally unchecked construction
   * of a T by calling C++ new T (a), no idea if T is a suitable
   * type nor if a is a suitable argument (or argument list) for
   * the constructor. Need to fix this, but first see if we can
   * generate the right code if the detector here finds some cases
   * Ideally, the idea is that this is an optimisation, i.e.
   * T (a) was already valid (via _ctor_T probably) and then
   * new T(a) is like the ordinary copy of a value, except theres
   * no copy, constuction is in place on the heap.
   *
   * How to resolve the ambiguity? One way would be to require "cls"
   * to be a cstruct .. however google re2::RE2_ is a primitive, not
   * cstruct, and RE2 is currently also a primitive with a primitive ctor.
   *)
(* Temporarily disable this so we can distinguish existing constuctors
   and calls to C++ class constructors

  | EXPR_new (srr,(EXPR_apply(sre,(cls,a)) as e)) ->
    begin try
      let cls = bt sre (typecode_of_expr cls) in
print_endline ("CLASS NEW " ^sbt bsym_table cls);
      bexpr_class_new cls (be a)
    with _ ->
    bexpr_new (be e)
    end
*)

  | EXPR_new (srr,e) ->
    bexpr_new (be e)

  | EXPR_literal (sr,v) ->
    let t = type_of_literal inner_bind_type state bsym_table env sr v in
    bexpr_literal t v

  | EXPR_map (sr,f,a) ->
    handle_map sr (be f) (be a)

  (* generic str routine *)
  | EXPR_apply (sr,(EXPR_name (_,"_strr",[]), a)) -> 
    let be rs e = bind_expr env rs e [] in
    Flx_strr.strr bsym_table state.sym_table state.counter be rs sr a


  (* generic equality routine *)
  | EXPR_apply (sr,(EXPR_name (_,"_eq",[]), pair)) -> 
    let be rs e = bind_expr env rs e [] in
    Flx_eq.bind_eq bsym_table state inner_lookup_name_in_env be rs sr env pair

  | EXPR_apply 
    (
      sr,
      (
        EXPR_apply (_,(EXPR_name (_,"_map",[]), EXPR_name (_,func,[]))),
        b
      )
    ) ->
    let be rs e = bind_expr env rs e [] in
    Flx_gmap.generic_map bsym_table state.counter be rs sr env func b

  | EXPR_apply 
    (
      sr,
      (
        EXPR_apply (_,(EXPR_name (_,"_rev_map",[]), EXPR_name (_,func,[]))),
        b
      )
    ) ->
    let be rs e = bind_expr env rs e [] in
    Flx_gmap.generic_rev_map bsym_table state.counter be rs sr env func b

  | EXPR_apply 
    (
      sr,
      (
        EXPR_name (_,"_rev",[]), 
        b
      )
    ) ->
    let be rs e = bind_expr env rs e [] in
    Flx_gmap.generic_rev bsym_table state.counter be rs sr env b


  | EXPR_apply (sr,(f',a')) -> 
(*
print_endline ("Bind_expression general apply " ^ string_of_expr e);
*)
    Flx_bind_apply.cal_bind_apply 
      bsym_table state be bt env build_env
      koenig_lookup cal_apply bind_type' 
      lookup_qn_with_sig' mkenv
      inner_lookup_name_in_env eval_module_expr
      get_pub_tables 
      bind_expr_orig
      rs sr f' a' args

  | EXPR_arrayof (sr,es) ->
    let bets = List.map be es in
    let _, bts = List.split bets in
    let n = List.length bets in
    if n > 1 then begin
      let t = List.hd bts in
      List.iter
      (fun t' -> if t <> t' then
         clierrx "[flx_bind/flx_lookup.ml:4941: E208] " sr
         (
           "Elements of this array must all be of type:\n" ^
           sbt bsym_table t ^ "\ngot:\n"^ sbt bsym_table t'
         )
      )
      (List.tl bts)
      ;
      let t = btyp_array (t, btyp_unitsum n) in
      bexpr_tuple t bets
    end else if n = 1 then List.hd bets
    else syserr sr "Empty array?"

  (* the code for this is pretty messy and inefficient but it should work *)
  (* actually no, it only works at binding time! we need tuple_cons, which
     should work at instantiation time!
  *)
  | EXPR_extension (sr, es, e') ->  
    let e'',t' = be e' in
    let es' = List.map be es in
    let ts = List.map snd es' in
    begin match t' with
    | BTYP_record (fields) ->
      let new_fields = ref [] in
      List.iter (fun e ->
        let _,t = be e in
        match t with
        | BTYP_record (fields) -> 
          let fields = List.map (fun (s,t)-> 
            s,EXPR_get_named_variable (sr,(s,e))
          )
          fields
          in
          new_fields := List.rev fields @ (!new_fields)
        | _ -> clierrx "[flx_bind/flx_lookup.ml:4975: E209] " sr ("Record extension requires bases be records too, got value of type " ^ sbt bsym_table t)
      )
      es
      ;
      let fields = List.map (fun (s,_)-> s,EXPR_get_named_variable (sr, (s,e'))) fields in
      new_fields := fields @ !new_fields;
      let unique_fields = ref [] in
      List.iter (fun (s,t) ->
        if not (List.mem_assoc s (!unique_fields)) then
        unique_fields := (s,t) :: (!unique_fields)
      )
      (!new_fields);
      be (EXPR_record (sr,!unique_fields))

    | _ -> 
      let ntimes t n = 
        let rec aux n ts = if n=0 then ts else aux (n-1) (t::ts) in
        aux n []
      in 
      let rec check t n ts = 
        match ts with
        | [] -> Some (t,n)
        | BTYP_array (t',BTYP_unitsum m)::ts when t = t' -> check t (n+m) ts
        | t'::ts when t = t'  -> check t (n+1) ts
        | _ -> None
      in
      let compatible_arrays ts = 
        match ts with 
        | [] -> clierrx "[flx_bind/flx_lookup.ml:5003: E210] " sr "empty extension"
        | BTYP_array (t,BTYP_unitsum n) :: ts -> check t n ts
        | t::ts -> check t 1 ts
      in
      match compatible_arrays (ts @ [t']) with
      | Some  _ ->
        clierrx "[flx_bind/flx_lookup.ml:5009: E211] " sr "Can't extend arrays yet"
      | None ->
        (* if it isn't a record extension, treat it as a tuple extension *)
        let values = ref [] in
        let types = ref [] in
        List.iter (fun  xpr -> 
          match xpr with
          | BEXPR_tuple flds,BTYP_tuple ts -> 
            values := !values @ flds; 
            types := !types @ ts
          | e,BTYP_tuple ts -> 
            let k = List.length ts in
            let n = ref (-1) in
            values := !values @ List.map (fun w -> incr n;
(*
                print_endline ("8:get_n arg" ^ sbe bsym_table xpr);         
*)
                bexpr_get_n w (!n) xpr) ts; 
            types := !types @ ts
          | BEXPR_tuple flds, BTYP_array (t, BTYP_unitsum n) ->
            values := !values @ flds; 
            types := !types @ ntimes t n;
          | e, BTYP_array (t, BTYP_unitsum n) ->
            if n > 20 then clierrx "[flx_bind/flx_lookup.ml:5032: E212] " sr "Array too big (>20) for tuple extension"
            else (
              let k = ref (-1) in
              values := !values @ List.map (fun w -> incr k; 
(*
                print_endline ("9:get_n arg" ^ sbe bsym_table xpr);         
*)
                bexpr_get_n w (!k) xpr) (ntimes t n); 
              types := !types @ ntimes t n;
            )
          | _ -> 
            values := !values @ [xpr];
            types := !types @ [snd xpr]
        )
        (es' @[e'',t'])
        ;
        let tt = btyp_tuple (!types) in
        let ee = bexpr_tuple tt (!values) in
        ee
    end

  | EXPR_record_type _ -> assert false
  | EXPR_polyrecord_type _ -> assert false
  | EXPR_variant_type _ -> assert false

  | EXPR_record (sr,ls) ->
    begin match ls with
    | [] -> bexpr_tuple (btyp_tuple []) []
    | _ ->
    let ss,es = List.split ls in
    let es = List.map be es in
    bexpr_record (List.combine ss es)
    end

  | EXPR_rnprj (sr,name,seq,e) -> 
    let (e',domain) as e = be e in
    begin match domain with
    | BTYP_record flds ->
      let dcnt = ref 0 in
      let idx = ref 0 in
      begin try
        List.iter (fun (s,t) -> 
          if s <> name then incr idx else
          if (!dcnt) = seq then raise Not_found 
          else begin incr idx; incr dcnt end
        ) 
        flds;
        print_endline ("[Flx_lookup:bind_expression] Invalid named projection " ^ name ^ ", seq=" ^ string_of_int seq);
        assert false
      with Not_found ->
        let codomain = snd (List.nth flds (!idx)) in
        bexpr_apply codomain (bexpr_prj (!idx) domain codomain,e)
      end

    | BTYP_polyrecord (flds,x) ->
      failwith "rnprj not implemented for polyrecord type argument yet"

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5089: E213] " sr ("Argument of rnprj must be record or polyrecord type, got " ^ sbt bsym_table domain)
    end
 
  | EXPR_polyrecord (sr,ls,e) ->
    let ss,es = List.split ls in
    let es = List.map be es in
    bexpr_polyrecord (List.combine ss es) (be e)

  | EXPR_remove_fields (sr,e,ss) ->
    bexpr_remove_fields (be e) ss

  | EXPR_replace_fields (sr, e, fs) ->
    let fs = List.map (fun (s,e) -> s,be e) fs in
    let cmp (s1,e1) (s2,e2) = compare s1 s2 in
    let fs = List.stable_sort cmp fs in
    let (_,t) as e = be e in
    let check ls = 
      let rec aux ls fs = match ls,fs with
      | _,[] -> () (* exhausted replacement list *)
      | (s1,t1)::tail1,(s2,e2)::tail2 when s1 = s2 -> 
        if type_eq bsym_table state.counter t1 (snd e2) 
        then aux tail1 tail2
        else 
         clierr sr ("Flx_lookup: Attempt to replace field " ^ s1 ^
         " of type " ^ sbt bsym_table t1 ^
         " with field of wrong type " ^  sbt bsym_table (snd e2)
         )

      | _::tail1,fs -> aux tail1 fs

      | _,(s2,_)::_ -> clierr sr ("Flx_lookup: Attempt to replace field " ^ s2 ^
        " which is not present in record of type " ^
        sbt bsym_table t)
      in aux ls fs
    in
    begin match t with
    | BTYP_record ls -> check ls
    | BTYP_polyrecord (ls,_) -> check ls
    | _ -> clierr sr ("flx_lookup: replace fields in non record type " ^ sbt bsym_table t)
    end;
    bexpr_polyrecord fs (bexpr_remove_fields e (List.map fst fs))

  | EXPR_tuple (_,es) ->
    let bets = List.map be es in
    let _, bts = List.split bets in
    let n = List.length bets in
    if n > 1 then
      try
        let t = List.hd bts in
        List.iter
        (fun t' -> if t <> t' then raise Not_found)
        (List.tl bts)
        ;
        let t = btyp_array (t, btyp_unitsum n) in
        bexpr_tuple t bets
      with Not_found ->
        bexpr_tuple (btyp_tuple bts) bets
    else if n = 1 then
      List.hd bets
    else
    bexpr_tuple (btyp_tuple []) []


  | EXPR_match_case (sr,(v,e)) ->
     bexpr_match_case (v,be e)

  | EXPR_match_variant(sr,(v,e)) ->
     bexpr_match_variant (v,be e)

  (* Note this ONLY checks the tags, not the types! To be correct, 
     the types have to work as well, the extractor must organise that
  *)
  | EXPR_match_variant_subtype (sr, (e,t)) ->
(*
print_endline ("EXPR_match_variant_subtype");
*)
    let t = bt sr t in
    begin match unfold "Flx_lookup.match_variant_subtype(target)" t with
    | BTYP_variant ls ->
      let e = be e in
      let arg_t = snd e in
      begin match unfold "Flx_lookup.match_variant_subtype(src)" arg_t with
      | BTYP_variant ts ->
        let intersection = List.filter (fun (s,t) -> List.mem_assoc s ls) ts in
        let matches = List.map (fun (s,_) -> bexpr_match_variant (s,e) ) intersection in
        let match_expr = List.fold_left (fun acc term -> bexpr_lor acc term) bexpr_false matches in
        match_expr
      | _ -> clierr sr
        ("Variant subtype matching requires polymorphic variant type as argument, got " ^ sbt bsym_table arg_t);
      end
    | _ -> clierr sr 
      ("Variant subtype matching requires polymorphic variant type as pattern, got " ^ sbt bsym_table t);
    end

  | EXPR_variant_subtype_match_coercion (sr,(e,t)) ->
(*
print_endline ("EXPR_variant_subtype_match_coercion");
*)
    (* we need to do TWO coercions here! The first one is the unsafe
       coercion that the pattern match checked was safe. This is a flat
       coercion that removes cases from the argument.
       The second coercion is applies to that and is a standard
       conversion to a supertype, this can add cases, and also 
       covariantly change the argument type
    *)
    let t = bt sr t in
    begin match unfold "Flx_lookup.variant_subtype_match_coercion(target)" t with
    | BTYP_variant ls ->
      let e = be e in
      let arg_t = snd e in
      begin match unfold "Flx_lookup.variant_subtype_match_coercion(src)" arg_t with
      | BTYP_variant ts ->
        let intersection = List.filter (fun (s,t) -> List.mem_assoc s ls) ts in
        let narrowed_src_type = Flx_btype.btyp_variant intersection in
        (* this case is safe because the match checker ensure it *)
        let narrowed_argument = bexpr_reinterpret_cast (e,narrowed_src_type) in

        (* this coercion may not be safe, it should be a coercion to
        a supertype. We don't bother checking that here because the coercion
        expander should do it anyhow
        *)
        let final_value = bexpr_coerce  (narrowed_argument, t) in
        final_value

      | _ -> clierr sr
        ("Variant subtype matching requires polymorphic variant type as argument, got " ^ sbt bsym_table arg_t);
      end
    | _ -> clierr sr 
      ("Variant subtype matching requires polymorphic variant type as pattern, got " ^ sbt bsym_table t);
    end



  | EXPR_match_ctor (sr,(qn,e)) ->
    begin match qn with
    | `AST_name (sr,name,ts) ->
(*
      print_endline ("WARNING(deprecate): match constructor by name! " ^ name);
*)
      let (_,ut) as ue = be e in
      let ut = rt ut in
(*
      print_endline ("Union type is " ^ sbt bsym_table ut);
*)
      begin match ut with
      | BTYP_inst (i,ts',_) ->
(*
        print_endline ("OK got type " ^ si i);
*)
        begin match hfind "lookup" state.sym_table i with
        | { Flx_sym.id=id; symdef=SYMDEF_union ls } ->
(*
          print_endline ("UNION TYPE! " ^ id);
*)
          let vidx =
            let rec scan = function
            | [] -> None
            | (vn,vidx,vs',vat,vct,gadt)::_ when vn = name -> Some vidx
            | _:: t -> scan t
            in scan ls
          in
          begin match vidx with
          | Some vidx ->
(*
            print_endline ("Index is " ^ si vidx);
*)
            bexpr_match_case (vidx,ue)

          | None->
            begin try
              let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
              be (EXPR_apply ( sr, (fname,e)))
            with _ -> 
              clierrx "[flx_bind/flx_lookup.ml:5168: E214] " sr ("[flx_lookup: EXPR_match_ctor]: Can't find union variant " ^ name ^ 
                 " or bind user function _match_ctor_" ^ name ^ " to arg " ^ 
                 string_of_expr e)
            end
          end

        (* this handles the case of a C type we want to model
        as a union by provding _match_ctor_name style function
        as C primitives ..
        *)
        | { Flx_sym.id=id; symdef=SYMDEF_abs _ } ->
          let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* experimental!! Allow for any nominal type other than union *)
        | _ ->
          let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* | _ ->  clierrx "[flx_bind/flx_lookup.ml:5187: E215] " sr ("expected union of abstract type, got" ^ sbt bsym_table ut) *)
        end

      (* experimental!! Allow for any type other than union *)
      | _ -> 
        let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
        be (EXPR_apply ( sr, (fname,e)))

 
      (* | _ -> clierrx "[flx_bind/flx_lookup.ml:5196: E216] " sr ("expected nominal type, got" ^ sbt bsym_table ut) *)
      end

    | `AST_typed_case (sr,v,_)
    | `AST_case_tag (sr,v) ->
       be (EXPR_match_case (sr,(v,e)))

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5203: E217] " sr "Expected variant constructor name in union decoder"
    end

  | EXPR_match_ho_ctor (sr,(qn,es)) ->
(*
print_endline ("match ho ctor : exprs = " ^ catmap "," string_of_expr es);
*)
    begin match qn with
    | `AST_name (sr,name,ts) ->
      let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
      begin match es with
      | [] -> assert false (* shouldn't allow less then 2 arguments! *)
      | ls ->  
        let e = List.fold_left (fun acc e -> EXPR_apply (sr, (acc,e))) fname es in
(*
print_endline ("match ho ctor, binding expr = " ^ string_of_expr e);
*)
        be e
      end
    | _ -> clierrx "[flx_bind/flx_lookup.ml:5203: E217] " sr "Expected variant constructor name in union decoder"
    end

  | EXPR_variant_arg (sr,(v,e)) ->
     let (_,t) as e' = be e in
     ignore (try unfold "flx_lookup" t with _ -> failwith "AST_variant_arg unfold screwd");
     begin match unfold "flx_lookup" t with
     | BTYP_variant ls ->
       begin 
         try 
           List.iter (fun (cname,t) ->
             if cname = v then raise (Tfound t))
             ls
           ; 
           clierrx "[flx_bind/flx_lookup.ml:5217: E218] " sr ("[bind_expression] [Expr_variant_arg] " ^" Variant case " ^ v ^ 
              " not found in variant type: " ^ sbt bsym_table t)
         with Tfound t -> 
           bexpr_variant_arg t (v,e')
       end
     | _ -> clierrx "[flx_bind/flx_lookup.ml:5222: E219] " sr ("Expected variant type, got " ^ sbt bsym_table t)
     end

  | EXPR_case_arg (sr,(v,e)) ->
     let (_,t) as e' = be e in
     ignore (try unfold "flx_lookup" t with _ -> failwith "AST_case_arg unfold screwd");
     begin match unfold "flx_lookup" t with
     | BTYP_unitsum n ->
       if v < 0 || v >= n
       then clierrx "[flx_bind/flx_lookup.ml:5232: E220] " sr "Invalid sum index"
       else
         bexpr_case_arg unit_t (v, e')

     | BTYP_sum ls ->
       let n = List.length ls in
       if v<0 || v>=n
       then clierrx "[flx_bind/flx_lookup.ml:5239: E221] " sr "Invalid sum index"
       else let t = List.nth ls v in
       bexpr_case_arg t (v, e')

     | BTYP_rptsum (BTYP_unitsum n,t) ->
       if v<0 || v>=n
       then clierrx "[flx_bind/flx_lookup.ml:5239: E221] " sr "Invalid sum index"
       else
         bexpr_case_arg t (v, e')


     (* EXPR_case_arg is used for variants with an integer index producted from
        the field name with a hash of the field name and type. We do this so a variant
        run time representation is the same as a union 
     *)
     | BTYP_variant ts ->
(*
       print_endline ("Argument of variant with hash code " ^ string_of_int v);
       print_endline ("Variant type is " ^ Flx_print.sbt bsym_table t);
       print_endline ("cofields are: ");
       List.iter (fun ((s,t) as x) -> 
         print_endline (s ^ " of " ^ sbt bsym_table t ^ " HASH: " ^ string_of_int (vhash x))
       ) 
       ts;
*)
       let vsh = List.map (fun ((s,t) as x) -> vhash x,x) ts in
       let _,vt = 
         try List.assoc v vsh 
         with Not_found ->
           clierr sr ("flx_bind/flx_lookup: case_arg of variant: Hashcode " ^ string_of_int v ^
           " not found in variant type:\n" ^
           List.fold_left (fun acc (h,(s,t)) -> 
              acc ^ s ^ " of " ^ sbt bsym_table t ^ " HASH: " ^ string_of_int h ^ "\n")
           ""
           vsh)
       in
       bexpr_case_arg vt (v,e')
 
     | _ -> clierrx "[flx_bind/flx_lookup.ml:5243: E222] " sr ("Expected sum type, got " ^ sbt bsym_table t)
     end

  | EXPR_ctor_arg (sr,(qn,e)) ->

    let (_,ut) as ue = be e in
    let ut = rt ut in

    begin match qn with
    | `AST_name (sr,name,ts) ->
(*
      print_endline ("ctor_arg: Constructor to extract: " ^ name ^ "[" ^ catmap "," string_of_typecode ts ^ "]"); 
*)
      begin match ut with
      | BTYP_inst (i,ts',_) ->
        begin match hfind "lookup" state.sym_table i with
        | { Flx_sym.id=id; symdef=SYMDEF_union ls } ->
          let _,vs,_  = find_split_vs state.sym_table bsym_table i in
(*
        print_endline ( "OK got union type " ^ id ^ "<"^si i ^ "> vs= " ^ catmap "," (fun (id,j,_)-> id^"<"^si j^">") vs 
             ^ "instance ts = [" ^catmap "," (sbt bsym_table) ts'^ "]"
        );
*)
(*
print_endline ("Constructor to extract " ^ name ^ " should agree with encoded constuctor of union " ^ id); 
*)
(*
print_endline ("Union parent vs = " ^  catmap "," (fun (s,_,_) -> s) parent_vs ^ 
  " local vs = " ^ catmap "," (fun (s,_,_) -> si i) vs'');
*)

          let result =
            let rec scan = function
            | [] -> None
            | (vn,vidx,vs',vt,vct,gadt)::_ when vn = name -> Some (vidx,vs',vt,vct)
            | _:: t -> scan t
            in scan ls
          in
          begin match result with
          | None ->
            begin try
              let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
              be (EXPR_apply ( sr, (fname,e)))
            with _ ->
              clierrx "[flx_bind/flx_lookup.ml:5282: E223] " sr ("[flx_lookup: EXPR_ctor_arg]: Can't find union variant " ^ name ^ 
                 " or bind user function _ctor_arg_" ^ name ^ " to arg " ^ 
                 string_of_expr e)
            end
(* failwith ("EXPR_ctor_arg: Can't find union variant " ^ name); *)

          | Some ( vidx,vs', vt,vct) ->
(*
          print_endline ("Constructor Index is " ^ si vidx ^ " vs'=" ^ catmap "," (fun (name,idx,_) -> name) (fst vs')); 
*)
            let vt,vct =
              let bvs = List.map
                (fun (name,i,kind) -> 
(*
print_endline ("AST_name(BTYP_inst): "^name^"=T<"^string_of_int i^">");
*)
                 name, btyp_type_var (i, bmt "Flx_bind_expression" kind))
                vs
              in
(*
            print_endline ("Binding ctor arg type = " ^ string_of_typecode vt); 
            print_endline ("Binding ctor result type = " ^ string_of_typecode vct); 
*)
              let env' = build_env state bsym_table (Some i) in
              bind_type' state bsym_table env' rsground sr vt bvs mkenv,
              bind_type' state bsym_table env' rsground sr vct bvs mkenv
            in
(*
          print_endline ("-----+++>>");
          print_endline ("Bound polymorphic ctor arg type = " ^ sbt bsym_table vt); 
          print_endline ("Bound polymorphic ctor result type = " ^ sbt bsym_table vct); 
          print_endline ("Bound polymorphic union value type = " ^ sbt bsym_table ut);
          print_endline ("-----+++>>");

*)
(*
print_endline ("Unification of result type with union value type\n");
*)
          let dvars = ref BidSet.empty in
          List.iter (fun (_,i,_) -> dvars := BidSet.add i (!dvars)) vs;
          List.iter (fun (_,i,_) -> dvars := BidSet.add i (!dvars)) (fst vs');
(*
print_endline ("Dependent variables to solve for = ");
          BidSet.iter (fun i-> print_endline ("DVAR= " ^ string_of_int i)) !dvars;
*)
          let maybe_mgu = 
            let eqns = [vct,ut] in
            try Some (unification bsym_table state.counter eqns !dvars)
            with Not_found -> None
          in
          begin match maybe_mgu with
          | None -> raise GadtUnificationFailure
          | Some mgu ->
(*
            print_endline ("Flx_lookup unification manual: MGU=");
            List.iter (fun (j,t) -> print_endline ("  ** tvar " ^ string_of_int j ^ " --> " ^ sbt bsym_table t))
            mgu;
*)
          let varmap = Hashtbl.create 3 in
          List.iter (fun (j,t) -> Hashtbl.add varmap j t) mgu;
          let vt = varmap_subst varmap vt in
          let vs' = List.map (fun (s,i,tp) -> s,i, Flx_btype.bmt "Flx_bind_expression.1" tp) vs in
(*
          print_endline ("vs in union type = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") vs'); 
          print_endline ("ts' to bind to them = " ^ catmap "," (sbt bsym_table) ts'); 
*)
          let ts' = adjust_ts state.sym_table bsym_table sr i ts' in
(*
          print_endline ("ts' to bind to them after adjust = " ^ catmap "," (sbt bsym_table) ts');
*)
            let vt = tsubst sr vs' ts' vt in
(*
            print_endline ("  && BOUND CASE ARG: Instantiated type of constructor argument = " ^ sbt bsym_table vt); 
            print_endline ("  && BOUND CASE ARG: Constructor index " ^ si vidx);
            print_endline ("  && BOUND CASE ARG: Union type to deconstruct: " ^ sbt bsym_table (snd ue));
*)
            let x = bexpr_case_arg vt (vidx,ue) in
(*
            print_endline ("  && BOUND CASE ARG: function type: " ^ sbt bsym_table (snd x));
*)
            x 
            end
          end
        (* this handles the case of a C type we want to model
        as a union by provding _ctor_arg style function
        as C primitives ..
        *)
        | { Flx_sym.id=id; symdef=SYMDEF_abs _ } ->
          let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* experimental allow for any nominal type other than union *)
        | _ ->
          let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* | _ -> failwith "Woooops expected union or abstract type" *)
        end
      (* experimental allow for any type other than union *)
      | _ ->
        let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
        be (EXPR_apply ( sr, (fname,e)))

      (* | _ -> failwith "Woops, expected nominal type" *)
      end


    | `AST_typed_case (sr,v,_)
    | `AST_case_tag (sr,v) ->
      be (EXPR_case_arg (sr,(v,e)))

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5340: E224] " sr "Expected variant constructor name in union dtor"
    end

  | EXPR_ho_ctor_arg (sr,(qn,es)) ->
(*
print_endline ("ho ctor arg: exprs = " ^ catmap "," string_of_expr es);
*)
    begin match qn with
    | `AST_name (sr,name,ts) ->
      let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
      begin match es with
      | [] -> assert false (* shouldn't allow less then 2 arguments! *)
      | ls ->  
        let e = List.fold_left (fun acc e -> EXPR_apply (sr, (acc,e))) fname es in
(*
print_endline ("ho ctor arg: expr = " ^ string_of_expr e);
*)
        be e
      end

    | _ -> clierr sr "Expected variant constructor name in union dtor"
    end

  | EXPR_lambda (sr,_) ->
    syserr sr
    (
      "[bind_expression] " ^
      "Unexpected lambda or object when binding expression (should have been lifted out)" ^
      string_of_expr e
    )

  | EXPR_match (sr,_) ->
    clierrx "[flx_bind/flx_lookup.ml:5352: E225] " sr
    (
      "[bind_expression] " ^
      "Unexpected match when binding expression (should have been lifted out)"
    )



