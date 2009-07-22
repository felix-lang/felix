open Flx_util
open Flx_list
open Flx_types
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_name
open Flx_unify
open Flx_csubst
open Flx_exceptions
open Flx_display
open List
open Flx_ctypes
open Flx_cexpr
open Flx_maps
open Flx_pgen
open Flx_beta

let string_of_string = Flx_string.c_quote_of_string

(* HACKERY: this assumes library dependent things:
  but we can't add literals in the library code :-(
*)
let csuffix_of_type s = match s with
  | "tiny" -> ""
  | "short" -> ""
  | "int" -> ""
  | "long" -> "l"
  | "vlong" -> "ll"
  | "utiny" -> "u"
  | "ushort" -> "u"
  | "uint" -> "u"
  | "ulong" -> "ul"
  | "uvlong" -> "ull"
  | "int8" -> ""
  | "int16" -> ""
  | "int32" -> "l"
  | "int64" -> "ll"
  | "uint8" -> "u"
  | "uint16" -> "u"
  | "uint32" -> "ul"
  | "uint64" -> "ull"
  | "double" -> ""
  | "float" -> "f"
  | "ldouble" -> "l"
  | _ -> failwith ("[csuffix_of_type]: Unexpected Type " ^ s)

let cstring_of_literal e = match e with
  | `AST_int (s,i) -> (Big_int.string_of_big_int i)^csuffix_of_type s
  | `AST_float (s,x) -> x ^ csuffix_of_type s
  | `AST_string s -> string_of_string s
  | `AST_cstring s -> string_of_string s
  | `AST_wstring s -> "L" ^ string_of_string s
  | `AST_ustring s -> "L" ^ string_of_string s

(* a native literal is one not needing a cast to get the type right *)
let is_native_literal e = match e with
  | `AST_int ("int",_)
  | `AST_int ("long",_)
  | `AST_int ("uint",_)
  | `AST_int ("ulong",_)
  | `AST_int ("vlong",_)
  | `AST_int ("uvlong",_)
  | `AST_float ("double",_) -> true
  | _ -> false

let get_var_frame syms bbdfns this index ts : string =
  match
    try Hashtbl.find bbdfns index
    with Not_found -> failwith ("[get_var_frame(1)] Can't find index " ^ si index)
  with (id,parent,sr,entry) ->
  match entry with
  | `BBDCL_val (vs,t)
  | `BBDCL_var (vs,t)
  | `BBDCL_ref (vs,t) ->
    begin match parent with
    | None -> "ptf"
    | Some i ->
      if i <> this
      then "ptr" ^ cpp_instance_name syms bbdfns i ts
      else "this"
    end
  | `BBDCL_tmp (vs,t) ->
     failwith ("[get_var_frame] temporaries aren't framed: " ^ id)

  | _ -> failwith ("[get_var_frame] Expected name "^id^" to be variable or value")

let get_var_ref syms bbdfns this index ts : string =
  match
    try Hashtbl.find bbdfns index
    with Not_found -> failwith ("[get_var_ref] Can't find index " ^ si index)
  with (id,parent,sr,entry) ->
  (*
  print_endline ("get var ref for " ^ id ^ "<" ^ si index ^ ">["^catmap "," (string_of_btypecode syms.dfns) ts^"]");
  *)
  match entry with
  | `BBDCL_val (vs,t)
  | `BBDCL_var (vs,t)
  | `BBDCL_ref (vs,t) ->
    begin match parent with
    | None -> (* print_endline "No parent ...?"; *)
      "PTF " ^ cpp_instance_name syms bbdfns index ts
    | Some i ->
      (*
      print_endline ("Parent " ^ si i);
      *)
      (
        if i <> this
        then "ptr" ^ cpp_instance_name syms bbdfns i ts ^ "->"
        else ""
      ) ^
      cpp_instance_name syms bbdfns index ts
    end

  | `BBDCL_tmp (vs,t) ->
      cpp_instance_name syms bbdfns index ts

  | _ -> failwith ("[get_var_ref(3)] Expected name "^id^" to be variable, value or temporary")

let get_ref_ref syms bbdfns this index ts : string =
  match
    try Hashtbl.find bbdfns index
    with Not_found -> failwith ("[get_var_ref] Can't find index " ^ si index)
  with (id,parent,sr,entry) ->
  (*
  print_endline ("get var ref for " ^ id ^ "<" ^ si index ^ ">["^catmap "," (string_of_btypecode syms.dfns) ts^"]");
  *)
  match entry with
  | `BBDCL_val (vs,t)
  | `BBDCL_var (vs,t)
  | `BBDCL_ref (vs,t) ->
    begin match parent with
    | None -> (* print_endline "No parent ...?"; *)
      "PTF " ^ cpp_instance_name syms bbdfns index ts
    | Some i ->
      (*
      print_endline ("Parent " ^ si i);
      *)
      (
        if i <> this
        then "ptr" ^ cpp_instance_name syms bbdfns i ts ^ "->"
        else ""
      ) ^
      cpp_instance_name syms bbdfns index ts
    end

  | `BBDCL_tmp (vs,t) ->
      cpp_instance_name syms bbdfns index ts

  | _ -> failwith ("[get_var_ref(3)] Expected name "^id^" to be variable, value or temporary")

let nth_type ts i =
  try match ts with
  | `BTYP_tuple ts -> nth ts i
  | `BTYP_array (t,`BTYP_unitsum n) -> assert (i<n); t
  | _ -> assert false
  with Not_found ->
    failwith ("Can't find component " ^ si i ^ " of type!")

let rec gen_expr' syms bbdfns this (e,t) vs ts sr : cexpr_t =
  (*
  print_endline ("Generating expression " ^ string_of_bound_expression_with_type syms.dfns bbdfns (e,t));
  print_endline ("Location " ^ Flx_srcref.short_string_of_src sr);
  *)
  let ge' e = gen_expr' syms bbdfns this e vs ts sr in
  let ge e = gen_expr syms bbdfns this e vs ts sr in
  let ge'' sr e = gen_expr' syms bbdfns this e vs ts sr in
  if length ts <> length vs then
  failwith
  (
    "[gen_expr} wrong number of args, expected vs = " ^
    si (length vs) ^
    ", got ts=" ^
    si (length ts)
  );
  let tsub t = reduce_type (beta_reduce syms sr  (tsubst vs ts t)) in
  let tn t = cpp_typename syms (tsub t) in

  (* NOTE this function does not do a reduce_type *)
  let raw_typename t = cpp_typename syms (beta_reduce syms sr  (tsubst vs ts t)) in
  let gen_case_index e =
    let _,t = e in
    begin match t with
    | `BTYP_sum _
    | `BTYP_unitsum _
    | `BTYP_variant _ ->
      if is_unitsum t then ge' e
      else ce_dot (ge' e) "variant"
    | `BTYP_inst (i,ts) ->
      let ts = map tsub ts in
      let id,_,_,entry =
        try Hashtbl.find bbdfns i
        with Not_found -> failwith ("[gen_expr: case_index] Can't find index " ^ si i)
      in
      begin match entry with
      | `BBDCL_union (bvs,cts) ->
        let tsub' t = reduce_type (beta_reduce syms sr  (tsubst bvs ts t)) in
        let cts = map (fun (_,_,t) -> tsub' t) cts in
        if all_voids cts then ge' e
        else ce_dot (ge' e) "variant"
      | _ -> failwith ("Woops expected union, got " ^ id)
      end
    | _ -> failwith ("Woops expected union or sum, got " ^ sbt syms.dfns t)
    end

  in
  let ge_arg ((x,t) as a) =
    let t = tsub t in
    match t with
    | `BTYP_tuple [] -> ""
    | _ -> ge a
  in
  let id,parent,_,entry =
    try Hashtbl.find bbdfns this
    with Not_found -> failwith ("[gen_expr] Can't find this = " ^ si this)
  in
  let our_display = get_display_list syms bbdfns this in
  let our_level = length our_display in
  let rt t = reduce_type (beta_reduce syms sr  (tsubst vs ts t)) in
  let t = rt t in
  match t with
  | `BTYP_tuple [] ->
      clierr sr
     ("[egen] In "^sbe syms.dfns bbdfns (e,t)^":\nunit value required, should have been eliminated")

     (* ce_atom ("UNIT_ERROR") *)
  | _ ->
  match e with
  | BEXPR_expr (s,_) -> ce_top s

  | BEXPR_case_index e -> gen_case_index e

  | BEXPR_range_check (e1,e2,e3) ->
     let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
     let f = ce_atom ("\""^ f ^"\"") in
     let sl = ce_atom (si sl) in
     let sc = ce_atom (si sc) in
     let el = ce_atom (si el) in
     let ec = ce_atom (si ec) in
     let sref = ce_call (ce_atom "flx::rtl::flx_range_srcref_t") [f;sl;sc;el;ec] in
     let cf = ce_atom "__FILE__" in
     let cl = ce_atom "__LINE__" in
     let args : cexpr_t list =
       [ ge' e1 ; ge' e2; ge' e3; sref; cf; cl]
     in
     ce_call (ce_atom "flx::rtl::range_check") args

  | BEXPR_get_n (n,(e',t as e)) ->
    begin match rt t with
    | `BTYP_array (_,`BTYP_unitsum _) ->
      begin match e with
      | BEXPR_tuple _,_ -> print_endline "Failed to slice a tuple!"
      | _ -> ()
      end;
      ce_dot (ge' e) ("data["^si n^"]")
    | `BTYP_record es ->
      let field_name,_ =
        try nth es n
        with Not_found ->
          failwith "[flx_egen] Woops, index of non-existent struct field"
      in
      ce_dot (ge' e) field_name

    | `BTYP_inst (i,_) ->
      begin match Hashtbl.find bbdfns i with
      | _,_,_,`BBDCL_cstruct (_,ls)
      | _,_,_,`BBDCL_struct (_,ls) ->
        let name,_ =
          try nth ls n
          with _ ->
            failwith "Woops, index of non-existent struct field"
        in
        ce_dot (ge' e) name

      | _ -> failwith "[flx_egen] Instance expected to be (c)struct"
      end

    | _ -> ce_dot (ge' e) ("mem_" ^ si n)
    end

  | BEXPR_get_named (n,(e',t as e)) ->
    (*
    print_endline "Handling get_named expression";
    *)
    begin match rt t with
    | `BTYP_inst (i,ts) ->
      let cname = cpp_instance_name syms bbdfns n ts in
      ce_arrow (ge' e) cname
    | _ -> assert false
    end

  | BEXPR_match_case (n,((e',t') as e)) ->
    let t' = reduce_type (beta_reduce syms sr  t') in
    let x = gen_case_index e in
    ce_infix "==" x (ce_atom (si n))

    (*
    if is_unitsum t' then
      ce_infix "==" (ge' e) (ce_atom (si n))
    else
      ce_infix "=="
      (ce_dot (ge' e) "variant")
      (ce_atom (si n))
    *)

  | BEXPR_case_arg (n,e) ->
    (*
    print_endline ("Decoding nonconst ctor type " ^ sbt syms.dfns t);
    *)
    begin match t with (* t is the result of the whole expression *)
    | `BTYP_function _ ->
      let cast = tn t in
      ce_cast cast (ce_dot (ge' e) "data")
    | _ ->
      let cast = tn t ^ "*" in
      ce_prefix "*" (ce_cast cast (ce_dot (ge' e) "data"))
    end

  | BEXPR_deref ((BEXPR_ref (index,ts)),`BTYP_pointer t) ->
    ge' (BEXPR_name (index,ts),t)

  | BEXPR_address e -> ce_prefix "&" (ge' e)

  | BEXPR_deref e ->
    (*
    let cast = tn t ^ "*" in
    *)
    (*
    ce_prefix "*" (ce_cast cast (ce_dot (ge' e) "get_data()"))
    *)
    (*
    ce_prefix "*" (ce_cast cast (ge' e) )
    *)
    ce_prefix "*" (ge' e)

  (* fun reductions .. probably should be handled before
     getting here
  *)

  (* double negation is elimnated *)
  | BEXPR_not (BEXPR_not e,_) -> ge' e

  (* likelyhoods are reversed *)
  | BEXPR_not (BEXPR_likely e,_) ->
    ge' (BEXPR_unlikely ((BEXPR_not e),t),t)

  | BEXPR_not (BEXPR_unlikely e,_) ->
    ge' (BEXPR_likely ((BEXPR_not e),t),t)

  | BEXPR_likely e ->
    begin match t with
    | `BTYP_unitsum 2 ->
      ce_atom ("FLX_LIKELY("^ge e^")")
    | _ -> ge' e
    end

  | BEXPR_unlikely e ->
    begin match t with
    | `BTYP_unitsum 2 ->
      ce_atom ("FLX_UNLIKELY("^ge e^")")
    | _ -> ge' e
    end

  | BEXPR_not e ->
    ce_prefix "!" (ge' e)

  | BEXPR_new e ->
    let ref_type = tn t in
    let _,t' = e in
    let pname = shape_of syms bbdfns tn t' in
    let typ = tn t' in
    let frame_ptr =
      "new(*PTF gcp,"^pname^",true) " ^
      typ ^ "("^ge e ^")"
    in
    let reference = ref_type ^ "(" ^ frame_ptr ^ ")" in
    ce_atom reference


  | BEXPR_literal v ->
    if is_native_literal v
    then ce_atom (cstring_of_literal v)
    else
    let t = tn t in
    ce_atom ("("^t ^ ")(" ^ cstring_of_literal v ^ ")")

  | BEXPR_case (v,t') ->
    begin match unfold syms.dfns t' with
    | `BTYP_unitsum n ->
      if v < 0 or v >= n
      then
        failwith
        (
          "Invalid case index " ^ si v ^
          " of " ^ si n ^ " cases  in unitsum"
        )
     else ce_atom (si v)

    | `BTYP_sum ls ->
       let s =
         let n = length ls in
         if v < 0 or v >= n
         then
           failwith
           (
             "Invalid case index " ^ si v ^
             " of " ^ si n ^ " cases"
           )
         else let t' = nth ls v in
         if t' = `BTYP_tuple []
         then (* closure of const ctor is just the const value ???? *)
           if is_unitsum t then
             si v
           else
             "_uctor_(" ^ si v ^ ",0)"
         else
           failwith
           (
              "Can't handle closure of case " ^
              si v ^
              " of " ^
              string_of_btypecode syms.dfns t
           )
       in ce_atom s

    | _ -> failwith "Case tag must have sum type"
    end

  | BEXPR_name (index,ts') ->
    let id,parent,sr2,entry =
      try Hashtbl.find bbdfns index
      with _ ->
        match
          try Hashtbl.find syms.dfns index
          with Not_found -> assert false
        with
        {id=id; sr=sr} -> syserr sr
        ("[gen_expr(name)] Can't find "^ id ^ "<" ^ si index ^ ">")
    in
    let ts = map tsub ts' in
    begin match entry with
      | `BBDCL_val (_,`BTYP_function (`BTYP_void,_))  ->
          let ptr = (get_var_ref syms bbdfns this index ts) in
          ce_call (ce_arrow (ce_atom ptr) "apply") []

      | `BBDCL_var (_,t)
      | `BBDCL_val (_,t)
      | `BBDCL_ref (_,t)
      | `BBDCL_tmp (_,t)
        ->
          ce_atom (get_var_ref syms bbdfns this index ts)

      | `BBDCL_const (props,_,_,ct,_) ->
        if mem `Virtual props then
          print_endline ("Instantiate virtual const " ^ id)
        ;
        begin match ct with
        | `Identity -> syserr sr ("Nonsense Idendity const" ^ id)
        | `Virtual -> clierr2 sr sr2 ("Instantiate virtual const " ^ id)
        | `Str c
        | `StrTemplate c when c = "#srcloc" ->
           let f, l1, c1, l2, c2 = Flx_srcref.to_tuple sr in
           ce_atom ("flx::rtl::flx_range_srcref_t(" ^
             string_of_string f ^ "," ^
             si l1 ^ "," ^
             si c1 ^ "," ^
             si l2 ^ "," ^
             si c2 ^ ")"
           )

        | `Str c when c = "#this" ->
          begin match parent with
          | None -> clierr sr "Use 'this' outside class"
          | Some p ->
            let name = cpp_instance_name syms bbdfns p ts in
            (*
            print_endline ("class = " ^ si p ^ ", instance name = " ^ name);
            *)
            ce_atom("ptr"^name)
          end

        | `Str c
        | `StrTemplate c when c = "#memcount" ->
          begin match ts with
          | [`BTYP_void] -> ce_atom "0"
          | [`BTYP_unitsum n]
          | [`BTYP_array (_,`BTYP_unitsum n)] -> ce_atom (si n)
          | [`BTYP_sum ls]
          | [`BTYP_tuple ls] -> let n = length ls in ce_atom (si n)
          | [`BTYP_inst (i,_)] ->
            let _,_,_,entry = Hashtbl.find bbdfns i in
            begin match entry with
              | `BBDCL_struct (_,ls) -> let n = length ls in ce_atom (si n)
              | `BBDCL_union (_,ls) -> let n = length ls in ce_atom (si n)
              | _ ->
                clierr sr (
                  "#memcount function requires type with members to count, got: " ^
                  sbt syms.dfns (hd ts)
                )
            end
          | _ ->
            clierr sr (
              "#memcount function requires type with members to count, got : " ^
              sbt syms.dfns (hd ts)
            )
          end
        | `Str c -> ce_expr "expr" c
        | `StrTemplate c ->
          let ts = map tn ts in
          csubst sr sr2 c (ce_atom "Error") [] [] "Error" "Error" ts "expr" "Error" ["Error"] ["Error"] ["Error"]
        end

      (* | `BBDCL_function (_,_,([s,(_,`BTYP_void)],_),_,[BEXE_fun_return e]) -> *)
      | `BBDCL_function (_,_,([],_),_,[BEXE_fun_return (_,e)]) ->
        ge' e

      | `BBDCL_struct _
      | `BBDCL_function _
      | `BBDCL_procedure _
      | `BBDCL_fun _
      | `BBDCL_proc _ ->
         syserr sr
         (
           "[gen_expr: name] Open function '" ^
           id ^ "'<"^si index^
           "> in expression (closure required)"
         )
      | _ ->
        syserr sr
        (
          "[gen_expr: name] Cannot use this kind of name '"^
          id^"' in expression"
        )
    end

  | BEXPR_closure (index,ts') ->
    (*
    print_endline ("Generating closure of " ^ si index);
    *)
    let id,parent,sr,entry =
      try Hashtbl.find bbdfns index
      with _ -> failwith ("[gen_expr(name)] Can't find index " ^ si index)
    in
    (*
    Should not be needed now ..
    let ts = adjust_ts syms index ts' in
    *)
    let ts = map tsub ts' in
    begin match entry with
    | `BBDCL_function (props,_,_,_,_)
    | `BBDCL_procedure (props,_,_,_) ->
      let the_display =
        let d' =
          map (fun (i,vslen) -> "ptr"^ cpp_instance_name syms bbdfns i (list_prefix ts vslen))
          (get_display_list syms bbdfns index)
        in
          if length d' > our_level
          then "this" :: tl d'
          else d'
      in
      let name = cpp_instance_name syms bbdfns index ts in
      if mem `Cfun props then ce_atom name
      else
        ce_atom (
        "(FLX_NEWP("^name^")" ^ strd the_display props ^")"
        )

    | `BBDCL_callback _ ->
      print_endline "Mapping closure of callback to C function pointer";
      ce_atom id

    | `BBDCL_struct _
    | `BBDCL_fun _
    | `BBDCL_proc _ ->
      failwith ("[gen_expr: closure] Can't wrap primitive proc, fun, or struct '"^id^"' yet")
    | _ -> failwith ("[gen_expr: closure] Cannot use this kind of name '"^id^"' in expression")
    end

  | BEXPR_ref (index,ts') ->
    let ts = map tsub ts' in
    let ref_type = tn t in
    (*
    let frame_ptr, var_ptr =
      match t with
      | `BTYP_tuple [] -> "NULL","0"
      | _ ->

        let parent = match Hashtbl.find bbdfns index with _,parent,sr,_ -> parent in
        if Some this = parent &&
        (
          let props = match entry with
            | `BBDCL_procedure (props,_,_,_)
            | `BBDCL_function (props,_,_,_,_) -> props
            | _ -> assert false
          in
          mem `Pure props && not (mem `Heap_closure props)
        )
        then
          "NULL","&"^get_var_ref syms bbdfns this index ts ^"-NULL"
        else
          get_var_frame syms bbdfns this index ts,
          "&" ^ get_var_ref syms bbdfns this index ts
    in
    let reference = ref_type ^
      "(" ^ frame_ptr ^ ", " ^ var_ptr ^ ")"
    in
    ce_atom reference
    *)

    ce_cast ref_type
    begin match t with
      | `BTYP_tuple [] -> ce_atom "0"
      | _ ->
        let v = get_var_ref syms bbdfns this index ts in
        ce_prefix "&" (ce_atom v)
    end

  (* Hackery -- we allow a constructor with no
     arguments to be applied to a unit anyhow
  *)

  | BEXPR_variant (s,((_,t') as e)) ->
    print_endline ("Variant " ^ s);
    print_endline ("Type " ^ sbt syms.dfns t);
    let
      arg_typename = tn t' and
      union_typename = tn t
    in
    let aval =
      "new (*PTF gcp, "^arg_typename^"_ptr_map,true) " ^
      arg_typename ^ "(" ^ ge_arg e ^ ")"
    in
    let ls = match t with
      | `BTYP_variant ls -> ls
      | _ -> failwith "[egen] Woops variant doesn't have variant type"
    in
    let vidx = match list_assoc_index ls s with
      | Some i -> i
      | None -> failwith "[egen] Woops, variant field not in type"
    in
    print_endline ("Index " ^ si vidx);
    let uval = "_uctor_("^si vidx^"," ^ aval ^")"  in
    ce_atom uval

  | BEXPR_coerce ((srcx,srct) as srce,dstt) ->
    let coerce_variant () =
      let vts =
        match dstt with
        | `BTYP_variant ls -> ls
        | _ -> syserr sr "Coerce non-variant"
      in
      begin match srcx with
      | BEXPR_variant (s,argt) ->
        print_endline "Coerce known variant!";
        ge' (BEXPR_variant (s,argt),t)
      | _ ->
        let i =
          begin try
            Hashtbl.find syms.variant_map (srct,dstt)
          with Not_found ->
            let i = !(syms.counter) in incr (syms.counter);
            Hashtbl.add syms.variant_map (srct,dstt) i;
            i
        end
        in
        ce_atom ("_uctor_(vmap_"^si i^","^ge srce^")")
      end
    in
    begin match dstt with
    | `BTYP_variant _ -> coerce_variant ()
    | _ -> ce_atom ("reinterpret<"^tn dstt^","^tn srct^">("^ge srce^")")
    end

  | BEXPR_apply
     (
       (BEXPR_case (v,t),t'),
       (a,t'')
     ) ->
       (* t is the type of the sum,
          t' is the function type of the constructor,
          t'' is the type of the argument
       *)
       let
         arg_typename = tn t''
       and
         union_typename = tn t
       in
       let aval =
         "new (*PTF gcp, "^arg_typename^"_ptr_map,true) " ^
         arg_typename ^ "(" ^ ge_arg (a,t'') ^ ")"
       in
       let uval =
         if is_unitsum t then
           si v
         else
         "_uctor_(" ^ si v ^ ", " ^ aval ^")"
       in
       let s = "(" ^ union_typename ^ ")" ^ uval in
       ce_atom s

       (*
       failwith
       (
         "Trapped application, case " ^
         si v ^
         " of " ^ string_of_btypecode syms.dfns t ^
         "\ntype " ^ string_of_btypecode syms.dfns t' ^
         "\nargument=" ^
         string_of_bound_expression syms.dfns (a,t'') ^
         "\ntype " ^ string_of_btypecode syms.dfns t''
       )
      *)


  | BEXPR_apply_prim (index,ts,(arg,argt as a)) ->
    (*
    print_endline ("Prim apply, arg=" ^ sbe syms.dfns bbdfns a);
    *)
    let argt = tsub argt in
    let id,parent,sr2,entry =
      try Hashtbl.find bbdfns index
      with _ -> failwith ("[gen_expr(apply instance)] Can't find index " ^ si index)
    in
    begin
    match entry with
    | `BBDCL_fun (props,vs,ps,retyp,ct,_,prec) ->
      if length vs <> length ts then
      failwith
      (
        "[get_expr:apply closure of fun] function " ^
        id ^ "<" ^ si index ^">" ^
        ", wrong number of args, expected vs = " ^
        si (length vs) ^
        ", got ts=" ^
        si (length ts)
      );
      begin match ct with
      | `Identity -> ge' a

      | `Virtual ->
        let ts = map tsub ts in
        let index', ts' = Flx_typeclass.fixup_typeclass_instance syms bbdfns index ts in
        if index <> index' then
          clierr sr ("Virtual call of " ^ si index ^ " dispatches to " ^ si index')
        ;
        if index = index' then
        begin
          let entries =
            try Hashtbl.find syms.typeclass_to_instance index
            with Not_found -> (* print_endline ("Symbol " ^ si index ^ " Not instantiated?"); *) []
          in
          iter
          (fun (bvs,t,ts,j) -> print_endline ("Candidate Instance " ^ si j ^ "[" ^ catmap "," (sbt syms.dfns) ts ^ "]"))
          entries
          ;

          clierr2 sr sr2 ("Instantiate virtual function(2) " ^ id ^ "<" ^si index ^
            ">, no instance for ts="^ catmap "," (sbt syms.dfns) ts
          )
        end;
        begin let _,_,sr3,entry =
          try Hashtbl.find bbdfns index'
          with Not_found -> syserr sr ("MISSING INSTANCE BBDCL " ^ si index')
        in
        match entry with
        | `BBDCL_fun _ -> ge' (BEXPR_apply_prim (index',ts',a),t)
        | `BBDCL_function _ -> ge' (BEXPR_apply_direct (index',ts',a),t)
        | _ ->
          clierr2 sr sr3 ("expected instance to be function " ^ id)
        end

      | `Str s -> ce_expr prec s
      | `StrTemplate s ->
        let ts = map tsub ts in
        let retyp = reduce_type (beta_reduce syms sr  (tsubst vs ts retyp)) in
        let retyp = tn retyp in
        gen_prim_call syms bbdfns tsub ge'' s ts (arg,argt) retyp sr sr2 prec
      end

    | `BBDCL_callback (props,vs,ps_cf,ps_c,_,retyp,_,_) ->
      assert (retyp <> `BTYP_void);
      if length vs <> length ts then
      clierr sr "[gen_prim_call] Wrong number of type arguments"
      ;
      let ts = map tsub ts in
      let s = id ^ "($a)" in
      let retyp = reduce_type (beta_reduce syms sr  (tsubst vs ts retyp)) in
      let retyp = tn retyp in
      gen_prim_call syms bbdfns tsub ge'' s ts (arg,argt) retyp sr sr2 "atom"

    (* but can't be a Felix function *)
    | _ ->
      failwith
      (
        "[gen_expr: apply prim] Expected '"^id^"' to be primitive function instance, got:\n" ^
        string_of_bbdcl syms.dfns bbdfns entry index
      )
    end

  | BEXPR_apply_struct (index,ts,a) ->
    let id,parent,sr2,entry =
      try Hashtbl.find bbdfns index
      with _ -> failwith ("[gen_expr(apply instance)] Can't find index " ^ si index)
    in
    let ts = map tsub ts in
    begin match entry with
    | `BBDCL_cstruct (vs,_) ->
      let name = tn (`BTYP_inst (index,ts)) in
      ce_atom ("reinterpret<"^ name ^">(" ^ ge a ^ ")")

    | `BBDCL_struct (vs,cts) ->
      let name = tn (`BTYP_inst (index,ts)) in
      if length cts > 1 then
        (* argument must be an lvalue *)
        ce_atom ("reinterpret<"^ name ^">(" ^ ge a ^ ")")
      else if length cts = 0 then
        ce_atom (name ^ "()")
      else
        ce_atom (name ^ "(" ^ ge a ^ ")")

    | `BBDCL_nonconst_ctor (vs,uidx,udt,cidx,ct,evs, etraint) ->
      (* due to some hackery .. the argument of a non-const
         ctor can STILL be a unit .. prolly cause the stupid
         compiler is checking for voids for these pests,
         but units for sums .. hmm .. inconsistent!
      *)
      let ts = map tsub ts in
      let ct = reduce_type (beta_reduce syms sr  (tsubst vs ts ct)) in
      let _,t = a in
      let t = reduce_type (beta_reduce syms sr  (tsubst vs ts t)) in
      begin match ct with
      | `BTYP_tuple [] ->
        ce_atom ( "_uctor_(" ^ si cidx ^ ", NULL)")

      (* function types are already pointers .. any use of this
         should do a clone .. class types are also pointers ..
      *)
      | `BTYP_function _ ->
        ce_atom (
          "_uctor_(" ^ si cidx ^ ", " ^ ge a ^")"
        )

      | _ ->
        let ctt = tn ct in
        let ptrmap = shape_of syms bbdfns tn ct in
        let txt =
           "_uctor_(" ^ si cidx ^ ", new(*PTF gcp,"^ ptrmap^",true)"^
           ctt ^"("^ ge a ^"))"
        in
        ce_atom txt
      end
    | _ -> assert false
    end

  | BEXPR_apply_direct (index,ts,a) ->
    let ts = map tsub ts in
    let index', ts' = Flx_typeclass.fixup_typeclass_instance syms bbdfns index ts in
    if index <> index' then
      clierr sr ("Virtual call of " ^ si index ^ " dispatches to " ^ si index')
    ;
    if index <> index' then
    begin
      let _,_,sr3,entry =
        try Hashtbl.find bbdfns index'
        with Not_found -> syserr sr ("MISSING INSTANCE BBDCL " ^ si index')
      in
      match entry with
      | `BBDCL_fun _ -> ge' (BEXPR_apply_prim (index',ts',a),t)
      | `BBDCL_function _ -> ge' (BEXPR_apply_direct (index',ts',a),t)
      | _ ->
          clierr2 sr sr3 ("expected instance to be function " ^ id)
    end else

    let id,parent,sr2,entry =
      try Hashtbl.find bbdfns index
      with _ -> failwith ("[gen_expr(apply instance)] Can't find index " ^ si index)
    in
    begin
    (*
    print_endline ("apply closure of "^ id );
    print_endline ("  .. argument is " ^ string_of_bound_expression syms.dfns a);
    *)
    match entry with
    | `BBDCL_function (props,_,_,_,_) ->
      (*
      print_endline ("Generating closure[apply direct] of " ^ si index);
      *)
      let the_display =
        let d' =
          map (fun (i,vslen)-> "ptr"^ cpp_instance_name syms bbdfns i (list_prefix ts vslen))
          (get_display_list syms bbdfns index)
        in
          if length d' > our_level
          then "this" :: tl d'
          else d'
      in
      let name = cpp_instance_name syms bbdfns index ts in
      if mem `Cfun props
      then  (* this is probably wrong because it doesn't split arguments up *)
        ce_call (ce_atom name) [ce_atom (ge_arg a)]
      else
        ce_atom (
        "(FLX_NEWP("^name^")"^ strd the_display props ^")"^
        "\n      ->apply(" ^ ge_arg a ^ ")"
        )

    | `BBDCL_fun _ -> assert false
    (*
      ge' (BEXPR_apply_prim (index,ts,a),t)
    *)

    | _ ->
      failwith
      (
        "[gen_expr: apply_direct] Expected '"^id^"' to be generic function instance, got:\n" ^
        string_of_bbdcl syms.dfns bbdfns entry index
      )
    end

  | BEXPR_apply_stack (index,ts,a) ->
    let ts = map tsub ts in
    let index', ts' = Flx_typeclass.fixup_typeclass_instance syms bbdfns index ts in
    if index <> index' then
      clierr sr ("Virtual call of " ^ si index ^ " dispatches to " ^ si index')
    ;
    if index <> index' then
    begin
      let _,_,sr3,entry =
        try Hashtbl.find bbdfns index'
        with Not_found -> syserr sr ("MISSING INSTANCE BBDCL " ^ si index')
      in
      match entry with
      | `BBDCL_fun _ -> ge' (BEXPR_apply_prim (index',ts',a),t)
      | `BBDCL_function _ -> ge' (BEXPR_apply_direct (index',ts',a),t)
      | _ ->
          clierr2 sr sr3 ("expected instance to be function " ^ id)
    end else

    let id,parent,sr2,entry =
      try Hashtbl.find bbdfns index
      with _ -> failwith ("[gen_expr(apply instance)] Can't find index " ^ si index)
    in
    begin
    (*
    print_endline ("apply closure of "^ id );
    print_endline ("  .. argument is " ^ string_of_bound_expression syms.dfns a);
    *)
    match entry with
    | `BBDCL_function (props,vs,(ps,traint),retyp,_) ->
      let display = get_display_list syms bbdfns index in
      let name = cpp_instance_name syms bbdfns index ts in

      (* C FUNCTION CALL *)
      if mem `Pure props && not (mem `Heap_closure props) then
        let s =
          assert (length display = 0);
          match ps with
          | [] -> ""
          | [{pindex=ix; ptyp=t}] ->
            if Hashtbl.mem syms.instances (ix,ts)
            then ge_arg a
            else ""

          | _ ->
            begin match a with
            | BEXPR_tuple xs,_ ->
              (*
              print_endline ("Arg to C function is tuple " ^ sbe syms.dfns bbdfns a);
              *)
              fold_left2
              (fun s ((x,t) as xt) {pindex=ix} ->
                let x =
                  if Hashtbl.mem syms.instances (ix,ts)
                  then ge_arg xt
                  else ""
                in
                if String.length x = 0 then s else
                s ^
                (if String.length s > 0 then ", " else "") ^ (* append a comma if needed *)
                x
              )
              ""
              xs ps

            | _,tt ->
              let tt = reduce_type (beta_reduce syms sr  (tsubst vs ts tt)) in
              (* NASTY, EVALUATES EXPR MANY TIMES .. *)
              let n = ref 0 in
              fold_left
              (fun s i ->
                (*
                print_endline ( "ps = " ^ catmap "," (fun (id,(p,t)) -> id) ps);
                print_endline ("tt=" ^ sbt syms.dfns tt);
                *)
                let t = nth_type tt i in
                let a' = BEXPR_get_n (i,a),t in
                let x = ge_arg a' in
                incr n;
                if String.length x = 0 then s else
                s ^ (if String.length s > 0 then ", " else "") ^ x
              )
              ""
              (nlist (length ps))
            end
        in
        let s =
          if mem `Requires_ptf props then
            if String.length s > 0 then "FLX_FPAR_PASS " ^ s
            else "FLX_FPAR_PASS_ONLY"
          else s
        in
          ce_atom (name ^ "(" ^ s ^ ")")
      else
        let the_display =
          let d' =
            map (fun (i,vslen)-> "ptr"^ cpp_instance_name syms bbdfns i (list_prefix ts vslen))
            display
          in
            if length d' > our_level
            then "this" :: tl d'
            else d'
        in
        let s =
          name^ strd the_display props
          ^
          "\n      .apply(" ^ ge_arg a ^ ")"
        in ce_atom s

    | _ ->
      failwith
      (
        "[gen_expr: apply_stack] Expected '"^id^"' to be generic function instance, got:\n" ^
        string_of_bbdcl syms.dfns bbdfns entry index
      )
    end

  | BEXPR_apply ((BEXPR_closure (index,ts),_),a) ->
    assert false (* should have been factored out *)

  (* application of C function pointer, type
     f: a --> b
  *)
(*  | BEXPR_apply ( (_,`BTYP_lvalue(`BTYP_cfunction _)) as f,a) *)
  | BEXPR_apply ( (_,`BTYP_cfunction _) as f,a) ->
    ce_atom (
    (ge f) ^"(" ^ ge_arg a ^ ")"
    )

  (* General application*)
  | BEXPR_apply (f,a) ->
    ce_atom (
    "("^(ge f) ^ ")->clone()\n      ->apply(" ^ ge_arg a ^ ")"
    )

  | BEXPR_record es ->
    let rcmp (s1,_) (s2,_) = compare s1 s2 in
    let es = sort rcmp es in
    let es = map snd es in
    let ctyp = tn t in
    ce_atom (
    ctyp ^ "(" ^
      fold_left
      (fun s e ->
        let x = ge_arg e in
        if String.length x = 0 then s else
        s ^
        (if String.length s > 0 then ", " else "") ^
        x
      )
      ""
      es
    ^
    ")"
    )

  | BEXPR_tuple es ->
    (*
    print_endline ("Eval tuple " ^ sbe syms.dfns bbdfns (e,t));
    *)
    (* just apply the tuple type ctor to the arguments *)
    begin match t with
    | `BTYP_array (t',`BTYP_unitsum n) ->
      let tuple =
        let t'' = `BTYP_tuple (map (fun _ -> t') (nlist n)) in
        let ctyp = raw_typename t'' in
        ce_atom (
        ctyp ^ "(" ^
          fold_left
          (fun s e ->
            let x = ge_arg e in
            if String.length x = 0 then s else
            s ^
            (if String.length s > 0 then ", " else "") ^
            x
          )
          ""
          es
        ^
        ")"
        )
      in
        (* cast a tuple which is an array type to an array *)
        let atyp = tn t in
        ce_call
          (ce_atom ("reinterpret<" ^ atyp ^">"))
          [tuple]

    | `BTYP_tuple _ ->
      let ctyp = tn t in
      ce_atom (
      ctyp ^ "(" ^
        fold_left
        (fun s e ->
          let x = ge_arg e in
          if String.length x = 0 then s else
          s ^
          (if String.length s > 0 then ", " else "") ^
          x
        )
        ""
        es
      ^
      ")"
      )
    | _ -> assert false
    end

and gen_expr syms bbdfns this e vs ts sr =
  let e = Flx_maps.reduce_tbexpr bbdfns e in
  let s =
    try gen_expr' syms bbdfns this e vs ts sr
    with Unknown_prec p -> clierr sr
    ("[gen_expr] Unknown precedence name '"^p^"' in " ^ sbe syms.dfns bbdfns e)
  in
  string_of_cexpr s
