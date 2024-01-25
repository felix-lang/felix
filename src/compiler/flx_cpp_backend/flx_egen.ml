open Flx_util
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
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
open Flx_label
open Flx_btype_subst

let debug = false 
module CS = Flx_code_spec
module L = Flx_literal
exception Vfound (* for variants, Found already used elsewhere *)

let string_of_string = Flx_string.c_quote_of_string
let nth lst idx = 
  try List.nth lst idx 
  with _ -> failwith ("[flx_egen] List.nth failed, list length=" ^ 
    string_of_int (List.length lst) ^
    ", index sought=" ^string_of_int idx)


let get_var_frame syms bsym_table this index ts : string =
  let bsym_parent, bsym =
    try Flx_bsym_table.find_with_parent bsym_table index
    with Not_found ->
      failwith ("[get_var_frame(1)] Can't find index " ^ string_of_bid index)
  in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_label _ 
  | BBDCL_val (_,_,(`Val | `Var | `Ref | `Once)) ->
      begin match bsym_parent with
      | None -> "ptf"
      | Some i ->
          if i <> this
          then "ptr" ^ cpp_instance_name syms bsym_table i ts
          else "this"
      end
  | BBDCL_val (vs,t,`Tmp) ->
     failwith ("[get_var_frame] temporaries aren't framed: " ^ Flx_bsym.id bsym)

  | _ -> failwith ("[get_var_frame] Expected name " ^ Flx_bsym.id bsym ^ " to be variable or value")

let get_var_ref syms bsym_table this index ts : string =
  let bsym_parent, bsym =
    try Flx_bsym_table.find_with_parent bsym_table index
    with Not_found ->
      failwith ("[get_var_ref] Can't find index " ^ string_of_bid index)
  in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_val (vs,t,(`Val | `Var | `Ref | `Once)) ->
      begin match bsym_parent with
      | None -> "ptf-> " ^ cpp_instance_name syms bsym_table index ts
      | Some i ->
          (
            if i <> this
            then "ptr" ^ cpp_instance_name syms bsym_table i ts ^ "->"
            else ""
          ) ^ cpp_instance_name syms bsym_table index ts
      end

  | BBDCL_val (vs,t,`Tmp) ->
      cpp_instance_name syms bsym_table index ts

  | _ -> failwith ("[get_var_ref(3)] Expected name " ^ Flx_bsym.id bsym ^ " to be variable, value or temporary")

let get_ref_ref syms bsym_table this index ts : string =
  let bsym_parent, bsym =
    try Flx_bsym_table.find_with_parent bsym_table index
    with Not_found ->
      failwith ("[get_var_ref] Can't find index " ^ string_of_bid index)
  in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_val (vs,t,(`Val | `Var | `Ref | `Once )) ->
      begin match bsym_parent with
      | None ->
          "ptf-> " ^ cpp_instance_name syms bsym_table index ts
      | Some i ->
          (
            if i <> this
            then "ptr" ^ cpp_instance_name syms bsym_table i ts ^ "->"
            else ""
          ) ^
          cpp_instance_name syms bsym_table index ts
      end

  | BBDCL_val (vs,t,`Tmp) ->
      cpp_instance_name syms bsym_table index ts

  | _ -> failwith ("[get_var_ref(3)] Expected name " ^ Flx_bsym.id bsym ^ " to be variable, value or temporary")

let nth_type ts i =
  try match ts with
  | BTYP_tuple ts -> nth ts i
  | BTYP_compacttuple ts -> nth ts i
  | BTYP_array (t,BTYP_unitsum n) -> assert (i<n); t
  | BTYP_compactarray (t,BTYP_unitsum n) -> assert (i<n); t
  | _ -> assert false
  with Not_found ->
    failwith ("Can't find component " ^ si i ^ " of type!")

(* dumb routine to know if we need parens around a type name when used
 * as a cast: if it's an identifier, no we don't, otherwise we have
 * to use an old style cast
 *)

let idchars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"

let isid x = 
  let isid_char ch = 
    try
      for i = 0 to String.length idchars - 1 do
        if ch = idchars.[i] then raise Not_found
      done;
      false
    with Not_found -> true
  in
  try 
    for i = 0 to String.length x - 1 do
      if not (isid_char x.[i]) then raise Not_found;
    done;
    true
  with _ -> false

let rec gen_expr'
  syms
  bsym_table
  (shapes : Flx_set.StringSet.t ref)
  (shape_map: (string, Flx_btype.t) Hashtbl.t)
  label_info
  this
  this_vs
  this_ts
  sr
  (e,t)
  : cexpr_t
=
  if debug then
  print_endline ("Flx_egen: Gen_expr': " ^ sbe bsym_table (e,t) ^ ", type = " ^ Flx_btype.st t);
  let rec f_bexpr e = Flx_bexpr.map ~f_bexpr e in
  let e,t = f_bexpr (e,t) in
  match e with
  (* replace heap allocation of a unit with NULL pointer *)
  | BEXPR_new (BEXPR_tuple [],_) -> ce_atom "0/*NULL*/"
  | _ ->
  if length this_ts <> length this_vs then begin
    failwith
    (
      "[gen_expr} wrong number of args, expected vs = " ^
      si (length this_vs) ^
      ", got ts=" ^
      si (length this_ts)
    )
  end;
  let ate t1 t2 = assert (Flx_typeeq.type_eq (sbt bsym_table) syms.counter t1 t2) in
  let ge = gen_expr syms bsym_table shapes shape_map label_info this this_vs this_ts sr in
  let ge' = gen_expr' syms bsym_table shapes shape_map label_info this this_vs this_ts sr in
  let tsub t = beta_reduce "flx_egen" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr this_vs this_ts t) in
  let tn t = cpp_typename syms bsym_table (tsub t) in
  let clt t = islinear_type t in
  (* NOTE this function does not do a reduce_type *)
  let raw_typename t =
    cpp_typename
    syms
    bsym_table
    (beta_reduce "flx_egen2" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr this_vs this_ts t))
  in
  let ge_arg ((x,t) as a) =
    let t = tsub t in
    match t with
    | BTYP_tuple [] -> ""
    | _ -> ge a
  in
  let ge_carg ps ts vs a =
    match a with
    | BEXPR_tuple xs,_ ->
      (*
      print_endline ("Arg to C function is tuple " ^ sbe bsym_table a);
      *)
      assert (List.length xs = List.length ps);
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
      let k = List.length ps in
      let tt = beta_reduce "flx_egen3" syms.Flx_mtypes2.counter bsym_table sr  (tsubst sr vs ts tt) in
      (* NASTY, EVALUATES EXPR MANY TIMES .. *)
      let n = ref 0 in
      fold_left
      (fun s i ->
        (*
        print_endline ( "ps = " ^ catmap "," (fun (id,(p,t)) -> id) ps);
        print_endline ("tt=" ^ sbt bsym_table tt);
        *)
        let t = nth_type tt i in
        let a' = bexpr_get_n t i a in
        let x = ge_arg a' in
        incr n;
        if String.length x = 0 then s else
        s ^ (if String.length s > 0 then ", " else "") ^ x
      )
      ""
      (Flx_list.nlist k)
  in
  let our_display = get_display_list bsym_table this in
  let our_level = length our_display in
  let rt t = beta_reduce "flx_egen4" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr this_vs this_ts t) in
  let array_sum_offset_table = syms.array_sum_offset_table in
  let seq = syms.counter in
  let t = rt t in
  match t with
  (* This is now required for arrays of length 1 *)
(*
  | BTYP_tuple [] -> ce_atom "(::flx::rtl::unit())/*UNIT TUPLE?*/"
      clierrx "[flx_cpp_backend/flx_egen.ml:239: E281] " sr
     ("[egen] In "^sbe bsym_table (e,t)^":\nunit value required, should have been eliminated")
*)

     (* ce_atom ("UNIT_ERROR") *)
  | _ ->
  match e with
  (* p is a pointer to product type d, either machine pointer or compact linear pointer now, 
     v is a list of constant projections starting at d and ending with c
  *)
  | BEXPR_cltpointer (d,c,p,v) -> 
    let mode = match t with | BTYP_ptr (mode,_,_) -> mode | _ -> assert false in
    let n = Flx_btype.sizeof_linear_type () c in
    let rec cal_divisor d v div = 
      match v with 
      | [] -> div
      | n :: tail ->
        match d with
        | BTYP_compacttuple ls ->
          let divisor = 
            List.fold_left (fun acc t -> acc * (Flx_btype.sizeof_linear_type () t)) 1
            (Flx_list.list_tail ls (n+1))
          in
          let d = List.nth ls n in
          cal_divisor d tail (div * divisor)
        | BTYP_compactarray (base, index_type) ->
          let m = Flx_btype.sizeof_linear_type () index_type in
          let rec pow a b = match b with | 0 -> 1 | 1 -> a | _ -> a * pow a (b - 1) in
          let base_size = Flx_btype.sizeof_linear_type () base in
          let divisor = pow base_size (m - n - 1) in 
          cal_divisor base tail (div * divisor) 
        | _ -> print_endline ("Flx_egen: Compact Linear Pointer, unimplemented component type " ^ sbt bsym_table d); assert false
    in
    let ctor = match mode with
     | `RW ->"::flx::rtl::clptr_t" 
     | `R ->"::flx::rtl::const_clptr_t" 
     | `V ->"::flx::rtl::const_clptr_t" 
     | _ -> assert false
    in
    let divisor = cal_divisor d v 1 in
    ce_call (ce_atom ctor) [ge' p; ce_int divisor; ce_int n]

  | BEXPR_cltpointer_prj (d,c,v) -> 
    print_endline ("Flx_egen: Construct compact linear pointer projection");
    print_endline ("should have been removed??");
    print_endline ("But we can make one in C++ now anyhow!");
    assert false
(*
    let n = Flx_btype.sizeof_linear_type () c in
    ce_call (ce_atom "::flx::rtl::clprj_t") [ce_int v; ce_int n]
*) 

  | BEXPR_int i -> ce_atom (si i)
  | BEXPR_polyrecord _ -> print_endline "Attempt to generate polyrecord value, should have been factored out"; assert false
  | BEXPR_remove_fields _ -> print_endline "Attempt to generate remove fields, should have been factored out"; assert false
  | BEXPR_getall_field _ -> print_endline "Attempt to generate getall field, should have been factored out"; assert false
  | BEXPR_unitptr k -> 
    begin match k with
    | 0 -> print_endline "Generating unit expr"; ce_atom "0/*[gen_expr']CLT:UNIT*/"
    | _ -> print_endline "Generating unitptr expr"; ce_atom ("NULL/*UNITPTR<"^string_of_int k^">*/")
    end
  | BEXPR_lambda _ -> assert false
  (* | BEXPR_uniq _ -> assert false *)
  | BEXPR_funprod _ -> assert false
  | BEXPR_funsum _ -> assert false
  | BEXPR_lrangle _ -> assert false
  | BEXPR_lrbrack _ -> assert false
  | BEXPR_label (idx) -> 
      let label = Flx_name.cid_of_flxid (Flx_label.get_label_name bsym_table idx) in
      let pc = get_label_pc idx in
      let frame = get_label_frame bsym_table idx in
      let distance = Flx_label.find_label_distance bsym_table label_info.labels_by_proc this idx in
      begin match distance with
      | `Local -> 
        let target_instance =
          try Hashtbl.find syms.instances (this, this_ts)
          with Not_found -> failwith ("[Flx_egen:BEXPR_label:Local] Can't find instance for "^string_of_int this)
        in
        let frame_ptr = "this" in
        let ladr = "FLX_FARTARGET(" ^ cid_of_bid pc ^ "," ^ cid_of_bid target_instance ^ "," ^ label ^ ")" in
        let label_value = "::flx::rtl::jump_address_t(" ^ frame_ptr ^ "," ^ ladr ^ ")" in
        ce_atom label_value

 
      | `Nonlocal -> 
        let target_instance =
          try Hashtbl.find syms.instances (frame, this_ts)
          with Not_found -> failwith ("[Flx_egen:BEXPR_label:Nonlocal] Can't find instance for "^string_of_int frame)
        in
        let frame_ptr = "ptr" ^ cpp_instance_name syms bsym_table frame this_ts in
        let ladr = "FLX_FARTARGET(" ^ cid_of_bid pc ^ "," ^ cid_of_bid target_instance ^ "," ^ label ^ ")" in
        let label_value = "::flx::rtl::jump_address_t(" ^ frame_ptr ^ "," ^ ladr ^ ")" in
        ce_atom label_value

      | `Unreachable ->
        syserr sr ("Closure of unreachable label " ^ label)
      end


  | BEXPR_case (n,BTYP_sum ts) when clt t ->
    let get_array_sum_offset_values bsym_table ts =
      let sizes = List.map (sizeof_linear_type bsym_table) ts in
      let rec aux acc tsin tsout = 
        match tsin with
        | [] -> List.rev tsout
        | h :: t -> aux (acc + h) t (acc :: tsout)
      in
      aux 0 sizes []
    in
    let case_offset bsym_table ts caseno = 
      match caseno with
      | 0 -> ce_int 0
      | n -> ce_int (nth (get_array_sum_offset_values bsym_table ts) n)
    in
    case_offset bsym_table ts n

  | BEXPR_case (n,BTYP_unitsum _) ->
    ce_int n

  (* if a tuple is compact linear, all the components must be
     too, and the result is just a linear combination 
  *)
  | BEXPR_compacttuple es ->
(*
print_endline ("Compact linear tuple " ^ sbt bsym_table t);
*)
    let result =
      begin match t with
      | BTYP_compacttuple ts  -> 
(*
  print_endline ("..tuple subkind " ^ sbt bsym_table t);
*)
        assert (List.length ts = List.length es);
        (*  we get  ((0 * sizeof typeof i + i) * sizeof typeof j + j ) * sizeof typeof k + k 
            which is BIG ENDIAN. The sizeof i is eliminated by multiplying by 0.
            Example 3 * 4 * 5, so i:3, j:4, k:5 -> ijk = ((0 * 3 + i) * 4 + j) * 5 + k = 20i + 5j + k
         *)
        List.fold_left 
          (fun acc (elt,t) -> 
            ce_add (ce_mul acc  (ce_int (sizeof_linear_type bsym_table t))) (ge' elt)
          ) 
          (ce_int 0)
          (List.combine es ts)

      | BTYP_compactarray (t, BTYP_unitsum n)  -> 
(*
  print_endline ("..array subkind " ^ sbt bsym_table t);
*)
        assert (List.length es = n);
        let sa = ce_int (sizeof_linear_type bsym_table t) in
        List.fold_left (fun acc elt -> ce_add (ce_mul acc sa) (ge' elt)) (ce_int 0) es

      | _ -> assert false
      end
    in
(*
    print_endline ("CLT rendered = " ^ string_of_cexpr result);
*)
    result 
  (* if this is an injection into a compact linear type, then the argument
     must be compact linear also, the result is just its value plus
     the sum of the sizes on its right.
  *)
  | BEXPR_apply ( (BEXPR_inj (caseno,_,_),_), ((_,at) as a) ) 
    when clt t ->
    assert (clt at);
    (* should call vgen now! *)
    begin match t with
    | BTYP_compactsum ts
    | BTYP_sum ts ->
      let get_array_sum_offset_values bsym_table ts =
        let sizes = List.map (sizeof_linear_type bsym_table) ts in
        let rec aux acc tsin tsout = 
          match tsin with
          | [] -> List.rev tsout
          | h :: t -> aux (acc + h) t (acc :: tsout)
        in
          aux 0 sizes []
      in
      let case_offset bsym_table ts caseno = 
        match caseno with
        | 0 -> ce_int 0
        | n -> ce_int (nth (get_array_sum_offset_values bsym_table ts) n)
      in
      ce_add (case_offset bsym_table ts caseno) (ge' a)
    | _ -> assert false
    end

  (* the v is an integer constant *)
  | BEXPR_apply ( (BEXPR_inj (v,d,c),ft' as f), (_,argt as a)) -> 
    ate d argt;
    ate c t;
    assert (not (clt c)); 
    let cx = Flx_vgen.gen_make_nonconst_ctor ge' tn syms bsym_table shape_map c v a in
(*
print_endline ("Generated application of injection application " ^ sbe bsym_table (e,t) ^ " as " ^ string_of_cexpr cx);
*)
    cx

(* co-array, that is, c = N *+ d = d + d + d .... + d, N time *)
  (* the v is an expression *)
  | BEXPR_apply ( (BEXPR_ainj (v,d,c),ft' as f), (_,argt as a)) -> 
    ate d argt;
    ate c t;
    (* assert (not (clt c));  ???*)
    (* The index is just the repeat count, the argument is of type d *)
    let cx = Flx_vgen.gen_make_nonconst_rptsum ge' tn syms bsym_table shape_map c v a in
(*
print_endline ("Generated application of injection application " ^ sbe bsym_table (e,t) ^ " as " ^ string_of_cexpr cx);
*)
    cx


  | BEXPR_apply (
     (BEXPR_cltpointer_prj (jd,jc,v1),_),
     (BEXPR_cltpointer (pd,pc,ptr,v2),_)
   ) ->
    print_endline ("Special case apply clt projection to clt pointer ");
    assert (jd = pd);
    ge' (bexpr_cltpointer pd jc ptr (v1 @ v2))

(* -------------- CONSTANT PROJECTIONS ----------------------------- *)
  | BEXPR_apply ( (BEXPR_prj (ix, domain,codomain),prjt), (_,argt as arg)) ->
    Flx_egen_apply_prj.apply_prj syms bsym_table ge ge' sr (e,t) ix domain codomain arg

  (* record projection *)
  | BEXPR_apply ( (BEXPR_rprj (name,seq,BTYP_record (es),_),_), a) ->
print_endline "rprj should have been removed";
assert false;
    let field_name = if seq = 0 then name else "_" ^ name ^ "_" ^ string_of_int seq in
    ce_dot (ge' a) (cid_of_flxid field_name)

  (* pointer to record projection *)
  | BEXPR_apply ( (BEXPR_rprj (name,seq,BTYP_ptr (_,(BTYP_record (es)),[]),_),_), a) ->
print_endline "rprj should have been removed";
assert false;
    let field_name = if seq = 0 then name else "_" ^ name ^ "_" ^ string_of_int seq in
    ce_prefix "&" (ce_arrow (ge' a) (cid_of_flxid field_name))

(* ------------ ARRAY PROJECTIONS ---------------------- *)
  | BEXPR_apply ( (BEXPR_aprj (ix, domain,codomain),prjt), (_,argt as arg)) ->
    Flx_egen_apply_prj.apply_array_prj syms bsym_table ge ge' sr (e,t) ix domain codomain arg

  (* Felix allows naked projections BUT they should have been eta-expanded
     away, that is, replaced by a closure containing an application
     of the projection to the function parameter
  *)
  (* we CAN handle this now, since we have made a closure for it: FIXME *)
  | BEXPR_prj (n,_,_) -> assert false

  (* we CAN handle this now, since we have made a closure for it: FIXME *)
  | BEXPR_rprj (n,_,_,_) -> assert false

  (* we CAN handle this now, since we have made a closure for it: FIXME *)
  | BEXPR_inj _ -> assert false (* can't handle yet *)

  | BEXPR_ainj _ -> assert false (* can't handle yet *)

  (* we cannot handle this one yet, because we didn't yet make a closure for it: FIXME *)
  | BEXPR_aprj _ -> assert false (* can't handle yet *)

  | BEXPR_expr (s,retyp,e) ->
    begin match s with
    | CS.Virtual -> assert false
    | CS.Identity -> assert false
    | CS.Str s -> ce_atom ("(" ^  s ^ ")")
    | CS.Str_template s ->
      let gen_expr' = gen_expr' syms bsym_table shapes shape_map label_info this [] [] in
          gen_prim_call
            syms
            bsym_table
            shapes shape_map
            (fun t -> t)
            gen_expr'
            s
            []
            e 
            (retyp)
            sr
            (sr)
            "expr"
            "inline_apply" 
            
    end

  | BEXPR_case_index e -> 
(*
    print_endline ("Flx_egen: BEXPR_case_index " ^ sbe bsym_table e);
*)
    Flx_vgen.gen_get_case_index ge' bsym_table syms.Flx_mtypes2.array_sum_offset_table syms.Flx_mtypes2.counter e

  | BEXPR_range_check (e1,e2,e3) ->
     let f,sl,sc,el,ec = Flx_srcref.to_tuple sr in
     let f = ce_atom (Flx_string.escape_of_string '"' f) in
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

  | BEXPR_apply ((BEXPR_compose (f1, f2),_), e) ->
      failwith ("flx_egen: application of composition should have been reduced away")

  | BEXPR_apply ((BEXPR_closure (index,ts),_),a) ->
    print_endline "Compiler bug in flx_egen, application of closure found, should have been factored out!";
    assert false (* should have been factored out *)

  (* application of C function pointer, type
     f: a --> b
  *)

  | BEXPR_apply ( (_,BTYP_cfunction (d,_)) as f,a) ->
(*
print_endline "Apply cfunction";
*)
    begin match d with
    | BTYP_tuple ts ->
      begin match a with
      | BEXPR_tuple xs,_ ->
        let s = String.concat ", " (List.map (fun x -> ge x) xs) in
        ce_atom ( (ge f) ^"(" ^ s ^ ")")
      | _ ->
       failwith "[flx_egen][tuple] can't split up arg to C function yet"
      end
    | BTYP_array (t,BTYP_unitsum n) ->
      let ts = 
       let rec aux ts n = if n = 0 then ts else aux (t::ts) (n-1) in
       aux [] n
      in
      begin match a with
      | BEXPR_tuple xs,_ ->
        let s = String.concat ", " (List.map (fun x -> ge x) xs) in
        ce_atom ( (ge f) ^"(" ^ s ^ ")")
      | _ ->
        failwith "[flx_egen][array] can't split up arg to C function yet"
      end

    | _ ->
      ce_atom ( (ge f) ^"(" ^ ge_arg a ^ ")")
    end

  (* General application*)
  | BEXPR_apply (f,a) ->
(*
print_endline "Apply general";
*)
    ce_atom (
    "("^(ge f) ^ ")->clone()\n      ->apply(" ^ ge_arg a ^ ") /* general apply */"
    )



  | BEXPR_apply_prim (index,ts,arg) ->
(*
print_endline "Apply prim";
*)
    gen_apply_prim
      syms
      bsym_table
      shapes shape_map
      label_info
      this
      sr
      this_vs
      this_ts
      t
      index
      ts
      arg

  | BEXPR_apply_struct (index,ts,a) ->
(*
print_endline "Apply struct";
*)
    let bsym =
      try Flx_bsym_table.find bsym_table index with _ ->
        failwith ("[gen_expr(apply instance)] Can't find index " ^
          string_of_bid index)
    in
    let ts = map tsub ts in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_cstruct (vs,cts,_,variance) ->
      let name = tn (btyp_inst (`Nominal variance,index,ts,Flx_kind.KIND_type)) in
      let struct_field_names = List.map fst cts in
      let handle ps = 
        let initlist = match fst a with
        | BEXPR_tuple xs ->
          let nvlist = List.map2 (fun name value -> name,value) struct_field_names xs in
          let initlist = Flx_util.catmap ", " (fun (name, value) -> "."^name ^ "=" ^ ge value) nvlist in
          initlist
        | _ ->
          let rhs = "(" ^ ge a ^ ").mem_" in
          let nvlist = List.map2 (fun name value -> name,value) struct_field_names (Flx_list.nlist (List.length ps)) in
          let initlist = Flx_util.catmap ", " (fun (name, index) -> "."^name ^ "=" ^ rhs ^ si index) nvlist in
          initlist
        in ce_atom (name ^"{" ^ initlist ^ "}/* apply cstruct*/")
      in

      begin match snd a with
      (* tuple contains initialisers of exact field type of corresponding struct field *)
      | BTYP_tuple ps -> handle ps

      | BTYP_array (v_t,BTYP_unitsum n) ->
        let elts = 
          match fst a with
          | BEXPR_tuple xs -> List.map ge xs
          | _ -> 
            let ca = ge a in
            List.map (fun i -> ca ^".data["^si i^"]" ) (Flx_list.nlist n)
        in
        let initlist = String.concat ", " (List.map2 (fun name elt -> "."^name ^ "=" ^ elt) struct_field_names elts) in
        ce_atom (name ^ "{"^initlist^"}")
       
      | t when List.length cts = 1 ->
        let field_name = fst (List.hd cts) in
        let initlist =  "."^field_name^"="^ ge a in
        ce_atom (name ^"{" ^ initlist ^ "}/* apply cstruct*/")

      | x -> 
        print_endline ("FLx_egen: expected BEXPR_apply_struct with cstruct ctor to have tuple argument, got type " ^
          Flx_print.sbt bsym_table x);
        assert false
      end

    | BBDCL_struct (vs,cts,variance) ->
      let name = tn (btyp_inst (`Nominal variance,index,ts,Flx_kind.KIND_type)) in
      if length cts > 1 then
        (* argument must be an lvalue *)
        ce_atom ("reinterpret<"^ name ^">(" ^ ge a ^ ")/* apply struct */")
      else if length cts = 0 then
        ce_atom (name ^ "()")
      else
        ce_atom (name ^ "(" ^ ge a ^ ")")

    | BBDCL_nonconst_ctor (vs,uidx,udt,cidx,ct,evs, etraint) ->
      (* due to some hackery .. the argument of a non-const
         ctor can STILL be a unit .. prolly cause the stupid
         compiler is checking for voids for these pests,
         but units for sums .. hmm .. inconsistent!
      *)
      let ts = map tsub ts in
      let ct = beta_reduce "flx_egen: nonconst ctor" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts ct) in
      Flx_vgen.gen_make_nonconst_ctor ge' tn syms bsym_table shape_map udt cidx a 
    | _ -> assert false
    end

  | BEXPR_apply_direct (index,ts,a) ->
(*
print_endline ("Apply direct ");
*)
    let bsym = Flx_bsym_table.find bsym_table index in
    let ts = map tsub ts in
    let bsym =
      try Flx_bsym_table.find bsym_table index with _ ->
        failwith ("[gen_expr(apply instance)] Can't find index " ^
          string_of_bid index)
    in
    begin
    (*
    print_endline ("apply closure of "^ Flx_bsym.id bsym );
    print_endline ("  .. argument is " ^ sbe bsym_table a);
    *)
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,_,ps,_,_,_) ->
      (*
      print_endline ("Generating closure[apply direct] of " ^ si index);
      *)
      let the_display =
        let d' =
          map begin fun (i,vslen)->
            "ptr" ^ cpp_instance_name syms bsym_table i (Flx_list.list_prefix ts vslen)
          end (get_display_list bsym_table index)
        in
          if length d' > our_level
          then "this" :: tl d'
          else d'
      in
      let name = cpp_instance_name syms bsym_table index ts in
      (* C function modelling Felix function, one C parameter per 
         component in parameter s-expr
      *)
      if mem `Cfun props
      then begin 
        let prjs = Flx_bparams.get_prjs ps in
        let args = List.map (fun (_,prj) -> match prj with
          | None -> a
          | Some ((_,BTYP_function (_,c)) as prj) ->  print_endline ("bexpr_apply1"); bexpr_apply c (prj,a) 
          | _ -> assert false
          ) prjs
        in
        let s = String.concat ", " (List.map (fun x -> ge x) args) in
        ce_atom ( name ^"(" ^ s ^ ")")
      end else
        ce_atom (
        "(FLX_NEWP("^name^")"^ Flx_gen_display.strd the_display props ^")"^
        "\n      ->apply(" ^ ge_arg a ^ ")"
        )

    | BBDCL_external_fun _ -> assert false
    (*
      ge' (BEXPR_apply_prim (index,ts,a),t)
    *)

    | _ ->
      failwith
      (
        "[gen_expr: apply_direct] Expected '" ^ Flx_bsym.id bsym ^ "' to be generic function instance, got:\n" ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index
      )
    end

  | BEXPR_apply_stack (index,ts,a) ->
(*
print_endline ("Apply stack");
*)
    let bsym = Flx_bsym_table.find bsym_table index in
    let ts = map tsub ts in
    let bsym =
      try Flx_bsym_table.find bsym_table index with _ ->
        failwith ("[gen_expr(apply instance)] Can't find index " ^
          string_of_bid index)
    in
    begin
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,ps,retyp,effects,_) ->
      let display = get_display_list bsym_table index in
      let name = cpp_instance_name syms bsym_table index ts in
      (* C FUNCTION modelliung Felix function, one C param per s-expr component *)
      if mem `Pure props && not (mem `Heap_closure props) then
      begin
        let prjs = Flx_bparams.get_prjs ps in
        let args = List.map (fun (_,prj) -> match prj with
          | None -> a
          | Some ((_,BTYP_function (_,c)) as prj) ->  (* print_endline ("bexpr_apply2"); *) bexpr_apply c (prj,a) 
          | _ -> assert false
          ) prjs
        in
        let s = String.concat "," (List.map (fun x -> ge x) args) in
        let s =
          if mem `Requires_ptf props then
            if String.length s > 0 then "ptf," ^ s
            else "ptf"
          else s
        in
          ce_atom (name ^ "(" ^ s ^ ")")
      end else
        let the_display =
          let d' =
            map (fun (i,vslen)-> "ptr"^ cpp_instance_name syms bsym_table i (Flx_list.list_prefix ts vslen))
            display
          in
            if length d' > our_level
            then "this" :: tl d'
            else d'
        in
        let s =
          name^ Flx_gen_display.strd the_display props
          ^
          "\n      .apply(" ^ ge_arg a ^ ") /* apply_stack */"
        in ce_atom s

    | _ ->
      failwith
      (
        "[gen_expr: apply_stack] Expected '" ^ Flx_bsym.id bsym ^ "' to be generic function instance, got:\n" ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index
      )
    end


  | BEXPR_tuple_cons ((eh',th' as xh'), (et', tt' as xt')) -> 
    print_endline ("Flx_egen: Tuple_cons (" ^ sbe bsym_table xh' ^ " ,, " ^ sbe bsym_table xt');
    print_endline ("Type " ^ sbt bsym_table t);
    print_endline ("Head Type " ^ sbt bsym_table th');
    print_endline ("Tail Type " ^ sbt bsym_table tt');
    print_endline ("Should be eliminated");
    assert false;
    let tts = match tt' with
    | BTYP_tuple tts -> tts
    | BTYP_array (t,BTYP_unitsum n) -> 
      let tts = ref [] in
      for i = 0 to n - 1 do
        tts := t :: !tts;
      done;
      !tts
    | _ -> assert false
    in 
    let n = List.length tts in
(* NOTE: this is a hack! Doen't handle arrays. The code generated here
   is ALWAYS correct BUT we may have neglected to create a tuple
   constructor, because the scan for BEXPR_tuple is already done. 
*)
    let es = match et' with
    | BEXPR_tuple es -> es 
    | _ -> 
      let counter = ref 0 in
      List.map 
        (fun t-> 
          let i = !counter in incr counter;
          bexpr_get_n t i xt'
        )
        tts
    in
    let es = xh' :: es in
    let e = bexpr_tuple t es in
(*
print_endline ("Normalised expression " ^ sbe bsym_table e);
print_endline ("Normalised type " ^ sbt bsym_table t);
*)
    ge' e

  | BEXPR_tuple_snoc ((et', tt' as xt'),(eh',th' as xh') ) ->
    print_endline ("Flx_egen: Tuple_snoc (" ^ sbe bsym_table xh' ^ " , " ^ sbe bsym_table xt');
    print_endline ("Type " ^ sbt bsym_table t);
    print_endline ("Head Type " ^ sbt bsym_table th');
    print_endline ("Tail Type " ^ sbt bsym_table tt');
    print_endline ("Should be eliminated");
    assert false;
    let tts = match tt' with
    | BTYP_tuple tts -> tts
    | BTYP_array (t,BTYP_unitsum n) -> 
      let tts = ref [] in
      for i = 0 to n - 1 do
        tts := t :: !tts;
      done;
      !tts
    | _ -> assert false
    in 
    let n = List.length tts in
    let es = match et' with
    | BEXPR_tuple es -> es 
    | _ -> 
      let counter = ref 0 in
      List.map 
        (fun t-> 
          let i = !counter in incr counter;
          bexpr_get_n t i xt'
        )
        tts
    in
    let es = es @[xh'] in
    let e = bexpr_tuple t es in
    ge' e

  | BEXPR_tuple_head (e',t' as x') ->
    print_endline ("Flx_egen: WARNING Tuple head of expression " ^ sbe bsym_table x');
    print_endline ("Type " ^ sbt bsym_table t');
    print_endline ("Should be eliminated");
(*
    print_endline ("Normalised Type " ^ sbt bsym_table t');
    print_endline ("Tail Type " ^ sbt bsym_table t);
*)
    begin match t' with 
    | BTYP_tuple [] -> assert false
    | BTYP_tuple ts -> 
      let eltt = List.hd ts in
      let n = List.length ts in
      ge' (bexpr_get_n eltt 0 x')

    | BTYP_array (eltt,BTYP_unitsum n) ->
      assert (n > 0);
      ge' (bexpr_get_n eltt 0 x')
 
    | _ -> 
      print_endline ("Expected head to apply to a tuple, got " ^ 
        sbt bsym_table t');
      assert false
    end

  | BEXPR_tuple_last (e',t' as x') ->
assert false;
    begin match t' with 
    | BTYP_tuple [] -> assert false
    | BTYP_tuple ts -> 
      let eltt = List.hd (List.rev ts) in
      let n = List.length ts in
      ge' (bexpr_get_n eltt (n-1) x')

    | BTYP_array (eltt,BTYP_unitsum n) ->
      assert (n > 0);
      ge' (bexpr_get_n eltt (n-1) x')
 
    | _ -> 
      print_endline ("Expected tuple_last to apply to a tuple, got " ^ 
        sbt bsym_table t' );
      assert false
    end

  | BEXPR_tuple_tail (e',t' as x') ->
    print_endline ("Flx_egen: Tuple tail of expression " ^ sbe bsym_table x');
    print_endline ("Type " ^ sbt bsym_table t');
    print_endline ("Should be eliminated");
(*
    assert false;
*)
(*
    print_endline ("Normalised Type " ^ sbt bsym_table t');
    print_endline ("Tail Type " ^ sbt bsym_table t);
*)
    let ts = match t' with 
    | BTYP_tuple ts ->  ts
    | BTYP_array (t, BTYP_unitsum n) -> 
      let tts = ref [] in
      for i = 0 to n - 1 do
        tts := t :: !tts;
      done;
      !tts

    | _ -> 
      print_endline ("Expected tail to be tuple, got " ^ 
        sbt bsym_table t');
      assert false
    in
    let n = List.length ts in
    let counter = ref 0 in
    let es = 
      List.map (fun t-> 
        incr counter; 
        bexpr_get_n t (!counter) x'
     ) 
     (List.tl ts) 
    in
    let tail = match es with [x] -> x | es -> bexpr_tuple t es in
    ge' tail

  | BEXPR_tuple_body (e',t' as x') ->
assert false;
    let ts = match t' with 
    | BTYP_tuple ts ->  ts
    | BTYP_array (t, BTYP_unitsum n) -> 
      let tts = ref [] in
      for i = 0 to n - 1 do
        tts := t :: !tts;
      done;
      !tts

    | _ -> 
      print_endline ("Expected tuple_body to be tuple, got " ^ 
        sbt bsym_table t' );
      assert false
    in
    let n = List.length ts in
    let counter = ref 0 in
    let es = 
      List.map (fun t-> 
        let x = bexpr_get_n t (!counter) x' in
        incr counter;
        x
     ) 
     (List.rev (List.tl (List.rev ts))) 
    in
    let body = match es with [x] -> x | es -> bexpr_tuple t es in
    ge' body


  | BEXPR_match_case (n,((e',t') as e)) ->
    let t' = beta_reduce "flx_egen get_n: match_case" syms.Flx_mtypes2.counter bsym_table sr t' in
    let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
    let seq = syms.Flx_mtypes2.counter in
    let x = Flx_vgen.gen_get_case_index ge' bsym_table array_sum_offset_table seq e in
    ce_infix "==" x (ce_atom (si n))

  | BEXPR_not (BEXPR_match_case (n,((e',t') as e)),_) ->
    let t' = beta_reduce "flx_egen: not" syms.Flx_mtypes2.counter bsym_table sr t' in
    let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
    let seq = syms.Flx_mtypes2.counter in
    let x = Flx_vgen.gen_get_case_index ge' bsym_table array_sum_offset_table seq e in
    ce_infix "!=" x (ce_atom (si n))

  | BEXPR_cond (c,t,f) -> ce_cond (ge' c) (ge' t) (ge' f)

  | BEXPR_not e -> ce_prefix "!" (ge' e)

  | BEXPR_rptsum_arg (e) ->
    Flx_vgen.gen_get_rptsum_arg ge' tn bsym_table e

  | BEXPR_case_arg (n,e) ->
(*
    print_endline ("flx_egen[ge_carg]: Decoding nonconst ctor " ^ si n ^ " of value " ^ sbe bsym_table e);
    print_endline ("flx_egen[ge_carg]: type " ^ sbt bsym_table t);
*)
    Flx_vgen.gen_get_case_arg ge' tn bsym_table n e
    (*
    begin match t with (* t is the result of the whole expression *)
    | BTYP_function _ ->
      let cast = tn t in
      ce_cast cast (ce_dot (ge' e) "data")
    | _ ->
      let cast = tn t ^ "*" in
      ce_prefix "*" (ce_cast cast (ce_dot (ge' e) "data"))
    end
    *)

  | BEXPR_deref ((BEXPR_ref (index,ts)),BTYP_ptr (_,t,[])) ->
    ge' (bexpr_varname t (index,ts))

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

  | BEXPR_likely e ->
    begin match t with
    | BTYP_unitsum 2 ->
      ce_atom ("FLX_LIKELY("^ge e^")")
    | _ -> ge' e
    end

  | BEXPR_unlikely e ->
    begin match t with
    | BTYP_unitsum 2 ->
      ce_atom ("FLX_UNLIKELY("^ge e^")")
    | _ -> ge' e
    end

  | BEXPR_new e ->
    let ref_type = tn t in
    let _,t' = e in
    let pname = direct_shape_of syms bsym_table shape_map tn t' in
    let typ = tn t' in
    let frame_ptr = ce_new 
        [ ce_atom "*ptf-> gcp"; ce_atom pname; ce_atom "true"] 
        typ 
        [ge' e]
    in
    ce_cast ref_type frame_ptr

(* class new constructs an object _in place_ on the heap, unlike ordinary
 * new, which just makes a copy of an existing value.
 *)
  | BEXPR_class_new (t,a) ->
    let ref_type = tn t in
print_endline ("Generating class new for t=" ^ ref_type);
    let args = match a with
    | BEXPR_tuple [],_ -> []
    | BEXPR_tuple es,_ -> map ge' es
    | _ -> [ge' a]
    in
    ce_new [ce_atom "*ptf-> gcp";ce_atom (ref_type^"_ptr_map"); ce_atom "true"] ref_type args

  | BEXPR_literal {Flx_literal.c_value=v} ->
    ce_atom v
    (*
    let t = tn t in
    ce_cast t  (ce_atom (cstring_of_literal v))
    *)

  (* A case tag: this is a variant value for any variant case
   * which has no (or unit) argument: can't be used for a case
   * with an argument. This is here for constant constructors,
   * particularly enums.
   *)
  | BEXPR_case (v,t') -> (* assert false; *)
(*
    print_endline ("making constant ctor");
*)
    let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
    let seq = syms.Flx_mtypes2.counter in
    let clv = Flx_vgen.gen_make_const_ctor bsym_table array_sum_offset_table seq ge' (e,t) in
(*
    print_endline ("vgen:BEXPR_case: rendered lineralised index .. C index = " ^ string_of_cexpr clv);
*)
    clv


  | BEXPR_varname (index,ts') ->
    let bsym_parent, bsym =
      try Flx_bsym_table.find_with_parent bsym_table index
      with Not_found ->
        syserr sr ("[gen_expr(name)] Can't find <" ^ string_of_bid index ^ ">")
    in
    let ts = map tsub ts' in
    begin match Flx_bsym.bbdcl bsym with
      | BBDCL_val (_,BTYP_function (BTYP_void,_),`Val) 
      | BBDCL_val (_,BTYP_function (BTYP_void,_),`Once)  ->
          let ptr = (get_var_ref syms bsym_table this index ts) in
          ce_call (ce_arrow (ce_atom ptr) "apply") []

      | BBDCL_val (_,t,_) ->
          ce_atom (get_var_ref syms bsym_table this index ts)

      | BBDCL_const_ctor (vs,uidx,udt, ctor_idx, evs, etraint) ->
        let array_sum_offset_table = syms.Flx_mtypes2.array_sum_offset_table in
        let seq = syms.Flx_mtypes2.counter in
        Flx_vgen.gen_make_const_ctor bsym_table array_sum_offset_table seq ge' (e,t)

      | BBDCL_external_const (props,_,_,ct,_) ->
        if mem `Virtual props then
          print_endline ("Instantiate virtual const " ^ Flx_bsym.id bsym ^ "["^catmap "," (sbt bsym_table) ts^"]")
        ;
        begin match ct with
        | CS.Identity -> syserr sr ("Nonsense Idendity const" ^ Flx_bsym.id bsym)
        | CS.Virtual -> clierr2 sr (Flx_bsym.sr bsym) ("Instantiate virtual const " ^ Flx_bsym.id bsym)
        | CS.Str c
        | CS.Str_template c when c = "#srcloc" ->
           let f, l1, c1, l2, c2 = Flx_srcref.to_tuple sr in
           ce_atom ("flx::rtl::flx_range_srcref_t(" ^
             Flx_string.escape_of_string '"' f ^ "," ^
             si l1 ^ "," ^
             si c1 ^ "," ^
             si l2 ^ "," ^
             si c2 ^ ")"
           )

        | CS.Str c when c = "#this" ->
          begin match bsym_parent with
          | None -> clierrx "[flx_cpp_backend/flx_egen.ml:1173: E285] " sr "Use 'this' outside class"
          | Some p ->
            let name = cpp_instance_name syms bsym_table p ts in
            (*
            print_endline ("class = " ^ si p ^ ", instance name = " ^ name);
            *)
            ce_atom("ptr"^name)
          end

        | CS.Str c
        | CS.Str_template c when c = "#memcount" ->
          begin match ts with
          | [t] when Flx_btype.islinear_type t -> ce_atom (string_of_int (Flx_btype.int_of_linear_type bsym_table t))
          | [BTYP_array (_,BTYP_unitsum n)] -> ce_atom (si n)

          | [BTYP_sum ls] 
          | [BTYP_tuple ls] -> let n = length ls in ce_atom (si n)
          | [BTYP_inst (`Nominal _, i,_,_)] ->
            begin match Flx_bsym_table.find_bbdcl bsym_table i with
              | BBDCL_struct (_,ls,_) -> let n = length ls in ce_atom (si n)
              | BBDCL_cstruct (_,ls,_,_) -> let n = length ls in ce_atom (si n)
              | BBDCL_union (_,ls,_) -> let n = length ls in ce_atom (si n)
              | _ ->
                clierrx "[flx_cpp_backend/flx_egen.ml:1196: E286] " sr (
                  "#memcount function requires type with members to count, got: " ^
                  sbt bsym_table (hd ts)
                )
            end
          | _ ->
            clierrx "[flx_cpp_backend/flx_egen.ml:1202: E287] " sr (
              "#memcount function requires type with members to count, got : " ^
              sbt bsym_table (hd ts)
            )
          end
        | CS.Str_template c when c = "#arrayindexcount" ->
          (* we do hacked up processing of sums of unitsums here, to allow for
             the implicit flattening of array indices. 
             Also we allow a list in preparation for rank K arrays.
          *)
          begin try
            let n = fold_left (fun acc elt -> acc * Flx_btype.int_of_linear_type bsym_table elt) 1 ts in
            ce_atom (si n)
          with Invalid_int_of_unitsum ->
            clierrx "[flx_cpp_backend/flx_egen.ml:1216: E288] " sr (
              "#arrayindexcountfunction requires type which can be used as array index, got: " ^
              catmap "," (sbt bsym_table) ts
            )
          end
        | CS.Str c -> ce_expr "expr" c
        | CS.Str_template c ->
          let ts = map tn ts in
          csubst shapes sr (Flx_bsym.sr bsym) c 
            ~arg:(fun () -> ce_atom "Error") ~args:[] 
            ~typs:[] ~argtyp:"Error" ~retyp:"Error" 
            ~gargs:ts 
            ~prec:"expr" 
            ~argshape:"Error" 
            ~argshapes:["Error"] 
            ~display:["Error"] 
            ~gargshapes:["Error"]
            ~name:(Flx_bsym.id bsym)
        end

      (* | BBDCL_fun (_,_,([s,(_,BTYP_void)],_),_,[BEXE_fun_return e]) -> *)
      | BBDCL_fun (_,_,(Slist [],_),_,_,[BEXE_fun_return (_,e)]) ->
        ge' e

      | BBDCL_cstruct _
      | BBDCL_struct _
      | BBDCL_fun _
      | BBDCL_external_fun _ ->
         syserr sr
         (
           "[gen_expr: name] Open function '" ^
           Flx_bsym.id bsym ^ "'<" ^ string_of_bid index ^
           "> in expression (closure required)"
         )
      | _ ->
        syserr sr
        (
          "[gen_expr: name] Cannot use this kind of name '"^
          Flx_bsym.id bsym ^ "' in expression"
        )
    end

  | BEXPR_identity_function t -> assert false
 
  | BEXPR_closure (index,ts') ->
(*
    print_endline ("Generating closure of " ^ si index);
*)
    let bsym =
      try Flx_bsym_table.find bsym_table index with _ ->
        failwith ("[gen_expr(name)] Can't find index " ^ string_of_bid index)
    in
    (*
    Should not be needed now ..
    let ts = adjust_ts syms sym_table index ts' in
    *)
    let ts = map tsub ts' in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,_,_,_,_,_) ->
      let the_display =
        let d' =
          map begin fun (i,vslen) ->
            "ptr" ^ cpp_instance_name syms bsym_table i (Flx_list.list_prefix ts vslen)
          end (get_display_list bsym_table index)
        in
          if length d' > our_level
          then "this" :: tl d'
          else d'
      in
      let name = cpp_instance_name syms bsym_table index ts in
      if mem `Cfun props then ce_atom name
      else
        ce_atom (
        "(FLX_NEWP("^name^")" ^ Flx_gen_display.strd the_display props ^")"
        )

    | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) ->
(*
      print_endline "Mapping closure of callback to C function pointer";
*)
      ce_atom (Flx_bsym.id bsym)

    | BBDCL_cstruct _
    | BBDCL_struct _
    | BBDCL_external_fun _ ->
      failwith ("[gen_expr: closure] Can't wrap primitive proc, fun, or " ^
        "struct '" ^ Flx_bsym.id bsym ^ "' yet")
    | _ ->
      failwith ("[gen_expr: closure] Cannot use this kind of name '" ^
      Flx_bsym.id bsym ^ "' in expression")
    end

  (* downgraded semantics, rvalue reference is same as pointer now *)
  | BEXPR_rref (index,ts') 
  | BEXPR_vref (index,ts') 
  | BEXPR_wref (index,ts') 
  | BEXPR_ref (index,ts') ->
    let ts = map tsub ts' in
    let ref_type = tn t in
    (*
    let frame_ptr, var_ptr =
      match t with
      | BTYP_tuple [] -> "NULL","0"
      | _ ->

        let parent = match Flx_bsym_table.find bsym_table index with _,parent,sr,_ -> parent in
        if Some this = parent &&
        (
          let props = match entry with
            | BBDCL_fun (props,_,_,_,_) -> props
            | _ -> assert false
          in
          mem `Pure props && not (mem `Heap_closure props)
        )
        then
          "NULL","&"^get_var_ref syms bsym_table this index ts ^"-NULL"
        else
          get_var_frame syms bsym_table this index ts,
          "&" ^ get_var_ref syms bsym_table this index ts
    in
    let reference = ref_type ^
      "(" ^ frame_ptr ^ ", " ^ var_ptr ^ ")"
    in
    ce_atom reference
    *)

    ce_cast ref_type
    begin match t with
      | BTYP_tuple [] -> ce_atom "0"
      | _ ->
        let v = get_var_ref syms bsym_table this index ts in
        ce_prefix "&" (ce_atom v)
    end

  (* Hackery -- we allow a constructor with no
     arguments to be applied to a unit anyhow
  *)

  | BEXPR_reinterpret_cast ((srcx,srct) as srce,dstt) -> 
    ce_atom ("reinterpret<"^tn dstt^">("^ge srce^")")

  | BEXPR_coerce ((srcx,srct) as srce,dstt) -> 
    if Flx_typeeq.type_eq (sbt bsym_table) syms.Flx_mtypes2.counter srct dstt
    then ge' srce
    else
(*
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
*)
    begin match srct,dstt with

    (* assume this coercion is implicit in C *)
    | BTYP_ptr (`RW,t1,_),BTYP_ptr(`R,t2,_) when t1 = t2 -> ce_atom ("static_cast<"^tn dstt^">("^ge srce^")")
    | BTYP_ptr (`RW,t1,_),BTYP_ptr(`V,t2,_) when t1 = t2 -> ce_atom ("static_cast<"^tn dstt^">("^ge srce^")")
    | BTYP_ptr (`RW,t1,_),BTYP_ptr(`W,t2,_) when t1 = t2 -> ge' srce

    | BTYP_variant ls, BTYP_variant rs -> 
      print_endline ("Found variant coercion, should have been be eliminted");
      print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
      assert false; 
(*
print_endline ("Coercion is variant to variant, ignore");

THIS IS NO LONGER CORRECT. UNFORTUNATELY ITS A F'ING MESS TO DO RIGHT.
The problem is we HAVE to coerce the input argument's constructor 
argument value, which actually creates a new variant. To do this
we have to match on all possible cases of the argument's type
to find the one we need, get the value out of it, convert it,
and rebuild the variant with the same constructor name, but
a the new value of the new coerced type. The coercion is covariant
as for records. 

There is ALSO a problem here in that there is no encoding in the
variant of the sequence number of the variant, for variants of 
the same argument type. The type IS encoded at run time by virtue
of the hashcode.

There's one more thing, an optimisation: if we only have width subtyping,
there's no need to do anything at all due to the universal representation.
This also applies to covariant argument width subtyping on variants
recursively.
*)
(*
      (* check for the special case where the argument constructors all
      have the same type as the corresponding parameters
      *)
      begin try 
        List.iter (fun (name, ltyp) ->
          (* find ALL the parameter constructors of the same name
          at least one of them has to have the same type *)
          begin try List.iter (fun (rname, rtyp) ->
            if name = rname 
            &&  Flx_typeeq.type_eq (sbt bsym_table) syms.Flx_mtypes2.counter ltyp rtyp  
            then raise Vfound
            ) rs;
            raise Not_found
          with Vfound -> () 
          end
        ) ls; 
        ge' srce (* safe, already checked, universal rep *)
      with 
      | Not_found -> 
      (* OK, now lets handle the special case where there's no choice
         because the target has only ONE constructor with a given name
         In fact, I am going to cheat, and just use the first name
         every time, which will crash is there isn't actually 
         a coercion for it .. we could then fix that by trying to
         generate the coercion, and if that fails, try the next
         case. What we really should do may be to pick the most
         general target type, but even that seems problematic at the moment.
         The first case is not bad, because there's currently no way
         to pattern match variant types with duplicated constructors,
         to make that work you would have to specify which one with
         the type. If there were duplicates with the same type,
         you'd have to go even further and allow a pattern match
         to repeat the same case. The first branch uses seq 0,
         the second seq 1, etc. Then the sequence has to also be
         encoded in the tag, and, the compiler has to analyse the
         pattern match and add in the sequence number, or, provide
         syntax for the user to do so.
      *)
        begin (* check the target for uniqueness of names *)
          let counts = Hashtbl.create 97 in
          let get_rel_seq name = 
            let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
            Hashtbl.replace counts name n;
            n
          in
          List.iter (fun (name,_) -> ignore (get_rel_seq name)) rs;
          Hashtbl.iter (fun name count -> 
            if count <> 0 then 
              print_endline ("Warning: Variant coercion target duplicates name " ^ 
                name ^ ", will use first one for coercion")
          ) counts;
          let coercions = List.map (fun (name, ltyp) ->
            let condition = bexpr_match_variant (name,srce) in
            let extracted = bexpr_variant_arg ltyp (name,srce) in
            (* just use first one .. later we could try next one if it fails *)
            let rtyp = List.assoc name rs in
            let coerced = bexpr_coerce (extracted,rtyp) in
            let new_variant = bexpr_variant dstt (name, coerced) in 
            condition, new_variant
            ) ls 
          in
          let rec chain cs = 
            match cs with
            | (cond,variant) :: second :: tail ->
               bexpr_cond cond variant (chain (second :: tail))
            | [_, variant] -> variant (* dubious, skipping check due to exhaustion *)
            | [] -> assert false
          in 
          let result = chain coercions in
          ge' result
        end
      end
*)
    | BTYP_record ls, BTYP_record rs ->
      print_endline ("Found record coercion, should have been be eliminted");
      print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
      assert false;
(*
      (* count duplicate fields in target *)
      let counts = Hashtbl.create 97 in
      let get_rel_seq name = 
        let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
        Hashtbl.replace counts name n;
        n
      in
      begin try
        let prjs = List.map (fun ( name,rtyp) -> name,
          begin  
            let rel_seq = get_rel_seq name in
(*
print_endline ("Dst Field " ^ name ^ ", rel_seq=" ^ string_of_int rel_seq ^ ",type=" ^sbt bsym_table rtyp);
*)
            let maybe = find_seq name rel_seq ls in
            match maybe with
            | None -> print_endline ("Missing field " ^ name); assert false 
            | Some (idx,ltyp) ->
(*
print_endline ("src field idx=" ^ string_of_int idx ^ ", type=" ^ sbt bsym_table ltyp);          
*)
            let prj = bexpr_prj idx srct ltyp in
            let raw_dst = bexpr_apply ltyp (prj,srce) in
            let final_dst = bexpr_coerce (raw_dst,rtyp) in
            final_dst
          end
          ) 
          rs 
        in
        let r = bexpr_record prjs in
        ge' r 
      with _ -> 
       failwith ("Bad record coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
      end
*)
    (* C should handle integer to compact linear type ok *)
    (* could be some other type to unit sum but checking for
       an integer is nasty
    *)
    | t, _ when clt t ->
(*
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
*)
      ge' srce


    | _, t when clt t ->
(*
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
*)
      ge' srce

    | BTYP_array (_,t), _ when clt t ->
(*
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
*)
      ce_atom ("reinterpret<"^tn dstt^">("^ge srce^")")


    | _, BTYP_array (_,t) when clt t ->
(*
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
*)
      ce_atom ("reinterpret<"^tn dstt^">("^ge srce^")")



    | BTYP_instancetype _,_ -> ce_atom ("reinterpret<"^tn dstt^">("^ge srce^")")

    | BTYP_fix(0,_),_ -> ce_atom ( ge srce)

    | _ -> 
print_endline ("Handling coercion in egen " ^ sbt bsym_table srct ^ " ===> " ^ sbt bsym_table dstt);
print_endline ("Coercion uses reinterpret cast!");
     ce_atom ("reinterpret<"^tn dstt^">("^ge srce^")")
    end

  | BEXPR_compose _ -> failwith "Flx_egen:Can't handle closure of composition yet"

  | BEXPR_record es ->
    let es = map snd es in
    let ctyp = tn t in

    (* keep track of duplicates with magic names *)
    let dups = Hashtbl.create (List.length es) in
    let name s = 
      if Hashtbl.mem dups s then
        let count = Hashtbl.find dups s in
        Hashtbl.replace dups s (count+1);
        "_" ^ s ^ "_" ^ string_of_int count
      else begin
        Hashtbl.add dups s 1;
        s
      end
    in
        
 
    ce_atom (
    ctyp ^ "(" ^
      fold_left
      (fun s e ->
        match snd e with
        | BTYP_tuple [] -> s (* unit arguments in records are elided *)
        | _ -> 
        let s = name s in
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
    print_endline ("Construct tuple " ^ sbe bsym_table (e,t));
*)
    if clt t then begin
      print_endline ("Construct tuple of linear type " ^ sbe bsym_table (e,t));
      print_endline ("type " ^ sbt bsym_table t);
      let sidx = Flx_ixgen.cal_symbolic_compact_linear_value bsym_table (e,t) in
(*
print_endline ("egen:BEXPR_tuple: Symbolic index = " ^ Flx_ixgen.print_index bsym_table sidx );
*)
      let cidx = Flx_ixgen.render_compact_linear_value bsym_table ge' array_sum_offset_table seq sidx in
(*
print_endline ("egen:BEXPR_tuple: rendered lineralised index .. C index = " ^ string_of_cexpr cidx);
*)
      cidx 
    end
    else
    (* just apply the tuple type ctor to the arguments *)
    begin match t with
    | BTYP_array (t',BTYP_unitsum n) ->
(*
print_endline "Construct tuple, subkind array";
*)
      let t'' = btyp_tuple (map (fun _ -> t') (Flx_list.nlist n)) in
      let ctyp = raw_typename t'' in
      ce_atom (
        ctyp ^ "(" ^
        List.fold_left begin fun s e ->
          let x = ge_arg e in
(*
print_endline ("Construct tuple, subkind array, component x=" ^ x);
*)
          if String.length x = 0 then s else
          s ^
          (if String.length s > 0 then ", " else "") ^
          x
        end "" es
        ^
        ")"
      )

    | BTYP_tuple _ ->
(*
print_endline "Construct tuple, subkind tuple";
*)
      let ctyp = tn t in
      ce_atom (
        ctyp ^ "(" ^
        List.fold_left begin fun s e ->
          let x = ge_arg e in
(*
print_endline ("Construct tuple, subkind tuple, component x=" ^ x);
*)
          if String.length x = 0 then s else
          s ^
          (if String.length s > 0 then ", " else "") ^
          x
        end "" es
        ^
        ")"
      )
    | _ -> assert false
    end

(** Code generate for the BEXPR_apply_prim variant. *)
and gen_apply_prim
  syms
  bsym_table
  (shapes: Flx_set.StringSet.t ref)
  shape_map
  label_info
  this
  sr
  this_vs
  this_ts
  t
  index
  ts
  ((arg,argt) as a)
=
  let gen_expr' = gen_expr' syms bsym_table shapes shape_map label_info this this_vs this_ts in
  let beta_reduce calltag vs ts t =
    beta_reduce calltag syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t)
  in
  let cpp_typename t = cpp_typename
    syms
    bsym_table
    (beta_reduce "flx_egen gen_apply_prim: cpp_typename" this_vs this_ts t)
  in
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("[gen_expr(apply instance)] Can't find index " ^
        string_of_bid index)
  in
  let id = Flx_bsym.id bsym in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_external_fun (_,vs,_,retyp,_,prec,kind) ->
      if length vs <> length ts then
      failwith
      (
        "[get_expr:apply closure of fun] function " ^
        Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
        ", wrong number of args, expected vs = " ^
        si (length vs) ^
        ", got ts=" ^
        si (length ts)
      );
      begin match kind with
      | `Code CS.Identity -> gen_expr' sr a
      | `Code CS.Virtual -> syserr sr ("[egen] Not expecting pure virtual primitive " ^ id)
      | `Code (CS.Str s) -> ce_expr prec s
      | `Code (CS.Str_template s) ->
          gen_prim_call
            syms
            bsym_table
            shapes shape_map
            (beta_reduce "flx_egen: Code" this_vs this_ts)
            gen_expr'
            s
            (List.map (beta_reduce "flx_egen: Code2 " this_vs this_ts) ts)
            (arg, beta_reduce "flx_egen: code2" this_vs this_ts argt)
            (beta_reduce "flx_egen: code4" vs ts retyp)
            sr
            (Flx_bsym.sr bsym)
            prec
            id
      | `Callback (_,_) ->
          assert (retyp <> btyp_void ());
          gen_prim_call
            syms
            bsym_table
            shapes shape_map
            (beta_reduce "flx_egen: callback" this_vs this_ts)
            gen_expr'
            (Flx_bsym.id bsym ^ "($a)")
            (List.map (beta_reduce "flx_egen: callback2" this_vs this_ts) ts)
            (arg, beta_reduce "flx_egen: callback3" this_vs this_ts argt)
            (beta_reduce "flx_egen: callback4" vs ts retyp)
            sr
            (Flx_bsym.sr bsym)
            "atom"
            id
      end

  (* but can't be a Felix function *)
  | _ ->
      failwith
      (
        "[gen_expr: apply prim] Expected '" ^ Flx_bsym.id bsym ^
        "' to be primitive function instance, got:\n" ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index
      )

and gen_expr syms bsym_table shapes shape_map label_info this vs ts sr e : string =
  let e = Flx_bexpr.reduce e in
  let s =
    try gen_expr' syms bsym_table shapes shape_map label_info this vs ts sr e
    with Unknown_prec p -> clierrx "[flx_cpp_backend/flx_egen.ml:1561: E289] " sr
    ("[gen_expr] Unknown precedence name '"^p^"' in " ^ sbe bsym_table e)
  in
  string_of_cexpr s




