open Flx_btype
open Flx_bbdcl
open Flx_cexpr
open Flx_ctypes
open Flx_vrep
open Flx_print
open Flx_btype_subst

let si = string_of_int
exception Found_type of Flx_btype.t

let nth lst idx = 
  try List.nth lst idx 
  with _ -> failwith ("[flx_vgen] List.nth failed, list length=" ^ 
    string_of_int (List.length lst) ^
    ", index sought=" ^string_of_int idx)


let check_case_index bsym_table t i =
  let n = cal_variant_cases bsym_table t in
  assert (0 <= i && i < n)

(* get the index of a variant value for purpose of matching cases *)
let gen_get_case_index (ge:Flx_bexpr.t -> cexpr_t) bsym_table array_sum_offset_table seq e: cexpr_t  =
  let _,ut = e in
  let rep = cal_variant_rep bsym_table ut in
  match rep with
  | VR_self -> ce_atom "0"
  | VR_clt -> 
    begin match ut with
    | BTYP_tuple [] -> ce_atom "0"


    (* NOTE: the index of a sum can be a tuple .. *)
    | BTYP_compacttuple _ (* delegates to case BEXPR_compacttuple *)
    | BTYP_unitsum _ -> ge e (* circular?? *)

    | BTYP_compactsum ts 
    | BTYP_sum ts ->  
(*
print_endline ("Gen_get_case_index " ^ Flx_print.string_of_bound_expression bsym_table e);
print_endline ("Gen_get_case_index, type = "  ^ sbt bsym_table ut);
print_endline ("Gen_get_case_index, rep = " ^ Flx_vrep.string_of_variant_rep rep);
*)
      let series = Flx_ixgen.get_array_sum_offset_values bsym_table ts in
      let series = List.tl series in (* n terms now *)
      let n = List.length ts in
      let s2 = Flx_list.nlist (n - 1) in (* n terms *)
      let series =List.rev (List.combine series s2) in (* n - 1 terms *)
(*
      print_endline ("Array sum offset values for " ^ sbt bsym_table ut ^ "\n");
      let j = ref 0 in
      List.iter (fun (i,k) -> print_endline ("pos " ^ si !j ^ " lt " ^si i ^ " pos " ^ si k); incr j) (List.rev series);
*)
      let x = ce_atom "x" in
      let v = 
        List.fold_left (fun acc (lt, pos) ->
          let cond = ce_infix "<" x (ce_int lt) in
          ce_cond cond (ce_int pos) acc
        )
        (ce_int (n - 1))
        series 
      in
      let v = ce_letin "x" "::flx::rtl::cl_t" (ge e) v in
(*
      print_endline ("Symbolc value = " ^ string_of_cexpr v);
*)
      v

    | BTYP_compactrptsum (rpt,base)
    | BTYP_rptsum (rpt,base) ->
      let size = sizeof_linear_type bsym_table base in
      ce_infix "/" (ge e) (ce_int size)

    | _ -> assert false
    end  

  | VR_int -> ge e
  | VR_nullptr -> ce_call (ce_atom "FLX_VNI") [ge e]
  | VR_packed -> ce_call (ce_atom "FLX_VI") [ge e]
  | VR_uctor -> ce_dot (ge e) "variant"

(* helper to get the argument type of a non-constant variant constructor *)
let cal_case_type bsym_table n t : Flx_btype.t =
(*
print_endline "cal_case_type";
*)
  match unfold "cal_case_type" t with
  | BTYP_unitsum _ -> Flx_btype.btyp_tuple []
  | BTYP_rptsum (_,k) -> k
  | BTYP_compactrptsum (_,k) -> k
  | BTYP_sum ls -> nth ls n
  | BTYP_compactsum ls -> nth ls n
  | BTYP_variant ls -> 
    let ct = 
      try 
        List.iter (fun (s,ct)-> if vhash (s,ct) = n then raise (Found_type ct)) ls; 
print_endline ("Cannot find variant with tag " ^ string_of_int n ^ " in " ^
Flx_print.sbt bsym_table t);
        assert false 
      with Found_type ct -> ct 
    in
    ct

  | BTYP_inst (`Nominal _, _, i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    let sr = Flx_bsym.sr bsym in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,cts,_) -> 
      let ct = 
        try 
          List.iter (fun (s,i,evs,ct,_,_)-> if i = n then raise (Found_type ct)) cts; 
          assert false 
        with Found_type ct -> ct 
      in
      let ct = tsubst sr bvs ts ct in (* eliminate type variables *)
      ct
    | _ -> assert false
    end
  | _ -> assert false

let gen_get_arg ge tn bsym_table ct ut (e:Flx_bexpr.t) : cexpr_t =
  let cast = tn ct in
  match cal_variant_rep bsym_table ut with
  | VR_self -> ge e
  | VR_clt ->  
    begin match ut with
    | BTYP_unitsum n -> ce_atom "0" (* unit *)
    | BTYP_compactsum ts
    | BTYP_sum ts ->  
(*
print_endline ("compact linear type] Gen_get_arg value=" ^ sbe bsym_table e);
print_endline ("compact linear type] Gen_get_arg variant type = " ^ sbt bsym_table ut);
print_endline ("compact linear type] Gen_get_arg constructor arg type = " ^ sbt bsym_table ct);
*)
      let series = Flx_ixgen.get_array_sum_offset_values bsym_table ts in
      let s2 = List.tl series in
      let last = List.hd (List.rev series) in
      let series = List.rev (List.tl (List.rev series)) in
      let series = List.rev (List.combine s2 series) in
(*
      print_endline ("Array sum offset values for " ^ sbt bsym_table ut ^ "\n");
      let j = ref 0 in
      List.iter (fun (i,k) -> print_endline ("pos " ^ si !j ^ " lt " ^si i ^ " subtractand " ^ si k); incr j) (List.rev series);
*)
      let x = ce_atom "x" in
      let v = 
        List.fold_left (fun acc (lt, sub) ->
          let cond = ce_infix "<" x (ce_int lt) in
          ce_cond cond (ce_sub x (ce_int sub)) acc
        )
        (ce_sub x (ce_int last))
        series 
      in
      let v = ce_letin "x" "::flx::rtl::cl_t" (ge e) v in
(*
      print_endline ("Symbolc value = " ^ string_of_cexpr v);
*)
      v
    | BTYP_compactrptsum (_,base)
    | BTYP_rptsum (_,base) -> 
      let size = sizeof_linear_type bsym_table base in
      ce_infix "%" (ge e) (ce_int size)

    | _ -> assert false
   end

  | VR_int -> ce_atom "0"  (* constant constructor argument is unit *)
  | VR_nullptr ->
    begin match size ct with
    | 0 -> assert false
    | 1 -> ce_cast cast (ce_call (ce_atom "FLX_VNP") [ge e])
    | _ -> ce_prefix "*" (ce_cast (cast^"*") (ce_call (ce_atom "FLX_VNP") [ge e]))
    end

  | VR_packed ->
    begin match size ct with
    | 0 -> assert false
    | 1 -> ce_cast cast (ce_call (ce_atom "FLX_VP") [ge e])
    | _ -> ce_prefix "*" (ce_cast (cast^"*") (ce_call (ce_atom "FLX_VP") [ge e]))
    end

  | VR_uctor ->
    begin match size ct with
    | 0 -> assert false
           (* convert to uintptr_t first *) 
    | 1 -> ce_cast cast (ce_cast "uintptr_t" (ce_dot (ge e) "data")) 
    | _ -> ce_prefix "*" (ce_cast (cast^"*") (ce_dot (ge e) "data"))
    end

(* get the argument of a non-constant variant constructor. To be used in a match
 *  when the case index has been found.
 *)
let gen_get_case_arg ge tn bsym_table n (e:Flx_bexpr.t) : cexpr_t =
  let x,ut = e in
  let ct = cal_case_type bsym_table n ut in
  gen_get_arg ge tn bsym_table ct ut e 

let gen_get_rptsum_arg ge tn bsym_table (e:Flx_bexpr.t) : cexpr_t =
  let x,ut = e in
  match ut with
  | BTYP_compactrptsum (_,ct)
  | BTYP_rptsum (_,ct) ->
    gen_get_arg ge tn bsym_table ct ut e 
  | _ -> assert false


(* Value constructor for constant (argumentless) variant constructor case *)
let gen_make_const_ctor bsym_table array_sum_offset_table seq ge' e : cexpr_t =
(*print_endline "gen_make_const_ctor"; *)
  let x,ut = e in
  let v,ut' = 
    match x with 
    | Flx_bexpr.BEXPR_case (v,ut) -> v,ut 
    | Flx_bexpr.BEXPR_varname (i,ts) ->
      begin 
        try 
          let bsym = Flx_bsym_table.find bsym_table i in
          let sr = Flx_bsym.sr bsym in
          match Flx_bsym.bbdcl bsym with
          | BBDCL_const_ctor (vs,uidx,udt, ctor_idx, evs, etraint) ->
            let t = tsubst sr vs ts udt in
            ctor_idx,t 
          | _ -> assert false
        with Not_found -> assert false
      end

    | _ -> assert (false) 
  in
  if ut <> ut' then begin (* original code did an unfold here .. *)
print_endline ("Mismatched types in make_const_ctor expr type is " ^ sbt bsym_table ut ^ 
 " synthesised type = " ^ sbt bsym_table ut');
assert false
  end
  check_case_index bsym_table ut v;
  match cal_variant_rep bsym_table ut with
  | VR_self -> assert false (* will fail if there's one trivial case! FIX! Felix should elide completely *)
  | VR_int -> ce_atom ("/*VR_int*/"^(si v))
  | VR_nullptr -> ce_cast "void* /*VR_nullptr*/ " (ce_atom (si v))
  | VR_packed -> ce_cast "void* /*VR_packed*/ " (ce_atom (si v))
  | VR_uctor -> ce_atom ("::flx::rtl::_uctor_(" ^ si v ^ ",0)") 

  | VR_clt -> 
(*
print_endline ("vgen:BEXPR_case: sum type = " ^ sbt bsym_table ut );
print_endline ("vgen:BEXPR_case: sum value = " ^ sbe bsym_table e);
*)
    let sidx = Flx_ixgen.cal_symbolic_compact_linear_value bsym_table e in
(*
print_endline ("vgen:BEXPR_case: Symbolic sum = " ^ Flx_ixgen.print_index bsym_table sidx );
*)
    let cidx = Flx_ixgen.render_compact_linear_value bsym_table ge' array_sum_offset_table seq sidx in
(*
print_endline ("vgen:BEXPR_case: rendered lineralised index .. C index = " ^ string_of_cexpr cidx);
*)
    cidx

let gen_make_ctor_arg rep ge tn syms bsym_table shape_map a : cexpr_t =
  let _,ct = a in
  match size ct with
  | 0 -> ce_atom "0"                (* NULL: for unit tuple *)
  | 1 -> ce_cast "void*" (ge a)     (* small value goes right into data slot *)
  | _ ->                            (* make a copy on the heap and return pointer *)
    let ctt = tn ct in
    let ptrmap = Flx_pgen.direct_shape_of syms bsym_table shape_map tn ct in
    ce_new [ce_atom "*ptf->gcp"; ce_atom ptrmap; ce_atom "true"] ctt [ge a]


(* Value constructor for non-constant (argumentful) variant constructor case *)
let gen_make_nonconst_ctor ge tn syms bsym_table shape_map codt cidx a : cexpr_t =
(*
print_endline ("gen_make_nonconst_ctor arg=" ^ Flx_print.sbe bsym_table a ^ 
" type=" ^ Flx_print.sbt bsym_table codt); 
*)
  let rep = cal_variant_rep bsym_table codt in
  match rep with
  | VR_self -> ge a
  | VR_clt ->
(*
print_endline ("gen_make_nonconst_sum (clt): " ^
"index = " ^ si cidx ^ "\n" ^
" arg=" ^ sbe bsym_table a ^ ", domain type = " ^ sbt bsym_table codt);
*)
    (* formula is just ctor index * argtype size * value *)
    let base_size = sizeof_linear_type bsym_table codt in
    let v = ce_mul (ce_mul (ce_int cidx) (ce_int base_size)) (ge a) in
(*
    print_endline ("Formula = " ^ string_of_cexpr v);
*)
    v


 
  | VR_int -> ce_call (ce_atom "/*VR_int*/") [ge a]

  | VR_nullptr -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VNR") [ce_atom (si cidx); arg]

  | VR_packed -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VR") [ce_atom (si cidx); arg]

  | VR_uctor ->  
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "::flx::rtl::_uctor_") [ce_atom (si cidx); arg] 

let gen_make_nonconst_rptsum ge tn syms bsym_table shape_map codt (_,idxt as cidx) a : cexpr_t =
  let rep = cal_variant_rep bsym_table codt in
  match rep with
  | VR_self -> assert false (* ge a*)
  | VR_clt -> 
(*
print_endline ("gen_make_nonconst_rptsum (clt): " ^
"index = " ^ sbe bsym_table cidx ^ "\n" ^
" arg=" ^ sbe bsym_table a ^ ", domain type = " ^ sbt bsym_table codt);
*)
    (* formula is just ctor index * argtype size * value *)
    let idx_size = sizeof_linear_type bsym_table idxt in
    let v = ce_mul (ce_mul (ge cidx) (ce_int idx_size)) (ge a) in
(*
    print_endline ("Formula = " ^ string_of_cexpr v);
*)
    v

  | VR_int -> assert false (* ce_call (ce_atom "/*VR_int*/") [ge a] *)

  | VR_nullptr -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VNR") [ge cidx; arg]

  | VR_packed -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VR") [ge cidx; arg]

  | VR_uctor ->  
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "::flx::rtl::_uctor_") [ge cidx; arg] 


