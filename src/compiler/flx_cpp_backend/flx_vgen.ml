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
let gen_get_case_index ge bsym_table e: cexpr_t  =
(*print_endline ("Gen_get_case_index " ^ Flx_print.string_of_bound_expression bsym_table e); *)
  let _,t = e in
  match cal_variant_rep bsym_table t with
  | VR_self -> ce_atom "0"
  | VR_int -> 
    let n = cal_variant_cases bsym_table t in 
     ce_infix "%" (ge e) (ce_atom (si n))
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
  | BTYP_sum ls -> nth ls n
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

  | BTYP_inst (i,ts,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found -> assert false
    in
    let sr = Flx_bsym.sr bsym in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_union (bvs,cts) -> 
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
  | VR_int ->  assert false
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
  | BTYP_rptsum (_,ct) ->
    gen_get_arg ge tn bsym_table ct ut e 
  | _ -> assert false


(* Value constructor for constant (argumentless) variant constructor case *)
let gen_make_const_ctor bsym_table e : cexpr_t =
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


(*
(* Helper function to make suitable argument for non-constant variant constructor *)
let gen_make_ctor_arg rep ge tn syms bsym_table a : cexpr_t =
(* print_endline "gen_make_ctor_arg"; *)
  let _,ct = a in
  match rep with
  | VR_uctor ->
  begin match size ct with
  | 0 -> ce_atom "0"                (* NULL: for unit tuple *)
  | 1 -> ce_cast "/* uctor, arg size1 */ void*" (ge a)   (* small value goes right into data slot *)
  | _ ->                            (* make a copy on the heap and return pointer *)
    let ctt = "/* tn= "^ tn ct ^ " */ "^ tn ct in
    let ptrmap = Flx_pgen.direct_shape_of syms bsym_table tn ct in
    ce_new [ce_atom "*PTF gcp /* uctor, arg size2 */ "; ce_atom ptrmap; ce_atom "true"] ctt [ge a]
 end
 | VR_packed ->
   begin match size ct with
   | 0 -> ce_atom "0"                (* NULL: for unit tuple *)
   | 1 -> ce_cast "/* packed arg size1 */ void*" (ge a)   (* small value goes right into data slot *)
   | _ ->
    let ctt = "/* tn= "^ tn ct ^ " */ "^ tn ct in
    let ptrmap = Flx_pgen.direct_shape_of syms bsym_table tn ct in
    ce_new [ce_atom "*PTF gcp /* packed, arg size2 */ "; ce_atom ptrmap; ce_atom "true"] ctt [ge a]
 
   end
 | VR_int -> ge a
 | VR_nullptr -> ce_cast "void*" (ge a)

 | _ -> assert false
*)

let gen_make_ctor_arg rep ge tn syms bsym_table shape_map a : cexpr_t =
  let _,ct = a in
  match size ct with
  | 0 -> ce_atom "0"                (* NULL: for unit tuple *)
  | 1 -> ce_cast "void*" (ge a)     (* small value goes right into data slot *)
  | _ ->                            (* make a copy on the heap and return pointer *)
    let ctt = tn ct in
    let ptrmap = Flx_pgen.direct_shape_of syms bsym_table shape_map tn ct in
    ce_new [ce_atom "*PTF gcp"; ce_atom ptrmap; ce_atom "true"] ctt [ge a]


(* Value constructor for non-constant (argumentful) variant constructor case *)
let gen_make_nonconst_ctor ge tn syms bsym_table shape_map codt cidx a : cexpr_t =
(*
print_endline ("gen_make_nonconst_ctor arg=" ^ Flx_print.sbe bsym_table a ^ 
" type=" ^ Flx_print.sbt bsym_table codt); 
*)
  let rep = cal_variant_rep bsym_table codt in
  match rep with
  | VR_self -> ge a
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

let gen_make_nonconst_rptsum ge tn syms bsym_table shape_map codt cidx a : cexpr_t =
(*
print_endline ("gen_make_nonconst_ctor arg=" ^ Flx_print.sbe bsym_table a ^ 
" type=" ^ Flx_print.sbt bsym_table codt); 
*)
  let rep = cal_variant_rep bsym_table codt in
  match rep with
  | VR_self -> assert false (* ge a*)
  | VR_int -> assert false (* ce_call (ce_atom "/*VR_int*/") [ge a] *)

  | VR_nullptr -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VNR") [cidx; arg]

  | VR_packed -> 
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "FLX_VR") [cidx; arg]

  | VR_uctor ->  
    let arg = gen_make_ctor_arg rep ge tn syms bsym_table shape_map a in
    ce_call (ce_atom "::flx::rtl::_uctor_") [cidx; arg] 


