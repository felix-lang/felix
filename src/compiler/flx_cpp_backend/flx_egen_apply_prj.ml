open Flx_btype
open Flx_bexpr
open Flx_exceptions
open Flx_print
open Flx_cexpr

exception Recname of string

let debug = false

let si x = string_of_int x

let max n m = if n < m then m else n 
let pad n s = 
  let s = s ^ "                  "  in
  String.sub s 0 n

let ate bsym_table counter sr msg t1desc t2desc t1 t2 = 
  let n = String.length t1desc in
  let m = String.length t2desc in
  let n = max n m  + 1 in
  let t1desc = pad n t1desc in
  let t2desc = pad n t2desc in
  if not (Flx_typeeq.type_eq (sbt bsym_table) counter t1 t2)
  then clierr sr (msg ^ " got" ^
    "\n" ^ t1desc ^ "type = " ^ sbt bsym_table t1 ^ 
    "\n" ^ t2desc ^ "type = " ^ sbt bsym_table t2) 

(* handle application of constant projections *)

(* all this code should be an optimisation, since projections are functions,
it suffices to apply these functions directly to the argument.

Currently this is not the case I think, not all projections have
closure forms yet.
*)

(* Application of projection to ordinary pointer value 
  Note the pointer can still point to a compact linear value!
*)
let apply_pointer_prj syms bsym_table ge ge' sr (e,t) (ix:int) target codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate msg t1desc t2desc ixt expt = ate bsym_table syms.Flx_mtypes2.counter sr msg t1desc t2desc ixt expt in
  let a = arg in
  let at = argt in
  let ixc = codomain in
  let ixd = target in
  let n = ix in
  match target with
  (* constant projection on ordinary pointer to compact linear array
     yield compact linear pointer 
  *)
  | (BTYP_array (vt,BTYP_unitsum  n)) when clt vt ->
    if debug then print_endline ("Constant projection of pointer to compact-linear array");
    assert (0 <= ix && ix < n);
    let modulus = sizeof_linear_type bsym_table vt in 
    let divisor = modulus * (n - ix - 1)  in
    ce_call (ce_atom "::flx::rtl::clptr_t") [ge' a; ce_int divisor; ce_int modulus]

  (* if this is a constant projection of a pointer to a non-compact linear array *) 
  | (BTYP_array _) ->
    ce_prefix "&" (ce_array (ce_arrow (ge' a) "data") (ce_int ix))

  (* A projection on an ordinary pointer to a compact linear tuple yields a compect linear pointer! *)
  | (BTYP_tuple ts as vt) when clt vt ->
    if debug then print_endline ("Constant projection of pointer to compact-linear tuple");
    assert (0 <= n && n < List.length ts);
    let rec aux ls i out = match ls with [] -> assert false | h :: t ->
      if i = 0 then out else aux t (i-1) (sizeof_linear_type bsym_table h * out)
    in 
    let divisor = aux (List.rev ts) (List.length ts - n - 1) 1 in
    let modulus = sizeof_linear_type bsym_table (List.nth ts n) in
    ce_call (ce_atom "::flx::rtl::clptr_t") [ge' a; ce_int divisor; ce_int modulus]

  (* constant projection of pointer to non-compact linear tuple *)
  | (BTYP_tuple _) ->
    if debug then print_endline ("Constant projection of pointer to non-compact-linear tuple");
    ce_prefix "&" (ce_arrow (ge' a) ("mem_" ^ si n)) 

  (* pointer to record projection *)
  | (BTYP_record es) ->
    let field_name,_ =
      try List.nth es n
      with Not_found ->
        failwith "[flx_egen] Woops, index of non-existent struct field"
    in
    let field_name = if field_name = "" then "_blank_" else field_name in
    ce_prefix "&" (ce_arrow (ge' a) (Flx_name.cid_of_flxid field_name))

  (* pointer to struct or cstruct projection *)
  | (BTYP_inst (i,_,_)) ->
    begin match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_cstruct (_,ls,_)
    | BBDCL_struct (_,ls) ->
      let name,_ =
        try List.nth ls n
        with Not_found ->
          failwith "Woops, index of non-existent struct field"
      in
      ce_prefix "&" (ce_arrow (ge' a) (Flx_name.cid_of_flxid name))

    | t when n = 0 -> ce_prefix "&" (ge' a) (* identity projection *)

    | _ -> failwith ("[flx_egen] Expr "^sbe bsym_table (e,t)^ " type " ^ sbt bsym_table t ^
      " object " ^ sbe bsym_table a ^ " type " ^ sbt bsym_table at ^ 
      " Instance of " ^string_of_int i^ " expected to be (c)struct")
    end

 | _ -> assert false

(* Application of projection to compact linear pointer *)
let apply_cltpointer_prj syms bsym_table ge ge' sr (e,t) (ix:int) (mach, target) codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate msg t1desc t2desc ixt expt = ate bsym_table syms.Flx_mtypes2.counter sr msg t1desc t2desc ixt expt in
  let a = arg in
  let at = argt in
  let n = ix in
  match target with
  (* constant projection of cltpointer to compact linear tuple *)
  | BTYP_tuple ts ->
    assert (0 <= n && n < List.length ts);
    let rec aux ls i out = match ls with [] -> assert false | h :: t ->
      if i = 0 then out else aux t (i-1) (sizeof_linear_type bsym_table h * out)
    in 
    let divisor = aux (List.rev ts) (List.length ts - n - 1) 1 in
    let modulus = sizeof_linear_type bsym_table (List.nth ts n) in
 
    let prj = ce_call (ce_atom "::flx::rtl::clprj_t") [ce_int divisor; ce_int modulus] in
    let ptr = ge' a in
    ce_call (ce_atom "::flx::rtl::applyprj") [ptr;prj]

  | BTYP_array (vt,BTYP_unitsum n) -> (* vt has to be compact linear *)
    let ipow v i = match i with 0 -> 1 | _ -> v * ipow v (i-1) in
    let modulus = Flx_btype.sizeof_linear_type bsym_table vt in 
    let divisor = ipow modulus (n - ix - 1) in
    let prj = ce_call (ce_atom "::flx::rtl::clprj_t") [ce_int divisor; ce_int modulus] in
    let ptr = ge' a in
    ce_call (ce_atom "::flx::rtl::applyprj") [ptr;prj]
 
  | _ -> assert false

let ipow' bsym_table power_table base exp array_len = 
  match exp with
  | `Ce_int i -> 
    let rec ipow = begin function 0 -> 1 | n -> base * (ipow (n - 1)) end in
    ce_int (ipow i)
  | _ ->
    let ipow = Flx_ixgen.get_power_table bsym_table power_table base array_len in
    ce_array (ce_atom ipow) exp

(* Application of projection to non pointer value *)
let apply_value_prj syms bsym_table ge ge' sr (e,t) (ix:int) domain codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate msg t1desc t2desc ixt expt = ate bsym_table syms.Flx_mtypes2.counter sr msg t1desc t2desc ixt expt in
  let a = arg in
  let at = argt in
  let ixc = codomain in
  let ixd = domain in
  let n = ix in
  match domain with
  (* if this is a constant projection of a compact linear array *) 
  | BTYP_array (vt,aixt) when clt at ->
    if debug then print_endline ("Constant value projection  " ^ si ix ^ " of compact-linear array type " ^ sbt bsym_table at);
    ate "value projection of compact linear array" "proj domain" "exponent" ixd  at;
    ate "value projection of compact linear array" "proj codomain" "array base" ixc vt;
    assert (clt ixd);
    assert (clt ixc);
    let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
    let seq = syms.Flx_mtypes2.counter in
    let power_table = syms.Flx_mtypes2.power_table in
    let array_value_size = sizeof_linear_type bsym_table vt in
    let a = ge' a in
    let ix = ce_int ix in
    let exp = ce_sub (ce_int (array_len - 1)) ix in
    let sdiv = ipow' bsym_table power_table array_value_size exp array_len in
    let result = (ce_rmd (ce_div a sdiv) (ce_int array_value_size)) in
    result

  (* if this is a constant projection of a non-compact linear array *) 
  | BTYP_array _-> 
    if debug then print_endline ("Constant value projection " ^ si ix ^ " of non-compact-linear array type " ^ sbt bsym_table at);
    ce_array (ce_dot (ge' a) "data") (ce_int ix)

  (* if this is a constant projection of a compact linear tuple *) 
  | BTYP_tuple ts when clt at ->
    if debug then print_endline ("Constant value projection "^si ix ^" of compact-linear tuple type" ^ sbt bsym_table at);
    let n = ix in 
    assert (0 <= n && n < List.length ts);
    let rec aux ls i out = match ls with [] -> assert false | h :: t ->
      if i = 0 then out else aux t (i-1) (sizeof_linear_type bsym_table h * out)
    in 
    let x = aux (List.rev ts) (List.length ts - n - 1) 1 in
    let y = sizeof_linear_type bsym_table (List.nth ts n) in
    let a = ge' a in
    let result = ce_rmd (ce_div a (ce_int x)) (ce_int y) in
    result

  (* a constant projection of a non-compact linear tuple, just
     fetch the n'th component by its field name
  *) 
  | BTYP_tuple _ ->
    if debug then print_endline ("Constant value projection of non-compact-linear array type " ^ sbt bsym_table at);
    ce_dot (ge' a) ("mem_" ^ si ix) 

  (* record projection *)
  | BTYP_record es ->
    let index = ref 0 in
    let seq = ref 0 in
    let name,_ =
        try List.nth es n
        with Not_found ->
          failwith "Woops, index of non-existent struct field"
    in
    begin try
    List.iter (fun (s,_) ->
      if n = (!index) then
        raise (Recname (Flx_name.cid_of_flxid ( if (!seq) = 0 then s else "_" ^ s ^ "_" ^ string_of_int (!seq))))
      else begin
        incr index;
        if s = name then incr seq
      end 
    )
    es
    ;
    failwith "Error calculating record name"
    with Recname s ->
      let s = if s = "" then "_blank_" else s in
      ce_dot (ge' a) s 
    end

  (* struct or cstruct projection *)
  | BTYP_inst (i,_,_) ->
    begin match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_cstruct (_,ls,_)
    | BBDCL_struct (_,ls) ->
      let name,_ =
        try List.nth ls n
        with Not_found ->
          failwith "Woops, index of non-existent struct field"
      in
      ce_dot (ge' a) (Flx_name.cid_of_flxid name)

    | t when n = 0 -> ge' a (* identity projection *)

    | _ -> failwith ("[flx_egen] Expr "^sbe bsym_table (e,t)^ " type " ^ sbt bsym_table t ^
      " object " ^ sbe bsym_table a ^ " type " ^ sbt bsym_table at ^ 
      " Instance of " ^string_of_int i^ " expected to be (c)struct")
    end

  | _ -> assert false

let apply_prj syms bsym_table ge ge' sr (e,t) (ix:int) domain codomain (argv,argt as arg) =
  (* if the argument is literally a tuple we just extract the 
  ix'th component and discard the rest. Works for records too since name indexes have
  been replaced by integer ones by the time we get here
  *)
  match argv with
  | BEXPR_tuple es ->
    let v = List.nth es ix in
    ge' v

  | BEXPR_record flds ->
    let v = snd (List.nth flds ix) in
    ge' v
 
  | _ ->
  match domain with
  | BTYP_pointer target ->
    apply_pointer_prj syms bsym_table ge ge' sr (e,t) ix target codomain arg

  | BTYP_cltpointer (mach, target) -> 
    apply_cltpointer_prj syms bsym_table ge ge' sr (e,t) ix (mach, target) codomain arg

  | target ->
    apply_value_prj syms bsym_table ge ge' sr (e,t) ix target codomain arg


let apply_array_prj syms bsym_table ge ge' sr (e,t) ix ixd ixc (_,at as a) =
  let ate msg t1desc t2desc ixt expt = ate bsym_table syms.Flx_mtypes2.counter sr msg t1desc t2desc ixt expt in
  let clt t = islinear_type bsym_table t in

  match at with 
  (* if this is an array projection of a compact linear array *)
  |  BTYP_array (vt,aixt) when clt at ->

if debug then begin
print_endline ("Array value projection of compact linear array type " ^ sbt bsym_table at);
print_endline ("Array base value type = " ^ sbt bsym_table vt);
print_endline ("Array exponent type = " ^ sbt bsym_table aixt);
print_endline ("Projection: domain = " ^ sbt bsym_table ixd);
print_endline ("Projection: codomain = " ^ sbt bsym_table ixc);
print_endline ("Index type = " ^ sbt bsym_table (snd ix));
end;
    ate "Array value projection of compact linear array" "index domain" "array type" ixd  at;
    ate "Array value projection of compact linear array" "index codomain" "array base type" ixc  vt;
    assert (clt ixd);
    assert (clt ixc);
    let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
    let seq = syms.Flx_mtypes2.counter in
    let power_table = syms.Flx_mtypes2.power_table in
    let array_value_size = sizeof_linear_type bsym_table vt in
    let a = ge' a in
    let ix = ge' ix in
    let sdiv = ipow' bsym_table power_table array_value_size (ce_sub (ce_int (array_len - 1)) ix) array_len in
    let result = (ce_rmd (ce_div a sdiv) (ce_int array_value_size)) in
    result

  (* if this is an array projection of a non-compact linear array with a compact linear index*)
  (* this is general case at the moment because ALL array indices are compact linear *)
  (* note this code "works" even if the array index type is not a unitsum! *)

  | BTYP_array (vt,aixt) when clt aixt ->
    if debug then print_endline ("Array projection of non-compact linear array, index is compact linear of course");
    let cix = ge' ix in
    if debug then print_endline ("Felix index expression is " ^ sbe bsym_table ix);
    if debug then print_endline ("C expression to calculate index is " ^ Flx_cexpr.string_of_cexpr cix);
    ce_array (ce_dot (ge' a) "data") (ge' ix) 

  (* if this is an array projection of a non-compact linear array *)
  | BTYP_array _ ->
    print_endline ("Flx_egen: projection of array with non-compact linear index can't happen");
    print_endline ("Flx_egen: no support for, eg, arrays indexed by strings");
    assert false;
    ce_array (ce_dot (ge' a) "data") (ge' ix) 

  (* array projection of an ORINDARY pointer to a compact linear array 
    result is a compact linear pointer
  *)
  | BTYP_pointer (BTYP_array (vt,aixt) as at) when clt at -> 
print_endline ("Array projection of ordinary pointer to compact linear array type " ^ sbt bsym_table at);
print_endline ("Array base value type = " ^ sbt bsym_table vt);
print_endline ("Array exponent type = " ^ sbt bsym_table aixt);
print_endline ("Projection: domain = " ^ sbt bsym_table ixd);
print_endline ("Projection: codomain = " ^ sbt bsym_table ixc);
print_endline ("Index type = " ^ sbt bsym_table (snd ix));

(*
    ate "Array projection of ordinary pointer to compact linear array" "index domain" "array type" ixd  at;
    ate "Array projection of ordinary pointer to compact linear array" "index codomain" "array base type" ixc vt;
    assert (clt ixd);
    assert (clt ixc);
*)
    let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
    let seq = syms.Flx_mtypes2.counter in
    let power_table = syms.Flx_mtypes2.power_table in
    let array_value_size = sizeof_linear_type bsym_table vt in
    let a = ge' a in
    let ix = ge' ix in
    let exp = ce_sub (ce_int (array_len - 1)) ix in
    let divisor = ipow' bsym_table power_table array_value_size exp array_len in
    let modulus = ce_int array_value_size in
    ce_call (ce_atom "::flx::rtl::clptr_t") [a; divisor; modulus]


  (* array projection of a compact linear pointer to a compact linear array 
     result is a compact linear pointer
  *)
  | BTYP_cltpointer (mach, BTYP_array (vt,aixt))  -> 
(*
    ate "Array projection of compact linear pointer to compact linear array" "index domain" "array type" ixd  at;
    ate "Array projection of compact linear pointer to compact linear array" "index codomain" "array base type" ixc vt;
    assert (clt ixd);
    assert (clt ixc);
*)
    let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
    let seq = syms.Flx_mtypes2.counter in
    let power_table = syms.Flx_mtypes2.power_table in
    let array_value_size = sizeof_linear_type bsym_table vt in
    let a = ge' a in
    let ix = ge' ix in
    let exp = ce_sub (ce_int (array_len - 1)) ix in
    let divisor = ipow' bsym_table power_table array_value_size exp array_len in
    let modulus = ce_int array_value_size in

    (* multiply new and old divisors *)
    let old_divisor = ce_dot a "divisor" in
    let divisor = ce_infix "*" old_divisor divisor in
    let ptr = ce_dot a "p" in
    ce_call (ce_atom "::flx::rtl::clptr_t") [ptr; divisor; modulus]

  (* if this is an array projection of a POINTER to a non-compact linear array *)
  | BTYP_pointer (BTYP_array _) ->
    ce_prefix "&" (ce_array (ce_arrow (ge' a) "data") (ge' ix)) 

  | _-> assert false
