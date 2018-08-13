open Flx_btype
open Flx_bexpr
open Flx_exceptions
open Flx_print
open Flx_cexpr

exception Recname of string

let si x = string_of_int x

(* handle application of constant projections *)

(* all this code should be an optimisation, since projections are functions,
it suffices to apply these functions directly to the argument.

Currently this is not the case I think, not all projections have
closure forms yet.
*)

(* Application of projection to ordinary pointer value 
  Note the pointer can still point to a compact linear value!
*)
let apply_pointer_prj syms bsym_table ge ge' (e,t) (ix:int) target codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate t1 t2 = assert (Flx_typeeq.type_eq (sbt bsym_table) syms.Flx_mtypes2.counter t1 t2) in
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
    assert (0 <= ix && ix < n);
    let modulus = sizeof_linear_type bsym_table vt in 
    let divisor = modulus * (n - ix - 1)  in
    ce_call (ce_atom "::flx::rtl::clptr_t") [ge' a; ce_int divisor; ce_int modulus]

  (* if this is a constant projection of a pointer to a non-compact linear array *) 
  | (BTYP_array _) ->
    ce_prefix "&" (ce_array (ce_arrow (ge' a) "data") (ce_int ix))

  (* A projection on an ordinary pointer to a compact linear tuple yields a compect linear pointer! *)
  | (BTYP_tuple ts as vt) when clt vt ->
    assert (0 <= n && n < List.length ts);
    let rec aux ls i out = match ls with [] -> assert false | h :: t ->
      if i = 0 then out else aux t (i-1) (sizeof_linear_type bsym_table h * out)
    in 
    let divisor = aux (List.rev ts) (List.length ts - n - 1) 1 in
    let modulus = sizeof_linear_type bsym_table (List.nth ts n) in
    ce_call (ce_atom "::flx::rtl::clptr_t") [ge' a; ce_int divisor; ce_int modulus]

  (* constant projection of pointer to non-compact linear tuple *)
  | (BTYP_tuple _) ->
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
let apply_cltpointer_prj syms bsym_table ge ge' (e,t) (ix:int) (mach, target) codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate t1 t2 = assert (Flx_typeeq.type_eq (sbt bsym_table) syms.Flx_mtypes2.counter t1 t2) in
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

(* Application of projection to non pointer value *)
let apply_value_prj syms bsym_table ge ge' (e,t) (ix:int) domain codomain (_,argt as arg) =
  let clt t = islinear_type bsym_table t in
  let ate t1 t2 = assert (Flx_typeeq.type_eq (sbt bsym_table) syms.Flx_mtypes2.counter t1 t2) in
  let a = arg in
  let at = argt in
  let ixc = codomain in
  let ixd = domain in
  let n = ix in
  match domain with
  (* if this is a constant projection of a compact linear array *) 
  | BTYP_array (vt,aixt) when clt at ->
    ate ixd  at;
    ate ixc vt;
    assert (clt ixd);
    assert (clt ixc);
    let array_len = Flx_btype.sizeof_linear_type bsym_table aixt in
    let seq = syms.Flx_mtypes2.counter in
    let power_table = syms.Flx_mtypes2.power_table in
    let ipow' base exp = 
      match exp with
      | `Ce_int i -> 
        let rec ipow = begin function 0 -> 1 | n -> base * (ipow (n - 1)) end in
        ce_int (ipow i)
      | _ ->
        let ipow = Flx_ixgen.get_power_table bsym_table power_table base array_len in
        ce_array (ce_atom ipow) exp
    in
    let array_value_size = sizeof_linear_type bsym_table vt in
    let a = ge' a in
    let ix = ce_int ix in
    let exp = ce_sub (ce_int (array_len - 1)) ix in
    let sdiv = ipow' array_value_size exp in
    let result = (ce_rmd (ce_div a sdiv) (ce_int array_value_size)) in
    result

  (* if this is a constant projection of a non-compact linear array *) 
  | BTYP_array _-> 
    ce_array (ce_dot (ge' a) "data") (ce_int ix)

  (* if this is a constant projection of a compact linear tuple *) 
  | BTYP_tuple ts when clt at ->
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

let apply_prj syms bsym_table ge ge' (e,t) (ix:int) domain codomain (argv,argt as arg) =
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
    apply_pointer_prj syms bsym_table ge ge' (e,t) ix target codomain arg

  | BTYP_cltpointer (mach, target) -> 
    apply_cltpointer_prj syms bsym_table ge ge' (e,t) ix (mach, target) codomain arg

  | target ->
    apply_value_prj syms bsym_table ge ge' (e,t) ix target codomain arg


