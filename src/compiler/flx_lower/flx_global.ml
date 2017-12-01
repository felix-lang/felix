open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_bid

(* Garbage collector usage. The gc is required for non-stacked
  procedure calls, applications, closure formations,
  and variant constructors
*)

let throw_on_heap_type bsym_table t =
    begin match t with
    | BTYP_variant _ -> raise Not_found
    | BTYP_sum args when not (all_units args) -> raise Not_found
    | BTYP_inst (i,ts,_) ->
      begin match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_union (vs,idts) when not
          (all_voids (List.map (fun (_,_,evs,d,c,_)->d) idts)) -> raise Not_found
      | _ -> ()
      end
    | _ -> ()
    end

let throw_on_gc bsym_table e : unit = 
  match e with
  | BEXPR_new _,_
  | BEXPR_class_new _,_ -> raise Not_found

  | BEXPR_closure (i,_),_ ->
    (*
    print_endline ("Found closure of " ^ si i);
    *)
    raise Not_found

  | BEXPR_apply_direct _,_ -> raise Not_found
  | BEXPR_apply( (BEXPR_closure (_,_),_),_),_ -> raise Not_found
  | BEXPR_apply_struct (i,_,_),_ ->
    begin match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_nonconst_ctor _ -> raise Not_found
    | _ -> ()
    end

  | BEXPR_inj (_,d,c),_ -> throw_on_heap_type bsym_table c

  | BEXPR_case (_,t),_ -> throw_on_heap_type bsym_table t
  | _ -> ()

let expr_uses_gc bsym_table e =
  Flx_bexpr.iter ~f_bexpr:(throw_on_gc bsym_table) e

let exe_uses_gc bsym_table exe =
try
  begin match exe with
  | BEXE_jump_direct _
  | BEXE_call_direct _ -> raise Not_found

  (* this test is used to trap use of gc by primitives *)
  | BEXE_call_prim (sr,i,ts,a) ->
      let bsym = Flx_bsym_table.find bsym_table i in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_external_fun (props,vs,ps,BTYP_fix (0,_),_,_,_)
      | BBDCL_external_fun (props,vs,ps,BTYP_void,_,_,_) ->
          if List.mem `Uses_gc props then begin
            (* Flagged as using gc *)
            raise Not_found
          end;

          Flx_bexe.iter ~f_bexpr:(expr_uses_gc bsym_table) exe
      | _ ->
          print_endline ("Call primitive to non-primitive " ^ Flx_bsym.id bsym ^
            "<" ^ string_of_bid i ^ ">");
          assert false
      end

  | _ ->
      Flx_bexe.iter ~f_bexpr:(expr_uses_gc bsym_table) exe
  end;
  
with
  | Not_found ->  raise Not_found

let exes_use_gc bsym_table exes =
  try
    List.iter (exe_uses_gc bsym_table) exes;
    false
  with
    Not_found ->
    (*
    print_endline "GC USED HERE";
    *)
    true

let exe_uses_yield exe =
  match exe with
  | BEXE_yield _ -> raise Not_found
  | _ -> ()

let exes_use_yield exes =
  try
    List.iter exe_uses_yield exes;
    false
  with
    Not_found ->
    (*
    print_endline "YIELD USED HERE";
    *)
    true

(* ALSO calculates if a function uses a yield *)
let set_gc_use bsym_table index bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props, vs, ps, rt, effects, exes) ->
      let props = if exes_use_gc bsym_table exes
        then `Uses_gc :: props
        else props
      in
      let props =
        match rt with
        | BTYP_void -> props
        | _ ->
            if exes_use_yield exes
              then `Heap_closure :: `Yields :: `Generator :: props
              else props
      in
      let bbdcl = bbdcl_fun (props,vs,ps,rt,effects, exes) in
      Flx_bsym_table.update_bbdcl bsym_table index bbdcl

  | _ -> ()

let throw_on_global bsym_table i =
  if Flx_bsym_table.is_global_var bsym_table i then raise Not_found

let expr_uses_global bsym_table e =
  Flx_bexpr.iter ~f_bid:(throw_on_global bsym_table) e

let exe_uses_global bsym_table exe =
  Flx_bexe.iter
    ~f_bid:(throw_on_global bsym_table)
    ~f_bexpr:(expr_uses_global bsym_table)
    exe

let exes_use_global bsym_table exes =
  try
    List.iter (exe_uses_global bsym_table) exes;
    false
  with Not_found -> true


let set_local_globals bsym_table index bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,rt,effects, exes) ->
      if exes_use_global bsym_table exes then begin
        let bbdcl = bbdcl_fun (`Uses_global_var :: props,vs,ps,rt,effects, exes) in
        Flx_bsym_table.update_bbdcl bsym_table index bbdcl
      end
  | _ -> ()

let set_base_ptf_use bsym_table bid bsym  =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,rt,effects, exes) ->
    if List.mem `Not_requires_ptf props then begin
      print_endline "Set_base_ptf_use found symbol with Not_requires_ptf use already set!";
      assert false;
    end
    ;
    if List.mem `Uses_gc props || List.mem `Heap_closure props || List.mem `Uses_global_var props then begin
      if List.mem `Not_requires_ptf props then begin
        print_endline "Conflicting requirements";
        assert false
      end else 
      if not (List.mem `Requires_ptf props) then begin
        let bbdcl = bbdcl_fun (`Requires_ptf :: props,vs,ps,rt,effects,exes) in
        Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
      end
    end 

  | BBDCL_external_fun (props,vs,ps,ret,ct,reqs,prec) ->
(*
    print_endline ("Set base ptf usage of primitive fun " ^ Flx_bsym.id bsym ^ "<"^si bid^">");
*)
(*
    if List.mem `Requires_ptf props then begin
      print_endline "Set_base_ptf_use found symbol with Requires_ptf set!";
    end
    ;
*)
    if List.mem `Not_requires_ptf props then begin
      print_endline "Set_base_ptf_use found symbol with Not_requires_ptf set!";
      assert false;
    end
    ;
(*
    if List.mem `Uses_gc props then begin
      print_endline "Set_base_ptf_use found symbol with Uses_gc set!";
    end
    ;
*)
(*
    if List.mem `Heap_closure props then begin
      print_endline "Set_base_ptf_use found symbol with Heap_closure set!";
    end
    ;
*)
    if List.mem `Uses_gc props || List.mem `Heap_closure props then begin
      if List.mem `Not_requires_ptf props then begin
        print_endline "Conflicting requirements";
        assert false
      end else
      if not (List.mem `Requires_ptf props) then begin
        let bbdcl = bbdcl_external_fun ( `Requires_ptf :: props, vs, ps, ret, ct, reqs, prec) in
        Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
      end
    end 


  | _ -> ()

let set_ptf_for_symbol bsym_table usedby bid bsym  =
  let props = 
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,_,_,_,_,_) 
    | BBDCL_external_fun (props,_,_,_,_,_,_) -> props
    | BBDCL_external_const (props,_,_,_,_) -> props
    | _ -> []
  in
  if List.mem `Requires_ptf props then
  let cls = Flx_call.use_closure usedby bid in
  BidSet.iter (fun bid ->
    let bsym = Flx_bsym_table.find bsym_table bid in
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_fun (props,vs,ps,rt,effects,exes) ->
      if List.mem `Not_requires_ptf props then begin
        print_endline "Conflicting requirements";
        assert false
      end else 
      if not (List.mem `Requires_ptf props) then begin
        let bbdcl = bbdcl_fun (`Requires_ptf :: props,vs,ps,rt,effects,exes) in
        Flx_bsym_table.update_bbdcl bsym_table bid bbdcl
      end
    | _ -> ()
  )
  cls


let set_globals bsym_table =
(*
print_endline "Set globals";
*)
  Flx_bsym_table.iter begin fun bid _ bsym ->
    set_local_globals bsym_table bid bsym
  end bsym_table;

(*
print_endline "Set gc use";
*)
  Flx_bsym_table.iter begin fun bid _ bsym ->
    set_gc_use bsym_table bid bsym
  end bsym_table;

  let uses, usedby = Flx_call.call_data bsym_table in

(*
  print_endline "calculating base ptf usage";
*)
  Flx_bsym_table.iter (fun bid _ bsym ->
     set_base_ptf_use bsym_table bid bsym;
  )
  bsym_table
  ;

  (* Iterate through each symbol and mark if the function needs a frame. *)
  Flx_bsym_table.iter begin fun bid _ bsym ->
    set_ptf_for_symbol bsym_table usedby bid bsym
  end bsym_table

let find_global_vars bsym_table =
  let global_vars = ref BidSet.empty in
  Flx_bsym_table.iter begin fun bid _ _ ->
    if Flx_bsym_table.is_global_var bsym_table bid
    then global_vars := BidSet.add bid !global_vars
  end bsym_table;

  !global_vars

let check_used used i =
  Hashtbl.mem used i

let check_all_used used ii =
  let all_used = ref true in
  BidSet.iter (fun i -> if not (check_used used i)
    then begin
      print_endline ("FOUND UNUSED VARIABLE " ^ string_of_bid i);
      all_used := false
    end
  )
  ii
  ;
  if !all_used then
    print_endline "ALL GLOBAL VARS ARE USED"
  else
    print_endline "Some UNUSED vars!"

let check_global_vars_all_used bsym_table used =
  let ii = find_global_vars bsym_table in
  check_all_used used ii

