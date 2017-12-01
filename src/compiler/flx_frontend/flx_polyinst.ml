open Flx_bbdcl
open Flx_btype
open Flx_bexpr
open Flx_bexe

let debug = false
let si i = string_of_int i
let catmap x = Flx_util.catmap x
let sbt x = Flx_print.sbt x

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING REFS TO POLYMORPHS WITH MONOS        *)
(* ----------------------------------------------------------- *)

let flat_poly_fixup_type syms bsym_table polyinst sr t =
(*
  if not (complete_type t) then
    print_endline ("flat_poly_fixup_type: type isn't complete " ^ sbt bsym_table t);
*)

  match t with
  | BTYP_vinst (i,ts,_) ->
    let parent,bsym = Flx_bsym_table.find_with_parent bsym_table i in
    if debug then
    print_endline ("Virt: flat_poly_fixup_type is using polyinst to instantiate type " ^ Flx_bsym.id bsym ^
    "<"^ si i^">[" ^catmap "," (sbt bsym_table) ts^ "]");
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_virtual_type bvs ->
      if debug then
      print_endline ("  Virt: *** VIRTUAL TYPE");
      begin match parent with
      | None -> assert false
      | Some tc ->
        if debug then print_endline ("   *** parent type class " ^ si tc);
        let t' = Flx_build_tctab.remap_virtual_types syms bsym_table (* tc *) t in
        if debug then print_endline ("   *** mapped to " ^ sbt bsym_table t');
        t'
      end
    | _ -> assert false; 
    end
  | BTYP_inst (i,ts,_) ->
    let parent,bsym = Flx_bsym_table.find_with_parent bsym_table i in
    if debug then
    print_endline ("Inst: flat_poly_fixup_type is using polyinst to instantiate type " ^ Flx_bsym.id bsym ^
    "<"^ si i^">[" ^catmap "," (sbt bsym_table) ts^ "]");
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_virtual_type _
    | BBDCL_instance_type _ ->  assert false;
    | _ -> 
      if debug then  
      print_endline ("Inst: OTHER TYPE");
      let i',ts' = polyinst sr i ts in
      let t' = btyp_inst (i',ts',Flx_kind.KIND_type) in
      if debug then
      print_endline ("poly_fixup_type: " ^ Flx_monodebug.showts bsym_table i ts ^ " --> " ^ Flx_monodebug.showts bsym_table i' ts');
      t'
      end
  | x -> x

(* this has to be top down, so instances i,ts use the original
ts, then the ts get analysed. Otherwise, we'd get new symbols
in the ts and there's be no match on the replacement table
*)

let rec rec_poly_fixup_type trail syms bsym_table polyinst sr t =
  let level = Flx_list.list_index trail t in
  match level with
  | Some i -> btyp_fix (-i-1) (Flx_kind.KIND_type)
  | None ->
  let t' = unfold "rec_poly_fixup_type" t in
  let t' = flat_poly_fixup_type syms bsym_table polyinst sr t' in
  let f_btype t' = rec_poly_fixup_type (t::trail) syms bsym_table polyinst sr t' in
  let t' = Flx_btype.map ~f_btype t' in
  t'

let poly_fixup_type syms bsym_table polyinst sr t =
 let t' = rec_poly_fixup_type [] syms bsym_table polyinst sr t in
(*
 print_endline ("POLY FIXUP TYPE: " ^ sbt bsym_table t ^ " --> " ^ sbt bsym_table t');
*)
 t'

let flat_poly_fixup_expr syms bsym_table polyinst sr (e,t) =
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) -> assert false
  | BEXPR_apply_direct (i,ts,a) -> assert false
  | BEXPR_apply_struct (i,ts,a) -> assert false
  | BEXPR_apply_stack (i,ts,a) -> assert false
  | BEXPR_ref (i,ts)  ->
    let i,ts = polyinst sr i ts in
    bexpr_ref t (i,ts)

  | BEXPR_varname (i',ts') ->
    let i,ts = polyinst sr i' ts' in
    bexpr_varname t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = polyinst sr i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  x

let rec poly_fixup_expr syms bsym_table polyinst sr e =
  let f_bexpr e = poly_fixup_expr  syms bsym_table polyinst sr e in
  let f_btype t = poly_fixup_type syms bsym_table polyinst sr t in
  let e = flat_poly_fixup_expr syms bsym_table polyinst sr e in
  let e = Flx_bexpr.map ~f_bexpr ~f_btype e in
  e 

(* mt is only used to fixup svc and init hacks *)
let flat_poly_fixup_exe syms bsym_table polyinst parent_ts mt exe =
(*
  print_endline ("TYPECLASS FIXUP EXE[In] =" ^ string_of_bexe bsym_table 0 exe);
*)
  let result =
  match exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
  | BEXE_jump_direct (sr, i,ts,a) -> assert false
  | BEXE_call_prim (sr, i',ts,a) -> assert false
  | BEXE_call_stack (sr, i,ts,a) -> assert false

  (* this is deviant case: implied ts is vs of parent! *)
  | BEXE_init (sr,i,e) ->
(*
    print_endline ("[flat_poly_fixup_exe: init] Deviant case variable " ^ si i);
*)
    let vs = 
      try Flx_bsym_table.find_bvs bsym_table i 
      with Not_found -> assert false
    in
    assert (List.length vs = List.length parent_ts);
    let j,ts = polyinst sr i parent_ts in
(*
    if i <> j then 
      print_endline ("[init] Remapped deviant variable to " ^ si j);
*)
    bexe_init (sr,j,e)

  | BEXE_svc (sr,i) ->
    (*
    print_endline ("[flat_poly_fixup_exe: svc] Deviant case variable " ^ si i);
    *)
    let vs = 
      try Flx_bsym_table.find_bvs bsym_table i 
      with Not_found -> assert false
    in
    assert (List.length vs = List.length parent_ts);
    let j,ts = polyinst sr i parent_ts in
    (*
      if i <> j then
        print_endline ("[svc] Remapped deviant variable to " ^ si j);
    *)
    bexe_svc (sr,j)

  | BEXE_label (sr,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_label (sr,j)

  | BEXE_goto  (sr,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_goto (sr,j)

  | BEXE_ifgoto (sr,e,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_ifgoto (sr,e,j)



  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result


