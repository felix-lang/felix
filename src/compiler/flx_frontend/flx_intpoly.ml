open Flx_types
open Flx_btype
open Flx_bexe
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_maps
open List
open Flx_util
open Flx_print
open Flx_mtypes2

let ident x = x

exception Skip

let polyfix syms polyvars i ts =
  let poly = 
    try Hashtbl.find polyvars i with Not_found -> [] 
  in
  let ts = Array.of_list ts in
  iter (fun (i,j) -> ts.(j) <- btyp_void) poly
  ;
  let ts = Array.to_list ts in
  ts

let remove i ls = filter (fun (k,j) -> i <> k) ls

let rec check_abstract_type syms rls t =
  match t with
  | BTYP_pointer (BTYP_type_var _) -> ()
  | BTYP_type_var (i,_) ->
      (*
      print_endline ("Removing type variable " ^ string_of_int i);
      *)
      rls := remove i !rls;
      if !rls = [] then raise Not_found
  | t' -> Flx_btype.iter ~ft:(check_abstract_type syms rls) t'

(* note this routine doesn't check types in ts lists, because
 * these apply to variables including parameters as qualifiers:
 * we're only interested in the actual types.
 *)
let rec check_abstract_expr syms rls ((x,t) as e) =
  let fi = ignore in
  let fe e = check_abstract_expr syms rls e in
  let ft t = check_abstract_type syms rls t in
  check_abstract_type syms rls t;
  match x with
  | BEXPR_deref e -> fe e
  | BEXPR_ref (i,ts) -> fi i
  | BEXPR_likely e -> fe e
  | BEXPR_unlikely e -> fe e
  | BEXPR_new e -> fe e
  | BEXPR_address e -> fe e

  | BEXPR_apply (e1,e2) -> fe e1; fe e2

  | BEXPR_apply_prim (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_direct (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_struct (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_stack (i,ts,e2) -> fi i; fe e2
  | BEXPR_tuple  es -> iter fe es
  | BEXPR_record es -> iter (fun (s,e) -> fe e) es
  | BEXPR_variant (s,e) -> fe e

  | BEXPR_get_n (i,e) -> fe e

  | BEXPR_closure (i,ts) -> fi i
  | BEXPR_name (i,ts) -> fi i
  | BEXPR_case (i,t') -> ft t'
  | BEXPR_match_case (i,e) -> fe e
  | BEXPR_case_arg (i,e) -> fe e
  | BEXPR_case_index e -> fe e

  | BEXPR_literal x -> ft t
  | BEXPR_expr (s,t1) -> ft t1
  | BEXPR_range_check (e1,e2,e3) -> fe e1; fe e2; fe e3
  | BEXPR_coerce (e,t) -> fe e; ft t


let check_abstract_exe syms rls exe =
 Flx_bexe.iter
   ~ft:(check_abstract_type syms rls)
   ~fe:(check_abstract_expr syms rls)
   exe 


let cal_polyvars syms bsym_table child_map =
  let absvars = Hashtbl.create 97 in
  Flx_bsym_table.iter (fun i bsym ->
  match bsym.Flx_bsym.bbdcl with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    if mem `Virtual props then () else
    let j = ref 0 in
    let tvars = ref (map (fun (_,i) -> incr j; i,!j-1) vs) in
    if !tvars <> [] then
    begin try 
  (*
  print_endline ("Checking abstract " ^ name ^ "<"^si i^">");
  *)
      iter (check_abstract_exe syms tvars) exes;
      if !tvars <> [] then begin
        (*
        print_endline ("Fun  " ^ name ^ "<" ^ string_of_bid i ^
          "> polyvars = " ^ catmap "," (fun (i,j)-> string_of_bid i) !tvars);
        *)
        Hashtbl.add absvars i (!tvars)
      end
    with Not_found -> ()
    end

  | BBDCL_procedure (props,vs,(ps,traint), exes) ->
    if mem `Virtual props then () else
    let j = ref 0 in
    let tvars = ref (map (fun (_,i) -> incr j; i,!j-1) vs) in
    if !tvars <> [] then
    begin try 
  (*
  print_endline ("Checking abstract " ^ name ^ "<"^si i^">");
  *)
      iter (check_abstract_exe syms tvars) exes;
      if !tvars <> [] then begin
        (*
        print_endline ("Proc " ^ name ^ "<" ^ string_of_bid i ^
          "> polyvars = " ^ catmap "," (fun (i,j) -> string_of_bid i) !tvars);
        *)
        Hashtbl.add absvars i (!tvars)
      end
    with Not_found -> ()
    end

  | _ -> ()      

  )
  bsym_table
  ;
  (* Now, add in all the children *)
  let polyvars = Hashtbl.create 97 in
  let merge i pvs = 
    let pvs' = try Hashtbl.find polyvars i with Not_found -> [] in
    Hashtbl.replace polyvars  i (pvs @ pvs')
  in
  Hashtbl.iter (fun i pvs ->
     merge i pvs;
     let kids = Flx_child.find_children child_map i in
     iter (fun j -> merge j pvs) kids
  )
  absvars
  ;
  let cast_a i e =
    let pvs = try Hashtbl.find polyvars i with Not_found -> [] in
    if pvs = [] then e else begin 
      (*
      print_endline ("Found polyvars for " ^ string_of_bid i);
      *)
      let varmap = map (fun (i,j) -> i, btyp_void) pvs in
      let t = 
          let ps =
            match Flx_bsym_table.find_bbdcl bsym_table i with
            | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps
            | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps
            | _ -> assert false
          in
          let pts = Flx_bparameter.get_btypes ps in
          let pt = match pts with | [t]->t | ts -> btyp_tuple ts in
          pt
      in
      let t = Flx_unify.list_subst syms.counter varmap t in
      (*
      print_endline ("COERCION arg(output) " ^ sbt syms.sym_table t);
      *)
      bexpr_coerce t (e,t)
    end
  in
  let cast_r i ((x,t) as e) =
    let pvs = try Hashtbl.find polyvars i with Not_found -> [] in
    if pvs = [] then e else begin 
      (*
      print_endline ("Found polyvars for " ^ string_of_bid i);
      *)
      let varmap = map (fun (i,j) -> i, btyp_void) pvs in
      let ta = 
        match Flx_bsym_table.find_bbdcl bsym_table i with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ret
        | _ -> assert false
      in
      let t' = Flx_unify.list_subst syms.counter varmap ta in
      (*
      print_endline ("COERCION result(input) " ^ sbt syms.sym_table t');
      *)
      bexpr_coerce t ((x,t'),t)
    end
  in
  let cal_ft i t =
    let pvs = try Hashtbl.find polyvars i with Not_found -> [] in
    if pvs = [] then t else begin 
      let varmap = map (fun (i,j) -> i, btyp_void) pvs in
      let tf = 
          let ps,ret =
            match Flx_bsym_table.find_bbdcl bsym_table i with
            | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,ret
            | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps, btyp_void
            | _ -> assert false
          in
          let pts = Flx_bparameter.get_btypes ps in
          let pt = match pts with | [t]->t | ts -> btyp_tuple ts in
          btyp_function (pt,ret)
      in
      let t' = Flx_unify.list_subst syms.counter varmap tf in
      (*
      print_endline ("fun type " ^ sbt syms.sym_table t');
      *)
      t'
    end
  in
  let rec fixexpr e = match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),t'),e2),t ->
    cast_r i (bexpr_apply t ((bexpr_closure (cal_ft i t') (i, polyfix syms polyvars i ts)), cast_a i (fixexpr e2)))
  | BEXPR_apply_prim (i,ts,e2),t ->
    cast_r i (bexpr_apply_prim t (i, polyfix syms polyvars i ts, cast_a i (fixexpr e2)))
  | BEXPR_apply_direct (i,ts,e2),t ->
    cast_r i (bexpr_apply_direct t (i, polyfix syms polyvars i ts, cast_a i (fixexpr  e2)))
  | BEXPR_apply_struct (i,ts,e2),t ->
    cast_r i (bexpr_apply_struct t (i, polyfix syms polyvars i ts, cast_a i (fixexpr  e2)))
  | BEXPR_apply_stack (i,ts,e2),t ->
    cast_r i (bexpr_apply_stack t (i, polyfix syms polyvars i ts, cast_a i (fixexpr e2)))
  | e -> Flx_bexpr.map ~fe:fixexpr e
  in

  Flx_bsym_table.update_bexes (List.map begin function
    | BEXE_call (sr,(BEXPR_closure (i,ts),t'),e) ->
        BEXE_call (sr,
          (bexpr_closure (cal_ft i t') (i, polyfix syms polyvars i ts)),
          cast_a i (fixexpr e))
    | BEXE_call_prim (sr,i,ts,e) ->
        BEXE_call_prim (sr, i, polyfix syms polyvars i ts, cast_a i (fixexpr e))
    | BEXE_call_direct (sr,i,ts,e) ->
        BEXE_call_direct (sr, i, polyfix syms polyvars i ts,
          cast_a i (fixexpr e))
    | BEXE_call_stack (sr,i,ts,e) ->
        BEXE_call_stack (sr, i, polyfix syms polyvars i ts,
          cast_a i (fixexpr e))
    | BEXE_jump_direct (sr,i,ts,e) ->
        BEXE_jump_direct (sr, i, polyfix syms polyvars i ts,
          cast_a i (fixexpr e))
    | x -> Flx_bexe.map ~fe:fixexpr x
  end) bsym_table;

  let polyfix2 i ts =
    match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_function _
    | BBDCL_procedure _ ->
        List.map begin function
        | BTYP_pointer _ -> btyp_pointer btyp_void
        | t -> t
        end ts
    | _ -> ts
  in
  let cast_a2 i ts e =
    try
    let counter = ref 0 in 
    let t,vsi = 
        let ps,vs =
          match Flx_bsym_table.find_bbdcl bsym_table i with
          | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,vs
          | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps,vs
          | _ -> raise Skip
        in
        let pts = Flx_bparameter.get_btypes ps in
        let pt = match pts with | [t]->t | ts -> btyp_tuple ts in
        let vsi = map (fun (s,i) -> i) vs in
        pt,vsi
    in
    let varmap =
      List.map2 begin fun i t ->
        i,
        match t with
        | BTYP_pointer _ ->
            incr counter;
            btyp_pointer btyp_void
        | _ -> t
      end vsi ts
    in
    if !counter = 0 then e else
    let t = Flx_unify.list_subst syms.counter varmap t in
    (*
    print_endline ("COERCION2 arg(output) " ^ sbt syms.sym_table t);
    *)
    bexpr_coerce t (e,t)
    with Skip -> e
  in

  let cast_r2 i ts ((x,t) as e) =
    try
    let counter = ref 0 in
    let ta,vsi = 
      match Flx_bsym_table.find_bbdcl bsym_table i with
      | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
          ret,map (fun (s,i) -> i) vs
      | _ -> raise Skip
    in
    let varmap =
      List.map2 begin fun i t ->
        i,
        match t with
        | BTYP_pointer _ ->
            incr counter;
            btyp_pointer btyp_void
        | _ -> t
      end vsi ts
    in
    if !counter = 0 then e else
    let t' = Flx_unify.list_subst syms.counter varmap ta in
    (*
    print_endline ("COERCION2 result(input) " ^ sbt syms.sym_table t');
    *)
    bexpr_coerce t ((x,t'),t)
    with Skip -> e
  in

  let cal_ft2 i ts t =
    try
    let counter = ref 0 in
    let tf,vsi = 
      let ps,ret,vs =
        match Flx_bsym_table.find_bbdcl bsym_table i with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,ret,vs
        | BBDCL_procedure (props,vs,(ps,traint),exes) -> ps, btyp_void, vs
      | _ -> raise Skip 
      in
      let pts = Flx_bparameter.get_btypes ps in
      let pt = match pts with | [t]->t | ts -> btyp_tuple ts in
      let tf = btyp_function (pt,ret) in
      let vsi = map (fun (s,i) -> i) vs in
      tf,vsi
    in
    let varmap =
      List.map2 begin fun i t ->
        i,
        match t with
        | BTYP_pointer _ ->
            incr counter;
            btyp_pointer btyp_void
        | _ -> t
      end vsi ts
    in
    if !counter = 0 then t else
    let t' = Flx_unify.list_subst syms.counter varmap tf in
    (*
    print_endline ("fun2 type " ^ sbt syms.sym_table t');
    *)
    t'
    with Skip -> t
  in

  let rec fixexpr2 e = match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),t'),e2),t ->
    cast_r2 i ts (bexpr_apply t ((bexpr_closure (cal_ft2 i ts t') (i, polyfix2 i ts)), cast_a2 i ts (fixexpr2 e2)))
  | BEXPR_apply_prim (i,ts,e2),t ->
    cast_r2 i ts (bexpr_apply_prim t (i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e2)))
  | BEXPR_apply_direct (i,ts,e2),t ->
    cast_r2 i ts (bexpr_apply_direct t (i, polyfix2 i ts, cast_a2 i ts (fixexpr2  e2)))
  | BEXPR_apply_struct (i,ts,e2),t ->
    cast_r2 i ts (bexpr_apply_struct t (i, polyfix2 i ts, cast_a2 i ts (fixexpr2  e2)))
  | BEXPR_apply_stack (i,ts,e2),t ->
    cast_r2 i ts (bexpr_apply_stack t (i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e2)))
  | e -> Flx_bexpr.map ~fe:fixexpr2 e
  in

  Flx_bsym_table.update_bexes (List.map begin function
    | BEXE_call (sr,(BEXPR_closure (i,ts),t'),e) ->
        BEXE_call (sr,
          (bexpr_closure (cal_ft i t') (i, polyfix2 i ts)),
          cast_a2 i ts (fixexpr2 e))
    | BEXE_call_prim (sr,i,ts,e) ->
        BEXE_call_prim (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
    | BEXE_call_direct (sr,i,ts,e) ->
        BEXE_call_direct (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
    | BEXE_call_stack (sr,i,ts,e) ->
        BEXE_call_stack (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
    | BEXE_jump_direct (sr,i,ts,e) ->
        BEXE_jump_direct (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
    | x -> Flx_bexe.map ~fe:fixexpr2 x
  end) bsym_table
