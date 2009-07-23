open Flx_types
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
  iter  (fun (i,j) -> ts.(j) <- BTYP_void) poly
  ;
  let ts = Array.to_list ts in
  ts

let remove i ls = filter (fun (k,j) -> i <> k) ls

let rec check_abstract_type syms bbdfns rls (t:btypecode_t) = match t with
    | BTYP_pointer (BTYP_var _) -> ()
    | BTYP_var (i,_) ->
       (*
       print_endline ("Removing type variable " ^ string_of_int i);
       *)
       rls := remove i !rls;
       if !rls = [] then raise Not_found
    | t' -> iter_btype (check_abstract_type syms bbdfns rls) t'

(* note this routine doesn't check types in ts lists, because
 * these apply to variables including parameters as qualifiers:
 * we're only interested in the actual types.
 *)
let rec check_abstract_expr syms bbdfns rls ((x,t) as e) =
  let fi = ignore in
  let fe e = check_abstract_expr syms bbdfns rls e in
  let ft t = check_abstract_type syms bbdfns rls t in
  check_abstract_type syms bbdfns rls t;
  match x with
  | BEXPR_deref e -> fe e
  | BEXPR_ref (i,ts) -> fi i
  | BEXPR_likely e -> fe e
  | BEXPR_unlikely e -> fe e
  | BEXPR_new e -> fe e
  | BEXPR_address e -> fe e
  | BEXPR_not e -> fe e

  | BEXPR_apply (e1,e2) -> fe e1; fe e2

  | BEXPR_apply_prim (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_direct (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_struct (i,ts,e2) -> fi i; fe e2
  | BEXPR_apply_stack (i,ts,e2) -> fi i; fe e2
  | BEXPR_tuple  es -> iter fe es
  | BEXPR_record es -> iter (fun (s,e) -> fe e) es
  | BEXPR_variant (s,e) -> fe e

  | BEXPR_get_n (i,e) -> fe e
  | BEXPR_get_named (i,e) -> fi i; fe e

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


let check_abstract_exe syms bbdfns rls (exe:bexe_t) =
 iter_bexe 
   ignore 
   (check_abstract_expr syms bbdfns rls) 
   (check_abstract_type syms bbdfns rls) 
   ignore 
   ignore 
   exe 


let cal_polyvars syms bbdfns child_map =
  let absvars = Hashtbl.create 97 in
  Hashtbl.iter (fun i (name,parent,sr,bbdfn) -> 
  match bbdfn with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    if mem `Virtual props then () else
    let j = ref 0 in
    let tvars = ref (map (fun (_,i) -> incr j; i,!j-1) vs) in
    if !tvars <> [] then
    begin try 
  (*
  print_endline ("Checking abstract " ^ name ^ "<"^si i^">");
  *)
      iter (check_abstract_exe syms bbdfns tvars) exes;
      if !tvars <> [] then begin
        print_endline ("Fun  " ^ name ^ "<"^si i ^ "> polyvars = " ^ catmap ","
        (fun (i,j)-> si i) !tvars);
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
      iter (check_abstract_exe syms bbdfns tvars) exes;
      if !tvars <> [] then begin
        print_endline ("Proc " ^ name ^ "<"^si i ^ "> polyvars = " ^ catmap ","
        (fun (i,j) -> si i) !tvars);
        Hashtbl.add absvars i (!tvars)
      end
    with Not_found -> ()
    end

  | _ -> ()      

  )
  bbdfns
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
      print_endline ("Found polyvars for " ^ si i);
      let varmap = map (fun (i,j) -> i,BTYP_void) pvs in
      let t = 
          let ps = match Hashtbl.find bbdfns i with
          | name, parent, sr, bbdfn -> match bbdfn with
          | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps
          | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps
          | _ -> assert false
          in
          let pts = map (fun {ptyp=t}->t) ps in
          let pt = match pts with | [t]->t | ts -> BTYP_tuple ts in
          pt
      in
      let t = Flx_unify.list_subst syms.counter varmap t in
      print_endline ("COERCION arg(output) " ^ sbt syms.dfns t);
      BEXPR_coerce (e,t),t
    end
  in
  let cast_r i ((x,t) as e) =
    let pvs = try Hashtbl.find polyvars i with Not_found -> [] in
    if pvs = [] then e else begin 
      print_endline ("Found polyvars for " ^ si i);
      let varmap = map (fun (i,j) -> i,BTYP_void) pvs in
      let ta = 
        match Hashtbl.find bbdfns i with
        | name, parent, sr, bbdfn -> match bbdfn with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ret
        | _ -> assert false
      in
      let t' = Flx_unify.list_subst syms.counter varmap ta in
      print_endline ("COERCION result(input) " ^ sbt syms.dfns t');
      BEXPR_coerce ((x,t'),t),t
    end
  in
  let cal_ft i t =
    let pvs = try Hashtbl.find polyvars i with Not_found -> [] in
    if pvs = [] then t else begin 
      let varmap = map (fun (i,j) -> i,BTYP_void) pvs in
      let tf = 
          let ps,ret = match Hashtbl.find bbdfns i with
          | name, parent, sr, bbdfn -> match bbdfn with
          | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,ret
          | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps, BTYP_void
          | _ -> assert false
          in
          let pts = map (fun {ptyp=t}->t) ps in
          let pt = match pts with | [t]->t | ts -> BTYP_tuple ts in
          BTYP_function (pt,ret)
      in
      let t' = Flx_unify.list_subst syms.counter varmap tf in
      print_endline ("fun type " ^ sbt syms.dfns t');
      t'
    end
  in
  let rec fixexpr e = match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),t'),e2),t ->
    cast_r i (BEXPR_apply ((BEXPR_closure (i, polyfix syms polyvars i ts),cal_ft i t'), cast_a i (fixexpr e2)),t)
  | BEXPR_apply_prim (i,ts,e2),t ->
    cast_r i (BEXPR_apply_prim (i, polyfix syms polyvars i ts, cast_a i (fixexpr e2)),t)
  | BEXPR_apply_direct (i,ts,e2),t ->
    cast_r i (BEXPR_apply_direct (i, polyfix syms polyvars i ts, cast_a i (fixexpr  e2)),t)
  | BEXPR_apply_struct (i,ts,e2),t ->
    cast_r i (BEXPR_apply_struct (i, polyfix syms polyvars i ts, cast_a i (fixexpr  e2)),t)
  | BEXPR_apply_stack (i,ts,e2),t ->
    cast_r i (BEXPR_apply_stack (i, polyfix syms polyvars i ts, cast_a i (fixexpr e2)),t)
  | e -> map_tbexpr ident fixexpr ident e
  in

  let fixexe x = match x with
  | BEXE_call (sr,(BEXPR_closure (i,ts),t'),e) ->
    BEXE_call (sr,(BEXPR_closure (i, polyfix syms polyvars i ts), cal_ft i t'), cast_a i (fixexpr e))
  | BEXE_call_prim (sr,i,ts,e) ->
    BEXE_call_prim (sr,i, polyfix syms polyvars i ts, cast_a i (fixexpr e))
  | BEXE_call_direct (sr,i,ts,e) ->
    BEXE_call_direct (sr,i, polyfix syms polyvars i ts, cast_a i (fixexpr e))
  | BEXE_call_stack (sr,i,ts,e) ->
    BEXE_call_stack (sr,i, polyfix syms polyvars i ts, cast_a i (fixexpr e))
  | BEXE_jump_direct (sr,i,ts,e) ->
    BEXE_jump_direct (sr,i, polyfix syms polyvars i ts, cast_a i (fixexpr e))
  | x -> map_bexe ident fixexpr ident ident ident x
  in

  let fixexes exes = map fixexe exes in
  Hashtbl.iter (fun i (name,parent,sr,bbdfn) -> 
  match bbdfn with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let exes = fixexes exes in
    let bbdfn = BBDCL_function (props,vs,(ps,traint),ret,exes) in
    Hashtbl.replace bbdfns i (name,parent,sr,bbdfn)

  | BBDCL_procedure (props,vs,(ps,traint), exes) ->
    let exes = fixexes exes in
    let bbdfn = BBDCL_procedure (props,vs,(ps,traint), exes) in
    Hashtbl.replace bbdfns i (name,parent,sr,bbdfn)
  | _ -> ()      

  )
  bbdfns
  ;

  let polyfix2 i ts =
    match Hashtbl.find bbdfns i with
    | name, parent, sr, bbdfn -> match bbdfn with
    | BBDCL_function _
    | BBDCL_procedure _ ->
      map (fun t -> match t with | BTYP_pointer _ -> BTYP_pointer BTYP_void | _ -> t) ts
    | _ -> ts
  in
  let cast_a2 i ts e =
    try
    let counter = ref 0 in 
    let t,vsi = 
        let ps,vs = match Hashtbl.find bbdfns i with
        | name, parent, sr, bbdfn -> match bbdfn with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,vs
        | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps,vs
        | _ -> raise Skip
        in
        let pts = map (fun {ptyp=t}->t) ps in
        let pt = match pts with | [t]->t | ts -> BTYP_tuple ts in
        let vsi = map (fun (s,i) -> i) vs in
        pt,vsi
    in
    let varmap = map2 (fun i t -> i, match t with | BTYP_pointer _ -> incr counter; BTYP_pointer BTYP_void | _ -> t ) vsi ts in
    if !counter = 0 then e else
    let t = Flx_unify.list_subst syms.counter varmap t in
    print_endline ("COERCION2 arg(output) " ^ sbt syms.dfns t);
    BEXPR_coerce (e,t),t
    with Skip -> e
  in

  let cast_r2 i ts ((x,t) as e) =
    try
    let counter = ref 0 in
    let ta,vsi = 
      match Hashtbl.find bbdfns i with
      | name, parent, sr, bbdfn -> match bbdfn with
      | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ret,map (fun (s,i) -> i) vs
      | _ -> raise Skip
    in
    let varmap = map2 (fun i t -> i, match t with | BTYP_pointer _ -> incr counter; BTYP_pointer BTYP_void | _ -> t ) vsi ts in
    if !counter = 0 then e else
    let t' = Flx_unify.list_subst syms.counter varmap ta in
    print_endline ("COERCION2 result(input) " ^ sbt syms.dfns t');
    BEXPR_coerce ((x,t'),t),t
    with Skip -> e
  in

  let cal_ft2 i ts t =
    try
    let counter = ref 0 in
    let tf,vsi = 
      let ps,ret,vs = match Hashtbl.find bbdfns i with
      | name, parent, sr, bbdfn -> match bbdfn with
      | BBDCL_function (props,vs,(ps,traint),ret,exes) -> ps,ret,vs
      | BBDCL_procedure (props,vs,(ps,traint), exes) -> ps, BTYP_void,vs
      | _ -> raise Skip 
      in
      let pts = map (fun {ptyp=t}->t) ps in
      let pt = match pts with | [t]->t | ts -> BTYP_tuple ts in
      let tf = BTYP_function (pt,ret) in
      let vsi = map (fun (s,i) -> i) vs in
      tf,vsi
    in
    let varmap = map2 (fun i t -> i, match t with | BTYP_pointer _ -> incr counter; BTYP_pointer BTYP_void | _ -> t ) vsi ts in
    if !counter = 0 then t else
    let t' = Flx_unify.list_subst syms.counter varmap tf in
    print_endline ("fun2 type " ^ sbt syms.dfns t');
    t'
    with Skip -> t
  in

  let rec fixexpr2 e = match e with
  | BEXPR_apply ((BEXPR_closure (i,ts),t'),e2),t ->
    cast_r2 i ts (BEXPR_apply ((BEXPR_closure (i, polyfix2 i ts),cal_ft2 i ts t'), cast_a2 i ts (fixexpr2 e2)),t)
  | BEXPR_apply_prim (i,ts,e2),t ->
    cast_r2 i ts (BEXPR_apply_prim (i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e2)),t)
  | BEXPR_apply_direct (i,ts,e2),t ->
    cast_r2 i ts (BEXPR_apply_direct (i, polyfix2 i ts, cast_a2 i ts (fixexpr2  e2)),t)
  | BEXPR_apply_struct (i,ts,e2),t ->
    cast_r2 i ts (BEXPR_apply_struct (i, polyfix2 i ts, cast_a2 i ts (fixexpr2  e2)),t)
  | BEXPR_apply_stack (i,ts,e2),t ->
    cast_r2 i ts (BEXPR_apply_stack (i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e2)),t)
  | e -> map_tbexpr ident fixexpr2 ident e
  in

  let fixexe2 x = match x with
  | BEXE_call (sr,(BEXPR_closure (i,ts),t'),e) ->
    BEXE_call (sr,(BEXPR_closure (i, polyfix2 i ts), cal_ft i t'), cast_a2 i ts (fixexpr2 e))
  | BEXE_call_prim (sr,i,ts,e) ->
    BEXE_call_prim (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
  | BEXE_call_direct (sr,i,ts,e) ->
    BEXE_call_direct (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
  | BEXE_call_stack (sr,i,ts,e) ->
    BEXE_call_stack (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
  | BEXE_jump_direct (sr,i,ts,e) ->
    BEXE_jump_direct (sr,i, polyfix2 i ts, cast_a2 i ts (fixexpr2 e))
  | x -> map_bexe ident fixexpr2 ident ident ident x
  in
  let fixexes2 exes = map fixexe2 exes in


  Hashtbl.iter (fun i (name,parent,sr,bbdfn) -> 
  match bbdfn with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let exes = fixexes2 exes in
    let bbdfn = BBDCL_function (props,vs,(ps,traint),ret,exes) in
    Hashtbl.replace bbdfns i (name,parent,sr,bbdfn)

  | BBDCL_procedure (props,vs,(ps,traint), exes) ->
    let exes = fixexes2 exes in
    let bbdfn = BBDCL_procedure (props,vs,(ps,traint), exes) in
    Hashtbl.replace bbdfns i (name,parent,sr,bbdfn)
  | _ -> ()      

  )
  bbdfns



