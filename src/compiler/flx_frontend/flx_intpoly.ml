open Flx_types
open Flx_maps
open List
open Flx_util
open Flx_print
open Flx_mtypes2

let remove i ls = filter (fun (k,j) -> i <> k) ls

let rec check_abstract_type syms bbdfns rls (t:btypecode_t) = match t with
    | `BTYP_pointer (`BTYP_var _) -> ()
    | `BTYP_var (i,_) -> 
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
  | `BEXPR_deref e -> fe e
  | `BEXPR_ref (i,ts) -> fi i
  | `BEXPR_likely e -> fe e
  | `BEXPR_unlikely e -> fe e
  | `BEXPR_new e -> fe e
  | `BEXPR_not e -> fe e

  | `BEXPR_apply (e1,e2) -> fe e1; fe e2

  | `BEXPR_apply_prim (i,ts,e2) -> fi i; fe e2
  | `BEXPR_apply_direct (i,ts,e2) -> fi i; fe e2
  | `BEXPR_apply_struct (i,ts,e2) -> fi i; fe e2
  | `BEXPR_apply_stack (i,ts,e2) -> fi i; fe e2
  | `BEXPR_tuple  es -> iter fe es
  | `BEXPR_record es -> iter (fun (s,e) -> fe e) es
  | `BEXPR_variant (s,e) -> fe e

  | `BEXPR_get_n (i,e) -> fe e
  | `BEXPR_get_named (i,e) -> fi i; fe e

  | `BEXPR_closure (i,ts) -> fi i
  | `BEXPR_name (i,ts) -> fi i
  | `BEXPR_case (i,t') -> ft t'
  | `BEXPR_match_case (i,e) -> fe e
  | `BEXPR_case_arg (i,e) -> fe e
  | `BEXPR_case_index e -> fe e

  | `BEXPR_literal x -> ft t
  | `BEXPR_expr (s,t1) -> ft t1
  | `BEXPR_range_check (e1,e2,e3) -> fe e1; fe e2; fe e3
  | `BEXPR_coerce (e,t) -> fe e; ft t


let check_abstract_exe syms bbdfns rls (exe:bexe_t) =
 iter_bexe 
   ignore 
   (check_abstract_expr syms bbdfns rls) 
   (check_abstract_type syms bbdfns rls) 
   ignore 
   ignore 
   exe 


let cal_polyvars syms bbdfns =
  let absvars = Hashtbl.create 97 in
  Hashtbl.iter (fun i (name,parent,sr,bbdfn) -> 
  match bbdfn with
  | `BBDCL_function (props,vs,(ps,traint),ret,exes) ->
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

  | `BBDCL_procedure (props,vs,(ps,traint), exes) ->
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
  bbdfns;
  absvars

