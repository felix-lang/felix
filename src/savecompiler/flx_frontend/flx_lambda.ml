
open Flx_btype
open Flx_bexpr
open Flx_bid
open Flx_bexe

let subst_expr expr varidx arg =
  let rec replace e = match e with
    | BEXPR_varname (j,_),_ when varidx = j -> arg
    | _ -> Flx_bexpr.map ~f_bexpr:replace e
  in replace expr

let add_var_param new_table fidx pidx dt =
  (* parameter symbol table entry *)
  let bbdcl = Flx_bbdcl.bbdcl_val ([],dt, `Var)  in
  let pname = "_lambda_local" ^ string_of_int pidx in
  let bsym = Flx_bsym.create pname bbdcl in
  Flx_bsym_table.add new_table pidx (Some fidx) bsym;
  pname 

let add_wrapper_function new_table parent fidx pidx dt effects ct exes = 
  let pname = add_var_param new_table fidx pidx dt in
  (* wrapper function *)
  let effects = Flx_btype.btyp_unit () in
  let param = {Flx_bparameter.pid=pname;pindex=pidx;pkind=`PVar;ptyp=dt} in
  let params = Flx_ast.Satom param, None in
  let bbdcl = Flx_bbdcl.bbdcl_fun ([],[],params,ct,effects,exes) in
  let fname = "_lambda" ^ string_of_int fidx in
  let bsym = Flx_bsym.create fname bbdcl in
  Flx_bsym_table.add new_table fidx parent bsym;
  fname

let add_label new_table sr fidx label_index =
  let s = "_lab" ^ string_of_int label_index in
  let bbdcl = Flx_bbdcl.bbdcl_label s in
  let bsym = Flx_bsym.create ~sr s bbdcl in
  Flx_bsym_table.add new_table label_index (Some fidx) bsym;
  s
 
let add_array_map new_table counter parent fidx src lambda = 
  match src,lambda with
  | (_,(BTYP_array (elt_t, BTYP_unitsum n) as srct)),
    (_,BTYP_function (d,c)) 
  ->
  assert (elt_t = d);
  let dstt = btyp_array (c, btyp_unitsum n) in
  let effects = btyp_unit () in
  let sr = Flx_srcref.dummy_sr in 

  let fidx = fresh_bid counter in (* function *)
  let iidx = fresh_bid counter in (* loop index *)
  let pidx = fresh_bid counter in (* parameter index *)
  let didx = fresh_bid counter in (* target local index *)
  let si x = string_of_int x in

  let int_t = btyp_int () in

  let iname = add_var_param new_table fidx iidx int_t in
  let dname = add_var_param new_table fidx didx dstt in

  (* destination element pointer type *)
  let dptr_t = btyp_pointer c in 

  (* index variable *)
  let idx_val = bexpr_varname int_t (iidx,[]) in

  (* source array element *)
  let srcprj = bexpr_aprj idx_val srct elt_t in
  let srcval = bexpr_apply elt_t (srcprj, src) in
  (* mapped value *)
  let dval = bexpr_apply c (lambda, srcval) in

  (* destination array pointer *)
  let dstprj = bexpr_aprj idx_val (btyp_pointer dstt) dptr_t in
  let pdst = bexpr_apply dptr_t (dstprj, bexpr_ref (btyp_pointer dstt) (didx,[])) in
  (* store instruction *)
  let map_elt = bexe_storeat (sr,pdst,dval) in

  let label_index = fresh_bid counter in
  let label_name = add_label new_table sr fidx label_index in

  let bool_t = btyp_unitsum 2 in
  let pint_t = btyp_pointer int_t in

  let idx_ptr = bexpr_ref pint_t (iidx,[]) in
  let init_idx = bexpr_literal_int (n-1) in
  let isnonneg_idx = bexpr_apply_prim bool_t (Flx_concordance.flx_isnonneg_int, [], idx_val) in
  let decr_idx = bexe_call_prim (sr,Flx_concordance.flx_decr_int, [], idx_ptr) in

  let exes = [
     bexe_init (sr,iidx,init_idx); (* i = n - 1 *)
     bexe_label (sr,label_index); 
       map_elt;
       decr_idx;
       bexe_ifgoto (sr, isnonneg_idx, label_index);
     bexe_fun_return (sr,(bexpr_varname dstt (didx,[])))
  ]
  in
  let fname = add_wrapper_function new_table parent fidx pidx srct effects dstt exes in
  fidx
  | _ -> 
   print_endline ("Bad array map");
   assert false

let rec process_expr new_table bsym_table counter parent expr = 
  let f_bexpr expr = process_expr new_table bsym_table counter parent expr in
  let remap expr = Flx_bexpr.map ~f_bexpr expr in
  (* perform top down expansion, required for normal order *)
  match expr with
  (* coercion with argument free of reducible coercions *)
  | BEXPR_apply ((BEXPR_lambda (i,vt,e),ft), arg),_ ->
    remap (subst_expr e i arg)
     
  | BEXPR_lambda (i,dt,((x,ct) as expr)),ft ->
    let fidx = fresh_bid counter in
    let pidx = fresh_bid counter in
    let p = bexpr_varname dt (pidx, []) in 
    let expr = remap (subst_expr expr i p) in
    let effects = Flx_btype.btyp_unit () in
    let exes = 
       [Flx_bexe.bexe_fun_return (Flx_srcref.dummy_sr, expr)]
    in 
    let fname = add_wrapper_function new_table parent fidx pidx dt effects ct exes in
    let f = bexpr_closure ft (fidx,[]) in
    f

  (* descend into subterms *)
  | _ -> remap expr


let process_exe new_table bsym_table counter parent exe =
  let newexe = Flx_bexe.map ~f_bexpr:(process_expr new_table bsym_table counter parent) exe in
(*
  print_endline ("Old bexe=" ^ Flx_print.sbx bsym_table exe);
  print_endline ("New bexe=" ^ Flx_print.sbx bsym_table newexe);
*)
  newexe


let process_exes new_table bsym_table counter parent exes =
  List.map (process_exe new_table bsym_table counter parent) exes 

let process_entry new_table bsym_table counter parent i (bsym : Flx_bsym.t) =
  match bsym.Flx_bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun (props,vs,ps,ret,effects,exes) ->
(*
print_endline ("Processing function " ^ Flx_bsym.id bsym);
*)
    let exes = process_exes new_table bsym_table counter parent exes in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props, vs, ps, ret,effects, exes) in 
    let bsym = Flx_bsym.replace_bbdcl bsym bbdcl in
    Flx_bsym_table.add new_table i parent bsym 

  | bbdcl -> Flx_bsym_table.add new_table i parent bsym


let strip_lambdas syms bsym_table = 
  let new_table = Flx_bsym_table.create_from bsym_table in
  Flx_bsym_table.iter 
   (fun i parent bsym -> process_entry new_table bsym_table syms.Flx_mtypes2.counter parent i bsym)
    bsym_table
  ;
  new_table

