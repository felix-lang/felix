
open Flx_btype
open Flx_bexpr
open Flx_bid

let rec process_expr new_table bsym_table counter expr = 
  let f_bexpr expr = process_expr new_table bsym_table counter expr in
  let remap expr = Flx_bexpr.map ~f_bexpr expr in
  (* perform top down expansion, required for normal order *)
  match expr with
  (* coercion with argument free of reducible coercions *)
  | BEXPR_apply ((BEXPR_lambda (i,vt,e),ft), arg),_ ->
    (* beta reduction, normal order *)
    let rec replace e = 
      match e with
      | BEXPR_varname (j,_),_ when i = j -> arg
      | _ -> Flx_bexpr.map ~f_bexpr:replace e
    in
    remap (replace expr)
      
  | BEXPR_lambda (i,dt,((x,ct) as expr)),ft ->
    let fidx = fresh_bid counter in
    let pidx = fresh_bid counter in

    (* parameter symbol table entry *)
    let bbdcl = Flx_bbdcl.bbdcl_val ([],dt, `Var)  in
    let pname = "_lamba_param" ^ string_of_int pidx in
    let bsym = Flx_bsym.create pname bbdcl in
    Flx_bsym_table.add new_table pidx (Some fidx) bsym;

    (* code for wrapper function *)
    let p = bexpr_varname dt (pidx, []) in 
    let rec replace e = 
      match e with
      | BEXPR_varname (j,_),_ when i = j -> p
      | _ -> Flx_bexpr.map ~f_bexpr:replace e
    in
    let expr = remap (replace expr) in
    let exe = 
       [Flx_bexe.bexe_fun_return (Flx_srcref.dummy_sr, expr)]
    in 

    (* wrapper function *)
    let effects = Flx_btype.btyp_unit () in
    let param = {Flx_bparameter.pid=pname;pindex=pidx;pkind=`PVal;ptyp=dt} in
    let params = [param], None in
    let bbdcl = Flx_bbdcl.bbdcl_fun ([],[],params,ct,effects,exe) in
    let bsym = Flx_bsym.create ("_lambda" ^ string_of_int fidx) bbdcl in
    Flx_bsym_table.add new_table  fidx None bsym;
    let f = bexpr_closure ft (fidx,[]) in
    f

  (* descend into subterms *)
  | _ -> remap expr


let process_exe new_table bsym_table counter exe =
  let newexe = Flx_bexe.map ~f_bexpr:(process_expr new_table bsym_table counter) exe in
(*
  print_endline ("Old bexe=" ^ Flx_print.sbx bsym_table exe);
  print_endline ("New bexe=" ^ Flx_print.sbx bsym_table newexe);
*)
  newexe


let process_exes new_table bsym_table counter exes =
  List.map (process_exe new_table bsym_table counter) exes 

let process_entry new_table bsym_table counter parent i (bsym : Flx_bsym.t) =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun (props,vs,ps,ret,effects,exes) ->
(*
print_endline ("Processing function " ^ Flx_bsym.id bsym);
*)
    let exes = process_exes new_table bsym_table counter exes in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props, vs, ps, ret,effects, exes) in 
    let bsym = Flx_bsym.replace_bbdcl bsym bbdcl in
    Flx_bsym_table.add new_table i parent bsym 

  | bbdcl -> Flx_bsym_table.add new_table i parent bsym


let strip_lambdas syms bsym_table = 
  let new_table = Flx_bsym_table.create () in
  Flx_bsym_table.iter 
   (fun i parent bsym -> process_entry new_table bsym_table syms.Flx_mtypes2.counter parent i bsym)
    bsym_table
  ;
  new_table

