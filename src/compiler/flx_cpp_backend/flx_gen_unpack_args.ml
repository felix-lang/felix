open List

open Flx_bbdcl
open Flx_beta
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_cexpr
open Flx_ctorgen
open Flx_ctypes
open Flx_display
open Flx_egen
open Flx_exceptions
open Flx_label
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_name
open Flx_ogen
open Flx_options
open Flx_pgen
open Flx_print
open Flx_types
open Flx_typing
open Flx_unify
open Flx_util
open Flx_gen_helper
open Flx_bid
open Flx_btype_subst

let unpack_args syms bsym_table shapes shape_table label_info index vs ts sr argtype bps params =
  assert (ts = []);
  let xps,_ = bps in
  match xps with
  | Flx_ast.Slist [] -> ""
  | Flx_ast.Satom {pindex=i} ->
    if Hashtbl.mem syms.instances (i, ts)
    && not (argtype = btyp_tuple [] || argtype = btyp_void ())
    then
      "  " ^ cpp_instance_name syms bsym_table i ts ^ " = _arg;\n"
    else ""
  | _ ->
    let ge' e : Flx_ctypes.cexpr_t = 
      Flx_egen.gen_expr' syms bsym_table shapes shape_table label_info index vs ts sr e 
    in
    let arg = bexpr_literal argtype {Flx_literal.felix_type="";internal_value=""; c_value="_arg"} in 

    if Flx_btype.islinear_type argtype then
      let counter = ref 0 in
      List.fold_left begin fun s i ->
        let n = !counter in incr counter;
        if Hashtbl.mem syms.instances (i,ts)
        then
            let component = match argtype with
            | BTYP_array (v,idxt) -> 
              let index = bexpr_const_case (n,idxt) in
              Flx_ixgen.handle_get_n_array_clt syms bsym_table ge' index idxt v idxt argtype arg  
            | BTYP_tuple ls -> 
              let rt = List.nth ls n in
              let dummy = arg in (* dont care doesn't seem to be used *)
              Flx_ixgen.handle_get_n syms bsym_table ls rt ge' dummy argtype n arg
            | _ -> assert false
            in
            let component = Flx_cexpr.string_of_cexpr component in
            s ^ "  " ^ cpp_instance_name syms bsym_table i ts ^ " = " ^ component ^";\n"
        else s (* elide initialisation of elided variable *)
      end "" params
    else (* not clt *)
      let inits = List.map (fun ({pindex=pindex; ptyp=ptyp},prj) -> 
        let init = 
          match prj with
          | None -> arg
          | Some (_,BTYP_function (_,c) as prj) -> bexpr_apply c (prj,arg)
          | _ -> assert false
        in
        let init = Flx_cexpr.string_of_cexpr (ge' init) in
          "  " ^ cpp_instance_name syms bsym_table pindex ts ^ " = " ^ init ^ ";\n"
      )
      (Flx_bparams.get_prjs bps)
      in
      String.concat "" inits


