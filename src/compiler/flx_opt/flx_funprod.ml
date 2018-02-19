let sbt b t = Flx_print.sbt b t
let noeffects = Flx_btype.btyp_unit ()

let rec reduce_bexpr syms prodmap pa sr e =
  let f_bexpr e = reduce_bexpr syms prodmap pa sr e in
  match Flx_bexpr.map ~f_bexpr e with
  | Flx_bexpr.BEXPR_funprod e,t ->
    let i = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    Hashtbl.add prodmap i (pa,(sr,e,t));
    Flx_bexpr.bexpr_closure t (i,[])
 
  | e -> e

let reduce_exe syms prodmap pa exe =
  let sr = Flx_bexe.get_srcref exe in 
  Flx_bexe.map ~f_bexpr:(reduce_bexpr syms prodmap pa sr) exe

let elim_funprods syms bsym_table =
  let prodmap = Hashtbl.create 97 in 
  Flx_bsym_table.iter
  (fun id pa sym -> 
     match sym.Flx_bsym.bbdcl with 
     | Flx_bbdcl.BBDCL_fun (prop, bvs, ps, res, effects,exes) ->
       let exes = List.map (reduce_exe syms prodmap pa) exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res,effects,exes) in
       Flx_bsym_table.update_bbdcl bsym_table id bbdcl
     | _ -> () 
  )
  bsym_table
  ;
  Hashtbl.iter (fun i (pa,(sr,e,pt)) ->
(*
    print_endline ("Add function product " ^ string_of_int i);
    print_endline ("Child of  " ^ match pa with | Some p -> string_of_int p | None -> "NONE");
*)
    let _,e_t = e in
(*
    print_endline ("Tuple of functions has type " ^ sbt bsym_table e_t);
*)
    let dt,ct = match pt with | Flx_btype.BTYP_function (dt,ct) -> dt,ct | _ -> assert false in
(*
    print_endline ("Final product type: " ^ sbt bsym_table pt);
*)

(* dt: domain of whole function, ct: codomain of whole function 
   pt = dt -> ct, type of whole function
*)

    (* add function parameter *)
    let pindex = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    let bbdcl = Flx_bbdcl.bbdcl_val ([],dt,`Var) in 
    let bsym = Flx_bsym.create ~sr "_a" bbdcl in
    Flx_bsym_table.add bsym_table pindex (Some i) bsym;
    let param = Flx_bexpr.bexpr_varname dt (pindex,[]) in

    (* calculate domain and codomain component types *)
    let ds = match dt with 
      | Flx_btype.BTYP_tuple ds -> ds 
      | Flx_btype.BTYP_array (t,Flx_btype.BTYP_unitsum n) -> Flx_list.repeat t n 
      | _ -> assert false 
    in
    let cs = match ct with 
      | Flx_btype.BTYP_tuple cs -> cs 
      | Flx_btype.BTYP_array (t,Flx_btype.BTYP_unitsum n) -> Flx_list.repeat t n 
      | _ -> assert false 
    in

(* ds, cs: domains and codomains of component functions *)
    let n = List.length ds in
    assert (n = List.length cs);
(*
print_endline "Calculate function product ... ";
*)
    (* generate applications f.i a.i *)
    let fts = List.combine ds cs in
    let ixs = Flx_list.nlist n in
    let appls = List.map2  (fun i (pd,pc) ->
(* pd,pc: domain and codomain of component function *)
      let paramprj = Flx_bexpr.bexpr_prj i dt pd in
(* projection dt -> pd to extract component value a.i *)
      let elt = Flx_bexpr.bexpr_apply pd (paramprj, param) in
(* elt = a.i : pd component of parameter *)

      let ft = Flx_btype.btyp_function (pd,pc) in
(* ft: pd -> pc, type of component function *)
      let fnprj = Flx_bexpr.bexpr_prj i e_t ft in
(* fnprj, projection of tuple of functions to get component function f.i
   type function tuple is pt, component function is ft
*)
      let fn = Flx_bexpr.bexpr_apply ft (fnprj, e) in
(* fn, the actual component function, type ft, result from applying
    projection fnprj: pt -> ft to function product e
*)
      let result = Flx_bexpr.bexpr_apply pc (fn,elt) in
(* finally the resulting value component i, type pc from applying
   component function type ft = pd -> pc to elt, of type pd
*)
      result
      )
      ixs fts 
    in
(*
print_endline " .. Calculate function product";
*)
    let re = Flx_bexpr.bexpr_tuple ct appls in (* product codomain type *)
    let exe = Flx_bexe.bexe_fun_return (sr,re) in 
    let params = Flx_ast.Satom {Flx_bparameter.pid="_a"; pindex=pindex; pkind=`PVar;ptyp=dt},None in
    let props = [`Generated "funprod"] in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props,[],params, ct,noeffects,[exe]) in 
    let bsym = Flx_bsym.create "funprod" bbdcl in
    Flx_bsym_table.add bsym_table i pa bsym
  )
  prodmap
  ;
  bsym_table


