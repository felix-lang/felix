(* handles <f,g,h> etc, the mediating morphism of a product,

   <f,g,h> (x) = (\prod (f,g,h)) (x,x,x)

   but without duplicating the argument
*)
let noeffects = Flx_btype.btyp_unit ()

let rec reduce_bexpr syms prodmap pa sr e =
  let f_bexpr e = reduce_bexpr syms prodmap pa sr e in
  match Flx_bexpr.map ~f_bexpr e with
  | Flx_bexpr.BEXPR_lrangle e,t ->
    let i = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    Hashtbl.add prodmap i (pa,(sr,e,t));
    Flx_bexpr.bexpr_closure t (i,[])
 
  | e -> e

let reduce_exe syms prodmap pa exe =
  let sr = Flx_bexe.get_srcref exe in 
  Flx_bexe.map ~f_bexpr:(reduce_bexpr syms prodmap pa sr) exe

let elim_lrangles syms bsym_table =
  let prodmap = Hashtbl.create 97 in 
  Flx_bsym_table.iter
  (fun id pa sym -> 
     match sym.Flx_bsym.bbdcl with 
     | Flx_bbdcl.BBDCL_fun (prop, bvs, ps, res, effects, exes) ->
       let exes = List.map (reduce_exe syms prodmap pa) exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res,effects, exes) in
       Flx_bsym_table.update_bbdcl bsym_table id bbdcl
     | _ -> () 
  )
  bsym_table
  ;
  Hashtbl.iter (fun i (pa,(sr,e,pt)) ->
(*
    print_endline ("********** Add lrangle  product " ^ string_of_int i);
*)
    let _,e_t = e in
(*
    print_endline ("Child of  " ^ match pa with | Some p -> string_of_int p | None -> "NONE");
*)
    let dt,ct = match pt with | Flx_btype.BTYP_function (dt,ct) -> dt,ct | _ -> assert false in
(*
    print_endline ("Whole lrangle Type: " ^ Flx_print.sbt bsym_table pt);
*)
    (* add function parameter *)
    let pindex = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    let bbdcl = Flx_bbdcl.bbdcl_val ([],dt,`Var) in 
    let bsym = Flx_bsym.create ~sr "_a" bbdcl in
    Flx_bsym_table.add bsym_table pindex (Some i) bsym;
    let param = Flx_bexpr.bexpr_varname dt (pindex,[]) in

    (* calculate domain and codomain component types *)
    let cs = match ct with 
      | Flx_btype.BTYP_tuple cs -> cs 
      | Flx_btype.BTYP_array (t,Flx_btype.BTYP_unitsum n) -> Flx_list.repeat t n 
      | _ -> assert false 
    in
    let n = List.length cs in

    (* generate applications f.i a *)
    let fts = List.map (fun c ->  dt,c) cs in
    let ixs = Flx_list.nlist n in
    let appls = List.map2  (fun i (pd,pc) ->
      let ft = Flx_btype.btyp_function (pd,pc) in
      let fnprj = Flx_bexpr.bexpr_prj i e_t ft in
      let fn = Flx_bexpr.bexpr_apply ft (fnprj, e) in
      Flx_bexpr.bexpr_apply pc (fn,param)
      )
      ixs fts 
    in
(*
print_endline "Calculated lrangle *************";
*)
    let re = Flx_bexpr.bexpr_tuple ct appls in (* product codomain type *)
    let exe = Flx_bexe.bexe_fun_return (sr,re) in 
    let params = Flx_ast.Satom {Flx_bparameter.pid="_a"; pindex=pindex; pkind=`PVar;ptyp=dt},None in
    let props = [`Generated "lrangle"] in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props,[],params, ct,noeffects,[exe]) in 
    let bsym = Flx_bsym.create "lrangle" bbdcl in
    Flx_bsym_table.add bsym_table i pa bsym
  )
  prodmap
  ;
  bsym_table


