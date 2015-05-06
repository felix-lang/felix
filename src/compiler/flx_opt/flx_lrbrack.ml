(* calculate [f,g,h] the mediating morphism of the categorical sum of functions
with a common codomain.
*)
let rec reduce_bexpr syms summap pa sr e =
  let f_bexpr e = reduce_bexpr syms summap pa sr e in
  match Flx_bexpr.map ~f_bexpr e with
  | Flx_bexpr.BEXPR_lrbrack e,t ->
    let i = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    Hashtbl.add summap i (pa,(sr,e,t));
    Flx_bexpr.bexpr_closure t (i,[])
 
  | e -> e

let reduce_exe syms summap pa exe =
  let sr = Flx_bexe.get_srcref exe in 
  Flx_bexe.map ~f_bexpr:(reduce_bexpr syms summap pa sr) exe

let elim_lrbracks syms bsym_table =
  let summap = Hashtbl.create 97 in 
  Flx_bsym_table.iter
  (fun id pa sym -> 
     match sym.Flx_bsym.bbdcl with 
     | Flx_bbdcl.BBDCL_fun (prop, bvs, ps, res, exes) ->
       let exes = List.map (reduce_exe syms summap pa) exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res,exes) in
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
    let dt,ct = match pt with | Flx_btype.BTYP_function (dt,ct) -> dt,ct | _ -> assert false in
(*
    print_endline ("Type: " ^ Flx_print.sbt bsym_table pt);
*)
    (* add function parameter *)
    let pindex = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    let bbdcl = Flx_bbdcl.bbdcl_val ([],dt,`Var) in 
    let bsym = Flx_bsym.create ~sr "_a" bbdcl in
    Flx_bsym_table.add bsym_table pindex (Some i) bsym;
    let param = Flx_bexpr.bexpr_varname dt (pindex,[]) in

    (* calculate domain and codomain component types *)
    let d,c = match pt with | Flx_btype.BTYP_function (d,c) -> d,c | _ -> assert false in
    assert (Flx_unify.type_eq bsym_table syms.Flx_mtypes2.counter c ct);
    let unit_t = Flx_btype.btyp_tuple [] in
    let bool_t = Flx_btype.btyp_unitsum 2 in
    let ds = match d with 
      | Flx_btype.BTYP_sum ds -> ds 
      | Flx_btype.BTYP_unitsum n -> Flx_list.repeat unit_t n 
      | _ -> assert false 
    in
    let n = List.length ds in

    (* the summed function works by examining its single
       variant input, and returning a single variant
       with the same case index i, but with argument
       f_i a, where a is the argument of the input variant,
       therefore, it is a switch..
   *)
    let fts = List.map (fun d ->  d,c) ds in
    let ixs = Flx_list.nlist n in

    let exes = ref [] in
    List.iter2  (fun i (pd,pc) ->
      (* calculate a label for the next case *)
      let labno = !(syms.Flx_mtypes2.counter) in 
      incr (syms.Flx_mtypes2.counter);
      (* label _funsum_5_9999 used for case 5 *)
      let label = "_lrbrack"^ string_of_int (i+1) ^ "_" ^ string_of_int labno in

      (* check the case number .. note, inefficient, extracts case no multiple times
         but this is easier to write than extracting the index and comparing for equality
      *)
      let exe = Flx_bexe.bexe_ifgoto 
        (sr,
          Flx_bexpr.bexpr_not (Flx_bexpr.bexpr_match_case (i, param)), 
          label
        )
      in 
      exes := exe :: !exes;
      (* extract parameter argument *)
      let paramarg = Flx_bexpr.bexpr_case_arg pd (i,param) in

      (* component function type *)
      let ft = Flx_btype.btyp_function (pd,pc) in

      (* projection to extract the function *)
      let fnprj = Flx_bexpr.bexpr_prj i pt ft in

      (* extract the function *)
      let fn = Flx_bexpr.bexpr_apply pt (fnprj, e) in

      (* apply the function *)
      let outarg = Flx_bexpr.bexpr_apply pc (fn,paramarg) in

      (* return the variant *)
      let exe = Flx_bexe.bexe_fun_return (sr,outarg) in 
      exes := exe :: !exes;

      (* stick a label for skipping the above case *)
      let exe = Flx_bexe.bexe_label (sr,label) in
      exes := exe :: !exes

      )
      ixs fts 
    ;
    let exes = List.rev (!exes) in
    let params = [{Flx_bparameter.pid="_a"; pindex=pindex; pkind=`PVar;ptyp=dt}] in
    let params = params,None in 
    let props = [`Generated "lrbrack"] in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props,[],params, ct,exes) in 
    let bsym = Flx_bsym.create "lrbrack" bbdcl in
    Flx_bsym_table.add bsym_table i pa bsym
  )
  summap
  ;
  bsym_table

