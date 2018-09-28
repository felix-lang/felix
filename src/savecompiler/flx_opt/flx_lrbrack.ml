(* calculate [f,g,h] the mediating morphism of the categorical sum of functions
with a common codomain.
*)
let noeffects = Flx_btype.btyp_unit ()

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
     | Flx_bbdcl.BBDCL_fun (prop, bvs, ps, res, effects, exes) ->
       let exes = List.map (reduce_exe syms summap pa) exes in
       let bbdcl = Flx_bbdcl.bbdcl_fun (prop, bvs, ps, res,effects,exes) in
       Flx_bsym_table.update_bbdcl bsym_table id bbdcl
     | _ -> () 
  )
  bsym_table
  ;
  Hashtbl.iter (fun lrbrack_index (pa,(sr,e,pt)) ->
(*
    print_endline ("Add lrbrack " ^ string_of_int i);
    print_endline ("Child of  " ^ match pa with | Some p -> string_of_int p | None -> "NONE");
*)
    let _,e_t = e in
    let dt,ct = match pt with | Flx_btype.BTYP_function (dt,ct) -> dt,ct | _ -> assert false in
(*
    print_endline ("Final lrbrack Type: " ^ Flx_print.sbt bsym_table pt);
*)
    (* add function parameter *)
    let pindex = !(syms.Flx_mtypes2.counter) in 
    incr (syms.Flx_mtypes2.counter);
    let bbdcl = Flx_bbdcl.bbdcl_val ([],dt,`Var) in 
    let bsym = Flx_bsym.create ~sr "_a" bbdcl in
    Flx_bsym_table.add bsym_table pindex (Some lrbrack_index) bsym;
    let param = Flx_bexpr.bexpr_varname dt (pindex,[]) in

    (* calculate domain and codomain component types *)
    let unit_t = Flx_btype.btyp_tuple [] in
    let bool_t = Flx_btype.btyp_unitsum 2 in
    let ds = match dt with 
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
    let fts = List.map (fun d ->  d,ct) ds in
    let ixs = Flx_list.nlist n in

    let exes = ref [] in
    List.iter2  (fun caseno (pd,pc) ->
      (* calculate a label for the next case *)
      let labno = !(syms.Flx_mtypes2.counter) in 
      incr (syms.Flx_mtypes2.counter);
      (* label _funsum_5_9999 used for case 5 *)
      let label = "_lrbrack"^ string_of_int (caseno+1) ^ "_" ^ string_of_int labno in

      (* check the case number .. note, inefficient, extracts case no multiple times
         but this is easier to write than extracting the index and comparing for equality
      *)
      let exe = Flx_bexe.bexe_ifgoto 
        (sr,
          Flx_bexpr.bexpr_not (Flx_bexpr.bexpr_match_case (caseno, param)), 
          labno
        )
      in 
      (* we made a new label so we have to add it to the bsym_table *) 
      let bbdcl = Flx_bbdcl.bbdcl_label label in
      let bsym = {Flx_bsym.id=label; sr=sr; bbdcl=bbdcl} in 
      Flx_bsym_table.add bsym_table labno (Some lrbrack_index) bsym;

      exes := exe :: !exes;
      (* extract parameter argument *)
      let paramarg = Flx_bexpr.bexpr_case_arg pd (caseno,param) in

      (* component function type *)
      let ft = Flx_btype.btyp_function (pd,pc) in

      (* projection to extract the function *)
      let fnprj = Flx_bexpr.bexpr_prj caseno e_t ft in

      (* extract the function *)
      let fn = Flx_bexpr.bexpr_apply ft (fnprj, e) in

      (* apply the function *)
      let outarg = Flx_bexpr.bexpr_apply pc (fn,paramarg) in

      (* return the variant *)
      let exe = Flx_bexe.bexe_fun_return (sr,outarg) in 
      exes := exe :: !exes;

      (* stick a label for skipping the above case *)
      let exe = Flx_bexe.bexe_label (sr,labno) in
      exes := exe :: !exes

      )
      ixs fts 
    ;
(*
print_endline "lrbrack done ****";
*)
    let exes = List.rev (!exes) in
    let params = Flx_ast.Satom {Flx_bparameter.pid="_a"; pindex=pindex; pkind=`PVar;ptyp=dt},None in
    let props = [`Generated "lrbrack"] in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props,[],params, ct,noeffects,exes) in 
    let bsym = Flx_bsym.create "lrbrack" bbdcl in
    Flx_bsym_table.add bsym_table lrbrack_index pa bsym
  )
  summap
  ;
  bsym_table


