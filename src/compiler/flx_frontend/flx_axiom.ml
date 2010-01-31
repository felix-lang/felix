open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_typeclass

let string_of_bvs bvs =
  catmap "," (fun (s,i)->s^"<"^si i^">") bvs

let verify syms bsym_table csr e =
  let xx = ref [] in
  iter (fun (id, axsr, parent, axiom_kind, bvs, (bpl,precond), x) ->
    match x with | `BEquation _ -> () | `BPredicate x ->
    (*
    print_endline ("Checking for cases of axiom " ^ id);
    *)
    let param = match bpl with
      | [] -> BEXPR_tuple [], btyp_tuple []
      | [{pindex=i;ptyp=t}] -> BEXPR_name (i,[]), t
      | ls ->
        let xs = map (fun { pindex=i; ptyp=t } -> BEXPR_name (i,[]), t) ls in
        let ts = map snd xs in
        BEXPR_tuple xs, btyp_tuple ts
    in
    let tvars = map (fun (_,i) -> i) bvs in
    let evars = Flx_bparameter.get_bids bpl in
    let result = expr_maybe_matches syms.counter tvars evars param e in
    match result with
    | None -> ()
    | Some (tmgu, emgu) ->
      (*
      print_endline (sbe sym_table e ^  " MATCHES AXIOM " ^ id);
      print_endline ("Axiom vs =" ^ string_of_bvs bvs);
      print_endline ("TMgu=" ^ string_of_varlist sym_table tmgu);
      *)
      let ok = match parent with
      | None -> true
      | Some i ->
        try
          let tc_bsym = Flx_bsym_table.find bsym_table i in
          match tc_bsym.Flx_bsym.bbdcl with
          | BBDCL_typeclass (_,tcbvs) ->
            begin
              (*
              print_endline ("Axiom "^id^" is owned by typeclass " ^ tcid);
              print_endline ("Typeclass bvs=" ^ string_of_bvs tcbvs);
              *)
              let ts =
                try
                  Some (map (fun (s,i) -> assoc i tmgu) tcbvs)
                with Not_found ->
                  (*
                  print_endline "Can't instantiate typeclass vars- FAIL";
                  *)
                  None
              in
              match ts with None -> false | Some ts ->
              let insts =
                try
                  Some (Hashtbl.find syms.instances_of_typeclass i)
                with Not_found ->
                  (*
                  print_endline "Typeclass has no instances";
                  *)
                  None
              in
              match insts with | None -> false | Some insts ->
              try
                iter begin fun (instidx,(inst_bvs, inst_traint, inst_ts)) ->
                  match
                    tcinst_chk
                      syms
                      bsym_table
                      true
                      i
                      ts
                      tc_bsym.Flx_bsym.sr
                      (inst_bvs, inst_traint, inst_ts, instidx)
                  with
                  | None -> ()
                  | Some _ -> raise Not_found
                end insts;
                (*
                print_endline "Couldn't find instance";
                *)
                false
              with Not_found ->
                (*
                print_endline "FOUND INSTANCE";
                *)
                true
            end
          | _ -> true
        with
          Not_found ->
          (*
          print_endline "Wha .. can't find axiom's parent";
          *)
          true
      in
      if not ok then () else
      let xsub x = fold_left (fun x (i,e) -> expr_term_subst x i e) x emgu in
      let tsub t = list_subst syms.counter tmgu t in
      (*
      print_endline ("tmgu= " ^ catmap ", " (fun (i,t) -> si i ^ "->" ^ sbt sym_table t) tmgu);
      *)
      let ident x = x in
      let rec aux x = Flx_bexpr.map ~ft:tsub ~fe:aux x in
      let cond = aux (xsub x) in
      let precond = match precond with
      | Some x -> Some (aux (xsub x))
      | None -> None
      in
      let comment = BEXE_comment (csr,"Check " ^ id) in
      let ax = BEXE_assert2 (csr,axsr,precond,cond) in
      (*
      print_endline ("Assertion: " ^ tsbe sym_table cond);
      *)
      xx := ax :: comment :: !xx
  )
  syms.axioms
  ;
  !xx

let fixup_exes syms bsym_table bexes =
  let rec aux inx outx = match inx with
  | [] -> rev outx
  | BEXE_axiom_check (sr,e) :: t ->
    (*
    print_endline ("Axiom check case "  ^ sbe sym_table e);
    *)
    aux t ((verify syms bsym_table sr e) @ outx)

  | h :: t -> aux t (h::outx)
  in
  aux bexes []

let axiom_check syms bsym_table =
  Flx_bsym_table.update_bexes (fixup_exes syms bsym_table) bsym_table
