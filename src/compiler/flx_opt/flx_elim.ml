type elim_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bsym_table: Flx_bsym_table.t;
}


let make_elim_state syms bsym_table =
  {
    syms=syms;
    bsym_table=bsym_table;
  }


let rec lvof x =
  match x with
  | Flx_types.BEXPR_name (i, _), _ -> i
  | Flx_types.BEXPR_get_n (_, e), _ -> lvof e
  | _ -> Flx_types.dummy_bid


let eliminate_init maybe_unused exes =
  List.filter begin function
    | Flx_types.BEXE_init (_, i, _) -> not (Flx_types.BidSet.mem i maybe_unused)
    | Flx_types.BEXE_assign (_, x, _) -> not (Flx_types.BidSet.mem (lvof x) maybe_unused)
    | _ -> true
  end exes


let eliminate_unused_pass state =
  (* Check for unused things .. possible, just a diagnostic for now *)
  let full_use = Flx_use.full_use_closure state.syms state.bsym_table in
  let partial_use = Flx_use.cal_use_closure state.syms state.bsym_table false in
  let maybe_unused = Flx_types.BidSet.diff full_use partial_use in

  (* Iterate over every symbol and check if anyone's referencing it. *)
  Flx_bsym_table.iter begin fun i (id, parent, sr, entry) ->
    match entry with
    | Flx_types.BBDCL_procedure (props, bvs, (ps, tr), exes) ->
      (* Filter out all the unused value creation and assignments in the
       * procedure. *)
      let exes = eliminate_init maybe_unused exes in

      (* Update the procedure with the new exes *)
      let entry = Flx_types.BBDCL_procedure (props,bvs,(ps,tr),exes) in
      Flx_bsym_table.add state.bsym_table i (id, parent, sr, entry)

    | Flx_types.BBDCL_function (props,bvs,(ps,rt),ret,exes) ->
      (* Filter out all the unused value creation and assignments in the
       * function. *)
      let exes = eliminate_init maybe_unused exes in

      (* Update the function with the new exes *)
      let entry = Flx_types.BBDCL_function (props,bvs,(ps,rt),ret,exes) in
      Flx_bsym_table.add state.bsym_table i (id,parent,sr,entry)

    | _ -> ()
  end state.bsym_table;

  (* Delete all the unused symbols *)
  Flx_types.BidSet.iter begin fun i->
    let id,_,_,_ = Flx_bsym_table.find state.bsym_table i in
    (*
    print_endline ("Removing unused " ^ id ^ "<" ^ si i ^ ">");
    *)
    Flx_bsym_table.remove state.bsym_table i
  end maybe_unused;

  Flx_types.BidSet.is_empty maybe_unused

let eliminate_unused state =
  while not (eliminate_unused_pass state) do
    ()
  done
