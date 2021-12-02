(* Eliminate unused non-parameter variables
*)

open Flx_bid

type elim_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bsym_table: Flx_bsym_table.t;
}


let make_elim_state syms bsym_table =
  {
    syms=syms;
    bsym_table=bsym_table;
  }


let eliminate_init maybe_unused exes =
  List.filter begin function
    | Flx_bexe.BEXE_assign (_, i, _) 
    | Flx_bexe.BEXE_init (_, i, _) -> not (BidSet.mem i maybe_unused)
    | _ -> true
  end exes


let eliminate_unused_pass state =
  (* Check for unused things .. possible, just a diagnostic for now *)
  let full_use = Flx_use.full_use_closure state.syms state.bsym_table in
  let partial_use = Flx_use.cal_use_closure state.syms state.bsym_table false in
  let maybe_unused = BidSet.diff full_use partial_use in

  (* Iterate over every symbol and check if anyone's referencing it. *)
  Flx_bsym_table.update_bexes (eliminate_init maybe_unused) state.bsym_table;

  (* Delete all the unused symbols *)
  BidSet.iter begin fun i->
    (*
    let id,_,_,_ = Flx_bsym_table.find state.bsym_table i in
    print_endline ("Removing unused " ^ id ^ "<" ^ si i ^ ">");
    *)
    Flx_bsym_table.remove state.bsym_table i
  end maybe_unused;

  BidSet.is_empty maybe_unused

let eliminate_unused state =
  while not (eliminate_unused_pass state) do
    ()
  done

