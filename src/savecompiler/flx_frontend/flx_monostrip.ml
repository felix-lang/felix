open Flx_bexe
open Flx_btype
open Flx_bbdcl
open Flx_bexpr

let notunitassign exe = match exe with
  | BEXE_assign (_,_,(_,BTYP_tuple [])) 
  | BEXE_init (_,_, (_,BTYP_tuple []))
    -> false
  | _ -> true

let rec notemptycall (bsym_table:Flx_bsym_table.t) (trail: int list) exe : bool = 
  match exe with
  | BEXE_call (sr,(BEXPR_closure (f,ts),_),(_,BTYP_tuple[])) ->
    if List.mem f trail then false (* INFINITE RECURSION! *)
    else
    begin 
      let bsym = Flx_bsym_table.find bsym_table f in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (_,_,_,_,_,exes)  ->
        begin match exes with
        | [BEXE_proc_return _] -> false
        | ls  ->
          begin try List.iter 
            (fun exe -> 
              if notemptycall bsym_table (f::trail) exe 
              then raise Not_found
              else ()
            )
            ls;
            false
          with Not_found -> true
          end
        end
      | _ -> true
    end 
  | _ -> true

let strip_unit_assigns exes = List.filter notunitassign exes 

(* remove calls to procedures that do nothing. Do NOT remove
the procedures, let the GC do that: they might be passed as arguments
to some HOF
*)


(* THIS ROUTINE DOES LOOKUP!  So it must be run BEFORE monofixup
on the original table.
*)
let strip_empty_calls bsym_table exes = 
  List.filter (notemptycall bsym_table []) exes 


