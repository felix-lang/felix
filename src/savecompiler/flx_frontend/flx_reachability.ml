open Flx_label
open Flx_bexe

exception Found_label of int

type rrec = { index:int; mutable reachable:bool; instr: Flx_bexe.t; } 

let next_reachable exe = match exe with
  | BEXE_proc_return  _
  | BEXE_fun_return  _
  | BEXE_jump _
  | BEXE_jump_direct _
  | BEXE_halt _
  | BEXE_nonreturn_code _
  | BEXE_goto _
    -> false
  | _ -> true

let check_reachability_exes bsym_table label_info idx sr name rt exes =
  (* remove unused labels to avoid confusing algorithm *)
  let exes = List.filter 
    (fun exe -> match exe with 
      | BEXE_label (_,idx) -> 
        (match get_label_kind_from_index label_info.label_usage idx with 
        | `Far | `Near -> true 
        | `Unused -> (* print_endline ("Removing unused label " ^ s); *) false 
        )
      | _ -> true
    )
    exes
  in

  let n = List.length exes in
  let unprocessed_targets = ref (Flx_set.IntSet.singleton 0) in
  let a = 
    let tmp_exes = ref exes in
    Array.init n (fun i -> 
     let exe = List.hd (!tmp_exes) in tmp_exes := List.tl (!tmp_exes);
     begin match exe with
     | BEXE_label (_,idx) ->
       (match get_label_kind_from_index label_info.label_usage idx with 
       | `Far -> unprocessed_targets := Flx_set.IntSet.add i (!unprocessed_targets)
       | _ -> ()
       )
     | _ -> ()
     end
     ;
     {index=i; reachable=false; instr=exe }
    )
  in
  let find_label s = 
    try Array.iter 
      (fun {index=i; instr=instr;} -> 
        match instr with BEXE_label (_,s') when  s' = s -> 
        raise (Found_label i)
        | _ -> ()
      ) 
      a
      ; 
      raise Not_found 
    with 
    | Found_label i -> Some i 
    | Not_found -> None
  in
  let drops_thru_end = ref false in
  let processed_targets = ref Flx_set.IntSet.empty in
  while not (Flx_set.IntSet.is_empty (!unprocessed_targets)) do
    let target = Flx_set.IntSet.choose (!unprocessed_targets) in
    unprocessed_targets := Flx_set.IntSet.remove target (!unprocessed_targets);
    processed_targets := Flx_set.IntSet.add target (!processed_targets);

    (* When a try is reachable, mark corresponding catch and entry as
       reachable targets
    *)
    let rec scan_trycatch level index =
      match a.(index).instr with
      | BEXE_try _ ->  (* nested, maybe add later*) scan_trycatch (level + 1) (index + 1) 
      | BEXE_catch _ -> 
        if level = 0 && not (Flx_set.IntSet.mem index (!processed_targets)) then 
          unprocessed_targets := Flx_set.IntSet.add index (!unprocessed_targets)
        ;
        scan_trycatch level (index + 1)
      | BEXE_endtry _ ->
        if level = 0 then
          if not (Flx_set.IntSet.mem index (!processed_targets)) then 
            unprocessed_targets := Flx_set.IntSet.add index (!unprocessed_targets)
          else scan_trycatch (level - 1) (index + 1)
      | _ ->
        scan_trycatch level (index + 1)
    in

    try 
      for i = target to n - 1 do
        (* mark this instruction reachable *)
        a.(i).reachable <- true;
        (* if this instruction is a goto, 
         * going to a label we haven't processed, 
         * then add it to the set of unprocessed labels
         *)
        begin match a.(i).instr with
        | BEXE_goto (_,idx) 
        | BEXE_ifgoto (_,_,idx) -> 
          if not (Flx_set.IntSet.mem idx (!processed_targets)) then 
            unprocessed_targets := Flx_set.IntSet.add idx (!unprocessed_targets) 
        | BEXE_try _ ->
          scan_trycatch 0 (i+1)

        | _ -> ()
        end
        ;
        (* if the next instruction isn't reachable, escape loop *)
        if not (next_reachable a.(i).instr) then raise Not_found;
      done
      ;
      (* if we dropped through the loop,
       * then flag that the function drops off its end 
       *)
      drops_thru_end := true
    with Not_found -> ()
  done
  ;
  let is_proc rt = match rt with | Flx_btype.BTYP_void |Flx_btype.BTYP_fix (0,_) -> true | _ -> false in
  if !drops_thru_end && not (is_proc rt) then begin
(*
    print_endline ("WARNING: [check_reachability] Function " ^ name ^ " drops thru end, code:");
*)
(*
    List.iter (fun exe -> print_endline (Flx_print.string_of_bexe bsym_table 2 exe)) exes;
    print_endline ("Source:");
    print_endline (long_string_of_src sr);
*)
(*
    Flx_exceptions.clierrx "[flx_frontend/flx_reachability.ml:133: E355] " sr ("[check_reachability] Function " ^ name ^ " drops thru end")
*)
  end
  ;
  let new_exes = ref [] in
  Array.iter (fun {reachable=reachable; instr=instr;} -> match instr with
  | BEXE_comment _  -> new_exes := instr :: (!new_exes)
  | _ when reachable -> new_exes := instr :: (!new_exes)
  | _ -> ()
  )
  a
  ;
  List.rev (!new_exes)
 
(* `Tag "reachability-done" to avoid reprocessing library functions
 * every compile.
 *)
let tagged s ps =
  let rec aux ps = match ps with
  | `Tag s':: _ when s = s' -> true
  | h::t -> aux t
  | [] -> false
  in aux ps

let check_reachability bsym_table = () 
(* 
  let counter = ref 0 in
  let label_info = Flx_label.create_label_info bsym_table in 
  Flx_bsym_table.iter
  (fun idx parent bsym -> match Flx_bsym.bbdcl bsym with
  | Flx_bbdcl.BBDCL_fun (ps,bvs,bpar,rt,exes) when not (tagged "reachability-done" ps) ->
    let name = Flx_bsym.id bsym in
    let sr = Flx_bsym.sr bsym in 
    let newexes = check_reachability_exes bsym_table label_info idx sr name rt exes in
    let newbbdcl = Flx_bbdcl.bbdcl_fun (`Tag "reachability-done"::ps, bvs, bpar, rt, newexes) in
    Flx_bsym_table.update_bbdcl bsym_table idx newbbdcl
  | _ -> ()
  )
  bsym_table
*)

