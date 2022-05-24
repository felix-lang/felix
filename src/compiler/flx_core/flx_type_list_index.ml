let type_list_index counter bsym_table (ls: (Flx_btype.t * int) list) t =
  (*
  print_endline ("Comparing : " ^ sbt bsym_table t ^ " with ..");
  *)
  let rec aux ls = match ls with
  | [] -> None
  | (hd, depth) :: tl ->
    (*
    print_endline ("Candidate : " ^ sbt bsym_table hd);
    *)
    if
      begin try Flx_unify.type_eq bsym_table counter hd t
      with x ->
        print_endline ("Exception: " ^ Printexc.to_string x);
        false
      end
    then begin 
(*
      print_endline ("Type list index found term " ^ Flx_btype.st hd ^ " in trail depth " ^ string_of_int depth);
*)
      Some depth
    end
    else aux tl 
  in aux ls 


