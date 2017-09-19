open Flx_btype

(* NOTE: this routine doesn't adjust fixpoints! Probably should! *)
let normalise_tuple_cons bsym_table t = 
  let rec nt t = 
    match Flx_btype.map ~f_btype:nt t with
    | BTYP_tuple_cons (t1, BTYP_tuple ls) ->
      let r = btyp_tuple (t1 :: ls) in
      r

    | BTYP_tuple_cons (t1, BTYP_array (t2, BTYP_unitsum n)) when t1 = t2 ->
      let r = btyp_array (t1, btyp_unitsum (n+1)) in
      r

    | BTYP_tuple_cons (t1, BTYP_array (t2, BTYP_unitsum n)) ->
      assert (n < 50);
      let rec arr n ts = match n with 0 -> ts | _ -> arr (n-1) (t2::ts) in
      let ts = arr n [] in
      let r = btyp_tuple (t1 :: ts) in
      r

    | BTYP_tuple_snoc (BTYP_tuple ls,t1) ->
      let r = btyp_tuple (ls@[t1]) in
      r

    | BTYP_tuple_snoc (BTYP_array (t2, BTYP_unitsum n),t1) when t1 = t2 ->
      let r = btyp_array (t1, btyp_unitsum (n+1)) in
      r

    | BTYP_tuple_cons (BTYP_array (t2, BTYP_unitsum n),t1) ->
      assert (n < 50);
      let rec arr n ts = match n with 0 -> ts | _ -> arr (n-1) (t2::ts) in
      let ts = arr n [] in
      let r = btyp_tuple (ts@[t1]) in
      r



(*

    | BTYP_tuple_cons (t1, (BTYP_type_var _ )) as x ->
      x

    | BTYP_tuple_cons (t1, (BTYP_tuple_cons (t2, BTYP_type_var _ ))) as x ->
      x

    | BTYP_tuple_cons (t1,t2) -> btyp_tuple [t1;t2]

    | BTYP_tuple_cons (_,t) -> 
      print_endline ("Error, tuple cons value to non-tuple, type  " ^ sbt bsym_table t); 
      assert false
*)
    | t -> t 
  in 
  let t' = nt t in
(*
  if t' <> t then
    print_endline ("Normalise " ^ sbt bsym_table t ^ " --> " ^ sbt bsym_table t');
*)
  t'

