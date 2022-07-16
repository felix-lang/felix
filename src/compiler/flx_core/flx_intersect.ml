open Flx_mtypes2
open Flx_bid
open Flx_print
open Flx_kind
open Flx_btype



let record_pair_intersect counter br ls rs =
  let rec aux ls rs out =
  match ls, rs with
  | ls, [] -> List.rev out @ ls
  | [] , rs -> List.rev out @ rs
  | (s1,t1) :: tl1, (s2,t2) :: tl2 ->
    if s1 = s2 then 
      let t = br (btyp_intersect [t1;t2]) in
      aux tl1 tl2 ((s1, t)::out)
    else if s1 < s2 then aux tl1 rs ((s1,t1)::out)
    else aux ls tl2 ((s2,t2)::out)
  in aux ls rs []
 
let record_intersect bsym_table counter br ls =
  let flds = List.fold_left (fun acc r -> 
    match r with 
    | BTYP_record rs -> record_pair_intersect counter br acc rs
    | t -> 
      print_endline ("Flx_intersect: Record intersection requires records, got " ^ Flx_btype.st t);
      failwith ("Flx_intersect: Record intersection requires records, got " ^ Flx_btype.st t)
    ) [] ls 
  in
  btyp_record flds


let nominal_intersect bsym_table counter br ls =
  let js = List.fold_left (fun acc elt ->
    match elt with
    | BTYP_inst (`Nominal, j, [], KIND_type) -> j :: acc
    | t -> 
      print_endline ("Flx_intersect: Nominal type intersect requires monomorphic nominal type, got " ^ Flx_btype.st t);
      failwith ("Flx_intersect: Nominal type intersect requires monomorphic nominal type, got " ^ Flx_btype.st t)
    ) [] ls
  in
  let maybe_meet = Flx_bsym_table.greatest_subtype bsym_table js in
  match maybe_meet with
  | None -> 
    print_endline ("Flx_intersect: No unique greatest subtype found: " ^ Flx_print.sbt bsym_table (btyp_intersect ls) );
    failwith ("Flx_intersect: No unique greatest subtype found")
  | Some i -> btyp_inst (`Nominal, i, [], KIND_type)


let intersect bsym_table counter br ls =
  let ls = List.map br ls in
  match ls with
  | [] -> btyp_any ()
  | BTYP_record _ :: _ -> record_intersect bsym_table counter br ls 
  | BTYP_inst (`Nominal, _, [], KIND_type) :: _ -> nominal_intersect bsym_table counter br ls
  | t :: tail  -> 
    print_endline ("Flx_intersect: Intersection only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t);
    failwith ("Flx_intersect: Intersection only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t);

