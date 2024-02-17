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

(* only matching fields are output, and the type is
  the union of both types
*)
let record_pair_union counter br ls rs =
  let rec aux ls rs out =
  match ls, rs with
  | ls, [] -> List.rev out 
  | [] , rs -> List.rev out 
  | (s1,t1) :: tl1, (s2,t2) :: tl2 ->
    if s1 = s2 then begin 
      let t = br (btyp_union [t1;t2]) in
      aux tl1 tl2 ((s1, t)::out)
    end
    else if s1 < s2 then aux tl1 rs out
    else aux ls tl2 out
  in 
  let result = aux ls rs [] in
  result

(* Note: void is the unit of a union, that is 0 \/ t = t
However an "empty" record has type unit. So if we find an
empty set of fields in a union, it's void and we need a special case
*) 
let record_union bsym_table counter br ls =
  let flds = List.fold_left (fun acc r -> 
    match r with 
    | BTYP_record rs -> 
      begin match acc with 
      | [] -> rs 
      | _ -> record_pair_union counter br acc rs
      end
    | t -> 
      print_endline ("Flx_intersect: Record union requires records, got " ^ Flx_btype.st t);
      failwith ("Flx_intersect: Record iunion requires records, got " ^ Flx_btype.st t)
    ) [] ls 
  in
  match flds with
  | [] -> btyp_void ()
  | _ -> btyp_record flds


let nominal_intersect bsym_table counter br ls =
  let js = List.fold_left (fun acc elt ->
    match elt with
    | BTYP_inst (`Nominal _, _, j, [], KIND_type) -> j :: acc
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
  | Some i -> btyp_inst (`Nominal [], i, [], KIND_type)

let nominal_union bsym_table counter br ls =
  let js = List.fold_left (fun acc elt ->
    match elt with
    | BTYP_inst (`Nominal [], _, j, [], KIND_type) -> j :: acc
    | t -> 
      print_endline ("Flx_intersect: Nominal type union requires monomorphic nominal type, got " ^ Flx_btype.st t);
      failwith ("Flx_intersect: Nominal type union requires monomorphic nominal type, got " ^ Flx_btype.st t)
    ) [] ls
  in
  let maybe_join = Flx_bsym_table.least_supertype bsym_table js in
  match maybe_join with
  | None -> 
    print_endline ("Flx_intersect: No unique least supertype found: " ^ Flx_print.sbt bsym_table (btyp_intersect ls) );
    failwith ("Flx_intersect: No unique least supertype found")
  | Some i -> btyp_inst (`Nominal [], i, [], KIND_type)



let intersect bsym_table counter br ls =
  let ls = List.map br ls in
  match ls with
  | [] -> btyp_any ()
  | BTYP_record _ :: _ -> record_intersect bsym_table counter br ls 
  | BTYP_inst (`Nominal _, _, _, [], KIND_type) :: _ -> nominal_intersect bsym_table counter br ls
  | t :: tail  -> 
    print_endline ("Flx_intersect: Intersection only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t);
    failwith ("Flx_intersect: Intersection only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t)

let union bsym_table counter br ls =
  let ls = List.map br ls in
  match ls with
  | [] -> btyp_any ()
  | BTYP_record _ :: _ -> record_union bsym_table counter br ls 
  | BTYP_inst (`Nominal _, _, _, [], KIND_type) :: _ -> nominal_union bsym_table counter br ls
  | t :: tail  -> 
    print_endline ("Flx_intersect: Union only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t);
    failwith ("Flx_intersect: Union only defined for records and monomorphic nominal types, got " ^ Flx_btype.st t)

