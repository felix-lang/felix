type btyp =
 | BTYP_fix of int
 | BTYP_function of btyp * btyp
 | BTYP_tuple of btyp list
 | BTYP_hole

let rec string_of_btyp t =
  match t with
  | BTYP_fix i -> "fix"^string_of_int i
  | BTYP_function (a,b) -> "(" ^ string_of_btyp a ^ " -> " ^string_of_btyp b ^ ")"
  | BTYP_tuple ls -> "[" ^ String.concat ", " (List.map string_of_btyp ls) ^ "]"
  | BTYP_hole -> "@"

exception Found of btyp

(* checks if a pair of terms is identically on the trail,
  that is, it checks addresses: for example two equal valued
  but distinct pairs of leaves don't match
*)
let rec memq trail (a,b) = match trail with
  | [] -> false
  | (i,j)::t -> i == a && j == b || memq t (a,b)


(* This routine compares two types for equality, with 
 a special modification: a hole matches any term
*)

let rec type_eq' ltrail ldepth rtrail rdepth trail t1 t2 =
  if memq trail (t1,t2) then true
  else let te a b = type_eq'
    ((ldepth,t1)::ltrail) (ldepth+1)
    ((rdepth,t2)::rtrail) (rdepth+1)
    ((t1,t2)::trail)
    a b
  in
  match t1,t2 with
  | BTYP_tuple ts1,BTYP_tuple ts2 ->
    let result =
    if List.length ts1 = List.length ts2
    then
      List.fold_left2
      (fun tr a b -> tr && te a b)
      true ts1 ts2
    else false
    in
    result

  | BTYP_function (s1,d1),BTYP_function (s2,d2)
    -> te s1 s2 && te d1 d2

  | BTYP_fix (i),BTYP_fix (j) ->
    if i = j then true
    else 
    begin try
      let a = List.assoc (ldepth+i) ltrail in
      let b = List.assoc (rdepth+j) rtrail in
      type_eq' ltrail ldepth rtrail rdepth trail a b
      with Not_found -> false
    end

  | BTYP_fix (i),t ->
    begin try
      let a = List.assoc (ldepth+i) ltrail in
      type_eq' ltrail ldepth rtrail rdepth trail a t
      with Not_found -> false
    end

  | t,BTYP_fix (j) ->
    begin try
    let b = List.assoc (rdepth+j) rtrail in
    type_eq' ltrail ldepth rtrail rdepth trail t b
    with Not_found -> false
    end

  | BTYP_hole, _
  | _,BTYP_hole -> true 
  
  | _ -> false

let type_eq t1 t2 =
  type_eq' [] 0 [] 0 [] t1 t2

(* won't work with types with holes *)
let unfold t =
  let rec aux depth t' =
    let uf t = aux (depth + 1) t in
    match t' with
    | BTYP_tuple ls -> BTYP_tuple (List.map uf ls)
    | BTYP_function (a,b) -> BTYP_function (uf a,uf b)
    | BTYP_fix (i) when (-i) = depth -> t
    | BTYP_fix (i) when (-i) > depth -> 
      failwith "Free fixpoint"
    | BTYP_fix _ -> t'
    | BTYP_hole -> assert false
  in aux 0 t

(* works with holes *)
let map f t = 
  match t with
  | BTYP_tuple ts -> BTYP_tuple (List.map f ts)
  | BTYP_function (a,b) -> BTYP_function (f a, f b)
  | BTYP_fix _ as x -> x
  | BTYP_hole as x -> x

(* won't work with types with holes *)
let fold t =
  let rec aux trail depth t' =
    let ax t = aux ((depth,t')::trail) (depth+1) t in
    match t' with
    | BTYP_tuple ls -> List.iter ax ls
    | BTYP_function (a,b) -> ax a; ax b
    | BTYP_fix (0) -> ()

    | BTYP_fix (i) ->
      let k = depth + i in
      begin try
        let t'' = List.assoc k trail in
        if type_eq t'' t then raise (Found t'')
      with Not_found -> ()
      end
    | BTYP_hole -> assert false
  in
    try aux [] 0 t; t
    with Found t -> t

exception Discard of int * int 

let plug zipper term = match zipper with
  | BTYP_tuple ls -> BTYP_tuple (List.map 
    (fun t -> match t with | BTYP_hole -> term | _ -> t) ls)

  | BTYP_function (a,b) -> 
    BTYP_function (
      (match a with | BTYP_hole -> term | _ -> a),
      (match b with | BTYP_hole -> term | _ -> b))
  | _ -> assert false

let wrap t = 
print_endline ("Trying to wrap " ^ string_of_btyp t);
  let rec aux (trail: (int * btyp) list) depth t' =
(*
print_endline ("Depth "^string_of_int depth ^ " Analysing subterm " ^ string_of_btyp t');
*)
  try
    match t' with
    | BTYP_hole -> assert false
    | BTYP_tuple ls ->
      let rec scan left right =
        match right with
        | pivot::tail ->
          let trail = (depth,BTYP_tuple (left @ [BTYP_hole] @ tail))::trail in
          let r = aux trail (depth+1) pivot in
          scan (left @ [r]) tail
        | _ -> BTYP_tuple left
      in scan [] ls

    | BTYP_function (a,b) ->
      let trail = (depth, (BTYP_function (BTYP_hole,b))):: trail in
      let a' = aux trail (depth+1) a in
      let trail = (depth, (BTYP_function (a,BTYP_hole))):: trail in
      let b' = aux trail (depth+1) b in
      BTYP_function (a',b')

    | BTYP_fix (0) as x -> x
    | BTYP_fix i when depth + i = 0 -> BTYP_fix i (* original term is recursive *)
    | BTYP_fix (i) ->
      let rec slide j = 
        (* off the top! *)
        if depth - j + i < 0 then raise (Discard (j-1,i)) else

        (* grab the unzipped term above the binderl *)
        let binder_zipper_m1 = List.assoc (depth - j + i) trail in 

        (* grab the unzipped term above fixpoint *)
        let fix_zipper_m1 = List.assoc (depth - j) trail in

        (* check if the two terms are equal with holes *)
        if type_eq binder_zipper_m1 fix_zipper_m1 
        then begin
          slide (j+1) (* if equal, slide up again *)
        end else begin
          raise (Discard (j-1,i))
        end
      in 
      slide 1

    with Discard (n,i) -> if n=0 then BTYP_fix i else raise (Discard (n-1,i))
  in
  aux [] 0 t

let minimise t = fold (map fold t)
;;
begin
  let pt t = print_endline (string_of_btyp t) in
  let pst s t = print_endline (s ^ " " ^string_of_btyp t) in
  let sb b = string_of_bool b in
  let pb b = print_endline (sb b) in
  let peq t1 t2 = pb (type_eq t1 t2) in

  let u = BTYP_tuple [] in
  let d1 = BTYP_function (u,u) in
  let f2 = BTYP_tuple [u;BTYP_function (u,BTYP_fix (-2))] in

  pt d1;
  print_string "f2="; pt f2;
  print_string "unfold f2="; pt (unfold f2);
  pt (fold (unfold f2));
  peq f2 (unfold f2);
  let d3 = BTYP_tuple [u;unfold f2] in
  pt d3;
  pt (minimise d3);

  let t1 = BTYP_function (u,BTYP_tuple [BTYP_function (u,BTYP_fix (-2))]) in
  let t2 = BTYP_function (u,BTYP_tuple [BTYP_fix (-2)]) in
  let u1 = wrap t1 in
  let u2 = wrap t2 in

  print_endline ("wrap " ^ string_of_btyp t1 ^ " ==> " ^ string_of_btyp u1);
  print_endline ("wrap " ^ string_of_btyp t2 ^ " ==> " ^ string_of_btyp u2);

  assert (type_eq t1 t2);
  assert (type_eq t1 u1);
  assert (type_eq t1 u2);
  assert (type_eq t2 t1);
  assert (type_eq t1 u2);
  assert (u1 = u2);

  print_endline (
    "f2="^string_of_btyp f2 ^ 
    "\nUnfold f2="^string_of_btyp (unfold f2) ^ 
    "\nwrapped=" ^ string_of_btyp (wrap (unfold f2)) ^
    "\nwrapped twice=" ^ string_of_btyp (wrap (wrap (unfold f2)))
  );
end
;;
