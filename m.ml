type btyp =
 | BTYP_fix of int
 | BTYP_function of btyp * btyp
 | BTYP_tuple of btyp list

let rec string_of_btyp t =
  match t with
  | BTYP_fix i -> "fix"^string_of_int i
  | BTYP_function (a,b) -> "(" ^ string_of_btyp a ^ " -> " ^string_of_btyp b ^ ")"
  | BTYP_tuple ls -> "(" ^ String.concat ", " (List.map string_of_btyp ls) ^ ")"

exception Found of btyp

let rec memq trail (a,b) = match trail with
  | [] -> false
  | (i,j)::t -> i == a && j == b || memq t (a,b)


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

  | _ -> false

let type_eq t1 t2 =
  type_eq' [] 0 [] 0 [] t1 t2

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
  in aux 0 t

let map f t = 
  match t with
  | BTYP_tuple ts -> BTYP_tuple (List.map f ts)
  | BTYP_function (a,b) -> BTYP_function (f a, f b)
  | BTYP_fix _ as x -> x

let fold bsym_table counter t =
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

  in
    try aux [] 0 t; t
    with Found t -> t


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

  in
    try aux [] 0 t; t
    with Found t -> t

let minimise t =
  fold (map fold t)

let p t = print_endline (string_of_btyp t)

let u = BTYP_tuple [] ;;
let f1 = BTYP_function (u,u);;
let f2 = BTYP_tuple [u;BTYP_function (u,BTYP_fix (-2))];;

p f1;;
p f2;;
p (unfold f2);;
p (fold (unfold f2));;

