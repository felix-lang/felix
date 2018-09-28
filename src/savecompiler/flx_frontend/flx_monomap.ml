open Flx_bid

module MonoMap = 
  struct 
    type key = int * Flx_btype.t list 
    type target = int
    type kv = key * target
    type data = kv list

    let counter = ref 1 (* HACK, should be private *)
    let dummy = Flx_bsym_table.create_fresh ()   (* should be private .. *)
    let cmp (i,ts) (i',ts') =
      i = i' && List.length ts = List.length ts' &&
      List.fold_left2 (fun r t t' -> r && Flx_unify.type_eq dummy counter t t') true ts ts'

    let ecmp k (k',_) = cmp k k'

    let mem k d = List.exists (ecmp k) d
    let find (k:key) (d:data):target = snd (List.find (ecmp k) d)
    let choose d = List.hd d
    let remove k d =
      List.filter (fun (k',_) -> not (cmp k k')) d
    let empty = []
    let is_empty d = d = []
    let add k v d = (k,v) :: d (* unchecked! *)
    let iter f d = List.iter f d
  end

let find_felix_inst syms bsym_table processed to_process nubids i ts : int =
  (* is this right? Is the result always monomorphic? *)
  let ts = List.map (Flx_remap_vtypes.remap_virtual_types syms bsym_table) ts  in
  let find_inst syms processed to_process i ts =
    try 
      Some (MonoMap.find (i,ts) !processed)
    with Not_found ->
    try
      Some (MonoMap.find (i,ts) !to_process)
    with Not_found -> None
  in
  match find_inst syms processed to_process i ts with
  | None ->
    let k = 
      if List.length ts = 0 then i else  
       let nubid = fresh_bid syms.Flx_mtypes2.counter  in
       nubids := BidSet.add nubid (!nubids);
       nubid
    in
    let target = k in
    to_process := MonoMap.add (i,ts) target !to_process;
    (*
    if i <> k then
      print_endline ("Add inst to process: " ^ showts bsym_table i ts ^ " --> "^si k);
    *)
    k
  | Some (k) -> k


