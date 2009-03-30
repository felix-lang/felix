type
  ('n,'e) vertex = {
    mutable vertex_label : 'n;
    mutable succ_edges : (('n,'e) edge) list;
    (*mutable pred_edges : (('n,'e) edge) list*)
    mutable det_depth : int; (* deterministic depth, see Elkhound TR sect. 3.1 *)
    mutable ref_count : int;  (* reference count, see Elkhound TR sect. 3.1 *)
  }
  and ('n,'e) edge = {
    mutable edge_label : 'e;
    mutable dest : ('n,'e) vertex;
    (*mutable source : ('n,'e) vertex*)
  }

(* let update_depth topmost f = *)

let update_depth topmost f =
  let rec aux g sn =
    if f sn.vertex_label then match sn.succ_edges with
      | [e] when sn.ref_count=1 ->
          let h x = sn.det_depth <- x+1; g (x+1) in
          aux h e.dest
      | _ -> sn.det_depth <- 0; g 0
  in
  List.iter (aux (fun _ -> ())) topmost

let create_e v1 label v2 f topmost =
  let new_edge = { edge_label = label; (*source = v1;*) dest = v2 } in
  v1.succ_edges <- new_edge::(v1.succ_edges);
  v2.ref_count <- v2.ref_count+1;
  if v2.ref_count > 2 then
    update_depth topmost f;
  (*v2.pred_edges <- new_edge::(v2.pred_edges);*)
  new_edge

(*let remove_edge_e edge =
  let v1 = edge.source in
  let v2 = edge.dest in
  let f e = e!=edge in
  v1.succ_edges <- List.filter f v1.succ_edges;
  v2.pred_edges <- List.filter f v2.pred_edges*)

let create_v label depth =
  { vertex_label = label; succ_edges = []; (*pred_edges = []*)
  det_depth = depth; ref_count = 0; }


