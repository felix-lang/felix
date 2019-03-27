open Flx_btype
open Flx_exceptions
open Flx_bid

let si x = string_of_int x

let var_subst t (i, j) =
  let rec f_btype t =
    match t with
    | BTYP_type_var (k,t) when i = k -> btyp_type_var (j,t)
    | t -> Flx_btype.map ~f_btype t
  in
  f_btype t

let vars_subst ls t = List.fold_left var_subst t ls

(* NOTE: BUG perhaps .. this ONLY converts function parameters! *)
let rec alpha counter t =
  match t with
  | BTYP_type_function (ps,r,b) ->
      let remap_list = List.map (fun (i,_) -> i, fresh_bid counter) ps in
      let remap i = List.assoc i remap_list in
      let cvt t = alpha counter (vars_subst remap_list t) in
      let ps = List.map (fun (i,t) -> remap i,t) ps in
      btyp_type_function (ps, r, cvt b)
  | t -> Flx_btype.map ~f_btype:(alpha counter) t

let alpha_convert counter t =
  let t = alpha counter t in (* convert function parameters first *)
  let remap_list = ref [] in
  let remap i = 
    try List.assoc i !remap_list
    with Not_found ->
      let j = fresh_bid counter in
      remap_list := (i,j) :: !remap_list;
      j
  in
  let rec aux t = match t with
  | BTYP_type_function (ps,r,b) ->
    (* now leave function parameters alone, they're bound and already converted *)
    List.iter (fun (i,_) -> remap_list := (i,i) :: !remap_list) ps; 
    btyp_type_function (ps, r, aux b)

  | BTYP_type_var (i,mt) -> btyp_type_var (remap i, mt)

  | t -> Flx_btype.map ~f_btype:aux t
  in aux t


let term_subst counter src i arg =
  let rec aux level t =
    match t with
    | BTYP_type_var (k,_) when k = i -> widen_fixgap level arg 

    | BTYP_type_match (tt, pts) ->
        let tt =  aux level tt in
        let pts =
          List.map begin fun ((bpat, x) as case) ->
            if BidSet.mem i bpat.pattern_vars then case else
            let asgs = (* not sure about level adjust here .. *)
            List.map (fun (i,t) -> i,aux (level+1) t) bpat.assignments in
            { bpat with
              pattern=aux level bpat.pattern;
              assignments=asgs }, aux (level+1) x (* not sure about level adjust here either .. *)
          end pts
        in
        btyp_type_match (tt,pts)

    | t -> Flx_btype.map ~f_btype:(aux (level+1)) t
  in
  aux 0 src 

let list_subst counter x t =
  let t = alpha counter t in
  List.fold_left (fun t1 (i,t2) ->
    term_subst counter t1 i (alpha counter t2))
  t
  x

let varmap0_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,_) as x ->
        if Hashtbl.mem varmap i
        then Hashtbl.find varmap i
        else x
    | x -> x
  in
  f_btype t

let varmap_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,_) as x ->
        if Hashtbl.mem varmap i
        then Hashtbl.find varmap i
        else x
    | BTYP_type_function (p,r,b) ->
        let
          p = List.map (fun (name,kind) -> (name,kind)) p and
          r = r and
          b = f_btype b
        in
        btyp_type_function (p,r,b)
    | x -> x
  in
  f_btype t

(* the type arguments are matched up with the type
  variables in order so that
  vs_i -> ts_i

  NOTE: the meta-type is dropped here, because there's
  no way to find the meta-type of a type without
  looking up the symbol table. Hmm.
*)
let mk_varmap sr (vs:Flx_kind.bvs_t) ts =
  if List.length ts <> List.length vs
  then
    clierrx "[flx_core/flx_btype_subst.ml: E280] " sr 
    (
      "[mk_varmap] wrong number of type args, expected vs=" ^
      si (List.length vs) ^
      ", got ts=" ^
      si (List.length ts) ^
      "\nvs= " ^ Flx_util.catmap "," (fun (s,i,mt) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^
      "\nts= " ^ Flx_util.catmap "," Flx_btype.st ts
    )
  ;
  let varmap = Hashtbl.create 97 in
  List.iter2
  (fun (_, varidx,_) typ -> Hashtbl.add varmap varidx typ)
  vs ts
  ;
  varmap

let varmap_of_mgu mgu = 
  let varmap = Hashtbl.create 97 in
  List.iter (fun (varidx, typ) -> Hashtbl.add varmap varidx typ) mgu;
  varmap


let tsubst sr (vs:Flx_kind.bvs_t) ts t =
  varmap_subst (mk_varmap sr vs ts) t

let neuter_polyrecs msg t = 
  let rec aux t = 
    match Flx_btype.map ~f_btype:aux t with
    | Flx_btype.BTYP_polyrecord (flds,s,v) -> 
      (* if s <> "" then print_endline (msg ^ " Neutering row variable " ^ s);  *)
      Flx_btype.btyp_polyrecord flds "" v
    | t -> t
  in aux t




