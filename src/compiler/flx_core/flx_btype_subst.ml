open Flx_btype
open Flx_exceptions
open Flx_bid

let si x = string_of_int x

let var_subst t (i, j) =
  let rec f_btype t =
    match t with
    | BTYP_type_var (k,m,kind) when i = k -> btyp_type_varm (j,m,kind)
    | t -> Flx_btype.map ~f_btype t
  in
  f_btype t

let vars_subst ls t = List.fold_left var_subst t ls

let term_subst counter src i arg =
  let rec aux level t =
    match t with
    | BTYP_type_var (k,`N,_) when k = i -> widen_fixgap level arg 
    | BTYP_type_var (k,`V,_) when k = i -> 
print_endline ("TERMS SUBST VIEW VARIABLE");
      widen_fixgap level (viewify_type arg)

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

let list_subst counter ls t =
  let t = Flx_alpha.alpha_convert counter t in
  List.fold_left (fun t1 (i,t2) -> term_subst counter t1 i t2) t ls

let varmap0_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,m,_) as x ->
        if Hashtbl.mem varmap i
        then 
          let t = Hashtbl.find varmap i in
          match m with
          | `N -> t
          | `V -> viewify_type t
        else x
    | x -> x
  in
  f_btype t

let varmap_subst varmap t =
  let rec f_btype t =
    match Flx_btype.map ~f_btype t with
    | BTYP_type_var (i,m,_) as x ->
        if Hashtbl.mem varmap i
        then 
          let t = Hashtbl.find varmap i in
          match m with
          | `N -> t
          | `V -> viewify_type t
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

let str_of_varmap table = String.concat ", " 
  (Hashtbl.fold (fun k v lst -> (string_of_int k ^ " |-> " ^ Flx_btype.st v) :: lst) table []) 

let varmap_of_mgu mgu = 
  let varmap = Hashtbl.create 97 in
  List.iter (fun (varidx, typ) -> Hashtbl.add varmap varidx typ) mgu;
  varmap

let compose_varmaps a b =
  let c = Hashtbl.create 97 in
  Hashtbl.iter (fun k t -> Hashtbl.add c k (varmap_subst b t)) a;
  c

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




