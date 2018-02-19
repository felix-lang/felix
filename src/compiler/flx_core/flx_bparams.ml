open Flx_bparameter
type xps_t = Flx_bparameter.t Flx_ast.sexpr_t
type t = xps_t * Flx_bexpr.t option

let bparams_of_list ps =
  match ps with
  | [p] -> Flx_ast.Satom p
  | _ -> Flx_ast.Slist (List.map (fun p -> Flx_ast.Satom p) ps)

(** Returns the bids from a bparameters sexpr as a list. *)
let rec xget_bids ps = match ps with
  | Flx_ast.Satom p -> [p.pindex]
  | Flx_ast.Slist ps -> List.concat (List.map xget_bids ps)
    

(** Returns the bound types from a bparameter sexpr as a list *)
let rec xget_btypes ps = match ps with
  | Flx_ast.Satom p -> [p.ptyp]
  | Flx_ast.Slist ps -> List.concat (List.map xget_btypes ps)


(** Returns the bound type of a of bparameter sexpr as a tuple *)
let rec xget_btype ps = match ps with
  | Flx_ast.Satom p -> p.ptyp
  | Flx_ast.Slist ps -> Flx_btype.btyp_tuple (List.map xget_btype ps)


(** Returns the bids from a bparameters sexpr as a list. *)
let get_bids (bp,_) = xget_bids bp

(** Returns the bound types from a bparameter sexpr as a list *)
let get_btypes (bp,_) = xget_btypes bp

(** Returns the bound type of a of bparameter sexpr as a tuple *)
let get_btype (bp,_) = xget_btype bp

let rec xiter f_bid f_btype f_bexpr ps = match ps with
  | Flx_ast.Satom {pindex=pindex; ptyp=ptyp} -> f_bid pindex; f_btype ptyp
  | Flx_ast.Slist pss -> List.iter (xiter f_bid f_btype f_bexpr) pss

let iter 
  ?(f_bid=fun _ -> ()) 
  ?(f_btype=fun _ -> ()) 
  ?(f_bexpr=fun _ -> ()) 
  (ps,traint) 
=
  xiter f_bid f_btype f_bexpr ps;
  match traint with
  | Some x -> f_bexpr x
  | None -> ()

let rec xpiter f ps = match ps with
  | Flx_ast.Satom p -> f p
  | Flx_ast.Slist pps -> List.iter (xpiter f) pps

let piter f (ps,_) = xpiter f ps

let rec xpmap f ps = match ps with
  | Flx_ast.Satom p -> Flx_ast.Satom (f p)
  | Flx_ast.Slist pps -> Flx_ast.Slist (List.map (xpmap f) pps)

(* returns list of parameter index/projection pairs, where a projection
is a list consisting of the indices of the projection to
extract the component, eg in (p,(q,r)), q has projection 0,1
*)

let rec xget_prj_ixs ps = match ps with
  | Flx_ast.Satom p -> [p.pindex,[]]
  | Flx_ast.Slist pss -> 
    List.concat 
    (
      List.map2 
      (fun ps j -> 
        List.map 
        (fun (ix,prj) -> ix, j::prj) 
        (xget_prj_ixs ps) 
      )
      pss 
      (Flx_list.nlist (List.length pss)) 
    )

let get_prj_ixs (ps,_) = xget_prj_ixs ps

(* this one returns actual projections.*)
let rec xget_prjs ps = match ps with
  | Flx_ast.Satom p -> [p, None]

  | Flx_ast.Slist pss -> 
    let domain = xget_btype ps in
    List.concat 
    (
      List.map2 
      (fun p pos ->
         let codomain = xget_btype p in
         let prj = Flx_bexpr.bexpr_prj pos domain codomain in
         let rs = xget_prjs p in
         List.map
         (fun (p,prjs) -> match prjs with 
           | None -> p,Some prj
           | Some ((_, Flx_btype.BTYP_function (d,c)) as sprj) ->
             let ft = Flx_btype.btyp_function (domain,c) in 
             p, Some (Flx_bexpr.bexpr_compose ft (sprj,prj))
           | _ -> assert false
         )
         rs
      ) 
      pss (Flx_list.nlist (List.length pss))
    )

let get_prjs (ps,_) = xget_prjs ps
let get_params (ps,_) = List.map fst (xget_prjs ps)
let get_names ps = List.map (fun {pid=id}->id) (get_params ps)

let rec xmap f_bid f_btype f_bexpr ps = match ps with
  | Flx_ast.Satom {pid=pid;pindex=pindex;pkind=pkind;ptyp=ptyp} -> 
    Flx_ast.Satom {pid=pid; pindex=f_bid pindex; pkind=pkind; ptyp=f_btype ptyp}
  | Flx_ast.Slist pss -> Flx_ast.Slist (List.map (xmap f_bid f_btype f_bexpr) pss)

let map ?(f_bid=fun i -> i) ?(f_btype=fun t -> t) ?(f_bexpr=fun x->x)  (ps,traint) =
  xmap f_bid f_btype f_bexpr ps,
  (match traint with
  | Some x -> Some ( f_bexpr x)
  | None -> None
  )

let unit_bparams : t = Slist [], None

