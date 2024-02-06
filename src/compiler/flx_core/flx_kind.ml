(* See Flx_btype_kind for routine metatype, which finds the bound kind of a bound type *)

type sort = | SORT_kind
let str_of_sort s = match s with | SORT_kind -> "KIND"
let bind_sortcode s = match s with | Flx_ast.SRT_kind -> SORT_kind

type kind =
  | KIND_type    (* copyable *)
  | KIND_linear  (* copyable or unique *)
  | KIND_borrowed (* copyable or shared *)
  | KIND_unitsum
  | KIND_compactlinear
  | KIND_bool
  | KIND_nat
  | KIND_typeset
  | KIND_tuple of kind list
  | KIND_function of kind * kind (* the kind of a type function from domain to codomain kinds *)
  | KIND_var of string
  | KIND_view (* same as linear with write pointers removed *)

let rec sk k =
  match k with
  | KIND_type -> "TYPE"
  | KIND_linear -> "LINEAR"
  | KIND_borrowed -> "BORROWED"
  | KIND_unitsum -> "UNITSUM"
  | KIND_compactlinear -> "COMPACTLINEAR"
  | KIND_bool -> "BOOL"
  | KIND_nat -> "NAT"
  | KIND_typeset -> "TYPESET"
  | KIND_tuple ks -> "(" ^ Flx_util.catmap ", " sk ks ^")"
  | KIND_function (d,c) -> sk d ^ " -> " ^ sk c
  | KIND_var s -> s
  | KIND_view  -> "VIEW"


let map f (k:kind):kind = match k with
  | KIND_tuple ks -> KIND_tuple (List.map f ks)
  | KIND_function (a,b) -> KIND_function (f a, f b)
  | k -> k

type bk_t = string * Flx_bid.bid_t * sort  
type bks_t = bk_t list

let sks ks = String.concat "," (List.map sk ks)


let rec ksubst1 (knd:kind) (name:string) (k:kind) : kind =
  match knd with
  | KIND_var name' when name = name' -> k
  | knd -> map (fun knd -> ksubst1 knd name k) knd

let ksubst_eqns sr knks k = 
  List.fold_left (fun acc (name,k)  -> ksubst1 acc name k) k knks

let ksubst sr (bks:bks_t) (ks: kind list) (k:kind):kind =
  if List.length bks <> List.length ks then assert false;
  let knks = List.map2 (fun (name, index, srt) k -> name,k) bks ks in
  ksubst_eqns sr knks k

let kind_eq k1 k2 = k1 = k2

let kind_type = KIND_type
let kind_linear = KIND_linear
let kind_borrowed = KIND_borrowed
let kind_unitsum = KIND_unitsum
let kind_compactlinear = KIND_compactlinear
let kind_bool = KIND_bool
let kind_nat = KIND_nat
let kind_typeset = KIND_typeset
let kind_function (d, c) = KIND_function (d,c)
let kind_tuple ks = KIND_tuple ks
let kind_var s = KIND_var s
let kind_view = KIND_view 

(* this probably doesn't belong here .. *)
type bv_t = string * Flx_bid.bid_t * kind
type bvs_t = bv_t list
(* Unification *)
type keqn_t = kind * kind
type keqns_t = keqn_t list

(* lhs param type = supertype, rhs argtype = subtype 
 subyping chain:
 unitsum -> compactlinear -> linear -> type -> borrowed
*)

type kmgu_t = (string * kind) list

(* NOTE: here the rhs is a subtype of the lhs *)
let ksolve_subtypes add_eqn lhs rhs (mgu:kmgu_t ref) =
  match lhs, rhs with
  | KIND_borrowed, KIND_borrowed
  | KIND_borrowed, KIND_type
  | KIND_borrowed, KIND_linear
  | KIND_borrowed, KIND_compactlinear
  | KIND_borrowed, KIND_unitsum

  | KIND_linear, KIND_linear
  | KIND_linear, KIND_type
  | KIND_linear, KIND_unitsum
  | KIND_linear, KIND_compactlinear

  | KIND_type, KIND_type
  | KIND_type, KIND_unitsum
  | KIND_type, KIND_compactlinear

  | KIND_compactlinear, KIND_unitsum
  | KIND_compactlinear, KIND_compactlinear

  | KIND_unitsum, KIND_unitsum


  | KIND_nat, KIND_nat
  | KIND_typeset, KIND_typeset
  | KIND_bool, KIND_bool
 
  (* A view is basically the types not allowing mutation *)
  | KIND_view, KIND_view
  | KIND_borrowed, KIND_view
  | KIND_linear, KIND_view
  | KIND_type, KIND_view
  | KIND_view, KIND_compactlinear
  | KIND_view, KIND_unitsum

    -> ()

  (* depth covariant *)
  | KIND_tuple ls, KIND_tuple rs ->
    if List.length ls = List.length rs then
      List.iter2 (fun l r -> add_eqn (l, r)) ls rs
    else raise Not_found

  (* contra-variant in domain, covariant in codomain *)
  | KIND_function (ld,lc), KIND_function (rd,rc) ->
    add_eqn (rd,ld); add_eqn (lc,rc)

  (* We assume at the moment, all variables are dependent. A variable
     can get on the wrong side because of the contravariance of function 
     domains. FIXME: we need to do as in the type unification algo,
     and keep track of which are the dependent variables!

     EMERGENCY QUESTION: what happens in the type unification algo?
  *)
  
  | x, KIND_var name 
  | KIND_var name, x ->
   (* we add the specialisation to the MGU unless it is
   already in there. If it is in there with the same value,
   just proceed. If it is in there with a different value,
   we have to barf *)
   if not (List.mem_assoc name !mgu) then
     mgu := (name, x) :: !mgu
   else if x = List.assoc name !mgu then ()
   else raise Not_found
    

  | _ -> raise Not_found



(* returns the MGU if eqns can be satisfied, otherwise raises Not_found.
Note, they're actually inequalities, and, this is the specialisation
version of unification.
*)
let kind_unif (eqns:keqns_t) : kmgu_t = 
  let mgu = ref [] in
  let history : keqns_t ref = ref eqns in
  let eqns : keqns_t ref = ref eqns in
  let add_eqn (eqn:keqn_t) =
    if List.mem eqn (!history) then ()
    else begin
       eqns := eqn :: (!eqns);
       history := eqn :: (!eqns)
    end
  in
  let rec loop () : unit =
    match !eqns with
    | [] -> ()
    | h :: t ->
      eqns := t;
      let (lhs,rhs): keqn_t = h in 
      ksolve_subtypes add_eqn lhs rhs mgu;
      loop ()
  in
  loop ();
  !mgu

(* returns true if lhs >= rhs for all eqns *)
let kind_ge eqns =
  try 
    let _ =  kind_unif eqns  in
    true
  with Not_found -> false


let kind_ge2 a b = kind_ge [a,b]

(* we could do this faster with a specialised equality routine, in fact just a = b 
should work since there are, as yet, no kind variables or functions
*)
let kind_eq2 a b =
  kind_ge2 a b  && kind_ge2 b a

(* returns the most specialised kind *)
(* FIXME: should throw exception which can be trapped by caller *)
let kind_min2 a b =
  if kind_ge2 a b then b else if kind_ge2 b a then a
  else failwith ("Flx_kind: kind_unify, " ^ sk a ^ " doesn't unify with " ^ sk b)

(* returns most general kind *)
let kind_max2 a b =
  if kind_ge2 a b then a else if kind_ge2 b a then b
  else failwith ("Flx_kind: kind_unify, " ^ sk a ^ " doesn't unify with " ^ sk b)

let kind_min ks = 
  match ks with
  | [] -> assert false (* should return TOP *)
  | h::t -> List.fold_left kind_min2 h t

let kind_max ks = 
  match ks with
  | [] -> assert false (* should return BOTTOM *)
  | h::t -> List.fold_left kind_max2 h t


