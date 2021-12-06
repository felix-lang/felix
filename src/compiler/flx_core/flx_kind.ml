(* See Flx_btype_kind for routine metatype, which finds the bound kind of a bound type *)
type kind =
  | KIND_type    (* copyable *)
  | KIND_linear  (* copyable or unique *)
  | KIND_borrowed (* copyable or shared *)
  | KIND_unitsum
  | KIND_compactlinear
  | KIND_bool
  | KIND_nat
  | KIND_tuple of kind list
  | KIND_function of kind * kind (* the kind of a type function from domain to codomain kinds *)

let kind_eq k1 k2 = k1 = k2

let rec sk k =
  match k with
  | KIND_type -> "TYPE"
  | KIND_linear -> "LINEAR"
  | KIND_borrowed -> "BORROWED"
  | KIND_unitsum -> "UNITSUM"
  | KIND_compactlinear -> "COMPACTLINEAR"
  | KIND_bool -> "BOOL"
  | KIND_nat -> "NAT"
  | KIND_tuple ks -> "(" ^ Flx_util.catmap ", " sk ks ^")"
  | KIND_function (d,c) -> sk d ^ " -> " ^ sk c

let kind_type = KIND_type
let kind_linear = KIND_linear
let kind_borrowed = KIND_borrowed
let kind_unitsum = KIND_unitsum
let kind_compactlinear = KIND_compactlinear
let kind_bool = KIND_bool
let kind_nat = KIND_nat
let kind_function (d, c) = KIND_function (d,c)
let kind_tuple ks = KIND_tuple ks

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
let ksolve_subtypes add_eqn lhs rhs =
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
  | KIND_bool, KIND_bool
    -> ()

  (* depth covariant *)
  | KIND_tuple ls, KIND_tuple rs ->
    if List.length ls = List.length rs then
      List.iter2 (fun l r -> add_eqn (l, r)) ls rs
    else raise Not_found

  (* contra-variant in domain, covariant in codomain *)
  | KIND_function (ld,lc), KIND_function (rd,rc) ->
    add_eqn (rd,ld); add_eqn (lc,rc)

  | _ -> raise Not_found



(* returns true if lhs >= rhs for all eqns *)
let kind_ge (eqns:keqns_t) = 
  try
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
        ksolve_subtypes add_eqn lhs rhs;
        loop ()
    in
    loop ();
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
  if kind_ge [a, b] then b else if kind_ge [b, a] then a
  else failwith ("Flx_kind: kind_unify, " ^ sk a ^ " doesn't unify with " ^ sk b)

(* returns most general kind *)
let kind_max2 a b =
  if kind_ge [a, b] then a else if kind_ge [b, a] then b
  else failwith ("Flx_kind: kind_unify, " ^ sk a ^ " doesn't unify with " ^ sk b)

let kind_min ks = 
  match ks with
  | [] -> assert false (* should return BOTTOM *)
  | h::t -> List.fold_left kind_min2 h t

let kind_max ks = 
  match ks with
  | [] -> assert false (* should return BOTTOM *)
  | h::t -> List.fold_left kind_min2 h t


