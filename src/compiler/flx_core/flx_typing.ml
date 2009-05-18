open Flx_ast
open Flx_types
open Flx_srcref
open List

(* MOVED FROM PARSER so flx_sex2flx can do the nasty work
   -- too lazy to implement this in Scheme at the moment --
*)

(* handle curried type functions *)
let mktypefun sr (name:string) (vs:vs_list_t) (args: (string * typecode_t) list list) (return_type:typecode_t) (body:typecode_t) : statement_t =
  let argtyp t = match t with
    | [] -> failwith "Lambda abstraction requires nonunit parameter"
    | [x] -> x
    | x -> `TYP_type_tuple x
  in
  let body =
    let p = ref (List.rev args) in
    let r = ref return_type in
    let b = ref body in
    while !p <> [] do
      let arg = List.hd !p in
      p := List.tl !p;
      b := `TYP_typefun (arg, !r, !b);
      r := `TYP_function(argtyp (List.map snd (arg)),!r)
    done;
    !b
  in
  `AST_type_alias
  (
    sr,
    name,
    vs,
    body
  )


let sye {base_sym=i} = i

let all_voids ls =
    fold_left
    (fun acc t -> acc && (t = `BTYP_void))
    true ls

let all_units0 ls =
    fold_left
    (fun acc t -> acc && (t = `BTYP_tuple []))
    true ls

let all_units ls = all_units0 ls

let is_unitsum (t:btypecode_t) = match t with
  | `BTYP_unitsum _ -> true
  | `BTYP_sum ls ->  all_units ls
  | _ -> false


let int_of_unitsum t = match t with
  | `BTYP_void -> 0
  | `BTYP_tuple [] -> 1
  | `BTYP_unitsum k -> k
  | `BTYP_sum [] ->  0
  | `BTYP_sum ls ->
    if all_units ls then length ls
    else raise Not_found

  | _ -> raise Not_found

exception UnificationError of btypecode_t * btypecode_t

(* unbound type *)
let type_of_argtypes ls = match ls with
 | [x] -> x
 | _ -> `TYP_tuple ls

let funparamtype (_,_,t,_) = t

module FuntypeSet = Set.Make(
  struct type t=typecode_t let compare = compare end
)

module FunInstSet = Set.Make(
  struct
    type t= bid_t * btypecode_t list
    let compare = compare
  end
)


let typeofbps bps =
  map
  (fun {ptyp=t; pkind=k} ->
    match k with
(*    | `PRef -> `BTYP_pointer t *)
    | `PFun -> `BTYP_function (`BTYP_tuple [],t)
    | _ ->t
  )
  bps

let typeofbps_traint (bps,_) = typeofbps bps

(* bound type! *)
let typeoflist typlist = match typlist with
  | [] -> `BTYP_tuple []
  | [t] -> t
  | h :: t ->
    try
      iter
      (fun t -> if t <> h then raise Not_found)
      t;
      `BTYP_array (h,`BTYP_unitsum (length typlist))
    with Not_found ->
      `BTYP_tuple typlist

let flx_bool = `TYP_unitsum 2
let flx_bbool = `BTYP_unitsum 2

(* Note floats are equal iff they're textually identical,
   we don't make any assumptions about the target machine FP model.
   OTOH, int comparisons are infinite precision, for the same
   int kind, even if the underlying machine model is not
*)

let cmp_literal (l:literal_t) (l':literal_t) = match l, l' with
  | `AST_int (a,b), `AST_int (a',b') -> a = a' && Big_int.eq_big_int b b'
  | `AST_float (a,b), `AST_float (a',b') -> a = a' && b = b'
  | `AST_string s, `AST_string s' -> s = s'
  | `AST_cstring s, `AST_cstring s' -> s = s'
  | `AST_wstring s, `AST_wstring s' -> s = s'
  | `AST_ustring s, `AST_ustring s' -> s = s'
  | _ -> false

(* Note that we don't bother comparing the type subterm:
  this had better be equal for equal expressions: the value
  is merely the cached result of a synthetic context
  independent type calculation
*)

let rec cmp_tbexpr ((a,_) as xa) ((b,_) as xb) =
  let ecmp = cmp_tbexpr in match a,b with
  | `BEXPR_coerce (e,t),`BEXPR_coerce (e',t') ->
    (* not really right .. *)
    ecmp e e'

  | `BEXPR_record ts,`BEXPR_record ts' ->
    length ts = length ts' &&
    let rcmp (s,t) (s',t') = compare s s' in
    let ts = sort rcmp ts in
    let ts' = sort rcmp ts' in
    map fst ts = map fst ts' &&
    fold_left2 (fun r a b -> r && a = b) true (map snd ts) (map snd ts')

  | `BEXPR_variant (s,e),`BEXPR_variant (s',e') ->
    s = s' && ecmp e e'

  | `BEXPR_deref e,`BEXPR_deref e' -> ecmp e e'

  | `BEXPR_name (i,ts),`BEXPR_name (i',ts')
  | `BEXPR_ref (i,ts),`BEXPR_ref (i',ts')
  | `BEXPR_closure (i,ts),`BEXPR_closure (i',ts') ->
     i = i' &&
     fold_left2 (fun r a b -> r && a = b) true ts ts'

  (* Note any two distinct new expressions are distinct ...
    not sure what is really needed here
  *)
  | `BEXPR_new e1,`BEXPR_new e2 -> false
  | `BEXPR_not e1,`BEXPR_not e2 -> false

  | _,`BEXPR_likely e2
  | _,`BEXPR_unlikely e2 -> ecmp xa e2

  | `BEXPR_likely e1,_
  | `BEXPR_unlikely e1,_ -> ecmp e1 xb

  | `BEXPR_literal a,`BEXPR_literal a' -> cmp_literal a a'

  | `BEXPR_apply (a,b),`BEXPR_apply (a',b') ->  ecmp a a' && ecmp b b'

  | `BEXPR_apply_prim (i,ts,b),`BEXPR_apply_prim (i',ts',b')
  | `BEXPR_apply_direct (i,ts,b),`BEXPR_apply_direct (i',ts',b')
  | `BEXPR_apply_struct (i,ts,b),`BEXPR_apply_struct (i',ts',b')
  | `BEXPR_apply_stack (i,ts,b),`BEXPR_apply_stack (i',ts',b') ->
     i = i' &&
     fold_left2 (fun r a b -> r && a = b) true ts ts' &&
     ecmp b b'

  | `BEXPR_tuple ls,`BEXPR_tuple ls' ->
     fold_left2 (fun r a b -> r && ecmp a b) true ls ls'

  | `BEXPR_case_arg (i,e),`BEXPR_case_arg (i',e')

  | `BEXPR_match_case (i,e),`BEXPR_match_case (i',e')
  | `BEXPR_get_n (i,e),`BEXPR_get_n (i',e') ->
    i = i' && ecmp e e'

  (* this is probably wrong: says x.y = x'.y' iff x = x && y = y',
  however, x.y should unify with a simple value .. oh well..
  hmm .. this should REALLY be a pointer to member, that is,
  an actual projection function
  *)
  | `BEXPR_get_named (i,e),`BEXPR_get_named (i',e') ->
    i = i' && ecmp e e'

  | `BEXPR_case_index e,`BEXPR_case_index e' -> ecmp e e'

  | `BEXPR_case (i,t),`BEXPR_case (i',t') -> i = i' && t = t'
  | `BEXPR_expr (s,t),`BEXPR_expr (s',t') -> s = s' && t = t'
  | `BEXPR_range_check (e1,e2,e3), `BEXPR_range_check (e1',e2',e3') ->
    ecmp e1 e1' && ecmp e2 e2' && ecmp e3 e3'

  | _ -> false
