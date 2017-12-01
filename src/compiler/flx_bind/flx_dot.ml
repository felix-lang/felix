open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_btype_subst

exception OverloadResolutionError

(*
  Order of sugars for operator dot.

  1. If the lhs and rhs are functions and the codomain of the lhs agrees with
      the domain of the rhs, this is a reverse composition, eg:

      1 . (str . trim) --> compose(trim, str) 1
 
  2. If the lhs is a struct, cstruct, or record AND the rhs is a simple name
      AND the name is a component, it's the component

  3. Try get method (now, for any type not just struct!)

  4. Try reverse application

  5. Deref once, try for field name. If found, result is POINTER to component.

  PROPERTIES
  ----------

  Commutativity of reverse composition and reverse application, eg:

  1 . str . trim == (1 . str) . trim == 1 . (str . trim)

  This property is VITAL for comprehension: the dot operator is syntactically
  associative. SPECIAL NOTE: COUNTEREXAMPLE: for any non-polymorphic function
  it is impossible to confuse composition and application. But for polymorphic
  functions it's possible! Therefore the order MATTERS.
  

*)

exception Not_field

let handle_field_name state bsym_table build_env env rs be bt koenig_lookup cal_apply bind_type' mkenv 
  sr e e2 name ts i ts' isptr
=
  let rt t = beta_reduce "flx_dot: handle_field_name" state.counter bsym_table sr t in
  let (_,t) as te = be e in
  let ttt =rt t in
  match hfind "lookup" state.sym_table i with

  (* STRUCT *)
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_struct ls }
  | { Flx_sym.id=id; sr=sra; symdef=SYMDEF_cstruct (ls,_) } ->
    let _,vs,_ = find_split_vs state.sym_table bsym_table i in
    let cidx,ct =
      let rec scan i = function
      | [] -> raise Not_field
      | (vn,vat)::_ when vn = name -> i,vat
      | _:: t -> scan (i+1) t
      in scan 0 ls
    in
    let ct =
      let bvs = List.map
        (fun (n,i,mt) -> n, btyp_type_var (i, Flx_btype.bmt "Flx_dot1" mt))
        (vs)
      in
      let env' = build_env state bsym_table (Some i) in
      bind_type' state bsym_table env' rsground sr ct bvs mkenv
    in
    let vs' = List.map (fun (s,i,tp) -> s,i, Flx_btype.bmt "Flx_dot" tp) vs in
    let ct = tsubst sr vs' ts' ct in
    let ct = if isptr then btyp_pointer ct else ct in
    (* messy .. we generalised get_n to accept any type instead
       of an integer selector. to replace integer n,
       we have to use case n of m where m is the number of
       cases: n is an int, whereas m is unitsum m' where m'
       is the number of cases.

       Note: bexpr_case is bugged! It can only be used
       for const constructors, the type of the case of T
       is always T. 
    *)
    bexpr_get_n ct cidx te
  | _ ->  raise Not_field




let handle_constant_projection bsym_table sr a ta n =
  begin match unfold "flx_lookup" ta with
  | BTYP_record fs -> 
    let m = List.fold_left (fun acc (s,_) -> acc + (if s = "" then 1 else 0)) 0 fs in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml: E70A] " sr ("AST_dot, record index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta ^ "\n" ^
      " only blank fields can be indexed!"
      )
    else
    bexpr_get_n (snd (List.nth fs n)) n a

  | BTYP_pointer (BTYP_record fs) ->
    let m = List.fold_left (fun acc (s,_) -> acc + (if s = "" then 1 else 0)) 0 fs in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml: E70A] " sr ("AST_dot, record index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta ^ "\n" ^
      " only blank fields can be indexed!"
      )
    else
    bexpr_get_n (btyp_pointer (snd (List.nth fs n))) n a



  | BTYP_tuple ls ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:109: E70B] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (List.nth ls n) n a

  | BTYP_tuple_cons (head,tail) ->
    let rec cal_prj head tail i =
    if i == 0 then
      bexpr_get_n head n a
    else  match tail with
    | BTYP_tuple_cons (h,t) ->
      cal_prj h t (i-1)

    | BTYP_tuple (h::ts) ->
      cal_prj h (btyp_tuple ts) (i-1)
    | _ ->
      clierrx "[flx_bind/flx_dot.ml:127: E71] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range for type " ^ sbt bsym_table ta
      )
    in 
    cal_prj head tail n 


  | BTYP_array (t,BTYP_unitsum m) ->
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:136: E72] " sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
      bexpr_get_n t n a

  | BTYP_pointer (BTYP_tuple ls as tup) when Flx_btype.islinear_type () tup ->
(*
print_endline ("projection " ^ si n ^ " of pointer to compact linear type " ^ sbt bsym_table tup);
*)
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
    let c = List.nth ls n in
(*
print_endline ("Component type = " ^ sbt bsym_table c);
*)
    (* let domain_cltptr_t = Flx_btype.btyp_cltpointer tup tup in *)
    let codomain_cltptr_t = Flx_btype.btyp_cltpointer tup c in

    (* coerce the machine pointer to a compact linear pointer *)
    let ptr = bexpr_cltpointer_of_pointer a in

    (* calculate the projection *)
(*
print_endline ("Tail components = " ^ catmap "," (sbt bsym_table) (Flx_list.list_tail ls (n+1)));
*)
    let divisor = 
      List.fold_left (fun acc t -> acc * (Flx_btype.sizeof_linear_type () t)) 1
        (Flx_list.list_tail ls (n+1))
    in
(*
print_endline ("Divisor for term " ^ si n ^ " is " ^ si divisor);
*)
    let prj = bexpr_cltpointer_prj tup c divisor in

    (* apply clt pointer projection to coerced pointer *)
    bexpr_apply codomain_cltptr_t ( prj, ptr )

  | BTYP_cltpointer (baseptr_t, (BTYP_tuple ls as tup)) when Flx_btype.islinear_type () tup ->
(*
print_endline ("projection " ^ si n ^ " of cltpointer to compact linear type " ^ sbt bsym_table tup);
*)
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
    let c = List.nth ls n in
(*
print_endline ("Component type = " ^ sbt bsym_table c);
*)
    let codomain_cltptr_t = Flx_btype.btyp_cltpointer tup c in

    let ptr =  a in

    (* calculate the projection *)
(*
print_endline ("Tail components = " ^ catmap "," (sbt bsym_table) (Flx_list.list_tail ls (n+1)));
*)
    let divisor = 
      List.fold_left (fun acc t -> acc * (Flx_btype.sizeof_linear_type () t)) 1
        (Flx_list.list_tail ls (n+1))
    in
(*
print_endline ("Divisor for term " ^ si n ^ " is " ^ si divisor);
*)
    let prj = bexpr_cltpointer_prj tup c divisor in

    (* apply clt pointer projection to coerced pointer *)
    bexpr_apply codomain_cltptr_t ( prj, ptr )

  (* ARRAY CASE *)
  | BTYP_pointer (BTYP_array (array_base, BTYP_unitsum array_count) as tup) when Flx_btype.islinear_type () tup ->
(*
print_endline ("projection " ^ si n ^ " of pointer to compact linear array type " ^ sbt bsym_table tup);
*)
    let m = array_count in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
    let c = array_base in
(*
print_endline ("Component type = " ^ sbt bsym_table c);
*)
    (* let domain_cltptr_t = Flx_btype.btyp_cltpointer tup tup in *)
    let codomain_cltptr_t = Flx_btype.btyp_cltpointer tup c in

    (* coerce the machine pointer to a compact linear pointer *)
    let ptr = bexpr_cltpointer_of_pointer a in

    (* calculate the projection *)

    (* the selected index is n, so there are m terms, if n is 0, there are
      m-1 on the right, and 0 on the left, if n is m - 1, there are 0 on
      the right, and m-1 on the left, so generally, there are m - n - 1
      terms on the right

      to get rid of x terms on the right, we divide by the array base
      raise to the power of x.
    *)
    let rec pow a b = match b with | 0 -> 1 | 1 -> a | _ -> a * pow a (b - 1) in
    let base_size = Flx_btype.sizeof_linear_type () array_base in
    let divisor = pow base_size (m - n - 1) in 
(*
print_endline ("Divisor for term " ^ si n ^ " is " ^ si divisor);
*)
    let prj = bexpr_cltpointer_prj tup c divisor in

    (* apply clt pointer projection to coerced pointer *)
    bexpr_apply codomain_cltptr_t ( prj, ptr )

  (* ARRAY CASE *)
  | BTYP_cltpointer (baseptr_t, (BTYP_array (array_base, BTYP_unitsum array_count) as tup)) when Flx_btype.islinear_type () tup ->
(*
print_endline ("projection " ^ si n ^ " of cltpointer to compact linear array type " ^ sbt bsym_table tup);
*)
    let m = array_count in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
    let c = array_base in
(*
print_endline ("Component type = " ^ sbt bsym_table c);
*)
    let codomain_cltptr_t = Flx_btype.btyp_cltpointer tup c in

    let ptr =  a in

    (* calculate the projection *)
    let rec pow a b = match b with | 0 -> 1 | 1 -> a | _ -> a * pow a (b - 1) in
    let base_size = Flx_btype.sizeof_linear_type () array_base in
    let divisor = pow base_size (m - n - 1) in 
(*
print_endline ("Divisor for term " ^ si n ^ " is " ^ si divisor);
*)
    let prj = bexpr_cltpointer_prj tup c divisor in

    (* apply clt pointer projection to coerced pointer *)
    bexpr_apply codomain_cltptr_t ( prj, ptr )

  | BTYP_pointer (BTYP_tuple ls) ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
      bexpr_get_n (btyp_pointer (List.nth ls n)) n a

  | BTYP_pointer (BTYP_array (t,BTYP_unitsum m)) -> 
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:155: E74] " sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (btyp_pointer t) n a

  | _ -> raise OverloadResolutionError
  end

let handle_array_projection bsym_table int_t sr a ta n =
  let n = 
    let ixt = match unfold "flx_lookup" ta with
      | BTYP_array (_,ixt)
      | BTYP_pointer (BTYP_array (_,ixt)) -> ixt
      | _ -> assert false
    in
    if snd n = int_t then bexpr_coerce (n,ixt)
    else n
  in
  match unfold "flx_lookup" ta with
  | BTYP_array (vt,ixt) ->
    assert (snd n = ixt);
    bexpr_apply vt (bexpr_aprj n ta vt, a)

  | BTYP_pointer (BTYP_array (vt,ixt)) ->
    assert (snd n = ixt);
    bexpr_apply (btyp_pointer vt) (bexpr_aprj n ta vt, a)
  | _ -> assert false


let try_bind_tie bsym_table counter sr ((_,ta) as a) =
  match ta with
  | BTYP_pointer (BTYP_tuple ls) ->
    let n = List.length ls in
    let ts = List.map (fun t -> btyp_pointer t) ls in
    let t = btyp_tuple ts in
    let es = List.map2 (fun t i -> bexpr_get_n (btyp_pointer t) i a) ls (Flx_list.nlist n) in
    bexpr_tuple t es
 
  | BTYP_pointer (BTYP_array (bt,ixt)) -> 
    begin match ixt with
    | BTYP_unitsum n when n<20 ->
      let t = (btyp_array (btyp_pointer bt, ixt)) in
      let es = List.map (fun i -> bexpr_get_n (btyp_pointer bt) i a) (Flx_list.nlist n) in
      bexpr_tuple t es

    | BTYP_unitsum _ -> clierr sr ("compiler restriction: " ^ 
      "tie of array requires unitsum less than 20, got " ^
      sbt bsym_table ixt)
    | _ -> clierr sr ("compiler restriction: " ^ 
      "tie of array requires index to be a unitsum, got " ^
      sbt bsym_table ixt)
    end
    
  | BTYP_pointer (BTYP_record fs) -> 
    let n = List.length fs in
    let es = List.map2 (fun (s,t) i -> s,bexpr_get_n (btyp_pointer t) i a) fs (Flx_list.nlist n) in
    bexpr_record es
 

  | _ -> raise OverloadResolutionError




