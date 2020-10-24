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

let handle_constant_projection bsym_table sr a ta n =
(*
print_endline ("Constant projection " ^ string_of_int n ^ " of type " ^ sbt bsym_table ta);
*)
  begin match unfold "flx_lookup" ta with

(* RECORD *)
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

(* POINTER TO RECORD *)
  | BTYP_ptr (mode,BTYP_record fs,[]) ->
    let m = List.fold_left (fun acc (s,_) -> acc + (if s = "" then 1 else 0)) 0 fs in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml: E70A] " sr ("AST_dot, record index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta ^ "\n" ^
      " only blank fields can be indexed!"
      )
    else
    bexpr_get_n (btyp_ptr mode (snd (List.nth fs n)) []) n a

(* TUPLE *)
  | BTYP_compacttuple ls
  | BTYP_tuple ls ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:109: E70B] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (List.nth ls n) n a

(* TUPLE CONS *)
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

(* ARRAY *)
  | BTYP_compactarray (t,BTYP_unitsum m)
  | BTYP_array (t,BTYP_unitsum m) ->
(* print_endline ("get n=" ^ string_of_int n ^ " of " ^ string_of_int m ^ " base type " ^ sbt bsym_table t); *)
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:136: E72] " sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
      bexpr_get_n t n a

(* POINTER TO TUPLE: compact linear *)
  | BTYP_ptr (mode,(BTYP_compacttuple ls as tup),baseptr_t) ->
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
    bexpr_cltpointer tup c a [n] 

(* POINTER TO ARRAY: compact linear *)
  (* ARRAY CASE *)
  | BTYP_ptr (mode,(BTYP_compactarray (array_base, BTYP_unitsum array_count) as tup),[]) ->
(*
print_endline ("projection " ^ si n ^ " of pointer to compact linear array type " ^ sbt bsym_table tup);
*)
    let m = array_count in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
    let c = array_base in
(*
print_endline ("Component type = " ^ sbt bsym_table c);
*)
    bexpr_cltpointer tup c a [n] 
(*
    (* let domain_cltptr_t = Flx_btype.btyp_cltpointer tup tup in *)
    let codomain_cltptr_t = Flx_btype.btyp_ptr mode c [tup] in

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
*)

   | BTYP_ptr (mode,BTYP_tuple ls,[]) ->
    let m = List.length ls in
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:146: E73] " sr ("AST_dot, tuple index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
      bexpr_get_n (btyp_ptr mode (List.nth ls n) []) n a

  | BTYP_ptr (mode,BTYP_array (t,BTYP_unitsum m),[]) -> 
    if n < 0 || n >= m then
      clierrx "[flx_bind/flx_dot.ml:155: E74] " sr ("AST_dot, constant array index "^ string_of_int n ^ 
      " out of range 0 to " ^ string_of_int (m-1) ^
      " for type " ^ sbt bsym_table ta
      )
    else
     bexpr_get_n (btyp_ptr mode t []) n a

  (* soft error because user could overload with apply function *)
  | _ -> raise OverloadResolutionError
  end

let handle_array_projection bsym_table int_t sr a ta n =
(*
print_endline ("Array projection " ^ sbe bsym_table n ^ " of array type " ^ sbt bsym_table ta);
*)
  let n = 
    let ixt = match unfold "flx_lookup" ta with
      | BTYP_array (_,ixt)
      | BTYP_compactarray (_,ixt)
      | BTYP_ptr (_,BTYP_array (_,ixt),[]) -> ixt
      | BTYP_ptr (_,BTYP_compactarray (_,ixt),[]) -> ixt
      | _ -> assert false
    in
    if snd n = int_t then bexpr_coerce (n,ixt)
    else n
  in
  match unfold "flx_lookup" ta with
  | BTYP_compactarray (vt,ixt)
  | BTYP_array (vt,ixt) ->
    assert (snd n = ixt);
    bexpr_apply vt (bexpr_aprj n ta vt, a)

  | BTYP_ptr (mode,BTYP_array (vt,ixt),[]) ->
    assert (snd n = ixt);
    bexpr_apply (btyp_ptr mode vt []) (bexpr_aprj n ta vt, a)

  | _ -> 
    (* We have to do a warning not a hard error here, because applying
       an integer to some non-array type is could be done with
       an apply operator
    *)
    print_endline ("Flx_dot: Array projection requires array or pointer thereto, got type:\n" ^
      sbt bsym_table ta);
    print_endline (Flx_srcref.long_string_of_src sr);
    raise OverloadResolutionError


(* rref/wref .. did I get this right *)
let try_bind_tie bsym_table counter sr ((_,ta) as a) =
  match ta with
  | BTYP_ptr (mode,BTYP_tuple ls,[]) ->
    let n = List.length ls in
    let ts = List.map (fun t -> btyp_ptr mode t []) ls in
    let t = btyp_tuple ts in
    let es = List.map2 (fun t i -> bexpr_get_n (btyp_ptr mode t []) i a) ls (Flx_list.nlist n) in
    bexpr_tuple t es

  | BTYP_ptr (mode,BTYP_array (bt,ixt),[]) -> 
    begin match ixt with
    | BTYP_unitsum n when n<20 ->
      let t = (btyp_array (btyp_ptr mode bt [], ixt)) in
      let es = List.map (fun i -> bexpr_get_n (btyp_ptr mode bt []) i a) (Flx_list.nlist n) in
      bexpr_tuple t es

    | BTYP_unitsum _ -> clierr sr ("compiler restriction: " ^ 
      "tie of array requires unitsum less than 20, got " ^
      sbt bsym_table ixt)
    | _ -> clierr sr ("compiler restriction: " ^ 
      "tie of array requires index to be a unitsum, got " ^
      sbt bsym_table ixt)
    end

  | BTYP_ptr (mode,BTYP_record fs,[]) -> 
    let n = List.length fs in
    let es = List.map2 (fun (s,t) i -> s,bexpr_get_n (btyp_ptr mode t []) i a) fs (Flx_list.nlist n) in
    bexpr_record es
 
  (* this is a hard error, the generic _tie cannot be overloaded *)
  | _ -> 
    clierr sr ("Flx_dot: Generic _tie requires product or pointer thereto, got type:\n" ^
    sbt bsym_table ta)




