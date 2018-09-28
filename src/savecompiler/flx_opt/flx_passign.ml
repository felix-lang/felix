open Flx_util
open Flx_ast
open Flx_types
open Flx_bexpr
open Flx_bexe
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_call
open Flx_bid

type aentry_t =
  bid_t *
  (string * Flx_btype.t * Flx_bexpr.t * BidSet.t)
(* parameter index, (parameter name, parameter type, initialising expression, dependendency set) *)


(* Parallel Assignment algorithm.
   Given a set of assignments, xi = ei,
   we need a sequence of assignments of xi, ei, tj,
   where tj are fresh variables, xi on left, ei on
   right, and tj on either side, such that no RHS
   term depends on a prior LHS term.

   A pair x1 = e1, x2 = e2 which are mutually dependent
   can always by resolved as

   t1 = e1; x2 = e2; x1 = t1

   Here e1 doesn't depend on a prior term, vaccuously,
   e2 can't depend on t1 since it is fresh, and
   t1 can't depend on anything, since it just a fresh variable

   Let's start by taking the equations, and making
   two lists -- a head list and a tail list.
   Head assignments are done first, tails last,
   the head list is in reverse order.

   Any equations setting variables no one depends on
   can be moved into the head list, they can safely
   be done first.

   Any equations whose RHS depend on nothing are
   moved into the tail list, its safe to do them last.

   Any dependencies on variables set by equations
   moved into the tail list can now be removed
   from the remaining equations, since it is determined
   now that these variables will be changed after
   any of the remaining assignments are one.

   Repeat until the set of remaining equations is fixed.

   We can now pick (somehow!!) an equation, and break
   it into two using a fresh temporary. The temporary
   assignment goes on the head list, the variable
   assignment from the temporary on the tail list,
   and as above, any dependencies on the variable
   can now be removed from the remaining equations.

   Repeat everything until the set of remaining
   equations is empty, the result is the reverse
   of the heap list plus the tail list.

   This process is certain to terminate, since
   each outer step removes one equation,
   and it is certain to be correct (obvious).

   What is NOT clear is that the result is minimal.
   And it is NOT clear how to best 'choose' which
   equation to split.
*)


(* Parallel Assignment Algorithm *)

(* input: a list of equations of the form
  x = expr

Represented by:

    i,(name,t,e,u)

where

  i = the LHS target
  name = the LHS target name for debug purpose
  t = the LHS type
  e = the RHS expression
  u = an BidSet of all the symbols used in e
      being a subset of the set of all the LHS variables
      but including any indirect use!

Output:

  A sequence of assignments minimising temporary usage
  IN REVERSE ORDER

*)

let passign syms bsym_table (pinits:aentry_t list) sr =
(*
print_endline ("Running parallel assignment routine");
*)
  let parameters = ref [] in
  (* strip trivial assignments like x = x *)
  let pinits =
    filter
    (fun (i,(name,t,e,u)) ->
      match e with
      | BEXPR_varname (j,_),_ when i = j -> false
      | _ -> true
    )
    pinits
  in
  let fixdeps pinits =
    let vars = fold_left (fun s (i,_) -> BidSet.add i s) BidSet.empty pinits in
    map
    (fun (i,(name,t,e,u)) ->
      let u = BidSet.remove i (BidSet.inter u vars) in
      i,(name,t,e,u)
    )
    pinits
  in
  (*
  iter
  (fun (i,(name,t,e,u)) ->
    print_endline ("ASG " ^ name ^ "<"^string_of_bid i ^ "> = " ^ sbe bsym_table e);
    print_string "  Depends: ";
      BidSet.iter (fun i -> print_string (string_of_bid i ^ ", ")) u;
    print_endline "";
  )
  pinits;
  *)
  (* this function measures if the expression assigning i
  depends on the old value of j
  *)
  let depend pinits i j =
     let u = match assoc i pinits with _,_,_,u -> u in
     BidSet.mem j u
  in
  (* return true if an assignment in inits depends on j *)
  let used j inits =
    fold_left (fun r (i,_)-> r || depend inits i j) false inits
  in
  let rec aux ((head, middle, tail) as arg) = function
    | [] -> arg
    | (i,(name,ty,e,u)) as h :: ta ->
      if BidSet.cardinal u = 0 then
        aux (head,middle,h::tail) ta
      else if not (used i (middle @ ta)) then
        aux (h::head, middle, tail) ta
      else
        aux (head,h::middle,tail) ta
  in

  let printem (h,m,t) =
    print_endline "HEAD:";
    iter
    (fun (i,(name,t,e,u)) ->
      print_endline ("ASG " ^ name ^ "<"^string_of_bid i ^ "> = " ^ sbe bsym_table e)
    )
    h;

    print_endline "MIDDLE:";
    iter
    (fun (i,(name,t,e,u)) ->
      print_endline ("ASG " ^ name ^ "<"^string_of_bid i ^ "> = " ^ sbe bsym_table e)
    )
    m;

    print_endline "TAIL:";
    iter
    (fun (i,(name,t,e,u)) ->
      print_endline ("ASG " ^ name ^ "<"^string_of_bid i ^ "> = " ^ sbe bsym_table e)
    )
    t
  in

  let rec aux2 (hh,mm,tt) =
    let h,m,t = aux ([],[],[]) (fixdeps mm) in
    (* printem (h,m,t); *)
    (* reached a fixpoint? *)
    if length h = 0 && length t = 0 then hh,m,tt (* m = mm *)
    else begin
      (*
      print_endline "Recursing on MIDDLE";
      *)
      aux2 (h @ hh, m, t @ tt)
    end
  in
  let tmplist = ref [] in
  let rec aux3 (hh,mm,tt) =
    let h,m,t = aux2 (hh,mm,tt) in
    (*
    print_endline "SPLIT STEP result:";
    printem(h,m,t);
    *)
    match m with
    | [] -> rev h @ t
    | [_] -> assert false
    | (i,(name,ty,e,u)) :: ta ->
      let k = fresh_bid syms.counter in
      let name2 = "_tmp_" ^ name in
      parameters := (ty,k) :: !parameters;
      tmplist := k :: !tmplist;
      let h' = k,(name2,ty,e,BidSet.empty) in
      let e' = bexpr_varname ty (k,[]) in
      let t' = i,(name,ty,e',BidSet.empty) in
      aux3 (h' :: h, ta, t' :: t)
  in
  let m = aux3 ([],pinits,[]) in
  (*
  print_endline "FINAL SPLIT UP:";
  iter
  (fun (i,(name,t,e,u)) ->
    print_endline ("ASG " ^ name ^ "<"^string_of_bid i ^ "> = " ^ sbe bsym_table e)
  )
  m;
  *)
  let result = ref [] in
  result :=  bexe_comment (sr,"parallel assignment") :: !result;
  iter
  (fun (i,(name,ty,e,_)) ->
    if i <> 0 then begin (* unit assign if i = 0 *)
      if mem i !tmplist then
        result := bexe_begin () :: !result;
      result := bexe_init (sr,i,e) :: !result;
    end
  )
  m;
  while length !tmplist > 0 do
    result := bexe_end () :: !result;
    tmplist := tl !tmplist
  done;
  !parameters, !result

