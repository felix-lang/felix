(* 
This module is responsible for reparenting children. When a function f is
inlined into g, f's children must become children of g, otherwise the
code body of f inlined into g will not work when it need to access a child.

To do this reparenting we clone the children of f and assign them the new
parent g. However this is not enough. We must also change the names
of all variables and labels (alpha conversion) so they don't clash
with those of g, which may also happen to include more than one
inlined copy of f. In particular, the child itself must be
renamed.. and of course references to it in the inlined body
of f have to be adjusted as well.

That too is not enough. When f is inlined, its parameters are
eliminated, either by substitution (lazy evaluation, pass-by-name),
or by creating variable and assigning the argument to it. Either
way, the child has to undergo the same process of eliminating
references to f's parameters.

But that too is not enough. If f is polymorphic, its type variables
will have been replaced, which means specialising every type variable
in the child as well.

The process is expensive and difficult, at least in part because
every expression and subexpression in Felix carries its type with it.
It is not enough to simply beta-reduce (substitute) because doing
so may leave a non-normal form: we must normalise too.

Finally, because some things are duplicated, for example in caches,
we must either ensure the caches are, or will eventually, be updated.
*)
open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_bid

let mk_remap counter d =
  let m = Hashtbl.create 97 in
  BidSet.iter (fun i -> Hashtbl.add m i (fresh_bid counter)) d;
  m

let remap_expr
  syms
  bsym_table
  revariable    (** revariable remaps indices. *)
  e
=
(*
  print_endline ("Remapping expression " ^ sbe bsym_table e);
*)
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let fixup i ts =
    try
      let j= Hashtbl.find revariable i in
      j, ts
    with Not_found -> i,ts
  in
  let rec aux e =
    match e with
    | BEXPR_varname (i,ts),t ->
        let i,ts = fixup i ts in
        bexpr_varname (t) (i,ts)

    | BEXPR_ref (i,ts) as x,t ->
        let i,ts = fixup i ts in
        bexpr_ref (t) (i,ts)

    | BEXPR_closure (i,ts),t ->
        let i,ts = fixup i ts in
        bexpr_closure (t) (i,ts)

    | BEXPR_apply_direct (i,ts,e),t ->
        let i,ts = fixup i ts in
        bexpr_apply_direct (t) (i,ts,aux e)

    | BEXPR_apply_stack (i,ts,e),t ->
        let i,ts = fixup i ts in
        bexpr_apply_stack (t) (i,ts,aux e)

    | BEXPR_apply_prim (i,ts,e),t ->
        let i,ts = fixup i ts in
        bexpr_apply_prim (t) (i,ts,aux e)

    | _ -> 
      Flx_bexpr.map ~f_bexpr:aux ~f_bid:revar e
  in
  let a = aux e in
(*
  print_endline ("replace " ^ sbe bsym_table e ^ "-->" ^ sbe bsym_table a);
*)
  a

let remap_exe
  syms
  bsym_table
  revariable    (** revariable remaps indices. *)
  exe
=
(*
  print_endline ("remap_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  let ge e = remap_expr syms bsym_table revariable e in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in

  let exe =
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2) -> 
    let fixup i ts =
      try
        let j= Hashtbl.find revariable i in
        j, ts
      with Not_found -> i,ts
    in
    let j,ts2 = fixup i ts in
    if not (i = j && ts = ts2) then begin
     print_endline ("Unexpected: Reparent call to primitive adjusts: " ^ 
       string_of_int i ^ "[" ^ catmap "," (Flx_print.sbt bsym_table) ts ^ "] to " ^ 
       string_of_int j ^ "[" ^ catmap "," (Flx_print.sbt bsym_table) ts2);
    end;
    bexe_call_prim (sr,j,ts2, ge e2)

  | BEXE_call_direct (sr,i,ts,e2) -> 
    let fixup i ts =
      try
        let j= Hashtbl.find revariable i in
        j, ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in

    (* attempt to instantiate typeclass virtual *)
    bexe_call_direct (sr,i,ts, ge e2)

  | BEXE_call_stack (sr,i,ts,e2) -> assert false

  | BEXE_label (sr,index) -> bexe_label (sr,revar index)
  | BEXE_goto (sr,index) -> bexe_goto (sr,revar index)
  | BEXE_ifgoto (sr,e,index) -> bexe_ifgoto (sr,ge e,revar index)

  | x -> Flx_bexe.map ~f_bid:revar ~f_bexpr:ge  x
  in
(*
  print_endline ("remapped_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  exe


let remap_exes syms bsym_table revariable exes =
  map (remap_exe syms bsym_table revariable) exes

let remap_reqs syms bsym_table revariable reqs : breqs_t =
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let fixup (i, ts) =
    try
      let j= Hashtbl.find revariable i in
      j, ts
    with Not_found -> i,ts
  in
  map fixup reqs


(* this routine makes a (type) specialised version of a symbol:
   a function, procedure, variable, or whatever.

   revariable: maps old variables and functions to fresh ones
   varmap: maps type variables to types (type specialisation)
   index: this routine
   parent: the new parent

   this routine doesn't specialise any children,
   just any reference to them: the kids need
   to be specialised by reparent_children.
*)

let allow_rescan flag props =
  match flag with
  | false -> props
  | true -> filter (function | `Inlining_complete | `Inlining_started -> false | _ -> true ) props

let reparent1
  (syms:sym_state_t)
  uses
  bsym_table
  revariable
  index         (** Routine index. *)
  parent        (** The parent symbol. *)
  k             (** New index, perhaps the caller. *)
  rescan_flag   (** Allow rescan of cloned stuff? *)
=
  let sop = function
    | None -> "NONE?"
    | Some i -> string_of_bid i
  in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let remap_ps ps = Flx_bparams.map ~f_bid:revar ps in
  let rexes xs = remap_exes syms bsym_table revariable xs in
  let rexpr e = remap_expr syms bsym_table revariable e in
  let rreqs rqs = remap_reqs syms bsym_table revariable rqs in
  let bsym = Flx_bsym_table.find bsym_table index in
  let bsym_parent = Flx_bsym_table.find_parent bsym_table index in
  if syms.compiler_options.Flx_options.print_flag then
  print_endline
  (
    "COPYING " ^ Flx_bsym.id bsym ^ " index " ^ string_of_bid index ^
    " with old parent " ^ sop bsym_parent ^ " to index " ^
    string_of_bid k ^ " with new parent " ^ sop parent
  );
  let update_bsym bbdcl =
    (* Flx_bsym_table.remove bsym_table k; *)
    Flx_bsym_table.add bsym_table k parent (Flx_bsym.replace_bbdcl bsym bbdcl)
  in

  match Flx_bsym.bbdcl bsym with
  | BBDCL_label s ->
    update_bsym (bbdcl_label s);
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls


  | BBDCL_fun (props, vs, ps, ret, effects, exes) ->
    let props = allow_rescan rescan_flag props in
    let props = filter (fun p -> p <> `Virtual) props in
    let ps = remap_ps ps in
    let exes = rexes exes in
    update_bsym (bbdcl_fun (props,vs,ps,ret,effects, exes));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_val (vs,t,kind) ->
    update_bsym (bbdcl_val (vs,t,kind))

  | BBDCL_external_type (vs,quals,ct,breqs) ->
    let breqs = rreqs breqs in
    update_bsym (bbdcl_external_type (vs,quals,ct,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_cstruct (vs,ps,breqs) ->
    let breqs = rreqs breqs in
    update_bsym (bbdcl_cstruct (vs,ps,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_external_const (props,vs,t,ct,breqs) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let breqs = rreqs breqs in
    update_bsym (bbdcl_external_const (props,vs,t,ct,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_external_fun (props,vs,params,ret,breqs,prec,kind) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let breqs = rreqs breqs in
    update_bsym (bbdcl_external_fun (props,vs,params,ret,breqs,prec,kind));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_external_code (vs,ct,ik,breqs) ->
    let breqs = rreqs breqs in
    update_bsym (bbdcl_external_code (vs,ct,ik,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  (*
  |  _ ->
    Flx_bsym_table.add bsym_table k (id,parent,sr,entry)
  *)

  | _ ->
    syserr (Flx_bsym.sr bsym) ("[reparent1] Unexpected: bbdcl " ^
      string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index)

(* make a copy all the descendants of i, changing any
  parent which is i to the given new parent
*)

(* this routine reparents all the children of a given
   routine, but it doesn't reparent the routine itself
*)

let reparent_children syms uses bsym_table
  index (parent:bid_t option) rescan_flag extras
=
  let extras = List.filter (fun i -> i <> 0) extras in
  let pp p = match p with None -> "NONE" | Some i -> string_of_bid i in
  if syms.compiler_options.Flx_options.print_flag then
  print_endline
  (
    "Renesting children of callee " ^ si index ^
    " to caller " ^ pp parent
  );
  let closure = Flx_bsym_table.find_descendants bsym_table index in
(*
  assert (not (BidSet.mem 0 closure));
  assert (not (List.mem 0 extras));
  assert (not (BidSet.mem index closure));
*)
  let torevariable = fold_left (fun acc i -> BidSet.add i acc) closure extras in
  (*
  let cl = ref [] in BidSet.iter (fun i -> cl := i :: !cl) closure;
  print_endline ("Closure is " ^ catmap " " si !cl);
  *)
  assert (not (BidSet.mem 0 torevariable));
  let revariable = mk_remap syms.counter torevariable in

  BidSet.iter begin fun i ->
    let old_parent = try Flx_bsym_table.find_parent bsym_table i with Not_found -> failwith ("Bug8, can't find parent of " ^ string_of_int i) in
    let new_parent: bid_t option =
      match old_parent with
      | None -> assert false
      | Some p ->
        if p = index then parent
        else Some (Hashtbl.find revariable p)
    in
    let k = Hashtbl.find revariable i in
    reparent1 syms uses bsym_table revariable
      i new_parent k rescan_flag
  end closure;
  if syms.compiler_options.Flx_options.print_flag then begin
    Hashtbl.iter
    (fun i j ->
      print_endline ("//Reparent " ^ string_of_bid j ^ " <-- " ^
        string_of_bid i)
    )
    revariable
  end
  ;
  revariable

(* NOTE! when we specialise a routine, calls to the same
  routine (polymorphically recursive) need not end up
  recursive. They're only recursive if they call the
  original routine with the same type specialisations
  as the one we're making here.

  In particular a call is recursive if, and only if,
  it is fully polymorphic (that is, just resupplies
  all the original type variables). In that case,
  recursion is preserved by specialisation.

  However recursion can also be *introduced* by specialisation
  where it didn't exist before!

  So remapping function indices has to be conditional.

  Note that calls to children HAVE to be remapped
  because of reparenting -- the original kids
  are no longer reachable! But this is no problem
  because the kid's inherited type variables are
  specialised away: you can't supply a kid with
  type variable instances distinct from the kid's
  parents variables (or the kid would refer to the
  stack from of a distinct function!)

  So the only problem is on self calls of the main
  routine, since they can call self either with
  the current specialisation or any other.
*)


let specialise_symbol syms uses bsym_table
  index parent rescan_flag
 =
  try Hashtbl.find syms.transient_specialisation_cache index
  with Not_found ->
    let k = fresh_bid syms.counter in

    (* First we must insert the symbol into the bsym_table before we can
     * continue. We'll update it again after we've processed the children. *)
    Flx_bsym_table.add bsym_table k parent
      (Flx_bsym_table.find bsym_table index);

    let revariable =
       reparent_children syms uses bsym_table
       index (Some k) rescan_flag []
    in

    (* Finally, reparent the symbol. *)
    reparent1 (syms:sym_state_t) uses bsym_table
      revariable
      index parent k rescan_flag;

    Hashtbl.add syms.transient_specialisation_cache index k;
    k


