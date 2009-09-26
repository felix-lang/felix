open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open List
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child

module BidSet = IntSet

let mk_remap counter d =
  let m = Hashtbl.create 97 in
  IntSet.iter
  (fun i ->
    let n = !counter in
    incr counter;
    Hashtbl.add m i n
  )
  d
  ;
  m

(* replace callee type variables with callers *)
let vsplice caller_vars callee_vs_len ts =
  if not (callee_vs_len <= length ts)
  then failwith
  (
    "Callee_vs_len = " ^
    si callee_vs_len ^
    ", len vs/ts= " ^
    si (length ts) ^
    ", length caller_vars = " ^
    si (length caller_vars) 
  )
  ;
  let rec aux lst n =  (* elide first n elements *)
    if n = 0 then lst
    else aux (tl lst) (n-1)
  in
  caller_vars @ aux ts callee_vs_len


(* varmap is the *typevariable* remapper,
 revariable remaps indices
*)
let ident x = x

let remap_expr syms bsym_table varmap revariable caller_vars callee_vs_len e =
  (*
  print_endline ("Remapping expression " ^ sbe syms.sym_table bsym_table e);
  *)
  let ftc i ts = Flx_typeclass.maybe_fixup_typeclass_instance syms bsym_table i ts in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let tmap t = match t with
  | BTYP_inst (i,ts) -> BTYP_inst (revar i,ts)
  | x -> x
  in
  let auxt t =
    let t' = varmap_subst varmap t in
    let rec s t = tmap (map_btype s t) in
    let t' = s t' in
    (* print_endline ("Remap type " ^ sbt syms.sym_table t ^ " to " ^ sbt syms.sym_table * t'); *)
    t'
  in
  let fixup i ts =
    let ts = map auxt ts in
    try
      let j= Hashtbl.find revariable i in
      j, vsplice caller_vars callee_vs_len ts
    with Not_found -> i,ts
  in
  let rec aux e = match map_tbexpr ident aux auxt e with
  | BEXPR_name (i,ts),t ->
    let i,ts = fixup i ts in
    BEXPR_name (i,ts), auxt t

  | BEXPR_ref (i,ts) as x,t ->
    let i,ts = fixup i ts in
    BEXPR_ref (i,ts), auxt t

  | BEXPR_closure (i,ts),t ->
    let i,ts = fixup i ts in
    BEXPR_closure (i,ts), auxt t

  | BEXPR_apply_direct (i,ts,e),t ->
    let i,ts = fixup i ts in

    (* attempt to fixup typeclass virtual *)
    let i,ts = ftc i ts in
    BEXPR_apply_direct (i,ts,aux e), auxt t

  | BEXPR_apply_stack (i,ts,e),t ->
    let i,ts = fixup i ts in
    BEXPR_apply_stack (i,ts,aux e), auxt t

  | BEXPR_apply_prim (i,ts,e),t ->
    let i,ts = fixup i ts in
    BEXPR_apply_prim (i,ts,aux e), auxt t

  | x,t -> x, auxt t
  in
    let a = aux e in
    (*
    print_endline ("replace " ^ sbe syms.sym_table e ^ "-->" ^ sbe syms.sym_table a);
    *)
    a

let remap_exe syms bsym_table relabel varmap revariable caller_vars callee_vs_len exe =
  (*
  print_endline ("remap_exe " ^ string_of_bexe syms.sym_table bsym_table 0 exe);
  *)
  let ge e = remap_expr syms bsym_table varmap revariable caller_vars callee_vs_len e in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let relab s = try Hashtbl.find relabel s with Not_found -> s in
  let ftc i ts = Flx_typeclass.maybe_fixup_typeclass_instance syms bsym_table i ts in

  let tmap t = match t with
  | BTYP_inst (i,ts) -> BTYP_inst (revar i,ts)
  | x -> x
  in
  let auxt t =
    let t' = varmap_subst varmap t in
    let rec s t = tmap (map_btype s t) in
    let t' = s t' in
    (* print_endline ("Remap type " ^ sbt syms.sym_table t ^ " to " ^ sbt syms.sym_table * t'); *)
    t'
  in
  let exe =
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2)  ->  assert false
    (*
    let fixup i ts =
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    BEXE_call_prim (sr,i,ts, ge e2)
    *)

  | BEXE_call_direct (sr,i,ts,e2)  ->  assert false
    (*
    let fixup i ts =
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in

    (* attempt to instantiate typeclass virtual *)
    let i,ts = ftc i ts in
    BEXE_call_direct (sr,i,ts, ge e2)
    *)

  | BEXE_call_stack (sr,i,ts,e2)  ->  assert false
    (*
    let fixup i ts =
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    BEXE_call_stack (sr,i,ts, ge e2)
    *)

  | x -> map_bexe revar ge ident relab relab x
  in
  (*
  print_endline ("remapped_exe " ^ string_of_bexe syms.sym_table bsym_table 0 exe);
  *)
  exe


let remap_exes syms bsym_table relabel varmap revariable caller_vars callee_vs_len exes =
  map (remap_exe syms bsym_table relabel varmap revariable caller_vars callee_vs_len) exes

let remap_reqs syms bsym_table varmap revariable caller_vars callee_vs_len reqs : breqs_t =
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let tmap t = match t with
  | BTYP_inst (i,ts) -> BTYP_inst (revar i,ts)
  | x -> x
  in
  let auxt t =
    let t' = varmap_subst varmap t in
    let rec s t = tmap (map_btype s t) in
    let t' = s t' in
    (* print_endline ("Remap type " ^ sbt syms.sym_table t ^ " to " ^ sbt syms.sym_table * t'); *)
    t'
  in
  let fixup (i, ts) =
    let ts = map auxt ts in
    try
      let j= Hashtbl.find revariable i in
      j, vsplice caller_vars callee_vs_len ts
    with Not_found -> i,ts
  in
  map fixup reqs


(* this routine makes a (type) specialised version of a symbol:
   a function, procedure, variable, or whatever.

   relabel: maps old labels onto fresh labels
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

let reparent1 (syms:sym_state_t) (uses,child_map,bsym_table )
  relabel varmap revariable
  caller_vs callee_vs_len index parent k rescan_flag
=
  let splice vs = (* replace callee type variables with callers *)
    vsplice caller_vs callee_vs_len vs
  in
  let sop = function
    | None -> "NONE?"
    | Some i -> si i
  in
  let caller_vars = map (fun (s,i) -> BTYP_var (i,BTYP_type 0)) caller_vs in

  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let tmap t = match t with
  | BTYP_inst (i,ts) -> BTYP_inst (revar i,ts)
  | x -> x
  in
  let auxt t =
    let t' = varmap_subst varmap t in
    let rec s t = tmap (map_btype s t) in
    let t' = s t' in
    (* print_endline ("Remap type " ^ sbt syms.sym_table t ^ " to " ^ sbt syms.sym_table * t'); *)
    t'
  in
  let remap_ps ps = map (fun {pid=id; pindex=i; ptyp=t; pkind=k} ->
    {pid=id; pindex=revar i; ptyp=auxt t; pkind=k})
     ps
   in

  let rexes xs = remap_exes syms bsym_table relabel varmap revariable caller_vars callee_vs_len xs in
  let rexpr e = remap_expr syms bsym_table varmap revariable caller_vars callee_vs_len e in
  let rreqs rqs = remap_reqs syms bsym_table varmap revariable caller_vars callee_vs_len rqs in
  let id,old_parent,sr,entry = Hashtbl.find bsym_table index in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "COPYING " ^ id ^ " index " ^ si index ^ " with old parent " ^
    sop old_parent ^ " to index " ^ si k ^ " with new parent " ^
    sop parent
  );
  begin match parent with
  | Some p ->
    let old_kids = try Hashtbl.find child_map p with Not_found -> [] in
    (*
    print_endline ("ADDING " ^ si k ^ " as child of " ^ si p);
    *)
    Hashtbl.replace child_map p (k::old_kids)
  | None -> ()
  end
  ;
  let id2 = id ^ "_clone_" ^ si index in
  match entry with
  | BBDCL_procedure (props,vs,(ps,traint),exes) ->
    let exes = rexes exes in
    let ps = remap_ps ps in
    let props = allow_rescan rescan_flag props in
    let props = filter (fun p -> p <> `Virtual) props in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_procedure (props,splice vs,(ps,traint),exes));
    (*
    print_endline "NEW PROCEDURE (clone):";
    print_function syms.sym_table bsym_table k;
    *)
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    (*
    print_endline ("Cal new usage of proc " ^ si k ^ ": " ^
      catmap "," (fun (j,_) -> si j) calls);
    *)
    Hashtbl.add uses k calls

  | BBDCL_function (props, vs, (ps,traint), ret, exes) ->
    let props = allow_rescan rescan_flag props in
    let props = filter (fun p -> p <> `Virtual) props in
    let ps = remap_ps ps in
    let exes = rexes exes in
    let ret = auxt ret in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_function (props,splice vs,(ps,traint),ret,exes));
    (*
    print_endline "NEW FUNCTION (clone):";
    print_function syms.sym_table bsym_table k;
    *)
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    (*
    print_endline ("Cal new usage of fun " ^ si k ^ ": " ^
      catmap "," (fun (j,_) -> si j) calls);
    *)
    Hashtbl.add uses k calls

  | BBDCL_var (vs,t) ->
    (*
    print_endline ("Reparent variable old: id<"^si index^"> vs=" ^
      catmap "," (fun (s,i) -> s^"<"^si i^">") vs);
    print_endline ("         variable new: id<"^si k^"> vs=" ^
      catmap "," (fun (s,i) -> s^"<"^si i^">") (splice vs));
     print_endline ("Type old " ^ sbt syms.sym_table t ^ " -> type new " ^ sbt
     syms.sym_table (auxt t));
    *)
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_var (splice vs,auxt t))

  | BBDCL_val (vs,t) ->
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_val (splice vs,auxt t))

  | BBDCL_ref (vs,t) ->
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_ref (splice vs,auxt t))

  | BBDCL_tmp (vs,t) ->
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_tmp (splice vs,auxt t))

  | BBDCL_abs (vs,quals,ct,breqs) ->
    let vs = splice vs in
    let breqs = rreqs breqs in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_abs (vs,quals,ct,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_const (props,vs,t,ct,breqs) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let vs = splice vs in
    let breqs = rreqs breqs in
    let t = auxt t in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_const (props,vs,t,ct,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  | BBDCL_proc (props,vs,params,ct,breqs) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let params = map auxt params in
    let vs = splice vs in
    let breqs = rreqs breqs in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_proc (props,vs,params,ct,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    (*
    print_endline ("Cal new usage of proc " ^ si k ^ ": " ^
      catmap "," (fun (j,_) -> si j) calls);
    *)
    Hashtbl.add uses k calls

  | BBDCL_fun (props,vs,params,ret,ct,breqs,prec) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let params = map auxt params in
    let vs = splice vs in
    let ret = auxt ret in
    let breqs = rreqs breqs in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_fun (props,vs,params,ret,ct,breqs,prec));
    (*
    print_endline "NEW FUNCTION (clone):";
    print_function syms.sym_table bsym_table k;
    *)
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    (*
    print_endline ("Cal new usage of fun " ^ si k ^ ": " ^
      catmap "," (fun (j,_) -> si j) calls);
    *)
    Hashtbl.add uses k calls

  | BBDCL_insert (vs,ct,ik,breqs) ->
    let breqs = rreqs breqs in
    let vs = splice vs in
    Hashtbl.add bsym_table k (id,parent,sr,BBDCL_insert (vs,ct,ik,breqs));
    let calls = try Hashtbl.find uses index with Not_found -> [] in
    let calls = map (fun (j,sr) -> revar j,sr) calls in
    Hashtbl.add uses k calls

  (*
  |  _ ->
    Hashtbl.add bsym_table k (id,parent,sr,entry)
  *)

  | _ -> syserr sr ("[reparent1] Unexpected: bbdcl " ^ string_of_bbdcl syms.sym_table bsym_table entry index)

(* make a copy all the descendants of i, changing any
  parent which is i to the given new parent
*)

(* this routine reparents all the children of a given
   routine, but it doesn't reparent the routine itself
*)

let reparent_children syms (uses,child_map,bsym_table)
  caller_vs callee_vs_len index parent relabel varmap rescan_flag extras
=
  let pp p = match p with None -> "NONE" | Some i -> si i in
  (*
  print_endline
  (
    "Renesting children of callee " ^ si index ^
    " to caller " ^ pp parent ^
     "\n  -- Caller vs len = " ^ si (length caller_vs) ^
     "\n  -- Callee vs len = " ^ si (callee_vs_len)
  );
  *)
  let closure = descendants child_map index in
  assert (not (IntSet.mem index closure));
  let revariable = fold_left (fun acc i -> IntSet.add i acc) closure extras in
  (*
  let cl = ref [] in IntSet.iter (fun i -> cl := i :: !cl) closure;
  print_endline ("Closure is " ^ catmap " " si !cl);
  *)
  let revariable = mk_remap syms.counter revariable in
  IntSet.iter
  (fun i ->
    let old_parent =
      match Hashtbl.find bsym_table i with id,oldp,_,_ -> oldp
    in
    let new_parent: bid_t option =
      match old_parent with
      | None -> assert false
      | Some p ->
        if p = index then parent
        else Some (Hashtbl.find revariable p)
    in
    let k = Hashtbl.find revariable i in
    reparent1 syms (uses,child_map,bsym_table) relabel varmap revariable
    caller_vs callee_vs_len i new_parent k rescan_flag
  )
  closure
  ;
  if syms.compiler_options.print_flag then begin
    Hashtbl.iter
    (fun i j ->
      print_endline ("//Reparent " ^ si j ^ " <-- " ^ si i)
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


let specialise_symbol syms (uses,child_map,bsym_table)
  caller_vs callee_vs_len index ts parent relabel varmap rescan_flag
=
  try Hashtbl.find syms.transient_specialisation_cache (index,ts)
  with Not_found ->
    let k = !(syms.counter) in incr (syms.counter);
    let revariable =
       reparent_children syms (uses,child_map,bsym_table)
       caller_vs callee_vs_len index (Some k) relabel varmap rescan_flag []
    in
    reparent1 (syms:sym_state_t) (uses,child_map,bsym_table )
      relabel varmap revariable
      caller_vs callee_vs_len index parent k rescan_flag
    ;
    let caller_vars = map (fun (s,i) -> BTYP_var (i,BTYP_type 0)) caller_vs in
    let ts' = vsplice caller_vars callee_vs_len ts in
    Hashtbl.add syms.transient_specialisation_cache (index,ts) (k,ts');
    k,ts'
