open Flx_util
open Flx_list
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
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_beta
open Flx_prop

let cal_parent syms bsym_table i' ts' =
  let bsym = Flx_bsym_table.find bsym_table i' in
  let bsym_parent = Flx_bsym_table.find_parent bsym_table i' in
  match bsym_parent with
  | None -> None
  | Some i ->
    let vsc = Flx_bsym.get_bvs bsym in
    assert (length vsc = length ts');
    if not (Flx_bsym_table.mem bsym_table i) then
    (
      (*
      print_endline ("WHA?? Parent " ^ si i ^ " of " ^ si i' ^ " does not exist??");
      *)
      None
    )
    else
    let vsp = Flx_bsym_table.find_bvs bsym_table i in
    let n = length vsp in
    assert (n <= length vsc);
    let ts = list_prefix ts' n in
    let k =
      (* Try to find the instance, but if it's not there, just use the
       * parent. *)
      try (Hashtbl.find syms.instances (i,ts)) with Not_found -> i
    in
      if ts = [] then assert (i=k);
      (*
      print_endline ("Parent of " ^ si i' ^ " was " ^ si i ^ " is now " ^ si k);
      *)
      Some k

let fixup_type' syms bsym_table fi t =
  match t with
  | BTYP_inst (i,ts) ->
    let i,ts = fi i ts in
    btyp_inst (i,ts)
  | x -> x

let rec fixup_type syms bsym_table fi t =
  let ft t = fixup_type syms bsym_table fi t in
  let ft' t = fixup_type' syms bsym_table fi t in
  let t = Flx_btype.map ~ft t in
  ft' t

let fixup_expr' syms bsym_table fi mt (e,t) =
  (*
  print_endline ("FIXUP EXPR(up) " ^ sbe sym_table (e, btyp_void));
  *)
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) ->
    let i,ts = fi i' ts in
    if i = i' then
      bexpr_apply_prim t (i,ts,a)
    else
      bexpr_apply_direct t (i,ts,a)

  | BEXPR_apply_direct (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_direct t (i,ts,a)

  | BEXPR_apply_struct (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_struct t (i,ts,a)

  | BEXPR_apply_stack (i,ts,a) ->
    let i,ts = fi i ts in
    bexpr_apply_stack t (i,ts,a)

  | BEXPR_ref (i,ts)  ->
    let i,ts = fi i ts in
    bexpr_ref t (i,ts)

  | BEXPR_name (i',ts') ->
    let i,ts = fi i' ts' in
    (*
    print_endline (
      "Ref to Variable " ^ si i' ^ "[" ^ catmap "," (sbt bsym_table) ts' ^"]" ^
      " mapped to " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^"]"
    );
    *)
    bexpr_name t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = fi i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  (*
  print_endline ("FIXed UP EXPR " ^ sbe sym_table (x, btyp_void));
  *)
  x

let rec fixup_expr syms bsym_table fi mt e =
  (*
  print_endline ("FIXUP EXPR(down) " ^ sbe sym_table e);
  *)
  let fe e = fixup_expr syms bsym_table fi mt e in
  let fe' e = fixup_expr' syms bsym_table fi mt e in
  let e = Flx_bexpr.map ~ft:mt ~fe e in
  fe' e

let fixup_exe syms bsym_table fi mt exe =
  (*
  print_endline ("FIXUP EXE[In] =" ^ string_of_bexe sym_table 0 exe);
  *)
  let fe e = fixup_expr syms bsym_table fi mt e in
  let result =
  match Flx_bexe.map ~ft:mt ~fe exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_call_direct (sr,i,ts,a)
    *)

  | BEXE_jump_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_jump_direct (sr,i,ts,a)
    *)

  | BEXE_call_prim (sr, i',ts,a) -> assert false
    (*
    let i,ts = fi i' ts in
    if i = i' then
      bexe_call_prim (sr,i,ts,a)
    else
      bexe_call_direct (sr,i,ts,a)
    *)

  | BEXE_call_stack (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    bexe_call_stack (sr,i,ts,a)
    *)

  (* this is deviant case: implied ts is vs of parent! *)
  | BEXE_init (sr,i,e) ->
    (*
    print_endline ("[init] Deviant case variable " ^ si i);
    *)
    let vs = Flx_bsym_table.find_bvs bsym_table i in
    let ts = map (fun (s,j) -> mt (btyp_type_var (j, btyp_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[init] Remapped deviant variable to " ^ si i);
    *)
    bexe_init (sr,i,e)

  | BEXE_svc (sr,i) ->
    (*
    print_endline ("[svc] Deviant case variable " ^ si i);
    *)
    let vs = Flx_bsym_table.find_bvs bsym_table i in
    let ts = map (fun (s,j) -> mt (btyp_type_var (j, btyp_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[svc] Remapped deviant variable to " ^ si i);
    *)
    bexe_svc (sr,i)

  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result


let fixup_exes syms bsym_table fi mt exes =
  map (fixup_exe syms bsym_table fi mt) exes

let mono syms bsym_table fi ts bsym =
  let mt vars t =
    beta_reduce
      syms
      bsym_table
      (Flx_bsym.sr bsym)
      (fixup_type syms bsym_table fi (list_subst syms.counter vars t))
  in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let ret = mt vars ret in
    (*
    let fi i ts = fi i (map mt ts) in
    *)
    let ps = map (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
      {pkind=pk;pid=s;pindex=fst (fi i ts);ptyp=mt vars t}) ps
    in
    let traint =
      match traint with
      | None -> None
      | Some x -> Some (fixup_expr syms bsym_table fi (mt vars) x)
    in
    let exes = fixup_exes syms bsym_table fi (mt vars) exes in
    Some (bbdcl_function (props,[],(ps,traint),ret,exes))

  | BBDCL_procedure (props,vs,(ps,traint), exes) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let ps = map (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
      let k = fst (fi i ts) in
      let u = mt vars t in
      (*
      print_endline ("Remap parameter " ^ s ^"<"^ si i ^ "> (type " ^
        sbt bsym_table t ^
      ")to " ^ si k ^ " type " ^ sbt bsym_table u);
      *)
      {pkind=pk;pid=s;pindex=k;ptyp=u}) ps
    in
    let traint =
      match traint with
      | None -> None
      | Some x -> Some (fixup_expr syms bsym_table fi (mt vars) x)
    in
    (*
    let fi i ts = fi i (map mt ts) in
    *)
    let exes = fixup_exes syms bsym_table fi (mt vars) exes in
    Some (bbdcl_procedure (props,[],(ps,traint), exes))

  | BBDCL_val (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_val ([],t))

  | BBDCL_var (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_var ([],t))

  | BBDCL_ref (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_ref ([],t))

  | BBDCL_tmp (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_tmp ([],t))

  (* we have tp replace types in interfaces like Vector[int]
    with monomorphic versions if any .. even if we don't
    monomorphise the bbdcl itself.

    This is weak .. it's redone for each instance, relies
    on mt being idempotent..
  *)
  | BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let argtypes = map (mt vars) argtypes in
    let ret = mt vars ret in
    Some (bbdcl_fun (props,vs,argtypes,ret,ct,reqs,prec))

  | BBDCL_proc (props,vs,argtypes,ct,reqs) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let argtypes = map (mt vars) argtypes in
    Some (bbdcl_proc (props,vs,argtypes,ct,reqs))

  | BBDCL_const (props, vs, t, CS_str "#this", reqs) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_const (props, [], t, CS_str "#this", reqs))

  | _ -> None

let chk_mono syms bsym_table i =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_module -> false
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->  true
  | BBDCL_procedure (props,vs,(ps,traint), exes) -> true
  | BBDCL_val (vs,t) -> true
  | BBDCL_var (vs,t) -> true
  | BBDCL_ref (vs,t) -> true
  | BBDCL_tmp (vs,t) -> true
  | BBDCL_const (_,_,_,CS_str "#this",_) -> true
  | BBDCL_union (vs,ps) -> false
  | BBDCL_cstruct (vs,ps) -> false
  | BBDCL_struct (vs,ps) -> false
  | BBDCL_newtype (vs,t) -> false
  | BBDCL_const (props,vs,t,ct,reqs) -> false
  | BBDCL_insert (vs,s,ikind,reqs) ->  false
  | BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) -> false
  | BBDCL_callback (props,vs,argtypes_cf,argtypes_c,k,ret,reqs,prec) -> false
  | BBDCL_proc (props,vs,argtypes,ct,reqs) -> false
  | BBDCL_abs (vs,tqual,ct,reqs) ->  false
  | BBDCL_nonconst_ctor (vs,uidx,udt, ctor_idx, ctor_argt, evs, etraint) -> false
  | BBDCL_typeclass (props,vs) ->  false
  | BBDCL_instance (props,vs,con,tc,ts) ->  false
  | BBDCL_axiom -> false
  | BBDCL_lemma -> false
  | BBDCL_reduce -> false

(* monomorphic instances are already equal to their indices ..
  replace some polymorphic instances with monomorphic ones
*)
let monomorphise syms bsym_table =
  let polyinst = Hashtbl.create 97 in
  Hashtbl.iter
  (fun (i,ts) n ->
   if ts = [] then assert (i = n )
   else
     if chk_mono syms bsym_table i
     then begin
       (*
       print_endline ("polyinst " ^ si n ^ " = " ^
       si i ^ "["^catmap "," (sbt bsym_table) ts^"]");
       *)
       Hashtbl.add polyinst (i,ts) n
     end else begin
       (*
       print_endline ("*** NO polyinst " ^ si n ^ " = " ^
       si i ^ "["^catmap "," (sbt bsym_table) ts^"]");
       *)
     end

  )
  syms.instances
  ;

  let fi polyinst i ts =
    let i,ts = Flx_typeclass.maybe_fixup_typeclass_instance syms bsym_table i ts in
    try Hashtbl.find polyinst (i,ts),[]
    with Not_found ->  i,ts
  in

  (* make a new table where the ts are ALSO converted to monomorphised
     class clones .. we still need the originals for non-type uses
     of the class (eg constructor)
  *)
  let polyinst2 = Hashtbl.create 97 in
  Hashtbl.iter
  (fun (i,ts) n ->
    Hashtbl.replace polyinst2 (i,ts) n;
    let ts = map (fixup_type syms bsym_table (fi polyinst)) ts in
    Hashtbl.replace polyinst2 (i,ts) n;
  )
  polyinst
  ;
  let fi i ts = fi polyinst2 i ts in

  Hashtbl.iter begin fun (i,ts) n ->
    if syms.compiler_options.print_flag then begin
      if (n <> i) then print_endline (
         "[monomorphise] Adding instance " ^ string_of_bid n ^ " = " ^
         string_of_bid i ^ "["^catmap "," (sbt bsym_table) ts^"]"
      ) else print_endline (
         "[monomorphise] Process instance " ^ string_of_bid n ^ " = " ^
         string_of_bid i ^ "["^catmap "," (sbt bsym_table) ts^"]"
      );
    end;

    (* Find the symbol we're going to be monomorphosize. *)
    let bsym = Flx_bsym_table.find bsym_table i in

    match mono syms bsym_table fi ts bsym with
    | None -> ()
    | Some bbdcl ->
        (* Look up the new parent. *)
        let parent = cal_parent syms bsym_table i ts in

        (* We remove the symbol first as it may have a new parent. *)
        Flx_bsym_table.remove bsym_table n;

        (* And re-insert it. *)
        let bsym = Flx_bsym.replace_bbdcl bsym bbdcl in
        Flx_bsym_table.add bsym_table parent n bsym

  end syms.instances;

  Hashtbl.iter (fun (i,ts) n ->
    Hashtbl.remove syms.instances (i,ts);
    Hashtbl.add syms.instances (n,[]) n;
  )
  polyinst
  ;
