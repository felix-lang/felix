open Flx_util
open Flx_list
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
open Flx_reparent
open Flx_spexes
open Flx_beta
open Flx_prop

let cal_parent syms bbdfns i' ts' =
  let id,parent,sr,_ = Hashtbl.find bbdfns i' in
  match parent with
  | None -> None
  | Some i ->
    let vsc = get_vs bbdfns i' in
    assert (length vsc = length ts');
    if not (Hashtbl.mem bbdfns i) then
    (
      (*
      print_endline ("WHA?? Parent " ^ si i ^ " of " ^ si i' ^ " does not exist??");
      *)
      None
    )
    else
    let vsp = get_vs bbdfns i in
    let n = length vsp in
    assert (n <= length vsc);
    let ts = list_prefix ts' n in
    let k =
       try (Hashtbl.find syms.instances (i,ts))
       with Not_found ->
        print_endline ("Wah? Not found parent of " ^
          id ^ "<" ^ si i' ^ ">" ^
          "[" ^ catmap "," (sbt syms.dfns) ts ^ "]\n" ^
          "Which should be " ^ si i ^
          "[" ^ catmap "," (sbt syms.dfns) ts ^ "]"
        )
        ;
        assert false
    in
      if ts = [] then assert (i=k);
      (*
      print_endline ("Parent of " ^ si i' ^ " was " ^ si i ^ " is now " ^ si k);
      *)
      Some k

let fixup_type' syms bbdfns fi t =
  match t with
  | `BTYP_inst (i,ts) ->
    let i,ts = fi i ts in
    `BTYP_inst (i,ts)
  | x -> x

let rec fixup_type syms bbdfns fi t =
  let ft t = fixup_type syms bbdfns fi t in
  let ft' t = fixup_type' syms bbdfns fi t in
  let t = map_btype ft t in
  ft' t

let fixup_expr' syms bbdfns fi mt (e:bexpr_t) =
  (*
  print_endline ("FIXUP EXPR(up) " ^ sbe syms.dfns (e,`BTYP_void));
  *)
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) ->
    let i,ts = fi i' ts in
    if i = i' then
      BEXPR_apply_prim (i,ts,a)
    else
      BEXPR_apply_direct (i,ts,a)

  | BEXPR_apply_direct (i,ts,a) ->
    let i,ts = fi i ts in
    BEXPR_apply_direct (i,ts,a)

  | BEXPR_apply_struct (i,ts,a) ->
    let i,ts = fi i ts in
    BEXPR_apply_struct (i,ts,a)

  | BEXPR_apply_stack (i,ts,a) ->
    let i,ts = fi i ts in
    BEXPR_apply_stack (i,ts,a)

  | BEXPR_ref (i,ts)  ->
    let i,ts = fi i ts in
    BEXPR_ref (i,ts)

  | BEXPR_name (i',ts') ->
    let i,ts = fi i' ts' in
    (*
    print_endline (
      "Ref to Variable " ^ si i' ^ "[" ^ catmap "," (sbt syms.dfns) ts' ^"]" ^
      " mapped to " ^ si i ^ "[" ^ catmap "," (sbt syms.dfns) ts ^"]"
    );
    *)
    BEXPR_name (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = fi i ts in
    BEXPR_closure (i,ts)

  | x -> x
  in
  (*
  print_endline ("FIXed UP EXPR " ^ sbe syms.dfns (x,`BTYP_void));
  *)
  x

let id x = x

let rec fixup_expr syms bbdfns fi mt e =
  (*
  print_endline ("FIXUP EXPR(down) " ^ sbe syms.dfns e);
  *)
  let fe e = fixup_expr syms bbdfns fi mt e in
  let fe' (e,t) = fixup_expr' syms bbdfns fi mt e,t in
  (* this is deviant case: implied ts is vs of parent!,
     it has to be done FIRST before the type is remapped
  *)
  let e = match e with
  | BEXPR_get_named (i,(e,t)),t' ->
    (*
    print_endline ("REMAPPING component variable " ^ si i);
    *)
    let vs = get_vs bbdfns i in
    (*
    print_endline ("vs = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") vs);
    *)
    begin match t with
    | `BTYP_inst (j,ts)
(*    | `BTYP_lvalue (`BTYP_inst (j,ts))  *)
    ->
      (*
      print_endline ("type=" ^ si j ^ ", ts = " ^ catmap "," (sbt syms.dfns) ts);
      *)
      let i,ts = fi i ts in
      (*
      print_endline ("Remapped to " ^ si i);
      *)
      BEXPR_get_named (i,(e,t)),t'
    | _ -> assert false
    end
  | x -> x
  in
  let e = map_tbexpr id fe mt e in
  fe' e

let fixup_exe syms bbdfns fi mt exe =
  (*
  print_endline ("FIXUP EXE[In] =" ^ string_of_bexe syms.dfns 0 exe);
  *)
  let fe e = fixup_expr syms bbdfns fi mt e in
  let result =
  match map_bexe id fe mt id id exe with
  | `BEXE_call_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    `BEXE_call_direct (sr,i,ts,a)
    *)

  | `BEXE_jump_direct (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    `BEXE_jump_direct (sr,i,ts,a)
    *)

  | `BEXE_call_prim (sr, i',ts,a) -> assert false
    (*
    let i,ts = fi i' ts in
    if i = i' then
      `BEXE_call_prim (sr,i,ts,a)
    else
      `BEXE_call_direct (sr,i,ts,a)
    *)

  | `BEXE_call_stack (sr, i,ts,a) -> assert false
    (*
    let i,ts = fi i ts in
    `BEXE_call_stack (sr,i,ts,a)
    *)

  (* this is deviant case: implied ts is vs of parent! *)
  | `BEXE_init (sr,i,e) ->
    (*
    print_endline ("[init] Deviant case variable " ^ si i);
    *)
    let vs = get_vs bbdfns i in
    let ts = map (fun (s,j) -> mt (`BTYP_var (j,`BTYP_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[init] Remapped deviant variable to " ^ si i);
    *)
    `BEXE_init (sr,i,e)

  | `BEXE_svc (sr,i) ->
    (*
    print_endline ("[svc] Deviant case variable " ^ si i);
    *)
    let vs = get_vs bbdfns i in
    let ts = map (fun (s,j) -> mt (`BTYP_var (j,`BTYP_type 0))) vs in
    let i,ts = fi i ts in
    (*
    print_endline ("[svc] Remapped deviant variable to " ^ si i);
    *)
    `BEXE_svc (sr,i)

  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe syms.dfns 0 result);
  *)
  result


let fixup_exes syms bbdfns fi mt exes =
  map (fixup_exe syms bbdfns fi mt) exes

let mono syms (bbdfns: fully_bound_symbol_table_t) fi i ts n =
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with

  | `BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let ret = mt ret in
    (*
    let fi i ts = fi i (map mt ts) in
    *)
    let ps = map (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
      {pkind=pk;pid=s;pindex=fst (fi i ts);ptyp=mt t}) ps
    in
    let traint = match traint with | None -> None | Some x -> Some (fixup_expr syms bbdfns fi mt x) in
    let exes = fixup_exes syms bbdfns fi mt exes in
    let entry = `BBDCL_function (props,[],(ps,traint),ret,exes) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | `BBDCL_procedure (props,vs,(ps,traint), exes) ->
    let props = filter (fun p -> p <> `Virtual) props in
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let ps = map (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
      let k = fst (fi i ts) in
      let u = mt t in
      (*
      print_endline ("Remap parameter " ^ s ^"<"^ si i ^ "> (type " ^
        sbt syms.dfns t ^
      ")to " ^ si k ^ " type " ^ sbt syms.dfns u);
      *)
      {pkind=pk;pid=s;pindex=k;ptyp=u}) ps
    in
    let traint = match traint with | None -> None | Some x -> Some (fixup_expr syms bbdfns fi mt x) in
    (*
    let fi i ts = fi i (map mt ts) in
    *)
    let exes = fixup_exes syms bbdfns fi mt exes in
    let entry = `BBDCL_procedure (props,[],(ps,traint), exes) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | `BBDCL_val (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let t = mt t in
    let entry = `BBDCL_val ([],t) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | `BBDCL_var (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let t = mt t in
    let entry = `BBDCL_var ([],t) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | `BBDCL_ref (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let t = mt t in
    let entry = `BBDCL_ref ([],t) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | `BBDCL_tmp (vs,t) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let t = mt t in
    let entry = `BBDCL_tmp ([],t) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  (* we have tp replace types in interfaces like Vector[int]
    with monomorphic versions if any .. even if we don't
    monomorphise the entry itself.

    This is weak .. it's redone for each instance, relies
    on mt being idempotent..
  *)
  | `BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let argtypes = map mt argtypes in
    let ret = mt ret in
    let entry = `BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) in
    Hashtbl.replace bbdfns i (id,parent, sr, entry)


  | `BBDCL_proc (props,vs,argtypes,ct,reqs) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let argtypes = map mt argtypes in
    let entry = `BBDCL_proc (props,vs,argtypes,ct,reqs) in
    Hashtbl.replace bbdfns i (id,parent, sr, entry)

  | `BBDCL_const (props,vs,t,`Str "#this",reqs) ->
    let vars = map2 (fun (s,i) t -> i,t) vs ts in
    let mt t = reduce_type (beta_reduce syms sr (fixup_type syms bbdfns fi (list_subst syms.counter vars t))) in
    let t = mt t in
    let entry = `BBDCL_const(props,[],t,`Str "#this",reqs) in
    let parent = cal_parent syms bbdfns i ts in
    Hashtbl.replace bbdfns n (id,parent,sr,entry)

  | _ -> ()

let chk_mono syms (bbdfns: fully_bound_symbol_table_t) i =
  let id,parent,sr,entry = Hashtbl.find bbdfns i in
  match entry with
  | `BBDCL_function (props,vs,(ps,traint),ret,exes) ->  true
  | `BBDCL_procedure (props,vs,(ps,traint), exes) -> true
  | `BBDCL_val (vs,t) -> true
  | `BBDCL_var (vs,t) -> true
  | `BBDCL_ref (vs,t) -> true
  | `BBDCL_tmp (vs,t) -> true
  | `BBDCL_const (_,_,_,`Str "#this",_) -> true
  | `BBDCL_union (vs,ps) -> false
  | `BBDCL_cstruct (vs,ps) -> false
  | `BBDCL_struct (vs,ps) -> false
  | `BBDCL_newtype (vs,t) -> false
  | `BBDCL_const (props,vs,t,ct,reqs) -> false
  | `BBDCL_insert (vs,s,ikind,reqs) ->  false
  | `BBDCL_fun (props,vs,argtypes,ret,ct,reqs,prec) -> false
  | `BBDCL_callback (props,vs,argtypes_cf,argtypes_c,k,ret,reqs,prec) -> false
  | `BBDCL_proc (props,vs,argtypes,ct,reqs) -> false
  | `BBDCL_abs (vs,tqual,ct,reqs) ->  false
  | `BBDCL_nonconst_ctor (vs,uidx,udt, ctor_idx, ctor_argt, evs, etraint) -> false
  | `BBDCL_typeclass (props,vs) ->  false
  | `BBDCL_instance (props,vs,con,tc,ts) ->  false

(* monomorphic instances are already equal to their indices ..
  replace some polymorphic instances with monomorphic ones
*)
let monomorphise syms bbdfns =
  let polyinst = Hashtbl.create 97 in
  Hashtbl.iter
  (fun (i,ts) n ->
   if ts = [] then assert (i = n )
   else
     if chk_mono syms bbdfns i
     then begin
       (*
       print_endline ("polyinst " ^ si n ^ " = " ^
       si i ^ "["^catmap "," (sbt syms.dfns) ts^"]");
       *)
       Hashtbl.add polyinst (i,ts) n
     end else begin
       (*
       print_endline ("*** NO polyinst " ^ si n ^ " = " ^
       si i ^ "["^catmap "," (sbt syms.dfns) ts^"]");
       *)
     end

  )
  syms.instances
  ;

  let fi polyinst i ts =
    let ts = map reduce_type ts in
    let i,ts = Flx_typeclass.maybe_fixup_typeclass_instance syms bbdfns i ts in
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
    let ts = map (fixup_type syms bbdfns (fi polyinst)) ts in
    Hashtbl.replace polyinst2 (i,ts) n;
  )
  polyinst
  ;
  let fi i ts = fi polyinst2 i ts in

  Hashtbl.iter
  (fun (i,ts) n ->
    if syms.compiler_options.print_flag then begin
      if (n <> i) then print_endline (
         "[monomorphise] Adding instance " ^ si n ^ " = " ^
         si i ^ "["^catmap "," (sbt syms.dfns) ts^"]"
      ) else print_endline (
         "[monomorphise] Process instance " ^ si n ^ " = " ^
         si i ^ "["^catmap "," (sbt syms.dfns) ts^"]"
      );
    end;


    mono syms bbdfns fi i ts n;
  )
  syms.instances
  ;

  Hashtbl.iter (fun (i,ts) n ->
    Hashtbl.remove syms.instances (i,ts);
    Hashtbl.add syms.instances (n,[]) n;
  )
  polyinst
  ;
