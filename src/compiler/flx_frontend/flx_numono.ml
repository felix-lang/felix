(* New monomorphisation routine *)
open Flx_util
open Flx_btype
open Map
open Flx_mtypes2
open Flx_print
open Flx_types
open Flx_bbdcl
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
module CS = Flx_code_spec

(* NOTES
   We make monomorphic specialisations of everything used.
   For Felix entities, references lose their type arguments (ts) and the
   entity loses its type paramaters (vs).

   For C things, we monomorphise the visible code, and so get
   a copy of the C thing, however the C thing doesn't lose
   its type parameters (vs) and references don't lose the
   type arguments (ts) .. even though the copy is monomorphic.

   This is because this routine does not substitute type arguments
   into the C code fragments, so the C code part of the entity
   remains polymorphic, even though the Felix part of the interface
   is monomorphised.

*)


let show bsym_table i = 
  try 
    Flx_bsym_table.find_id bsym_table i ^ "<" ^ si i ^ ">"
   with Not_found -> "index_" ^ si i

let showts bsym_table i ts =
  show bsym_table i ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"

let showvars bsym_table vars = 
  catmap "," (fun (i,t)-> si i ^ " |-> " ^ sbt bsym_table t) vars

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING TYPE VARIABLES IN TYPES              *)
(* ----------------------------------------------------------- *)

let check_mono bsym_table sr t =
  if Flx_unify.var_occurs bsym_table t then begin
    print_endline (" **** Failed to monomorphise type " ^ sbt bsym_table t);
    print_endline (" **** got type " ^ sbt bsym_table t);
    assert false
  end
  ;
  match t with
  | BTYP_none 
  | BTYP_type_apply _
  | BTYP_type_function _
  | BTYP_type_tuple _
  | BTYP_type_match _
  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    -> 
    (* print_endline ("[flx_numono:check_mono]: Unexpected type expression" ^ sbt bsym_table t); *)
     ()
  | _ -> ()

  
let check_mono_vars bsym_table vars sr t =
  try check_mono bsym_table sr t
  with _ -> 
    print_endline (" **** using varmap " ^ showvars bsym_table vars);
    assert false

let mono_type syms bsym_table vars sr t = 
(*
print_endline (" ** begin mono_type " ^ sbt bsym_table t);
*)
  let t = Flx_unify.list_subst syms.counter vars t in
(*
print_endline (" ** mono_type after variable replacement " ^ sbt bsym_table t);
*)
  let t = Flx_beta.beta_reduce "mono_type"
    syms.Flx_mtypes2.counter
    bsym_table
    Flx_srcref.dummy_sr
    t
  in 
  let t = Flx_unify.normalise_tuple_cons bsym_table t in
  begin try check_mono bsym_table sr t with _ -> assert false end;
(*
print_endline (" ** end Mono_type " ^ sbt bsym_table t);
*)
  t

let rec mono_expr syms bsym_table vars sr e =
(*
print_endline (" ** begin mono_expr " ^ sbe bsym_table e);
print_endline (" ** begin mono_expr: type " ^ sbt bsym_table (snd e));
*)
  let f_btype t = mono_type syms bsym_table vars sr t in
  let f_bexpr e = mono_expr syms bsym_table vars sr e in
  let e = Flx_bexpr.map ~f_btype ~f_bexpr e in
(*
print_endline (" ** end mono_expr " ^ sbe bsym_table e);
print_endline (" ** end mono_expr: type " ^ sbt bsym_table (snd e));
*)
  e

let rec mono_exe syms bsym_table vars exe =
(*
print_endline (" ** begin mono_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  let sr = Flx_bexe.get_srcref exe in
  let f_btype t = mono_type syms bsym_table vars sr t in
  let f_bexpr e = mono_expr syms bsym_table vars sr e in
  let exe = Flx_bexe.map ~f_btype ~f_bexpr exe in
(*
print_endline (" ** end mono_exe " ^ string_of_bexe bsym_table 0 exe);
*)
  exe

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING REFS TO VIRTUALS WITH INSTANCES      *)
(* ----------------------------------------------------------- *)

let flat_typeclass_fixup_type syms bsym_table virtualinst sr t =
  match t with
  | BTYP_inst (i,ts) ->
    let i',ts' = virtualinst sr i ts in
    let t = btyp_inst (i',ts') in
    t
  | x -> x

let rec typeclass_fixup_type syms bsym_table virtualinst sr t =
  let f_btype t = typeclass_fixup_type syms bsym_table virtualinst sr t in
  let t = Flx_btype.map ~f_btype t in
  let t = flat_typeclass_fixup_type syms bsym_table virtualinst sr t in
  t

let flat_typeclass_fixup_expr syms bsym_table virtualinst sr (e,t) =
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) -> assert false
  | BEXPR_apply_direct (i,ts,a) -> assert false
  | BEXPR_apply_struct (i,ts,a) -> assert false
  | BEXPR_apply_stack (i,ts,a) -> assert false
  | BEXPR_ref (i,ts)  ->
    let i,ts = virtualinst sr i ts in
    bexpr_ref t (i,ts)

  | BEXPR_varname (i',ts') ->
    let i,ts = virtualinst sr i' ts' in
    bexpr_varname t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = virtualinst sr i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  x

let rec typeclass_fixup_expr syms bsym_table virtualinst sr e =
  let f_bexpr e = typeclass_fixup_expr  syms bsym_table virtualinst sr e in
  let e = flat_typeclass_fixup_expr syms bsym_table virtualinst sr e in
  let e = Flx_bexpr.map ~f_bexpr e in
  e 

(* mt is only used to fixup svc and init hacks *)
let flat_typeclass_fixup_exe syms bsym_table polyinst mt exe =
(*
  print_endline ("TYPECLASS FIXUP EXE[In] =" ^ string_of_bexe bsym_table 0 exe);
*)
  let result =
  match exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
  | BEXE_jump_direct (sr, i,ts,a) -> assert false
  | BEXE_call_prim (sr, i',ts,a) -> assert false
  | BEXE_call_stack (sr, i,ts,a) -> assert false

  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING REFS TO POLYMORPHS WITH MONOS        *)
(* ----------------------------------------------------------- *)

let flat_poly_fixup_type syms bsym_table polyinst sr t =
(*
  if not (complete_type t) then
    print_endline ("flat_poly_fixup_type: type isn't complete " ^ sbt bsym_table t);
*)

  match t with
  | BTYP_inst (i,ts) ->
    let i',ts' = polyinst sr i ts in
    let t' = btyp_inst (i',ts') in
(*
print_endline ("poly_fixup_type: " ^ showts bsym_table i ts ^ " --> " ^ showts bsym_table i' ts');
*)
    t'
  | x -> x

(* this has to be top down, so instances i,ts use the original
ts, then the ts get analysed. Otherwise, we'd get new symbols
in the ts and there's be no match on the replacement table
*)

let rec rec_poly_fixup_type trail syms bsym_table polyinst sr t =
  let level = Flx_list.list_index trail t in
  match level with
  | Some i -> btyp_fix (-i-1) (btyp_type 0)
  | None ->
  let t' = unfold "rec_poly_fixup_type" t in
  let t' = flat_poly_fixup_type syms bsym_table polyinst sr t' in
  let f_btype t' = rec_poly_fixup_type (t::trail) syms bsym_table polyinst sr t' in
  let t' = Flx_btype.map ~f_btype t' in
  t'

let poly_fixup_type syms bsym_table polyinst sr t =
 let t' = rec_poly_fixup_type [] syms bsym_table polyinst sr t in
(*
 print_endline ("POLY FIXUP TYPE: " ^ sbt bsym_table t ^ " --> " ^ sbt bsym_table t');
*)
 t'

let flat_poly_fixup_expr syms bsym_table polyinst sr (e,t) =
  let x = match e with
  | BEXPR_apply_prim (i',ts,a) -> assert false
  | BEXPR_apply_direct (i,ts,a) -> assert false
  | BEXPR_apply_struct (i,ts,a) -> assert false
  | BEXPR_apply_stack (i,ts,a) -> assert false
  | BEXPR_ref (i,ts)  ->
    let i,ts = polyinst sr i ts in
    bexpr_ref t (i,ts)

  | BEXPR_varname (i',ts') ->
    let i,ts = polyinst sr i' ts' in
    bexpr_varname t (i,ts)

  | BEXPR_closure (i,ts) ->
    let i,ts = polyinst sr i ts in
    bexpr_closure t (i,ts)

  | x -> x, t
  in
  x

let rec poly_fixup_expr syms bsym_table polyinst sr e =
  let f_bexpr e = poly_fixup_expr  syms bsym_table polyinst sr e in
  let f_btype t = poly_fixup_type syms bsym_table polyinst sr t in
  let e = flat_poly_fixup_expr syms bsym_table polyinst sr e in
  let e = Flx_bexpr.map ~f_bexpr ~f_btype e in
  e 

(* mt is only used to fixup svc and init hacks *)
let flat_poly_fixup_exe syms bsym_table polyinst parent_ts mt exe =
(*
  print_endline ("TYPECLASS FIXUP EXE[In] =" ^ string_of_bexe bsym_table 0 exe);
*)
  let result =
  match exe with
  | BEXE_call_direct (sr, i,ts,a) -> assert false
  | BEXE_jump_direct (sr, i,ts,a) -> assert false
  | BEXE_call_prim (sr, i',ts,a) -> assert false
  | BEXE_call_stack (sr, i,ts,a) -> assert false

  (* this is deviant case: implied ts is vs of parent! *)
  | BEXE_init (sr,i,e) ->
(*
    print_endline ("[flat_poly_fixup_exe: init] Deviant case variable " ^ si i);
*)
    let vs = 
      try Flx_bsym_table.find_bvs bsym_table i 
      with Not_found -> assert false
    in
    assert (List.length vs = List.length parent_ts);
    let j,ts = polyinst sr i parent_ts in
(*
    if i <> j then 
      print_endline ("[init] Remapped deviant variable to " ^ si j);
*)
    bexe_init (sr,j,e)

  | BEXE_svc (sr,i) ->
    (*
    print_endline ("[flat_poly_fixup_exe: svc] Deviant case variable " ^ si i);
    *)
    let vs = 
      try Flx_bsym_table.find_bvs bsym_table i 
      with Not_found -> assert false
    in
    assert (List.length vs = List.length parent_ts);
    let j,ts = polyinst sr i parent_ts in
    (*
      if i <> j then
        print_endline ("[svc] Remapped deviant variable to " ^ si j);
    *)
    bexe_svc (sr,j)

  | BEXE_label (sr,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_label (sr,j)

  | BEXE_goto  (sr,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_goto (sr,j)

  | BEXE_ifgoto (sr,e,i) ->
    let j,ts = polyinst sr i parent_ts in
    bexe_ifgoto (sr,e,j)



  | x -> x
  in
  (*
  print_endline ("FIXUP EXE[Out]=" ^ string_of_bexe sym_table 0 result);
  *)
  result

(* ----------------------------------------------------------- *)
(* COMPLETE PROCESSING ROUTINES                                *)
(* ----------------------------------------------------------- *)

let fixup_type syms bsym_table vars bsym virtualinst polyinst sr t =
(*
  print_endline ("    ** mono_type " ^ sbt bsym_table t);
*)
  let t = mono_type syms bsym_table vars sr t in
(*
  print_endline ("    ** typeclass_fixup_type " ^ sbt bsym_table t);
*)
  let t = typeclass_fixup_type syms bsym_table virtualinst sr  t in
(*
  print_endline ("    ** Betareduce " ^ sbt bsym_table t);
*)
  let t = Flx_beta.beta_reduce "flx_mono: mono, metatype"
    syms.Flx_mtypes2.counter
    bsym_table
    (Flx_bsym.sr bsym)
    t
  in 
(*
  print_endline ("    ** poly_fixup_type " ^ sbt bsym_table t);
*)
  let t = poly_fixup_type syms bsym_table polyinst sr t in
(*
  print_endline ("    ** Polyfixedup" ^ sbt bsym_table t );
*)
  t

let fixup_req syms bsym_table vars polyinst sr (i,ts) : Flx_types.bid_t * Flx_btype.t list =
  let ts = List.map (mono_type syms bsym_table vars sr) ts in
  let j,ts = polyinst sr i ts in
  let ts = List.map (poly_fixup_type syms bsym_table polyinst sr) ts in
  j,ts

let fixup_reqs syms bsym_table vars polyinst sr reqs : Flx_btype.breqs_t = 
  List.map (fixup_req syms bsym_table vars polyinst sr) reqs

(* HUH? Never called??? Oh, yes, used in constraints .. *) 
let fixup_expr syms bsym_table monotype virtualinst polyinst sr e =
  print_endline ("[fixup_expr] input               : " ^ sbe bsym_table e);
  (* monomorphise the code by eliminating type variables *)
  let e = Flx_bexpr.map ~f_btype:monotype e in
(*
  print_endline ("[fixup_expr] monomorphised       : " ^ sbe bsym_table e);
*)
  (* eliminate virtual calls by mapping to instances *)
  let e = typeclass_fixup_expr syms bsym_table virtualinst sr e in
(*
  print_endline ("[fixup_expr] virtuals eliminated : " ^ sbe bsym_table e);
*)
  (* replace applications of polymorphic function (or variable)
    with applications of new monomorphic ones
  *)
  let e = poly_fixup_expr syms bsym_table polyinst sr e in
(*
  print_endline ("[fixup_expr] polysyms eliminated : " ^ sbe bsym_table e);
*)
  e

let show_exe bsym_table exe = string_of_bexe bsym_table 4 exe
let show_exes bsym_table exes = catmap "\n" (show_exe bsym_table) exes


(* completely process a list of exes *)
(* rewrite to do in one pass *)
let fixup_exes syms bsym_table vars virtualinst polyinst parent_ts exes =
 let mt t = mono_type syms bsym_table vars t in

 (* monomorphise the code by eliminating type variables *)
(*
  print_endline ("To fixup exes:\n" ^ show_exes bsym_table exes);
*)
  let rexes = List.fold_left 
    (fun oexes iexe -> 
      match iexe with
      | BEXE_call (sr,(BEXPR_closure (f,[]),_),_) ->
        begin match Flx_bsym_table.find_bbdcl bsym_table f with
        (* elide calls to empty non-virtual procedures 
           Inlining does this anyhow, but this cleans up the
           diagnostic prints and reduces the crap in the symbol
           table a little earlier.
        *) 
        | BBDCL_fun (props,_,_,_,_,[]) when not (List.mem `Virtual props) -> oexes 
        | _ -> mono_exe syms bsym_table vars iexe :: oexes
        end 
      | _ ->  mono_exe syms bsym_table vars iexe :: oexes
    ) 
    [] 
    exes 
  in
(*
  print_endline ("Monomorphised:\n" ^ show_exes bsym_table (List.rev rexes));
  print_endline ("VARS=" ^ showvars bsym_table vars);
*)
  (* eliminate virtual calls by mapping to instances *)
  (* order doesn't matter here *)
  let exes = List.rev_map 
    (fun exe -> 
      let sr = Flx_bexe.get_srcref exe in 
      Flx_bexe.map ~f_bexpr:(typeclass_fixup_expr syms bsym_table virtualinst sr) 
      exe
    ) 
    rexes 
  in
  let rexes = List.rev_map (fun exe -> flat_typeclass_fixup_exe syms bsym_table virtualinst mt exe) exes in
(*
  print_endline ("Virtuals Instantiated:\n" ^ show_exes bsym_table (List.rev exes));
*)
  let exes = List.rev_map (flat_poly_fixup_exe syms bsym_table polyinst parent_ts mt)  rexes in
(*
  print_endline ("Special calls monomorphised:\n" ^ show_exes bsym_table exes);
*)
  (* replace applications of polymorphic function (or variable)
    with applications of new monomorphic ones
  *)
  let exes = List.map 
    (
      fun exe -> let sr = Flx_bexe.get_srcref exe in
      Flx_bexe.map ~f_bexpr:(poly_fixup_expr syms bsym_table polyinst sr) 
      exe
    ) 
    exes 
  in
(*
  print_endline ("Applies polyinst:\n" ^ show_exes bsym_table exes);
*)
  exes

let fixup_qual vars mt qual = 
  match qual with
  | `Bound_needs_shape t -> `Bound_needs_shape (mt vars t)
  | x -> x
 
(*
let monomap_compare (i,ts) (i',ts') = 
  let counter = ref 1 in (* HACK *)
  let dummy = Flx_bsym_table.create () in 
  if i = i' && List.length ts = List.length ts' &&
    List.fold_left2 (fun r t t' -> r && Flx_unify.type_eq dummy counter t t') true ts ts'
  then 0 else compare (i,ts) (i',ts') 
*)

module MonoMap = 
  struct 
    type key = int * Flx_btype.t list 
    type target = int
    type kv = key * target
    type data = kv list

    let counter = ref 1 (* HACK *)
    let dummy = Flx_bsym_table.create ()  
    let cmp (i,ts) (i',ts') =
      i = i' && List.length ts = List.length ts' &&
      List.fold_left2 (fun r t t' -> r && Flx_unify.type_eq dummy counter t t') true ts ts'

    let ecmp k (k',_) = cmp k k'

    let mem k d = List.exists (ecmp k) d
    let find (k:key) (d:data):target = snd (List.find (ecmp k) d)
    let choose d = List.hd d
    let remove k d =
      List.filter (fun (k',_) -> not (cmp k k')) d
    let empty = []
    let is_empty d = d = []
    let add k v d = (k,v) :: d (* unchecked! *)
  end

let find_felix_inst syms bsym_table processed to_process nubids i ts : int =
  let find_inst syms processed to_process i ts =
    try 
      Some (MonoMap.find (i,ts) !processed)
    with Not_found ->
    try
      Some (MonoMap.find (i,ts) !to_process)
    with Not_found -> None
  in
  match find_inst syms processed to_process i ts with
  | None ->
    let k = 
      if List.length ts = 0 then i else  
       let nubid = fresh_bid syms.counter  in
       nubids := BidSet.add nubid (!nubids);
       nubid
    in
    let target = k in
    to_process := MonoMap.add (i,ts) target !to_process;
    (*
    if i <> k then
      print_endline ("Add inst to process: " ^ showts bsym_table i ts ^ " --> "^si k);
    *)
    k
  | Some (k) -> k

let notunitassign exe = match exe with
  | BEXE_assign (_,_,(_,BTYP_tuple [])) 
  | BEXE_init (_,_, (_,BTYP_tuple []))
    -> false
  | _ -> true

let rec notemptycall (bsym_table:Flx_bsym_table.t) (trail: int list) exe : bool = 
  match exe with
  | BEXE_call (sr,(BEXPR_closure (f,ts),_),(_,BTYP_tuple[])) ->
    if List.mem f trail then false (* INFINITE RECURSION! *)
    else
    begin 
      let bsym = Flx_bsym_table.find bsym_table f in
      match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (_,_,_,_,_,exes)  ->
        begin match exes with
        | [BEXE_proc_return _] -> false
        | ls  ->
          begin try List.iter 
            (fun exe -> 
              if notemptycall bsym_table (f::trail) exe 
              then raise Not_found
              else ()
            )
            ls;
            false
          with Not_found -> true
          end
        end
      | _ -> true
    end 
  | _ -> true

let strip_unit_assigns exes = List.filter notunitassign exes 

(* remove calls to procedures that do nothing. Do NOT remove
the procedures, let the GC do that: they might be passed as arguments
to some HOF
*)
let strip_empty_calls bsym_table exes = 
  List.filter (notemptycall bsym_table []) exes 

let mono_bbdcl syms bsym_table processed to_process nubids virtualinst polyinst ts bsym i j =
(*
if List.length ts > 0 then begin
  print_endline ("[mono_bbdcl] " ^ Flx_bsym.id bsym);
  print_endline ("ts=[" ^ catmap "," (sbt bsym_table) ts ^ "]");
end;
*)
  List.iter (fun t -> if not (complete_type t) then 
    print_endline ("Argument not complete!!!!!!!!!!!!!!!!!!!!!!!")
  )
  ts;
  let sr = Flx_srcref.make_dummy ("[mono_bbdcl] " ^ Flx_bsym.id bsym) in 
  begin try List.iter (check_mono bsym_table sr) ts with _ -> assert false end;

(*
  let original_instance_type = BTYP_inst (i,ts) in
  let unfolded_instance_type = 
    try 
      unfold "unfold instance in numono" original_instance_type
    with 
    | _ -> 
      print_endline ("Unfold instance failed! " ^ sbt bsym_table original_instance_type); 
      assert false
  in
  let ts' = match unfolded_instance_type with
    | BTYP_inst (_, ts') -> ts'
    | _ -> assert false
  in
*)

  let mt vars t = fixup_type syms bsym_table vars bsym virtualinst polyinst sr t in
  let bbdcl = Flx_bsym.bbdcl bsym in
  match bbdcl with
  | BBDCL_label s -> Some (bbdcl_label s)

  | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
    begin try
      let props = List.filter (fun p -> p <> `Virtual) props in
      if List.length vs <> List.length ts then begin try
        print_endline ("[mono] vs/ts mismatch in " ^ Flx_bsym.id bsym ^ " vs=[" ^ 
          catmap "," (fun (s,i) -> s) vs ^ "]");
        print_endline ("ts=[" ^ catmap "," (sbt bsym_table) ts ^ "]");
        assert false
        with Not_found -> print_endline "Not_found printint ts?"; assert false
      end;
      let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
      let ret = 
        try mt vars ret 
        with Not_found -> print_endline "Not_found fixing up return type"; 
          print_endline ("Ret=" ^ sbt bsym_table ret); 
          assert false 
      in
      let effects = 
        try mt vars effects
        with Not_found -> print_endline "Not_found fixing up effects type"; 
          print_endline ("Effects=" ^ sbt bsym_table effects); 
          assert false 
      in
      let ps = try
        (*
        print_endline ("+++processing parameters of " ^ Flx_bsym.id bsym);
        *)
        let ps = List.map (fun {pkind=pk; pid=s;pindex=i; ptyp=t} ->
        {pkind=pk;pid=s;pindex=fst (polyinst sr i ts);ptyp=mt vars t}) ps in
        (*
        print_endline ("+++parameters processed: " ^ Flx_bsym.id bsym);
        *)
        ps
        with Not_found -> print_endline ("Not Found FIXING parameters"); assert false
      in
      (* fudge unit parameters *)
      let ps = List.map (fun {pkind=pk; pid=s; pindex=i; ptyp=t} ->
        {pkind=pk;pid=s;pindex=(match t with BTYP_tuple [] -> 0 | _ -> i);ptyp=t}) ps 
      in
      let traint =
        match traint with
        | None -> None
        | Some x -> Some (fixup_expr syms bsym_table (mt vars) virtualinst polyinst sr x)
      in
      let exes = strip_empty_calls bsym_table exes in
      let exes = 
        try fixup_exes syms bsym_table vars virtualinst polyinst ts exes 
        with Not_found -> assert false
      in
      let exes = strip_unit_assigns exes in
      let exes = List.map (fun exe -> Flx_bexe.map ~f_bexpr:Flx_bexpr.reduce exe) exes in
      let props = List.filter (fun p -> p <> `Virtual) props in
      Some (bbdcl_fun (props,[],(ps,traint),ret,effects,exes))
    with Not_found ->
      assert false
    end

  | BBDCL_val (vs,t,kind) ->
(*
print_endline ("Monomorphising variable "^Flx_bsym.id bsym ^" polytype " ^ sbt bsym_table t);
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    (* eliminate unit variables *)
    begin match t with
    | BTYP_void -> print_endline ("Void variable?"); assert false
    | BTYP_tuple [] -> 
      (* print_endline ("Elim unit var " ^ Flx_bsym.id bsym ^ " old index " ^ si i ^ " new index would be " ^ si j); i*)
      None
    | _ -> Some (bbdcl_val ([],t,kind))
    end

  (* we have tp replace types in interfaces like Vector[int]
    with monomorphic versions if any .. even if we don't
    monomorphise the bbdcl itself.

    This is weak .. it's redone for each instance, relies
    on mt being idempotent..
  *)
  | BBDCL_external_fun (props,vs,argtypes,ret,reqs,prec, fkind) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let argtypes = List.map (mt vars) argtypes in
    let ret = mt vars ret in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    let props = List.filter (fun p -> p <> `Virtual) props in
    Some (bbdcl_external_fun (props,vs,argtypes,ret,reqs,prec,fkind))

  | BBDCL_external_const (props, vs, t, CS.Str "#this", reqs) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let _ = mt vars t in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_const (props, [], t, CS.Str "#this", reqs))

  | BBDCL_external_const (props, vs, t,cs, reqs) ->
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_const (props,vs, t, cs, reqs))
 
  | BBDCL_external_type (vs,quals,cs,reqs)  -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    let quals = List.map (fixup_qual vars mt) quals in
    Some (bbdcl_external_type (vs,quals,cs, reqs))

  | BBDCL_external_code (vs,cs,ikind,reqs)   -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_external_code (vs,cs,ikind,reqs))

  | BBDCL_union (vs,cps) -> 
(*
    if List.length vs <> List.length ts then begin
      print_endline ("Monomorphise union " ^ sbt bsym_table (btyp_inst (i,ts)) ^ 
      " with ts/vs mismatch, expected " ^ string_of_int (List.length vs) ^
      " type variables to match " ^ string_of_int (List.length ts) ^ " arguments");
      print_endline "Probably a GADT?";
    end;
*)
    List.iter (fun t -> if not (Flx_btype.complete_type  t) then 
    print_endline ("type variable substitution type is not complete " ^ 
      sbt bsym_table t)) ts;
    let gadt = List.fold_left (fun acc (name,index,evs,d,c,gadt) -> 
       gadt || acc) false cps
    in
    let ut = btyp_inst (i,ts) in
if gadt then
begin
(*
print_endline ("Monomorphising union " ^ sbt bsym_table ut);
print_endline ("  Polymorphic union index = " ^ string_of_int i);
print_endline ("  Union universal type variable instances = " ^ catmap "," (fun t -> sbt bsym_table t) ts);
print_endline ("  Target monomorphic index = " ^ string_of_int j);
*)
    let try_cal_ctor (name,index,evs,d,c,gadt) =
(*
print_endline ("    Examining constructor " ^ name ^ "<" ^ string_of_int index ^">[" ^
  catmap "," (fun (s,k) -> s^"<"^string_of_int k^">") evs ^ "] of " ^ 
  sbt bsym_table d ^ " => " ^ sbt bsym_table c);
*)

      let dvars = ref BidSet.empty in
      List.iter (fun (_,i) -> dvars := BidSet.add i (!dvars)) vs;
      List.iter (fun (_,i) -> dvars := BidSet.add i (!dvars)) evs;
      let maybe_mgu = 
        let eqns = [c,ut] in
(*
print_endline ("Attempting unification: " ^ sbt bsym_table c ^ " =? " ^ sbt bsym_table ut);
print_endline ("Dependent variables:"); 
  BidSet.iter (fun i -> print_endline ("DVAR=" ^ string_of_int i)) (!dvars);
*)
        try Some (Flx_unify.unification bsym_table syms.counter eqns !dvars)
        with 
          | Free_fixpoint _ -> print_endline ("Free fixpoint"); None 
          | Not_found -> None
      in
      match maybe_mgu with
      | None -> 
(*
        print_endline ("      Unification FAILED"); 
*)
        raise Flx_exceptions.GadtUnificationFailure
      | Some mgu ->
(*
        print_endline ("      Unified with MGU=" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) mgu);
*)
        let mgu = List.map (fun (i,t) -> i, Flx_unify.minimise bsym_table syms.counter t) mgu in
(*
        print_endline ("      Miniused MGU   =" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) mgu);
*)
(* NOTE: for non GADT, this should agree with the argument var -> ts binding!! *)
(*
    if (List.length vs = List.length ts) then begin
      let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
        print_endline ("      ORIGINAL ASSIGN=" ^ catmap "," (fun (i,t) ->
          string_of_int i ^ "->" ^ sbt bsym_table t) vars);
    end else begin
       print_endline  ("      ORIGINAL ASSIGN WOULD FAIL TO MONOMORPHISE (ts/vs mismatch due to GADT)");    
    end;
*)
(*
        let varmap = Hashtbl.create 3 in
        List.iter (fun (j,t) -> Hashtbl.add varmap j t) mgu;
        let d = Flx_unify.varmap_subst varmap d in
        print_endline ("      ctor domain (poly) = " ^ sbt bsym_table d);
        let c = Flx_unify.varmap_subst varmap c in
*)
(*
        let d = unfold "union monomorphisation" d in
        print_endline ("      ctor domain (unfolded) = " ^ sbt bsym_table d);
*)
(*
        let d = Flx_unify.list_subst syms.counter mgu d in
*)
        let d = mt mgu d in
(*
        print_endline ("      ctor domain (mono) = " ^ sbt bsym_table d);
*)
        name,index,[],d,(btyp_void ()),gadt
    in
    let cal_ctor x = try Some (try_cal_ctor x) 
      with Flx_exceptions.GadtUnificationFailure -> None 
    in
    let cps = List.rev_map cal_ctor cps in
    let cps = List.fold_left (fun acc x -> 
      match x with Some x -> x::acc | None -> acc) [] cps
    in
(*
print_endline ("Finished union by GADT **");
*)
    Some (bbdcl_union ([], cps))
end else begin
  if (List.length vs <> List.length ts) then
    print_endline ("Union "^sbt bsym_table ut ^ " vs length " ^ string_of_int (List.length vs) ^ 
    " doesn't agree with ts length " ^ string_of_int (List.length ts));
  assert (List.length vs = List.length ts);
(*
       print_endline ("******* Union "^sbt bsym_table ut);
*)
(*
    if Flx_unify.is_recursive_type ut then
       print_endline ("Union "^sbt bsym_table ut ^" is recursive");
    List.iter (fun t -> if not (Flx_btype.complete_type  t) then 
    print_endline ("non-gadt: Union: "^sbt bsym_table (btyp_inst (i,ts))^ ",type variable substitution type is not complete " ^ 
      sbt bsym_table t)) ts;
*)
(* THIS IS A HACK, it only works with fix-1, i.e. an argument
which is precisely a pointer to the union type. We need to fix this
so if the recursion is more deeply embedded it also works
*)
(*
  let ut' = 
    try 
      unfold "unfold recursive union in numono" ut 
    with 
    | _ -> print_endline "Unfold union failed!"; assert false
  in
  let ts' = match ut' with
    | BTYP_inst (_, ts') -> ts'
    | _ -> assert false
  in
*)
(*
    if Flx_unify.is_recursive_type ut then
    print_endline ("Unfolded union = " ^ sbt bsym_table ut');
*)
(*
    let ts = List.map (fun t -> match t with 
    | BTYP_fix (-1,_) -> ut
    | t -> t) ts
    in
*)
(*
  let vars = List.map2 (fun (s,i) t -> i,t) vs ts' in
*)
  let vars = List.map2 (fun (s,i) t -> i,t) vs ts in


(*
if Flx_unify.is_recursive_type ut then
begin
  print_endline ("Recursive Union type " ^ sbt bsym_table ut ^
  " should be replaced by assigned monomorphic type " ^ sbt bsym_table (mt vars ut))
end;
*)
  let cps = List.map (fun (name,index,ivs,argt, resultt,gadt) -> 
(*
if Flx_unify.is_recursive_type ut then begin
  print_endline ("Recursive Union type " ^ sbt bsym_table ut ^
  " constructor " ^ name ^ ": argument type " ^ sbt bsym_table argt ^ 
  " will be replaced by monomorphic type " ^ sbt bsym_table (mt vars argt))
end;
*)
    name,index, [],mt vars argt, btyp_none (),gadt 
    ) cps 
  in
(*
print_endline ("Finished union by non GADT **");
*)
  Some (bbdcl_union ([], cps))
end

  | BBDCL_cstruct (vs,cps, reqs) -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let cps = List.map (fun (name,argt) -> name,mt vars argt) cps in
    let reqs = fixup_reqs syms bsym_table vars polyinst sr reqs in
    Some (bbdcl_cstruct ([], cps, reqs))

  | BBDCL_struct (vs,cps)  -> 
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let cps = List.map (fun (name,argt) -> name,mt vars argt) cps in
    Some (bbdcl_struct ([], cps))


  | BBDCL_const_ctor (vs,uidx,ut,ctor_idx,evs,etraint) ->
(*
print_endline "Monomorphising constant constructor?";
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let ut = mt vars ut in
    let uidx = find_felix_inst syms bsym_table processed to_process nubids uidx ts in
    Some (bbdcl_const_ctor ([],uidx,ut,ctor_idx,evs,etraint)) (* ignore GADT stuff *)
 
  | BBDCL_nonconst_ctor (vs,uidx,ut,ctor_idx,ctor_argt,evs,etraint) ->
    assert (List.length vs = List.length ts);
(*
print_endline ("Monomorphising nonconst ctor argt=: " ^ sbt bsym_table ctor_argt ^ " => " ^
   sbt bsym_table ut);
*)
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let ut = mt vars ut in
    let uidx = find_felix_inst syms bsym_table processed to_process nubids uidx ts in
    let ctor_argt = mt vars ctor_argt in
(*
print_endline ("Monomorphised nonconst ctor argt=: " ^ sbt bsym_table ctor_argt ^ " => " ^
   sbt bsym_table ut);
*)
    Some (bbdcl_nonconst_ctor ([],uidx, ut,ctor_idx,ctor_argt,evs,etraint)) (* ignore GADT stuff *)
 

  | BBDCL_typeclass _ -> assert false
  | BBDCL_instance _ -> assert false

  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce -> assert false 

  | BBDCL_invalid  -> assert false

  | BBDCL_newtype (vs,t) ->  
(*
print_endline ("ADJUSTING NEWTYPE " ^Flx_bsym.id bsym );
*)
    assert (List.length vs = List.length ts);
    let vars = List.map2 (fun (s,i) t -> i,t) vs ts in
    let t = mt vars t in
    Some (bbdcl_newtype ([],t))
  
  | BBDCL_module -> assert false


let rec mono_element debug syms to_process processed bsym_table nutab nubids i ts j =
(*
  print_endline ("mono_element: " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]" ^ " --> " ^ si j);
*)
  let virtualinst sr i ts =
    try Flx_typeclass.fixup_typeclass_instance syms bsym_table sr i ts 
    with Not_found -> 
      print_endline ("[mono-element:virtualinst] Can't find index " ^ si i); 
      if BidSet.mem i (!nubids) then begin
        print_endline "FOUND IN NEW TABLE .. OK";
        i,ts
      end else
        assert false
  in

  let polyinst sr i ts =  
    let sym = 
      try Some (Flx_bsym_table.find bsym_table i)
      with Not_found ->
         print_endline ("[mono-element:polyinst] Can't find index " ^ si i); 
         if BidSet.mem i (!nubids) then begin
           print_endline "FOUND IN NEW TABLE .. OK";
           None
         end else
         assert false
    in
    match sym with
    | None -> assert (ts = []); i, ts
    | Some sym ->
    let {Flx_bsym.id=id;sr=sr; bbdcl=bbdcl} = sym in
    match bbdcl with
    | BBDCL_external_type _ 
    | BBDCL_external_const _ 
    | BBDCL_external_fun _ 
    | BBDCL_external_code _  -> 
      let j = find_felix_inst syms bsym_table processed to_process nubids i ts in
      j,ts
    | _ ->
      let j = find_felix_inst syms bsym_table processed to_process nubids i ts in
      j,[]
  in
  let sr = Flx_srcref.make_dummy "[mono_element]" in
  begin try List.iter (check_mono bsym_table sr) ts with _ -> assert false end;
  try
    let parent,sym = 
      try Flx_bsym_table.find_with_parent bsym_table i 
      with Not_found -> assert false
    in
    let {Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} = sym in
    let parent = match parent with
      | None -> None
      | Some 0 -> Some 0 
      | Some p -> 
        let psym = 
          try Flx_bsym_table.find bsym_table p 
          with Not_found -> 
            print_endline ("[mono_element] Cannot find parent " ^ si p);
            assert false 
        in
        let {Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} = psym in
        begin match bbdcl with
        | BBDCL_fun (_,vs,_,_,_,_) ->
          let n = List.length vs in
          let pts = Flx_list.list_prefix ts n in 
(*
print_endline ("Our ts = " ^ catmap "," (sbt bsym_table) ts);
print_endline ("Parent vs = " ^ catmap "," (fun (s,i) -> s) vs);
print_endline ("Parent ts = " ^ catmap "," (sbt bsym_table) pts);
*)
(*
          print_endline ("  mono_element: adding parent " ^ si p ^" = " ^ id ^ ", ts=" ^ catmap "," (sbt bsym_table) pts);
*)
          let nuparent = find_felix_inst syms bsym_table processed to_process nubids p pts in
(*
          print_endline ("Nu parent: " ^ si nuparent);
*)
          Some nuparent

        | BBDCL_instance _
        | BBDCL_module
        | BBDCL_typeclass _ -> None
        | _ -> assert false 
        end
    in
    let maybebbdcl = 
      try mono_bbdcl syms bsym_table processed to_process nubids virtualinst polyinst ts sym i j 
      with Not_found -> assert false 
    in
    begin match maybebbdcl with
    | Some nubbdcl -> 
      (* NOTE: we don't use [] here bpair[unit, int]ecause it's confusing with polymorphism *)
      let nuname = Flx_bsym.id sym (* ^ ( 
        if List.length ts = 0 then "" 
         else "{" ^ catmap "," (sbt bsym_table) ts ^ "}")  *)
      in
      let nusym ={Flx_bsym.id=nuname; sr=sr; bbdcl=nubbdcl} in
      Flx_bsym_table.add nutab j parent nusym
    | None -> ()
    end
  with Not_found -> 
   print_endline "NOT FOUND in mono_element";
   raise Not_found

let monomorphise2 debug syms bsym_table =
(*
    print_endline "";
    print_endline "---------------------------";
    print_endline "PRE NUMONO";
    print_endline "---------------------------";
    print_endline "";

    Flx_print.print_bsym_table bsym_table;
*)
  let roots: BidSet.t = !(syms.roots) in
  assert (BidSet.cardinal roots > 0);


  (* to_process is the set of symbols yet to be scanned
     searching for symbols to monomorphise
  *)
  let to_process = ref MonoMap.empty in
  BidSet.iter (fun i -> to_process := MonoMap.add (i,[]) (i) (!to_process)) roots;
  
  let processed = ref MonoMap.empty in

  (* new bsym_table *)
  let nutab = Flx_bsym_table.create () in

  (* Set of indices of NEW symbols to go or already gone into it *)
  let nubids = ref  BidSet.empty in 

  let sr = Flx_srcref.make_dummy "[monomorphise2]" in
  while not (MonoMap.is_empty (!to_process)) do
    let (i,ts),j = MonoMap.choose (!to_process) in
    assert (not (MonoMap.mem (i,ts) (!processed) ));
    begin try List.iter (check_mono bsym_table sr) ts with _ -> assert false end;

    to_process := MonoMap.remove (i,ts) (!to_process);
    processed := MonoMap.add (i,ts) j (!processed);

    (*
    (* if i <> j then *)
      print_endline ("numono: "^showts bsym_table i ts ^" ==> " ^ si j);
    *)
    assert (List.length ts > 0 || i == j);
    assert (not (Flx_bsym_table.mem nutab j));
    mono_element debug syms to_process processed bsym_table nutab nubids i ts j;
  done
  ;

  Hashtbl.clear syms.instances_of_typeclass;
  Hashtbl.clear syms.virtual_to_instances;
  syms.axioms := [];
(*
  syms.reductions := [];
*)
(*
print_endline ("Allowing " ^ string_of_int (List.length !(syms.reductions)) ^ " reductions");
*)
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then 
  begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST NUMONO";
    print_endline "---------------------------";
    print_endline "";

    Flx_print.print_bsym_table nutab
  end;
  nutab


