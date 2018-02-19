open Flx_util
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_btype_subst


(* this module, strabs, is responsible for upgrading an abstract
  type to its implementation: strip abstract types

  well .. actually its a downgrade 
*)

(* identify a type which is a union with a single constructor *)
let is_solo_union bsym_table t =
  match t with
  | BTYP_inst (i,ts,_) ->  (* ts already upgraded by the Flx_btype.map *)
    let bsym =
      try Flx_bsym_table.find bsym_table i 
      with Not_found -> failwith ("can't find entry " ^ string_of_int i ^ " in bsym table")
    in
    let entry =  Flx_bsym.bbdcl bsym in
    begin match entry with
    | BBDCL_union (vs,[id,idx,[],dt,ct,gadt]) -> not gadt
    | _ -> false
    end
  | _ -> false

let is_nonconst_ctor bsym_table i =
  let bbdcl = Flx_bsym_table.find_bbdcl bsym_table i in
  match bbdcl with
  | BBDCL_nonconst_ctor _ -> true
  | _ -> false

let get_solo_union_ctor_arg_type bsym_table t =
  match t with
  | BTYP_inst (i,ts,_) ->  (* ts already upgraded by the Flx_btype.map *)
    let bsym =
      try Flx_bsym_table.find bsym_table i 
      with Not_found -> failwith ("can't find entry " ^ string_of_int i ^ " in bsym table")
    in
    let entry =  Flx_bsym.bbdcl bsym in
    begin match entry with
    | BBDCL_union (vs,[id,idx,[],dt,ct,gadt]) -> dt
    | _ -> assert false
    end
  | _ -> assert false


let fixtype bsym_table t =
  let rec f_btype t =
    let t = Flx_btype.map ~f_btype t in
    match t with 
    | BTYP_inst (i,ts,_) ->  (* ts already upgraded by the Flx_btype.map *)
      let bsym =
        try Flx_bsym_table.find bsym_table i 
        with Not_found -> failwith ("can't find entry " ^ string_of_int i ^ " in bsym table")
      in
      let entry =  Flx_bsym.bbdcl bsym in
      begin match entry with

(* Eliminate new types *)
      | BBDCL_newtype (vs,t) -> 
assert (vs = []);
(* tsubst should be redundant! *)
        let t = tsubst (Flx_bsym.sr bsym) vs ts t in 
        let t' = f_btype t in (* rescan replacement type *) 
        t'
      
(* Eliminate unions with one constructor *)
      | BBDCL_union (vs,[id,idx,[],dt,ct,gadt]) when not gadt ->
(*
print_endline ("[flx_strabs] Eliminating union with one constructor: union name="  ^
  Flx_bsym.id bsym ^
", constructor name=" ^ id ^ 
", constructor argument type = " ^ sbt bsym_table ct
);
*)
assert (vs = []);
(* tsubst should be redundant! *)
        let t = tsubst (Flx_bsym.sr bsym) vs ts dt in 
        let t' = f_btype t in (* rescan replacement type *) 
(* argumentless constructors use void argument type instead of unit ... *)
        begin match t' with
        | BTYP_void -> Flx_btype.btyp_unit ()
        | _ -> t'
        end
      | _ -> 
        t
      end 
    | _ -> 
    t
  in
  f_btype t

let fixqual bsym_table qual = 
  match qual with
  | `Bound_needs_shape t -> `Bound_needs_shape (fixtype bsym_table t)
  | x -> x

let fixquals bsym_table quals =
  map (fixqual bsym_table) quals

let fixreq bsym_table  (bid,ts) =
  bid, map (fixtype bsym_table) ts

let fixreqs bsym_table reqs = map (fixreq bsym_table) reqs

(* this analysis has to be top down not bottom up, so that
the solo union detectors work
*)
let rec fixexpr' bsym_table e =
(* print_endline ("OMG fix expr " ^ sbe bsym_table e); *)

    let f_btype t = fixtype bsym_table t in 
    let f_bexpr e = fixexpr' bsym_table e in
    match e with
    | BEXPR_apply ( (BEXPR_closure(i,_),_),a),_
    | BEXPR_apply_direct (i,_,a),_
    | BEXPR_apply_prim (i,_,a),_
      when (
       try Flx_bsym_table.is_identity bsym_table i 
       with Not_found -> 
         failwith ("strabs:is_identity checked not found on " ^ string_of_int i)
      ) -> f_bexpr a

(* for union U with one constant constructor C, replace C with unit *)
    | BEXPR_case (_,ut),_ when is_solo_union bsym_table ut -> 
      print_endline "  ** [fixexpr] Replace constant constructor of solo SUM with unit";
      Flx_bexpr.bexpr_unit

(* for union U with one non-constant constructor C, replace C a with a *)
    | BEXPR_apply ((BEXPR_inj (idx, ct,ut),_), a),_ when is_solo_union bsym_table ut -> 
      print_endline "  ** [fixexpr] Replace application of non-constant constructor of solo union (INJECTION) with argument";
      f_bexpr a

(* DEPRECATED CASE! Should use injection now *)
(* for union U with one non-constant constructor C, replace C a with a *)
    | BEXPR_apply ((BEXPR_varname (k,ts),BTYP_function (atyp1,ut)), a),ut2 
      when is_solo_union bsym_table ut && is_nonconst_ctor bsym_table k
      -> 
      print_endline "  ** [fixexpr]xx Replace application of non-constant constructor of solo union (VARNAME) with argument";
      f_bexpr a

(* for union U with one non-constant constructor C, replace C a with a *)
    | BEXPR_apply ((BEXPR_closure (k,ts),BTYP_function (atyp1,ut)), a),ut2 
      when is_solo_union bsym_table ut && is_nonconst_ctor bsym_table k
      -> 
(*
      print_endline "  ** [fixexpr]yy Replace application of non-constant constructor of solo union (CLOSURE) with argument";
*)
      f_bexpr a


    | BEXPR_case_index (x,ut),_ when is_solo_union bsym_table ut -> 
      print_endline ("  ** [fixexpr] Woops, case index of solo union, should be 0 but of what type? Let try int");
      Flx_bexpr.bexpr_int 0


    | BEXPR_match_case (idx,(x,ut)),_ when is_solo_union bsym_table ut ->
(*
       print_endline ("  ** [fixexpr] match case index= " ^ si idx^ " of solo union type "^sbt bsym_table ut^
        ", constructor " ^ si idx);
*)
(*
      assert (idx = 0);
*)
      Flx_bexpr.bexpr_true

    | BEXPR_case_arg (idx,(x,ut)),argt as y when is_solo_union bsym_table ut ->
(*
      print_endline ("  ** [fixexpr] Constructor argument "^si idx^ " of (solo union?) type "^sbt bsym_table ut^
      ", constructor " ^ si idx);
*)
      f_bexpr (x,ut)

    | ((BEXPR_inj (idx, ct,ut),_)) when is_solo_union bsym_table ut -> 
print_endline ("FIXME: Found constructor of solo union used as injection style closure");
      assert false

    | BEXPR_closure (i,ts),BTYP_function (argt,ut) when 
      is_solo_union bsym_table ut &&
      is_nonconst_ctor bsym_table i
      ->
      let t = f_btype argt in
(*
print_endline ("[  ** [fixexp] Found constructor of solo union used as closure using identity type " ^ 
  sbt bsym_table argt ^ " ==? " ^
  sbt bsym_table t
);
*)
      (* let ct = get_solo_union_ctor_arg_type bsym_table t in *)
      bexpr_identity_function t

(*
      begin match ct with
      | BTYP_void
      | BTYP_tuple [] ->
        print_endline ("  ** [fixexpr] Replace closure " ^ si i^ " of solo UNION type " ^ 
          sbt bsym_table t ^ " with unit");
        Flx_bexpr.bexpr_unit
      | _ ->
        print_endline ("  ** [fixexpr] Replace closure " ^ si i ^ " of solo UNION type "^ 
          sbt bsym_table t ^ " with nonconst ctor of type " ^ sbt bsym_table ct ^ " with closure of that type");
        Flx_bexpr.bexpr_closure ct (i, ts)
      end
*)

    | BEXPR_varname (i,ts),t when is_solo_union bsym_table t ->
      let ct = get_solo_union_ctor_arg_type bsym_table t in
      begin match ct with
      | BTYP_void
      | BTYP_tuple [] ->
(*
        print_endline ("  ** [fixexpr] Replace variable " ^ si i ^ " of solo UNION type "^ 
          sbt bsym_table t ^ " with unit");
*)
        Flx_bexpr.bexpr_unit
      | _ ->
(*
        print_endline ("  ** [fixexpr] Replace variable " ^ si i ^ " of solo UNION type "^ 
          sbt bsym_table t ^ " with nonconst ctor of type " ^ sbt bsym_table ct ^ " with variable of that type");
*)
        Flx_bexpr.bexpr_varname ct (i, ts)
      end


    | x -> Flx_bexpr.map ~f_btype ~f_bexpr x 

let fixexpr bsym_table x = 
  let y = fixexpr' bsym_table x in
(* print_endline ("  %%%%% Fixexpr " ^ sbe bsym_table x ^ " --> " ^ sbe bsym_table y); *)
  y

let fixbexe bsym_table x =
  let y = Flx_bexe.map ~f_btype:(fixtype bsym_table) ~f_bexpr:(fixexpr bsym_table) x in
(* print_endline ("    &&&&&& Fixbexe " ^ string_of_bexe bsym_table 4 x ^ " ----> " ^ string_of_bexe bsym_table 4 y); *)
  y

let fixbexes bsym_table bexes = 
  let unit_t = Flx_btype.btyp_unit () in
  let fbx x = fixbexe bsym_table x in
  let pr lst x = match fbx x with
  | Flx_bexe.BEXE_assign (sr,(a,at),(_,t)) when t = unit_t -> assert (t=at); lst
  | Flx_bexe.BEXE_init (sr,_,(_,t)) when t = unit_t -> lst
  | y -> y::lst
  in
  List.rev (
    List.fold_left pr [] bexes
  )

let fixps bsym_table (ps,traint) =
  Flx_bparams.xpmap (fun p -> { p with ptyp=fixtype bsym_table p.ptyp }) ps,
  (
  match traint with
  | None -> None
  | Some t -> Some (fixexpr bsym_table t)
  )

let strabs_symbol bsym_table index parent bsym bsym_table' =
  let ft t = fixtype bsym_table t in
  let fts ts = map (fixtype bsym_table) ts in
  let fe e = fixexpr bsym_table e in
  let fxs xs = fixbexes bsym_table xs in
  let fp bps = fixps bsym_table bps in
  let fb rqs = fixreqs bsym_table rqs in
  let fq qs = fixquals bsym_table qs in

  let h bbdcl = Flx_bsym_table.add bsym_table' index parent { bsym with Flx_bsym.bbdcl= bbdcl } in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_nominal_type_alias _
  | BBDCL_structural_type_alias _
  | BBDCL_instance_type _  
  | BBDCL_virtual_type _  
  | BBDCL_invalid
  | BBDCL_module
  | BBDCL_typeclass _
  | BBDCL_instance _
  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce 
    -> assert false

  | BBDCL_newtype (bvs, t) -> ()

  | BBDCL_label s as x -> h x

  | BBDCL_fun (props, bvs, bps, ret, effects, bexes) ->
      h (bbdcl_fun (props, bvs, fp bps, ft ret, ft effects, fxs bexes))

  | BBDCL_val (bvs, t, kind) ->
      h (bbdcl_val (bvs, ft t, kind))

  | BBDCL_external_type (bvs, btqs, c, breqs) ->
      h (bbdcl_external_type (bvs, fq btqs, c, fb breqs))

  | BBDCL_external_const (props, bvs, t, c, breqs) ->
      h (bbdcl_external_const (props,  bvs, ft t, c, fb breqs))

  | BBDCL_external_fun (props, bvs, ts, t, breqs, prec, kind) ->
      (* Ignore identity functions. *)
      if kind = `Code Flx_code_spec.Identity then () else
      let kind =
        match kind with
        | `Callback (ts2, j) -> `Callback (fts ts2, j)
        | _ -> kind
      in
      h (bbdcl_external_fun (props, bvs, fts ts, ft t, fb breqs, prec, kind))

  | BBDCL_external_code (bvs, c, ikind, breqs) ->
      h (bbdcl_external_code (bvs, c, ikind, fb breqs))

(*
  (* eliminate unions with solo constructors *)
  | BBDCL_union (bvs, [id,idx,ct]) -> 
print_endline ("Removing entry for index = "^si index^" : union " ^Flx_bsym.id bsym ^ " with solo constructor " ^ id);
    ()
*)

  | BBDCL_union (bvs, cts) ->
      let cts = map (fun (s,j,evs,d,c,gadt) -> s,j,evs,ft d, ft c,gadt) cts in
      h (bbdcl_union (bvs, cts))

  | BBDCL_struct (bvs, cts) ->
      let cts = map (fun (s,t) -> s,ft t) cts in
      h (bbdcl_struct (bvs, cts))

  | BBDCL_cstruct (bvs, cts, breqs) ->
      let cts = map (fun (s,t) -> s,ft t) cts in
      h (bbdcl_cstruct (bvs, cts, fb breqs))
(*  
  | BBDCL_const_ctor (bvs, j, t1, k, evs, etraint) when is_solo_union bsym_table t1 -> 
print_endline ("Removing entry for index = "^si index^
" : const constructor index "^si k^ " name " ^ Flx_bsym.id bsym ^
" for union " ^sbt bsym_table t1 ^ " (solo constructor)");
    ()

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) when is_solo_union bsym_table t1 ->
print_endline ("Removing entry for index = "^si index^
" : nonconst constructor index "^si k^ " name " ^ Flx_bsym.id bsym ^
" arg type " ^ sbt bsym_table t2 ^
" for union " ^sbt bsym_table t1 ^ " (solo constructor)");
    ()
*)

  | BBDCL_const_ctor (bvs, j, t1, k, evs, etraint) ->
      h (bbdcl_const_ctor (bvs, j, ft t1, k, evs, ft etraint))

  | BBDCL_nonconst_ctor (bvs, j, t1, k,t2, evs, etraint) ->
      h (bbdcl_nonconst_ctor (bvs, j, ft t1, k, ft t2, evs, ft etraint))

let strabs bsym_table =
  let bsym_table' : Flx_bsym_table.t = Flx_bsym_table.create_from bsym_table in
  Flx_bsym_table.iter
    (fun bid parent sym -> 
      try
        strabs_symbol bsym_table bid parent sym bsym_table'
      with Not_found ->
        failwith ("Strabs chucked not found processing " ^ string_of_int bid)
    )
    bsym_table
  ;
  bsym_table'



