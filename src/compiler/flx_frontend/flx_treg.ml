open Flx_types
open Flx_btype
open Flx_bbdcl
open Flx_mtypes2
open Flx_typing
open Flx_unify
open Flx_print
open Flx_exceptions
open Flx_util
open Flx_list
open List
open Flx_maps
open Flx_beta

let register_type_nr syms bsym_table t =
let t = normalise_tuple_cons bsym_table t in
(*
  if Flx_unify.is_recursive_type t 
    then print_endline ("Register type nr: recursive type " ^ sbt bsym_table t)
  ;
*)
(*
print_endline ("Register type nr " ^ sbt bsym_table t);
*)
  (*
  if t <> t' then print_endline ("UNREDUCED TYPE! " ^ sbt bsym_table t ^ " <> " ^ sbt bsym_table t');
  *)
  match t with
  | BTYP_label -> ()
  | BTYP_fix _
  | BTYP_tuple []
    -> ()
  | _
    ->
    let t = fold bsym_table syms.counter t in
    if not (Hashtbl.mem syms.registry t)
    then begin
      let () = check_recursion t in
      let n = fresh_bid syms.counter in
      if syms.compiler_options.Flx_options.print_flag then
      print_endline ("//Register type " ^ string_of_bid n ^ ": " ^
        sbt bsym_table t);
      Hashtbl.add syms.registry t n
    end


let register_tuple syms bsym_table t =
  let t = normalise_tuple_cons bsym_table t in
  let t = fold bsym_table syms.counter t in
  let record_tuple t =
    register_type_nr syms bsym_table t;
    try Hashtbl.replace syms.array_as_tuple_registry (Hashtbl.find syms.registry t) ()
    with Not_found -> ()
  in
  match t with
  | BTYP_tuple [] -> ()
  | BTYP_tuple [_] -> assert false

  | BTYP_tuple ts -> record_tuple t

  | BTYP_array (t',BTYP_unitsum n) ->
    let ts = rev_map (fun _ -> t') (nlist n) in
    record_tuple (btyp_tuple ts)

  | BTYP_record (ts) ->
    begin match t with
    | BTYP_tuple [] -> ()
    | _ -> record_tuple t
    end

  | _ ->
print_endline ("flx_treg: Try to register tuple " ^ sbt bsym_table t);
    assert false

let rec register_type_r ui syms bsym_table exclude sr t =
let t = normalise_tuple_cons bsym_table t in
(*
print_endline ("Register type r " ^ sbt bsym_table t);
*)
  let t = beta_reduce "flx_treg: register_type" syms.Flx_mtypes2.counter bsym_table sr t in
  (*
  let sp = String.make (length exclude * 2) ' ' in
  print_endline (sp ^ "Register type " ^ sbt sym_table t);
  if (mem t exclude) then print_endline (sp ^ "Excluded ..");
  *)
  if not (Hashtbl.mem syms.registry t) then
  if not (mem t exclude) then
  if complete_type t then
  let rr t' = register_type_r ui syms bsym_table (t :: exclude) sr t' in
  let rnr t = register_type_nr syms bsym_table t in
  let t' = unfold "flx_treg" t in
  (*
  print_endline (sp ^ "Unfolded type " ^ sbt sym_table t');
  *)
  match t' with
  | BTYP_label -> ()
  | BTYP_void -> ()
  | BTYP_fix (0,_) -> ()
  | BTYP_fix (i,_) -> clierr sr ("[register_type_r] Fixpoint "^si i^" encountered")
  | BTYP_polyrecord _ -> clierr sr ("[register_type_r] attempt to bind polyrecord type")
  (*
  | BTYP_type_var (i,mt) -> clierr sr ("Attempt to register type variable " ^ si i ^":"^sbt sym_table mt)
  *)
  | BTYP_type_var (i,mt) ->
    (*
    print_endline ("Attempt to register type variable " ^ string_of_bid i ^
      ":" ^ sbt bsym_table mt);
    *)
    ()
  | BTYP_function (ps,ret) ->
    let ps = match ps with
    | BTYP_void -> btyp_tuple []
    | x -> x
    in
    rr ps;
    rr ret;
    rnr (btyp_function (ps,ret))

  | BTYP_cfunction (ps,ret) ->
    rr ps;
    rr ret;
    rnr t

  | BTYP_array (ps,ret) ->
(*
print_endline ("Array type " ^ sbt bsym_table t ^ " base type " ^ sbt bsym_table ps ^ " index type " ^ sbt bsym_table ret);
*)
    begin match ret with
    | BTYP_unitsum 0 | BTYP_void -> syserr sr "Unexpected array length 0"
(*
    | BTYP_unitsum 1 | BTYP_tuple [] -> syserr sr "Unexpected array length 1"
*)
    | BTYP_unitsum _ ->
      rr ps; rr ret; rnr t
    | _ -> rr ps; rr ret; rnr t
    (* | _ -> syserr sr ("Array index type must be unitsum, got " ^ sbt bsym_table ret) *)
    end

  | BTYP_tuple ps -> iter rr ps; rnr t
  | BTYP_tuple_cons (t1,t2) ->  assert false

  | BTYP_record (ps) -> iter (fun (s,t)->rr t) ps; rnr t
  | BTYP_variant ps -> iter (fun (s,t)->rr t) ps; rnr t

  | BTYP_sum ps ->
    (* iter rr ps; *) (* should be driven by constructors *)
    rnr t

  | BTYP_unitsum k -> rnr t
  (* NOTE: pointer type is registered before the type it points
    to because it can be incomplete, whereas the type it
    points to may need a complete pointer type: this
    is always the case for recursion under a pointer
  *)

  (* pointer type is no longer registered, just us t* .. 
     WRONG! What if have a varray of pointers?
   *)

  | BTYP_pointer t' -> rr t'; rnr t

  | BTYP_inst (i,ts)->
(*
print_endline ("Instance type, registering argument ts=" ^ catmap "," (sbt bsym_table) ts);
*)
    iter rr ts;

    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found ->
        failwith ("[register_type_r] Can't find index " ^ string_of_bid i)
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_newtype (vs,r) ->
      let r' = tsubst (Flx_bsym.sr bsym) vs ts r in
      rr r';
      rnr t

    | BBDCL_union (vs,cs) ->
(*
print_endline ("Register type r: union -----------" ^ Flx_bsym.id bsym);
print_endline ("vs len = " ^ si (List.length vs));
print_endline ("ts len = " ^ si (List.length ts));
*)
      let cts = map (fun (_,_,t) -> t) cs in
      let cts = map (tsubst (Flx_bsym.sr bsym) vs ts) cts in
      iter rr cts;
      rnr t

    | BBDCL_cstruct (vs,cs,_)
    | BBDCL_struct (vs,cs) ->
      ui i ts;
      let cts = map snd cs in
      let cts = map (tsubst (Flx_bsym.sr bsym) vs ts) cts in
      iter rr cts;
      rnr t

    | BBDCL_external_type (vs,bquals,_,_)  -> 
     (* instantiate the type too *)
(*
print_endline ("External primitive instance, registering base " ^ si i);
print_endline ("External primitive instance, registering whole type " ^ sbt bsym_table t);
*)
      ui i ts; rnr t;
      begin (* if there is an associated shape required, we have to register the type *)
       let handle_qual bqual = match bqual with
        | `Bound_needs_shape t ->
          (*
          print_endline ("treg: Needs shape (uninstantiated) " ^ sbt bsym_table t);
          *)
          (*
          let varmap = mk_varmap vs ts in
          let t = varmap_subst varmap t in
          *)
          let t' = tsubst (Flx_bsym.sr bsym) vs ts t in
          (*
          print_endline ("treg: Needs shape (instantiated) " ^ sbt bsym_table t');
          *)
          rr t'
        | _ -> ()
        in
        let rec aux quals = match quals with
        | [] -> ()
        | h :: t -> handle_qual h; aux t
        in aux bquals
      end

    | _ ->
      clierr sr
      (
        "[register_type_r] expected type declaration, got " ^
        string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) i
      )
    end

  | BTYP_none
  | BTYP_intersect _

  | BTYP_type _
  | BTYP_type_tuple _
  | BTYP_type_function _
  | BTYP_type_apply _
  | BTYP_type_match _

  | BTYP_type_set _
  | BTYP_type_set_union _
  | BTYP_type_set_intersection _
    ->
    clierr sr
    (
      "Unexpected kind in register type: " ^
      sbt bsym_table t
    )
