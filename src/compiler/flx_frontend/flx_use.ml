open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions


(* These routines find the absolute use closure of a symbol,
in particular they include variables which are initialised
but never used: these routine are intended to be used
to extract all the bound symbol table entries required
to process a set of roots.

Contrast with the 'Flx_call' usage routines, which
find some symbols which are useful, this excludes
types, and it excludes LHS vals and perhaps vars,
which are not used in some expression.

It seems a pity these routines are almost identical
(and the lot gets repeated yet again in the instantiator,
and weakly in the 'useless call eliminator', we hope
to find a better code reuse solution.. for now,
remember to update all three sets of routines when
changing the data structures.

*)

let nop x = ()

let rec uses_type syms used bsym_table count_inits (t:btypecode_t) =
  let ut t = uses_type syms used bsym_table count_inits t in
  match t with
  | BTYP_inst (i,ts)
    ->
      uses syms used bsym_table count_inits i; (* don't care on uses inits? *)
      List.iter ut ts

  (*
  | BTYP_type
    ->
      failwith "[uses_type] Unexpected metatype"
  *)

  | _ -> iter_btype ut t

and uses_exes syms used bsym_table count_inits exes =
  List.iter (uses_exe syms used bsym_table count_inits) exes

and uses_exe syms used bsym_table count_inits (exe:bexe_t) =
  (*
  print_endline ("EXE=" ^ string_of_bexe syms.sym_table 0 exe);
  *)
  (* check is a term is a tuple projection of a variable *)
  let rec is_proj e = match e with
    | BEXPR_name _,_ -> true
    | BEXPR_get_n (_,e),_ -> is_proj e
    | _ -> false
  in
  let ue e = uses_tbexpr syms used bsym_table count_inits e in
  let ui i = uses syms used bsym_table count_inits i in
  let ut t = uses_type syms used bsym_table count_inits t in
  match exe,count_inits with
  | BEXE_init (_,i,e),false -> ue e
  | BEXE_assign (_,lhs,rhs),_ ->
     if count_inits or not (is_proj lhs)
     then ue lhs;
     ue rhs
  | _ ->
    iter_bexe ui ue ut nop nop exe

and uses_tbexpr syms used bsym_table count_inits ((e,t) as x) =
  let ue e = uses_tbexpr syms used bsym_table count_inits e in
  let ut t = uses_type syms used bsym_table count_inits t in
  let ui i = uses syms used bsym_table count_inits i in

  (* already done in the iter .. *)
  (*
  ut t;
  *)
  (* use a MAP now *)
  iter_tbexpr ui ignore ut x;

and uses_production syms used bsym_table count_inits p =
  let uses_symbol (_,nt) = match nt with
  | `Nonterm ii -> List.iter (uses syms used bsym_table count_inits) ii
  | `Term i -> () (* HACK! This is a union constructor name  we need to 'use' the union type!! *)
  in
  List.iter uses_symbol p

and faulty_req syms i =
  match Hashtbl.find syms.sym_table i with {id=id; sr=sr } ->
  clierr sr (id ^ " is used but has unsatisfied requirement")

and uses syms used bsym_table count_inits i =
  let ui i = uses syms used bsym_table count_inits i in
  let ut t = uses_type syms used bsym_table count_inits t in
  let rq reqs =
    let ur (j,ts) =
      if j = 0 then
        faulty_req syms i
      else begin ui j; List.iter ut ts end
    in
    List.iter ur reqs
  in
  let ux x = uses_exes syms used bsym_table count_inits x in
  let ue e = uses_tbexpr syms used bsym_table count_inits e in
  if not (BidSet.mem i !used) then
  begin
    match Flx_hashtbl.find bsym_table i with
    | Some (id,_,_,bbdcl) ->
      used := BidSet.add i !used;
      begin match bbdcl with
      | BBDCL_typeclass _ -> ()

      | BBDCL_instance (_,_,con,i,ts) ->
        ut con;
        List.iter ut ts

      | BBDCL_function (props,_,(ps,traint),ret,exes) ->
        List.iter (fun {pindex=i;ptyp=t} -> ui i; ut t) ps;
        ut ret;
        ux exes

      | BBDCL_procedure (props,_,(ps,traint), exes) ->
        List.iter (fun {pindex=i;ptyp=t} -> ui i; ut t) ps;
        ux exes

      | BBDCL_union (_,ps)
        -> ()

        (* types of variant arguments are only used if constructed
          .. OR ..  matched against ??
        *)

      | BBDCL_cstruct (_,ps)
      | BBDCL_struct (_,ps) ->
        List.iter ut (List.map snd ps)

      | BBDCL_val (_,t)
      | BBDCL_var (_,t)
      | BBDCL_tmp (_,t) -> ut t

      | BBDCL_ref (_,t) -> ut (BTYP_pointer t)

      | BBDCL_const (_,_,t,_,reqs) -> ut t; rq reqs
      | BBDCL_fun (_,_,ps, ret, _,reqs,_) -> List.iter ut ps; ut ret; rq reqs

      | BBDCL_callback (_,_,ps_cf, ps_c, _, ret, reqs,_) ->
        List.iter ut ps_cf;
        List.iter ut ps_c;
        ut ret; rq reqs

      | BBDCL_proc (_,_,ps, _, reqs)  -> List.iter ut ps; rq reqs

      | BBDCL_newtype (_,t) -> ut t
      | BBDCL_abs (_,_,_,reqs) -> rq reqs
      | BBDCL_insert (_,s,ikind,reqs)  -> rq reqs
      | BBDCL_nonconst_ctor (_,_,unt,_,ct,evs, etraint) ->
        ut unt; ut ct

      end
    | None ->
      let id =
        try match Hashtbl.find syms.sym_table i with {id=id} -> id
        with Not_found -> "not found in unbound symbol table"
      in
      failwith
      (
        "[Flx_use.uses] Cannot find bound defn for " ^ id ^ "<"^si i ^ ">"
      )
  end

let find_roots syms bsym_table
  (root:bid_t)
  (bifaces:biface_t list)
=
  (* make a list of the root and all exported functions,
  add exported types and components thereof into the used
  set now too
  *)
  let roots = ref (BidSet.singleton root) in

  List.iter begin function
  | BIFACE_export_python_fun (_,x,_)
  | BIFACE_export_fun (_,x,_) -> roots := BidSet.add x !roots
  | BIFACE_export_type (_,t,_) -> uses_type syms roots bsym_table true t
  end bifaces;

  syms.roots := !roots

let cal_use_closure_for_symbols syms bsym_table bids (count_inits:bool) =
  let u = ref BidSet.empty in
  let v : BidSet.t = !(syms.roots) in
  let v = ref v in

  let add j =
    if not (BidSet.mem j !u) then
    begin
       (*
       print_endline ("Scanning " ^ si j);
       *)
       u := BidSet.add j !u;
       uses syms v bsym_table count_inits j
    end
  in
  let ut t = uses_type syms u bsym_table count_inits t in

  List.iter begin fun bid ->
    match Flx_hashtbl.find syms.typeclass_to_instance bid with
    | Some instances ->
        List.iter begin fun (vs, con, st, j) ->
          add bid;
          add j;
          ut con;
        end instances
    | None -> ()
  end bids;

  !u

let full_use_closure_for_symbols syms bsym_table bids =
  cal_use_closure_for_symbols syms bsym_table bids true

let cal_use_closure syms bsym_table (count_inits:bool) =
  let u = ref BidSet.empty in
  let v : BidSet.t = !(syms.roots) in
  let v = ref v in

  let add j =
    if not (BidSet.mem j !u) then
    begin
       (*
       print_endline ("Scanning " ^ si j);
       *)
       u:= BidSet.add j !u;
       uses syms v bsym_table count_inits j
    end
  in
  let ut t = uses_type syms u bsym_table count_inits t in

  Hashtbl.iter begin fun i entries ->
    List.iter begin fun (vs,con,ts,j) ->
      add i; add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.typeclass_to_instance;

  while not (BidSet.is_empty !v) do
    let j = BidSet.choose !v in
    v := BidSet.remove j !v;
    add j
  done
  ;
  !u

let full_use_closure syms bsym_table =
  cal_use_closure syms bsym_table true

let copy_used syms bsym_table =
  if syms.compiler_options.print_flag then
    print_endline "COPY USED";

  let h = Hashtbl.create 97 in
  let u = full_use_closure syms bsym_table in

  (* Iterate through the used symbols and copy them to the new table. *)
  BidSet.iter begin fun i ->
    (*
    if syms.compiler_options.print_flag then
      print_endline ("Copying " ^ si i);
    *)
    Hashtbl.add h i (Hashtbl.find bsym_table i)
  end u;

  h
