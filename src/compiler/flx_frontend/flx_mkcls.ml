(*
This module is responsible for generating functional wrappers for 
constructions which are used as function values requiring
a closure, but which are not functions. For example union constructors
are not functions, but Felix allows them to be used as fuctions
because they take arguments. They can be converted to functions
on demand by simply wrapping them inside a function. In effect
this is lambda lifting done late.

Certain other terms represent functions but are not in the right
form to make a closure, in particular the series composition 
operator is intended to represent a function but needs a wrapper
to effect this.

This module generates wrappers as required and replaces cases of
closures of non-function entities with closures over the generated
wrappers.
*)
open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_unify
open Flx_exceptions
open Flx_use
open Flx_prop

type closure_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  wrappers : (Flx_types.bid_t, Flx_types.bid_t) Hashtbl.t;
}


let make_closure_state syms =
  {
    syms = syms;
    wrappers = Hashtbl.create 97;
  }


let make_inner_function state bsym_table closure_bid sr vs ps =
  (* Make the type of the closed value. *)
  let closed_type = match ps with [t] -> t | ts -> btyp_tuple ts in

  (* Make the closed value that's hidden inside our wrapper function. *)
  let closed_bid = fresh_bid state.syms.counter in
  let closed_name = "_a" ^ string_of_bid closed_bid in
  let closed_val = bbdcl_val (vs,closed_type,`Val) in

  Flx_bsym_table.add bsym_table closed_bid (Some closure_bid)
    (Flx_bsym.create ~sr closed_name closed_val);

  (* Make the type variables of the inner call. *)
  let ts = List.map (fun (_,i) -> btyp_type_var (i,btyp_type 0)) vs in

  (* Make the parameters for the wrapper function. *)
  let param =
    { pkind=`PVal;
      pid=closed_name;
      pindex=closed_bid;
      ptyp=closed_type }
  in

  (* Make the argument that we'll pass to our wrapped function. *)
  let arg = bexpr_name closed_type (closed_bid, ts) in

  (* Return a couple parameters *)
  ts, param, arg



let gen_case_closure_entry state bsym_table sr f at rt =
  let vs = [] in (* HACK, temporary, WRONG *)
  let ts = [] in (* HACK, temporary, WRONG *)
  (* Make a bid for our closure wrapper function. *)
  let closure_bid = fresh_bid state.syms.counter in

  (* Make the wrapper function parameter variable. *)
  let p_bid = fresh_bid state.syms.counter in
  let p_name = "_a" ^ string_of_bid p_bid in
  let p_val = bbdcl_val (vs,at,`Val) in

  (* Make the parameters for the wrapper function. *)
  let param =
    { pkind=`PVal;
      pid=p_name;
      pindex=p_bid;
      ptyp=at}
  in

  (* Make the argument that we'll pass to our wrapped function. *)
  let arg = bexpr_name at (p_bid, []) in

  (* the instructions of the function *)
  let exes =
    let e1 = bexpr_apply rt (f, arg) in
    [ bexe_fun_return (sr, e1) ]
  in

  (* the function record *)
  let bbdcl = bbdcl_fun ([],vs,([param],None),rt,exes) in

  (* the complete symbol *)
  let bsym = Flx_bsym.create ~sr:sr ("_a" ^ string_of_int closure_bid ) (bbdcl) in

  (* now add it to the table *)
  Flx_bsym_table.add bsym_table closure_bid None bsym;

  (* add the parameter afterwards so its parent exists *)
  Flx_bsym_table.add bsym_table p_bid (Some closure_bid)
    (Flx_bsym.create ~sr p_name p_val)
  ;

(* return the index of the wrapper generated *)
  closure_bid,ts
 

let gen_composite_closure_entry state bsym_table sr (f1,t1) (f2,t2) =
  let vs = [] in (* HACK, temporary, WRONG *)
  let ts = [] in (* HACK, temporary, WRONG *)
  let a1t,r1t,a2t,r2t = match t1, t2 with
  | BTYP_function (a1t,r1t), BTYP_function (a2t,r2t) -> a1t,r1t,a2t,r2t
  | _ -> assert false
  in
  (* Make a bid for our closure wrapper function. *)
  let closure_bid = fresh_bid state.syms.counter in

  (* Make the wrapper function parameter variable. *)
  let p_bid = fresh_bid state.syms.counter in
  let p_name = "_a" ^ string_of_bid p_bid in
  let p_val = bbdcl_val (vs,a1t,`Val) in

  (* Make the parameters for the wrapper function. *)
  let param =
    { pkind=`PVal;
      pid=p_name;
      pindex=p_bid;
      ptyp=a1t}
  in

  (* Make the argument that we'll pass to our wrapped function. *)
  let arg = bexpr_name a1t (p_bid, []) in

  (* the instructions of the function *)
  let exes =
    let e1 = bexpr_apply r1t ((f1,t1), arg) in
    let e2 = bexpr_apply r2t ((f2,t2), e1) in
    [ bexe_fun_return (sr, e2) ]
  in

  (* the function record *)
  let bbdcl = bbdcl_fun ([],vs,([param],None),r2t,exes) in

  (* the complete symbol *)
  let bsym = Flx_bsym.create ~sr:sr ("_a" ^ string_of_int closure_bid ) (bbdcl) in

  (* now add it to the table *)
  Flx_bsym_table.add bsym_table closure_bid None bsym;

  (* add the parameter afterwards so its parent exists *)
  Flx_bsym_table.add bsym_table p_bid (Some closure_bid)
    (Flx_bsym.create ~sr p_name p_val)
  ;

(* return the index of the wrapper generated *)
  closure_bid,ts
 

(** This generates closures for calling external functions. It does this by
 * generating a new function that contains part of the closed values. *)
let gen_closure state bsym_table bid t =
  let bsym_parent, bsym = Flx_bsym_table.find_with_parent bsym_table bid in

  (* Make a bid for our closure wrapper function. *)
  let closure_bid = fresh_bid state.syms.counter in

(*
print_endline ("Generating closure "^ string_of_int closure_bid ^ " for " ^ string_of_int bid);
*)
  (* Add the closure wrapper to symbol table. We'll replace it later with the
   * real values. *)
  Flx_bsym_table.add bsym_table closure_bid bsym_parent (Flx_bsym.create
    ~sr:(Flx_bsym.sr bsym)
    ("_a" ^ string_of_int closure_bid ^ "_" ^ Flx_bsym.id bsym)
    (bbdcl_invalid ()));

  let make_inner_function = make_inner_function
    state
    bsym_table
    closure_bid
    (Flx_bsym.sr bsym)
  in

  let bbdcl =
    match Flx_bsym.bbdcl bsym with
    | BBDCL_external_fun (_,vs,ps,ret,_,_,_) ->
        let ts, param, arg = make_inner_function vs ps in

        (* Generate a call to the wrapped function. *)
        let exes =
          match ret with
          | BTYP_void ->
              [ bexe_call_prim (Flx_bsym.sr bsym, bid, ts, arg);
                bexe_proc_return (Flx_bsym.sr bsym) ]
          | _ ->
              let e = bexpr_apply_prim ret (bid, ts, arg) in
              [ bexe_fun_return (Flx_bsym.sr bsym, e) ]
        in

        bbdcl_fun ([],vs,([param],None),ret,exes)

    | BBDCL_struct (vs,ps)
    | BBDCL_cstruct (vs,ps,_) ->
        let ts, param, arg = make_inner_function vs (List.map snd ps) in

        (* Generate a call to the wrapped function. *)
        let e = bexpr_apply_struct t (bid, ts, arg) in
        let exes = [bexe_fun_return (Flx_bsym.sr bsym, e)] in

        bbdcl_fun ([],vs,([param],None),btyp_inst (bid,[]),exes)

    | BBDCL_nonconst_ctor (vs,_,ret,_,p,_,_) as foo ->
        let ts, param, arg = make_inner_function vs [p] in

        (* Generate a call to the wrapped function. *)
        let e = bexpr_apply_struct ret (bid, ts, arg) in
        let exes = [bexe_fun_return (Flx_bsym.sr bsym, e)] in

        bbdcl_fun ([],vs,([param],None),ret,exes)

    | _ -> assert false
  in

  (* Finally, replace our wrapper temp bbdcl with our new bbdcl. *)
  Flx_bsym_table.update_bbdcl bsym_table closure_bid bbdcl;

  (* Return the wrapper. *)
  closure_bid

let mkcls state bsym_table all_closures i ts t =
  let j =
    try Hashtbl.find state.wrappers i
    with Not_found ->
      let j = gen_closure state bsym_table i t in
      Hashtbl.add state.wrappers i j;
      j
  in
    all_closures := BidSet.add j !all_closures;
    bexpr_closure t (j,ts)

let check_prim state bsym_table all_closures i ts t =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_external_fun (_,_,_,_,_,_,`Callback _) ->
(*
print_endline ("mlcls: WARNING: NOT replacing use of callback function "^string_of_int i^" with wrapper ");
*)
   bexpr_closure t (i,ts)
(*
    let cls = mkcls state bsym_table all_closures i ts t in
    begin match cls with
    | BEXPR_closure (j,ts2),_ ->
print_endline ("mlcls: WARNING: NOT replacing use of callback function "^string_of_int i^" with wrapper " ^ string_of_int j);
    | _ -> assert false
    end
    ;
    cls
*)

  | BBDCL_external_fun _
  | BBDCL_struct _
  | BBDCL_cstruct _
  | BBDCL_nonconst_ctor _ ->
      mkcls state bsym_table all_closures i ts t

  | x ->
      all_closures := BidSet.add i !all_closures;
      bexpr_closure t (i,ts)


(* processes closures *)
let rec adj_cls state bsym_table all_closures sr e =
  let adj e = adj_cls state bsym_table all_closures sr e in
  match Flx_bexpr.map ~f_bexpr:adj e with
  | BEXPR_closure (i,ts),t ->
      check_prim state bsym_table all_closures i ts t

  | BEXPR_apply_direct (i,ts,a),t as x ->
      (* Direct calls to non-stacked functions require heap but not a clone. *)
      all_closures := BidSet.add i !all_closures;
      x

  | x -> x

(* processes explicit lambda terms *)
let rec adj_lambda state bsym_table all_closures sr e =
  let adj e = adj_lambda state bsym_table all_closures sr e in
  match Flx_bexpr.map ~f_bexpr:adj e with
  | BEXPR_compose (f1,f2),t ->
      let i,ts = gen_composite_closure_entry state bsym_table sr f1 f2 in
      all_closures := BidSet.add i !all_closures;
      Flx_bexpr.bexpr_closure t (i,ts)

  (* THIS SHOULDN'T HAPPEN NOW *)
  | BEXPR_case (v,d),ft as x ->
      begin match unfold d with
      | t when Flx_btype.islinear_type bsym_table t -> x (* ??? *)

      | BTYP_sum ls ->
          let n = List.length ls in
          if v < 0 or v >= n
          then
            failwith
            (
              "Invalid case index " ^ si v ^
              " of " ^ si n ^ " cases"
            )
          else let c = List.nth ls v in
          if c = btyp_tuple []
          then x
          else begin
            print_endline ("Deprecated closure of BEXE_case " ^ si v ^ 
             " domain type=" ^ sbt bsym_table d ^ 
             " codomain type= " ^ sbt bsym_table c ^
             " term function type= " ^ sbt bsym_table ft
             );
            assert (ft = BTYP_function (d,c));
            let i,ts = gen_case_closure_entry state bsym_table sr x d c in
            all_closures := BidSet.add i !all_closures;
            Flx_bexpr.bexpr_closure ft (i,ts)
         end

      | _ -> failwith ("flx_mkcls: Unexpected case of non sum")
      end
  | (BEXPR_inj (_,d,c),t as x)
  | (BEXPR_prj (_,d,c),t as x)
  | (BEXPR_aprj (_,d,c),t as x)
     ->
      let i,ts = gen_case_closure_entry state bsym_table sr x d c in
      all_closures := BidSet.add i !all_closures;
      Flx_bexpr.bexpr_closure t (i,ts)


  | x -> x


let process_exe ue state bsym_table all_closures exe =
  let ue sr e = ue state bsym_table all_closures sr e in
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2) -> bexe_call_prim (sr,i,ts, ue sr e2)

  | BEXE_call_direct (sr,i,ts,e2) ->
    all_closures := BidSet.add i !all_closures;
    bexe_call_direct (sr,i,ts, ue sr e2)

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    all_closures := BidSet.add i !all_closures;
    bexe_jump_direct (sr,i,ts, ue sr e2)

  | BEXE_call_stack (sr,i,ts,e2)  ->
    (* stack calls do use closures -- but not heap allocated ones *)
    bexe_call_stack (sr,i,ts, ue sr e2)

  | BEXE_call (sr,e1,e2) -> bexe_call (sr,ue sr e1, ue sr e2)
  | BEXE_jump (sr,e1,e2) -> bexe_jump (sr,ue sr e1, ue sr e2)

  | BEXE_ifgoto (sr,e,l) -> bexe_ifgoto (sr, ue sr e,l)
  | BEXE_fun_return (sr,e) -> bexe_fun_return (sr,ue sr e)
  | BEXE_yield (sr,e) -> bexe_yield (sr,ue sr e)

  | BEXE_init (sr,i,e) -> bexe_init (sr,i,ue sr e)
  | BEXE_assign (sr,e1,e2) -> bexe_assign (sr, ue sr e1, ue sr e2)
  | BEXE_assert (sr,e) -> bexe_assert (sr, ue sr e)
  | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ue sr e) | None -> None in
    bexe_axiom_check2 (sr, sr2,e1,ue sr e2)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e -> Some (ue sr e) | None -> None in
    bexe_assert2 (sr, sr2,e1,ue sr e2)

  | BEXE_svc (sr,i) -> exe

  | BEXE_catch _ 
  | BEXE_try _
  | BEXE_endtry  _
  | BEXE_label _
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_goto _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_proc_return _
  | BEXE_begin
  | BEXE_end
    -> exe

let process_exes ue state bsym_table all_closures exes =
  List.map (process_exe ue state bsym_table all_closures) exes

let process_entry ue state bsym_table all_closures i =
  let bsym = Flx_bsym_table.find bsym_table i in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,exes) ->
    let exes = process_exes ue state bsym_table all_closures exes in
    let bbdcl = bbdcl_fun (props,vs,ps,ret,exes) in
    Flx_bsym_table.update_bbdcl bsym_table i bbdcl

  | _ -> ()

(* NOTE: before monomorphisation, we can't tell if a
  typeclass method will dispatch to a C function
  or a Felix function .. so we have to mark all typeclass
  methods and probably instances as requiring a closure ..

  This is overkill and will defeat some optimisations ..
  needs to be fixed. .. Ouch .. this is too late,
  enstack has already run .. won't affect enstack.
*)

let set_closure bsym_table i = add_prop bsym_table `Heap_closure i

let make_closures state bsym_table =
  let ue = adj_cls in
  let all_closures = ref BidSet.empty in
  let used = full_use_closure state.syms bsym_table in
  BidSet.iter (process_entry ue state bsym_table all_closures) used;
  BidSet.iter (set_closure bsym_table) !all_closures

(* this make a set of closures, but doesn't mark heap usage.
 * It is used before inlining to get rid of any lambdas such
 * as composition in the code .. 
 * expensive, since we do this for functions that never get called
 *)
let premake_closures (syms:Flx_mtypes2.sym_state_t) bsym_table =
  let ue = adj_lambda in
  let state = make_closure_state syms in
  let all_closures = ref BidSet.empty in
  let used = full_use_closure state.syms bsym_table in
  BidSet.iter (process_entry ue state bsym_table all_closures) used

