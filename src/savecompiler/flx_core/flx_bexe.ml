open Flx_ast
open Flx_types
open Flx_bid

type t =
  | BEXE_label of Flx_srcref.t * bid_t
  | BEXE_comment of Flx_srcref.t * string (* for documenting generated code *)
  | BEXE_halt of Flx_srcref.t * string  (* for internal use only *)
  | BEXE_trace of Flx_srcref.t * string * string  (* for internal use only *)
  | BEXE_goto of Flx_srcref.t * bid_t  (* for internal use only *)
  | BEXE_cgoto of Flx_srcref.t * Flx_bexpr.t (* computed goto *)
  | BEXE_ifgoto of Flx_srcref.t * Flx_bexpr.t * bid_t (* for internal use only *)
  | BEXE_ifcgoto of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_call of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_call_with_trap of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_call_direct of Flx_srcref.t * bid_t * Flx_btype.t list * Flx_bexpr.t
  | BEXE_call_stack of Flx_srcref.t * bid_t * Flx_btype.t list * Flx_bexpr.t
  | BEXE_call_prim of Flx_srcref.t * bid_t * Flx_btype.t list * Flx_bexpr.t
  | BEXE_jump of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_jump_direct of Flx_srcref.t * bid_t * Flx_btype.t list * Flx_bexpr.t
  | BEXE_svc of Flx_srcref.t * bid_t
  | BEXE_fun_return of Flx_srcref.t * Flx_bexpr.t
  | BEXE_yield of Flx_srcref.t * Flx_bexpr.t
  | BEXE_proc_return of Flx_srcref.t
  | BEXE_nop of Flx_srcref.t * string
  | BEXE_code of Flx_srcref.t * Flx_code_spec.t * Flx_bexpr.t
  | BEXE_nonreturn_code of Flx_srcref.t * Flx_code_spec.t * Flx_bexpr.t

  | BEXE_assign of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_init of Flx_srcref.t * bid_t * Flx_bexpr.t
  | BEXE_storeat of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t

  | BEXE_begin
  | BEXE_end
  | BEXE_assert of Flx_srcref.t * Flx_bexpr.t
  | BEXE_assert2 of Flx_srcref.t * Flx_srcref.t * Flx_bexpr.t option * Flx_bexpr.t
  | BEXE_axiom_check of Flx_srcref.t * Flx_bexpr.t
  | BEXE_axiom_check2 of Flx_srcref.t * Flx_srcref.t * Flx_bexpr.t option * Flx_bexpr.t

  | BEXE_try of Flx_srcref.t
  | BEXE_endtry of Flx_srcref.t
  | BEXE_catch of Flx_srcref.t * string * Flx_btype.t 

(* -------------------------------------------------------------------------- *)

let bexe_label (sr,i) = BEXE_label (sr,i)
let bexe_comment (sr,s) = BEXE_comment (sr,s)
let bexe_halt (sr,s) = BEXE_halt (sr,s)
let bexe_trace (sr,s1,s2) = BEXE_trace (sr,s1,s2)
let bexe_goto (sr,i) = BEXE_goto (sr,i)
let bexe_cgoto (sr,e) = BEXE_cgoto (sr,e)
let bexe_ifcgoto (sr,e1,e2) = BEXE_ifcgoto (sr,e1,e2)
let bexe_ifgoto (sr,e,i) = BEXE_ifgoto (sr,e,i)
let bexe_call (sr,e1,e2) = BEXE_call (sr,e1,e2)
let bexe_call_with_trap (sr,e1,e2) = BEXE_call_with_trap (sr,e1,e2)
let bexe_call_direct (sr,bid,ts,e) = BEXE_call_direct (sr,bid,ts,e)
let bexe_call_stack (sr,bid,ts,e) = BEXE_call_stack (sr,bid,ts,e)
let bexe_call_prim (sr,bid,ts,e) = BEXE_call_prim (sr,bid,ts,e)
let bexe_jump (sr,e1,e2) = BEXE_jump (sr,e1,e2)
let bexe_jump_direct (sr,bid,ts,e) = BEXE_jump_direct (sr,bid,ts,e)
let bexe_svc (sr,bid) = BEXE_svc (sr,bid)
let bexe_fun_return (sr,e) = BEXE_fun_return (sr,e)
let bexe_yield (sr,e) = BEXE_yield (sr,e)
let bexe_proc_return sr = BEXE_proc_return sr
let bexe_nop (sr,s) = BEXE_nop (sr,s)
let bexe_code (sr,code,e) = BEXE_code (sr,code,e)
let bexe_nonreturn_code (sr,code,e) = BEXE_nonreturn_code (sr,code,e)
let bexe_assign (sr,e1,e2) = 
  begin match e1 with
  | Flx_bexpr.BEXPR_varname _, _ -> ()
  | _ -> print_endline ("Warning, assign to non-variable")
  end;
  BEXE_assign (sr,e1,e2)

let bexe_init (sr,bid,e) = BEXE_init (sr,bid,e)

let bexe_storeat (sr,((_,pt) as e1),e2) = 
  match Flx_btype.trivorder pt with
  | Some _ -> bexe_nop (sr,"Trivassign elided") 
  | _ -> BEXE_storeat (sr,e1,e2)

let bexe_begin () = BEXE_begin
let bexe_end () = BEXE_end
let bexe_assert (sr,e) = BEXE_assert (sr,e)
let bexe_assert2 (sr1,sr2,e1,e2) = BEXE_assert2 (sr1,sr2,e1,e2)
let bexe_axiom_check (sr,e) = BEXE_axiom_check (sr,e)
let bexe_axiom_check2 (sr1,sr2,e1,e2) = BEXE_axiom_check2 (sr1,sr2,e1,e2)

let bexe_try sr = BEXE_try sr
let bexe_endtry sr = BEXE_endtry sr
let bexe_catch sr s t  = BEXE_catch (sr,s, t)

(* -------------------------------------------------------------------------- *)

(** Extract the source of the bound executable. *)
let get_srcref = function
  | BEXE_goto (sr,_)
  | BEXE_cgoto (sr,_)
  | BEXE_ifcgoto (sr,_,_)
  | BEXE_assert (sr,_)
  | BEXE_assert2 (sr,_,_,_)
  | BEXE_axiom_check2 (sr,_,_,_)
  | BEXE_axiom_check (sr,_)
  | BEXE_halt (sr,_)
  | BEXE_trace (sr,_,_)
  | BEXE_ifgoto (sr,_,_)
  | BEXE_label (sr,_)
  | BEXE_comment (sr,_)
  | BEXE_call (sr,_,_)
  | BEXE_call_with_trap (sr,_,_)
  | BEXE_call_direct (sr,_,_,_)
  | BEXE_jump_direct (sr,_,_,_)
  | BEXE_call_stack (sr,_,_,_)
  | BEXE_call_prim (sr,_,_,_)
  | BEXE_jump (sr,_,_)
  | BEXE_svc (sr,_)
  | BEXE_fun_return (sr,_)
  | BEXE_yield (sr,_)
  | BEXE_proc_return sr
  | BEXE_nop (sr,_)
  | BEXE_code (sr,_,_)
  | BEXE_nonreturn_code (sr,_,_)
  | BEXE_assign (sr,_,_)
  | BEXE_init (sr,_,_) -> sr
  | BEXE_storeat (sr,_,_) -> sr
  | BEXE_begin
  | BEXE_end -> Flx_srcref.dummy_sr

  | BEXE_try sr -> sr
  | BEXE_catch (sr,_,_) -> sr
  | BEXE_endtry sr -> sr

(* -------------------------------------------------------------------------- *)

(** Returns whether or not this executable is terminating. *)
let is_terminating = function
  | BEXE_halt _ 
  | BEXE_fun_return _
  | BEXE_proc_return _
  | BEXE_nonreturn_code _ -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)

(** iterate over each bound exe and call the function on it. *)
let iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
  ?(f_label_use=fun _ ->())
  ?(f_label_def=fun _ ->())
  exe
=
  match exe with
  | BEXE_call_prim (sr,i,ts,e2)
  | BEXE_call_stack (sr,i,ts,e2)
  | BEXE_call_direct (sr,i,ts,e2)
  | BEXE_jump_direct (sr,i,ts,e2) ->
      f_bid i;
      List.iter f_btype ts;
      f_bexpr e2
  | BEXE_assign (sr,e1,e2)
  | BEXE_storeat (sr,e1,e2)
  | BEXE_call (sr,e1,e2)
  | BEXE_call_with_trap (sr,e1,e2)
  | BEXE_jump (sr,e1,e2) ->
      f_bexpr e1;
      f_bexpr e2
  | BEXE_ifgoto (sr,e,idx) ->
      f_bid idx;
      f_label_use idx;
      f_bexpr e
  | BEXE_label (sr,idx) -> f_label_def idx; f_bid idx
  | BEXE_goto (sr,idx) -> f_label_use idx; f_bid idx
  | BEXE_ifcgoto (sr,e1,e2) -> f_bexpr e2; f_bexpr e2
  | BEXE_cgoto (sr,e) -> f_bexpr e
  | BEXE_fun_return (sr,e) -> f_bexpr e
  | BEXE_yield (sr,e) -> f_bexpr e
  | BEXE_axiom_check (_,e) -> f_bexpr e
  | BEXE_assert2 (_,_,e1,e2) 
  | BEXE_axiom_check2 (_,_,e1,e2) ->
      (match e1 with Some e -> f_bexpr e | None->());
      f_bexpr e2

  | BEXE_code (_,_,e) 
  | BEXE_nonreturn_code (_,_,e)
  | BEXE_assert (_,e) -> f_bexpr e

  | BEXE_init (sr,i,e) -> f_bid i; f_bexpr e
  | BEXE_svc (sr,i) -> f_bid i
  | BEXE_catch (_, s, t) -> f_btype t


  | BEXE_try _
  | BEXE_endtry _
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_begin
  | BEXE_end -> ()

let map
  ?(f_bid=fun i -> i)
  ?(f_btype=fun t -> t)
  ?(f_bexpr=fun e -> e)
  exe
=
  match exe with
  | BEXE_call_prim (sr,i,ts,e2)  ->
      BEXE_call_prim (sr,f_bid i,List.map f_btype ts,f_bexpr e2)
  | BEXE_call_stack (sr,i,ts,e2) ->
      BEXE_call_stack (sr,f_bid i,List.map f_btype ts,f_bexpr e2)
  | BEXE_call_direct (sr,i,ts,e2) ->
      BEXE_call_direct (sr,f_bid i,List.map f_btype ts,f_bexpr e2)
  | BEXE_jump_direct (sr,i,ts,e2) ->
      BEXE_jump_direct (sr,f_bid i,List.map f_btype ts,f_bexpr e2)
  | BEXE_assign (sr,e1,e2) -> BEXE_assign (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_storeat(sr,e1,e2) -> BEXE_storeat(sr,f_bexpr e1,f_bexpr e2)
  | BEXE_call (sr,e1,e2) -> BEXE_call (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_call_with_trap (sr,e1,e2) -> BEXE_call_with_trap (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_jump (sr,e1,e2) -> BEXE_jump (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_ifgoto (sr,e,idx) -> BEXE_ifgoto (sr,f_bexpr e, f_bid idx)
  | BEXE_label (sr,idx) -> BEXE_label (sr, f_bid idx)
  | BEXE_goto (sr,idx) -> BEXE_goto (sr, f_bid idx)
  | BEXE_cgoto (sr,e) -> BEXE_cgoto (sr, f_bexpr e)
  | BEXE_ifcgoto (sr,e1,e2) -> BEXE_ifcgoto (sr, f_bexpr e1, f_bexpr e2)
  | BEXE_fun_return (sr,e) -> BEXE_fun_return (sr,f_bexpr e)
  | BEXE_yield (sr,e) -> BEXE_yield (sr,f_bexpr e)
  | BEXE_assert (sr,e) -> BEXE_assert (sr,f_bexpr e)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
      let e1 = match e1 with Some e1 -> Some (f_bexpr e1) | None -> None in
      BEXE_assert2 (sr,sr2,e1,f_bexpr e2)
  | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
      let e1 = match e1 with Some e1 -> Some (f_bexpr e1) | None -> None in
      BEXE_axiom_check2 (sr,sr2,e1,f_bexpr e2)
  | BEXE_axiom_check (sr,e) -> BEXE_axiom_check (sr,f_bexpr e)
  | BEXE_init (sr,i,e) -> BEXE_init (sr,f_bid i,f_bexpr e)
  | BEXE_svc (sr,i) -> BEXE_svc (sr,f_bid i)
  | BEXE_catch (sr,s,t) -> BEXE_catch (sr, s, f_btype t)

  | BEXE_code (sr,s,e) -> BEXE_code (sr,s, f_bexpr e)
  | BEXE_nonreturn_code (sr,s,e) -> BEXE_nonreturn_code (sr,s,f_bexpr e)

  | BEXE_halt _
  | BEXE_trace _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_try _
  | BEXE_endtry _
  | BEXE_begin
  | BEXE_end -> exe

(* -------------------------------------------------------------------------- *)

(** Simplify the bound exe. *)
let reduce exe =
  match map ~f_bexpr:Flx_bexpr.reduce exe with
  (*
  | BEXE_call (sr,(Flx_bexpr.BEXPR_closure (i,ts),_),a) ->
      BEXE_call_direct (sr,i,ts,a)
  *)
  | x -> x



