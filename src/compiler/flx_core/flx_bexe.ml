open Flx_ast
open Flx_types
open Flx_format

type t =
  | BEXE_label of Flx_srcref.t * string
  | BEXE_comment of Flx_srcref.t * string (* for documenting generated code *)
  | BEXE_halt of Flx_srcref.t * string  (* for internal use only *)
  | BEXE_trace of Flx_srcref.t * string * string  (* for internal use only *)
  | BEXE_goto of Flx_srcref.t * string  (* for internal use only *)
  | BEXE_ifgoto of Flx_srcref.t * Flx_bexpr.t * string  (* for internal use only *)
  | BEXE_call of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
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
  | BEXE_code of Flx_srcref.t * code_spec_t
  | BEXE_nonreturn_code of Flx_srcref.t * code_spec_t
  | BEXE_assign of Flx_srcref.t * Flx_bexpr.t * Flx_bexpr.t
  | BEXE_init of Flx_srcref.t * bid_t * Flx_bexpr.t
  | BEXE_begin
  | BEXE_end
  | BEXE_assert of Flx_srcref.t * Flx_bexpr.t
  | BEXE_assert2 of Flx_srcref.t * Flx_srcref.t * Flx_bexpr.t option * Flx_bexpr.t
  | BEXE_axiom_check of Flx_srcref.t * Flx_bexpr.t

(* -------------------------------------------------------------------------- *)

let bexe_label (sr,s) = BEXE_label (sr,s)
let bexe_comment (sr,s) = BEXE_comment (sr,s)
let bexe_halt (sr,s) = BEXE_halt (sr,s)
let bexe_trace (sr,s1,s2) = BEXE_trace (sr,s1,s2)
let bexe_goto (sr,s) = BEXE_goto (sr,s)
let bexe_ifgoto (sr,e,s) = BEXE_ifgoto (sr,e,s)
let bexe_call (sr,e1,e2) = BEXE_call (sr,e1,e2)
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
let bexe_code (sr,code) = BEXE_code (sr,code)
let bexe_nonreturn_code (sr,code) = BEXE_nonreturn_code (sr,code)
let bexe_assign (sr,e1,e2) = BEXE_assign (sr,e1,e2)
let bexe_init (sr,bid,e) = BEXE_init (sr,bid,e)
let bexe_begin () = BEXE_begin
let bexe_end () = BEXE_end
let bexe_assert (sr,e) = BEXE_assert (sr,e)
let bexe_assert2 (sr1,sr2,e1,e2) = BEXE_assert2 (sr1,sr2,e1,e2)
let bexe_axiom_check (sr,e) = BEXE_axiom_check (sr,e)

(* -------------------------------------------------------------------------- *)

(** Extract the source of the bound executable. *)
let get_srcref = function
  | BEXE_goto (sr,_)
  | BEXE_assert (sr,_)
  | BEXE_assert2 (sr,_,_,_)
  | BEXE_axiom_check (sr,_)
  | BEXE_halt (sr,_)
  | BEXE_trace (sr,_,_)
  | BEXE_ifgoto (sr,_,_)
  | BEXE_label (sr,_)
  | BEXE_comment (sr,_)
  | BEXE_call (sr,_,_)
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
  | BEXE_code (sr,_)
  | BEXE_nonreturn_code (sr,_)
  | BEXE_assign (sr,_,_)
  | BEXE_init (sr,_,_) -> sr
  | BEXE_begin
  | BEXE_end -> Flx_srcref.dummy_sr

(* -------------------------------------------------------------------------- *)

(** Returns whether or not this executable is terminating. *)
let is_terminating = function
  | BEXE_halt _ 
  | BEXE_fun_return _
  | BEXE_proc_return _
  | BEXE_nonreturn_code _ -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)

(** Recursively iterate over each bound exe and call the function on it. *)
let iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  ?(f_bexpr=fun _ -> ())
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
  | BEXE_call (sr,e1,e2)
  | BEXE_jump (sr,e1,e2) ->
      f_bexpr e1;
      f_bexpr e2
  | BEXE_ifgoto (sr,e,lab) ->
      f_bexpr e
  | BEXE_label (sr,lab)
  | BEXE_goto (sr,lab) -> ()
  | BEXE_fun_return (sr,e) -> f_bexpr e
  | BEXE_yield (sr,e) -> f_bexpr e
  | BEXE_axiom_check (_,e) -> f_bexpr e
  | BEXE_assert2 (_,_,e1,e2) ->
      (match e1 with Some e -> f_bexpr e | None->());
      f_bexpr e2
  | BEXE_assert (_,e) -> f_bexpr e
  | BEXE_init (sr,i,e) -> f_bid i; f_bexpr e
  | BEXE_svc (sr,i) -> f_bid i
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_begin
  | BEXE_end -> ()

(** Recursively iterate over each bound type and transform it with the
 * function. *)
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
  | BEXE_call (sr,e1,e2) -> BEXE_call (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_jump (sr,e1,e2) -> BEXE_jump (sr,f_bexpr e1,f_bexpr e2)
  | BEXE_ifgoto (sr,e,lab) -> BEXE_ifgoto (sr,f_bexpr e,lab)
  | BEXE_label (sr,lab) -> exe
  | BEXE_goto (sr,lab) -> exe
  | BEXE_fun_return (sr,e) -> BEXE_fun_return (sr,f_bexpr e)
  | BEXE_yield (sr,e) -> BEXE_yield (sr,f_bexpr e)
  | BEXE_assert (sr,e) -> BEXE_assert (sr,f_bexpr e)
  | BEXE_assert2 (sr,sr2,e1,e2) ->
      let e1 = match e1 with Some e1 -> Some (f_bexpr e1) | None -> None in
      BEXE_assert2 (sr,sr2,e1,f_bexpr e2)
  | BEXE_axiom_check (sr,e) -> BEXE_axiom_check (sr,f_bexpr e)
  | BEXE_init (sr,i,e) -> BEXE_init (sr,f_bid i,f_bexpr e)
  | BEXE_svc (sr,i) -> BEXE_svc (sr,f_bid i)
  | BEXE_halt _
  | BEXE_trace _
  | BEXE_code _
  | BEXE_nonreturn_code _
  | BEXE_proc_return _
  | BEXE_comment _
  | BEXE_nop _
  | BEXE_begin
  | BEXE_end -> exe

(* -------------------------------------------------------------------------- *)

(** Simplify the bound exe. *)
let reduce exe =
  match map ~f_bexpr:Flx_bexpr.reduce exe with
  | BEXE_call (sr,(Flx_bexpr.BEXPR_closure (i,ts),_),a) ->
      BEXE_call_direct (sr,i,ts,a)
  | x -> x

(* -------------------------------------------------------------------------- *)

(** Prints a bexe to a formatter. *)
let print f = function
  | BEXE_label (sr, s) ->
      print_variant2 f "BEXE_label"
        Flx_srcref.print sr
        print_string s
  | BEXE_comment (sr, s) ->
      print_variant2 f "BEXE_comment"
        Flx_srcref.print sr
        print_string s
  | BEXE_halt (sr, s) ->
      print_variant2 f "BEXE_halt"
        Flx_srcref.print sr
        print_string s
  | BEXE_trace (sr, v, s) ->
      print_variant3 f "BEXE_trace"
        Flx_srcref.print sr
        print_string v
        print_string s
  | BEXE_goto (sr, s) ->
      print_variant2 f "BEXE_goto"
        Flx_srcref.print sr
        print_string s
  | BEXE_ifgoto (sr, e, s) ->
      print_variant3 f "BEXE_ifgoto"
        Flx_srcref.print sr
        Flx_bexpr.print e
        print_string s
  | BEXE_call (sr, p, a) ->
      print_variant3 f "BEXE_call"
        Flx_srcref.print sr
        Flx_bexpr.print p
        Flx_bexpr.print a
  | BEXE_call_direct (sr, bid, ts, a) ->
      print_variant4 f "BEXE_call_direct"
        Flx_srcref.print sr
        print_bid bid
        (Flx_list.print Flx_btype.print) ts
        Flx_bexpr.print a
  | BEXE_call_stack (sr, bid, ts, a) ->
      print_variant4 f "BEXE_call_stack"
        Flx_srcref.print sr
        print_bid bid
        (Flx_list.print Flx_btype.print) ts
        Flx_bexpr.print a
  | BEXE_call_prim (sr, bid, ts, a) ->
      print_variant4 f "BEXE_call_prim"
        Flx_srcref.print sr
        print_bid bid
        (Flx_list.print Flx_btype.print) ts
        Flx_bexpr.print a
  | BEXE_jump (sr, p, a) ->
      print_variant3 f "BEXE_jump"
        Flx_srcref.print sr
        Flx_bexpr.print p
        Flx_bexpr.print a
  | BEXE_jump_direct (sr, bid, ts, a) ->
      print_variant4 f "BEXE_jump_direct"
        Flx_srcref.print sr
        print_bid bid
        (Flx_list.print Flx_btype.print) ts
        Flx_bexpr.print a
  | BEXE_svc (sr, bid) ->
      print_variant2 f "BEXE_srv"
        Flx_srcref.print sr
        print_bid bid
  | BEXE_fun_return (sr, e) ->
      print_variant2 f "BEXE_fun_return"
        Flx_srcref.print sr
        Flx_bexpr.print e
  | BEXE_yield (sr, e) ->
      print_variant2 f "BEXE_yield"
        Flx_srcref.print sr
        Flx_bexpr.print e
  | BEXE_proc_return sr ->
      print_variant1 f "BEXE_proc_return"
        Flx_srcref.print sr
  | BEXE_nop (sr, s) ->
      print_variant2 f "BEXE_nop"
        Flx_srcref.print sr
        print_string s
  | BEXE_code (sr, code) ->
      print_variant2 f "BEXE_code"
        Flx_srcref.print sr
        print_code_spec code
  | BEXE_nonreturn_code (sr, code) ->
      print_variant2 f "BEXE_nonreturn_code"
        Flx_srcref.print sr
        print_code_spec code
  | BEXE_assign (sr, l, r) ->
      print_variant3 f "BEXE_assign"
        Flx_srcref.print sr
        Flx_bexpr.print l
        Flx_bexpr.print r
  | BEXE_init (sr, l, r) ->
      print_variant3 f "BEXE_jump"
        Flx_srcref.print sr
        print_bid l
        Flx_bexpr.print r
  | BEXE_begin ->
      print_variant0 f "BEXE_begin"
  | BEXE_end ->
      print_variant0 f "BEXE_end"
  | BEXE_assert (sr, e) ->
      print_variant2 f "BEXE_assert"
        Flx_srcref.print sr
        Flx_bexpr.print e
  | BEXE_assert2 (sr1, sr2, e1, e2) ->
      print_variant4 f "BEXE_assert2"
        Flx_srcref.print sr1
        Flx_srcref.print sr2
        (print_opt Flx_bexpr.print) e1
        Flx_bexpr.print e2
  | BEXE_axiom_check (sr, e) ->
      print_variant2 f "BEXE_axiom_check"
        Flx_srcref.print sr
        Flx_bexpr.print e
