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
