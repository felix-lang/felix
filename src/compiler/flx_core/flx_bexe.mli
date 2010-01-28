open Flx_ast
open Flx_types

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

val get_srcref : t -> Flx_srcref.t

(** Prints a bexe to a formatter. *)
val print : Format.formatter -> t -> unit
