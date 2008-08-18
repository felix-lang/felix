(** Compile time exceptions *)

open Flx_ast
open Flx_types
exception RDP_match_fail of range_srcref * range_srcref * string
exception RDP_alternatives_exhausted of range_srcref * string
exception SyntaxError of string
exception ParseError of string
exception LexError of string
exception TokenError of string
exception ClientErrorn of range_srcref list * string
exception ClientError of range_srcref * string
exception ClientError2 of range_srcref * range_srcref * string
exception SystemError of range_srcref * string
exception Exit of int
exception Bad_recursion
exception Expr_recursion of expr_t
exception Free_fixpoint of btypecode_t
exception Unresolved_return of range_srcref * string

val clierrn: range_srcref list -> string -> 'a
val clierr: range_srcref -> string -> 'a
val clierr2: range_srcref -> range_srcref -> string -> 'a
val syserr: range_srcref -> string -> 'a
val catch: string -> (unit -> 'a) -> 'a
val rdp_match_fail: range_srcref -> range_srcref -> string -> 'a
val rdp_alternatives_exhausted: range_srcref -> string -> 'a
