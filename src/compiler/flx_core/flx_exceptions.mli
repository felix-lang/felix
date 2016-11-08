(** Compile time exceptions *)

open Flx_ast
open Flx_types
exception GadtUnificationFailure

exception SyntaxError of string
exception ParseError of string
exception TokenError of string
exception ClientErrorn of Flx_srcref.t list * string
exception ClientError of Flx_srcref.t * string
exception ClientError2 of Flx_srcref.t * Flx_srcref.t * string
exception SystemError of Flx_srcref.t * string
exception Exit of int
exception Bad_recursion
exception Expr_recursion of expr_t
exception Unresolved_return of Flx_srcref.t * string
exception SimpleNameNotFound of Flx_srcref.t * string * string
exception FunctionNameNotFound of Flx_srcref.t * string * string * string list


val clierrn: Flx_srcref.t list -> string -> 'a
val clierr: Flx_srcref.t -> string -> 'a
val clierrx: string -> Flx_srcref.t -> string -> 'a
val clierr2: Flx_srcref.t -> Flx_srcref.t -> string -> 'a
val syserr: Flx_srcref.t -> string -> 'a
