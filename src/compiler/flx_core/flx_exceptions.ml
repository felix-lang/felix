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


let clierrn srs s = raise (ClientErrorn (srs,s))
let clierr2 sr sr2 s = raise (ClientError2 (sr,sr2,s))
let clierr sr s = raise (ClientError (sr,s))
let clierrx dog sr s = raise (ClientError (sr,dog^s))
let syserr sr s = raise (SystemError (sr,s))

