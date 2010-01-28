(** Lexer generator *)

type reg_kind_t =
[
  | `regmatch of string * string
  | `reglex of string * string * string
]

type regular_args_t =
    int list *                      (* alphabet *)
    int *                           (* state count *)
    (int, Flx_bexpr.t) Hashtbl.t *  (* state->expression map *)
    (int * int, int) Hashtbl.t      (* transition matrix *)

val regen:
  Buffer.t ->
  Flx_srcref.t ->
  regular_args_t ->
  reg_kind_t ->
  (Flx_bexpr.t -> string) ->
  unit
