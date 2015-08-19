(** Code fragment inliner *)

val csubst:
  Flx_set.StringSet.t ref ->
  Flx_srcref.t ->
  Flx_srcref.t ->
  string ->

  arg:       (unit -> Flx_ctypes.cexpr_t) -> (* $t : value argument 'as is'    *)
  args:      Flx_ctypes.cexpr_t list -> (* $n : value arguments           *)
  typs:      string list             -> (* #n : value argument type       *)
  argtyp:    string                  -> (* #t   value arg tuple type      *)
  retyp:     string                  -> (* #0 : return type               *)
  gargs:     string list             -> (* ?n : generic arg type          *)
  prec:      Flx_ast.prec_t          -> (*      context precedence        *)
  argshape:  string                  -> (* @t : value arg tuple shape     *)
  argshapes: string list             -> (* @n : value arg shape           *)
  display:   string list             -> (* @dn: first n types of display  *)
  gargshapes:string list             -> (* @?n: generic arg shape         *)
  name:      string                  -> (* $0 : entity name in Felix      *)

  Flx_ctypes.cexpr_t
