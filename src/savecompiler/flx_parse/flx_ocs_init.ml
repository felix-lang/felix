open Ocs_types

exception Scheme_error of sval

let giveup () = raise Dyp.Giveup; Sunspec
let sraise s  = raise (Scheme_error s); Sunspec
let sunescape s =
  match s with
  | Sstring s -> Sstring (Bytes.of_string (Flx_string.unescape (Bytes.to_string s)))
  | _ -> raise (Ocs_error.Error ("sunescape: not a string"))

let cquote s =
  match s with
  | Sstring s -> Sstring (Bytes.of_string (Flx_string.c_quote_of_string (Bytes.to_string s)))
  | _ -> raise (Ocs_error.Error ("c-quote-string: not a string"))

let utf2ucn s =
  match s with
  | Sstring s -> Sstring (Bytes.of_string (Flx_utf.utf8_to_ucn (Bytes.to_string s)))
  | _ -> raise (Ocs_error.Error ("utf8->ucn: not a string"))

let ocs_to_string s = 
  Sstring (Bytes.of_string (Ocs_print.string_of_ocs s))
 
let flx_ocs_init env =
  Ocs_env.set_pf0 env giveup "giveup";
  Ocs_env.set_pf1 env sraise "raise";
  Ocs_env.set_pf1 env sunescape "unescape";
  Ocs_env.set_pf1 env cquote "c-quote-string";
  Ocs_env.set_pf1 env utf2ucn "utf8->ucn";
  Ocs_env.set_pf1 env ocs_to_string "sexpr->string";
  Ocs_env.set_pf1 env (Flx_ocs_run.scheme_run_sexpr env) "evalsex";
  Ocs_env.set_pf1 env (Flx_ocs_run.silly_scheme_lex ) "schemelex";
  Ocs_env.set_pf1 env (Flx_ocs_run.silly_scheme_run env) "schemerun"

let init_env () =
  let env = Ocs_top.make_env () in
  flx_ocs_init env;

  let v1:Ocs_types.sval = Ocs_sym.get_symbol "_filebase" in
  let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
  Ocs_env.bind_name env v1 g1;

  let v1:Ocs_types.sval = Ocs_sym.get_symbol "_sr" in
  let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
  Ocs_env.bind_name env v1 g1;

  let v1:Ocs_types.sval = Ocs_sym.get_symbol "_arg" in
  let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sunbound } in
  Ocs_env.bind_name env v1 g1;

  for n = 1 to 20 do
    let v1:Ocs_types.sval = Ocs_sym.get_symbol ("_" ^ string_of_int n) in
    let g1:Ocs_types.vbind = Vglob { g_sym=v1; g_val = Sstring (Bytes.empty)} in
    Ocs_env.bind_name env v1 g1;
  done;
  env



