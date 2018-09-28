let dummy_sr = Flx_srcref.make ("dummy",0,0,0,0) 

let string_of_sval  (s:Ocs_types.sval) : string =
  match s with
  | Ocs_types.Sstring s -> Bytes.to_string s
  | _ -> failwith 
    ("string of sval expected a Scheme string, got " ^ Ocs_print.string_of_ocs s)

let scheme_lex sr (s:string):Ocs_types.sval =
  let sr = Flx_srcref.short_string_of_src sr in
  let inp = Ocs_port.string_input_port s in
  let lex = Ocs_lex.make_lexer inp sr in
  match Ocs_read.read_expr lex with
  | Ocs_types.Seof -> print_endline "END OF FILE?"; Ocs_types.Snull
  | v ->  v

let silly_scheme_lex (s:Ocs_types.sval):Ocs_types.sval =
  let s = string_of_sval s in
  scheme_lex dummy_sr s

let scheme_compile env (s:Ocs_types.sval):Ocs_types.code =
  Ocs_compile.compile env s

let scheme_eval (c:Ocs_types.code):Ocs_types.sval =
  let th = Ocs_top.make_thread () in
  let term = ref None in
  Ocs_eval.eval th (fun (r:Ocs_types.sval) -> term := Some r) c;
  match !term with
  | None -> failwith "Scheme term not returned!"
  | Some r -> r

let scheme_run_sexpr env (l:Ocs_types.sval):Ocs_types.sval =
  let c :Ocs_types.code = scheme_compile env l in
  let r :Ocs_types.sval = scheme_eval c in
  r


let scheme_run sr env (s:string):Ocs_types.sval =
  let l :Ocs_types.sval = scheme_lex sr s in
  let c :Ocs_types.code = scheme_compile env l in
  let r :Ocs_types.sval = scheme_eval c in
  r

let silly_scheme_run env (s:Ocs_types.sval):Ocs_types.sval =
  scheme_run dummy_sr env (string_of_sval s)


