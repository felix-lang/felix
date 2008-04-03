(* Various primitives *)

open Ocs_types
open Ocs_error
open Ocs_env
open Ocs_eval
open Ocs_misc
open Ocs_sym
open Ocs_io
open Ocs_compile
open Ocs_macro

let logical_not =
  function
    Sfalse -> Strue
  | _ -> Sfalse
;;

(* Type predicates *)

let is_boolean =
  function
    Strue | Sfalse -> Strue
  | _ -> Sfalse
;;

let is_string =
  function
    Sstring _ -> Strue
  | _ -> Sfalse
;;

let is_char =
  function
    Schar _ -> Strue
  | _ -> Sfalse
;;

let is_pair =
  function
    Spair _ -> Strue
  | _ -> Sfalse
;;

let is_null =
  function
    Snull -> Strue
  | _ -> Sfalse
;;

let is_vector =
  function
    Svector _ -> Strue
  | _ -> Sfalse
;;

let is_proc =
  function
    Sproc (_, _) | Sprim _ -> Strue
  | _ -> Sfalse
;;

let is_port =
  function
    Sport _ -> Strue
  | _ -> Sfalse
;;

let is_symbol =
  function
    Ssymbol _ -> Strue
  | _ -> Sfalse
;;

let symbol_to_string =
  function
    Ssymbol s -> Sstring s
  | _ -> raise (Error "symbol->string: not a symbol")
;;

let string_to_symbol =
  function
    Sstring s -> get_symbol (String.copy s)
  | _ -> raise (Error "string->symbol: not a string")
;;

let is_eq a b =
  if a == b then Strue else Sfalse
;;

let is_eqv a b =
  if test_eqv a b then Strue else Sfalse
;;

let is_equal a b =
  if test_equal a b then Strue else Sfalse
;;

let do_apply th cc av =
  let n = Array.length av in
    if n < 1 then
      raise (Error "apply: bad args")
    else
      let f = av.(0) in
      let rec loop i r =
	if i = 0 then
	  r
	else if i = n - 1 then	(* r must be [] *)
	  loop (i - 1) (list_to_caml av.(i))
	else
	  loop (i - 1) (av.(i)::r)
      in
	let args = Array.map (fun x -> Cval x)
			     (Array.of_list (loop (n - 1) []))
	in
	  eval th cc (mkapply (Cval f) args)
;;

let force _ cc =
  function
    [| Spromise ({ promise_code = c;
	           promise_val = None;
		   promise_th = Some th } as p) |] ->
      eval th
	(fun v ->
	  match p.promise_val with		(* Computed before returning? *)
	    Some v -> cc v
	  | None ->
	      p.promise_val <- Some v;
	      p.promise_th <- None;		(* Release reference for gc *)
	      cc v) c
  | [| Spromise { promise_code = _;
	          promise_val = Some v;
	          promise_th = _ } |] ->
      cc v
  | _ -> raise (Error "force: bad args")
;;

let map_for_each th cc av is_map =
  let my_name = if is_map then "map" else "for-each"
  and na = Array.length av - 1 in
    if na <= 0 then
      raise (Error (my_name ^ ": bad args"));
    let proc = av.(0)
    and get_cdr =
      function
	Spair { car = _; cdr = t } -> t
      | _ -> raise (Error (my_name ^ ": list lengths don't match"))
    and get_carc =
      function
	Spair { car = h; cdr = _ } -> Cval h
      | _ -> raise (Error (my_name ^ ": list lengths don't match"))
    and result = ref (if is_map then Snull else Sunspec)
    and rtail = ref Snull in
    let append v =
      if !rtail == Snull then
	begin
	  result := Spair { car = v; cdr = Snull };
	  rtail := !result;
	end
      else
	begin
	  match !rtail with
	    Spair p ->
	      p.cdr <- Spair { car = v; cdr = Snull };
	      rtail := p.cdr
	  | _ -> assert false
	end
    in
      let rec loop args =
	match args.(0) with
	  Snull -> cc !result
	| Spair _ ->
	    eval th
	      (fun v ->
		if is_map then append v;
		loop (Array.map get_cdr args))
	      (mkapply (Cval proc) (Array.map get_carc args))
	| _ -> raise (Error (my_name ^ ": invalid argument lists"))
      in
	loop (Array.sub av 1 na)
;;

let map th cc av =
  map_for_each th cc av true
;;

let for_each th cc av =
  map_for_each th cc av false
;;

let load_file e th name =
  let th = { th with th_display = [| |]; th_depth = -1 }
  and inp = Ocs_port.open_input_port name in
  let lex = Ocs_lex.make_lexer inp name in
  let rec loop () =
    match Ocs_read.read_expr lex with
      Seof -> ()
    | v ->
	let c = compile e v in
	  eval th (fun _ -> ()) c;
	  loop ()
  in
    loop ()
;;

let load_prim e th cc =
  function
    [| Sstring name |] -> load_file e th name; cc Sunspec
  | _ -> raise (Error "load: invalid name argument")
;;

let eval_prim th cc =
  function
    [| expr; Sesym (e, _) |] ->
      eval { th with th_display = [| |]; th_depth = -1 } cc
	(compile e expr)
  | _ -> raise (Error "eval: invalid args")
;;

let report_env e _ =
  Sesym (env_copy e, Ssymbol "")
;;

let null_env _ =
  let e = top_env () in
    bind_lang e;
    bind_macro e;
    Sesym (e, Ssymbol "")
;;

let interact_env e =
  fun () -> Sesym (e, Ssymbol "") 
;;

let init e =
  set_pf1 e logical_not "not";
  set_pf1 e is_boolean "boolean?";
  set_pf1 e is_string "string?";
  set_pf1 e is_char "char?";
  set_pf1 e is_vector "vector?";
  set_pf1 e is_pair "pair?";
  set_pf1 e is_null "null?";
  set_pf1 e is_proc "procedure?";
  set_pf1 e is_port "port?";
  set_pf1 e is_symbol "symbol?";

  set_pf1 e symbol_to_string "symbol->string";
  set_pf1 e string_to_symbol "string->symbol";

  set_pf2 e is_eq "eq?";
  set_pf2 e is_eqv "eqv?";
  set_pf2 e is_equal "equal?";

  set_pfcn e do_apply "apply";

  set_pfcn e force "force";

  set_pfcn e map "map";
  set_pfcn e for_each "for-each";

  set_pfcn e (load_prim e) "load";
  set_pfcn e eval_prim "eval";

  set_pf1 e (report_env (env_copy e)) "scheme-report-environment";
  set_pf1 e null_env "null-environment";
  set_pf0 e (interact_env e) "interaction-environment";
;;

