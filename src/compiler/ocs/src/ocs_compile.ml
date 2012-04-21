(* Compile Scheme expressions into a form that can be evaluated efficiently.  *)

open Ocs_types
open Ocs_error
open Ocs_sym
open Ocs_misc
open Ocs_env
open Ocs_vartable

(* Split the variables that are arguments to let/let*/letrec *)
let letsplit f =
  function
    Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
	    cdr = Spair { car = v; cdr = Snull }} -> f s v
  | _ -> raise (Error "invalid let arglist")
;;

(* Split the variables that are arguments to do *)
let dosplit f =
  function
    Spair { car = (Ssymbol _ | Sesym (_, _)) as sym;
            cdr = Spair { car = init; cdr = t }} ->
      begin
	match t with
	  Snull -> f sym init sym
	| Spair { car = step; cdr = Snull } -> f sym init step
	| _ -> raise (Error "invalid do arglist")
      end
  | _ -> raise (Error "invalid do arglist")
;;

let genset b v =
  match b with
    Vglob g -> Csetg (g, v)
  | Vloc (d, i) -> Csetl (d, i, v)
  | _ -> raise (Error "cannot change syntactic keywords")
;;

let gendef b v =
  match b with
    Vglob g -> Cdefine (g, v)
  | Vloc (d, i) -> Csetl (d, i, v)
  | _ -> raise (Error "cannot change syntactic keywords")
;;

let genref =
  function
    Vglob g -> Cgetg g
  | Vloc (d, i) -> Cgetl (d, i)
  | Vsyntax _ -> Cval Sunspec
  | Vmacro _ -> Cval Sunspec
  | Vkeyword _ -> Cval Sunbound
;;

let mkseq s =
  match Array.length s with
    0 -> Cval Sunspec
  | 1 -> s.(0)
  | 2 -> Cseq2 (s.(0), s.(1))
  | 3 -> Cseq3 (s.(0), s.(1), s.(2))
  | _ -> Cseqn s
;;

let mkand s =
  match Array.length s with
    0 -> Cval Strue
  | 1 -> s.(0)
  | 2 -> Cand2 (s.(0), s.(1))
  | 3 -> Cand3 (s.(0), s.(1), s.(2))
  | _ -> Candn s
;;

let mkor s =
  match Array.length s with
    0 -> Cval Sfalse
  | 1 -> s.(0)
  | 2 -> Cor2 (s.(0), s.(1))
  | 3 -> Cor3 (s.(0), s.(1), s.(2))
  | _ -> Corn s
;;

let make_proc c n hr fs =
  { proc_body = c;
    proc_nargs = n;
    proc_has_rest = hr;
    proc_frame_size = fs;
    proc_name = "#<unknown>#" }
;;

let chksplice a =
  let n = Array.length a in
    let rec loop i =
      if i < n then
	begin
	  match a.(i) with
	    Cqqspl _ -> true
	  | _ -> loop (i + 1)
	end
      else
	false
    in
      loop 0
;;

(* Scan quoted sections, eliminate environment-specific symbols *)
let rec scanquoted =
  function
    Sesym (_, sym) -> sym
  | Spair { car = h; cdr = t } ->
      Spair { car = scanquoted h; cdr = scanquoted t }
  | Svector v ->
      Svector (Array.map (fun x -> scanquoted x) v)
  | x -> x
;;

let is_uglobal e =
  vt_global e.env_vartable
;;

let is_global e =
  e.env_depth < 0
;;

let rec mkdefine e args =
  let narg = Array.length args in
    if narg < 1 then
      raise (Error "define: not enough args");
    match args.(0) with
      Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = al } when narg > 1 ->
	begin
	  match mklambda e al args with
	    Clambda p as l ->
	      p.proc_name <- sym_name s;
	      gendef (get_var e s) l
	  | _ -> assert false
	end
    | (Ssymbol _ | Sesym (_, _)) as s when narg = 2 ->
	gendef (get_var e s) (compile e args.(1))
    | (Ssymbol _ | Sesym (_, _)) as s when narg = 1 ->
	gendef (get_var e s) (Cval Sunspec)
    | _ -> raise (Error "define: invalid syntax")

(* The following functions up to mkbody are used to compile the body
   of a lambda, let etc., with possible internal definitions.  The
   internal definitions may be created by macro expansion, so we need
   to do that here, too...and we might end up expanding a macro more
   than once (so there must be no side-effects to expansion).  *)
and idpp =
  function
    Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
	    cdr = Spair { car = v; cdr = Snull }} -> (s, s, v)
  | Spair { car = Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
			  cdr = _ } as x;
	    cdr = _ } as v -> (s, x, v)
  | _ -> raise (Error "invalid internal definition")

and getidef e =
  function
    Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = t } ->
      begin
	match find_var e s with
	  Some (Vsyntax f) when f == mkdefine -> Some (idpp t)
	| Some (Vmacro f) -> getidef e (f e (Array.of_list (list_to_caml t)))
	| _ -> None
      end
  | _ -> None

and mkid e x v =
  match x with
    Spair { car = _; cdr = al } ->
      mklambda e al (Array.of_list (list_to_caml v))
  | _ -> compile e v

and expand_begin e =
  function
    (Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = t }) as x ->
      begin
	match find_var e s with
	  Some (Vsyntax f) when f == mkbegin ->
	    Array.concat (List.map (expand_begin e) (list_to_caml t))
	| Some (Vmacro f) ->
	    expand_begin e (f e (Array.of_list (list_to_caml t)))
	| _ -> [| x |]
      end
  | x -> [| x |]

and mkbody e args =
  let args = Array.concat (List.map (expand_begin e) (Array.to_list args)) in
  let n = Array.length args in
  let rec loop i r =
    if i < n then
      begin
	match getidef e args.(i) with
	  Some d -> loop (i + 1) (d::r)
	| None -> r
      end
    else
      r
  in
    let ids = Array.map (fun (s, x, v) -> let r = bind_var e s in (r, x, v))
                        (Array.of_list (List.rev (loop 0 []))) in
    let sets = Array.map (fun (r, x, v) -> gendef r (mkid e x v)) ids in
    let nid = Array.length sets in
    let rest = Array.map (fun x -> compile e x)
			 (Array.sub args nid (n - nid))
    in
      Array.append sets rest

and mkset e args =
  if Array.length args != 2 then
    raise (Error "set!: requires exactly two args");
  match args.(0) with
    (Ssymbol _ | Sesym (_, _)) as s ->
      let v = compile e args.(1) in
	genset (get_var e s) v
  | _ -> raise (Error "set!: not a symbol")

(* Note that the first item of the "body" array is ignored, it
   corresponds to the argument list but may be in the form expected
   by either define or lambda.  *)
and mklambda e args body =
  let ne = new_frame e
  and nargs = ref 0
  and has_rest = ref false in
    let rec scanargs =
      function
	Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = tl } ->
	  let _ = bind_var ne s in
	    incr nargs;
	    scanargs tl
      | (Ssymbol _ | Sesym (_, _)) as s ->
	  let _ = bind_var ne s in
	    incr nargs;
	    has_rest := true;
	    ()
      | Snull -> ()
      | _ -> raise (Error "lambda: bad arg list")
    in
      scanargs args;
      let body =
	mkseq (mkbody ne (Array.sub body 1 (Array.length body - 1)))
      in
	Clambda (make_proc body !nargs !has_rest !(ne.env_frame_size))

and mkif e args =
  match Array.length args with
    2 -> Cif (compile e args.(0), compile e args.(1), Cval Sunspec)
  | 3 -> Cif (compile e args.(0), compile e args.(1), compile e args.(2))
  | _ -> raise (Error "if: needs two or three args")

and mknamedlet e s args =
  let argv =
    Array.map
      (letsplit (fun s v -> s, compile e v))
      (Array.of_list (list_to_caml args.(1))) in
  let ar = new_var e in
  let ne = new_frame e in
    bind_name ne s ar;
    let av =
      Array.map (fun (s, v) -> let _ = bind_var ne s in v) argv in
    let body = mkseq (mkbody ne (Array.sub args 2 (Array.length args - 2))) in
    let proc =
      Clambda (make_proc body (Array.length av) false !(ne.env_frame_size))
    in
      Cseq2 (gendef ar proc, mkapply (genref ar) av)

and mklet e args =
  if Array.length args < 2 then
    raise (Error "let: too few args");
  match args.(0) with
    (Ssymbol _ | Sesym (_, _)) as s -> mknamedlet e s args
  | Snull -> mkseq (mkbody e (Array.sub args 1 (Array.length args - 1)))
  | Spair _ as al ->
      let argv =
	Array.map
	  (letsplit (fun s v -> s, compile e v))
	  (Array.of_list (list_to_caml al)) in
      let ne = new_frame e in
      let av = Array.map (fun (s, v) -> let _ = bind_var ne s in v) argv in
      let body = mkseq (mkbody ne (Array.sub args 1 (Array.length args - 1))) in
      let proc =
	Clambda (make_proc body (Array.length av) false !(ne.env_frame_size))
      in
	mkapply proc av
  | _ -> raise (Error "let: missing argument list")

and mkletstar e args =
  if Array.length args < 2 then
    raise (Error "let*: too few args");
  let rec build e =
    function
      x::t ->
	let (s, v) = letsplit (fun s v -> s, compile e v) x in
	let ne = new_frame e in
	let _ = bind_var ne s in
	let body = build ne t in
	let proc = Clambda (make_proc body 1 false !(ne.env_frame_size)) in
	  mkapply proc [| v |]
    | [] -> mkseq (mkbody e (Array.sub args 1 (Array.length args - 1)))
  in
    build e (list_to_caml args.(0))

and mkletrec e args =
  if Array.length args < 2 then
    raise (Error "letrec: too few args");
  let ne = new_frame e in
  let av =
    Array.map (letsplit (fun s v -> let r = bind_var ne s in (r, v)))
	      (Array.of_list (list_to_caml args.(0))) in
  let avi = Array.map (fun (r, v) -> compile ne v) av in
  let ne' = new_frame ne in
  let sets = Array.map (fun (r, v) -> gendef r (genref (new_var ne'))) av in
  let body = mkseq (Array.append sets
    (mkbody ne' (Array.sub args 1 (Array.length args - 1)))) in
  let proc =
    Clambda (make_proc body (Array.length av) false !(ne'.env_frame_size)) in
  let proc =
    Clambda (make_proc (mkapply proc avi)
		       (Array.length av) false !(ne.env_frame_size))
  in
    mkapply proc (Array.map (fun _ -> Cval Sunspec) av)

and compileseq e s =
  mkseq (Array.map (fun x -> compile e x)
                   (Array.of_list (list_to_caml s)))

and mkcond e args =
  Ccond
    (Array.map
      (function
	  Spair { car = test;
	          cdr = Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
			        cdr = Spair { car = x; cdr = Snull }}}
	    when is_keyword e s "=>" ->
	      (Ccondspec (compile e test), (compile e x))
	| Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = body }
	    when is_keyword e s "else" ->
	    (Cval Strue, compileseq e body)
	| Spair { car = test; cdr = body } ->
	    (compile e test, compileseq e body)
	| _ -> raise (Error "cond: syntax error"))
      args)

and mkcase e args =
  Ccase
    (compile e args.(0),
     Array.map
      (function
	  Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = body }
	    when is_keyword e s "else" ->
	    ([| |], compileseq e body)
	| Spair { car = Spair _ as c; cdr = body } ->
	    (Array.of_list (list_to_caml c), compileseq e body)
	| _ -> raise (Error "case: syntax error"))
      (Array.sub args 1 (Array.length args - 1)))

and mkdo e args =
  if Array.length args < 2 then
    raise (Error "do: bad args");
  let vv =
    Array.map
      (dosplit (fun sym init step -> sym, compile e init, step))
      (Array.of_list (list_to_caml args.(0)))
  and (test, result) =
    match args.(1) with
      Spair { car = t; cdr = r } -> t, r
    | _ -> raise (Error "do: bad args")
  and anonvar = new_var e
  and ne = new_frame e in
    let av = Array.map (fun (sym, init, _) ->
			  let _ = bind_var ne sym in init) vv in
    let body =
      Cif (compile ne test, compileseq ne result,
	   mkseq 
	    (Array.append
	      (Array.map (fun x -> compile ne x)
			 (Array.sub args 2 (Array.length args - 2)))
	      [| mkapply (genref anonvar)
	         (Array.map (fun (_, _, step) -> compile ne step) vv) |]))
    in
      let proc =
	Clambda (make_proc body (Array.length av) false !(ne.env_frame_size))
      in
	Cseq2 (gendef anonvar proc, mkapply (genref anonvar) av)

and mkdelay e =
  function
    [| expr |] -> Cdelay (compile e expr)
  | _ -> raise (Error "delay: bad args")

and mkqq e args =
  if Array.length args <> 1 then
    raise (Error "quasiquote: need exactly one arg")
  else
    let rec qq depth =
      function
	Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
	        cdr = Spair { car = x; cdr = Snull }} ->
	  if is_syntax e s mkqq then
	    Cqqp (Cval s, Cqqp (qq (depth + 1) x, Cval Snull))
	  else if is_keyword e s "unquote" then
	    begin
	      if depth > 0 then
		Cqqp (Cval s, Cqqp (qq (depth - 1) x, Cval Snull))
	      else
		compile e x
	    end
	  else if is_keyword e s "unquote-splicing" then
	    begin
	      if depth > 0 then
		Cqqp (Cval s, Cqqp (qq (depth - 1) x, Cval Snull))
	      else
		Cqqspl (compile e x)
	    end
	  else
	    Cqqp (Cval s, Cqqp (qq depth x, Cval Snull))
      | Spair { car = h; cdr = t } -> Cqqp (qq depth h, qq depth t)
      | Svector v ->
	  let qv = Array.map (fun x -> qq depth x) v in
	    if chksplice qv then
	      Cqqvs (Array.to_list qv)
	    else
	      Cqqv qv
      | x -> Cval (scanquoted x)
    in
      qq 0 args.(0)

and applysym e s args =
  match get_var e s with
    Vsyntax f -> f e args
  | Vmacro f -> compile e (f e args)
  | r -> mkapply (genref r) (Array.map (fun x -> compile e x) args)

and compile e =
  function
    (Ssymbol _ | Sesym (_, _)) as s -> genref (get_var e s)
  | Spair p ->
      let args = Array.of_list (list_to_caml p.cdr) in
	begin
	  match p.car with
	    (Ssymbol _ | Sesym (_, _)) as s -> applysym e s args
	  | x ->
	      mkapply (compile e x) (Array.map (fun x -> compile e x) args)
        end
  | x -> Cval (scanquoted x)

and mkbegin e args =
  mkseq (Array.map (fun x -> compile e x) args)
;;

let bind_lang e =
  let spec =
    [ sym_define, mkdefine;
      sym_set, mkset;
      sym_let, mklet;
      sym_letstar, mkletstar;
      sym_letrec, mkletrec;
      sym_if, mkif;
      sym_cond, mkcond;
      sym_case, mkcase;
      sym_do, mkdo;
      sym_begin, mkbegin;
      sym_and, (fun e args -> mkand (Array.map (fun x -> compile e x) args));
      sym_or, (fun e args -> mkor (Array.map (fun x -> compile e x) args));
      sym_lambda,
	(fun e args ->
	  if Array.length args >= 1 then
	    mklambda e args.(0) args
	  else
	    raise (Error "lambda: needs at least one arg"));
      sym_delay, mkdelay;
      sym_quote,
	(fun e args ->
	  if Array.length args = 1 then
	    Cval (scanquoted args.(0))
	  else
	    raise (Error "quote: need exactly one arg"));
      sym_quasiquote, mkqq ]
  in
    List.iter (fun (s, f) -> bind_name e s (Vsyntax f)) spec;
    bind_name e sym_else (Vkeyword "else");
    bind_name e sym_arrow (Vkeyword "=>");
    bind_name e sym_unquote (Vkeyword "unquote");
    bind_name e sym_unquote_splicing (Vkeyword "unquote-splicing");
    bind_name e sym_syntax_rules (Vkeyword "syntax-rules");
;;

