(* Macro definitions and expanders.  *)

open Ocs_types
open Ocs_error
open Ocs_sym
open Ocs_env
open Ocs_compile
open Ocs_misc

(* Pattern/template type in syntax rules.  *)
type pattern =
    Pkeyword of env * sval
  | Pvar of sval
  | Pvalue of sval
  | Ppair of pattern * pattern
  | Pvector of pattern array
  | Pmulti of pattern * pattern		(* pattern ... in a list *)
  | Pvmulti of pattern 			(* pattern ... in a vector *)
  | Pmvector of pattern list		(* Vector with Pvmulti patterns *)

type ruleset = {
  mutable r_rules : (pattern * pattern) list
}
(* Values of pattern variables *)
type pattvar =
    Vitem of sval
  | Vmulti of pattvar list

let rec var_name =
  function
    Sesym (_, s) -> var_name s
  | Ssymbol _ as s -> sym_name s
  | _ -> assert false
;;

let kw_name =
  function
    Vkeyword s -> s
  | _ -> assert false
;;

(* Parse a pattern or template *)
let parseptt e patt dovar =
  let rec parse =
    function
      Spair { car = h; cdr = t } ->
	let x = parse h in
	  begin
	    match t with		(* Special case, ... *)
	      Spair { car = (Ssymbol _ | Sesym (_, _)) as s; cdr = t }
		when is_keyword e s "..." -> Pmulti (x, parse t)
	    | _ -> Ppair (x, parse t)
	  end
    | (Ssymbol _ | Sesym (_, _)) as s -> dovar s
    | Svector v ->
	let n = Array.length v
	and has_multi = ref false in
	let rec loop r i =
	  if i < 0 then
	    r
	  else if safe_is_keyword e v.(i) "..." && i > 0 then
	    begin
	      has_multi := true;
	      loop (Pvmulti (parse v.(i - 1))::r) (i - 2)
	    end
	  else
	    loop (parse v.(i)::r) (i - 1)
	in
	  let nl = loop [] (n - 1) in
	    if !has_multi then
	      Pmvector nl
	    else
	      Pvector (Array.of_list nl)
    | x -> Pvalue x
  in
    parse patt
;;

let rec match_sym s s' =
  match (s, s') with
    Sesym (e, s), Sesym (e', s') -> e == e' && match_sym s s'
  | _, _ -> s == s'
;;

let parsepatt e litlist patt =
  let vars = ref [] in
  let p = parseptt e patt
    (fun s ->
      try
	let _ = List.find (fun s' -> match_sym s s') litlist in
	  Pkeyword (e, s)
      with Not_found ->
	vars := s::!vars;
	Pvar s)
  in
    (!vars, p)
;;

let parsetmpl e varlist tmpl =
  let assocvar =
    function
      Sesym (e, s) as sym ->
	begin
	  try
	    Pvar (List.find (function
			      Sesym (e', s') -> e' == e && s' == s
			    | _ -> false) varlist)
	  with Not_found ->
	    Pvalue sym
	end
    | s -> if List.memq s varlist then Pvar s else Pvalue s
  in
    parseptt e tmpl assocvar
;;

let parserule e ll =
  function
    Spair { car = Spair { car = _; cdr = patt };
	    cdr = Spair { car = tmpl; cdr = Snull }} ->
      let (vars, patt) = parsepatt e ll patt in
      let tmpl = parsetmpl e vars tmpl in
	(patt, tmpl)
  | _ -> raise (Error "syntax definition: invalid syntax rule")
;;

let parsetspec e sym =
  function
    Spair { car = (Ssymbol _ | Sesym (_, _)) as s;
	    cdr = Spair { car = literals; cdr = rules }}
      when is_keyword e s "syntax-rules" ->
      let litlist = list_to_caml literals in
        List.map (parserule e litlist) (list_to_caml rules)
  | _ -> raise (Error "syntax definition: invalid transformer spec")
;;

let rebuild a =
  let rec loop i r =
    if i < 0 then
      r
    else
      loop (i - 1) (Spair { car = a.(i); cdr = r })
  in
    loop (Array.length a - 1) Snull
;;

(* Given a pattern, return an association list of pattern variables
   with empty multiple value lists.  *)
let rec empty_vars =
  function
    Pvar v -> [ v, Vmulti [] ]
  | Ppair (h, t) -> (empty_vars h) @ (empty_vars t)
  | Pmulti (p, t) -> (empty_vars p) @ (empty_vars t)
  | Pvmulti p -> empty_vars p
  | Pvector v ->
      Array.fold_left (@) [] (Array.map empty_vars v)
  | Pmvector l ->
      List.fold_left (@) [] (List.map empty_vars l)
  | _ -> []
;;

(* Merge an association list of multi-vars and a set of values *)
let merge_vars mv vs =
  let rec merge =
    function
      (v, Vmulti m)::t -> (v, Vmulti (m @ [ List.assq v vs ]))::(merge t)
    | [] -> []
    | _ -> assert false
  in
    merge mv
;;

let rec normalize_name =
  function
    Ssymbol s -> s
  | Sesym (_, s) -> normalize_name s
  | _ -> "<not a symbol>"
;;

let rec match_patt e patt expr =
  let vars = ref [] in
  let rec match_sub p x =
    match p with
      Pkeyword (e', s') ->
	begin
	  match x with
	    Ssymbol _ | Sesym (_, _) -> (get_var e x) == (get_var e' s')
	  | _ -> false
	end
    | Pvar v -> vars := (v, Vitem x)::!vars; true
    | Pvalue v -> test_equal v x
    | Ppair (h, t) ->
	begin
	  match x with
	    Spair { car = xh; cdr = xt } -> match_sub h xh && match_sub t xt
	  | _ -> false
	end
    | Pvector v ->
	begin
	  match x with
	    Svector sv ->
	      let n = Array.length v in
		if Array.length sv <> n then
		  false
		else
		  let rec loop i =
		    if i = n then
		      true
		    else if match_sub v.(i) sv.(i) then
		      loop (i + 1)
		    else
		      false
		  in
		    loop 0
	  | _ -> false
	end
    | Pmulti (p, t) ->
	if t <> (Pvalue Snull) then
	  raise (Error "invalid pattern");
	begin
	  let rec loop r =
	    function
	      Spair { car = h; cdr = t } ->
		begin
		  match match_patt e p h with
		    Some v -> loop (merge_vars r v) t
		  | None -> None
		end
	    | Snull -> Some r
	    | _ -> None
	  in
	    match loop (empty_vars p) x with
	      Some vl -> vars := vl @ !vars; true
	    | None -> false
	end
    | Pmvector l ->
	begin
	  match x with
	    Svector v ->
	      begin
		let n = Array.length v in
		let rec loop i =
		  function
		    Pvmulti p::t ->
		      if i >= n then
			begin
			  vars := (empty_vars p) @ !vars;
			  loop i t
			end
		      else
			begin
			  let rec mloop r i =
			    if i >= n then Some r
			    else
			      match match_patt e p v.(i) with
				Some v -> mloop (merge_vars r v) (i + 1)
			      | None -> None
			  in
			    match mloop (empty_vars p) i with
			      Some vl -> vars := vl @ !vars; true
			    | None -> false
			end
		  | h::t ->
		      if i >= n then false
		      else if match_sub h v.(i) then loop (i + 1) t
		      else false
		  | [] -> i >= n
		in
		  loop 0 l
	      end
	  | _ -> false
	end
    | _ -> assert false
  in
    if match_sub patt expr then
      Some !vars
    else
      None
;;

(* Test whether a variable occurs in a pattern *)
let var_in_patt p (v, _) =
  let rec is_in =
    function
      Pvar pv -> pv == v
    | Ppair (h, t) -> is_in h || is_in t
    | Pmulti (p, t) -> is_in p || is_in t
    | Pvmulti p -> is_in p
    | Pvector v ->
	let rec loop i =
	  if i < 0 then false
	  else if is_in v.(i) then true
	  else loop (i - 1)
	in
	  loop (Array.length v - 1)
    | Pmvector l -> List.exists is_in l
    | _ -> false
  in
    is_in p
;;

(* Select variables that are applicable to this pattern *)
let subvars vl p =
  List.filter (var_in_patt p) vl
;;

(* Get the current values of variables, if available.  *)
let varvals vl =
  try
    let l = List.map
      (function
        (v, Vmulti (x::_)) -> (v, x)
      | (v, Vmulti []) -> raise Not_found
      | x -> x) vl
    in
      if l = [] then
	raise (Error "bad template")
      else
	(true, l)
  with Not_found -> (false, [])
;;

(* Get the next position in the variable list.  *)
let varnext vl =
  List.map (function (v, Vmulti (_::t)) -> (v, Vmulti t) | x -> x) vl
;;

let rec expand_var =
  function
    Vitem x -> x
  | Vmulti m -> make_slist Snull (List.rev_map expand_var m)
;;

let rec expand_tmpl e tmpl vars =
  let rec fix_syms =
    function
      Ssymbol _ as s -> Sesym (e, s)
    | Spair { car = h; cdr = t } ->
	Spair { car = fix_syms h; cdr = fix_syms t }
    | Svector v ->
	Svector (Array.map (fun x -> fix_syms x) v)
    | x -> x in
  let rec expand_sub =
    function
      Pvar v -> expand_var (List.assq v vars)
    | Pvalue v -> fix_syms v
    | Ppair (h, t) ->
	Spair { car = expand_sub h; cdr = expand_sub t }
    | Pvector v ->
	Svector (Array.map (fun x -> expand_sub x) v)
    | Pmulti (p, t) ->
	let rec loop r v =
	  let (ok, vv) = varvals v in
	    if ok then loop ((expand_tmpl e p vv)::r) (varnext v)
	    else r
	in
	  make_slist (expand_sub t) (loop [] (subvars vars p))
    | Pmvector l ->
	begin
	  let rec loop r =
	    function
	      Pvmulti p::t ->
		let rec mloop r v =
		  let (ok, vv) = varvals v in
		    if ok then mloop ((expand_tmpl e p vv)::r) (varnext v)
		    else r
		in
		  loop (mloop [] (subvars vars p) @ r) t
	    | h::t -> loop (expand_sub h::r) t
	    | [] -> r
	  in
	    Svector (Array.of_list (List.rev (loop [] l)))
	end
    | _ -> assert false
  in
    expand_sub tmpl
;;

let expand name me rs e av =
  let me = new_scope me in
  let al = rebuild av in
  let rec try_rule =
    function
      (patt, tmpl)::t ->
	begin
	  match match_patt e patt al with
	    Some vars -> expand_tmpl me tmpl vars
	  | None -> try_rule t
	end
    | [] -> raise (Error (name ^ ": no matching syntax rule"))
  in
    try_rule rs.r_rules
;;

let mkdefine_syntax e =
  function
    [| (Ssymbol _ | Sesym (_, _)) as sym; tspec |] ->
      let rules = parsetspec (new_scope e) sym tspec in
	bind_name e sym (Vmacro (expand (normalize_name sym) e
				        { r_rules = rules }));
	Cval Sunspec
  | _ -> raise (Error "define-syntax: bad args")
;;

let mklet_syntax e args =
  if Array.length args < 2 then
    raise (Error "let-syntax: too few args");
  let av =
    Array.map (letsplit (fun s v -> s,
      Vmacro (expand (normalize_name s) e { r_rules = parsetspec e s v })))
	      (Array.of_list (list_to_caml args.(0))) in
  let ne = new_scope e in
    Array.iter (fun (s, r) -> bind_name ne s r) av;
    mkseq (mkbody ne (Array.sub args 1 (Array.length args - 1)))
;;

let mkletrec_syntax e args =
  if Array.length args < 2 then
    raise (Error "letrec-syntax: too few args");
  let ne = new_scope e in
  let t =
    Array.map (letsplit
      (fun s v -> let r = { r_rules = [] }
                  and name = normalize_name s in
		    bind_name ne s (Vmacro (expand name ne r));
		    r, s, v))
      (Array.of_list (list_to_caml args.(0)))
  in
    Array.iter (fun (r, s, v) -> r.r_rules <- parsetspec (new_scope e) s v) t;
    mkseq (mkbody ne (Array.sub args 1 (Array.length args - 1)))
;;

let bind_macro e =
  bind_name e sym_define_syntax (Vsyntax mkdefine_syntax);
  bind_name e sym_let_syntax (Vsyntax mklet_syntax);
  bind_name e sym_letrec_syntax (Vsyntax mkletrec_syntax);
  bind_name e sym_ellipsis (Vkeyword "...")
;;

