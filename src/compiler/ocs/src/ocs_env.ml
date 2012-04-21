(* Compilation environment, variable bindings.  *)

open Ocs_types
open Ocs_error
open Ocs_sym
open Ocs_vartable

let top_env () =
  { env_depth = -1;
    env_vartable = vt_create ();
    env_frame_size = ref 0;
    env_tagged = [] }
;;

let new_scope e =
  { env_depth = e.env_depth;
    env_vartable = vt_inherit e.env_vartable;
    env_frame_size = e.env_frame_size;
    env_tagged = e.env_tagged }
;;

let new_frame e =
  { env_depth = e.env_depth + 1;
    env_vartable = vt_inherit e.env_vartable;
    env_frame_size = ref 0;
    env_tagged = e.env_tagged }
;;

let new_var e =
  if e.env_depth < 0 then
    Vglob { g_sym = Snull; g_val = Sunbound }
  else
    let v = Vloc (e.env_depth, !(e.env_frame_size)) in
      incr e.env_frame_size;
      v
;;

let bind_name e sym v =
  match sym with
    Sesym (te, sym) ->
      e.env_tagged <- (te, sym, v)::e.env_tagged
  | _ ->
      begin
	match v with
	  Vglob g -> g.g_sym <- sym
	| _ -> ()
      end;
      var_insert e.env_vartable (sym_name sym) v
;;

let bind_var e sym =
  let r = new_var e in
    bind_name e sym r;
    r
;;

let find_tagged te ts l =
  let rec loop =
    function
      (e, s, v)::t ->
	if e == te && s == ts then
	  Some v
	else
	  loop t
    | [] -> None
  in
    loop l
;;

let rec find_var e sym =
  match sym with
    Sesym (te, sym) ->
      begin
	match find_tagged te sym e.env_tagged with
	  Some _ as v -> v
	| None -> find_var te sym
      end
  | _ -> var_find e.env_vartable (sym_name sym)
;;

let rec get_var e sym =
  match sym with
    Sesym (te, sym) ->
      begin
	match find_tagged te sym e.env_tagged with
	  Some v -> v
	| None -> get_var te sym
      end
  | _ -> var_get e.env_vartable (sym_name sym)
		 (fun () -> Vglob { g_sym = sym; g_val = Sunbound })
;;

let set_glob e sym v =
  match get_var e sym with
    Vglob g -> g.g_val <- v
  | _ -> raise (Error "set_glob: not a global")
;;

let vb_copy =
  function
      Vglob g -> Vglob { g with g_val = g.g_val }
    | x -> x

let env_copy e =
  { e with
    env_vartable = vt_copy e.env_vartable vb_copy;
    env_tagged = [] }

let is_a_keyword e sym =
  match find_var e sym with
    Some (Vkeyword _) -> true
  | _ -> false
;;

let is_keyword e sym name =
  match find_var e sym with
    Some (Vkeyword kw) -> kw = name
  | _ -> false
;;

let safe_is_keyword e sym name =
  match sym with
    Ssymbol _ | Sesym (_, _) -> is_keyword e sym name
  | _ -> false
;;

let is_syntax e sym sf =
  match find_var e sym with
    Some (Vsyntax f) -> f == sf
  | _ -> false
;;

let set_pfg e f n =
  set_glob e (get_symbol n) (Sprim { prim_fun = f; prim_name = n })
;;

let set_pf0 e f n = set_pfg e (Pf0 f) n
let set_pf1 e f n = set_pfg e (Pf1 f) n
let set_pf2 e f n = set_pfg e (Pf2 f) n
let set_pf3 e f n = set_pfg e (Pf3 f) n
let set_pfn e f n = set_pfg e (Pfn f) n
let set_pfcn e f n = set_pfg e (Pfcn f) n

