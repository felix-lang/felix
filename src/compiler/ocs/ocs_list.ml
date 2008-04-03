(* List functionality.  *)

open Ocs_types
open Ocs_error
open Ocs_env
open Ocs_misc

(* Primitives *)

let make_list av =
  let rec loop i r =
    if i < 0 then r
    else loop (i - 1) (Spair { car = Array.unsafe_get av i; cdr = r })
  in
    loop (Array.length av - 1) Snull
;;

let cons h t =
  Spair { car = h; cdr = t }
;;

let gcar =
  function
    Spair { car = r; cdr = _ } -> r
  | _ -> raise (Error "car: bad args")
;;

let gcdr =
  function
    Spair { car = _; cdr = r } -> r
  | _ -> raise (Error "cdr: bad args")
;;

let caar x = gcar (gcar x);;
let cadr x = gcar (gcdr x);;
let cdar x = gcdr (gcar x);;
let cddr x = gcdr (gcdr x);;

let gcxr seq =
  List.fold_left (fun f g -> fun x -> f (g x)) (fun x -> x) seq
;;

let set_car l v =
  match l with
    Spair p -> p.car <- v; Sunspec
  | _ -> raise (Error "set-car!: bad args")
;;

let set_cdr l v =
  match l with
    Spair p -> p.cdr <- v; Sunspec
  | _ -> raise (Error "set-cdr!: bad args")
;;

let safe_length l =
  let next =
    function
      Spair { car = _; cdr = t } -> t
    | _ -> raise (Error "length: invalid list") in
  let rec loop l r n =
    if l == Snull then
      n
    else if n land 1 = 0 then
      loop (next l) r (n + 1)
    else if l == r then
      raise (Error "length: loop detected")
    else
      loop (next l) (next r) (n + 1)
  in
    loop l l 0
;;

let is_list l =
  try
    let _ = safe_length l in Strue
  with
    _ -> Sfalse
;;

let length l =
  Sint (safe_length l)
;;

let reverse l =
  let rec loop nl =
    function
      Snull -> nl
    | Spair { car = h; cdr = t } -> loop (Spair { car = h; cdr = nl }) t
    | _ -> raise (Error "reverse: invalid list")
  in
    loop Snull l
;;

(* Copy list and set tail, used by append *)
let cptl tl =
  function
    Snull -> tl
  | Spair { car = h; cdr = t } ->
      let nl = Spair { car = h; cdr = Snull } in
      let rec loop =
	function
	  Spair p ->
	    begin
	      function
		Spair { car = h; cdr = t } ->
		  let n = Spair { car = h; cdr = Snull } in
		    p.cdr <- n; loop n t
	      | Snull -> p.cdr <- tl
	      | _ -> raise (Error "append: bad list")
	    end
	| _ -> assert false
      in
	loop nl t; nl
  | _ -> raise (Error "append: bad list")
;;

let append =
  function
    [| |] -> Snull
  | av ->
      let n = Array.length av in
      let rec loop i tl =
	if i >= 0 then
	  loop (i - 1) (cptl tl av.(i))
	else
	  tl
      in
	loop (n - 2) av.(n - 1)
;;

let list_tail l =
  function
    Sint k ->
      begin
	let rec tail i x =
	  if i = 0 then x
	  else
	    match x with
	      Spair { car = _; cdr = t } -> tail (i - 1) t
	    | _ -> raise (Error "list-tail: bad list")
	in
	  tail k l
      end
  | _ -> raise (Error "list-tail: bad args")
;;

let list_ref l k =
  match list_tail l k with
    Spair { car = x; cdr = _ } -> x
  | _ -> raise (Error "list-ref: bad args")
;;

let rec memq o =
  function
    Snull -> Sfalse
  | Spair { car = x; cdr = t } as p -> if o == x then p else memq o t
  | _ -> raise (Error "memq: bad list")
;;

let rec memv o =
  function
    Snull -> Sfalse
  | Spair { car = x; cdr = t } as p ->
      if o == x || test_eqv o x then p else memv o t
  | _ -> raise (Error "memv: bad list")
;;

let rec member o =
  function
    Snull -> Sfalse
  | Spair { car = x; cdr = t } as p ->
      if o == x || test_equal o x then p else member o t
  | _ -> raise (Error "member: bad list")
;;

let rec assq o =
  function
    Snull -> Sfalse
  | Spair { car = Spair { car = x; cdr = _ } as p; cdr = t } ->
      if o == x then p else assq o t
  | _ -> raise (Error "assq: bad list")
;;

let rec assv o =
  function
    Snull -> Sfalse
  | Spair { car = Spair { car = x; cdr = _ } as p; cdr = t } ->
      if o == x || test_eqv o x then p else assv o t
  | _ -> raise (Error "assv: bad list")
;;

let rec assoc o =
  function
    Snull -> Sfalse
  | Spair { car = Spair { car = x; cdr = _ } as p; cdr = t } ->
      if o == x || test_equal o x then p else assv o t
  | _ -> raise (Error "assoc: bad list")
;;

let list_to_vector =
  function
    Snull -> Svector [| |]
  | Spair _ as l ->
      let n = safe_length l in
      let v = Array.make n Snull in
      let rec loop i l =
	if i < n then
	  begin
	    match l with
	      Spair { car = h; cdr = t } -> v.(i) <- h; loop (i + 1) t
	    | _ -> assert false (* length was wrong?  *)
	  end
	else
	  ()
      in
	loop 0 l;
	Svector v
  | _ -> raise (Error "list->vector: bad args")
;;

let list_to_string =
  function
    Snull -> Sstring ""
  | Spair _ as l ->
      let n = safe_length l in
      let s = String.create n in
      let rec loop i l =
	if i < n then
	  begin
	    match l with
	      Spair { car = Schar c; cdr = t } -> s.[i] <- c; loop (i + 1) t
	    | _ -> raise (Error "list->string: non-characters in list")
	  end
	else
	  ()
      in
	loop 0 l;
	Sstring s
  | _ -> raise (Error "list->string: bad args")
;;

let init e =
  set_pf1 e is_list "list?";
  set_pfn e make_list "list";
  set_pf2 e cons "cons";

  set_pf1 e gcar "car";
  set_pf1 e gcdr "cdr";

  set_pf1 e caar "caar";
  set_pf1 e cadr "cadr";
  set_pf1 e cdar "cdar";
  set_pf1 e cddr "cddr";

  set_pf1 e (gcxr [ gcar; gcar; gcar ]) "caaar";
  set_pf1 e (gcxr [ gcar; gcar; gcdr ]) "caadr";
  set_pf1 e (gcxr [ gcar; gcdr; gcar ]) "cadar";
  set_pf1 e (gcxr [ gcar; gcdr; gcdr ]) "caddr";
  set_pf1 e (gcxr [ gcdr; gcar; gcar ]) "cdaar";
  set_pf1 e (gcxr [ gcdr; gcar; gcdr ]) "cdadr";
  set_pf1 e (gcxr [ gcdr; gcdr; gcar ]) "cddar";
  set_pf1 e (gcxr [ gcdr; gcdr; gcdr ]) "cdddr";

  set_pf1 e (gcxr [ gcar; gcar; gcar; gcar ]) "caaaar";
  set_pf1 e (gcxr [ gcar; gcar; gcar; gcdr ]) "caaadr";
  set_pf1 e (gcxr [ gcar; gcar; gcdr; gcar ]) "caadar";
  set_pf1 e (gcxr [ gcar; gcar; gcdr; gcdr ]) "caaddr";
  set_pf1 e (gcxr [ gcar; gcdr; gcar; gcar ]) "cadaar";
  set_pf1 e (gcxr [ gcar; gcdr; gcar; gcdr ]) "cadadr";
  set_pf1 e (gcxr [ gcar; gcdr; gcdr; gcar ]) "caddar";
  set_pf1 e (gcxr [ gcar; gcdr; gcdr; gcdr ]) "cadddr";
  set_pf1 e (gcxr [ gcdr; gcar; gcar; gcar ]) "cdaaar";
  set_pf1 e (gcxr [ gcdr; gcar; gcar; gcdr ]) "cdaadr";
  set_pf1 e (gcxr [ gcdr; gcar; gcdr; gcar ]) "cdadar";
  set_pf1 e (gcxr [ gcdr; gcar; gcdr; gcdr ]) "cdaddr";
  set_pf1 e (gcxr [ gcdr; gcdr; gcar; gcar ]) "cddaar";
  set_pf1 e (gcxr [ gcdr; gcdr; gcar; gcdr ]) "cddadr";
  set_pf1 e (gcxr [ gcdr; gcdr; gcdr; gcar ]) "cdddar";
  set_pf1 e (gcxr [ gcdr; gcdr; gcdr; gcdr ]) "cddddr";

  set_pf2 e set_car "set-car!";
  set_pf2 e set_cdr "set-cdr!";

  set_pf1 e length "length";
  set_pf1 e reverse "reverse";

  set_pfn e append "append";

  set_pf2 e list_tail "list-tail";
  set_pf2 e list_ref "list-ref";

  set_pf2 e memq "memq";
  set_pf2 e memv "memv";
  set_pf2 e member "member";
  set_pf2 e assq "assq";
  set_pf2 e assv "assv";
  set_pf2 e assoc "assoc";

  set_pf1 e list_to_vector "list->vector";
  set_pf1 e list_to_string "list->string";
;;

