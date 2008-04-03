(* Miscellaneous utility functions *)

open Ocs_types
open Ocs_error

let list_to_caml l =
  let rec loop r =
    function
      Snull -> List.rev r
    | Spair p -> loop (p.car::r) p.cdr
    | _ -> raise (Error "not a valid list")
  in
    loop [] l
;;

(* Create a Scheme list from a reversed native list.  *)
let make_slist tl l =
  let rec loop r =
    function
      h::t -> loop (Spair { car = h; cdr = r }) t
    | [] -> r
  in
    loop tl l
;;

(* Create one of Capply[0123n] depending on the number of arguments.  *)
let mkapply f av =
  match av with
    [| |] -> Capply0 (f)
  | [| a1 |] -> Capply1 (f, a1)
  | [| a1; a2 |] -> Capply2 (f, a1, a2)
  | [| a1; a2; a3 |] -> Capply3 (f, a1, a2, a3)
  | av -> Capplyn (f, av)
;;

(* Test equivalence (as in eqv?) *)

let test_eqv a b =
  if a == b then true
  else
    match (a, b) with
      (Sint i1, Sint i2) -> i1 = i2
    | (Schar c1, Schar c2) -> c1 = c2
    | (Sreal r1, Sreal r2) -> r1 = r2
    | (Sbigint bi1, Sbigint bi2) -> Big_int.compare_big_int bi1 bi2 = 0
    | (Srational r1, Srational r2) -> Ratio.compare_ratio r1 r2 = 0
    | (Scomplex z1, Scomplex z2) -> z1 = z2
    | (Sstring s1, Sstring s2) -> s1 = s2
    | (Svector v1, Svector v2) -> v1 == v2
    | _ -> false
;;

(* Test equality (as in equal?) *)

let rec test_equal a b =
  if a == b then true
  else
    match (a, b) with
      (Svector v1, Svector v2) ->
	  let n = Array.length v1 in
	    if Array.length v2 <> n then
	      false
	    else
	      let rec loop i =
		if i >= n then
		  true
		else
		  if test_equal v1.(i) v2.(i) then
		    loop (i + 1)
		  else
		    false
	      in
		loop 0
    | (Spair p1, Spair p2) ->
	test_equal p1.car p2.car && test_equal p1.cdr p2.cdr
    | _ -> test_eqv a b
;;
