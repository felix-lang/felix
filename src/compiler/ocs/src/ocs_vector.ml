(* Vector primitives.  *)

open Ocs_types
open Ocs_error
open Ocs_env

let get_int =
  function
    Sint i -> i
  | _ -> raise (Error "bad arg types")

let make_vector av =
  match Array.length av with
    (1 | 2) as n ->
      let size = get_int av.(0)
      and fill = if n = 2 then av.(1) else Snull in
	Svector (Array.make size fill)
  | _ -> raise (Error "make-vector: wrong number of args")

let vector_of v =
  Svector v
;;

let vector_length =
  function
    Svector v -> Sint (Array.length v)
  | _ -> raise (Error "vector-length: not a vector")
;;

let vector_ref sv i =
  match (sv, i) with
    (Svector vec, Sint i) -> vec.(i)
  | _ -> raise (Error "vector-ref: bad arg types")
;;

let vector_set sv i v =
  match (sv, i) with
    (Svector vec, Sint i) -> vec.(i) <- v; Sunspec
  | _ -> raise (Error "vector-set!: bad arg types")
;;

let vector_to_list =
  function
    Svector v ->
      begin
	let rec loop i r =
	  if i < 0 then r
	  else loop (i - 1) (Spair { car = v.(i); cdr = r })
	in
	  loop (Array.length v - 1) Snull
      end
  | _ -> raise (Error "vector->list: bad args")
;;

let vector_fill sv v =
  match sv with
    Svector vec ->
      for i = 0 to Array.length vec - 1 do
	vec.(i) <- v
      done;
      Sunspec
  | _ -> raise (Error "vector-fill!: bad args")
;;

let init e =
  set_pfn e make_vector "make-vector";

  set_pfn e vector_of "vector";

  set_pf1 e vector_length "vector-length";

  set_pf2 e vector_ref "vector-ref";
  set_pf3 e vector_set "vector-set!";

  set_pf1 e vector_to_list "vector->list";

  set_pf2 e vector_fill "vector-fill!";
;;

