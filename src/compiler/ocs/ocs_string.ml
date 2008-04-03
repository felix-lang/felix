(* String primitives *)

open Ocs_types
open Ocs_error
open Ocs_env

let make_string =
  function 
    [| Sint k |] -> Sstring (String.create k)
  | [| Sint k; Schar c |] -> Sstring (String.make k c)
  | _ -> raise (Error "make-string: bad args")
;;

let string_of av =
  let n = Array.length av in
  let s = String.create n in
    for i = 0 to n - 1 do
      match av.(i) with
	Schar c -> s.[i] <- c
      | _ -> raise (Error "string: bad args")
    done;
    Sstring s
;;

let string_length =
  function
    Sstring s -> Sint (String.length s)
  | _ -> raise (Error "string-length: not a string")
;;

let string_ref s k =
  match (s, k) with
    (Sstring s, Sint k) ->
      if k >= 0 && k < String.length s then
	Schar s.[k]
      else
	raise (Error "string-ref: out of bounds")
  | _ -> raise (Error "string-ref: bad args")
;;

let string_set s k c =
  match (s, k, c) with
    (Sstring s, Sint k, Schar c) ->
      if k >= 0 && k < String.length s then
	begin
	  s.[k] <- c; Sunspec
	end
      else
	raise (Error "string-set!: out of bounds")
  | _ -> raise (Error "string-set!: bad args")
;;

let string_cmp op s1 s2 =
  match (s1, s2) with
    (Sstring s1, Sstring s2) -> if op s1 s2 then Strue else Sfalse
  | _ -> raise (Error "bad args")
;;

let string_eq = string_cmp (=);;
let string_lt = string_cmp (<);;
let string_gt = string_cmp (>);;
let string_le = string_cmp (<=);;
let string_ge = string_cmp (>=);;

let string_ci_cmp op s1 s2 =
  match (s1, s2) with
    (Sstring s1, Sstring s2) ->
      if op (String.lowercase s1) (String.lowercase s2) then Strue else Sfalse
  | _ -> raise (Error "bad args")
;;

let string_ci_eq = string_ci_cmp (=);;
let string_ci_lt = string_ci_cmp (<);;
let string_ci_gt = string_ci_cmp (>);;
let string_ci_le = string_ci_cmp (<=);;
let string_ci_ge = string_ci_cmp (>=);;

let string_append av =
  Sstring
    (Array.fold_left (^) ""
      (Array.map (function
                    Sstring s -> s
                  | _ -> raise (Error "string-append: bad args")) av))
;;

let substring s sp ep =
  match (s, sp, ep) with
    (Sstring s, Sint sp, Sint ep) ->
      let n = String.length s in
	if sp >= 0 && sp <= ep && ep <= n then
	  Sstring (String.sub s sp (ep - sp))
	else
	  raise (Error "substring: out of bounds")
  | _ -> raise (Error "substring: bad args")
;;

let string_to_list =
  function
    Sstring s ->
      begin
	let rec loop i r =
	  if i < 0 then r
	  else loop (i - 1) (Spair { car = Schar s.[i]; cdr = r })
	in
	  loop (String.length s - 1) Snull
      end
  | _ -> raise (Error "string->list: not a string")
;;

let string_copy =
  function
    Sstring s -> Sstring (String.copy s)
  | _ -> raise (Error "string-copy: not a string")
;;

let string_fill s c =
  match (s, c) with
    (Sstring s, Schar c) ->
      String.fill s 0 (String.length s) c; Sunspec
  | _ -> raise (Error "string-fill!: bad args")
;;

let init e =
  set_pfn e make_string "make-string";
  set_pfn e string_of "string";

  set_pf1 e string_length "string-length";

  set_pf2 e string_ref "string-ref";
  set_pf3 e string_set "string-set!";

  set_pf2 e string_eq "string=?";
  set_pf2 e string_lt "string<?";
  set_pf2 e string_gt "string>?";
  set_pf2 e string_le "string<=?";
  set_pf2 e string_ge "string>=?";

  set_pf2 e string_ci_eq "string-ci=?";
  set_pf2 e string_ci_lt "string-ci<?";
  set_pf2 e string_ci_gt "string-ci>?";
  set_pf2 e string_ci_le "string-ci<=?";
  set_pf2 e string_ci_ge "string-ci>=?";

  set_pf3 e substring "substring";

  set_pfn e string_append "string-append";

  set_pf1 e string_to_list "string->list";

  set_pf1 e string_copy "string-copy";

  set_pf2 e string_fill "string-fill!";
;;
