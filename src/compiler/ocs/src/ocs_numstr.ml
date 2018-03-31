(* Conversions between numbers and strings.  *)

open Ocs_types
open Ocs_error
open Ocs_numaux
open Ocs_num
open Ocs_env

(* We need to scan strings and keep track of our position.  *)

type sbuf = {
  s_str : string;
  mutable s_pos : int
}

let speek s =
  if s.s_pos < String.length s.s_str then
    Some s.s_str.[s.s_pos]
  else
    None
;;

let skip s =
  s.s_pos <- s.s_pos + 1
;;

let sget s =
  match speek s with
    (Some _) as c -> skip s; c
  | _ -> None
;;

let ssleft s =
  (String.length s.s_str) - s.s_pos
;;

(* Converting strings to numbers is fairly complex.  We have a lot of
   cases to consider.  *)

type exactness =
    Exact
  | Inexact
  | Undef

let parse_prefix s =
  let rec nextp b e =
    match speek s with
      Some '#' ->
	begin
	  skip s;
	  match sget s with
	    Some ('E' | 'e') ->
	      if e <> Undef then raise (Error "invalid #e") else nextp b Exact
	  | Some ('I' | 'i') ->
	      if e <> Undef then raise (Error "invalid #i") else nextp b Inexact
	  | Some ('B' | 'b') ->
	      if b <> 0 then raise (Error "invalid #b") else nextp 2 e
	  | Some ('O' | 'o') ->
	      if b <> 0 then raise (Error "invalid #o") else nextp 8 e
	  | Some ('D' | 'd') ->
	      if b <> 0 then raise (Error "invalid #d") else nextp 10 e
	  | Some ('X' | 'x') ->
	      if b <> 0 then raise (Error "invalid #x") else nextp 16 e
	  | _ -> raise (Error "invalid prefix")
	end
    | _ -> (b, e)
  in
    nextp 0 Undef
;;

let strtoi s base =
  let n = String.length s
  and am v i =  i + (base * v) in
  let rec loop i v =
    if i >= n then
      v
    else
      loop (i + 1) (am v
	(match s.[i] with
	  '0' .. '9' as c -> int_of_char c - int_of_char '0'
	| 'a' .. 'f' as c -> int_of_char c - int_of_char 'a' + 10
	| 'A' .. 'F' as c -> int_of_char c - int_of_char 'A' + 10
	| _ -> raise (Error "invalid number")))
  in
    loop 0 0
;;

let read_int s base =
  if base = 10 then
    int_of_string s
  else
    strtoi s base
;;

(* The largest integer is at *least* this big (bigger on 64-bit machines).  *)
let max_int = 0x3fffffff
let min_int = -max_int - 1

let parse_num s base =
  let fixsign =
    match speek s with
      Some '-' -> skip s; negate
    | Some '+' -> skip s; fun x -> x
    | _ -> fun x -> x
  and maxv = max_int / base
  and maxi = max_int mod base
  in
    let rec scann v o =
      match speek s with
	Some ('0' .. '9' as c)
	  when (int_of_char c) - (int_of_char '0') < base ->
	    addo v ((int_of_char c) - (int_of_char '0')) o
      | Some ('a' .. 'f' as c) when base = 16 ->
	  addo v ((int_of_char c) - (int_of_char 'a') + 10) o
      | Some ('A' .. 'F' as c) when base = 16 ->
	  addo v ((int_of_char c) - (int_of_char 'A') + 10) o
      | _ -> (v, o)
    and addo v i o =
      skip s;
      if o || v > maxv || (v = maxv && i > maxi) then
	scann 0 true
      else
	scann (v * base + i) o
    and readn () =
      let sp = s.s_pos in
	match scann 0 false with
	  (i, false) ->
	    if s.s_pos = sp then
	      raise (Error "invalid number")
	    else
	      Sint i
	| (_, true) ->
	    raise (Error "too big number")
    in
      let num = readn () in
	match speek s with
	| Some ('+' | '-' | '@') | None -> fixsign num
	| _ -> raise (Error "invalid rational")
;;

let parse_flo10 s =
  let sp = s.s_pos in
    let rec skipd isfirst =
      match speek s with
	Some '0' .. '9' | Some '#' -> skip s; skipd false
      | Some ('+' | '-') when isfirst -> skip s; skipd false
      | _ -> ()
    in
      skipd true;
      if speek s = Some '.' then
	begin
	  skip s;
	  skipd false
	end;
      begin
	match speek s with
	  Some ('E' | 'e' | 'F' | 'f' | 'D' | 'd' | 'S' | 's' | 'L' | 'l') ->
	      skip s; skipd true
	  | _ -> ()
      end;
      let t = Bytes.of_string (String.sub s.s_str sp (s.s_pos - sp)) in
	for i = 0 to Bytes.length t - 1 do
	  match Bytes.get t i with
	    '#' -> Bytes.set t i '0'
	  | 'F' | 'f' | 'D' | 'd' | 'S' | 's' | 'L' | 'l' -> Bytes.set t i 'e'
	  | _ -> ()
	done;
	  try
	    Sreal (float_of_string (Bytes.to_string t))
	  with
	    Failure _ -> raise (Error "invalid float")
;;

let string_to_num str ub =
  let s = { s_str = str; s_pos = 0 } in
  let (base, ex) =
    match parse_prefix s with
      0, x -> if ub = 0 then (10, x) else (ub, x)
    | (b, x) as r ->
      if ub <> 0 && ub <> b then
	raise (Error "Base mismatch")
      else r
  in
    let getn () =
      if base = 10 && ex <> Exact then
	begin
	  let sp = s.s_pos in
	    try
	      parse_num s 10
	    with _ ->
	      s.s_pos <- sp;
	      parse_flo10 s
	end
      else
	parse_num s base
    and fixex n =
      match (ex, n) with
	(Inexact, (Sint _ )) -> promote_real n
      | (Exact, (Sreal _ )) -> raise (Error "Not exact")
      | _ -> n
    in
      let a = fixex (getn ()) in
	match speek s with
	| Some c -> raise (Error "invalid number")
	| None -> a
;;

let snum_strtonum av =
  match Array.length av with
    (1 | 2) as n ->
      let r =
	if n = 2 then
	  begin
	    match av.(1) with
	      Sint i -> i
	    | _ -> raise (Error "string->number: invalid radix")
	  end
	else
	  0
      in
	begin
	  match av.(0) with
	    Sstring s ->
	      begin
		try
		  if Bytes.to_string s = "" then
		    Sfalse
		  else
		    string_to_num (Bytes.to_string s) r
		with
		  _ -> Sfalse
	      end
	  | _ -> raise (Error "string->number: not a string")
	end
  | _ -> raise (Error "string->number: wrong number of args")
;;

let string_of_real_s r =
  let rec loop n =
    let s = Printf.sprintf "%.*g" n r in
      if n >= 25 || r = float_of_string s then s
      else loop (n + 1)
  in
    loop 14
;;

let string_of_real r =
  let s = string_of_real_s r in
  let n = String.length s in
  let rec loop i =
    if i >= n then s ^ ".0"
    else if s.[i] = '.' || s.[i] = 'e' then s
    else loop (i + 1)
  in
    loop 0
;;

let ichr i =
  if i < 10 then
    char_of_int (int_of_char '0' + i)
  else
    char_of_int (int_of_char 'a' + i - 10)
;;

let string_of_list l =
  let n = List.length l in
  let s = Bytes.create n in
  let rec loop i l =
    if i < n then
      begin
	match l with
	  c::t -> Bytes.set s i c; loop (i + 1) t
	| _ -> assert false
      end
    else
      ()
  in
    loop 0 l; Bytes.to_string s
;;

let itostr base i =
  if i = 0 then
    "0"
  else
    let pf = if i < 0 then "-" else "" in
    let rec loop i r =
      if i = 0 then
	r
      else
	loop (i / base) ((ichr (i mod base))::r)
    in
      pf ^ string_of_list (loop (abs i) [])
;;

let ntostr base =
  function
    Sint i -> itostr base i
  | _ -> raise (Error "number->string: invalid radix for inexact number")
;;

let rec snum_numtostr =
  function
    [| Sint i |] -> Sstring (Bytes.of_string (string_of_int i))
  | [| Sreal r |] -> Sstring (Bytes.of_string (string_of_real r))
  | [| snum; Sint radix |] ->
      if radix = 10 then
	snum_numtostr [| snum |]
      else if radix = 2 || radix = 8 || radix = 16 then
	Sstring (Bytes.of_string (ntostr radix snum))
      else
	raise (Error "number->string: invalid radix")
  | _ -> raise (Error "number->string: bad args")
;;

let init e =
  set_pfn e snum_strtonum "string->number";
  set_pfn e snum_numtostr "number->string";
;;


