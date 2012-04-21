(* Handle number types (Sint, Sreal, Sbigint, Srational, Scomplex) *)

open Ocs_types
open Ocs_error
open Ocs_env
open Ocs_numaux
open Ocs_complex

open Num
open Ratio
open Big_int

let rec negate =
  function
    (Sint i) as s ->
      if i >= min_int then Sint (-i)
      else negate (promote_bigint s)
  | Sbigint bi -> Sbigint (minus_big_int bi)
  | Srational r -> Srational (Ratio.minus_ratio r)
  | Sreal r -> Sreal (-.r)
  | Scomplex z -> Scomplex (Complex.neg z)
  | _ -> raise (Error "bad number type")
;;

let add2 a b =
  match (a, b) with
    (Sint i1, Sint i2) -> 
      let r = i1 + i2 in
	if (i1 lxor i2) lor (i1 lxor (r lxor (-1))) < 0
	then Sint r
	else Sbigint (add_big_int (big_int_of_int i1) (big_int_of_int i2))
  | (Scomplex z1, Scomplex z2) -> Scomplex (Complex.add z1 z2)
  | (Scomplex z, o) | (o, Scomplex z) ->
      Scomplex (Complex.add z { Complex.re = float_of_snum o;
			        Complex.im = 0.0 })
  | (Sreal r, o) | (o, Sreal r) ->
      Sreal ((float_of_snum o) +. r)
  | (Srational r, o) | (o, Srational r) ->
      Srational (add_ratio (rational_of_snum o) r)
  | (Sbigint bi, o) | (o, Sbigint bi) ->
      bigint_res (add_big_int (bigint_of_snum o) bi)
  | _ -> raise (Error "add: bad types")
;;

let sub2 a b =
  add2 a (negate b)
;;

let mul2 a b =
  match snum_fixtypes a b with
    (Scomplex z1, Scomplex z2) -> Scomplex (Complex.mul z1 z2)
  | (Sreal r1, Sreal r2) -> Sreal (r1 *. r2)
  | (Srational r1, Srational r2) ->
    snum_of_num (mult_num (Ratio r1) (Ratio r2))
  | (Sbigint bi1, Sbigint bi2) ->
    snum_of_num (mult_num (Big_int bi1) (Big_int bi2))
  | (Sint i1, Sint i2) ->
    snum_of_num (mult_num (Int i1) (Int i2))
  | _ -> raise (Error "mul: invalid args")
;;

let div2 a b =
  match snum_fixtypes a b with
    (Scomplex n, Scomplex d) -> Scomplex (Complex.div n d)
  | (Sreal n, Sreal d) -> Sreal (n /. d)
  | (Srational n, Srational d) ->
      snum_of_num (div_num (Ratio n) (Ratio d))
  | (Sbigint n, Sbigint d) ->
      snum_of_num (div_num (Big_int n) (Big_int d))
  | (Sint n, Sint d) ->
      if d = 0 then
	raise (Error "division by zero")
      else
	snum_of_num (div_num (Int n) (Int d))
  | _ -> raise (Error "div: invalid args")
;;

let cmp2 eq_only a b =
  match (a, b) with
    (Sint i1, Sint i2) ->
      if i1 > i2 then 1 else if i1 < i2 then -1 else 0
  | _ ->
    begin
      match snum_fixtypes a b with
	(Sreal r1, Sreal r2) ->
	  let r = r1 -. r2 in
	    if r > 0.0 then 1 else if r < 0.0 then -1 else 0
      | (Scomplex z1, Scomplex z2) ->
	  if eq_only then
	    if z1 = z2 then 0 else 1
	  else
	    if z1.Complex.im = 0.0 && z2.Complex.im = 0.0 then
	      begin
		let r = z1.Complex.re -. z2.Complex.re in
		  if r > 0.0 then 1 else if r < 0.0 then -1 else 0
	      end
	    else
	      raise (Error "complex numbers compared")
      | (Srational r1, Srational r2) ->
	  compare_ratio r1 r2
      | (Sbigint bi1, Sbigint bi2) ->
	  compare_big_int bi1 bi2
      | _ -> raise (Error "cmp: invalid args")
    end
;;

let is_exact =
  function
    (Sint _ | Sbigint _ | Srational _) -> Strue
  | _ -> Sfalse
;;

let is_inexact =
  function
    (Sreal _ | Scomplex _) -> Strue
  | _ -> Sfalse
;;

let to_exact =
  function
    Sreal r -> float_to_exact r
  | Scomplex z ->
      if z.Complex.im = 0.0 then
	float_to_exact z.Complex.re
      else
	raise (Error "inexact->exact: no exact complex representation")
  | (Sint _ | Sbigint _ | Srational _) as n -> n
  | _ -> raise (Error "inexact->exact: not a number")
;;

let to_inexact =
  function
    Sreal _ as r -> r
  | Scomplex _ as z -> z
  | x -> Sreal (float_of_snum x)
;;

let mkbool b = if b then Strue else Sfalse;;

let is_zero =
  function
    Sint i -> mkbool (i = 0)
  | (Sbigint _ | Srational _) -> Sfalse
  | Sreal r -> mkbool (r = 0.0)
  | Scomplex z -> mkbool (z.Complex.re = 0.0 && z.Complex.im = 0.0)
  | _ -> Sfalse
;;

let is_positive =
  function
    Sint i -> mkbool (i > 0)
  | Sbigint bi -> mkbool (sign_big_int bi > 0)
  | Srational r -> mkbool (sign_ratio r > 0)
  | Sreal r -> mkbool (r > 0.0)
  | _ -> raise (Error "positive?: bad arg type")
;;

let is_negative =
  function
    Sint i -> mkbool (i < 0)
  | Sbigint bi -> mkbool (sign_big_int bi < 0)
  | Srational r -> mkbool (sign_ratio r < 0)
  | Sreal r -> mkbool (r < 0.0)
  | _ -> raise (Error "positive?: bad arg type")
;;

let is_number =
  function
    Sint _ | Sbigint _ | Srational _ | Sreal _ | Scomplex _ -> Strue
  | _ -> Sfalse
;;

let is_real =
  function
    Sint _ | Sbigint _ | Srational _ | Sreal _ -> Strue
  | Scomplex { Complex.re = _; Complex.im = i } -> mkbool (i = 0.0)
  | _ -> Sfalse
;;

let is_rational =
  is_real
;;

let is_integer =
  function
    Sint _ | Sbigint _ -> Strue
  | Sreal r -> mkbool (float_is_int r)
  | Scomplex z -> mkbool (z.Complex.im = 0.0 && float_is_int z.Complex.re)
  | _ -> Sfalse
;;

let bi_modi bi i =
  int_of_big_int (mod_big_int bi (big_int_of_int i))
;;

let is_even =
  function
    Sint i -> mkbool (i land 1 = 0)
  | Sbigint bi -> mkbool (bi_modi bi 2 = 0)
  | _ -> raise (Error "even?: bad arg type")
;;

let is_odd =
  function
    Sint i -> mkbool (i land 1 <> 0)
  | Sbigint bi -> mkbool (bi_modi bi 2 <> 0)
  | _ -> raise (Error "odd?: bad arg type")
;;

let do_ops op av n =
  let rec oploop v i =
    if i < n then
      let r = op v av.(i) in
	oploop r (i + 1)
    else
      v
  in
    oploop av.(0) 1
;;

let snum_add av =
  let n = Array.length av in
    if n = 0 then
      Sint 0
    else
      do_ops add2 av n
;;

let snum_sub av =
  match Array.length av with
    0 -> raise (Error "-: need args")
  | 1 -> negate av.(0)
  | n -> do_ops sub2 av n
;;

let snum_mul av =
  match Array.length av with
    0 -> Sint 1
  | n -> do_ops mul2 av n
;;

let snum_div av =
  match Array.length av with
    0 -> raise (Error "/: need args")
  | 1 -> div2 (Sint 1) av.(0)
  | n -> do_ops div2 av n
;;

let snum_eq av =
  match Array.length av with
    0 | 1 -> Strue
  | n ->
      let a0 = av.(0) in
      let rec loop i =
	if i < n then
	  begin
	    if cmp2 true a0 av.(i) <> 0 then Sfalse
	    else loop (i + 1)
	  end
	else
	  Strue
      in
	loop 1
;;

let snum_rel op av =
  match Array.length av with
    0 | 1 -> Strue
  | n ->
      let rec loop v i =
	if i < n then
	  begin
	    if op (cmp2 false v av.(i)) 0 then loop av.(i) (i + 1)
	    else Sfalse
	  end
	else
	  Strue
      in
	loop av.(0) 1
;;

let snum_minormax op av =
  match Array.length av with
    0 -> raise (Error "args required")
  | 1 -> av.(0)
  | n ->
      let inex = ref false in
      let r = do_ops (fun a b ->
			if is_inexact a = Strue || is_inexact b = Strue then
			  inex := true;
			if op (cmp2 false a b) 0 then a else b) av n
      in
	if !inex then to_inexact r else r
;;

let snum_abs =
  function
    Sint i -> Sint (abs i)
  | Sbigint bi -> Sbigint (abs_big_int bi)
  | Sreal r -> Sreal (abs_float r)
  | Srational r -> Srational (abs_ratio r)
  | Scomplex _ -> raise (Error "abs: number is complex")
  | _ -> raise (Error "abs: not a number")
;;

let snum_floor =
  function
    (Sint _ | Sbigint _) as x -> x
  | Srational r -> bigint_res (floor_ratio r)
  | Sreal r -> Sreal (floor r)
  | _ -> raise (Error "floor: bad arg type")
;;

let snum_ceiling =
  function
    (Sint _ | Sbigint _) as x -> x
  | Srational r -> bigint_res (ceiling_ratio r)
  | Sreal r -> Sreal (ceil r)
  | _ -> raise (Error "ceiling: bad arg type")
;;

let snum_truncate =
  function
    (Sint _ | Sbigint _) as x -> x
  | Srational r -> bigint_res (integer_ratio r)
  | Sreal r ->
      if r < 0.0 then
	Sreal (ceil r)
      else
	Sreal (floor r)
  | _ -> raise (Error "truncate: bad arg type")
;;

let snum_round =
  function
    (Sint _ | Sbigint _) as x -> x
  | Srational r -> bigint_res (round_ratio r)
  | Sreal r -> Sreal (round_float r)
  | _ -> raise (Error "round: bad arg type")
;;

let rcsw rfun cfun =
  function
    Scomplex z -> Scomplex (cfun z)
  | x -> Sreal (rfun (float_of_snum x))
;;

let snum_exp = rcsw exp Complex.exp;;
let snum_log = rcsw log Complex.log;;

let snum_sin = rcsw sin sin_cplx;;
let snum_cos = rcsw cos cos_cplx;;
let snum_tan = rcsw tan tan_cplx;;
let snum_asin = rcsw asin asin_cplx;;
let snum_acos = rcsw acos acos_cplx;;

let snum_atan =
  function
    [| x |] -> rcsw atan atan_cplx x
  | [| y; x |] ->
    Sreal (Complex.arg { Complex.re = float_of_snum x;
			 Complex.im = float_of_snum y })
  | _ -> raise (Error "atan: bad args")
;;

let snum_sqrt =
  function
    Scomplex z -> Scomplex (Complex.sqrt z)
  | x ->
      let r = float_of_snum x in
	if r < 0.0 then
	  Scomplex (Complex.sqrt { Complex.re = r; Complex.im = 0.0 })
	else
	  let sq = sqrt r in
	    if is_exact x <> Sfalse && float_is_int sq then
	      float_to_exact sq
	    else
	      Sreal sq
;;

(* Optimize the simplest cases, leave the rest to Num.  *)
let snum_expt x y =
  match (x, y) with
    (_, Sint n) when n = 0 -> Sint 1
  | ((Sint _ | Sbigint _), Sint n) when n > 0 ->
      bigint_res (power_big_int_positive_int (bigint_of_snum x) n)
  | ((Sint _ | Sbigint _ | Srational _), (Sint _ | Sbigint _)) ->
      snum_of_num (power_num (num_of_snum x) (num_of_snum y))
  | (Scomplex _, _) | (_, Scomplex _) ->
      Scomplex (Complex.pow (complex_of_snum x) (complex_of_snum y))
  | _ ->
      Sreal (float_of_snum x ** float_of_snum y)
;;

let make_rectangular x y =
  Scomplex { Complex.re = float_of_snum x; Complex.im = float_of_snum y }
;;

let make_polar x y =
  Scomplex (Complex.polar (float_of_snum x) (float_of_snum y))
;;

let real_part x =
  Sreal (complex_of_snum x).Complex.re
;;

let imag_part x =
  Sreal (complex_of_snum x).Complex.im
;;

let magnitude x =
  Sreal (Complex.norm (complex_of_snum x))
;;

let angle x =
  Sreal (Complex.arg (complex_of_snum x))
;;

let quotient n d =
  match (n, d) with
    ((Sint _ | Sbigint _ | Srational _),
     (Sint _ | Sbigint _ | Srational _)) ->
      snum_of_num (integer_num (div_num (num_of_snum n) (num_of_snum d)))
  | _ ->
      let n = float_of_snum n
      and d = float_of_snum d in
	if not (float_is_int n && float_is_int d) then
	  raise (Error "quotient: non-integer arguments")
	else
	  Sreal (n /. d -. (mod_float n d) /. d)
;;

let remainder n d =
  match (n, d) with
    ((Sint _ | Sbigint _ | Srational _),
     (Sint _ | Sbigint _ | Srational _)) ->
      let n = num_of_snum n
      and d = num_of_snum d in
      let m = mod_num n d in
	if sign_num n + sign_num d = 0 then
	  snum_of_num (sub_num m d)
	else
	  snum_of_num m
  | _ ->
      let n = float_of_snum n
      and d = float_of_snum d in
	if not (float_is_int n && float_is_int d) then
	  raise (Error "quotient: non-integer arguments")
	else
	  Sreal (mod_float n d)
;;

let modulo n d =
  match (n, d) with
    ((Sint _ | Sbigint _ | Srational _),
     (Sint _ | Sbigint _ | Srational _)) ->
      snum_of_num (mod_num (num_of_snum n) (num_of_snum d))
  | _ ->
      let n = float_of_snum n
      and d = float_of_snum d in
	if not (float_is_int n && float_is_int d) then
	  raise (Error "quotient: non-integer arguments")
	else
	  let m = mod_float n d in
	    if (n < 0.0 && d > 0.0) || (n > 0.0 && d < 0.0) then
	      Sreal (d +. m)
	    else
	      Sreal m
;;

(* Compute the gcd of two numbers *)
let rec gcd2 a b =
  match (a, b) with
    ((Sint _ | Sbigint _), (Sint _ | Sbigint _)) ->
      bigint_res (gcd_big_int (bigint_of_snum (snum_abs a))
			      (bigint_of_snum (snum_abs b)))
  | _ ->
      if is_integer a <> Sfalse && is_integer b <> Sfalse then
	to_inexact (gcd2 (to_exact a) (to_exact b))
      else
	raise (Error "gcd: non-integer arguments")
;;

let snum_gcd av =
  let n = Array.length av in
    if n = 0 then
      Sint 0
    else
      do_ops gcd2 av n
;;

let lcm2 a b =
  let g = gcd2 a b in
    snum_abs (mul2 (div2 a g) b)
;;

let snum_lcm av =
  let n = Array.length av in
    if n = 0 then
      Sint 1
    else
      do_ops lcm2 av n
;;

(* The algorithm for calculating the simplest rational form of a number
   is translated from the appendix in the IEEE draft.  It implicitly
   preserves exactness.  *)

let simplest_rational x y =
  let one = Sint 1 in
  let rec sri x y =
    let fx = snum_floor x
    and fy = snum_floor y in
      if cmp2 false fx x >= 0 then
	fx
      else if cmp2 true fx fy = 0 then
	add2 fx
	  (div2 one (sri (div2 one (sub2 y fy)) (div2 one (sub2 x fx))))
      else
	add2 fx one
  in
    if cmp2 false y x < 0 then
      sri y x
    else if cmp2 false x y >= 0 then
      begin
	if is_rational x <> Sfalse then
	  x
	else
	  raise (Error "rationalize: not a rational")
      end
    else if is_positive x <> Sfalse then
      sri x y
    else if is_negative y <> Sfalse then
      negate (sri (negate y) (negate x))
    else if is_exact x <> Sfalse && is_exact y <> Sfalse then
      Sint 0
    else
      Sreal 0.0
;;

let snum_rationalize x e =
  simplest_rational (sub2 x e) (add2 x e)
;;

let rec snum_numerator =
  function
    (Sint _ | Sbigint _) as x -> x
  | Srational q -> bigint_res (numerator_ratio q)
  | Sreal _ as x -> to_inexact (snum_numerator (to_exact x))
  | Scomplex _ -> raise (Error "numerator: not defined for complex numbers")
  | _ -> raise (Error "numerator: not a numeric type")
;;

let rec snum_denominator =
  function
    Sint _ | Sbigint _ -> Sint 1
  | Srational q -> bigint_res (denominator_ratio q)
  | Sreal r as x ->
      if r = 0.0 then Sreal 1.0 else
	to_inexact (snum_denominator (to_exact x))
  | Scomplex _ -> raise (Error "denominator: not defined for complex numbers")
  | _ -> raise (Error "denominator: not a numeric type")
;;

let init e =
  set_pf1 e is_exact "exact?";
  set_pf1 e is_inexact "inexact?";
  set_pf1 e is_zero "zero?";
  set_pf1 e is_positive "positive?";
  set_pf1 e is_negative "negative?";
  set_pf1 e is_number "number?";
  set_pf1 e is_number "complex?";
  set_pf1 e is_real "real?";
  set_pf1 e is_rational "rational?";
  set_pf1 e is_integer "integer?";
  set_pf1 e is_even "even?";
  set_pf1 e is_odd "odd?";

  set_pfn e snum_add "+";
  set_pfn e snum_sub "-";
  set_pfn e snum_mul "*";
  set_pfn e snum_div "/";
  set_pfn e snum_eq "=";
  set_pfn e (snum_rel (>)) ">";
  set_pfn e (snum_rel (<)) "<";
  set_pfn e (snum_rel (>=)) ">=";
  set_pfn e (snum_rel (<=)) "<=";

  set_pfn e (snum_minormax (>)) "max";
  set_pfn e (snum_minormax (<)) "min";

  set_pf1 e snum_abs "abs";

  set_pf1 e snum_floor "floor";
  set_pf1 e snum_ceiling "ceiling";
  set_pf1 e snum_truncate "truncate";
  set_pf1 e snum_round "round";

  set_pf1 e snum_exp "exp";
  set_pf1 e snum_log "log";

  set_pf1 e snum_sin "sin";
  set_pf1 e snum_cos "cos";
  set_pf1 e snum_tan "tan";
  set_pf1 e snum_asin "asin";
  set_pf1 e snum_acos "acos";
  set_pfn e snum_atan "atan";

  set_pf1 e snum_sqrt "sqrt";
  set_pf2 e snum_expt "expt";

  set_pf2 e make_rectangular "make-rectangular";
  set_pf2 e make_polar "make-polar";
  set_pf1 e real_part "real-part";
  set_pf1 e imag_part "imag-part";
  set_pf1 e magnitude "magnitude";
  set_pf1 e angle "angle";

  set_pf2 e quotient "quotient";
  set_pf2 e remainder "remainder";
  set_pf2 e modulo "modulo";

  set_pf1 e to_exact "inexact->exact";
  set_pf1 e to_inexact "exact->inexact";

  set_pfn e snum_gcd "gcd";
  set_pfn e snum_lcm "lcm";

  set_pf2 e snum_rationalize "rationalize";

  set_pf1 e snum_numerator "numerator";
  set_pf1 e snum_denominator "denominator";
;;
