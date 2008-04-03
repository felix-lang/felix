(* Implement functions missing from Complex.  *)

open Complex

let sin_cplx z =
  { re = sin z.re *. cosh z.im;
    im = cos z.re *. sinh z.im }
;;

let cos_cplx z =
  { re = cos z.re *. cosh z.im;
    im = -.(sin z.re) *. sinh z.im }
;;

let tan_cplx z =
  div (sin_cplx z) (cos_cplx z)
;;

(* asin z = -i Ln (iz + sqrt (1 - z^2)) *)

let asin_cplx =
  function { re = x; im = y } ->
    let t = sqrt { re = 1.0 +. y *. y -. x *. x; im = -2.0 *. x *. y } in
    let z = log { re = t.re -. y; im = t.im +. x } in
      { re = z.im; im = -.z.re }
;;

(* acos z = pi/2 - asin z *)

let acos_cplx z =
  match asin_cplx z with
    { re = x; im = y } -> { re = 1.57079632679489661923 -. x; im = -.y }
;;

(* atan z = [Ln (1 + iz) - Ln (1 - iz)] / 2i *)

let atan_cplx =
  function { re = x; im = y } ->
    let t1 = log { re = 1.0 -. y; im = x }
    and t2 = log { re = 1.0 +. y; im = -.x } in
      { re = (t1.im -. t2.im) *. 0.5;
        im = -.(t1.re -. t2.re) *. 0.5 }
;;

