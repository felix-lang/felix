(** Constant folding
 *
 * These routines provide constant folding.  Felix recognizes plain integers,
 * strings, and boolean values as constants, and folds expressions involving
 * all the basic operations on them as follows:
 * 
 * {[
   * // integers
   * -x, neg x
   * +x, pos x
   * abs x
   * x+y, add (x,y)
   * x-y, sub(x,y)
   * x*y, mul(x,y)
   * x/y, div(x,y)
   * x % y, mod(x,y)
   * x ** y, pow(x,y)
   * x<y, lt(x,y)
   * x>y, gt(x,y)
   * x<=y, le(x,y)
   * x>=y, ge(x,y)
   * x=y, eq(x,y)
   * x!=y, x<>y,ne(x,y)

   * // strings
   * x+y, add
   * x*n, mul(x,n) // concatenate n copies of x
   * x=y, eq(x,y)
   * x!=y, x<>y,ne(x,y)
   * x n // append the utf8 encoding of n to x
   * x y // same as x + y

   * // ustrings
   * x+y, add
   * x*n, mul(x,n) // concatenate n copies of x
   * x=y, eq(x,y)
   * x!=y, x<>y,ne(x,y)
   * x n // append the char of code value n to x

   * // bool
   * not x, lnot(x,y)
   * x or y, lor(x,y)
   * x and y, land(x,y)
   * x=y, eq(x,y)
   * x!=y, x<>y,ne(x,y)

   * // conditional
   * if c then e1 else e2 endif // compile time shortcut
 * ]}
 * 
 * Note the conditional fold, which replaces the conditional with either e1 or
 * e2 if c is a boolean constant expression.
 *
 * Note the string formation forms:
 *
 * {[
   * "" 27 987
   * u"" 27 987
 * ]}
 *
 * have the same ISO-10646 interpretation, however the first string is 8 bit,
 * and the 987 is replaced by its UTF-8 encoding, whilst the second string is
 * 32 bit, and the integral value is represented directly.
 *
 * Implementation note: both string types are represented internally by 8 bit
 * UTF-8 encoded strings.
 *)

open Flx_ast
val const_fold:
  expr_t -> expr_t
