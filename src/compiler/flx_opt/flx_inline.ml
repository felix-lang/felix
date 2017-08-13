open List
open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_exceptions
open Flx_foldvars
(* open Flx_foldvars2 *)
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_print
open Flx_reparent
open Flx_set
open Flx_spexes
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util
open Flx_bid

let print_time syms msg f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  if syms.Flx_mtypes2.compiler_options.Flx_options.showtime
  then print_endline (String.sub (msg ^ "                                        ") 0 40
        ^ string_of_int (int_of_float elapsed) ^ "s");
  result


(* 
 revariable remaps indices
*)
let ident x = x

(* Heavy inlining routine. This routine can inline
any procedure. The basic operation is emit the body
of the target procedure. We have to do the following to
make it all work.

(1) Each declared label is replaced by a fresh one,
and all jumps to these labels modified accordingly.

(2) Variables are replaced by fresh ones. This requires
making additions to the output bound tables. References
to the variables are modified. Note the parent is the
caller now.

(3) Parameters are replaced like variables, initialised
by the arguments.

(4) Any type variables instantiated by the call must
also be instantiated in body expressions, as well as
the typing of any generated variables.

(5) If the procedure has any nested procedures, they
also must be replaced in toto by fresh ones, reparented
to the caller so that any calls to them will access
the fresh variables in the caller.

Note that the cache of children of the caller will
be wrong after the inlining (it may have acquired new
variables or procedure children).

Note that this inlining procedure is NOT recursive!
Its a flat one level inlining. This ensures recursive
calls don't cause an infinite unrolling, and hopefully
prevent gross bloat.
*)

let idt t = t

(*
(* APPEARS TO BE UNUSED .. *)
let is_simple_expr syms bsym_table e =
  print_endline ("Is " ^ sbe bsym_table e ^ " simple?");
  match e with
  | BEXPR_ref _,_ -> print_endline "YES"; true
  | _ -> print_endline "NO"; false
*)
(* note u sr e must return exes in reverse order, this
  function however returns exes in forward order
*)
let check_reductions syms bsym_table exes = 
  let exes = 
    try 
      Flx_reduce.reduce_exes syms bsym_table !(syms.reductions) exes 
    with Not_found -> assert false
  in
  exes

let make_specialisation syms uses bsym_table
  caller callee id sr parent props exes rescan_flag
=
  (*
  print_endline ("Specialising call " ^ id ^ "<"^si callee ^ ">");
  print_endline ("In procedure " ^ si caller );
  *)
  (*
  print_endline ("Found procedure "^id^": Inline it!");
  *)
  let k=
    specialise_symbol
      syms uses bsym_table
      callee parent rescan_flag
   in
   (*
   print_endline ("Specialised to " ^ id ^ "<"^si k ^ ">" );
   *)
   k

(* Dependency analyser. This should be generalised,
but for now we only use it in tail calls.

We wish to discover what *local* vals an expression e in
some routine i depends on.

These are (a) the symbols manifestly used in the expression,
and (b) any variable used by any function that is called.

We can calculate this, expensively as the union of the
use closures of each symbol in the expression intersected
with the candidate locals.
*)


(* note returns exes in reverse order *)
(* This routine analyses an expression to see if it has  the form

  f a

If so it is replaced by v and a statement v = f a, then
this initialisation is replaced by the body of f
with a replacing the parameter,
where returns are replaced by initialisations of v
and a goto the end of the routine.

Then in the special case the last line of the body
resolves to the form

  v = e'

the expression is replaced by e'. This works by a quirk,
that this code must have come from a sole tail return
in the body. If there were more than one return,
prior returns would be a return to a label after it,
however the inliner doesn't generate the label at the
end for a sole tail return, so we can assume this
is the only return.

The result leaves an expression in a place where
a tail call might be recognized, avoiding a temporary
which prevents simplistic patterns representing data
and control flow. Although its a hack, it is important
to ensure trivial functions have no overhead.

Note this routine, in itself, does NOT rescan anything:
there is no recursion -- other than the recursive traversal
of the original expression, done by the 'aux' function.
*)

(* This function currently checks if it is possible to inline
   a function or procedure, and it also checks if the inline/noinline
   attributes are consistent with the function, however it does
   not force inlining even when the target is a fun-function and contains a service
   call in which case inlining is actually mandatory.

   Forcing the user to correctly tag a function would not help here for two
   reasons: anonymous and generated functions cannot be or are not tagged,
   and functions cloned by the inliner itself may end up with the wrong
   tags (since they more or less just copy the property list instead of
   properly analysing things).

*)
let heavy_inlining syms bsym_table =
  let used = ref (!(syms.roots)) in
  let (uses,usedby) = Flx_call.call_data bsym_table in

  while not (BidSet.is_empty !used) do
    let i = BidSet.choose !used in
    used := BidSet.remove i !used;
    Flx_inline_bbdcl.heavily_inline_bbdcl syms uses bsym_table [i] i
  done;

  (* This code is here to attempt to optimise closures (and clones?)
     which aren't handled by the above loop.
  *)
  Flx_bsym_table.iter begin fun bid _ _ ->
    try Flx_inline_bbdcl.heavily_inline_bbdcl syms uses bsym_table [bid] bid
    with exn -> ()
    (*
      print_endline ("*** ERROR OPTIMISING [ignored?] " ^ si i);
      print_endline (Printexc.to_string exn);
      raise exn
    *)
  end bsym_table


(* NOTES: this algorithm ONLY WORKS if inlining is attempted
in the corect order. Attempting to inline into children
before parents, when they're mutually recursive, spawns
clones infinitely, because we end up cloning a function
on the exclusion list, but not adding the clone to it.


NOTE!!!! THIS SHOULD BE FIXED NOW. WE NO LONGER
PERMIT INLINING RECURSIVE FUNCTIONS UNLESS THE CALL
IS TO A CHILD. A CALL TO SELF, PARENT OR SIBLING NEVER
DOES INLINING .. AND THERE ARE NO OTHER CASES.

INLINING KIDS IS MANDATORY FOR TAIL RECURSION OPTIMISATION.

So we end up recursing into the clone, and inlining
into it, which spawns more clones which are not
excluded, and haven't been inlined into yet.

This needs to be fixed so the algorithm is proven
to terminate and also be complete.

What we need (and is NOT implemented) is something like this:

Cloning nested functions is should not be needed in general.
If we proceed from leaves towards the root, we can eliminate
from each function any nested children, by simply inlining
them. So only variable children need cloning.

Two things stop this working:

(a) non-inline functions and
(b) recursion.

The current algorithm has been hacked to only handle the
call graph from the roots. It used to consider the useage
closure, however that started to fail when I added
'pre-assigned' slot numbers (AST_index). Doing that meant
the natural order of the set wasn't a topological sort
of the parent-child order.

Unfortunately, the remaining recursive descent doesn't
proceed into noinline functions. Although these shouldn't
be inlined into their caller, that doesn't mean functions
shouldn't be inlined into them. Iterating over the usage
closure ensured noinline functions would still be inlined
into.

Recursive functions are a bit different: they currently
allow inlining, with a recursion stopper preventing
infinite recursion.

Unfortunately with a double nesting like this:

  fun f() { fun g() { fun h() { f(); } h(); } g(); }

trying to inline g into f causes h to be cloned.
But trying to inline f into the clone of h retriggers
the descent, causing the clone to be recloned, and
the recursion stopper doesn't prevent this, since it
isn't the same routine being inlined twice (just a clone
of it ..)

The thing is.. we HAVE to inline the original routine
AND the clone for completeness, since both may be
called independently, so even if we could clone the
recursion stoppers, it wouldn't work.

The only solution I can think of is to guarrantee that
you can only clone a routine that is inlined into
already (as fas as possible) so that no attempt will
be made to inline into the clone either.
--------------------------------------------------------------
Hum.... When I inline A -> B -> C -> A (all kid inlines) the
inline of A into C is done first. This creates clones B' and C'.
When we rescan the code to be put into C, we would try to
inline B' into it, and C' into that .. but C' is a cloned sibling
of C, and not the same function. So we try to inline into C',
and inlining A is allowed there .. which causes an infinite
recursion.
*)

