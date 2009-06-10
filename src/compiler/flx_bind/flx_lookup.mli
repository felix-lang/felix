(** Name lookup
 *
 * There are some tricky issues with the name binding rules. First, name
 * binding is complicated by the fact we support overloading. This defeats a
 * simple linear binding scheme: instead, we need to bind the type of the
 * argument of the application of a named function.
 *
 * In addition, felix provides simple first order generics by allowing declared
 * name to be parameterised by types.
 *
 * There are two names then: simple and indexed.  Indexed names must of course
 * refer to declarations with the right number of parameters.
 *
 * However, a non-indexed name may refer to a non-generic entity, or, refer to a
 * local generic entity, in which case the arguments are just the list of
 * parameter names.
 *
 * Actually we can further generalise because of nesting.  Name binding consists
 * of uniquely identifying every name, and replacing the concrete name with its
 * canonical representation. Each declared name is number in order of writing,
 * and takes type parameters in a single list which is the concatenation of the
 * visible parameters in order of writing, in other words starting with the
 * outermost construction: we can assume all names are parameterised by a list
 * of types, modeling non-generic names as if they had 0 type parameters.
 *
 * We need to note now how our code is driven.  We start with certain
 * non-generic root functions, and recurse through the call structure. In the
 * root of course, the type arguments used for a name must selves be monomorphic
 * (free of type variables), so the binding itself is monomorphic.
 *
 * What this all means is that routines like bind_type and bind_expression are
 * always accepting and returning monomorphic data. What all this means is that
 * the indexing scheme never needs any bound type variables: a name denoting a
 * type parameter is always being replaced by a monotype directly, without any
 * need to first go to variables and then instantiate them.
 *
 * Hmm .. messy .. consider:
 *
 * {[
   * val x0 = 1;
   * module p1[t1] {
   *   val x1 = x0;
   *   module p2[t2] {
   *     va1 x2a = x1 + x2; // x1[t1] + x2[t1,t2]
   *     va1 x2b = x1 + p2[int]::x2; // x1[t1] + x2[t1,int]
   * 
   *   .. fine .. but the equivalent function structure:
   * 
   * val x0 = 1;
   * proc p1[t1]() {
   *   val x1 = x0;
   *   proc p2[t2]() {
   *     va1 x2a = x1 + x2; // x1[t1] + x2[t1,t2]
   *     // explicit indexing here is not allowed
   *     // for *variables* since
   *     // we have to refer to a a stack from on
   *     // the display which has fixed type
   *     // parameters .. but it IS allowed for
   *     // enclosed types (since type are static ..)
 * ]}    
 *
 * SUMMARY .. the total number of variables needed to instaniate a name is the
 * length of the list of the concatenation of the type vaiable lists of the
 * entities ancestors including itself.  If any indexes are given explicitly,
 * they're always most local, and replace the last so many bindings from
 * context. Note the number of *implicit* variables needed may be less than
 * those given if the name is defined in a parent: in this case we just take
 * first part of the argument list.
 *
 * With this mechanism a simply list of bound type indices suffices provided
 * when a lookup is done we calculate how many values are needed.
 *
 * Hmm: this may cause a LOT of pain, if we're looking up generic functions ..
 * since we assumed the lookup could select on the number of arguments .. well,
 * it can, by adjusting as the search deepens .. nice!
 *
 * Technology: given an index i, find its vs list including that of its parents
 * (string -> int) form.
 *)

open Flx_ast
open Flx_types
open Flx_mtypes2
open Flx_overload

val lookup_name_in_htab:
  name_map_t ->
  string ->
  entry_set_t option

val build_env:
  sym_state_t ->
  int option -> (* parent *)
  env_t

val lookup_name_in_env :
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  id_t ->
  entry_set_t

val lookup_qn_in_env :
  sym_state_t ->
  env_t ->
  qualified_name_t ->
  entry_kind_t * typecode_t list

val lookup_qn_in_env2:
  sym_state_t ->
  env_t ->
  qualified_name_t ->
  entry_set_t * typecode_t list

val lookup_sn_in_env :
  sym_state_t ->
  env_t ->
  suffixed_name_t ->
  int * btypecode_t list

val lookup_code_in_env:
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  qualified_name_t ->
  entry_kind_t list * typecode_t list

(** This routine takes an unbound type term
and binds it. The term may contain explicit
type variables. If the term denotes a generative
type (abstract, union, or struct) then an instance
is made with type variables for the indices.

Note that the result of binding a term with type
variables is not a type function.
*)

val bind_type:
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  typecode_t ->
  btypecode_t

val eval_module_expr:
  sym_state_t ->
  env_t ->
  expr_t ->
  module_rep_t

val resolve_overload:
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  entry_kind_t list ->
  id_t ->
  btypecode_t list ->
  btypecode_t list ->      (* explicit param/arg bindings *)
  overload_result option

val bind_expression :
  sym_state_t ->
  env_t ->
  expr_t ->
  tbexpr_t

val bind_expression_with_args :
  sym_state_t ->
  env_t ->
  expr_t ->
  tbexpr_t list ->
  tbexpr_t

val type_of_index :
  sym_state_t ->
  int ->
  btypecode_t

val type_of_index_with_ts:
  sym_state_t ->
  Flx_srcref.t ->
  int ->
  btypecode_t list ->
  btypecode_t

val type_of_literal:
  sym_state_t ->
  env_t ->
  Flx_srcref.t ->
  literal_t ->
  btypecode_t

val lookup_qn_with_sig:
  sym_state_t ->
  Flx_srcref.t ->
  Flx_srcref.t ->
  env_t ->
  qualified_name_t ->
  btypecode_t list ->
  tbexpr_t
