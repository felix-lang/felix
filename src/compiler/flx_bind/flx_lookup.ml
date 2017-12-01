open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_name_lookup
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debug = false 

let svs (s,i,mt) = s ^ "<" ^ si i ^ ">:"^ string_of_typecode mt

type bfres_t = 
  | RecordAddition of Flx_bexpr.t
  | Function of Flx_bexpr.t

let crt = ref 0

module L = Flx_literal

type module_rep_t = Flx_bind_deferred.module_rep_t

let mkentry counter_ref vs i = Flx_name_map.mkentry counter_ref vs i

  (*
(*
  THIS IS A DUMMY BOUND SYMBOL TABLE
  REQUIRED FOR THE PRINTING OF BOUND EXPRESSIONS
*)
let bsym_table = Flx_bsym_table.create ()
*)

let dummy_sr = Flx_srcref.make_dummy "[flx_lookup] generated"

let unit_t = btyp_tuple []

exception Found of int

type kind_t = Parameter | Other

let get_data table index =
  try Flx_sym_table.find table index
  with Not_found ->
    failwith ("[Flx_lookup.get_data] No definition of <" ^
      string_of_bid index ^ ">")

(* this ugly thing merges a list of function entries
some of which might be inherits, into a list of
actual functions
*)

module EntrySet = Flx_inherit.EntrySet

let rec trclose state bsym_table rs sr fs =
  Flx_inherit.trclose 
  build_env
  lookup_qn_in_env2'
  state bsym_table rs sr fs

and resolve_inherits state bsym_table rs sr x =
  Flx_inherit.resolve_inherits
  build_env
  inner_build_env
  lookup_qn_in_env2'
  state bsym_table rs sr x

and inner_lookup_name_in_env state bsym_table env rs sr name : entry_set_t =
(*  print_endline ("[lookup_name_in_env] " ^ name); *)
  let rec aux env =
    match env with
    | [] -> None
    | (_,_,table,dirs,_) :: tail ->
        match lookup_name_in_table_dirs table dirs sr name with
        | Some _ as x -> x
        | None -> aux tail
  in
    match aux env with
    | Some x ->
      (*
      print_endline "[lookup_name_in_env] Got result, resolve inherits";
      *)
      resolve_inherits state bsym_table rs sr x
    | None ->
(*
print_endline ("[lookup_name_in_env]: Can't find name " ^ name ^ " in env "); print_env env; 
print_endline ("Issuing clierrx "[flx_bind/flx_lookup.ml:288: E81] " .. why isn't it trapped?");
*)
     raise (SimpleNameNotFound (sr,name,"inner_lookup_name_in_env"))
(*
      clierrx "[flx_bind/flx_lookup.ml:292: E82] " sr
      (
        "[lookup_name_in_env]: Name '" ^
        name ^
        "' not found in environment (depth "^
        string_of_int (List.length env)^ ")"
      )
*)

(* This routine looks up a qualified name in the
   environment and returns an entry_set_t:
   can be either non-function or function set
*)
and lookup_qn_in_env2'
  state
  (bsym_table:Flx_bsym_table.t)
  (env:env_t)
  (rs:recstop)
  (qn: qualified_name_t)
  : entry_set_t * typecode_t list
=
  (*
  print_endline ("[lookup_qn_in_env2] qn=" ^ string_of_qualified_name qn);
  *)
  match qn with
  | `AST_callback (sr,qn) -> clierrx "[flx_bind/flx_lookup.ml:317: E83] " sr "[lookup_qn_in_env2] qualified name is callback [not implemented yet]"
  | `AST_void sr -> clierrx "[flx_bind/flx_lookup.ml:318: E84] " sr "[lookup_qn_in_env2] qualified name is void"
  | `AST_case_tag (sr,_) -> clierrx "[flx_bind/flx_lookup.ml:319: E85] " sr "[lookup_qn_in_env2] Can't lookup a case tag"
  | `AST_typed_case (sr,_,_) -> clierrx "[flx_bind/flx_lookup.ml:320: E86] " sr "[lookup_qn_in_env2] Can't lookup a typed case tag"
  | `AST_index (sr,name,_) ->
    print_endline ("[lookup_qn_in_env2] synthetic name " ^ name);
    clierrx "[flx_bind/flx_lookup.ml:323: E87] " sr "[lookup_qn_in_env2] Can't lookup a synthetic name"

  | `AST_name (sr,name,ts) ->
    (*
    print_endline ("lookup_qn_in_env2': looking up simple name " ^ name);
    *)
    inner_lookup_name_in_env state bsym_table env rs sr name, ts

  | `AST_lookup (sr,(me,name,ts)) ->
    (*
    print_endline ("Searching for name " ^ name);
    *)
    match eval_module_expr state bsym_table env me with
    | Flx_bind_deferred.Simple_module (impl,ts', htab,dirs) ->
      let env' = mk_bare_env state.sym_table impl in
      let tables = get_pub_tables state bsym_table env' rs dirs in
      let result = lookup_name_in_table_dirs htab tables sr name in
      match result with
      | Some entry ->
        resolve_inherits state bsym_table rs sr entry,
        ts' @ ts
      | None ->
        clierrx "[flx_bind/flx_lookup.ml:345: E88] " sr
        (
          "[lookup_qn_in_env2] Can't find " ^ name
        )

      (*
      begin
      try
        let entry = Hashtbl.find htab name in
        resolve_inherits state bsym_table rs sr entry,
        ts' @ ts
      with Not_found ->
        clierrx "[flx_bind/flx_lookup.ml:357: E89] " sr
        (
          "[lookup_qn_in_env2] Can't find " ^ name
        )
      end
      *)
and lookup_qn_in_env'
  (state:lookup_state_t)
  bsym_table
  (env:env_t) rs
  (qn: qualified_name_t)
  : entry_kind_t * typecode_t list
=
  match lookup_qn_in_env2' state bsym_table env rs qn with
    | NonFunctionEntry x,ts -> x,ts
    (* experimental, allow singleton function *)
    | FunctionEntry [x],ts -> x,ts

    | FunctionEntry _,_ ->
      let sr = src_of_qualified_name qn in
      clierrx "[flx_bind/flx_lookup.ml:377: E90] " sr
      (
        "[lookup_qn_in_env'] Not expecting " ^
        string_of_qualified_name qn ^
        " to be function set"
      )

(* This routine binds a type expression to a bound type expression.
   Note in particular that a type alias is replaced by what
   it as an alias for, recursively so that the result
   globally unique

   if params is present it is a list mapping strings to types
   possibly bound type variable

   THIS IS WEIRD .. expr_fixlist is propagated, but 'depth'
   isn't. But the depth is essential to insert the correct
   fixpoint term .. ????

   i think this arises from:

   val x = e1 + y;
   val y = e2 + x;

   here, the implied typeof() operator is used
   twice: the first bind expression invoking a second
   bind expression which would invoke the first again ..
   here we have to propagate the bind_expression
   back to the original call on the first term,
   but we don't want to accumulate depths? Hmmm...
   I should test that ..

*)
and inner_bind_type state (bsym_table:Flx_bsym_table.t) env sr rs (t:typecode_t) =
(*
  print_endline ("[inner bind_type] " ^ string_of_typecode t);
*)
  let mkenv i = build_env state bsym_table (Some i) in
  let bt =
    try
      bind_type' state bsym_table env rs sr t [] mkenv

    with
      | Free_fixpoint b ->
        clierrx "[flx_bind/flx_lookup.ml:421: E91] " sr
        ("Unresolvable recursive type " ^ sbt bsym_table b)
      | Not_found ->
        failwith "Bind type' failed with Not_found"
  in
(*
  print_endline ("Bound type= " ^ sbt bsym_table bt);
*)
  let bt =
    try beta_reduce "flx_lookup: inner_bind_type" state.counter bsym_table sr bt
    with Not_found -> failwith ("Beta reduce failed with Not_found " ^ sbt bsym_table bt)
  in
(*
    print_endline ("Beta reduced type= " ^ sbt bsym_table bt);
*)
    bt

and inner_bind_expression state bsym_table env rs e  =
  let sr = src_of_expr e in
  let e',t' =
    try
     let x = bind_expression' state bsym_table env rs e [] in
     (*
     print_endline ("Bound expression " ^
       string_of_bound_expression_with_type bsym_table x
     );
     *)
     x
    with
     | Free_fixpoint b ->
       clierrx "[flx_bind/flx_lookup.ml:451: E92] " sr
       ("inner_bind_expression: Free fixpoint: Circular dependency typing expression " ^ string_of_expr e)

     | SystemError (sr,msg) as x ->
       print_endline ("System Error binding expression " ^ string_of_expr e);
       raise x

     | ClientError (sr,msg) as x ->
       print_endline ("inner_bind_expression: Client Error binding expression " ^ string_of_expr e);
       raise x

     | ClientError2 (sr,sr2,msg) as x ->
       print_endline ("inner_bind_expression: Client Error2 binding expression " ^ string_of_expr e);
       raise x

     | SimpleNameNotFound (sr,name,routine) as x ->
       print_endline ("inner_bind_expression: SimpleNameNotFound binding expression " ^ string_of_expr e);
       raise x

     | FunctionNameNotFound (sr,name,routine, args) as x ->
       print_endline ("inner_bind_expression: FunctionNameNotFound binding expression " ^ string_of_expr e);
       raise x


     | Failure msg as x ->
       print_endline ("inner_bind_expression: Failure binding expression " ^ string_of_expr e);
       raise x

     | Not_found ->
       print_endline ("inner_bind_expression raised Not_found [BUG] e="^
       string_of_expr e);
       failwith "bind_expression' raised Not_found [BUG]"

     | GadtUnificationFailure as x -> raise x

     | exn ->
       print_endline ("inner_bind_expression: unknown exception " ^  Printexc.to_string exn); 
       raise exn
  in
    let t' = 
      try beta_reduce "flx_lookup: inner_bind_expression" state.counter bsym_table sr t' 
      with Not_found -> failwith "beta_reduce raised Not_found [BUG]"
    in
    e',t'


(* =========================================== *)
(* INTERNAL BINDING ROUTINES *)
(* =========================================== *)

(* RECURSION DETECTORS

There are FOUR type recursion detectors:

idx_fixlist is a list of indexes, used by
bind_index to detect a recursion determining
the type of a function or variable:
the depth is calculated from the list length:
this arises from bind_expression, which uses
bind type : bind_expression is called to deduce
a function return type from returned expressions

TEST CASE:
  val x = (x,x) // type is ('a * 'a) as 'a

RECURSION CYCLE:
  type_of_index' -> bind_type'

type_alias_fixlist is a list of indexes, used by
bind_type_index to detect a recursive type alias,
[list contains depth]

TEST CASE:
  typedef a = a * a // type is ('a * 'a) as 'a


RECURSION CYCLE:
  bind_type' -> type_of_type_index

as_fixlist is a list of (name,depth) pairs, used by
bind_type' to detect explicit fixpoint variables
from the TYP_as terms (x as fv)
[list contains depth]

TEST CASE:
  typedef a = b * b as b // type is ('a * 'a) as 'a

RECURSION CYCLE:
  type_of_index' -> bind_type'

expr_fixlist is a list of (expression,depth)
used by bind_type' to detect recursion from
typeof(e) type terms
[list contains depth]

TEST CASE:
  val x: typeof(x) = (x,x) // type is ('a * 'a) as 'a

RECURSION CYCLE:
  bind_type' -> bind_expression'

TRAP NOTES:
  idx_fixlist and expr_fixlist are related :(

  The expr_fixlist handles an explicit typeof(expr)
  term, for an arbitrary expr term.

  idx_fixlist is initiated by type_of_index, and only
  occurs typing a variable or function from its
  declaration when the declaration is omitted
  OR when cal_ret_type is verifying it

BUG: cal_ret_type is used to verify or compute function
return types. However the equivalent for variables
exists, even uninitialised ones. The two cases
should be handled similarly, if not by the same
routine.

Note it is NOT a error for a cycle to occur, even
in the (useless) examples:

   val x = x;
   var x = x;

In the first case, the val simply might not be used.
In the second case, there may be an assignment.
For a function, a recursive call is NOT an error
for the same reason: a function may
contain other calls, or be unused:
  fun f(x:int)= { return if x = 0 then 0 else f (x-1); }
Note two branches, the first determines the return type
as 'int' quite happily.

DEPTH:
  Depth is used to determine the argument of the
  fixpoint term.

  Depth is incremented when we decode a type
  or expression into subterms.

PROPAGATION.
It appears as_fixlist can only occur
binding a type expression, and doesn't propagate
into bind_expression when a typeof() term is
part of the type expression: it's pure a syntactic
feature of a localised type expression.

  typedef t = a * typeof(x) as a;
  var x : t;

This is NOT the case, for example:

  typedef t = a * typeof (f of (a)) as a;

shows the as_fixlist label has propagated into
the expression: expressions can contain type
terms. However, the 'as' label IS always
localised to a single term.

Clearly, the same thing can happen with a type alias:

  typedef a = a * typeof (f of (a));

However, type aliases are more general because they
can span statement boundaries:

  typedef a = a * typeof (f of (b));
  typedef b = a;

Of course, it comes to the same thing after
substitution .. but lookup and binding is responsible
for that. The key distinction is that an as label
is just a string, whereas a type alias name has
an index in the symtab, and a fully qualified name
can be used to look it up: it's identifid by
its index, not a string label: OTOH non-top level
as labels don't map to any index.

NASTY CASE: It's possible to have this kind of thing:

  typedef a = typeof ( { typedef b = a; return x; } )

so that a type_alias CAN indeed be defined inside a type
expression. That alias can't escape however. In fact,
desugaring restructures this with a lambda (or should):

  typedef a = typeof (f of ());
  fun f() { typedef b = a; return x; }

This should work BUT if an as_label is propagated
we get a failure:

  typedef a = typeof ( { typedef c = b; return x; } ) as b;

This can be made to work by lifting the as label too,
which means creating a typedef. Hmmm. All as labels
could be replaced by typedefs ..


MORE NOTES:
Each of these traps is used to inject a fixpoint
term into the expression, ensuring analysis terminates
and recursions are represented in typing.

It is sometimes a bit tricky to know when to pass, and when
to reset these detectors: in bind_type' and inner
bind_type of a subterm should usually pass the detectors
with a pushed value in appropriate cases, however and
independent typing, say of an instance index value,
should start with reset traps.

*)

(*
  we match type patterns by cheating a bit:
  we convert the pattern to a type, replacing
  the _ with a dummy type variable. We then
  record the 'as' terms of the pattern as a list
  of equations with the as variable index
  on the left, and the type term on the right:
  the RHS cannot contain any as variables.

  The generated type can contain both,
  but we can factor the as variables out
  and leave the type a function of the non-as
  pattern variables
*)

(* params is list of string * bound type *)


and bind_type' 
  state bsym_table env rs sr t params mkenv 
=
Flx_bind_type.bind_type'
  bind_type_index
  bind_expression'
  lookup_type_qn_with_sig'
  lookup_qn_in_env'
  lookup_qn_with_sig'

  state bsym_table env rs sr t params mkenv 

and bind_type_index 
  state (bsym_table:Flx_bsym_table.t) (rs:recstop) sr index ts mkenv 
=
Flx_bind_type_index.bind_type_index
  bind_type'
  inner_bind_type
  state (bsym_table:Flx_bsym_table.t) (rs:recstop) sr index ts mkenv 

(* -------------------------------------------------------------------------- *)

(** Wrapper around inner_type_of_index that tries to cache the calculated type
 * for this index. *)
and type_of_index' state bsym_table rs sr bid =
  try
    let result = Hashtbl.find state.ticache bid in
(*
print_endline ("Found type of index " ^ si bid ^ " in cache, type=" ^ Flx_btype.st result);
*)
    result
  with Not_found ->
    let t =
      try 
        let result =  inner_type_of_index state bsym_table sr rs bid in
(*
print_endline ("Adding type of index " ^ si bid ^ " to cache, type=" ^ Flx_btype.st result);
*)
        result
      with | exn ->
(*
print_endline "Abnormal exit inner_type_of_index in type_of_index'";
*)
      raise exn
    in
    if complete_type t then begin
      (* Unfold any fixpoints. *)
      let t = unfold "flx_lookup" t in

      (* Beta reduce the type. *)
      let sr =
        try (hfind "lookup" state.sym_table bid).Flx_sym.sr
        with Not_found -> dummy_sr
      in
      let t = beta_reduce "flx_lookup: type_of_index'" state.counter bsym_table sr t in

      (* Finally, cache the type. *)
      if get_structural_typedefs state then begin
if debug then
print_endline ("Flx_lookup: Adding type of index " ^ si bid ^ " to cache, type=" ^ Flx_btype.st t);
      Hashtbl.add state.ticache bid t;
      end;
      t
    end 
    else

    t

(** Wrapper around inner_type_of_index that tries to cache the calculated type
 * for this index, and then substitutes any type variables. *)
and inner_type_of_index_with_ts state bsym_table rs sr bid ts =
  let t = 
    try type_of_index' state bsym_table rs sr bid 
    with exn ->
(*
print_endline "Abnormal exit type_of_index' in inner_type_of_index_with_ts";
*)
    raise exn
  in

  (* Make sure that we got the right number of type variables. *)
  let pvs,vs,_ = find_split_vs state.sym_table bsym_table bid in
  if (List.length ts != List.length vs + List.length pvs) then
    clierrx "[flx_bind/flx_lookup.ml:1659: E104] " sr (
       "inner_type_of_index_with_ts failed with ts/vs mismatch " ^
       Flx_bsym_table.find_id bsym_table bid ^ "<" ^ si bid ^ ">" ^
       "\n parent vs = " ^ string_of_plain_ivs pvs ^ 
       ", vs= " ^ string_of_plain_ivs vs ^
       ", ts= " ^ string_of_ts bsym_table ts
    )
  else

  (* Do any type substitutions. *)
  let varmap = make_varmap state.sym_table bsym_table sr bid ts in
  let t = varmap_subst varmap t in

  (* Beta reduce and return the type. *)
  beta_reduce "flx_lookup: inner_type_of_index_with_ts" state.counter bsym_table sr t

(** Wrapper around inner_type_of_index that substitutes any type variables. *)

(* -------------------------------------------------------------------------- *)

(* This routine should ONLY 'fail' if the return type
  is indeterminate. This cannot usually happen.

  Otherwise, the result may be recursive, possibly
  Fix 0 -- which is determinate 'indeterminate' value :-)

  For example: fun f(x:int) { return f x; }

  should yield fix 0, and NOT fail.
*)


(* cal_ret_type uses the private name map *)
(* args is string,btype list *)
and cal_ret_type state bsym_table (rs:recstop) index args =
  incr crt;
(*
  print_endline ("%%%%%%%%%%% Entering Calrettype level " ^ string_of_int (!crt));
*)
  try
    let result = Flx_cal_ret_type.cal_ret_type' 
    build_env
    bind_type'
    bind_expression' 
    state bsym_table (rs:recstop) index args
    in 
(*
    print_endline ("%%%%%%%%%%% Normal exit level " ^ string_of_int (!crt));
*)
    decr crt;
    result

  with exn ->
(*
    print_endline ("%%%%%%%%%%% Abnormal exit level " ^ string_of_int (!crt));
*)
    decr crt;
    raise exn


(* -------------------------------------------------------------------------- *)

(** Find the type of a bound symbol. *)
and btype_of_bsym state bsym_table sr bt bid bsym =
  Flx_btype_of_bsym.btype_of_bsym 
    state bsym_table sr bt bid bsym


(* -------------------------------------------------------------------------- *)

(** This routine is called to find the type of a function or variable.
 * .. so there's no type_alias_fixlist .. *)
and inner_type_of_index state bsym_table sr rs index =
try
(*
print_endline ("***** Enter inner_type_of_index " ^ si index);
*)
  let result = Flx_inner_type_of_index.inner_type_of_index' 
    build_env
    bind_type'
    btype_of_bsym
    cal_ret_type
    state bsym_table sr rs index 
  in
(*
print_endline ("***** normal exit inner_type_of_index " ^ si index ^ ", type=" ^Flx_btype.st result);
*)
  result
with exn ->
(*
print_endline ("***** abnormal exit inner_type_of_index " ^ si index);
*)
raise exn

and cal_apply state bsym_table sr rs tbe1 tbe2 =
  Flx_cal_apply.cal_apply
  build_env 
  bind_expression'
  state bsym_table sr rs tbe1 tbe2

and koenig_lookup state bsym_table env rs sra id' name_map fn t2 ts =
  (*
  print_endline ("Applying Koenig lookup for " ^ fn);
  *)
  let entries =
    try Hashtbl.find name_map fn
    with Not_found ->
      clierrx "[flx_bind/flx_lookup.ml:2285: E116] " sra
      (
        "Koenig lookup: can't find name "^
        fn^ " in " ^
        (match id' with
        | "" -> "top level module"
        | _ -> "module '" ^ id' ^ "'"
        )
      )
  in
  match (entries:entry_set_t) with
  | FunctionEntry fs ->
    (*
    print_endline ("Got candidates: " ^ string_of_entry_set entries);
    *)
    begin match resolve_overload state bsym_table env rs sra fs fn [t2] ts with
    | Some (index'',t,ret,mgu,ts) ->
      (*
      print_endline "Overload resolution OK";
      *)
if debug then
print_endline ("flx_lookup.koenig_lookup.bexpr_closure");
      bexpr_closure
        (inner_type_of_index_with_ts state bsym_table rs sra index'' ts)
        (index'',ts)

    | None ->
        (*
        let n = ref 0
        in Hashtbl.iter (fun _ _ -> incr n) name_map;
        print_endline ("module defines " ^ string_of_int !n^ " entries");
        *)
        clierrx "[flx_bind/flx_lookup.ml:2315: E117] " sra
        (
          "[flx_ebind] Koenig lookup: Can't find match for " ^ fn ^
          "\ncandidates are: " ^ full_string_of_entry_set state.sym_table bsym_table entries
        )
    end
  | NonFunctionEntry _ -> clierrx "[flx_bind/flx_lookup.ml:2321: E118] " sra "Koenig lookup expected function"

and lookup_qn_with_sig'
  state
  bsym_table
  sra srn
  env rs
  qn
  signs
=
  Flx_lookup_qn_with_sig.lookup_qn_with_sig'

  inner_bind_type
  resolve_overload
  inner_type_of_index_with_ts
  lookup_name_with_sig
  inner_type_of_index
  eval_module_expr
  get_pub_tables

  state
  bsym_table
  sra srn
  env rs
  qn
  signs



(* NOTE TO ME: this routine looks up a type function, it does NOT
   calculate the type OF something!
*)
and lookup_type_qn_with_sig'
  state
  bsym_table
  sra srn
  env rs
  qn
  signs
=
  Flx_lookup_type_qn_with_sig.lookup_type_qn_with_sig'
  inner_bind_type
  inner_type_of_index
  inner_type_of_index_with_ts
  lookup_type_name_with_sig
  eval_module_expr
  get_pub_tables
  resolve_overload
  state
  bsym_table
  sra srn
  env rs
  qn
  signs

and lookup_name_with_sig'
  state
  bsym_table
  sra srn
  caller_env env
  rs
  name
  ts
  t2
=
(*
if name = "EInt" then
  print_endline ("[lookup_name_with_sig'] " ^ name ^
    " of " ^ catmap "," (sbt bsym_table) t2)
  ;
*)
  match env with
  | [] ->
    clierrx "[flx_bind/flx_lookup.ml:2973: E149] " srn
    (
      "[lookup_name_with_sig] Can't find " ^ name ^ "[" ^catmap "," (Flx_btype.st ) ts^ "]" ^
      " of " ^ catmap "," (Flx_btype.st ) t2 ^ "=\n" ^
       name ^ "[" ^catmap "," (Flx_print.sbt bsym_table) ts^ "]" ^
      " of " ^ catmap "," (Flx_print.sbt bsym_table) t2 
    )
  | (_,_,table,dirs,_)::tail ->
    match
      lookup_name_in_table_dirs_with_sig
      state
      bsym_table
      table
      dirs
      caller_env env rs
      sra srn name ts t2
    with
    | Some result -> result
    | None ->
      let tbx=
        lookup_name_with_sig'
          state
          bsym_table
          sra srn
          caller_env tail rs name ts t2
       in tbx

and lookup_name_with_sig
  state
  bsym_table
  sra srn
  caller_env env
  rs
  name
  ts
  t2
: Flx_bexpr.t
=
(*
  print_endline ("[lookup_name_with_sig] " ^ name ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]" ^
    " of " ^ catmap "," (sbt bsym_table) t2)
  ;
*)
 let projection = 
   match t2 with
(*
   | [BTYP_tuple_cons (h,t)] -> 
     print_endline ("projection of tuple cons not implemented yet"); assert false
*)

   | [BTYP_inst (j,ts',_) as d] ->
     let bsym = try Some (Flx_bsym_table.find bsym_table j) with Not_found -> None in
     begin match bsym with
     | Some bsym ->
(*
       print_endline ("Found nominal type "^si j ^" in bound symbol table");
*)
       begin match Flx_bsym.bbdcl bsym with
       | BBDCL_struct (vs,fields) 
       | BBDCL_cstruct (vs, fields,_) ->
         begin match
            Flx_list.list_assoc_index_with_assoc fields name
         with
         | Some (k,ft) ->
           (*
           print_endline ("FOUND STRUCT FIELD " ^ name ^ " in bound table");
           *)
           let ft = 
             try tsubst (Flx_bsym.sr bsym) vs ts' ft 
             with _ -> print_endline "[lookup_name_with_sig] Hassle replacing vs with ts??"; assert false
           in
           Some (bexpr_prj k d ft) 
         | None -> None
         end
       | _ -> None
       end
     | None -> 
(*
       print_endline ("Can't find nominal type " ^ si j ^ " in bound symbol table .. trying unbound table");
*)
       begin try
         match hfind "lookup" state.sym_table j with
         | { Flx_sym.symdef=SYMDEF_struct fields; vs=vs } 
         | { Flx_sym.symdef=SYMDEF_cstruct (fields,_); vs=vs } ->
           begin match
              Flx_list.list_assoc_index_with_assoc fields name
           with
           | Some _ ->
             print_endline ("FOUND STRUCT FIELD " ^ name ^ " in unbound table: FIXME!!");
             assert false;
             None
           | None -> None
           end
         | _ -> None
       with _ ->
         print_endline ("Can't find nominal type " ^ si j ^ " in unbound symbol table????");
         assert false
       end
     end

   | [BTYP_pointer (BTYP_inst (j,ts',_)) as d] ->
     let bsym = try Some (Flx_bsym_table.find bsym_table j) with Not_found -> None in
     begin match bsym with
     | Some bsym ->
(*
       print_endline ("Found nominal type "^si j ^" in bound symbol table");
*)
       begin match Flx_bsym.bbdcl bsym with
       | BBDCL_struct (vs,fields) 
       | BBDCL_cstruct (vs, fields,_) ->
         begin match
            Flx_list.list_assoc_index_with_assoc fields name
         with
         | Some (k,ft) ->
           (*
           print_endline ("FOUND STRUCT FIELD " ^ name ^ " in bound table");
           *)
           let ft = 
             try tsubst (Flx_bsym.sr bsym) vs ts' ft 
             with _ -> print_endline "[lookup_name_with_sig] Hassle replacing vs with ts??"; assert false
           in
           Some (bexpr_prj k d (btyp_pointer ft)) 
         | None -> None
         end
       | _ -> None
       end
     | None -> 
(*
       print_endline ("Can't find nominal type " ^ si j ^ " in bound symbol table .. trying unbound table");
*)
       begin try
         match hfind "lookup" state.sym_table j with
         | { Flx_sym.symdef=SYMDEF_struct fields; vs=vs } 
         | { Flx_sym.symdef=SYMDEF_cstruct (fields,_); vs=vs } ->
           begin match
              Flx_list.list_assoc_index_with_assoc fields name
           with
           | Some _ ->
             print_endline ("FOUND STRUCT FIELD " ^ name ^ " in unbound table: FIXME!!");
             assert false;
             None
           | None -> None
           end
         | _ -> None
       with _ ->
         print_endline ("Can't find nominal type " ^ si j ^ " in unbound symbol table????");
         assert false
       end
     end


   | [BTYP_record (fields) as d] ->
     begin match
       Flx_list.list_assoc_index_with_assoc fields name
     with
     | Some (k,ft) ->
(*
       print_endline ("projection: FOUND RECORD FIELD " ^ name);
*)
       Some (bexpr_prj k d ft)
     | None -> None
     end

   | [BTYP_polyrecord (fields,v) as d] ->
     if List.mem_assoc name fields 
     then 
      let ft = List.assoc name fields in
      Some (bexpr_rprj name d ft)  (* MIGHT REQUIRE FIXPOINT FIXUP! *)
     else None

   | [BTYP_pointer (BTYP_record (fields)) as d] ->
     begin match
       Flx_list.list_assoc_index_with_assoc fields name
     with
     | Some (k,ft) ->
(*
       print_endline ("projection: FOUND RECORD POINTER FIELD " ^ name);
*)
       Some (bexpr_prj k d (btyp_pointer ft))
     | None -> None
     end

   | [BTYP_pointer (BTYP_polyrecord (fields,v)) as d] ->
     if List.mem_assoc name fields 
     then 
      let ft = List.assoc name fields in
      Some (bexpr_rprj name d (btyp_pointer ft))  (* MIGHT REQUIRE FIXPOINT FIXUP! *)
     else None

   | _ -> None
 in
 match projection with
 | Some p -> p
 | None ->
 try
 let result = 
  lookup_name_with_sig'
    state
    bsym_table
    sra srn
    caller_env env
    rs
    name
    ts
    t2
  in 
  result
  with exn -> 
  raise exn

and lookup_type_name_with_sig
  state
  bsym_table
  sra srn
  caller_env env
  rs
  name
  ts
  t2
=
(*
  print_endline ("[lookup_type_name_with_sig] " ^ name ^
    " of " ^ catmap "," (sbt bsym_table) t2)
  ;
*)
  match env with
  | [] ->
    clierrx "[flx_bind/flx_lookup.ml:3197: E150] " srn
    (
      "[lookup_type_name_with_sig] Can't find " ^ name ^
      " of " ^ catmap "," (sbt bsym_table) t2
    )
  | (_,_,table,dirs,_)::tail ->
(*
print_endline ("lookup_type_name_with_sig: trying top level table-dirs");
*)
    match
      lookup_type_name_in_table_dirs_with_sig
      state
      bsym_table
      table
      dirs
      caller_env env rs
      sra srn name ts t2
    with
    | Some result -> result
    | None ->
(*
print_endline ("lookup_type_name_with_sig: failed top level table-dirs, trying tail env");
*)
      let tbx=
        lookup_type_name_with_sig
          state
          bsym_table
          sra srn
          caller_env tail rs name ts t2
       in tbx

(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)

and lookup_name_in_table_dirs_with_sig
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2
=
  Flx_lookup_name_itdws.lookup_name_in_table_dirs_with_sig
  inner_type_of_index_with_ts
  resolve_overload
  build_env
  bind_type'
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2

and lookup_type_name_in_table_dirs_with_sig
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2
=
Flx_lookup_type_name_itdws.lookup_type_name_in_table_dirs_with_sig
  build_env
  bind_type'
  resolve_overload
  bind_type_index
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2

and lookup_label_in_env state bsym_table env sr name : bid_t option =
  let result = 
    try 
      Some (inner_lookup_name_in_env state bsym_table (env:env_t) rsground sr name)
    with _ -> None
  in
  match result with
  | Some (FunctionEntry _) -> clierrx "[flx_bind/flx_lookup.ml:3823: E164] " sr ("Expected "^name^" to be a label, got function set")
  | Some (NonFunctionEntry x) -> 
    begin match hfind "lookup" state.sym_table (sye x) with
    | { Flx_sym.symdef=SYMDEF_label s} -> Some (sye x)
    | { Flx_sym.symdef=symdef; vs=vs } -> clierrx "[flx_bind/flx_lookup.ml:3827: E165] " sr ("Expected " ^ name ^ " to be a label, got:\n" ^
      string_of_symdef symdef name vs)
    end
  | None -> None


and bind_expression_with_args state bsym_table env e args =
  try
    bind_expression' state bsym_table env rsground e args
  with Not_found -> failwith "bind expression with args raised Not_found [BUG]"

and bind_expression' state bsym_table env (rs:recstop) e args =
  Flx_bind_expression.bind_expression'
  build_env
  bind_type'
  inner_type_of_index_with_ts
  lookup_label_in_env
  lookup_qn_in_env2'
  lookup_name_with_sig
  inner_lookup_name_in_env
  resolve_overload
  eval_module_expr
  get_pub_tables
  lookup_qn_with_sig'
  inner_bind_type
  koenig_lookup
  cal_apply
  state bsym_table env (rs:recstop) e args

and grab_ts qn = match qn with
| `AST_name (sr,name,ts) -> ts
| `AST_lookup (sr,(e,name,ts)) -> ts
| _ -> failwith "expected qn .."


and check_instances state bsym_table call_sr calledname classname es ts' mkenv =
  let insts = ref [] in
  match es with
  | NonFunctionEntry _ -> print_endline "EXPECTED INSTANCES TO BE FUNCTION SET"
  | FunctionEntry es ->
    (*
    print_endline ("instance Candidates " ^ catmap "," string_of_entry_kind es);
    *)
    List.iter
    (fun {base_sym=i; spec_vs=spec_vs; sub_ts=sub_ts} ->
    match hfind "lookup" state.sym_table i  with
    { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry } ->
    match entry with
    | SYMDEF_instance qn' ->
      (*
      print_endline ("Verified " ^ si i ^ " is an instance of " ^ id);
      print_endline ("  base vs = " ^ print_ivs_with_index vs);
      print_endline ("  spec vs = " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") spec_vs);
      print_endline ("  view ts = " ^ catmap "," (fun t -> sbt bsym_table t) sub_ts);
      *)
      let inst_ts = grab_ts qn' in
      (*
      print_endline ("Unbound instance ts = " ^ catmap "," string_of_typecode inst_ts);
      *)
      let instance_env = mkenv i in
      let bt t = bind_type' state bsym_table instance_env rsground sr t [] mkenv in
      let inst_ts = List.map bt inst_ts in
      (*
      print_endline ("  instance ts = " ^ catmap "," (fun t -> sbt bsym_table t) inst_ts);
      print_endline ("  caller   ts = " ^ catmap "," (fun t -> sbt bsym_table t) ts');
      *)
      let matches =
        if List.length inst_ts <> List.length ts' then false else
        match maybe_specialisation bsym_table state.counter (List.combine inst_ts ts') with
        | None -> false
        | Some mgu ->
          (*
          print_endline ("MGU: " ^ catmap ", " (fun (i,t)-> si i ^ "->" ^ sbt bsym_table t) mgu);
          print_endline ("check base vs (constraint) = " ^ print_ivs_with_index vs);
          *)
          let cons = try
            Flx_tconstraint.build_type_constraints state.counter bsym_table bt id sr (fst vs)
            with _ -> clierrx "[flx_bind/flx_lookup.ml:5416: E226] " sr "Can't build type constraints, type binding failed"
          in
          let {raw_type_constraint=icons} = snd vs in
          let icons = bt icons in
          (*
          print_endline ("VS Constraint = " ^ sbt bsym_table icons);
          *)
          let cons = btyp_intersect [cons; icons] in
          let cons = list_subst state.counter mgu cons in
          let cons = beta_reduce "flx_lookup: check_instances: constraints" state.counter bsym_table sr cons in
          (*
          print_endline ("[flx_lookup:4] Reduced Constraint = " ^ sbt bsym_table cons);
          *)
          match cons with
          | BTYP_tuple [] -> true
          | BTYP_fix (0, _) -> true (* any *)
          | BTYP_void -> false
          | _ ->
              print_endline (
               "[instance_check] Can't reduce instance type constraint " ^
               sbt bsym_table cons
             );
             true
      in

      if matches then begin
        (*
        print_endline "INSTANCE MATCHES";
        *)
        insts := `Inst i :: !insts
      end
      (*
      else
        print_endline "INSTANCE DOES NOT MATCH: REJECTED"
      *)
      ;


    | SYMDEF_typeclass ->
      (*
      print_endline ("Verified " ^ si i ^ " is an typeclass specialisation of " ^ classname);
      print_endline ("  base vs = " ^ print_ivs_with_index vs);
      print_endline ("  spec vs = " ^ catmap "," (fun (s,i) -> s^"<"^si i^">") spec_vs);
      print_endline ("  view ts = " ^ catmap "," (fun t -> sbt bsym_table t) sub_ts);
      *)
      if sub_ts = ts' then begin
        (*
        print_endline "SPECIALISATION MATCHES";
        *)
        insts := `Typeclass (i,sub_ts) :: !insts
      end
      (*
      else
        print_endline "SPECIALISATION DOES NOT MATCH: REJECTED"
      ;
      *)

    | _ -> print_endline "EXPECTED TYPECLASS INSTANCE!"
    )
    es
    ;
    (*
    begin match !insts with
    | [`Inst i] -> ()
    | [`Typeclass (i,ts)] -> ()
    | [] ->
      print_endline ("WARNING: In call of " ^ calledname ^", Typeclass instance matching " ^
        classname ^"["^catmap "," (sbt bsym_table) ts' ^"]" ^
        " not found"
      )
    | `Inst i :: t ->
      print_endline ("WARNING: In call of " ^ calledname ^", More than one instances matching " ^
        classname ^"["^catmap "," (sbt bsym_table) ts' ^"]" ^
        " found"
      );
      print_endline ("Call of " ^ calledname ^ " at " ^ Flx_srcref.short_string_of_src call_sr);
      List.iter (fun i ->
        match i with
        | `Inst i -> print_endline ("Instance " ^ si i)
        | `Typeclass (i,ts) -> print_endline ("Typeclass " ^ si i^"[" ^ catmap "," (sbt bsym_table) ts ^ "]")
      )
      !insts

    | `Typeclass (i,ts) :: tail ->
      clierrx "[flx_bind/flx_lookup.ml:5501: E227] " call_sr ("In call of " ^ calledname ^", Multiple typeclass specialisations matching " ^
        classname ^"["^catmap "," (sbt bsym_table) ts' ^"]" ^
        " found"
      )
    end
    *)



and resolve_overload
  state
  bsym_table
  caller_env
  rs
  sr
  fs
  name
  sufs
  ts
=
Flx_resolve_overload.resolve_overload
  inner_build_env
  inner_bind_type
  inner_bind_expression
  lookup_qn_in_env2'
  trclose
  state
  bsym_table
  caller_env
  rs
  sr
  fs
  name
  sufs
  ts

(* an environment is a list of hastables, mapping
   names to definition indicies. Each entity defining
   a scope contains one hashtable, and a pointer to
   its parent, if any. The name 'root' is special,
   it is the name of the single top level module
   created by the desugaring phase. We have to be
   able to find this name, so if when we run out
   of parents, which is when we hit the top module,
   we create a parent name map with a single entry
   'top'->NonFunctionEntry 0.
*)
(* calculate the transitive closure of an i,ts list
  with respect to inherit clauses.

  The result is an i,ts list.

  This is BUGGED because it ignores typeclass requirements ..
  however
  (a) modules can't have them (use inherit clause)
  (b) typeclasses don't use them (use inherit clause)
  (c) the routine is only called for modules and typeclasses?
*)

and get_pub_tables state bsym_table env rs (dirs:sdir_t list) =
  Flx_env.get_pub_tables 
  lookup_qn_in_env' bind_type'
  state bsym_table env rs dirs

and inner_build_env state bsym_table rs parent : env_t =
  Flx_env.inner_build_env 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table rs parent

and build_env state bsym_table parent : env_t =
  Flx_env.build_env 
  lookup_qn_in_env' bind_type' lookup_qn_in_env2'
  state bsym_table parent

(*===========================================================*)
(* MODULE STUFF *)
(*===========================================================*)

(* This routine takes a bound type, and produces a unique form
   of the bound type, by again factoring out type aliases.
   The type aliases can get reintroduced by map_type,
   if an abstract type is mapped to a typedef, so we have
   to factor them out again .. YUK!!
*)

(* THIS ROUTINE APPEARS TO BE UNUSED! *)
(*
and rebind_btype state bsym_table env sr ts t =
  let rbt t = rebind_btype state bsym_table env sr ts t in
  match t with
  | BTYP_hole -> assert false
  | BTYP_tuple_cons _ -> assert false
  | BTYP_tuple_snoc _ -> assert false
  | BTYP_none -> assert false
  | BTYP_inst (i,_) ->
    begin match get_data state.sym_table i with
    | { Flx_sym.symdef=SYMDEF_type_alias t'} ->
      inner_bind_type state bsym_table env sr rsground t'
    | _ -> t
    end

  | BTYP_type_set_union ts -> btyp_type_set_union (List.map rbt ts)
  | BTYP_type_set_intersection ts -> btyp_type_set_intersection (List.map rbt ts)

  | BTYP_tuple ts -> btyp_tuple (List.map rbt ts)
  | BTYP_record (ts) ->
      let ss,ts = List.split ts in
      btyp_record (List.combine ss (List.map rbt ts))

  | BTYP_polyrecord (ts,v) ->
      let ss,ts = List.split ts in
      btyp_polyrecord (List.combine ss (List.map rbt ts)) (rbt v)


  | BTYP_variant ts ->
      let ss,ts = List.split ts in
      btyp_variant (List.combine ss (List.map rbt ts))

  | BTYP_type_set ts -> btyp_type_set (List.map rbt ts)
  | BTYP_intersect ts -> btyp_intersect (List.map rbt ts)
  | BTYP_union ts -> btyp_union (List.map rbt ts)

  | BTYP_sum ts ->
    let ts = List.map rbt ts in
    if all_units ts then
      btyp_unitsum (List.length ts)
    else
      btyp_sum ts

  | BTYP_function (a,r) -> btyp_function (rbt a, rbt r)
  | BTYP_effector (a,e,r) -> btyp_effector (rbt a, rbt e, rbt r)
  | BTYP_cfunction (a,r) -> btyp_cfunction (rbt a, rbt r)
  | BTYP_pointer t -> btyp_pointer (rbt t)
  | BTYP_rev t -> btyp_rev (rbt t)
  | BTYP_array (t1,t2) -> btyp_array (rbt t1, rbt t2)

  | BTYP_int
  | BTYP_label
  | BTYP_unitsum _
  | BTYP_void
  | BTYP_fix _ -> t

  | BTYP_type_var (i,mt) -> clierrx "[flx_bind/flx_lookup.ml:6403: E229] " sr ("[rebind_type] Unexpected type variable " ^ sbt bsym_table t)
  | BTYP_type_apply _
  | BTYP_type_map _
  | BTYP_type_function _
  | BTYP_type _
  | BTYP_type_tuple _
  | BTYP_type_match _
    -> clierrx "[flx_bind/flx_lookup.ml:6409: E230] " sr ("[rebind_type] Unexpected metatype " ^ sbt bsym_table t)

*)

and eval_module_expr state bsym_table env e : module_rep_t =
  Flx_eval_module.eval_module_expr
  inner_lookup_name_in_env
  get_pub_tables
  lookup_name_in_table_dirs
  state bsym_table env e

(* ********* THUNKS ************* *)
(* this routine has to return a function or procedure .. *)
let lookup_qn_with_sig
  state
  bsym_table
  sra srn
  env
  qn
  signs
=
try
  lookup_qn_with_sig'
    state
    bsym_table
    sra srn
    env rsground
    qn
    signs
with
  | Free_fixpoint b ->
    clierrx "[flx_bind/flx_lookup.ml:6497: E234] " sra
    ("Recursive dependency resolving name " ^ string_of_qualified_name qn)

let lookup_name_in_env state bsym_table (env:env_t) sr name : entry_set_t =
  inner_lookup_name_in_env state bsym_table (env:env_t) rsground sr name

let lookup_qn_in_env2
  state
  bsym_table
  (env:env_t)
  (qn: qualified_name_t)
  : entry_set_t * typecode_t list
=
  lookup_qn_in_env2' state bsym_table env rsground qn


(* this one isn't recursive i hope .. *)
let lookup_code_in_env state bsym_table env sr qn =
  let result =
    try Some (lookup_qn_in_env2' state bsym_table env rsground qn)
    with _ -> None
  in match result with
  | Some (NonFunctionEntry x,ts) ->
    clierrx "[flx_bind/flx_lookup.ml:6520: E235] " sr
    (
      "[lookup_qn_in_env] Not expecting " ^
      string_of_qualified_name qn ^
      " to be non-function (code insertions use function entries) "
    )

  | Some (FunctionEntry x,ts) ->
    List.iter
    (fun i ->
      match hfind "lookup" state.sym_table (sye i) with
      | { Flx_sym.symdef=SYMDEF_insert _} -> ()
      | { Flx_sym.id=id; vs=vs; symdef=y} -> clierrx "[flx_bind/flx_lookup.ml:6532: E236] " sr
        (
          "Expected requirement '"^
          string_of_qualified_name qn ^
          "' to bind to a header or body insertion, instead got:\n" ^
          string_of_symdef y id vs
        )
    )
    x
    ;
    Some (x,ts)

  | None -> None

let lookup_qn_in_env
  state
  bsym_table
  (env:env_t)
  (qn: qualified_name_t)
  : entry_kind_t  * typecode_t list
=
  lookup_qn_in_env' state bsym_table env rsground qn


let lookup_uniq_in_env
  state
  bsym_table
  (env:env_t)
  (qn: qualified_name_t)
  : entry_kind_t  * typecode_t list
=
  match lookup_qn_in_env2' state bsym_table env rsground qn with
    | NonFunctionEntry x,ts -> x,ts
    | FunctionEntry [x],ts -> x,ts
    | _ ->
      let sr = src_of_qualified_name qn in
      clierrx "[flx_bind/flx_lookup.ml:6568: E237] " sr
      (
        "[lookup_uniq_in_env] Not expecting " ^
        string_of_qualified_name qn ^
        " to be non-singleton function set"
      )

(*
let lookup_function_in_env
  state
  bsym_table
  (env:env_t)
  (qn: qualified_name_t)
  : entry_kind_t  * typecode_t list
=
  match lookup_qn_in_env2' state bsym_table env rsground qn with
    | FunctionEntry [x],ts -> x,ts
    | _ ->
      let sr = src_of_expr (qn:>expr_t) in
      clierrx "[flx_bind/flx_lookup.ml:6587: E238] " sr
      (
        "[lookup_qn_in_env] Not expecting " ^
        string_of_qualified_name qn ^
        " to be non-function or non-singleton function set"
      )

*)

let lookup_sn_in_env
  state
  bsym_table
  env
  sn
=
(*
print_endline ("Lookup suffixed name " ^ string_of_suffixed_name sn);
*)
  let sr = src_of_suffixed_name sn in
  let bt t = inner_bind_type state bsym_table env sr rsground t in
  match sn with
  | #qualified_name_t as x ->
    begin match
      lookup_qn_in_env' state bsym_table env rsground x
    with
    | index,ts -> (sye index), List.map bt ts
    end

  | `AST_suffix (sr,(qn,suf)) ->
    let bsuf = inner_bind_type state bsym_table env sr rsground suf in
    (* OUCH HACKERY *)
    let (be,t) =
      lookup_qn_with_sig' state bsym_table sr sr env rsground qn [bsuf]
    in match be with
    | BEXPR_varname (index,ts) ->
      index,ts
    | BEXPR_closure (index,ts) -> index,ts

    | _ -> failwith "Expected expression to be index"

let bind_type state bsym_table env sr t =
  try
  inner_bind_type state bsym_table env sr rsground t
  with Not_found -> failwith "bind type raised Not_found [BUG]"

let bind_expression state bsym_table env e  =
  try
  inner_bind_expression state bsym_table env rsground e
  with 
  | Not_found -> failwith "xxxx bind expression raised Not_found [BUG]"
  | GadtUnificationFailure as x -> raise x
  | exn ->  
    print_endline ("Inner bind expression failed binding " ^ string_of_expr e);
    raise exn

let type_of_index state bsym_table sr bid =
  try
(*
print_endline ("^^^^^^ Enter type_of_index " ^ si bid);
*)
  let result = type_of_index' state bsym_table rsground sr bid in
(*
print_endline ("^^^^^^ Normal exit type_of_index " ^ si bid);
*)
  result
  with Not_found -> failwith "type of index raised Not_found [BUG]"
  | exn ->
(*
print_endline ("^^^^^^ Abnormal exit type_of_index " ^ si bid);
*)
   raise exn

let type_of_index_with_ts state bsym_table sr bid ts =
  try
  inner_type_of_index_with_ts state bsym_table rsground sr bid ts
  with Not_found -> failwith "type of index with ts raised Not_found [BUG]"





