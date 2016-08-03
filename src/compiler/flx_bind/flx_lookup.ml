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

let svs (s,i,mt) = s ^ "<" ^ si i ^ ">:"^ string_of_typecode mt

type bfres_t = 
  | RecordAddition of Flx_bexpr.t
  | Function of Flx_bexpr.t

let crt = ref 0

module L = Flx_literal

type module_rep_t = Flx_bind_deferred.module_rep_t


  (*
(*
  THIS IS A DUMMY BOUND SYMBOL TABLE
  REQUIRED FOR THE PRINTING OF BOUND EXPRESSIONS
*)
let bsym_table = Flx_bsym_table.create ()
*)

let dummy_sr = Flx_srcref.make_dummy "[flx_lookup] generated"

let unit_t = btyp_tuple []

(* use fresh variables, but preserve names *)
let mkentry state (vs:ivs_list_t) i =
  let is = List.map
    (fun _ -> fresh_bid state.counter)
    (fst vs)
  in
  let ts = List.map (fun i ->
    (*
    print_endline ("[mkentry] Fudging type variable type " ^ si i);
    *)
    btyp_type_var (i, btyp_type 0)) is
  in
  let vs = List.map2 (fun i (n,_,_) -> n,i) is (fst vs) in
  {base_sym=i; spec_vs=vs; sub_ts=ts}


exception Found of int
exception Tfound of Flx_btype.t

type kind_t = Parameter | Other

let get_data table index =
  try Flx_sym_table.find table index
  with Not_found ->
    failwith ("[Flx_lookup.get_data] No definition of <" ^
      string_of_bid index ^ ">")

let lookup_name_in_htab htab name : entry_set_t option =
  try Some (Hashtbl.find htab name)
  with Not_found -> None

let merge_functions
  (opens:entry_set_t list)
  name
: entry_kind_t list =
(*
if name = "ff" then print_endline "Merging 'ff'"; 
*)
  let result =
  List.fold_left
    (fun init x -> match x with
    | FunctionEntry ls ->
(*
if name = "ff" then print_endline "Function set ..";
*)
      List.fold_left
      (fun init ({base_sym=bid; spec_vs=vs; sub_ts=ts} as x) ->
(*
if name = "ff" then print_endline ("Merging view " ^ string_of_entry_kind x^ " len vs = " ^ string_of_int (List.length vs) ^ ", len ts = " ^ string_of_int (List.length ts));
*)
        if List.mem x init then 
          begin (* if name = "ff" then print_endline "Dup"; *) init end 
        else 
          begin (* if name = "ff" then print_endline "new";*) x :: init end
      )
      init ls
    | NonFunctionEntry x ->
      failwith
      ("[merge_functions] Expected " ^
        name ^ " to be function overload set in all open modules, got non-function:\n" ^
        string_of_entry_kind x
      )
    )
  []
  opens
  in
(*
  if name = "ff" then print_endline ("Merged list length=" ^ string_of_int (List.length opens));
*)
  result


(* compares entry kinds, with alpha conversion of type subscripts *)
let eq_entry_kinds y1 y2 =
  match y1, y2 with
    ({base_sym=bid1; spec_vs=vs1; sub_ts=ts1} as y1),
    ({base_sym=bid2; spec_vs=vs2; sub_ts=ts2} as y2)
   ->
   if bid2 <> bid2 then false else
   begin 
     assert (List.length ts1 = List.length ts2); (* got to be the same! *)
     if List.length vs1 <> List.length vs2 then false else
     let nvs = List.length vs1 in
     (* just hope these variables aren't used, since they're low indices this should be safe *)
     let nuvs = List.map (fun i -> btyp_type_var (i, BTYP_type 0)) (Flx_list.nlist nvs) in
     let sr = dummy_sr in 
     let ts1 = List.map (fun t-> Flx_unify.tsubst sr vs1 nuvs t) ts1 in
     let ts2 = List.map (fun t-> Flx_unify.tsubst sr vs2 nuvs t) ts2 in
     (* OK, a bit hacky! should use type_eq but that requires counter and bsym_table *)
     (btyp_type_tuple ts1) = (btyp_type_tuple ts2)
   end

let lookup_name_in_table_dirs table dirs sr name : entry_set_t option =
(*
if name = "ff" then print_endline ("Lookup name in table dirs " ^ name);
*)
  match lookup_name_in_htab table name with
  | Some x as y -> 
(*
    if name = "ff" then print_endline ("found core entry for ff");
*)
    y
  | None ->
(*
      if name = "ff" then print_endline ("Did not find core entry for ff, searching dirs .. ");
*)
      let opens = List.concat (
        List.map begin fun table ->
          match lookup_name_in_htab table name with
          | Some x -> [x]
          | None -> []
        end dirs)
      in
(*
      if name = "ff" then print_endline ("Found " ^ string_of_int (List.length opens) ^ " entries for ff in dirs");
*)
      match opens with
      | [x] -> 
        Some x
      | FunctionEntry ls :: rest ->
          Some (FunctionEntry (merge_functions opens name))

      | (NonFunctionEntry (i)) as some ::_ ->
          if
            List.fold_left begin fun t -> function
              | NonFunctionEntry (j) when eq_entry_kinds i j -> t
              | _ -> false
            end true opens
          then
            Some some
          else begin
            List.iter begin fun es ->
              print_endline ("[lookup_name_in_table_dirs] Symbol " ^ (string_of_entry_set es))
            end opens;
            print_endline ("[lookup_name_in_table_dirs] Conflicting nonfunction definitions for "^
              name ^" found in open modules");
            clierrx "[flx_bind/flx_lookup.ml:183: E80] " sr ("[lookup_name_in_table_dirs] Conflicting nonfunction definitions for "^
              name ^" found in open modules"
            )
          end
      | [] -> None


(* this ugly thing merges a list of function entries
some of which might be inherits, into a list of
actual functions
*)

module EntrySet = Set.Make(
  struct
    type t = entry_kind_t
    let compare = compare
  end
)

let rec trclose state bsym_table rs sr fs =
  let inset = ref EntrySet.empty in
  let outset = ref EntrySet.empty in
  let exclude = ref EntrySet.empty in
  let append fs = List.iter (fun i -> inset := EntrySet.add i !inset) fs in

  let rec trclosem () =
    if EntrySet.is_empty !inset then ()
    else
      (* grab an element *)
      let x = EntrySet.choose !inset in
      inset := EntrySet.remove x !inset;

      (* loop if already handled *)
      if EntrySet.mem x !exclude then trclosem ()
      else begin
        (* say we're handling this one *)
        exclude := EntrySet.add x !exclude;

        let parent, sym = Flx_sym_table.find_with_parent
          state.sym_table
          (sye x)
        in
        match sym.Flx_sym.symdef with
        | SYMDEF_inherit_fun qn ->
            let env = build_env state bsym_table parent in
            begin match fst (lookup_qn_in_env2' state bsym_table env rs qn) with
            | NonFunctionEntry _ ->
                clierr2 sr sym.Flx_sym.sr
                  "Inherit fun doesn't denote function set"
            | FunctionEntry fs' -> append fs'; trclosem ()
            end

        | _ -> outset := EntrySet.add x !outset; trclosem ()
      end
  in
  append fs;
  trclosem ();
  let output = ref [] in
  EntrySet.iter (fun i -> output := i :: !output) !outset;
  !output

and resolve_inherits state bsym_table rs sr x =
  match x with
  | NonFunctionEntry z ->
      let bid = sye z in

      (* If we've already bound this symbol, then we've already flatten out the
       * inherits. so, we'll just return the passed in expression. *)
      if Flx_bsym_table.mem bsym_table bid then x else

      (* Otherwise, we have to check if this entry is an inherit. If so, look up
       * the inherit entry in the environment. Otherwise, just return the
       * expression. *)
      let parent, sym = Flx_sym_table.find_with_parent state.sym_table bid in
      begin match sym.Flx_sym.symdef with
      | SYMDEF_inherit qn ->
          let env = inner_build_env state bsym_table rs parent in
          fst (lookup_qn_in_env2' state bsym_table env rs qn)

      | SYMDEF_inherit_fun qn ->
          clierr2 sr sym.Flx_sym.sr "NonFunction inherit denotes function"

      | _ -> x
      end
  | FunctionEntry fs -> FunctionEntry (trclose state bsym_table rs sr fs)

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
      let env' = mk_bare_env state bsym_table impl in
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
and inner_bind_type state (bsym_table:Flx_bsym_table.t) env sr rs t =
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
       ("inner_bind_expression: Circular dependency typing expression " ^ string_of_expr e)

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

     | exn ->
       print_endline ("inner_bind_expression: unknown exception " ^  Printexc.to_string exn); 
       raise exn
  in
    let t' = 
      try beta_reduce "flx_lookup: inner_bind_expression" state.counter bsym_table sr t' 
      with Not_found -> failwith "beta_reduce raised Not_found [BUG]"
    in
    e',t'

and expand_typeset t =
  match t with
  | BTYP_type_tuple ls
  | BTYP_type_set ls
  | BTYP_type_set_union ls -> List.fold_left (fun ls t -> expand_typeset t @ ls) [] ls
  | x -> [x]

and handle_typeset state sr elt tset =
  let ls = expand_typeset tset in
  (* x isin { a,b,c } is the same as
    typematch x with
    | a => 1
    | b => 1
    | c => 1
    | _ => 0
    endmatch

    ** THIS CODE ONLY WORKS FOR BASIC TYPES **

    This is because we don't know what to do with any
    type variables in the terms of the set. The problem
    is that 'bind type' just replaces them with bound
    variables. We have to assume they're not pattern
    variables at the moment, therefore they're variables
    from the environment.

    We should really allow for patterns, however bound
    patterns aren't just types, but types with binders
    indicating 'as' assignments and pattern variables.

    Crudely -- typesets are a hack that we should get
    rid of in the future, since a typematch is just
    more general .. however we have no way to generalise
    type match cases so they can be named at the moment.

    This is why we have typesets.. so I need to fix them,
    so the list of things in a typeset is actually
    a sequence of type patterns, not types.

  *)
  let e = BidSet.empty in
  let un = btyp_tuple [] in
  let lss = List.rev_map (fun t -> {pattern=t; pattern_vars=e; assignments=[]},un) ls in
  let fresh = fresh_bid state.counter in
  let dflt =
    {
      pattern = btyp_type_var (fresh,btyp_type 0);
      pattern_vars = BidSet.singleton fresh;
      assignments=[]
    },
    btyp_void ()
  in
  let lss = List.rev (dflt :: lss) in
  btyp_type_match (elt, lss)




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
  state
  bsym_table
  env
  rs
  sr t params
  mkenv
=
(*
print_endline ("Bind type " ^ string_of_typecode t);
*)
  let btp t params = bind_type' state bsym_table env
    {rs with depth = rs.depth+1}
    sr t params mkenv
  in
  let bt t = btp t params in
  let bi i ts = bind_type_index state bsym_table rs sr i ts mkenv in
  let bisub i ts = bind_type_index state bsym_table {rs with depth= rs.depth+1} sr i ts mkenv in
  let br t = Flx_beta.beta_reduce "flx_lookup: bind_type'" state.counter bsym_table sr t in

  let t =
  match t with
  | TYP_generic sr -> 
(*
    print_endline ("[bind_type'] trying to bind TYP_generic"); 
*)
    btyp_type (-1)
(*
    syserr sr ("[bind_type'] attempt to bind TYP_generic")
*)
  | TYP_defer (sr, tor) -> 
    begin match !tor with
    | None -> print_endline ("Bind type: undefined defered type found"); assert false
    | Some t -> bt t
    end

  | TYP_label -> BTYP_label
  | TYP_patvar _ -> failwith "Not implemented patvar in typecode"
  | TYP_patany _ -> failwith "Not implemented patany in typecode"

  | TYP_intersect ts -> btyp_intersect (List.map bt ts)
  | TYP_record ts -> btyp_record (List.map (fun (s,t) -> s,bt t) ts)
  | TYP_polyrecord (ts,v) -> btyp_polyrecord (List.map (fun (s,t) -> s,bt t) ts) (bt v)
  | TYP_variant ts -> btyp_variant (List.map (fun (s,t) -> s,bt t) ts)
  | TYP_type_extension (sr, ts, t') ->
(*
    print_endline "Binding type extension : note THIS SCREWED UP FIXPOINTS! FIXME! -- FIXED";
*)
    let ts = List.map bt ts in
    let t' = bt t' in
(*
    print_endline "Bases = ";
    List.iter (fun t ->
     print_endline (sbt bsym_table t)
    )
    ts
    ;
    print_endline ("Extension = " ^ sbt bsym_table t');
*)
    begin match t' with
    | BTYP_record (fields) ->
      let new_fields = ref [] in
      List.iter (fun t ->
        match t with
        (* reverse the fields so the second one with a given name takes precedence *)
        | BTYP_record (fields) -> new_fields := List.rev fields @ (!new_fields)
        | _ -> clierrx "[flx_bind/flx_lookup.ml:802: E93] " sr ("Record extension requires bases be records too, got " ^ sbt bsym_table t)
      )
      ts
      ;
      new_fields := fields @ !new_fields;
      let unique_fields = ref [] in
      List.iter (fun (s,t) ->
        if not (List.mem_assoc s (!unique_fields)) then
        unique_fields := (s,t) :: (!unique_fields)
      )
      (!new_fields);
      let t = btyp_record (!unique_fields) in
(*
print_endline ("Calling Flx_beta.adjust, possibly incorrectly, type = " ^ sbt bsym_table t);
*)
      Flx_beta.adjust bsym_table t

    | _ -> 
      let ntimes t n = 
        let rec aux n ts = if n=0 then ts else aux (n-1) (t::ts) in
        aux n []
      in 
      let rec check t n ts = 
        match ts with
        | [] -> Some (t,n)
        | BTYP_array (t',BTYP_unitsum m)::ts when t = t' -> check t (n+m) ts
        | t'::ts when t = t'  -> check t (n+1) ts
        | _ -> None
      in
      let compatible_arrays ts = 
        match ts with 
        | [] -> clierrx "[flx_bind/flx_lookup.ml:833: E94] " sr "empty extension"
        | BTYP_array (t,BTYP_unitsum n) :: ts -> check t n ts
        | t::ts -> check t 1 ts
      in
      match compatible_arrays (ts @ [t']) with
      | Some (t,n) -> 
        let t = btyp_array (t, btyp_unitsum n) in
(*
print_endline ("Calling Flx_beta.adjust, possibly incorrectly, type = " ^ sbt bsym_table t);
*)
        Flx_beta.adjust bsym_table t
      | None ->
        (* if it isn't a record extension, treat it as a tuple extension *)
        let fields = ref [] in
        List.iter (fun t -> 
          match t with
          | BTYP_tuple ts -> fields := !fields @ ts
          | BTYP_array (t, BTYP_unitsum n) ->
            if n > 20 then clierrx "[flx_bind/flx_lookup.ml:851: E95] " sr "Array type too big (>20) for tuple type extension"
            else fields := !fields @ ntimes t n
          | _ -> fields := !fields @ [t]
        )
        (ts @[t'])
        ;
        let t = btyp_tuple (!fields) in
(*
print_endline ("Calling Flx_beta.adjust, possibly incorrectly, type = " ^ sbt bsym_table t);
*)
        Flx_beta.adjust bsym_table t
    end

  (* We first attempt to perform the match at binding time as an optimisation,
   * if that fails, we generate a delayed matching construction. The latter
   * will be needed when the argument is a type variable. *)
  | TYP_type_match (t,ps) as ubt ->
    let t = bt t in
    bind_type_match bsym_table state.counter bt btp params sr t ps ubt 

  | TYP_dual t -> dual (bt t)

  | TYP_ellipsis ->
    failwith "Unexpected TYP_ellipsis (...) in bind type"
  | TYP_none ->
    failwith "Unexpected TYP_none in bind type"

  | TYP_typeset ts
  | TYP_setunion ts ->
    btyp_type_set (expand_typeset (btyp_type_set (List.map bt ts)))

  | TYP_setintersection ts -> btyp_type_set_intersection (List.map bt ts)


  | TYP_isin (elt,typeset) ->
      let elt = bt elt in
      let typeset = bt typeset in
      handle_typeset state sr elt typeset

  | TYP_var i ->
(*
print_endline ("Binding TYP_var " ^ si i);
*)
      (* HACK .. assume variable is type TYPE *)
      btyp_type_var (i, btyp_type 0)

  | TYP_as (t,s) ->
      bind_type'
        state
        bsym_table
        env
        { rs with as_fixlist = (s,rs.depth)::rs.as_fixlist }
        sr
        t
        params
        mkenv

  | TYP_typeof e ->
      if List.mem_assq e rs.expr_fixlist
      then begin
        (* Typeof is recursive *)
        let outer_depth = List.assq e rs.expr_fixlist in
        let fixdepth = outer_depth -rs.depth in
(* HACK metatype guess *)
        btyp_fix fixdepth (btyp_type 0)
      end else begin
        snd (bind_expression' state bsym_table env rs e [])
      end

  | TYP_array (t1,t2)->
      let t2 =
        match bt t2 with
        | BTYP_tuple [] -> btyp_unitsum 1
        | x -> x
      in
      btyp_array (bt t1, t2)

  | TYP_tuple ts -> btyp_tuple (List.map bt ts)
  | TYP_tuple_cons (_,t1,t2) -> btyp_tuple_cons (bt t1) (bt t2)
  | TYP_unitsum k ->
      begin match k with
      | 0 -> btyp_void ()
      | 1 -> btyp_tuple []
      | _ -> btyp_unitsum k
      end

  | TYP_sum ts ->
      let ts' = List.map bt ts in
      if Flx_btype.all_units ts' then
        btyp_unitsum (List.length ts)
      else
        btyp_sum ts'

  | TYP_function (d,c) -> btyp_function (bt d, bt c)
  | TYP_effector (d,e,c) -> btyp_effector (bt d, bt e,bt c)
  | TYP_cfunction (d,c) -> btyp_cfunction (bt d, bt c)
  | TYP_pointer t -> btyp_pointer (bt t)
  | TYP_void _ -> btyp_void ()

  | TYP_typefun (ps,r,body) ->
      let data = List.rev_map
        (fun (name, mt) -> name, bt mt, fresh_bid state.counter)
        ps
      in
      (* reverse order .. *)
      let pnames = List.map
        (fun (n, t, i) -> (n, btyp_type_var (i, t)))
        data
      in
      let bbody =
        bind_type'
          state
          bsym_table
          env
          { rs with depth=rs.depth + 1 }
          sr
          body
          (pnames@params)
          mkenv
      in
      (* order as written *)
      let bparams = List.rev_map (fun (n, t, i) -> (i, t)) data in

      btyp_type_function (bparams, bt r, bbody)

  | TYP_apply (TYP_name (_,"_flatten",[]),t2) ->
      let make_ts a t =
        List.fold_left begin fun acc b ->
          match b with
          | BTYP_unitsum b -> acc + b
          | BTYP_tuple [] -> acc + 1
          | BTYP_void -> acc
          | _ -> clierrx "[flx_bind/flx_lookup.ml:983: E96] " sr "Sum of unitsums required"
        end a t
      in
      let t2 = bt t2 in
      begin match t2 with
      | BTYP_unitsum a -> t2
      | BTYP_sum (BTYP_sum a :: t) ->
          let ts =
            List.fold_left begin fun acc b ->
              match b with
              | BTYP_sum b -> acc @ b
              | BTYP_void -> acc
              | _ -> clierrx "[flx_bind/flx_lookup.ml:995: E97] " sr "Sum of sums required"
            end a t
          in
          btyp_sum ts
      | BTYP_sum (BTYP_unitsum a :: t) -> btyp_unitsum (make_ts a t)
      | BTYP_sum (BTYP_tuple [] :: t) -> btyp_unitsum (make_ts 1 t)

      | _ -> clierrx "[flx_bind/flx_lookup.ml:1002: E98] " sr ("Cannot flatten type " ^ sbt bsym_table t2)
      end

  | TYP_apply (TYP_void _ as qn, t2')
  | TYP_apply (TYP_name _ as qn, t2')
  | TYP_apply (TYP_case_tag _ as qn, t2')
  | TYP_apply (TYP_typed_case _ as qn, t2')
  | TYP_apply (TYP_lookup _ as qn, t2')
  | TYP_apply (TYP_index _ as qn, t2')
  | TYP_apply (TYP_callback _ as qn, t2') ->
      let qn =
        match qualified_name_of_typecode qn with
        | Some qn -> qn
        | None -> assert false
       in
(*
print_endline ("Binding type application, qn = " ^ string_of_qualified_name qn ^" argument " ^ string_of_typecode t2');
*)
      let t2 = bt t2' in
(*
print_endline ("Type application,qn = " ^ string_of_qualified_name qn ^" argument " ^ string_of_typecode t2' ^ " bound=" ^ sbt bsym_table t2);
*)
      let sign = Flx_metatype.metatype state.sym_table bsym_table rs sr t2 in
(*
print_endline ("meta type of argument is " ^ sbt bsym_table sign);
*)
      begin try
        match qn with
        | `AST_name (sr,name,[]) ->
(*
print_endline ("Looking up name " ^ name ^ " in param list");
*)
          let fn = List.assoc name params in
          let r = btyp_type_apply (fn, t2) in
          let r = beta_reduce "flx_lookup: bind_TYP_apply" state.counter bsym_table sr r in
          r
        | _ -> raise Not_found
      with Not_found ->
        (* Note: parameters etc cannot be found with a qualified name, unless
         * it is a simple name .. which is already handled by the previous
         * case .. so we can drop them .. ? *)

        (* PROBLEM: we don't know if the term is a type alias or type
         * constructor. The former don't overload ..  the latter do ..
         * lookup_type_qn_with_sig is probably the wrong routine .. if it
         * finds a constructor, it seems to return the type of the constructor
         * instead of the actual constructor .. *)
(*
print_endline "Cannot find name in param list, doing lookup with metatype as sig";
*)
        let t1 = lookup_type_qn_with_sig'
          state
          bsym_table
          sr
          sr
          env
          {rs with depth=rs.depth + 1 }
          qn
          [sign]
        in
(*
print_endline ("Lookup type qn with sig: qn= " ^ string_of_qualified_name qn ^ " sig=" ^ sbt bsym_table sign);
print_endline ("Lookup type qn with sig found type " ^ sbt bsym_table t1);
*)
(*
        begin  match t1 with
        | BTYP_fix (j,mt) -> 
          print_endline ("Hmm .. lookup found a fixpoint?? " ^ string_of_int j); 
          print_endline "params = ";
          List.iter (fun (n,t) -> 
            print_endline (n ^ " value : " ^ sbt bsym_table t)
          )
          params
        | _ -> ()
        end;
*)
        let r = btyp_type_apply (t1,t2) in
(*
print_endline ("Completed binding type apply OK; " ^ sbt bsym_table r);
*)
(*
        let r = beta_reduce "flx_lookup: bind_TYP_apply: reduced application" state.counter bsym_table sr r in
*)
(*
print_endline ("reduced application is: " ^ sbt bsym_table r);
*)
        r
      end

  | TYP_apply (t1,t2) -> btyp_type_apply (bt t1, bt t2)
  | TYP_type_tuple ts -> btyp_type_tuple (List.map bt ts)
  | TYP_type -> btyp_type 0

  | TYP_name (sr,s,[]) when List.mem_assoc s rs.as_fixlist ->
(* HACK metatype guess *)
    btyp_fix ((List.assoc s rs.as_fixlist) - rs.depth) (btyp_type 0)

  | TYP_name (sr,s,[]) when List.mem_assoc s params ->
    let t = List.assoc s params in
(*
print_endline ("Binding TYP_name " ^s^ " via params to " ^ sbt bsym_table t);
*)
    t

  | TYP_index (sr,name,index) as x ->
      let sym =
        try hfind "lookup" state.sym_table index
        with Not_found ->
          syserr sr ("Synthetic name "^name ^ " not in symbol table!")
      in
      begin match sym.Flx_sym.symdef with
      | SYMDEF_struct _
      | SYMDEF_cstruct _
      | SYMDEF_union _
      | SYMDEF_abs _ ->
          let ts = List.map
            (fun (s,i,_) -> btyp_type_var (i, btyp_type 0))
            (fst sym.Flx_sym.vs)
          in
          btyp_inst (index,ts)
      | SYMDEF_typevar _ ->
          print_endline ("Synthetic name "^name ^ " is a typevar!");
          syserr sr ("Synthetic name "^name ^ " is a typevar!")

      | _ ->
          print_endline ("Synthetic name "^name ^ " is not a nominal type!");
          syserr sr ("Synthetic name "^name ^ " is not a nominal type!")
      end

  | TYP_name _
  | TYP_case_tag _
  | TYP_typed_case _
  | TYP_lookup _
  | TYP_callback _ as x ->
      let x =
        match qualified_name_of_typecode x with
        | Some q -> q
        | None -> assert false
      in
(*
if string_of_qualified_name x = "digraph_t" then begin
print_endline ("Bind type', name = " ^ string_of_qualified_name x);
end;
*)
      let sr2 = src_of_qualified_name x in
      let entry_kind, ts = lookup_qn_in_env' state bsym_table env rs x in
(*
if string_of_qualified_name x = "digraph_t" then begin
        print_endline ("bind_type': Type "^string_of_typecode t^"=Qualified name "^string_of_qualified_name x^" lookup finds index " ^
          string_of_bid entry_kind.Flx_btype.base_sym);
        print_endline ("Kind=" ^ match t with | TYP_name (_,s,ts) -> "TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "TYP_*");
        print_endline ("spec_vs=" ^
          catmap ","
            (fun (s,j)-> s ^ "<" ^ string_of_bid j ^ ">")
            entry_kind.Flx_btype.spec_vs);
        print_endline ("sub_ts=" ^
          catmap "," (sbt bsym_table) entry_kind.Flx_btype.sub_ts);
end;
*)
      let ts = List.map bt ts in
(*
if string_of_qualified_name x = "digraph_t" then begin
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
end;
*)
      let baset = bi
        entry_kind.Flx_btype.base_sym
        entry_kind.Flx_btype.sub_ts
      in
(*
if string_of_qualified_name x = "digraph_t" then begin
      print_endline ("Base type bound with sub_ts replacing type variables " ^ sbt bsym_table baset);
end;
*)
      (* SHOULD BE CLIENT ERROR not assertion *)
      if List.length ts != List.length entry_kind.Flx_btype.spec_vs then 
      begin
        print_endline ("bind_type': Type "^string_of_typecode t^"=Qualified name "^string_of_qualified_name x^" lookup finds index " ^
          string_of_bid entry_kind.Flx_btype.base_sym);
        print_endline ("Kind=" ^ match t with | TYP_name (_,s,ts) -> "TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "TYP_*");
        print_endline ("spec_vs=" ^
          catmap ","
            (fun (s,j)-> s ^ "<" ^ string_of_bid j ^ ">")
            entry_kind.Flx_btype.spec_vs);
        print_endline ("spec_ts=" ^
          catmap "," (sbt bsym_table) entry_kind.Flx_btype.sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        begin match
          hfind "lookup" state.sym_table entry_kind.Flx_btype.base_sym
        with
          | { Flx_sym.id=id; vs=vs; symdef=SYMDEF_typevar _ } ->
            print_endline (id ^ " is a typevariable, vs=" ^
              catmap ","
                (fun (s,j,_)-> s ^ "<" ^ string_of_bid j ^ ">")
                (fst vs)
            )
          | { Flx_sym.id=id } -> print_endline (id ^ " is not a type variable")
        end;

        clierr2 sr sr2
          ("Wrong number of type variables, expected " ^
          si (List.length entry_kind.Flx_btype.spec_vs) ^ ", but got " ^
          si (List.length ts))
      end;

      assert (List.length ts = List.length entry_kind.Flx_btype.spec_vs);
      let t = tsubst sr entry_kind.Flx_btype.spec_vs ts baset in
(*
if string_of_qualified_name x = "digraph_t" then begin
      print_endline ("Base type bound with input ts replacing spec type variables " ^ sbt bsym_table t);
end;
*)
      t

  | TYP_suffix (sr,(qn,t)) ->
      let sign = bt t in
      let result =
        lookup_qn_with_sig' state bsym_table sr sr env rs qn [sign]
      in
      begin match result with
      | BEXPR_closure (i,ts),_ -> bi i ts
      | _  ->
          clierrx "[flx_bind/flx_lookup.ml:1224: E99] " sr
          (
            "[typecode_of_expr] Type expected, got: " ^
            sbe bsym_table result
          )
    end
  in

(*
  if not (complete_type t) then
    print_endline ("-------->>>> *** Warning: bind_type' returns incomplete type " ^ sbt bsym_table t);
*)
  (*
  print_endline ("Bound type is " ^ sbt bsym_table t);
  *)
  t
(*
and cal_assoc_type state (bsym_table:Flx_bsym_table.t) sr t =
  let ct t = cal_assoc_type state bsym_table sr t in
  let chk ls =
    match ls with
    | [] -> btyp_void ()
    | h::t ->
      List.fold_left (fun acc t ->
        if acc <> t then
          clierrx "[flx_bind/flx_lookup.ml:1249: E100] " sr ("[cal_assoc_type] typeset elements should all be assoc type " ^ sbt bsym_table acc)
        ;
        acc
     ) h t
  in
  match t with
  | BTYP_type i -> t
  | BTYP_function (a,b) -> btyp_function (ct a, ct b)

  | BTYP_intersect ls
  | BTYP_type_set_union ls
  | BTYP_type_set ls -> let ls = List.map ct ls in chk ls

  | BTYP_tuple _
  | BTYP_record _
  | BTYP_variant _
  | BTYP_unitsum _
  | BTYP_sum _
  | BTYP_cfunction _
  | BTYP_pointer _
  | BTYP_array _
  | BTYP_void -> btyp_type 0

  | BTYP_inst (i,ts) ->
    (*
    print_endline ("Assuming named type "^si i^" is a TYPE");
    *)
    btyp_type 0


  | BTYP_type_match (_,ls) ->
    let ls = List.map snd ls in
    let ls = List.map ct ls in chk ls

  | _ -> clierrx "[flx_bind/flx_lookup.ml:1283: E101] " sr ("Don't know what to make of " ^ sbt bsym_table t)
*)

and bind_type_index state (bsym_table:Flx_bsym_table.t) (rs:recstop) sr index ts mkenv
=
(*
if index = 37335 then 
  print_endline "**** Bind_type_index, special 37335";
if index = 37335 then 
  print_endline (" **** RAW ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
*)
  let ts = adjust_ts state.sym_table bsym_table sr index ts in
(*
if index = 37335 then 
  print_endline (" **** Adjusted ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
*)
  (*
  print_endline ("Adjusted ts =h ["^ catmap ", " (sbt bsym_table) ts^ "]");
  *)
  let bt t =
      (*
      print_endline "Making params .. ";
      *)
      let vs,_ = find_vs state.sym_table bsym_table index in
(*
if index = 37335 then begin
        print_endline (" **** preparing to bind type " ^ string_of_typecode t);
        print_endline (" **** making params for call to bind type");
        print_endline (" **** vs=" ^
          catmap "," (fun (s,i,_)-> s ^ "<" ^ string_of_bid i ^ ">") vs);
        print_endline (" **** ts=" ^ catmap "," (sbt bsym_table) ts);
end;
*)
      if List.length vs <> List.length ts then 
      begin
        print_endline ("vs=" ^
          catmap "," (fun (s,i,_)-> s ^ "<" ^ string_of_bid i ^ ">") vs);
        print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
        failwith "len vs != len ts"
      end
      else
      (* I think this is the wrong idea: params is for type function parameters! *)
      (* let params = List.map2 (fun (s,i,_) t -> s,t) vs ts in *)
      (* so lets try with out them *)
      let params = [] in
(*
if index = 37335 then begin
  print_endline (" **** params = " ^ catmap "," (fun (s,t) -> s ^ " --> " ^ sbt bsym_table t) params);
end;
*)
      (*
      let params = make_params state.sym_table sr index ts in
      *)
      (*
      print_endline ("params made");
      *)
      let env:env_t = mkenv index in
      let t =
        bind_type' state bsym_table env
        { rs with type_alias_fixlist = (index,rs.depth):: rs.type_alias_fixlist }
        sr t params mkenv
      in
(*
if index = 37335 then begin
  print_endline (" **** Bound type is " ^ sbt bsym_table t);
  print_endline (" **** SHOULD HAVE VARIABLES REPLACED!");
end;
*)
(* DO A HACK NOW, cause params doesn't propagate *)
     let t = tsubst sr (List.map (fun (s,i,m) -> s,i) vs) ts t in 
(*
if index = 37335 then begin
  print_endline (" **** AFTER TSUBST Bound type is " ^ sbt bsym_table t);
  print_endline (" **** SHOULD HAVE VARIABLES REPLACED!");
end;
*)
(*
        print_endline ("Unravelled and bound is " ^ sbt bsym_table t);
        *)
        (*
        let t = beta_reduce state.counter sr t in
        *)
        (*
        print_endline ("Beta reduced: " ^ sbt bsym_table t);
        *)
        t
  in
(*
  print_endline
  (
    "BINDING INDEX " ^ string_of_int index ^
    " with ["^ catmap ", " (sbt bsym_table) ts^ "]"
  );
*)
  (*
  print_endline ("type alias fixlist is " ^ catmap ","
    (fun (i,j) -> si i ^ "(depth "^si j^")") type_alias_fixlist
  );
  *)
  if List.mem_assoc index rs.type_alias_fixlist
  then begin
(*
    print_endline (
      "Making fixpoint for Recursive type alias " ^
      (
        match get_data state.sym_table index with { Flx_sym.id=id;sr=sr}->
          id ^ " defined at " ^
          Flx_srcref.short_string_of_src sr
      )
    );
*)
    let mt = 
      begin try
        let data = get_data state.sym_table index in
        match data with { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
        match entry with
        | SYMDEF_type_alias t  -> 
          (*
          print_endline ("Index " ^ si index ^ " is a type alias " ^id ^ " = " ^ string_of_typecode t);
          *)
          let rec guess_metatype t =
            match t with
            | TYP_generic _ -> syserr sr ("[bind_type_index] Attempt to bind TYP_generic]")

            | TYP_defer _ -> print_endline "Guess metatype: defered type found"; assert false
            | TYP_tuple_cons (sr,t1,t2) -> assert false
            | TYP_type_tuple _ -> print_endline "A type tuple"; assert false
            | TYP_typefun (d,c,body) -> 
        (*
              print_endline ("A type fun: " ^ 
              catmap "," (fun (n,t) -> string_of_typecode t) d ^ " -> " ^ string_of_typecode c);
        *)
              let atyps = List.map (fun (_,t) -> bt t) d in
              let atyp = match atyps with
              | [x]->x
              | _ -> btyp_type_tuple atyps
              in
              let c = bt c in
              btyp_function (atyp, c)

            (* name like, its a big guess! *)
            | TYP_label
            | TYP_suffix _
            | TYP_index _
            | TYP_lookup _ 
            | TYP_name _ -> (* print_endline "A type name?"; *) btyp_type 0
            | TYP_as _ -> print_endline "A type as (recursion)?"; assert false

            (* usually actual types! *)
            | TYP_void _ 
            | TYP_case_tag _ 
            | TYP_typed_case _
            | TYP_callback _
            | TYP_patvar _ 
            | TYP_tuple _
            | TYP_unitsum _
            | TYP_sum _
            | TYP_intersect _
            | TYP_record _
            | TYP_polyrecord _
            | TYP_variant _
            | TYP_cfunction _
            | TYP_pointer _
            | TYP_type_extension _
            | TYP_array _ -> btyp_type 0

            (* note this one COULD be a type function type *)
            | TYP_function _ -> btyp_type 0
            | TYP_effector _ -> btyp_type 0

            | TYP_type -> btyp_type 1

            | TYP_dual t -> guess_metatype t

            | TYP_typeof _
            | TYP_var _
            | TYP_none 
            | TYP_ellipsis   
            | TYP_isin _ 

            | TYP_typeset _
            | TYP_setunion _
            | TYP_setintersection _


            | TYP_apply _

            | TYP_type_match _
            | TYP_patany _
              -> print_endline ("Woops, dunno meta type of " ^ string_of_typecode t); btyp_type 0
          in 
          guess_metatype t
        | _ -> print_endline ("Dunno, assume a type " ^ string_of_symdef entry id vs); assert false
      with _ ->
        print_endline "Can't bind type alias"; assert false
      end
    in
    let fixated = btyp_fix ((List.assoc index rs.type_alias_fixlist)-rs.depth) mt in
(*
print_endline ("flx_lookup: bind-type-index returning fixated " ^ sbt bsym_table fixated);
*)
    fixated
  end
  else begin
  (*
  print_endline "bind_type_index";
  *)
  let parent = Flx_sym_table.find_parent state.sym_table index in
  match get_data state.sym_table index with
  | { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
    (*
    if List.length vs <> List.length ts
    then
      clierrx "[flx_bind/flx_lookup.ml:1496: E102] " sr
      (
        "[bind_type_index] Wrong number of type arguments for " ^ id ^
        ", expected " ^
        si (List.length vs) ^ " got " ^ si (List.length ts)
      );
    *)
    match entry with
    | SYMDEF_typevar mt ->
      (* HACK! We will assume metatype are entirely algebraic,
        that is, they cannot be named and referenced, we also
        assume they cannot be subscripted .. the bt routine
        that works for type aliases doesn't seem to work for
        metatypes .. we get vs != ts .. ts don't make sense
        for type variables, only for named things ..
      *)
      (* WELL the above is PROBABLY because we're calling
      this routine using sye function to strip the view,
      so the supplied ts are wrong ..
      *)
      (*
      print_endline ("CALCULATING TYPE VARIABLE METATYPE " ^ si index ^ " unbound=" ^ string_of_typecode mt);
      *)
      (* weird .. a type variables parent function has an env containing
      the type variable .. so we need ITS parent for resolving the
      meta type ..??

      No? We STILL get an infinite recursion???????
      *)
      (*
      print_endline ("type variable index " ^ si index);
      *)
      let env = match parent with
        | Some parent ->
          (*
          print_endline ("It's parent is " ^ si parent);
          *)
          (*
          let {parent=parent} = hfind "lookup" state.sym_table parent in
          begin match parent with
          | Some parent ->
             print_endline ("and IT's parent is " ^ si parent);
          *)
            let mkenv i = mk_bare_env state bsym_table i in
            mkenv parent
          (*
          | None -> []
          end
          *)
        | None -> []
      in
      let mt = inner_bind_type state bsym_table env sr rs mt in
      (*
      print_endline ("Bound metatype is " ^ sbt bsym_table mt);
      let mt = cal_assoc_type state sr mt in
      print_endline ("Assoc type is " ^ sbt bsym_table mt);
      *)
      btyp_type_var (index,mt)

    (* type alias RECURSE *)
    | SYMDEF_type_alias t ->
(*
      if id = "digraph_t" then
      print_endline ("Bind type index: Unravelling type alias " ^ id ^ " index=" ^ si index);
*)
      let t = bt t in
(*
      if id = "digraph_t" then
      print_endline ("Bind type index: Alias "^id^"bound = " ^ sbt bsym_table t);
*)
      t

    | SYMDEF_abs _ ->
      btyp_inst (index,ts)

    | SYMDEF_newtype _
    | SYMDEF_union _
    | SYMDEF_struct _
    | SYMDEF_cstruct _
    | SYMDEF_typeclass
      ->
(*
print_endline ("bind type index, struct thing " ^ si index ^ " ts=" ^ catmap "," (sbt bsym_table) ts);
*)
      btyp_inst (index,ts)


    (* allow binding to type constructors now too .. *)
    | SYMDEF_const_ctor (uidx,ut,idx,vs') ->
      btyp_inst (index,ts)

    | SYMDEF_nonconst_ctor (uidx,ut,idx,vs',argt) ->
      btyp_inst (index,ts)

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:1591: E103] " sr
      (
        "[bind_type_index] Type " ^ id ^ "<" ^ string_of_bid index ^ ">" ^
        " must be a type [alias, abstract, union, struct], got:\n" ^
        string_of_symdef entry id vs
      )
  end

and base_type_of_literal sr {Flx_literal.felix_type=t } = TYP_name (sr,t,[])

and type_of_literal state bsym_table env sr v =
  let _,_,root,_,_ = List.hd (List.rev env) in
  let t = base_type_of_literal sr v in
  let bt = inner_bind_type state bsym_table env sr rsground t in
  bt

(* -------------------------------------------------------------------------- *)

(** Wrapper around inner_type_of_index that tries to cache the calculated type
 * for this index. *)
and type_of_index' state bsym_table rs sr bid =
  try
    Hashtbl.find state.ticache bid
  with Not_found ->
    let t =
      try 
        let result =  inner_type_of_index state bsym_table sr rs bid in
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
      Hashtbl.add state.ticache bid t;
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
    let result = cal_ret_type' state bsym_table (rs:recstop) index args
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


and cal_ret_type' state bsym_table (rs:recstop) index args =
  let mkenv i = build_env state bsym_table (Some i) in
  let env = mkenv index in
  let parent = Flx_sym_table.find_parent state.sym_table index in
  match (get_data state.sym_table index) with
  | { Flx_sym.id=id;
      sr=sr;
      dirs=dirs;
      symdef=SYMDEF_function ((ps,_),rt,effects,props,exes)
    } ->
(*
print_endline ("Cal ret type of " ^ id ^ " at " ^ Flx_srcref.short_string_of_src sr);
*)
    let rt = bind_type' state bsym_table env rs sr rt args mkenv in
    let rt = beta_reduce "flx_lookup: cal_ret_type" state.counter bsym_table sr rt in
    let ret_type = ref rt in
    let return_counter = ref 0 in
    List.iter
    (fun exe -> match exe with
    | (sr,EXE_fun_return e) ->
(*
print_endline ("Cal ret type of " ^ id ^ " got return: " ^ string_of_expr e);
*)
      incr return_counter;
      begin try
        let t =
          (* this is bad code .. we lose detection
          of errors other than recursive dependencies ..
          which shouldn't be errors anyhow ..
          *)
(*
print_endline ("Calling bind_epression'");
*)
            snd
            (
              bind_expression' state bsym_table env
              { rs with idx_fixlist = index::rs.idx_fixlist }
              e []
            )
        in
(* print_endline "Bound the expression .. "; *)
        if Flx_do_unify.do_unify
          state.counter
          state.varmap
          state.sym_table
          bsym_table
          !ret_type
          t
          (* the argument order is crucial *)
        then
          ret_type := varmap_subst state.varmap !ret_type
        else begin
          (*
          print_endline
          (
            "[cal_ret_type2] Inconsistent return type of " ^ id ^ "<"^string_of_int index^">" ^
            "\nGot: " ^ sbt bsym_table !ret_type ^
            "\nAnd: " ^ sbt bsym_table t
          )
          ;
          *)
          clierrx "[flx_bind/flx_lookup.ml:1776: E105] " sr
          (
            "[cal_ret_type2] Inconsistent return type of " ^ id ^ "<" ^
            string_of_bid index ^ ">" ^
            "\nGot: " ^ sbt bsym_table !ret_type ^
            "\nAnd: " ^ sbt bsym_table t
          )
        end
      with
        | Stack_overflow -> failwith "[cal_ret_type] Stack overflow"
        | Expr_recursion e -> (* print_endline "Expr recursion"; *)  ()
        | Free_fixpoint t ->  (* print_endline "Free fixpoint"; *) ()
        | Unresolved_return (sr,s) -> (* print_endline "Unresolved return"; *) ()
        | SimpleNameNotFound (sr,name,s) as e -> 
(*
          print_endline ("Whilst calculating return type:\nSimple name "^name^" not found"); 
*)
          raise e
        | ClientError (sr2,s) as e -> 
         print_endline ("ClientError Whilst calculating return type:\n"^s);
         raise (ClientError2 (sr,sr2,"Whilst calculating return type:\n"^s))

        | ClientError2 (sr,sr2,s) as e -> 
          print_endline ("ClientError2 Whilst calculating return type:\n"^s);
          raise e
        | x ->
        print_endline ("  .. Unable to compute type of " ^ string_of_expr e);
        print_endline ("Reason: " ^ Printexc.to_string x);
        raise x 
      end
    | _ -> ()
    )
    exes
    ;
    if !return_counter = 0 then (* it's a procedure .. *)
    begin
      let mgu = Flx_do_unify.do_unify
        state.counter
        state.varmap
        state.sym_table
        bsym_table
        !ret_type
        (btyp_void ())
      in
      ret_type := varmap_subst state.varmap !ret_type
    end
    ;
    (* not sure if this is needed or not ..
      if a type variable is computed during evaluation,
      but the evaluation fails .. substitute now
    ret_type := varmap_subst state.varmap !ret_type
    ;
    *)
    (*
    let ss = ref "" in
    Hashtbl.iter
    (fun i t -> ss:=!ss ^si i^ " --> " ^sbt bsym_table t^ "\n")
    state.varmap;
    print_endline ("state.varmap=" ^ !ss);
    print_endline ("  .. ret type index " ^ si index ^ " = " ^ sbt bsym_table !ret_type);
    *)
    !ret_type

  | _ -> assert false

(* -------------------------------------------------------------------------- *)

(** Find the type of a bound symbol. *)
and btype_of_bsym state bsym_table sr bt bid bsym =
  (* Helper function to convert function parameters to a type. *)
  let type_of_params params =
    btyp_tuple (Flx_bparameter.get_btypes params)
  in

  match Flx_bsym.bbdcl bsym with
  | BBDCL_label _ -> btyp_label ()
  | BBDCL_invalid -> assert false
  | BBDCL_module -> 
    clierrx "[flx_bind/flx_lookup.ml:1854: E106] " (Flx_bsym.sr bsym) ("Attempt to find type of module or library name " ^ Flx_bsym.id bsym)

  | BBDCL_fun (_,_,(params,_),return_type,effects,_) ->
    begin match effects with
    | BTYP_tuple [] ->
      btyp_function (type_of_params params, return_type)
    | _ ->
      btyp_effector (type_of_params params, effects, return_type)
    end
  | BBDCL_val (_,t,_) -> t
  | BBDCL_newtype (_,t) -> t
  | BBDCL_external_type _ -> clierr2 sr (Flx_bsym.sr bsym) ("Use type as if variable: " ^ Flx_bsym.id bsym)
  | BBDCL_external_const (_,_,t,_,_) -> t
  | BBDCL_external_fun (_,_,params,return_type,_,_,_) ->
      btyp_function (btyp_tuple params, return_type)
  | BBDCL_external_code _ -> assert false
  | BBDCL_union (_,ls) ->
      btyp_variant (List.map (fun (n,_,t) -> n,t) ls)
  | BBDCL_struct (_,ls)
  | BBDCL_cstruct (_,ls,_) ->
     let _,vs,_ = find_split_vs state.sym_table bsym_table bid in
(* print_endline ("btype of bsym struct " ^ Flx_bsym.id bsym ^ "<" ^ si bid ^ ">, #vs =" ^ si (List.length vs)); *)
      (* Lower a struct type into a function that creates the struct. *)
      let ts = List.map
        (fun (s,i,_) -> TYP_name (Flx_bsym.sr bsym,s,[]))
        vs 
      in
      let ts = List.map (bt (Flx_bsym.sr bsym)) ts in
      let ts = adjust_ts
        state.sym_table
        bsym_table
        (Flx_bsym.sr bsym)
        bid
        ts
      in
      let t = btyp_tuple (List.map snd ls) in
      btyp_function (t, btyp_inst (bid, ts))
  | BBDCL_typeclass _ 
  | BBDCL_instance _ 
  | BBDCL_const_ctor _ 
  | BBDCL_nonconst_ctor _ 
  | BBDCL_axiom 
  | BBDCL_lemma 
  | BBDCL_reduce ->
    clierrx "[flx_bind/flx_lookup.ml:1898: E107] " (Flx_bsym.sr bsym) ("Use entity as if variable:" ^ Flx_bsym.id bsym)
 

(* -------------------------------------------------------------------------- *)

(** This routine is called to find the type of a function or variable.
 * .. so there's no type_alias_fixlist .. *)
and inner_type_of_index state bsym_table sr rs index =
try
(*
print_endline ("***** Enter inner_type_of_index " ^ si index);
*)
  let result = inner_type_of_index' state bsym_table sr rs index in
(*
print_endline ("***** normal exit inner_type_of_index " ^ si index);
*)
  result
with exn ->
(*
print_endline ("***** abnormal exit inner_type_of_index " ^ si index);
*)
raise exn


and inner_type_of_index' state bsym_table sr rs index =
(*
  if index = 37461 then
     print_endline ("Inner type of index " ^ si index ^ "=pve");
*)
  (* Check if we've already cached this index. *)
  try Hashtbl.find state.ticache index with Not_found ->

  (* Check index recursion. If so, return a fix type. *)
(*  HACK: metatype guess *)
  if List.mem index rs.idx_fixlist then begin
(*
print_endline "inner_typeof+index returning fixpoint";
*)
    btyp_fix (-rs.depth) (btyp_type 0) 
  end else

  let mkenv i = build_env state bsym_table (Some i) in
  let env = mkenv index in
(*
if index = 37461 then print_env env;
*)
  (* Helper function that binds and beta reduces a type. *)
  let bt sr t =
    let t = bind_type' state bsym_table env rs sr t [] mkenv in
    beta_reduce "flx_lookup: inner_type_of_index" state.counter bsym_table sr t
  in

  (* First check if we've already bound this index. If so, return the type of
   * the symbol. Otherwise, look up the type in the environment. *)
  match
    try Some (Flx_bsym_table.find bsym_table index) with Not_found -> None
  with
  | Some bsym -> btype_of_bsym state bsym_table sr bt index bsym
  | None ->

  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_label s -> btyp_label ()

  | SYMDEF_callback _ ->
      print_endline "Inner type of index finds callback";
      assert false
  | SYMDEF_inherit qn ->
      failwith ("Woops inner_type_of_index found inherit " ^
        string_of_bid index)
  | SYMDEF_inherit_fun qn ->
      failwith ("Woops inner_type_of_index found inherit fun!! " ^
        string_of_bid index)
  | SYMDEF_type_alias t ->
(*
print_endline ("bind_type_index finds: Type alias name " ^ sym.Flx_sym.id);
*)
      let t = bt sym.Flx_sym.sr t in
      Flx_metatype.metatype state.sym_table bsym_table rs sym.Flx_sym.sr t

  | SYMDEF_function ((ps,_),rt,effects,props,_) ->
(*
print_endline ("** BEGIN ** Calculating Function type for function " ^ sym.Flx_sym.id ^ " index "^si index);
*)
      let pts = List.map (fun (_,_,t,_) -> t) ps in

      (* Calculate the return type. *)
      let rt =
        (* First, check if we've already calculated it. *)
        try Hashtbl.find state.varmap index with Not_found ->
          (* Nope! So let's and calculate it. Add ourselves to the fix list and
           * recurse. *)
          let rs = { rs with idx_fixlist = index::rs.idx_fixlist } in
          try
            let result = cal_ret_type state bsym_table rs index [] in
(*
print_endline ("** END **** Normal Exit Calculating Function type for function " ^ sym.Flx_sym.id );
*)
            result 
          with exn ->
(*
print_endline ("** END **** Abnormal Exit Function type for function " ^ sym.Flx_sym.id );
*)
          raise exn

      in
      (* this really isn't right .. need a better way to handle indeterminate
       * result .. hmm .. *)
      if var_i_occurs index rt then begin
        raise (Unresolved_return (sym.Flx_sym.sr,
          (
            "[type_of_index'] " ^
            "function " ^ sym.Flx_sym.id ^ "<" ^ string_of_bid index ^
            ">: Can't resolve return type, got : " ^
            sbt bsym_table rt ^
            "\nPossibly each returned expression depends on the return type" ^
            "\nTry adding an explicit return type annotation"
          )))
      end;

      let d = bt sym.Flx_sym.sr (type_of_list pts) in
      let e = bt sym.Flx_sym.sr effects in
      let ft = 
        if List.mem `Cfun props
        then btyp_cfunction (d, rt)
        else btyp_effector (d, e, rt)
      in
(*
print_endline ("** FINISH **** Calculating Function type for function " ^ sym.Flx_sym.id ^ ":" ^ sbt bsym_table ft);
*)
      ft

  | SYMDEF_const (_,t,_,_)
  | SYMDEF_val t
  | SYMDEF_var t -> bt sym.Flx_sym.sr t
  | SYMDEF_ref t -> btyp_pointer (bt sym.Flx_sym.sr t)

  | SYMDEF_lazy (t,x) -> bt sym.Flx_sym.sr t

  | SYMDEF_parameter (`PVal,t)
  | SYMDEF_parameter (`PVar,t) -> bt sym.Flx_sym.sr t

  | SYMDEF_const_ctor (_,t,_,_) -> bt sym.Flx_sym.sr t
  | SYMDEF_nonconst_ctor (_,ut,_,_,argt) ->
      bt sym.Flx_sym.sr (TYP_function (argt,ut))

  | SYMDEF_fun (_,pts,rt,_,_,_) ->
      bt sym.Flx_sym.sr (TYP_function (type_of_list pts,rt))

  | SYMDEF_union _ ->
      clierrx "[flx_bind/flx_lookup.ml:2048: E108] " sym.Flx_sym.sr ("Union " ^ sym.Flx_sym.id ^ " doesn't have a type")

  (* struct as function *)
  | SYMDEF_cstruct (ls,_)
  | SYMDEF_struct ls ->
      let _,vs,_  = find_split_vs state.sym_table bsym_table index in
      let ts = List.map
        (fun (s,i,_) -> TYP_name (sym.Flx_sym.sr,s,[]))
        vs
        (* (fst sym.Flx_sym.vs) *)
      in
      let ts = List.map (bt sym.Flx_sym.sr) ts in

(* print_endline "inner type of index .. struct .. about to adjust ts"; *)
      let ts = adjust_ts state.sym_table bsym_table sym.Flx_sym.sr index ts in
(* print_endline "inner type of index .. struct .. adjust ts done"; *)
      let t = type_of_list (List.map snd ls) in
      btyp_function (bt sym.Flx_sym.sr t, btyp_inst (index, ts))

  | SYMDEF_abs _ ->
      clierrx "[flx_bind/flx_lookup.ml:2068: E109] " sym.Flx_sym.sr
      (
        "[type_of_index] Expected declaration of typed entity for index " ^
        string_of_bid index ^ "\ngot abstract type " ^ sym.Flx_sym.id  ^
        " instead.\n" ^
        "Perhaps a constructor named " ^ "_ctor_" ^ sym.Flx_sym.id ^
        " is missing or out of scope?"
      )

  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2078: E110] " sym.Flx_sym.sr
      (
        "[type_of_index] Expected declaration of typed entity for index "^
        string_of_bid index ^ ", got " ^ sym.Flx_sym.id
      )

and cal_apply state bsym_table sr rs ((be1,t1) as tbe1) ((be2,t2) as tbe2) =
  let mkenv i = build_env state bsym_table (Some i) in
  let be i e = bind_expression' state bsym_table (mkenv i) rs e [] in
  let ((re,rt) as r) = cal_apply' state bsym_table be sr tbe1 tbe2 in
  r

and cal_apply' state bsym_table be sr ((be1,t1') as tbe1) ((be2,t2') as tbe2) =
  let t1 = normalise_tuple_cons bsym_table t1' in
  let t2 = normalise_tuple_cons bsym_table t2' in
(*
  if t1 <> t1' || t2 <> t2' then begin
print_endline ("cal_apply' BEFORE NORMALISE, fn = " ^ sbt bsym_table t1' ^ " arg=" ^ sbt bsym_table t2');
print_endline ("cal_apply', AFTER NORMALISE, fn = " ^ sbt bsym_table t1 ^ " arg=" ^ sbt bsym_table t2);
  end
  ;
*)
  let rest,reorder =
    match unfold "flx_lookup" t1 with
    | BTYP_function (argt,rest)
    | BTYP_cfunction (argt,rest) ->
      if type_match bsym_table state.counter argt t2
      then begin 
(*
        print_endline "Type of function parameter agrees with type of argument";
*)
        rest, None
      end
      else
      let reorder =
        match be1 with
        | BEXPR_closure (i,ts) ->
          begin match t2 with
            (* a bit of a hack .. *)
            | BTYP_record _
            | BTYP_tuple [] ->
              let rs = match t2 with
                | BTYP_record (rs) -> rs
                | BTYP_tuple [] -> []
                | _ -> assert false
              in
              let pnames =
                match hfind "lookup" state.sym_table i with
                | { Flx_sym.symdef=SYMDEF_function (ps,_,_,_,_) } ->
                  List.map 
                    begin fun (_,name,_,d) -> name, 
                      match d with 
                      | None -> None 
                      | Some e -> Some (be i e) 
                    end 
                    (fst ps)
                | _ -> assert false
              in
              let n = List.length rs in
              let rs = List.map2 (fun (name,t) j -> name,(j,t)) rs (nlist n) in

              begin try
                Some 
                  (List.map 
                    begin fun (name,d) ->
                      try
                        match List.assoc name rs with
                        | j,t -> 
(*
                          print_endline ("1:get_n arg" ^ sbe bsym_table tbe2); 
*)
                          bexpr_get_n t j tbe2
                      with Not_found ->
                        match d with
                        | Some d ->d
                        | None -> raise Not_found
                    end 
                    pnames
                  )
                with Not_found -> None
              end
            | _ -> None
          end
        | _ -> print_endline "WOOPS WHAT IF BE1 is NOT A CLOSURE?"; None
      in
(*
      print_endline "Type of function parameter DOES NOT agree with type of argument";
      print_endline ("Paramt = " ^ sbt bsym_table argt ^ " argt = " ^ sbt bsym_table t2);
*)
      begin match reorder with
      | Some _ -> rest,reorder
      | None ->
        clierrx "[flx_bind/flx_lookup.ml:2170: E111] " sr
        (
          "[cal_apply] Function " ^
          sbe bsym_table tbe1 ^
          "\nof type " ^
          sbt bsym_table t1 ^
          "\napplied to argument " ^
          sbe bsym_table tbe2 ^
          "\n of type " ^
          sbt bsym_table t2 ^
          "\nwhich doesn't agree with parameter type\n" ^
          sbt bsym_table argt
        )
      end

    (* HACKERY TO SUPPORT STRUCT CONSTRUCTORS *)
    | BTYP_inst (index,ts) ->
      begin match get_data state.sym_table index with
      { Flx_sym.id=id; symdef=entry } ->
        begin match entry with
        | SYMDEF_cstruct (cs,_) -> t1, None
        | SYMDEF_struct (cs) -> t1, None
        | _ ->
          clierrx "[flx_bind/flx_lookup.ml:2193: E112] " sr
          (
            "[cal_apply] Attempt to apply non-struct " ^ id ^ ", type " ^
            sbt bsym_table t1 ^
            " as constructor"
          )
        end
      end
    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2202: E113] " sr
      (
        "Attempt to apply non-function\n" ^
        sbe bsym_table tbe1 ^
        "\nof type\n" ^
        sbt bsym_table t1 ^
        "\nto argument of type\n" ^
        sbe bsym_table tbe2
      )
  in
  (*
  print_endline
  (
    "---------------------------------------" ^
    "\nApply type " ^ sbt bsym_table t1 ^
    "\nto argument of type " ^ sbt bsym_table t2 ^
    "\nresult type is " ^ sbt bsym_table rest ^
    "\n-------------------------------------"
  );
  *)

  let rest = varmap_subst state.varmap rest in
  if rest = btyp_void () then
    clierrx "[flx_bind/flx_lookup.ml:2225: E114] " sr
    (
      "[cal_apply] Function " ^
      sbe bsym_table tbe1 ^
      "\nof type " ^
      sbt bsym_table t1 ^
      "\napplied to argument " ^
      sbe bsym_table tbe2 ^
      "\n of type " ^
      sbt bsym_table t2 ^
      "\nreturns void"
    )
  else

  (* We have to allow type variables now .. the result
  should ALWAYS be determined, and independent of function
  return type unknowns, even if that means it is a recursive
  type, perhaps like 'Fix 0' ..: we should really test
  for the *function* return type variable not being
  eliminated ..
  *)
  (*
  if var_occurs rest
  then
    clierrx "[flx_bind/flx_lookup.ml:2249: E115] " sr
    (
      "[cal_apply] Type variable in return type applying\n" ^
        sbe bsym_table tbe1 ^
        "\nof type\n" ^
        sbt bsym_table t1 ^
        "\nto argument of type\n" ^
        sbe bsym_table tbe2
    )
  ;
  *)
  let x2 = match reorder with
  | None -> be2,t2
  | Some xs ->
    match xs with
    | [x]-> x
    | _ -> bexpr_tuple (btyp_tuple (List.map snd xs)) xs
  in
(*
print_endline ("ABout to bind apply result type=" ^ sbt bsym_table rest);
print_endline ("ABout to bind apply function type=" ^ sbt bsym_table t1);
print_endline ("ABout to bind apply argument type=" ^ sbt bsym_table (snd x2));
*)
  let x = bexpr_apply rest ((be1,t1), x2) in
(*
print_endline ("Bound apply = " ^ sbe bsym_table x);
*)
  x

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
  (*
  print_endline ("[lookup_qn_with_sig] " ^ string_of_qualified_name qn);
  print_endline ("sigs = " ^ catmap "," (sbt bsym_table) signs);
  print_endline ("expr_fixlist is " ^
    catmap ","
    (fun (e,d) -> string_of_expr e ^ " [depth " ^si d^"]")
    rs.expr_fixlist
  );
  *)
  let bt sr t =
    (*
    print_endline "NON PROPAGATING BIND TYPE";
    *)
    inner_bind_type state bsym_table env sr rs t
  in
  let handle_nonfunction_index index ts =
    begin match get_data state.sym_table index with
    | { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
      begin match entry with
      | SYMDEF_inherit_fun qn ->
          clierrx "[flx_bind/flx_lookup.ml:2351: E119] " sr "Chasing functional inherit in lookup_qn_with_sig'";

      | SYMDEF_inherit qn ->
          clierrx "[flx_bind/flx_lookup.ml:2354: E120] " sr "Chasing inherit in lookup_qn_with_sig'";

      | SYMDEF_cstruct _
      | SYMDEF_struct _ ->
        let sign = try List.hd signs with _ -> assert false in
        (*
        print_endline ("Lookup qn with sig' found a struct "^ id ^
        ", looking for constructor"); 
        *)
        (* this doesn't work, we need to do overload resolution to
           fix type variables
        let t = type_of_index_with_ts' state bsym_table rs sra index ts in
        *)
        let hack_vs,_ = vs in
        let hvs = List.map (fun (id,index,kind) -> id,index) hack_vs in
        let hts = List.map (fun (_,index) -> Flx_btype.btyp_type_var (index,Flx_btype.btyp_type 0)) hvs in
        let hacked_entry = { base_sym=index; spec_vs=hvs; sub_ts=hts } in
        let ro = resolve_overload state bsym_table env rs sra [hacked_entry] id signs ts in
        let (_,t),bts =
          match ro with
          | Some (index,t,ret,mgu,ts) ->
            handle_function
              state
              bsym_table
              rs
              sra srn id ts index, 
            ts
          | None ->
              clierrx "[flx_bind/flx_lookup.ml:2382: E121] " sra ("Struct "^id^" constructor arguments don't match member types")
        in
        assert (List.length hvs = List.length bts);
        (*
        print_endline ("Struct constructor found, type= " ^ sbt bsym_table t);
        print_endline ("Vs len= " ^ si (List.length hvs) ^ " ts len= " ^ si (List.length bts));
        *)
        (*
        print_endline (id ^ ": lookup_qn_with_sig: struct");
        *)
        (*
        let ts = adjust_ts state.sym_table sr index ts in
        *)
        begin match t with
        | BTYP_function (a,_) ->
          if not (type_match bsym_table state.counter a sign) then
            clierrx "[flx_bind/flx_lookup.ml:2398: E122] " sr
            (
              "[lookup_qn_with_sig] Struct constructor for "^id^" has wrong signature, got:\n" ^
              sbt bsym_table t ^
              "\nexpected:\n" ^
              sbt bsym_table sign
            )
        | _ -> assert false
        end
        ;
        (* actally the 'handle_function' call above already returns this .. *)
        bexpr_closure t (index,bts)

      | SYMDEF_newtype _
      | SYMDEF_union _
      | SYMDEF_abs _
      | SYMDEF_type_alias _ ->
        (*
        print_endline "mapping type name to _ctor_type [2]";
        *)
        let qn =  match qn with
          | `AST_name (sr,name,ts) -> `AST_name (sr,"_ctor_"^name,ts)
          | `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,"_ctor_"^name,ts))
          | _ -> failwith "Unexpected name kind .."
        in
        lookup_qn_with_sig' state bsym_table sra srn env rs qn signs

      | SYMDEF_const (_,t,_,_)
      | SYMDEF_val t
      | SYMDEF_var t
      | SYMDEF_ref t
      | SYMDEF_parameter (_,t)
        ->
        (*
        print_endline (id ^ ": lookup_qn_with_sig: val/var");
        let ts = adjust_ts state.sym_table sr index ts in
        *)
        let t = bt sr t in
        let bvs = List.map (fun (s,i,tp) -> s,i) (fst vs) in
        let t = try tsubst sr bvs ts t with _ -> failwith "[lookup_qn_with_sig] WOOPS" in
        begin match t with
        | BTYP_function (a,b) ->
          let sign = try List.hd signs with _ -> assert false in
          if not (type_match bsym_table state.counter a sign) then
          clierrx "[flx_bind/flx_lookup.ml:2442: E123] " srn
          (
            "[lookup_qn_with_sig] Expected variable "^id ^
            "<" ^ string_of_bid index ^
            "> to have function type with signature " ^
            sbt bsym_table sign ^
            ", got function type:\n" ^
            sbt bsym_table t
          )
          else begin
(*
print_endline ("LOOKUP 1: varname " ^ si index);
*)
            bexpr_varname t (index, ts)
          end
        | _ ->
          clierrx "[flx_bind/flx_lookup.ml:2458: E124] " srn
          (
            "[lookup_qn_with_sig] expected variable " ^
            id ^ "<" ^ string_of_bid index ^
            "> to be of function type, got:\n" ^
            sbt bsym_table t

          )
        end
      | _ ->
        (* This is WRONG, because it could be a type, in which
         * case we should try to find a constructor or it:
         * this works for simple name lookup, and the use of
         * a module name qualifier shouldn't make any difference!
         *
         * Well, the error is probably that the caller isn't
         * handling it, rather than this routine 
         *)
        clierrx "[flx_bind/flx_lookup.ml:2476: E125] " sr
        (
          "[lookup_qn_with_sig] Named Non function entry "^id^
          " must be function type: requires struct," ^
          "or value or variable of function type or primitve type or " ^
          "type alias with user defined constructor"
        )
      end
    end
  in
  match qn with
  | `AST_callback (sr,qn) ->
    failwith "[lookup_qn_with_sig] Callbacks not implemented yet"

  | `AST_void _ -> clierrx "[flx_bind/flx_lookup.ml:2490: E126] " sra "qualified-name is void"

  | `AST_case_tag _ -> clierrx "[flx_bind/flx_lookup.ml:2492: E127] " sra "Can't lookup case tag here"

  (* WEIRD .. this is a qualified name syntactically ..
    but semantically it belongs in bind_expression
    where this code is duplicated ..

    AH NO it isn't. Here, we always return a function
    type, even for constant constructors (because we
    have a signature ..)
  *)
  | `AST_typed_case (sr,v,t) ->
(*
    print_endline "AST_typed_case";
*)
    let t = bt sr t in
    begin match unfold "flx_lookup" t with
    | BTYP_unitsum k  ->
      if v<0 || v>= k
      then clierrx "[flx_bind/flx_lookup.ml:2510: E128] " sra "Case index out of range of sum"
      else
      begin match signs with 
      | [argt] ->
        if argt = btyp_tuple [] then begin
          let x = bexpr_unitsum_case v k in
(*
          print_endline ("unitsum case " ^ sbe bsym_table x);
*)
          x
        end else
          clierrx "[flx_bind/flx_lookup.ml:2521: E129] " sr 
            ("Unitsum case constructor requires argument of type unit, got " ^
             sbt bsym_table argt
            )
      | _ -> clierrx "[flx_bind/flx_lookup.ml:2525: E130] " sr "Case requires exactly one argument"
      end
    | BTYP_sum ls ->
      if v<0 || v >= List.length ls
      then clierrx "[flx_bind/flx_lookup.ml:2529: E131] " sra "Case index out of range of sum"
      else let vt = List.nth ls v in
      begin match signs with
      | [argt] -> 
        if vt = argt then begin
          let x = bexpr_nonconst_case vt (v,t) in
(*
          print_endline ("nonconst case " ^ sbe bsym_table x);
*)
          x
        end else
          clierrx "[flx_bind/flx_lookup.ml:2540: E132] " sr 
            ("Sum case constructor requires argument of type " ^
             sbt bsym_table vt ^ ", got " ^ sbt bsym_table argt
            )
      | _ -> clierrx "[flx_bind/flx_lookup.ml:2544: E133] " sr "Case requires exactly one argument"
      end

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2548: E134] " sr
      (
        "[lookup_qn_with_sig] Type of case must be sum, got " ^
        sbt bsym_table t
      )
    end

  | `AST_name (sr,name,ts) ->
(*
print_endline ("lookup_qn_with_sig' [AST_name] " ^ name ^ ", sigs=" ^ catmap "," (sbt bsym_table) signs);
*)
    (* HACKERY TO SUPPORT _ctor_type lookup -- this is really gross,
       since the error could be anything ..  the retry here should
       only be used if the lookup failed because sig_of_symdef found
       a typename..
    *)
    let ts = List.map (bt sr) ts in
    begin
      try
        lookup_name_with_sig
          state
          bsym_table
          sra srn
          env env rs name ts signs
      with
      | Free_fixpoint _ as x -> raise x
      | OverloadKindError (sr1,s1) ->
(*
print_endline ("OverloadKindError .. (trying ctor hack)");
*)
        begin
          try
            (*
            print_endline "Trying _ctor_ hack";
            *)
            lookup_name_with_sig
              state
              bsym_table
              sra srn
              env env rs ("_ctor_" ^ name) ts signs
          with ClientError (sr2,s2) ->
(*
print_endline ("ctor hack failed (client error)");
*)
             clierr2 sr1 sr2
             (
             "attempting name lookup of " ^ name ^ " got Overload Kind ERROR1: " ^ s1 ^
             "\nattempting name lookup of _ctor_" ^ name ^ " got ERROR2: " ^ s2
             )
        end
 
      | ClientError (sr1,s1) as x ->
(*
print_endline ("Client Error (trying ctor hack)?");
*)
(*
    print_endline ("Client Error: Lookup simple name " ^ name ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "] sig=" ^ catmap "," (sbt bsym_table) signs);
    print_env_long state.sym_table bsym_table env;
*)
        begin
          try
            (*
            print_endline "Trying _ctor_ hack";
            *)
            lookup_name_with_sig
              state
              bsym_table
              sra srn
              env env rs ("_ctor_" ^ name) ts signs
          with ClientError (sr2,s2) -> 
(*
print_endline ("ctor hack failed (client error)");
*)
           raise x
        end
      | x -> raise x
    end

  | `AST_index (sr,name,index) as x ->
    (*
    print_endline ("[lookup qn with sig] AST_index " ^ string_of_qualified_name x);
    *)
    begin match get_data state.sym_table index with
    | { Flx_sym.vs=vs; id=id; sr=sra; symdef=entry } ->
    match entry with
    | SYMDEF_fun _
    | SYMDEF_function _
      ->
      let vs = find_vs state.sym_table bsym_table index in
      let ts = List.map
        (fun (_,i,_) -> btyp_type_var (i,btyp_type 0))
        (fst vs)
      in
      let x = bexpr_closure
        (inner_type_of_index state bsym_table sr rs index)
        (index,ts)
      in
      x

    | _ ->
      (*
      print_endline "Non function ..";
      *)
      let ts = List.map
        (fun (_,i,_) -> btyp_type_var (i,btyp_type 0))
        (fst vs)
      in
      handle_nonfunction_index index ts
    end

  | `AST_lookup (sr,(qn',name,ts)) ->
(*
print_endline ("Lookup qn with sig: AST_lookup of " ^ name ^ " in " ^ string_of_expr qn');
*)
    let m =  eval_module_expr state bsym_table env qn' in
    match m with (Flx_bind_deferred.Simple_module (impl, ts',htab,dirs)) ->
    (* let n = List.length ts in *)
    let ts = List.map (bt sr)( ts' @ ts) in
    (*
    print_endline ("Module " ^ si impl ^ "[" ^ catmap "," (sbt bsym_table) ts' ^"]");
    *)
    let env' = mk_bare_env state bsym_table impl in
    let tables = get_pub_tables state bsym_table env' rs dirs in
    let result = lookup_name_in_table_dirs htab tables sr name in
    begin match result with
    | None ->
      clierrx "[flx_bind/flx_lookup.ml:2674: E135] " sr
      (
        "[lookup_qn_with_sig] AST_lookup: Simple_module: Can't find name " ^ name
      )
    | Some entries -> match entries with
    | NonFunctionEntry { base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts } ->
(*
      print_endline ("Lookup qn with sig, BASE index= " ^ si index );
*)
      handle_nonfunction_index (index) ts

    | FunctionEntry fs ->
      match
        resolve_overload
        state bsym_table env rs sra fs name signs ts
      with
      | Some (index,t,ret,mgu,ts) ->
        (*
        print_endline ("ts = [" ^ catmap ", " (sbt bsym_table) ts ^ "]");
        *)
        (*
        let ts = adjust_ts state.sym_table sr index ts in
        *)
        bexpr_closure
          (inner_type_of_index_with_ts state bsym_table rs sr index ts)
          (index,ts)

      | None ->
        try
        clierrx "[flx_bind/flx_lookup.ml:2703: E136] " sra
        (
          "[lookup_qn_with_sig] (Simple module) Unable to resolve overload of " ^
          string_of_qualified_name qn ^
          " of (" ^ catmap "," (sbt bsym_table) signs ^")\n" ^
          "candidates are: " ^ full_string_of_entry_set state.sym_table bsym_table entries
        )
        with Not_found -> failwith "Error generating error in lookup_qn_with_sig'"
    end

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
(*
print_endline ("Lookup type qn with sig, name = " ^ string_of_qualified_name qn);
*)
  let bt sr t =
    inner_bind_type state bsym_table env sr rs t
  in
  let handle_nonfunction_index index ts =
    print_endline ("Found non function? index " ^ string_of_bid index);
    begin match get_data state.sym_table index with
    { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs; symdef=entry } ->
      begin match entry with
      | SYMDEF_inherit_fun qn ->
          clierrx "[flx_bind/flx_lookup.ml:2736: E137] " sr "Chasing functional inherit in lookup_qn_with_sig'";

      | SYMDEF_inherit qn ->
          clierrx "[flx_bind/flx_lookup.ml:2739: E138] " sr "Chasing inherit in lookup_qn_with_sig'";

      | SYMDEF_cstruct _
      | SYMDEF_struct _ ->
        let sign = try List.hd signs with _ -> assert false in
        let t = inner_type_of_index_with_ts state bsym_table rs sr index ts in
        (*
        print_endline ("[lookup_type_qn_with_sig] Struct constructor found, type= " ^ sbt bsym_table t);
        *)
        begin match t with
        | BTYP_function (a,_) ->
          if not (type_match bsym_table state.counter a sign) then
            clierrx "[flx_bind/flx_lookup.ml:2751: E139] " sr
            (
              "[lookup_type_qn_with_sig] Struct constructor for "^id^" has wrong signature, got:\n" ^
              sbt bsym_table t ^
              "\nexpected:\n" ^
              sbt bsym_table sign
            )
        | _ -> assert false
        end
        ;
        t

      | SYMDEF_union _
      | SYMDEF_type_alias _ ->
        (*
        print_endline "mapping type name to _ctor_type [2]";
        *)
        let qn =  match qn with
          | `AST_name (sr,name,ts) -> `AST_name (sr,"_ctor_"^name,ts)
          | `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,"_ctor_"^name,ts))
          | _ -> failwith "Unexpected name kind .."
        in
        lookup_type_qn_with_sig' state bsym_table sra srn env rs qn signs

      | SYMDEF_const (_,t,_,_)
      | SYMDEF_val t
      | SYMDEF_var t
      | SYMDEF_ref t
      | SYMDEF_parameter (_,t)
        ->
        clierrx "[flx_bind/flx_lookup.ml:2781: E140] " sr (id ^ ": lookup_type_qn_with_sig: val/var/const/ref/param: not type");

      | _ ->
        clierrx "[flx_bind/flx_lookup.ml:2784: E141] " sr
        (
          "[lookup_type_qn_with_sig] Named Non function entry "^id^
          " must be type function"
        )
      end
    end
  in
  match qn with
  | `AST_callback (sr,qn) ->
    failwith "[lookup_qn_with_sig] Callbacks not implemented yet"

  | `AST_void _ -> clierrx "[flx_bind/flx_lookup.ml:2796: E142] " sra "qualified-name is void"

  | `AST_case_tag _ -> clierrx "[flx_bind/flx_lookup.ml:2798: E143] " sra "Can't lookup case tag here"

  | `AST_typed_case (sr,v,t) ->
    let t = bt sr t in
    begin match unfold "flx_lookup" t with
    | BTYP_unitsum k ->
      if v<0 || v>= k
      then clierrx "[flx_bind/flx_lookup.ml:2805: E144] " sra "Case index out of range of sum"
      else
        let ct = btyp_function (unit_t,t) in
        ct

    | BTYP_sum ls ->
      if v<0 || v >= List.length ls
      then clierrx "[flx_bind/flx_lookup.ml:2812: E145] " sra "Case index out of range of sum"
      else let vt = List.nth ls v in
      let ct = btyp_function (vt,t) in
      ct

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:2818: E146] " sr
      (
        "[lookup_qn_with_sig] Type of case must be sum, got " ^
        sbt bsym_table t
      )
    end

  | `AST_name (sr,name,ts) ->
(*
    print_endline ("AST_name " ^ name);
*)
    let ts = List.map (bt sr) ts in
    lookup_type_name_with_sig
      state
      bsym_table
      sra srn
      env env rs name ts signs

  | `AST_index (sr,name,index) as x ->
    print_endline ("[lookup_type_qn_with_sig] AST_index " ^ string_of_qualified_name x);
    begin match get_data state.sym_table index with
    | { Flx_sym.vs=vs; id=id; sr=sra; symdef=entry } ->
    match entry with
    | SYMDEF_fun _
    | SYMDEF_function _
      ->
      let vs = find_vs state.sym_table bsym_table index in
      let ts = List.map
        (fun (_,i,_) -> btyp_type_var (i, btyp_type 0))
        (fst vs)
      in
      begin try 
        let result = inner_type_of_index state bsym_table sr rs index in
        result
      with exn ->
(*
print_endline "Abnormal exit inner_type_of_index from `AST_index";
*)
      raise exn
      end

    | _ ->
      (*
      print_endline "Non function ..";
      *)
      let ts = List.map
        (fun (_,i,_) -> btyp_type_var (i, btyp_type 0))
        (fst vs)
      in
      handle_nonfunction_index index ts
    end

  | `AST_lookup (sr,(qn',name,ts)) ->
(*
print_endline ("Lookup type with qn found AST_lookup of " ^ name ^ " in " ^ string_of_expr qn');
*)
    let m =  eval_module_expr state bsym_table env qn' in
    match m with (Flx_bind_deferred.Simple_module (impl, ts',htab,dirs)) ->
    (* let n = List.length ts in *)
    let ts = List.map (bt sr)( ts' @ ts) in
    (*
    print_endline ("Module " ^ si impl ^ "[" ^ catmap "," (sbt bsym_table) ts' ^"]");
    *)
    let env' = mk_bare_env state bsym_table impl in
    let tables = get_pub_tables state bsym_table env' rs dirs in
    let result = lookup_name_in_table_dirs htab tables sr name in
    begin match result with
    | None ->
      clierrx "[flx_bind/flx_lookup.ml:2886: E147] " sr
      (
        "[lookup_type_qn_with_sig] AST_lookup: Simple_module: Can't find name " ^ name
      )
    | Some entries -> 
      print_endline "Found some entries .. ";
      match entries with
    | NonFunctionEntry (index) ->
(*
print_endline "Found non-function entry";
*)
      handle_nonfunction_index (sye index) ts

    | FunctionEntry fs ->
(*
print_endline "Found function entry";
*)
      match
        resolve_overload
        state bsym_table env rs sra fs name signs ts
      with
      | Some (index,t,ret,mgu,ts) ->
        print_endline ("Resolved overload for " ^ name ^ " index=" ^ string_of_int index);
        print_endline ("ts = [" ^ catmap ", " (sbt bsym_table) ts ^ "]");
        print_endline ("return kind is " ^ sbt bsym_table ret);
        print_endline ("argument kind is " ^ sbt bsym_table t);
        print_endline ("MGU: " ^ catmap ", " (fun (i,t)-> si i ^ "->" ^ sbt bsym_table t) mgu);
(* THIS IS WRONG!!!!! btyp_inst is ONLY for abstract types, never for type functions: 
  if we get a type function we should returns its (specialised) body! Otherwise beta-reduce
  wont work!
*)
(*
        btyp_inst (index, ts)
*)

        let sym = get_data state.sym_table index in
        begin match sym.Flx_sym.symdef with
        | SYMDEF_type_alias (TYP_typefun (args,ret,body) as tf)->
          print_endline ("Got type function " ^ string_of_typecode body); 
          let btf = 
            try bt sr tf 
            with _ -> assert false
          in 
          btf

        | SYMDEF_type_alias (TYP_name _) ->
          print_endline "Got some name, not expected!! "; assert false
        | _ -> print_endline "Got something weird"; assert false
        end
(*
        assert false;
        (* we should just return the actual function found, not its type! *)

        (*
        let ts = adjust_ts state.sym_table sr index ts in
        *)
        let t = type_of_index_with_ts' state bsym_table rs sr index ts in
        print_endline "WRONG!";
        t
*)
      | None ->
        clierrx "[flx_bind/flx_lookup.ml:2947: E148] " sra
        (
          "[lookup_type_qn_with_sig] (Simple module) Unable to resolve overload of " ^
          string_of_qualified_name qn ^
          " of (" ^ catmap "," (sbt bsym_table) signs ^")\n" ^
          "candidates are: " ^ full_string_of_entry_set state.sym_table bsym_table entries
        )
    end

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
  print_endline ("[lookup_name_with_sig'] " ^ name ^
    " of " ^ catmap "," (sbt bsym_table) t2)
  ;
*)
  match env with
  | [] ->
    clierrx "[flx_bind/flx_lookup.ml:2973: E149] " srn
    (
      "[lookup_name_with_sig] Can't find " ^ name ^ "[" ^catmap "," (sbt bsym_table) ts^ "]" ^
      " of " ^ catmap "," (sbt bsym_table) t2
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

   | [BTYP_inst (j,ts') as d] ->
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

   | [BTYP_pointer (BTYP_inst (j,ts')) as d] ->
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
           Some (bexpr_prj k d (BTYP_pointer ft)) 
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
       Some (bexpr_prj k d (BTYP_pointer ft))
     | None -> None
     end

   | [BTYP_pointer (BTYP_polyrecord (fields,v)) as d] ->
     if List.mem_assoc name fields 
     then 
      let ft = List.assoc name fields in
      Some (bexpr_rprj name d (BTYP_pointer ft))  (* MIGHT REQUIRE FIXPOINT FIXUP! *)
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
      let tbx=
        lookup_type_name_with_sig
          state
          bsym_table
          sra srn
          caller_env tail rs name ts t2
       in tbx

(* -------------------------------------------------------------------------- *)

and handle_type state bsym_table rs sra srn name ts index =
  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_function _
  | SYMDEF_fun _
  | SYMDEF_struct _
  | SYMDEF_cstruct _
  | SYMDEF_nonconst_ctor _
  | SYMDEF_callback _ -> btyp_inst (index,ts)
  | SYMDEF_type_alias _ ->
(*
print_endline ("Handle type " ^ name ^ " ... binding type index " ^ string_of_int index);
*)
      let mkenv i = build_env state bsym_table (Some i) in
      let t = bind_type_index state bsym_table rs sym.Flx_sym.sr index ts mkenv in
(*
print_endline ("Handle type " ^ name ^ " ... bound type is " ^ sbt bsym_table t);
*)
      t
 
  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:3245: E151] " sra ("[handle_type] Expected " ^ name ^ " to be function, got: " ^
        string_of_symdef sym.Flx_sym.symdef name sym.Flx_sym.vs)

and handle_function state bsym_table rs sra srn name ts index =
  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_function _
  | SYMDEF_fun _
  | SYMDEF_struct _
  | SYMDEF_cstruct _
  | SYMDEF_nonconst_ctor _
  | SYMDEF_callback _ ->
      let t = inner_type_of_index_with_ts
        state
        bsym_table
        rs
        sym.Flx_sym.sr
        index
        ts
      in

      (* Make sure we got a function type back. *)
      begin match t with
      | BTYP_cfunction _ | BTYP_function _ | BTYP_effector _ -> ()
      | BTYP_fix _ -> raise (Free_fixpoint t)
      | _ ->
          ignore (try unfold "flx_lookup" t with | _ -> raise (Free_fixpoint t));
          clierrx "[flx_bind/flx_lookup.ml:3272: E152] " sra
          (
            "[handle_function]: closure operator expected '" ^ name ^
            "' to have function type, got '" ^
            sbt bsym_table t ^ "'"
          )
      end;

      bexpr_closure t (index,ts)

  | SYMDEF_type_alias (TYP_typefun _) ->
      (* THIS IS A HACK .. WE KNOW THE TYPE IS NOT NEEDED BY THE CALLER .. *)
      let t = btyp_function (btyp_type 0, btyp_type 0) in
      bexpr_closure t (index,ts)

  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:3288: E153] " sra
      (
        "[handle_function] Expected " ^ name ^ " to be function, got: " ^
        string_of_symdef sym.Flx_sym.symdef name sym.Flx_sym.vs
      )

and handle_variable state bsym_table env rs index id sr ts t t2 =
  (* HACKED the params argument to [] .. this is WRONG!! *)
  let mkenv i = build_env state bsym_table (Some i) in

  (* we have to check the variable is the right type *)
  let t = bind_type' state bsym_table env rs sr t [] mkenv in
  let ts = adjust_ts state.sym_table bsym_table sr index ts in
  let vs = find_vs state.sym_table bsym_table index in
  let bvs = List.map (fun (s,i,tp) -> s,i) (fst vs) in
  let t = beta_reduce "flx_lookup: handle_variabe" state.counter bsym_table sr (tsubst sr bvs ts t) in
(*
print_endline ("Handle variable " ^ si index ^ "=" ^ id);
*)
  match t with
  | BTYP_cfunction (d,c)
  | BTYP_function (d,c) ->
      if type_match bsym_table state.counter d t2 then
      begin
(*
print_endline ("LOOKUP 2: varname " ^ si index);
*)
        Some (bexpr_varname t (index, ts))
      end
      else
        clierrx "[flx_bind/flx_lookup.ml:3318: E154] " sr
        (
          "[handle_variable(1)] Expected variable " ^ id ^
          "<" ^ string_of_bid index ^
          "> to have function type with signature " ^
          sbt bsym_table t2 ^
          ", got function type:\n" ^
          sbt bsym_table t
        )

    (* anything other than function type, dont check the sig,
       just return it..
    *)
  | _ -> 
(*
print_endline ("LOOKUP 3: varname " ^ si index);
*)
    Some (bexpr_varname t (index, ts))

(* -------------------------------------------------------------------------- *)

and lookup_name_in_table_dirs_with_sig
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2
=
  (*
  print_endline
  (
    "LOOKUP NAME "^name ^"["^
    catmap "," (sbt bsym_table) ts ^
    "] IN TABLE DIRS WITH SIG " ^ catmap "," (sbt bsym_table) t2
  );
  *)
  let result:entry_set_t =
    match lookup_name_in_htab table name with
    | Some x -> x
    | None -> FunctionEntry []
  in
  match result with
  | NonFunctionEntry (index) ->
    begin match get_data state.sym_table (sye index) with
    { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
    (*
    print_endline ("FOUND " ^ id);
    *)
    begin match entry with
    | SYMDEF_inherit _ ->
      clierrx "[flx_bind/flx_lookup.ml:3369: E155] " sra "Woops found inherit in lookup_name_in_table_dirs_with_sig"
    | SYMDEF_inherit_fun _ ->
      clierrx "[flx_bind/flx_lookup.ml:3371: E156] " sra "Woops found inherit function in lookup_name_in_table_dirs_with_sig"

    | (SYMDEF_cstruct _ | SYMDEF_struct _ )
      when
        (match t2 with
        | [BTYP_record _] -> true
        | _ -> false
        )
      ->
        (*
        print_endline ("lookup_name_in_table_dirs_with_sig finds struct constructor " ^ id);
        print_endline ("Record Argument type is " ^ catmap "," (sbt bsym_table) t2);
        *)
        Some (bexpr_closure (btyp_inst (sye index,ts)) (sye index,ts))
        (*
        failwith "NOT IMPLEMENTED YET"
        *)

    | SYMDEF_struct _
    | SYMDEF_cstruct _
    | SYMDEF_nonconst_ctor _
      ->
        (*
        print_endline ("lookup_name_in_table_dirs_with_sig finds struct constructor " ^ id);
        print_endline ("Argument types are " ^ catmap "," (sbt bsym_table) t2);
        *)
        let ro =
          resolve_overload
          state bsym_table caller_env rs sra [index] name t2 ts
        in
          begin match ro with
          | Some (index,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (1)";
            *)
            let tb =
              handle_function
              state
              bsym_table
              rs
              sra srn name ts index
            in
              Some tb
          | None -> None
          end
    | SYMDEF_newtype _
    | SYMDEF_abs _
    | SYMDEF_union _
    | SYMDEF_type_alias _ ->

      (* recursively lookup using "_ctor_" ^ name :
         WARNING: we might find a constructor with the
         right name for a different cclass than this one,
         it isn't clear this is wrong though.
      *)
      (*
      print_endline "mapping type name to _ctor_type";
      *)
      lookup_name_in_table_dirs_with_sig
        state
        bsym_table
        table
        dirs
        caller_env env rs sra srn ("_ctor_" ^ name) ts t2

    | SYMDEF_const_ctor (_,t,_,_)
    | SYMDEF_const (_,t,_,_)
    | SYMDEF_var t
    | SYMDEF_ref t
    | SYMDEF_val t
    | SYMDEF_parameter (_,t)
      ->
      let sign = try List.hd t2 with _ -> assert false in
      handle_variable state bsym_table env rs (sye index) id srn ts t sign

    | _
      ->
        clierrx "[flx_bind/flx_lookup.ml:3448: E157] " sra
        (
          "[lookup_name_in_table_dirs_with_sig] Expected " ^id^
          " to be struct or variable of function type, got " ^
          string_of_symdef entry id vs
        )
    end
    end

  | FunctionEntry fs ->
(*
    if name = "accumulate" then
    print_endline ("Lookup_name_in_table_dirs_with_sig Found function set size " ^ si (List.length fs));
*)
    let ro =
      resolve_overload
      state bsym_table caller_env rs sra fs name t2 ts
    in
    match ro with
      | Some (index,t,ret,mgu,ts) ->
(*
    if name = "accumulate" then begin 
       print_endline ("Overload resolved to index " ^ si index);
       print_endline ("handle_function (3) ts=" ^ catmap "," (sbt bsym_table) ts);
    end;
*)
        (*
        print_endline ("handle_function (3) ts=" ^ catmap "," (sbt bsym_table) ts);
        let ts = adjust_ts state.sym_table sra index ts in
        print_endline "Adjusted ts";
        *)
        let ((_,tt) as tb) =
          handle_function
          state
          bsym_table
          rs
          sra srn name ts index
        in
          (*
          print_endline ("SUCCESS: overload chooses " ^ full_string_of_entry_kind state.sym_table (mkentry state dfltvs index));
          print_endline ("Value of ts is " ^ catmap "," (sbt bsym_table) ts);
          print_endline ("Instantiated closure value is " ^ sbe bsym_table tb);
          print_endline ("type is " ^ sbt bsym_table tt);
          *)
          Some tb

      | None ->
        (*
        print_endline "Can't overload: Trying opens";
        *)
        let opens : entry_set_t list =
          uniq_cat []
          (
            List.concat
            (
              List.map
              (fun table ->
                match lookup_name_in_htab table name with
                | Some x -> [x]
                | None -> []
              )
              dirs
            )
          )
        in
        (*
        print_endline (si (List.length opens) ^ " OPENS BUILT for " ^ name);
        *)
        match opens with
        | [NonFunctionEntry i] when
          (
            match get_data state.sym_table (sye i) with
            { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
            (*
            print_endline ("FOUND " ^ id);
            *)
            match entry with
            | SYMDEF_abs _
            | SYMDEF_union _ -> true
            | _ -> false
           ) ->
            (*
            print_endline "mapping type name to _ctor_type2";
            *)
            lookup_name_in_table_dirs_with_sig
              state
              bsym_table
              table
              dirs
              caller_env env rs sra srn ("_ctor_" ^ name) ts t2
        | _ ->
        let fs =
          match opens with
          | [NonFunctionEntry i] -> [i]
          | [FunctionEntry ii] -> ii
          | _ ->
            merge_functions opens name
        in
          let ro =
            resolve_overload
            state bsym_table caller_env rs sra fs name t2 ts
          in
          (*
          print_endline "OVERLOAD RESOLVED .. ";
          *)
          match ro with
          | Some (result,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (4)";
            *)
            let tb =
              handle_function
              state
              bsym_table
              rs
              sra srn name ts result
            in
              Some tb
          | None ->
            (*
            print_endline "FAILURE"; flush stdout;
            *)
            None

and lookup_type_name_in_table_dirs_with_sig
  state
  bsym_table
  table
  dirs
  caller_env env rs
  sra srn name ts t2
=
(*
  print_endline
  (
    "LOOKUP TYPE NAME "^name ^"["^
    catmap "," (sbt bsym_table) ts ^
    "] IN TABLE DIRS WITH SIG " ^ catmap "," (sbt bsym_table) t2
  );
*)
  let mkenv i = build_env state bsym_table (Some i) in
  let bt sr t =
    bind_type' state bsym_table env rs sr t [] mkenv
  in

  let result:entry_set_t =
    match lookup_name_in_htab table name  with
    | Some x -> x
    | None -> FunctionEntry []
  in
  match result with
  | NonFunctionEntry (index) ->
    begin match get_data state.sym_table (sye index) with
    { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
    (*
    print_endline ("FOUND " ^ id);
    *)
    begin match entry with
    | SYMDEF_inherit _ ->
      clierrx "[flx_bind/flx_lookup.ml:3607: E158] " sra "Woops found inherit in lookup_type_name_in_table_dirs_with_sig"
    | SYMDEF_inherit_fun _ ->
      clierrx "[flx_bind/flx_lookup.ml:3609: E159] " sra "Woops found inherit function in lookup_type_name_in_table_dirs_with_sig"

    | SYMDEF_struct _
    | SYMDEF_cstruct _
    | SYMDEF_nonconst_ctor _
      ->
        (*
        print_endline "lookup_name_in_table_dirs_with_sig finds struct constructor";
        *)
        let ro =
          resolve_overload
          state bsym_table caller_env rs sra [index] name t2 ts
        in
          begin match ro with
          | Some (index,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (1)";
            *)
            let tb =
              handle_type
              state
              bsym_table
              rs
              sra srn name ts index
            in
              Some tb
          | None -> None
          end

    | SYMDEF_typevar mt ->
      let mt = bt sra mt in
      (* match function a -> b -> c -> d with sigs a b c *)
      let rec m f s = match f,s with
      | BTYP_function (d,c),h::t when d = h -> m c t
      | BTYP_type_function _,_ -> failwith "Can't handle actual lambda form yet"
      | _,[] -> true
      | _ -> false
      in
      if m mt t2
      then Some (btyp_type_var (sye index,mt))
      else
      (print_endline
      (
        "Typevariable has wrong meta-type" ^
        "\nexpected domains " ^ catmap ", " (sbt bsym_table) t2 ^
        "\ngot " ^ sbt bsym_table mt
      ); None)

    | SYMDEF_newtype _
    | SYMDEF_abs _
    | SYMDEF_union _
    | SYMDEF_type_alias _ ->
      print_endline "Found abs,union or alias";
      Some (btyp_inst (sye index, ts))


    | SYMDEF_label _
    | SYMDEF_const_ctor _
    | SYMDEF_const _
    | SYMDEF_var _
    | SYMDEF_ref _
    | SYMDEF_val _
    | SYMDEF_parameter _
    | SYMDEF_axiom _
    | SYMDEF_lemma _
    | SYMDEF_callback _
    | SYMDEF_fun _
    | SYMDEF_function _
    | SYMDEF_insert _
    | SYMDEF_instance _
    | SYMDEF_lazy _
    | SYMDEF_module
    | SYMDEF_library
    | SYMDEF_root _
    | SYMDEF_reduce _
    | SYMDEF_typeclass
      ->
        clierrx "[flx_bind/flx_lookup.ml:3686: E160] " sra
        (
          "[lookup_type_name_in_table_dirs_with_sig] Expected " ^id^
          " to be a type or functor, got " ^
          string_of_symdef entry id vs
        )
    end
    end

  | FunctionEntry fs ->
(*
    print_endline ("Found function set size " ^ si (List.length fs));
*)
    let ro =
      resolve_overload
      state bsym_table caller_env rs sra fs name t2 ts
    in
    match ro with
      | Some (index,t,ret,mgu,ts) ->
(*
        print_endline ("handle_function (3) ts=" ^ catmap "," (sbt bsym_table) ts);
        let ts = adjust_ts state.sym_table sra index ts in
        print_endline "Adjusted ts";
        print_endline ("Found functional thingo, " ^ string_of_bid index);
        print_endline (" ts=" ^ catmap "," (sbt bsym_table) ts);
*)
        let tb =
          handle_type
          state
          bsym_table
          rs
          sra srn name ts index
        in
(*
          print_endline ("SUCCESS: overload chooses " ^ full_string_of_entry_kind state.sym_table bsym_table (mkentry state dfltvs index));
          print_endline ("Value of ts is " ^ catmap "," (sbt bsym_table) ts);
          print_endline ("Instantiated type is " ^ sbt bsym_table tb);
*)
          Some tb

      | None ->
        (*
        print_endline "Can't overload: Trying opens";
        *)
        let opens : entry_set_t list =
          List.concat
          (
            List.map
            (fun table ->
              match lookup_name_in_htab table name with
              | Some x -> [x]
              | None -> []
            )
            dirs
          )
        in
        (*
        print_endline (si (List.length opens) ^ " OPENS BUILT for " ^ name);
        *)
        match opens with
        | [NonFunctionEntry i] when
          (
              match get_data state.sym_table (sye i) with
              { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
              (*
              print_endline ("FOUND " ^ id);
              *)
              match entry with
              | SYMDEF_abs _
              | SYMDEF_union _ -> true
              | _ -> false
           ) ->
           Some (btyp_inst (sye i, ts))

        | _ ->
        let fs =
          match opens with
          | [NonFunctionEntry i] -> [i]
          | [FunctionEntry ii] -> ii
          | _ ->
            merge_functions opens name
        in
          let ro =
            resolve_overload
            state bsym_table caller_env rs sra fs name t2 ts
          in
          (*
          print_endline "OVERLOAD RESOLVED .. ";
          *)
          match ro with
          | Some (result,t,ret,mgu,ts) ->
            (*
            print_endline "handle_function (4)";
            *)
            let tb =
              handle_type
              state
              bsym_table
              rs
              sra srn name ts result
            in
              Some tb
          | None ->
            (*
            print_endline "FAILURE"; flush stdout;
            *)
            None

and handle_map sr (f,ft) (a,at) =
    let t =
      match ft with
      | BTYP_function (d,c) ->
        begin match at with
        | BTYP_inst (i,[t]) ->
          if t <> d
          then clierrx "[flx_bind/flx_lookup.ml:3801: E161] " sr
            ("map type of data structure index " ^
            "must agree with function domain")
          else
            btyp_inst (i,[c])
        | _ -> clierrx "[flx_bind/flx_lookup.ml:3806: E162] " sr "map requires instance"
        end
      | _ -> clierrx "[flx_bind/flx_lookup.ml:3808: E163] " sr "map non-function"
    in
      (* actually this part is easy, it's just
      applies ((map[i] f) a) where map[i] denotes
      the map function generated for data structure i
      *)
      failwith "MAP NOT IMPLEMENTED"

and lookup_label_in_env state bsym_table env sr name : Flx_types.bid_t option =
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
  let sr = src_of_expr e in
(*
  print_endline ("[bind_expression'] " ^ string_of_expr e);
  print_endline ("expr_fixlist is " ^
    catmap ","
    (fun (e,d) -> string_of_expr e ^ " [depth " ^si d^"]")
    rs.expr_fixlist
  );
*)
  if List.mem_assq e rs.expr_fixlist
  then raise (Expr_recursion e)
  ;
  let rs = { rs with expr_fixlist=(e,rs.depth)::rs.expr_fixlist } in
  let be e' = bind_expression' state bsym_table env { rs with depth=rs.depth+1} e' [] in
  let bea e' = bind_expression' state bsym_table env { rs with depth=rs.depth+1} e' args in
  let mkenv i = build_env state bsym_table (Some i) in
  let bt sr t =
    (* we're really wanting to call bind type and propagate depth ? *)
    let t = bind_type' state bsym_table env { rs with depth=rs.depth +1 } sr t [] mkenv in
    let t = beta_reduce "flx_lookup: bind_expression'(1)" state.counter bsym_table sr t in
    t
  in
  let ti sr i ts =
    inner_type_of_index_with_ts
      state
      bsym_table
      { rs with depth = rs.depth + 1 }
      sr
      i
      ts
  in
  (*
  print_endline ("Binding expression " ^ string_of_expr e ^ " depth=" ^ string_of_int rs.depth);
  print_endline ("environment is:");
  print_env env;
  print_endline "==";
  *)
  let rt t = beta_reduce "flx_lookup: bind_expression'(2)" state.counter bsym_table sr t in
  let sr = src_of_expr e in
  match e with
  | EXPR_patvar _
  | EXPR_patany _
  | EXPR_vsprintf _
  | EXPR_interpolate _
  | EXPR_type_match _
  | EXPR_noexpand _
  | EXPR_letin _
  | EXPR_typeof _
  | EXPR_as _
  | EXPR_as_var _
  | EXPR_void _
  | EXPR_arrow _
  | EXPR_effector _
  | EXPR_longarrow _
  | EXPR_ellipsis _
  | EXPR_intersect _
  | EXPR_isin _
    ->
      clierrx "[flx_bind/flx_lookup.ml:3897: E166] " sr
     ("[bind_expression] Expected expression, got " ^ string_of_expr e)

  (* Use a trick! convert the casematch to a typematch
    which returns a unit sum, then use that to select
    the expression to bind
  *)
  | EXPR_typecase_match (sr,(uba,ms)) -> 
    let argt = bt sr uba in
(*
    let uba = Flx_typecode_of_btype.typecode_of_btype bsym_table state.counter sr argt in
*)
    let tpats,es = List.split ms in
    let n = List.length ms in
    let il = Flx_list.nlist n in
    let ps = List.map2 (fun t u-> t,TYP_unitsum u) tpats il in
    let ubt = TYP_type_match (uba,ps) in
    let selector =
      let btp t params = bind_type' state bsym_table env
        {rs with depth = rs.depth+1}
        sr t params mkenv
      in
      bind_type_match bsym_table state.counter (bt sr) btp [] sr argt ps ubt  
    in

(*
print_endline ("TYPEMATCH = " ^ sbt bsym_table selector);
*)
    let ix = beta_reduce "flx_lookup:typecase" state.counter bsym_table sr selector in 
(*
print_endline ("REDUCED = " ^ sbt bsym_table ix);
*)
    let index = match ix with
    | BTYP_void -> 0
    | BTYP_tuple [] -> 1
    | BTYP_unitsum n -> n
    | _ -> clierrx "[flx_bind/flx_lookup.ml:3933: E167] " sr ("Unable to match typecase argument type " ^ sbt bsym_table argt ^
      " with any case in\n" ^ sbt bsym_table selector)
    in
(*
print_endline ("Case number " ^ si index);
*)
    let x = List.nth es index in
    be x

  | EXPR_cond (sr,(c,t,f)) ->
    bexpr_cond (be c) (be t) (be f)

  | EXPR_label (sr,label) -> 
    let maybe_index = lookup_label_in_env state bsym_table env sr label in
    begin match maybe_index with
    | Some index -> bexpr_label index
    | None ->
      clierrx "[flx_bind/flx_lookup.ml:3950: E168] " sr ("Flx_lookup: Cannot find label " ^ label ^ " in environment");
    end

  | EXPR_range_check (sr, mi, v, mx) ->
    let (x,t) as v' = be v in
    bexpr_range_check t (be mi, v', be mx)

  | EXPR_callback (sr,qn) ->
    let es,ts = lookup_qn_in_env2' state bsym_table env rs qn in
    begin match es with
    | FunctionEntry [index] ->
       print_endline "Callback closure ..";
       let ts = List.map (bt sr) ts in
       bexpr_closure (ti sr (sye index) ts) (sye index, ts)
    | NonFunctionEntry  _
    | _ -> clierrx "[flx_bind/flx_lookup.ml:3965: E169] " sr
      "'callback' expression denotes non-singleton function set"
    end

  | EXPR_expr (sr,s,t,e) ->
    let t = bt sr t in
    let e = be e in
    bexpr_expr (s,t,e)

  | EXPR_andlist (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "land" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3978: E170] " sri "Not expecting empty and list"
    end

  | EXPR_orlist (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "lor" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3985: E171] " sri "Not expecting empty or list"
    end

  | EXPR_sum (sri,ls) ->
    begin let mksum a b = Flx_strr.apl2 sri "+" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mksum h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3992: E172] " sri "Not expecting empty product (unit)"
    end

  | EXPR_product (sri,ls) ->
    begin let mkprod a b = Flx_strr.apl2 sri "*" [a;b] in
    match ls with
    | h::t -> be (List.fold_left mkprod h t)
    | [] -> clierrx "[flx_bind/flx_lookup.ml:3999: E173] " sri "Not expecting empty sum (void)"
    end

  | EXPR_superscript (sri,(a,b)) ->
    be (Flx_strr.apl2 sri "pow" [a; b])

  | EXPR_coercion (sr,(x,t)) ->
    let (e',t') as x' = be x in
    let t'' = bt sr t in
    Flx_coerce.coerce state bsym_table sr x' t''

  | EXPR_tuple_cons (_, eh, et) ->
    let _,eht' as xh' = be eh in
    let _,ett' as xt' = be et in
    let t = btyp_tuple_cons eht' ett' in
(*
    print_endline ("Type of tuple cons is " ^ sbt bsym_table t);
*)
    let _,t as x = bexpr_tuple_cons t (xh',xt') in
(*
print_endline ("Bound tuple cons " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x


  | EXPR_get_tuple_tail (sr,e) ->
(*
print_endline "Binding tuple tail";
*)
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> x'
    | BTYP_tuple ts ->
      let n = List.length ts in
      let ts' = List.tl ts in
      let counter = ref 0 in
      let es' = List.map (fun t-> 
        incr counter; 
(*
        print_endline ("2:get_n arg" ^ sbe bsym_table x');         
*)
        bexpr_get_n t (!counter) x'
      ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x
    | BTYP_array (at,BTYP_unitsum n) ->
      if n>20 then begin
        print_endline ("Array type " ^ sbt bsym_table t' ^ " size " ^ string_of_int n ^ 
          " too long to get tuple tail"); 
        assert false
      end else begin
        let ts' = repeat at (n-1) in
        let counter = ref 0 in
        let es' = List.map (fun t-> 
          incr counter; 
          bexpr_get_n t (!counter) x'
        ) ts'
      in 
      let _,t as x = bexpr_tuple (btyp_tuple ts') es' in
      x
      end

    | BTYP_tuple_cons (t1,t2) -> 
      let _,t as x = bexpr_tuple_tail t2 x' in
(*
print_endline ("Bound tuple tail " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple tail of type " ^ sbt bsym_table t'); assert false
    end

  | EXPR_get_tuple_head (sr,e) ->
    let (e',t') as x' = be e in
    begin match t' with
    | BTYP_tuple [] -> assert false
    | BTYP_tuple ts ->
      let ht = List.hd ts in
      let _,t as x  = 
(*
        print_endline ("3:get_n arg" ^ sbe bsym_table x');         
*)
        bexpr_get_n ht 0 x'  
      in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | BTYP_array (at,BTYP_unitsum n) ->
      let _,t as x  = 
        bexpr_get_n at 0 x'  
      in
      x


    | BTYP_tuple_cons (t1,t2) -> 
      let _,t as x = bexpr_tuple_head t1 x' in
(*
print_endline ("Bound tuple head " ^ sbe bsym_table x ^ " has type " ^ sbt bsym_table t);
*)
      x

    | _ ->  print_endline ("tuple head of type " ^ sbt bsym_table t'); assert false
    end

  | EXPR_get_n (sr,(n,e')) ->
    let expr,typ = be e' in
    let ctyp,k = match unfold "flx_lookup" typ with
    | BTYP_array (t,BTYP_unitsum len)  ->
      if n<0 || n>len-1
      then clierrx "[flx_bind/flx_lookup.ml:4114: E174] " sr
        (
          "[bind_expression] Tuple index " ^
          string_of_int n ^
          " out of range 0.." ^
          string_of_int (len-1)
        )
      else t,len

    | BTYP_tuple ts
      ->
      let len = List.length ts in
      if n<0 || n>len-1
      then clierrx "[flx_bind/flx_lookup.ml:4127: E175] " sr
        (
          "[bind_expression] Tuple index " ^
          string_of_int n ^
          " out of range 0.." ^
          string_of_int (len-1)
        )
      else List.nth ts n,len

    | BTYP_tuple_cons (t1,_) ->
      if n = 0 then t1,2 (* HACK! We dunno the length of the tuple! *)
      else
      clierrx "[flx_bind/flx_lookup.ml:4139: E176] " sr
      (
        "[bind_expression] Expected tuple " ^
        string_of_expr e' ^
        " to have tuple type, got tuple cons " ^
        sbt bsym_table typ ^ " with non-zero projection " ^ si n
      )

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4148: E177] " sr
      (
        "[bind_expression] Expected tuple " ^
        string_of_expr e' ^
        " to have tuple type, got " ^
        sbt bsym_table typ
      )
    in
      let a = expr,typ in
(*
      print_endline ("4:get_n arg" ^ sbe bsym_table a);         
*)
      bexpr_get_n ctyp n a

  | EXPR_get_named_variable (sr,(name,e')) ->
(*
print_endline ("Find field name " ^ name ^ " of " ^ string_of_expr e');
*)
    let e'',t'' as x2 = be e' in
    begin match t'' with
    | BTYP_polyrecord (es,_) ->
      begin try 
        let ct = List.assoc name es in
        let prj = bexpr_rprj name t'' ct in
        bexpr_apply ct (prj,x2)
      with 
        Not_found ->
          clierrx "[flx_bind/flx_lookup.ml:4175: E178] " sr ("Field " ^ name ^ " is not a field of " ^ sbt bsym_table t'')
      end

    | BTYP_record (es)
      ->
      let k = List.length es in
      let field_name = name in
      begin match list_index (List.map fst es) field_name with
      | Some n -> 
        let t = List.assoc field_name es in
(*
        print_endline ("5:get_n arg" ^ sbe bsym_table x2);         
*)
        bexpr_get_n t n x2
      | None -> clierrx "[flx_bind/flx_lookup.ml:4189: E179] " sr
         (
           "Field " ^ field_name ^
           " is not a member of record " ^
           sbt bsym_table t''
          )
      end

    | t -> 
     clierrx "[flx_bind/flx_lookup.ml:4198: E180] " sr ("[bind_expression] Projection requires record or polyrecord instance, got type:\n" ^ sbt bsym_table t)
    end
  | EXPR_case_index (sr,e) ->
    let (e',t) as e  = be e in
    begin match t with
    | BTYP_unitsum _ -> ()
    | BTYP_sum _ -> ()
    | BTYP_variant _ -> ()
    | BTYP_type_var _ -> ()
    | BTYP_inst (i,_) ->
      begin match hfind "lookup" state.sym_table i with
      | { Flx_sym.symdef=SYMDEF_union _} -> ()
      | { Flx_sym.id=id} -> clierrx "[flx_bind/flx_lookup.ml:4210: E181] " sr ("Argument of caseno must be sum or union type, got abstract type " ^ id)
      end
    | _ -> clierrx "[flx_bind/flx_lookup.ml:4212: E182] " sr ("Argument of caseno must be sum or union type, got " ^ sbt bsym_table t)
    end
    ;
    let int_t = bt sr (TYP_name (sr,"int",[])) in
    begin match e' with
    | BEXPR_case (i,_) ->
      bexpr_literal int_t {Flx_literal.felix_type="int"; internal_value=string_of_int i; c_value=string_of_int i}
    | _ -> bexpr_case_index int_t e
    end

  | EXPR_case_tag (sr,v) ->
     clierrx "[flx_bind/flx_lookup.ml:4223: E183] " sr "plain case tag not allowed in expression (only in pattern)"

  | EXPR_variant (sr,(s,e)) ->
    let (_,t) as e = be e in
    bexpr_variant (btyp_variant [s,t]) (s,e)

  | EXPR_projection (sr,v,t) -> 
    let t = bt sr t in
    begin match t with
    | BTYP_tuple ts ->
      let n = List.length ts in
      if v < 0 || v >= n then
        clierrx "[flx_bind/flx_lookup.ml:4235: E184] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
          " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
      else
        let c = List.nth ts v in
        bexpr_prj v t c
 
    | BTYP_array (BTYP_unitsum n,base) ->
      if v < 0 || v >= n then
        clierrx "[flx_bind/flx_lookup.ml:4243: E185] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
          " negative or >= " ^ si n ^ "for array type " ^ sbt bsym_table t)
      else
        bexpr_prj v t base

    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4249: E186] " sr ("[Flx_lookup.bind_expression] projection requires tuple or array type, got " ^ sbt bsym_table t);
    end

  | EXPR_typed_case (sr,v,t) ->
(*
print_endline ("Evaluating EXPPR_typed_case index=" ^ si v ^ " type=" ^ string_of_typecode t);
*)
    let t = bt sr t in
    ignore (try unfold "flx_lookup" t with _ -> failwith "AST_typed_case unfold screwd");
    begin match unfold "flx_lookup" t with
    | BTYP_unitsum k ->
      if v<0 || v>= k
      then clierrx "[flx_bind/flx_lookup.ml:4261: E187] " sr "Case index out of range of sum"
      else begin
(*
        print_endline "unitsum case";
*)
        bexpr_unitsum_case v k  (* const ctor *)
      end
    | BTYP_sum ls ->
      if v<0 || v>= List.length ls
      then clierrx "[flx_bind/flx_lookup.ml:4270: E188] " sr "Case index out of range of sum"
      else let vt = List.nth ls v in
      begin match vt with
      | BTYP_tuple [] -> 
        let x = bexpr_const_case (v,t) in
(*
        print_endline ( "const case " ^ sbe bsym_table x); 
*)
        x
      | _ -> 
        let x = bexpr_nonconst_case vt (v,t) in
(*
        print_endline ("Nonconst case " ^ sbe bsym_table x);
*)
        x
      end
    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4287: E189] " sr
      (
        "[bind_expression] Type of case must be sum, got " ^
        sbt bsym_table t
      )
    end

  | EXPR_name (sr,name,ts) ->
    if name = "_felix_type_name" then
       let sname = catmap "," string_of_typecode ts in
       let x = EXPR_literal (sr, {Flx_literal.felix_type="string"; internal_value=sname; c_value=Flx_string.c_quote_of_string sname}) in
       be x
    else
    let ts = List.map (bt sr) ts in
    let lookup_result = 
      try inner_lookup_name_in_env state bsym_table env rs sr name 
      with exn -> 
(*
        print_endline ("[bind_expression'] lookup of name "^name^" FAILED with "^ Printexc.to_string exn); 
*)
        raise exn
    in
    begin match lookup_result with
    | NonFunctionEntry { base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts } ->
      (*
      let index = sye index in
      let ts = adjust_ts state.sym_table sr index ts in
      *)
      (*
      print_endline ("NAME lookup finds index " ^ string_of_bid index);
      print_endline ("spec_vs=" ^ catmap "," (fun (s,j)->s^"<"^si j^">") spec_vs);
      print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
      print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
      begin match hfind "lookup" state.sym_table index with
        | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
          print_endline (id ^ " is a typevariable, vs=" ^
            catmap "," (fun (s,j,_)->s^"<"^si j^">") (fst vs)
          )
        | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
      end;
      *)
      (* should be a client error not an assertion *)
      if List.length spec_vs <> List.length ts then begin
        (*
        print_endline ("bind_expression'; Expr_name: NonFunctionEntry: BINDING NAME " ^ name);
        begin match hfind "lookup" state.sym_table index with
          | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
            print_endline (id ^ " is a typevariable, vs=" ^
              catmap ","
                (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">")
                (fst vs)
            )
          | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
        end;
        print_endline ("NAME lookup finds index " ^ string_of_bid index);
        print_endline ("spec_vs=" ^
          catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") spec_vs);
        print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        *)
        clierrx "[flx_bind/flx_lookup.ml:4347: E190] " sr "[lookup,AST_name] ts/vs mismatch"
      end;

      let ts = List.map (tsubst sr spec_vs ts) sub_ts in
      let ts = adjust_ts state.sym_table bsym_table sr index ts in
      let t = ti sr index ts in

      (* If we have a bound symbol for this index, return it's type. Otherwise,
       * try to figure out the type of the unbound symbol. *)
      begin match
        try Some (Flx_bsym_table.find bsym_table index) with Not_found -> None
      with
      | Some bsym ->
          (* We got a bound symbol, so this should be easy. We now just have to
           * handle reference types directly, which we'll automatically convert
           * that into dereferencing the name. *)
          begin match Flx_bsym.bbdcl bsym with
          | BBDCL_val (_,_,`Ref) ->
              (* We've got a reference, so make sure the type is a pointer. *)
              let t' = 
                match t with 
                | BTYP_pointer t' -> t' 
                | _ ->
                  failwith ("[lookup, AST_name] expected ref " ^ name ^
                  " to have pointer type")
              in
(*
print_endline ("LOOKUP 4: varname " ^ si index);
*)
              bexpr_deref t' (bexpr_varname t (index, ts))

          | BBDCL_struct _
          | BBDCL_cstruct _
            ->
            bexpr_closure t (index, ts)

(* DEPRECATED *)
          | BBDCL_nonconst_ctor _ ->
            bexpr_closure t (index, ts)


          | BBDCL_external_const _ 
          | BBDCL_val _ 
            -> 
(*
print_endline ("LOOKUP 5: varname " ^ si index);
*)
            bexpr_varname t (index, ts)

(* DEPRECATED!! *)
          | BBDCL_const_ctor _  ->
            bexpr_varname t (index, ts)

          | BBDCL_label _ ->
            bexpr_label index

          | BBDCL_fun _ 
            ->
            clierrx "[flx_bind/flx_lookup.ml:4405: E191] " sr ("Flx_lookup: bind_expression: EXPR_name] Nonfunction entry: Expected name "^name^ 
            " of struct, cstruct, constructor, const, or variable, got function!")
 
          | _ ->
            clierrx "[flx_bind/flx_lookup.ml:4409: E192] " sr ("Flx_lookup: bind_expression: EXPR_name] Nonfunction entry: Expected name "^name^ 
            " of struct, cstruct, constructor, const, or variable")
 
          end
      | None ->
          (* We haven't bound this symbol yet. We need to specially handle
           * reference types, as I mentioned above. *)
          begin match hfind "lookup:ref-check" state.sym_table index with
          | { Flx_sym.symdef=SYMDEF_ref _ } ->
              (* We've got a reference, so make sure the type is a pointer. *)
              let t' = 
                match t with 
                | BTYP_pointer t' -> t' 
                | _ ->
                failwith ("[lookup, AST_name] expected ref " ^ name ^
                  " to have pointer type")
              in
(*
print_endline ("LOOKUP 6: varname " ^ si index);
*)
              bexpr_deref t' (bexpr_varname t (index, ts))

          (* these should have function entries *)
          | { Flx_sym.symdef=SYMDEF_fun _ }
          | { Flx_sym.symdef=SYMDEF_function _ } -> assert false

          | { Flx_sym.symdef=SYMDEF_struct _ }
          | { Flx_sym.symdef=SYMDEF_cstruct _ }
            ->
            (*
            print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to closure");
            *)
            bexpr_closure t (index,ts)

(* DEPRECATED *)
          | { Flx_sym.symdef=SYMDEF_nonconst_ctor _ } ->
            bexpr_closure t (index,ts)

          | { Flx_sym.symdef=SYMDEF_const  _ }
          | { Flx_sym.symdef=SYMDEF_var _ }
          | { Flx_sym.symdef=SYMDEF_val _ }
          | { Flx_sym.symdef=SYMDEF_parameter _ }
            ->
            (*
            print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to variable");
            *)
(*
print_endline ("LOOKUP 7: varname " ^ si index);
*)
            bexpr_varname t (index,ts)

(* DEPRECATED *)
          | { Flx_sym.symdef=SYMDEF_const_ctor  _ } ->
            bexpr_varname t (index,ts)

          | { Flx_sym.symdef=SYMDEF_label _ } ->
            bexpr_label index


          | _ -> 
            clierrx "[flx_bind/flx_lookup.ml:4469: E193] " sr ("[Flx_lookup.bind_expression: EXPR_name]: Nonfunction entry: Binding " ^ 
              name ^ "<"^si index^">"^ " requires closure or variable")
          end
      end

    | FunctionEntry [{base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts} as f]
    ->
      (* should be a client error not an assertion *)
      if List.length spec_vs <> List.length ts then begin
        (*
        print_endline ("bind_expression'; Expr_name: FunctionEntry: BINDING NAME " ^ name);
        begin match hfind "lookup" state.sym_table index with
          | { Flx_sym.id=id;vs=vs;symdef=SYMDEF_typevar _} ->
            print_endline (id ^ " is a typevariable, vs=" ^
              catmap "," (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">") (fst vs)
            )
          | { Flx_sym.id=id} -> print_endline (id ^ " is not a type variable")
        end;
        print_endline ("NAME lookup finds index " ^ string_of_bid index);
        print_endline ("spec_vs=" ^
          catmap "," (fun (s,j) -> s ^ "<" ^ string_of_bid j ^ ">") spec_vs);
        print_endline ("spec_ts=" ^ catmap "," (sbt bsym_table) sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        *)
        clierrx "[flx_bind/flx_lookup.ml:4493: E194] " sr ( "[lookup,AST_name] ts/vs mismatch binding " ^ string_of_expr e ^ "\nName " ^ name ^
                    " is bound to " ^
                    (full_string_of_entry_kind state.sym_table bsym_table f) )
      end;

      let ts = List.map (tsubst sr spec_vs ts) sub_ts in
      let ts = adjust_ts state.sym_table bsym_table sr index ts in
      let t = ti sr index ts in
      bexpr_closure t (index,ts)


    | FunctionEntry fs ->
      assert (List.length fs > 1);
(*
print_endline ("lookup_name_in_table_dirs_with_sig found functions " ^ name);
*)
      begin match args with
      | [] ->
(*
        print_endline
        (
          "ERROR! [bind_expression] Simple name " ^ name ^
          " binds to function set in\n" ^
          Flx_srcref.short_string_of_src sr ^
          "\nCandidates are\n: " ^ catmap "\n" (full_string_of_entry_kind state.sym_table bsym_table) fs 
        );
*)
        clierrx "[flx_bind/flx_lookup.ml:4520: E195] " sr
        (
          "[bind_expression] Simple name " ^ name ^
          " binds to function set in\n" ^
          Flx_srcref.short_string_of_src sr ^
          "\nCandidates are\n: " ^ catmap "\n" (full_string_of_entry_kind state.sym_table bsym_table) fs 
        )
      | args ->

        let sufs = List.map snd args in
        let ro = resolve_overload state bsym_table env rs sr fs name sufs ts in
        begin match ro with
         | Some (index, dom,ret,mgu,ts) ->
           (*
           print_endline "OK, overload resolved!!";
           *)
           bexpr_closure (ti sr index ts) (index,ts)

         | None -> clierrx "[flx_bind/flx_lookup.ml:4538: E196] " sr "Cannot resolve overload .."
        end
      end
    end

  | EXPR_index (_,name,index) as x ->
    (*
    print_endline ("[bind expression] AST_index " ^ string_of_qualified_name x);
    *)
    let ts = adjust_ts state.sym_table bsym_table sr index [] in
    (*
    print_endline ("ts=" ^ catmap "," (sbt bsym_table) ts);
    *)
    let t = ti sr index ts in
    (*
    print_endline ("Type is " ^ sbt bsym_table t);
    *)
    begin match hfind "lookup" state.sym_table index with
    | { Flx_sym.symdef=SYMDEF_fun _ }
    | { Flx_sym.symdef=SYMDEF_function _ }
    | { Flx_sym.symdef=SYMDEF_struct _ }
    | { Flx_sym.symdef=SYMDEF_cstruct _ } ->

      (*
      print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to closure");
      *)
      bexpr_closure t (index,ts)

(* DEPRECATED *)
    | { Flx_sym.symdef=SYMDEF_nonconst_ctor _ }
      ->
      bexpr_closure t (index,ts)

    | { Flx_sym.symdef=SYMDEF_const  _ }
    | { Flx_sym.symdef=SYMDEF_var _ }
    | { Flx_sym.symdef=SYMDEF_val _ }
    | { Flx_sym.symdef=SYMDEF_parameter _ }
      ->
      (*
      print_endline ("Indexed name: Binding " ^ name ^ "<"^si index^">"^ " to variable");
      *)
(*
print_endline ("LOOKUP 8: varname " ^ si index);
*)
      bexpr_varname t (index,ts)

(* DEPRECATED *)
    | { Flx_sym.symdef=SYMDEF_const_ctor  _ } ->
      bexpr_varname t (index,ts)


    | _ ->
      clierrx "[flx_bind/flx_lookup.ml:4590: E197] " sr ("[Flx_lookup.bind_expression: EXPR_index]: Indexed name: Binding " ^ 
        name ^ "<"^si index^">"^ " requires closure or variable")
      (* 
      bexpr_varname t (index,ts)
      *)
    end

  | (EXPR_lookup (sr,(e,name,ts))) as qn ->
    (*
    print_endline ("Handling qn " ^ string_of_qualified_name qn);
    *)
    let ts = List.map (bt sr) ts in
    let entry =
      match
        eval_module_expr
        state
        bsym_table
        env
        e
      with
      | (Flx_bind_deferred.Simple_module (impl, ts, htab,dirs)) ->
        let env' = mk_bare_env state bsym_table impl in
        let tables = get_pub_tables state bsym_table env' rs dirs in
        let result = lookup_name_in_table_dirs htab tables sr name in
        result

    in
      begin match entry with
      | Some entry ->
        begin match entry with
        | NonFunctionEntry (i) ->
          let i = sye i in
          begin match hfind "lookup" state.sym_table i with
          | { Flx_sym.sr=srn; symdef=SYMDEF_inherit qn} 
            -> 
            be (expr_of_qualified_name qn)

          (* these should have function entries *)
          | { Flx_sym.symdef=SYMDEF_fun _ }
          | { Flx_sym.symdef=SYMDEF_function _ } -> assert false


          | { Flx_sym.sr=srn; symdef=SYMDEF_struct _ } 
          | { Flx_sym.sr=srn; symdef=SYMDEF_cstruct _ } 
            ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
            bexpr_closure (ti sr i ts) (i,ts)

(* DEPRECATED *)
          | { Flx_sym.sr=srn; symdef=SYMDEF_nonconst_ctor  _ } ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
            bexpr_closure (ti sr i ts) (i,ts)

          | { Flx_sym.sr=srn; symdef=SYMDEF_var _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_val _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_parameter _} 
          | { Flx_sym.sr=srn; symdef=SYMDEF_const _} 
            ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
(*
print_endline ("LOOKUP 9: varname " ^ si i);
*)
            bexpr_varname (ti sr i ts) (i,ts)


(* DEPRECATED *)
          | { Flx_sym.sr=srn; symdef=SYMDEF_const_ctor _}  ->
            let ts = adjust_ts state.sym_table bsym_table sr i ts in
(*
print_endline ("LOOKUP 9A: varname " ^ si i);
*)
            bexpr_varname (ti sr i ts) (i,ts)


          | _ ->
            clierrx "[flx_bind/flx_lookup.ml:4665: E198] " sr ("[Flx_lookup.bind_expression: EXPR_lookup] Non function entry "^name^
            " must be const, struct, cstruct, constructor or variable  ")

          end

        | FunctionEntry [f] when args = []  ->
            let sufs = List.map snd args in
            let ro = resolve_overload state bsym_table env rs sr [f] name sufs ts in
            begin match ro with
             | Some (index, dom,ret,mgu,ts) ->
               (*
               print_endline "OK, overload resolved!!";
               *)
               bexpr_closure (ti sr index ts) (index,ts)

            | None ->
              clierrx "[flx_bind/flx_lookup.ml:4681: E199] " sr "Overload resolution failed .. "
            end

        | FunctionEntry fs ->
          begin match args with
          | [] ->
            clierrx "[flx_bind/flx_lookup.ml:4687: E200] " sr
            (
              "[bind_expression] Qualified name " ^
              string_of_expr qn ^
              " binds to function set in" ^
              Flx_srcref.short_string_of_src sr ^
              ", Candidates are: " ^ catmap "," string_of_entry_kind fs
            )

          | args ->
            let sufs = List.map snd args in
            let ro = resolve_overload state bsym_table env rs sr fs name sufs ts in
            begin match ro with
             | Some (index, dom,ret,mgu,ts) ->
               (*
               print_endline "OK, overload resolved!!";
               *)
               bexpr_closure (ti sr index ts) (index,ts)

            | None ->
              clierrx "[flx_bind/flx_lookup.ml:4707: E201] " sr "Overload resolution failed .. "
            end
          end
        end

      | None ->
        clierrx "[flx_bind/flx_lookup.ml:4713: E202] " sr
        (
          "Can't find " ^ name
        )
      end

  | EXPR_suffix (sr,(f,suf)) ->
    let sign = bt sr suf in
    let srn = src_of_qualified_name f in
    lookup_qn_with_sig' state bsym_table sr srn env rs f [sign]

  | EXPR_likely (srr,e) -> bexpr_likely (be e)
  | EXPR_unlikely (srr,e) -> bexpr_unlikely (be e)
  | EXPR_not (sr,e) -> 
    let x = Flx_strr.apl2 sr "lnot" [e]  in
    be x

  | EXPR_ref (_,(EXPR_deref (_,e))) -> be e

  | EXPR_ref (srr,e) ->
      (* Helper function to look up a property in a symbol. *)
      let has_property bid property =
        (* If the bound symbol has the bid, check if that symbol has the
         * property. *)
        match
          try Some (Flx_bsym_table.find bsym_table bid) with Not_found -> None
        with
        | Some bsym ->
            begin match Flx_bsym.bbdcl bsym with
            | BBDCL_fun (properties,_,_,_,_,_) 
            | BBDCL_external_fun (properties,_,_,_,_,_,_) ->
                List.mem property properties
            | _ -> false
            end
        | None ->
            begin match (get_data state.sym_table bid).Flx_sym.symdef with
            | SYMDEF_function (_,_,_,properties,_) -> List.mem property properties
            | SYMDEF_fun (properties,_,_,_,_,_) -> List.mem property properties
            | _ -> false
            end
      in
      let e = be e in
      begin match e with
      | BEXPR_deref e,_ -> e

      | BEXPR_varname (index,ts),_ ->
          (* Look up the type of the name, and make sure it's addressable. *)
          begin match
            try Some (Flx_bsym_table.find bsym_table index)
            with Not_found -> None
          with
          | Some bsym ->
              (* We found a bound symbol, check if it's an addressable symbol.
               * Otherwise, error out. *)
              begin match Flx_bsym.bbdcl bsym with
              | BBDCL_val (_,_,(`Var | `Ref)) ->
                  let vtype = inner_type_of_index_with_ts
                    state
                    bsym_table
                    { rs with depth = rs.depth + 1 }
                    (Flx_bsym.sr bsym)
                    index
                    ts
                  in
                  bexpr_ref (btyp_pointer vtype) (index, ts)

              | BBDCL_val (_,_,(`Val | `Tmp)) ->
                  clierr2 srr (Flx_bsym.sr bsym) ("[bind_expression] " ^
                    "Can't address a value " ^ Flx_bsym.id bsym)

              | _ ->
                  clierr2 srr (Flx_bsym.sr bsym) ("[bind_expression] " ^
                  "[1]Address non variable " ^ Flx_bsym.id bsym)
              end

          | None ->
              (* Otherwise, look up the name in the sym_table. *)
              let sym = get_data state.sym_table index in
              begin match sym.Flx_sym.symdef with
              | SYMDEF_inherit _ ->
                  clierrx "[flx_bind/flx_lookup.ml:4793: E203] " srr "Woops, bindexpr yielded inherit"
              | SYMDEF_inherit_fun _ ->
                  clierrx "[flx_bind/flx_lookup.ml:4795: E204] " srr "Woops, bindexpr yielded inherit fun"
              | SYMDEF_ref _
              | SYMDEF_var _
              | SYMDEF_parameter (`PVar,_) ->
                  let vtype = inner_type_of_index_with_ts
                    state
                    bsym_table
                    { rs with depth = rs.depth + 1 }
                    sym.Flx_sym.sr
                    index
                    ts
                  in
                  bexpr_ref (btyp_pointer vtype) (index, ts)

              | SYMDEF_parameter _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] [2]Address " ^
                    "value parameter " ^ sym.Flx_sym.id)
              | SYMDEF_const _
              | SYMDEF_val _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] " ^
                    "Can't address a value or const " ^ sym.Flx_sym.id)
              | _ ->
                  clierr2 srr sym.Flx_sym.sr ("[bind_expression] [3]Address non " ^
                    "variable " ^ sym.Flx_sym.id)
              end
          end

      | BEXPR_apply ((BEXPR_closure (i,ts),_),a),_ when has_property i `Lvalue ->
          bexpr_address e

      | BEXPR_apply ((BEXPR_closure (i,ts),_),a),_  ->
          let bsym = try Some (Flx_bsym_table.find bsym_table i) with Not_found -> None in
          let bsym = match bsym with | Some bsym -> bsym 
            | None -> failwith ("bind_expression': BEXPR_apply: Cannot find symbol index " ^ si i);
          in
          let name = Flx_bsym.id bsym in
          let sr2 = Flx_bsym.sr bsym in
          clierrx "[flx_bind/flx_lookup.ml:4832: E205] " srr ("[bind_expression] [4]Address application of non-lvalue function " ^
            name ^ " in " ^ sbe bsym_table e ^ 
            "\ndefined here:\n" ^
            Flx_srcref.long_string_of_src sr2
          )

      | _ ->
          clierrx "[flx_bind/flx_lookup.ml:4839: E206] " srr ("[bind_expression] [5]Address non variable " ^
            sbe bsym_table e)
      end

  | EXPR_deref (_,(EXPR_ref (sr,e) as x)) ->
    begin 
      try ignore (be x) 
      with err -> 
      print_endline ("WARNING: binding address of expression " ^ string_of_expr x ^ 
      " gave error: \n" ^ Printexc.to_string err  ^ "\n" ^ Flx_srcref.long_string_of_src sr )
    end;
    be e

  | EXPR_deref (sr,e) ->
    let e,t = be e in
    begin match unfold "flx_lookup" t with
    | BTYP_pointer t' -> bexpr_deref t' (e,t)
    | _ -> clierrx "[flx_bind/flx_lookup.ml:4856: E207] " sr "[bind_expression'] Dereference non pointer"
    end

  (* this is a bit hacky at the moment, try to cheat by using
   * plain old new (T a) where T turns out to be a type
   * down the track we have totally unchecked construction
   * of a T by calling C++ new T (a), no idea if T is a suitable
   * type nor if a is a suitable argument (or argument list) for
   * the constructor. Need to fix this, but first see if we can
   * generate the right code if the detector here finds some cases
   * Ideally, the idea is that this is an optimisation, i.e.
   * T (a) was already valid (via _ctor_T probably) and then
   * new T(a) is like the ordinary copy of a value, except theres
   * no copy, constuction is in place on the heap.
   *
   * How to resolve the ambiguity? One way would be to require "cls"
   * to be a cstruct .. however google re2::RE2_ is a primitive, not
   * cstruct, and RE2 is currently also a primitive with a primitive ctor.
   *)
(* Temporarily disable this so we can distinguish existing constuctors
   and calls to C++ class constructors

  | EXPR_new (srr,(EXPR_apply(sre,(cls,a)) as e)) ->
    begin try
      let cls = bt sre (typecode_of_expr cls) in
print_endline ("CLASS NEW " ^sbt bsym_table cls);
      bexpr_class_new cls (be a)
    with _ ->
    bexpr_new (be e)
    end
*)

  | EXPR_new (srr,e) ->
    bexpr_new (be e)

  | EXPR_literal (sr,v) ->
    let t = type_of_literal state bsym_table env sr v in
    bexpr_literal t v

  | EXPR_map (sr,f,a) ->
    handle_map sr (be f) (be a)

  (* generic str routine *)
  | EXPR_apply (sr,(EXPR_name (_,"_strr",[]), a)) -> 
    let be rs e = bind_expression' state bsym_table env rs e [] in
    Flx_strr.strr bsym_table state.sym_table state.counter be rs sr a


  (* generic equality routine *)
  | EXPR_apply (sr,(EXPR_name (_,"_eq",[]), pair)) -> 
    let be rs e = bind_expression' state bsym_table env rs e [] in
    Flx_eq.bind_eq bsym_table state inner_lookup_name_in_env be rs sr env pair

  | EXPR_apply 
    (
      sr,
      (
        EXPR_apply (_,(EXPR_name (_,"_map",[]), EXPR_name (_,func,[]))),
        b
      )
    ) ->
    let be rs e = bind_expression' state bsym_table env rs e [] in
    Flx_gmap.generic_map bsym_table state.counter be rs sr env func b

  | EXPR_apply (sr,(f',a')) -> 
(*
print_endline ("Bind_expression apply " ^ string_of_expr e);
*)
    Flx_bind_apply.cal_bind_apply 
      bsym_table state be bt env build_env
      koenig_lookup cal_apply bind_type' 
      lookup_qn_with_sig' mkenv
      inner_lookup_name_in_env eval_module_expr
      mk_bare_env get_pub_tables lookup_name_in_table_dirs
      bind_expression'
      rs sr f' a' args

  | EXPR_arrayof (sr,es) ->
    let bets = List.map be es in
    let _, bts = List.split bets in
    let n = List.length bets in
    if n > 1 then begin
      let t = List.hd bts in
      List.iter
      (fun t' -> if t <> t' then
         clierrx "[flx_bind/flx_lookup.ml:4941: E208] " sr
         (
           "Elements of this array must all be of type:\n" ^
           sbt bsym_table t ^ "\ngot:\n"^ sbt bsym_table t'
         )
      )
      (List.tl bts)
      ;
      let t = btyp_array (t, btyp_unitsum n) in
      bexpr_tuple t bets
    end else if n = 1 then List.hd bets
    else syserr sr "Empty array?"

  (* the code for this is pretty messy and inefficient but it should work *)
  (* actually no, it only works at binding time! we need tuple_cons, which
     should work at instantiation time!
  *)
  | EXPR_extension (sr, es, e') ->  
    let e'',t' = be e' in
    let es' = List.map be es in
    let ts = List.map snd es' in
    begin match t' with
    | BTYP_record (fields) ->
      let new_fields = ref [] in
      List.iter (fun e ->
        let _,t = be e in
        match t with
        | BTYP_record (fields) -> 
          let fields = List.map (fun (s,t)-> 
            s,EXPR_get_named_variable (sr,(s,e))
          )
          fields
          in
          new_fields := List.rev fields @ (!new_fields)
        | _ -> clierrx "[flx_bind/flx_lookup.ml:4975: E209] " sr ("Record extension requires bases be records too, got value of type " ^ sbt bsym_table t)
      )
      es
      ;
      let fields = List.map (fun (s,_)-> s,EXPR_get_named_variable (sr, (s,e'))) fields in
      new_fields := fields @ !new_fields;
      let unique_fields = ref [] in
      List.iter (fun (s,t) ->
        if not (List.mem_assoc s (!unique_fields)) then
        unique_fields := (s,t) :: (!unique_fields)
      )
      (!new_fields);
      be (EXPR_record (sr,!unique_fields))

    | _ -> 
      let ntimes t n = 
        let rec aux n ts = if n=0 then ts else aux (n-1) (t::ts) in
        aux n []
      in 
      let rec check t n ts = 
        match ts with
        | [] -> Some (t,n)
        | BTYP_array (t',BTYP_unitsum m)::ts when t = t' -> check t (n+m) ts
        | t'::ts when t = t'  -> check t (n+1) ts
        | _ -> None
      in
      let compatible_arrays ts = 
        match ts with 
        | [] -> clierrx "[flx_bind/flx_lookup.ml:5003: E210] " sr "empty extension"
        | BTYP_array (t,BTYP_unitsum n) :: ts -> check t n ts
        | t::ts -> check t 1 ts
      in
      match compatible_arrays (ts @ [t']) with
      | Some  _ ->
        clierrx "[flx_bind/flx_lookup.ml:5009: E211] " sr "Can't extend arrays yet"
      | None ->
        (* if it isn't a record extension, treat it as a tuple extension *)
        let values = ref [] in
        let types = ref [] in
        List.iter (fun  xpr -> 
          match xpr with
          | BEXPR_tuple flds,BTYP_tuple ts -> 
            values := !values @ flds; 
            types := !types @ ts
          | e,BTYP_tuple ts -> 
            let k = List.length ts in
            let n = ref (-1) in
            values := !values @ List.map (fun w -> incr n;
(*
                print_endline ("8:get_n arg" ^ sbe bsym_table xpr);         
*)
                bexpr_get_n w (!n) xpr) ts; 
            types := !types @ ts
          | BEXPR_tuple flds, BTYP_array (t, BTYP_unitsum n) ->
            values := !values @ flds; 
            types := !types @ ntimes t n;
          | e, BTYP_array (t, BTYP_unitsum n) ->
            if n > 20 then clierrx "[flx_bind/flx_lookup.ml:5032: E212] " sr "Array too big (>20) for tuple extension"
            else (
              let k = ref (-1) in
              values := !values @ List.map (fun w -> incr k; 
(*
                print_endline ("9:get_n arg" ^ sbe bsym_table xpr);         
*)
                bexpr_get_n w (!k) xpr) (ntimes t n); 
              types := !types @ ntimes t n;
            )
          | _ -> 
            values := !values @ [xpr];
            types := !types @ [snd xpr]
        )
        (es' @[e'',t'])
        ;
        let tt = btyp_tuple (!types) in
        let ee = bexpr_tuple tt (!values) in
        ee
    end

  | EXPR_record_type _ -> assert false
  | EXPR_polyrecord_type _ -> assert false
  | EXPR_variant_type _ -> assert false

  | EXPR_record (sr,ls) ->
    begin match ls with
    | [] -> bexpr_tuple (btyp_tuple []) []
    | _ ->
    let ss,es = List.split ls in
    let es = List.map be es in
    bexpr_record (List.combine ss es)
    end

  | EXPR_rnprj (sr,name,seq,e) -> 
    let (e',domain) as e = be e in
    begin match domain with
    | BTYP_record flds ->
      let dcnt = ref 0 in
      let idx = ref 0 in
      begin try
        List.iter (fun (s,t) -> 
          if s <> name then incr idx else
          if (!dcnt) = seq then raise Not_found 
          else begin incr idx; incr dcnt end
        ) 
        flds;
        print_endline ("[Flx_lookup:bind_expression] Invalid named projection " ^ name ^ ", seq=" ^ string_of_int seq);
        assert false
      with Not_found ->
        let codomain = snd (List.nth flds (!idx)) in
        bexpr_apply codomain (bexpr_prj (!idx) domain codomain,e)
      end

    | BTYP_polyrecord (flds,x) ->
      failwith "rnprj not implemented for polyrecord type argument yet"

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5089: E213] " sr ("Argument of rnprj must be record or polyrecord type, got " ^ sbt bsym_table domain)
    end
 
  | EXPR_polyrecord (sr,ls,e) ->
    let ss,es = List.split ls in
    let es = List.map be es in
    bexpr_polyrecord (List.combine ss es) (be e)

  | EXPR_remove_fields (sr,e,ss) ->
    bexpr_remove_fields (be e) ss

  | EXPR_replace_fields (sr, e, fs) ->
    let fs = List.map (fun (s,e) -> s,be e) fs in
    let cmp (s1,e1) (s2,e2) = compare s1 s2 in
    let fs = List.stable_sort cmp fs in
    let (_,t) as e = be e in
    let check ls = 
      let rec aux ls fs = match ls,fs with
      | _,[] -> () (* exhausted replacement list *)
      | (s1,t1)::tail1,(s2,e2)::tail2 when s1 = s2 -> 
        if type_eq bsym_table state.counter t1 (snd e2) 
        then aux tail1 tail2
        else 
         clierr sr ("Flx_lookup: Attempt to replace field " ^ s1 ^
         " of type " ^ sbt bsym_table t1 ^
         " with field of wrong type " ^  sbt bsym_table (snd e2)
         )

      | _::tail1,fs -> aux tail1 fs

      | _,(s2,_)::_ -> clierr sr ("Flx_lookup: Attempt to replace field " ^ s2 ^
        " which is not present in record of type " ^
        sbt bsym_table t)
      in aux ls fs
    in
    begin match t with
    | BTYP_record ls -> check ls
    | BTYP_polyrecord (ls,_) -> check ls
    | _ -> clierr sr ("flx_lookup: replace fields in non record type " ^ sbt bsym_table t)
    end;
    bexpr_polyrecord fs (bexpr_remove_fields e (List.map fst fs))

  | EXPR_tuple (_,es) ->
    let bets = List.map be es in
    let _, bts = List.split bets in
    let n = List.length bets in
    if n > 1 then
      try
        let t = List.hd bts in
        List.iter
        (fun t' -> if t <> t' then raise Not_found)
        (List.tl bts)
        ;
        let t = btyp_array (t, btyp_unitsum n) in
        bexpr_tuple t bets
      with Not_found ->
        bexpr_tuple (btyp_tuple bts) bets
    else if n = 1 then
      List.hd bets
    else
    bexpr_tuple (btyp_tuple []) []


  | EXPR_match_case (sr,(v,e)) ->
     bexpr_match_case (v,be e)

  | EXPR_match_variant(sr,(v,e)) ->
     bexpr_match_variant (v,be e)

  | EXPR_match_ctor (sr,(qn,e)) ->
    begin match qn with
    | `AST_name (sr,name,ts) ->
      (*
      print_endline ("WARNING(deprecate): match constructor by name! " ^ name);
      *)
      let (_,ut) as ue = be e in
      let ut = rt ut in
      (*
      print_endline ("Union type is " ^ sbt bsym_table ut);
      *)
      begin match ut with
      | BTYP_inst (i,ts') ->
        (*
        print_endline ("OK got type " ^ si i);
        *)
        begin match hfind "lookup" state.sym_table i with
        | { Flx_sym.id=id; symdef=SYMDEF_union ls } ->
          (*
          print_endline ("UNION TYPE! " ^ id);
          *)
          let vidx =
            let rec scan = function
            | [] -> None
            | (vn,vidx,vs',vat)::_ when vn = name -> Some vidx
            | _:: t -> scan t
            in scan ls
          in
          begin match vidx with
          | Some vidx ->
            (*
            print_endline ("Index is " ^ si vidx);
            *)
            bexpr_match_case (vidx,ue)

          | None->
            begin try
              let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
              be (EXPR_apply ( sr, (fname,e)))
            with _ -> 
              clierrx "[flx_bind/flx_lookup.ml:5168: E214] " sr ("[flx_lookup: EXPR_match_ctor]: Can't find union variant " ^ name ^ 
                 " or bind user function _match_ctor_" ^ name ^ " to arg " ^ 
                 string_of_expr e)
            end
          end

        (* this handles the case of a C type we want to model
        as a union by provding _match_ctor_name style function
        as C primitives ..
        *)
        | { Flx_sym.id=id; symdef=SYMDEF_abs _ } ->
          let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* experimental!! Allow for any nominal type other than union *)
        | _ ->
          let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* | _ ->  clierrx "[flx_bind/flx_lookup.ml:5187: E215] " sr ("expected union of abstract type, got" ^ sbt bsym_table ut) *)
        end

      (* experimental!! Allow for any type other than union *)
      | _ -> 
        let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
        be (EXPR_apply ( sr, (fname,e)))

 
      (* | _ -> clierrx "[flx_bind/flx_lookup.ml:5196: E216] " sr ("expected nominal type, got" ^ sbt bsym_table ut) *)
      end

    | `AST_typed_case (sr,v,_)
    | `AST_case_tag (sr,v) ->
       be (EXPR_match_case (sr,(v,e)))

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5203: E217] " sr "Expected variant constructor name in union decoder"
    end

  | EXPR_match_ho_ctor (sr,(qn,es)) ->
(*
print_endline ("match ho ctor : exprs = " ^ catmap "," string_of_expr es);
*)
    begin match qn with
    | `AST_name (sr,name,ts) ->
      let fname = EXPR_name (sr,"_match_ctor_" ^ name,ts) in
      begin match es with
      | [] -> assert false (* shouldn't allow less then 2 arguments! *)
      | ls ->  
        let e = List.fold_left (fun acc e -> EXPR_apply (sr, (acc,e))) fname es in
(*
print_endline ("match ho ctor, binding expr = " ^ string_of_expr e);
*)
        be e
      end
    | _ -> clierrx "[flx_bind/flx_lookup.ml:5203: E217] " sr "Expected variant constructor name in union decoder"
    end

  | EXPR_variant_arg (sr,(v,e)) ->
     let (_,t) as e' = be e in
     ignore (try unfold "flx_lookup" t with _ -> failwith "AST_variant_arg unfold screwd");
     begin match unfold "flx_lookup" t with
     | BTYP_variant ls ->
       begin 
         try 
           List.iter (fun (cname,t) ->
             if cname = v then raise (Tfound t))
             ls
           ; 
           clierrx "[flx_bind/flx_lookup.ml:5217: E218] " sr ("[bind_expression] [Expr_variant_arg] " ^" Variant case " ^ v ^ 
              " not found in variant type: " ^ sbt bsym_table t)
         with Tfound t -> 
           bexpr_variant_arg t (v,e')
       end
     | _ -> clierrx "[flx_bind/flx_lookup.ml:5222: E219] " sr ("Expected variant type, got " ^ sbt bsym_table t)
     end


  | EXPR_case_arg (sr,(v,e)) ->
     let (_,t) as e' = be e in
     ignore (try unfold "flx_lookup" t with _ -> failwith "AST_case_arg unfold screwd");
     begin match unfold "flx_lookup" t with
     | BTYP_unitsum n ->
       if v < 0 || v >= n
       then clierrx "[flx_bind/flx_lookup.ml:5232: E220] " sr "Invalid sum index"
       else
         bexpr_case_arg unit_t (v, e')

     | BTYP_sum ls ->
       let n = List.length ls in
       if v<0 || v>=n
       then clierrx "[flx_bind/flx_lookup.ml:5239: E221] " sr "Invalid sum index"
       else let t = List.nth ls v in
       bexpr_case_arg t (v, e')

     | _ -> clierrx "[flx_bind/flx_lookup.ml:5243: E222] " sr ("Expected sum type, got " ^ sbt bsym_table t)
     end

  | EXPR_ctor_arg (sr,(qn,e)) ->

    let (_,ut) as ue = be e in
    let ut = rt ut in

    begin match qn with
    | `AST_name (sr,name,ts) ->
(*      print_endline ("Constructor to extract: " ^ name ^ "[" ^ catmap "," string_of_typecode ts ^ "]"); *)
      begin match ut with
      | BTYP_inst (i,ts') ->
        begin match hfind "lookup" state.sym_table i with
        | { Flx_sym.id=id; symdef=SYMDEF_union ls } ->
          let _,vs,_  = find_split_vs state.sym_table bsym_table i in
(*
        print_endline (
            "OK got union type " ^ id ^ "<"^si i ^ "> vs= " ^ 
            catmap "," (fun (id,j,_)-> id^"<"^si j^">") (fst vs) 
             ^ "instance ts = [" ^catmap "," (sbt bsym_table) ts'^ "]"
        );
*)
(* print_endline ("Constructor to extract " ^ name ^ " should agree with encoded constuctor " ^ id); *)
(* print_endline ("Union parent vs = " ^ catmap "," (fun (s,_,_) -> s) parent_vs ^ " local vs = " ^ catmap "," (fun (s,_,_) -> si i) vs''); *)

          let result =
            let rec scan = function
            | [] -> None
            | (vn,vidx,vs',vt)::_ when vn = name -> Some (vidx,vs',vt)
            | _:: t -> scan t
            in scan ls
          in
          begin match result with
          | None ->
            begin try
              let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
              be (EXPR_apply ( sr, (fname,e)))
            with _ ->
              clierrx "[flx_bind/flx_lookup.ml:5282: E223] " sr ("[flx_lookup: EXPR_ctor_arg]: Can't find union variant " ^ name ^ 
                 " or bind user function _ctor_arg_" ^ name ^ " to arg " ^ 
                 string_of_expr e)
            end
(* failwith ("EXPR_ctor_arg: Can't find union variant " ^ name); *)

          | Some ( vidx,vs', vt) ->

(*          print_endline ("Constructor Index is " ^ si vidx ^ " vs'=" ^ catmap "," fst (fst vs')); *)
            let vt =
              let bvs = List.map
                (fun (n,i,_) -> n, btyp_type_var (i, btyp_type 0))
                vs
              in
(*            print_endline ("Binding ctor arg type = " ^ string_of_typecode vt); *)
              let env' = build_env state bsym_table (Some i) in
              bind_type' state bsym_table env' rsground sr vt bvs mkenv
            in
(*          print_endline ("Bound polymorphic arg type = " ^ sbt bsym_table vt); *)
            let vs' = List.map (fun (s,i,tp) -> s,i) vs in
(*          print_endline ("vs in union type = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") vs'); *)
(*          print_endline ("ts' to bind to them = " ^ catmap "," (sbt bsym_table) ts'); *)
(*
          let ts' = adjust_ts state.sym_table bsym_table sr i ts' in
          print_endline ("ts' to bind to them after adjust = " ^ catmap "," (sbt bsym_table) ts');
*)
            let vt = tsubst sr vs' ts' vt in
(*          print_endline ("Instantiated type = " ^ sbt bsym_table vt); *)
            bexpr_case_arg vt (vidx,ue)
          end
        (* this handles the case of a C type we want to model
        as a union by provding _ctor_arg style function
        as C primitives ..
        *)
        | { Flx_sym.id=id; symdef=SYMDEF_abs _ } ->
          let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* experimental allow for any nominal type other than union *)
        | _ ->
          let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
          be (EXPR_apply ( sr, (fname,e)))

        (* | _ -> failwith "Woooops expected union or abstract type" *)
        end
      (* experimental allow for any type other than union *)
      | _ ->
        let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
        be (EXPR_apply ( sr, (fname,e)))

      (* | _ -> failwith "Woops, expected nominal type" *)
      end


    | `AST_typed_case (sr,v,_)
    | `AST_case_tag (sr,v) ->
      be (EXPR_case_arg (sr,(v,e)))

    | _ -> clierrx "[flx_bind/flx_lookup.ml:5340: E224] " sr "Expected variant constructor name in union dtor"
    end

  | EXPR_ho_ctor_arg (sr,(qn,es)) ->
(*
print_endline ("ho ctor arg: exprs = " ^ catmap "," string_of_expr es);
*)
    begin match qn with
    | `AST_name (sr,name,ts) ->
      let fname = EXPR_name (sr,"_ctor_arg_" ^ name,ts) in
      begin match es with
      | [] -> assert false (* shouldn't allow less then 2 arguments! *)
      | ls ->  
        let e = List.fold_left (fun acc e -> EXPR_apply (sr, (acc,e))) fname es in
(*
print_endline ("ho ctor arg: expr = " ^ string_of_expr e);
*)
        be e
      end

    | _ -> clierr sr "Expected variant constructor name in union dtor"
    end

  | EXPR_lambda (sr,_) ->
    syserr sr
    (
      "[bind_expression] " ^
      "Unexpected lambda or object when binding expression (should have been lifted out)" ^
      string_of_expr e
    )

  | EXPR_match (sr,_) ->
    clierrx "[flx_bind/flx_lookup.ml:5352: E225] " sr
    (
      "[bind_expression] " ^
      "Unexpected match when binding expression (should have been lifted out)"
    )

and hack_name qn = match qn with
| `AST_name (sr,name,ts) -> `AST_name (sr,"_inst_"^name,ts)
| `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,"_inst_"^name,ts))
| _ -> failwith "expected qn .."

and grab_ts qn = match qn with
| `AST_name (sr,name,ts) -> ts
| `AST_lookup (sr,(e,name,ts)) -> ts
| _ -> failwith "expected qn .."

and grab_name qn = match qn with
| `AST_name (sr,name,ts) -> name
| `AST_lookup (sr,(e,name,ts)) -> name
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
          | BTYP_void -> false
          | _ ->
             (*
              print_endline (
               "[instance_check] Can't reduce instance type constraint " ^
               sbt bsym_table cons
             );
             *)
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


and clone state bsym_table fi ft fbt fe fx new_vs generic_alias index =
      begin 
      let parent,sym = Flx_sym_table.find_with_parent state.sym_table index in
      let {Flx_sym.id=id;sr=sr;vs=vs;pubmap=pubmap; privmap=privmap;dirs=dirs;symdef=symdef} = sym in
(*
      print_endline ("CLONING SYMBOL " ^ id ^"<"^si index^">");
*)
      let nupubmap = Flx_btype.map_name_map fi fbt pubmap in 
      let nuprivmap = Flx_btype.map_name_map fi fbt privmap in
      Hashtbl.iter (fun key value -> Hashtbl.replace nuprivmap key value)
      generic_alias;
(*
print_endline ("New public name map = " ^ string_of_name_map nupubmap);
print_endline ("New private name map = " ^ string_of_name_map nuprivmap);
*)
      let nudirs = dirs in
      let vs_list, {Flx_ast.raw_type_constraint=traint; raw_typeclass_reqs=tcreqs} = vs in

      let nusymdef = 
        match symdef with
        | SYMDEF_function (params,rett,effects,props,sexes) -> 
          let paramlist, ptraint = params in
          let nuparamlist = List.map (fun (pkind,pname,ptyp,pinitopt) -> 
            pkind,pname,ft ptyp, pinitopt (* HACK *)) 
            paramlist
          in
          let nuptraint = match ptraint with | None -> None | Some e -> Some (fe e) in
          let nuparams = nuparamlist, nuptraint in
(*
      print_endline ("Remapped parameters: " ^ string_of_parameters nuparams);
*)
          let nurett = ft rett in
          let nueffects = ft effects in
          let nuprops = props in (* HACK, FIXME! *)
          let nusexes = List.map (fun (sr,x) -> sr,fx x) sexes in
(*
      print_endline ("New exes =\n");
      List.iter (fun (sr,x) -> print_endline (Flx_print.string_of_exe 2 x)) nusexes;
*)
          SYMDEF_function (nuparams, nurett, nueffects,nuprops, nusexes)
        | SYMDEF_insert (cs,ik,rqs) -> symdef (* HACK, FIXME! *)
        | SYMDEF_var t -> 
          let t = ft t in
(*
          print_endline ("Cloned VAR type = " ^ string_of_typecode t);
*)
          SYMDEF_var (t)
        | SYMDEF_val t -> 
          let t = ft t in
(*
          print_endline ("Cloned VAL type = " ^ string_of_typecode t);
*)
          SYMDEF_val (t)
        | SYMDEF_ref t -> 
          let t = ft t in
(*
          print_endline ("Cloned REF type = " ^ string_of_typecode t); 
*)
          SYMDEF_ref (t)
        | SYMDEF_parameter (k,t) -> 
          let t = ft t in
(*
          print_endline ("Cloned PARAMETER type = " ^ string_of_typecode t); 
*)
          SYMDEF_parameter (k,t)

        | SYMDEF_label label as x -> x

        | _ -> 
          print_endline ("[Flx_lookup:clone] Unhandled symdef");
          print_endline (string_of_symdef symdef id vs);
          print_endline ("Parent is " ^ match parent with | None -> "NONE" | Some i -> si i);
          assert false
      in
      let nuvs_vars = List.map (fun (s,i,t) -> s,fi i, ft t) new_vs in 
      let nutraint = ft traint in
      let nutcreqs = tcreqs in (* just a qualified name list *)
      let nuvs_aux = {Flx_ast.raw_type_constraint=nutraint; raw_typeclass_reqs=nutcreqs} in
      let nuvs = nuvs_vars,nuvs_aux in
      let nuparent = match parent with None -> None | Some i -> Some (fi i) in
      let nusym = {Flx_sym.id=id;sr=sr;vs=nuvs;pubmap=nupubmap; privmap=nuprivmap;dirs=nudirs;symdef=nusymdef} in
      let nuindex = fi index in
      Flx_sym_table.add state.sym_table nuindex nuparent nusym;
(*
print_endline ("++++ Cloned " ^ si index ^ " -> " ^ si nuindex);
*)
      end


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
  if List.length fs = 0 then None else
  let env i =
    (*
    print_endline ("resolve_overload: Building env for " ^ name ^ "<" ^ si i ^ ">");
    *)
    inner_build_env state bsym_table rs (Some i)
  in
  let bt rs sr i t =
    inner_bind_type state bsym_table (env i) sr rs t
  in
  let be i e =
    inner_bind_expression state bsym_table (env i) rs e
  in
  let luqn2 i qn = lookup_qn_in_env2' state bsym_table (env i) rs qn in
  let fs = trclose state bsym_table rs sr fs in
  let result : overload_result option =
    overload state.counter state.sym_table bsym_table caller_env rs bt be luqn2 sr fs name sufs ts
  in
  begin match result with
  | None -> None 
  | Some (index,sign,ret,mgu,ts) ->
(*
if name = "accumulate" then begin
    print_endline ("RESOLVED OVERLOAD OF " ^ name);
    print_endline (" .. mgu = " ^ string_of_varlist bsym_table mgu);
    print_endline ("Resolve ts = " ^ catmap "," (sbt bsym_table) ts);
end;
*)
    let parent_vs,vs,{raw_typeclass_reqs=rtcr} = find_split_vs state.sym_table bsym_table index in
(*
if name = "accumulate" then begin
    print_endline ("Function vs=" ^ catmap "," svs vs);
    print_endline ("Parent vs=" ^ catmap "," svs parent_vs);
end;
*)
    let is_generic vs = List.fold_left (fun acc (s,i,mt) -> 
      acc || match mt with | TYP_generic _ -> true | _ -> false) 
      false 
      vs 
    in
    assert (not (is_generic parent_vs));
    if not (is_generic vs) then result else let () = () in

(*
    print_endline ("Found generic function " ^ name);
    print_endline ("REBINDING");
*)
    let new_vs = ref [] in
    let gen_vs = ref [] in
    let counter = ref 0 in
    let vmap = ref [] in
    let smap = ref [] in
    let parent_vs_len = List.length parent_vs in
    let parentts, funts = Flx_list.list_split ts parent_vs_len in
    let nufunts = ref [] in
    List.iter (fun (s,i,mt as v) -> 
      let n = !counter in
      begin match mt with 
      | TYP_generic _ -> 
        let bt = List.nth ts n in
        let ubt = Flx_typecode_of_btype.typecode_of_btype bsym_table state.counter sr bt in
        gen_vs := (v,n) :: !gen_vs;
        smap := (s, ubt) :: !smap;
        vmap := (i, bt) :: !vmap
      | _ -> 
        new_vs := v :: !new_vs;
        nufunts := (List.nth funts n) ::!nufunts
      end;
      incr counter
      )
      vs
    ;
    let new_vs = List.rev (!new_vs) in
    let gen_vs = List.rev (!gen_vs) in

    (* smap lis list that says replace type named s with unbound type expression *) 
    let smap = List.rev (!smap) in

    (* vmap is list that says replace type variable index i with given bound type *)
    let vmap = List.rev (!vmap) in
    let nufunts = List.rev (!nufunts) in
(*
    print_endline ("Polymorphic vs = " ^ catmap "," svs new_vs);
    print_endline ("Generic vs = " ^ catmap "," (fun (v,n) -> "POS " ^ si n ^" " ^ 
      svs v^ " --> " ^ sbt bsym_table (List.nth ts n)
      ) gen_vs
    );
    List.iter (fun (ix,bt) -> print_endline ("Rebind " ^ si ix ^ " -> " ^ sbt bsym_table bt)) vmap;
    List.iter (fun (s,ubt) -> print_endline ("Rebind " ^ s ^ " -> " ^ string_of_typecode ubt)) smap;
*)
    let previous_rebinding =
       try Some (Hashtbl.find state.generic_cache (index, vmap))
       with Not_found -> None
    in
    match previous_rebinding with
    | Some result -> Some result (* weird but correct *)
    | None ->
    (* prevent the generator running twice *)
    (* we have to actually calculate a new rebinding right now, the reason is
      we need the return type!
    *)
(*
    print_endline "*** Calculating new rebinding";
*)
    let sym = Flx_sym_table.find state.sym_table index in
    let fresh () = Flx_mtypes2.fresh_bid state.counter in
    let remap_table = Hashtbl.create 97 in 
    let add i = let j = fresh() in (Hashtbl.add remap_table i j);j in
    let fi i = try Hashtbl.find remap_table i with Not_found -> i in
    let nuindex = add index in

    let descendants = Flx_sym_table.find_descendants state.sym_table index in
(*
    print_endline ("Descendants = " ^ catmap "," si descendants);
*)
    (* calculate rebinding map for descendants *)
    List.iter (fun i -> let _ = add i in ()) descendants;
(*
    Hashtbl.iter (fun i j -> print_endline ("  Rebind index " ^ si i ^ " -> " ^ si j)) remap_table;
*)
    let rec fbt t = match t with
      | Flx_btype.BTYP_type_var (i,mt) ->
(*
print_endline ("Examining bound type variable index " ^ si i);
*)
        begin
          try 
            let r = List.assoc i vmap in
(*
print_endline ("Replaced by vmap: " ^ sbt bsym_table r);
*)
            r
          with Not_found ->  
            let j = fi i in
(*
print_endline ("Not found in vmap, remaping with index remapper: " ^ si j);
*)
            Flx_btype.btyp_type_var (j, mt)
        end
      | x -> Flx_btype.map ~f_bid:fi ~f_btype:fbt x
    in
    let rec fe e = Flx_maps.full_map_expr fi ft fe e
    and ft t = match t with 
    | TYP_typeof e ->
      let e' = fe e in
(*
print_endline ("Unbound type remapper typeof " ^ string_of_expr e ^ " --> " ^ string_of_expr e');
*)
      TYP_typeof e'

    | TYP_var index -> 
      let j = fi index in
(*
print_endline ("Ubound type remapper type variable " ^ si index ^ " --> " ^ si j);
*)
      TYP_var j  
    | TYP_name (sr,tname,[]) as t -> 
(*
print_endline ("Trying to bind type name=" ^ tname);
*)
      begin
        try let r = List.assoc tname smap in
(*
           print_endline ("Rebound type name " ^ tname ^ " -> " ^ string_of_typecode r);
*)
           r 
        with Not_found -> t
      end
    | x -> Flx_maps.map_type ft x
    in 
    let fx x = Flx_maps.map_exe fi ft fe x in
 
    (* this code is ALMOST but not quite fully general.
       The problem is that the main function has its vs list and ts list
       reduced by the generic substitution, wherease the descendants do not
     *)
    assert (nuindex = fi index);
    let noalias = Hashtbl.create 97 in
    let generic_alias = Hashtbl.create 97 in
    List.iter (fun (s, t) ->
      let alias_index = fresh() in
(*
print_endline ("Adding alias " ^ s ^ "<"^si alias_index^"> -> " ^ string_of_typecode t);
*)
      let entry = NonFunctionEntry {Flx_btype.base_sym=alias_index; spec_vs=[]; sub_ts=[]} in
      Hashtbl.add generic_alias s entry;
      let symdef = SYMDEF_type_alias t in
      let sym = {Flx_sym.id=s;sr=sr;vs=dfltvs;pubmap=noalias; privmap=noalias;dirs=[];symdef=symdef} in
      Flx_sym_table.add state.sym_table alias_index (Some nuindex) sym;
    )
    smap; 
    clone state bsym_table fi ft fbt fe fx new_vs generic_alias index;
(*
    print_endline ("Main symbol cloned and added");
*)
    (* HACK, FIXME! This will be the vs of the new symbol, for the master
       we have to replace the vs with one generics are stripped out of,
       but for kids, the vs is whatever they have. Use an option type
       instead to indicate an override or native calc.
    *)
    let hack_vs = [] in 
    List.iter (fun index -> 
      clone state bsym_table fi ft fbt fe fx hack_vs noalias index) 
    descendants;

(* end of cloning code, the rest is for recalculating the MGU *)
    let nuts = parentts @ nufunts in
(*
print_endline ("New  full ts = " ^ catmap "," (sbt bsym_table) nuts);
*)
    let numgu = mgu in (* HACK, FIXME! *)
(*
print_endline ("New mgu = " ^ catmap "," (fun (v,t) -> string_of_int v ^ "<-" ^ sbt bsym_table t) mgu);
*)
    let nusign = fbt sign in 
(*
print_endline ("New signature = " ^ sbt bsym_table nusign);
*)
    let nuret = fbt ret in (* HACK, FIXME! *)
(*
print_endline ("New return type = " ^ sbt bsym_table nuret);
*)
    let result_data = nuindex,nusign,nuret,numgu,nuts in
    Hashtbl.add state.generic_cache (index, vmap) result_data;
    Some result_data
  end

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

and split_dirs open_excludes dirs :
    (ivs_list_t * qualified_name_t) list *
    (ivs_list_t * qualified_name_t) list *
    (string * qualified_name_t) list
=
  let opens =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | DIR_open (vs,qn) -> if List.mem (vs,qn) open_excludes then [] else [vs,qn]
         | DIR_inject_module _  -> []
         | DIR_use _ -> []
       )
       dirs
     )
  and includes =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | DIR_open _-> []
         | DIR_inject_module (vs,qn) -> if List.mem (vs,qn) open_excludes then [] else [vs,qn]
         | DIR_use _ -> []
       )
       dirs
     )
  and uses =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | DIR_open _-> []
         | DIR_inject_module _ -> []
         | DIR_use (n,qn) -> [n,qn]
       )
       dirs
     )
  in opens, includes, uses

(* calculate the transitive closure of an i,ts list
  with respect to inherit clauses.

  The result is an i,ts list.

  This is BUGGED because it ignores typeclass requirements ..
  however
  (a) modules can't have them (use inherit clause)
  (b) typeclasses don't use them (use inherit clause)
  (c) the routine is only called for modules and typeclasses?
*)

and get_includes state bsym_table rs xs =
  let rec get_includes' includes (((invs: ivs_list_t),i, ts) as index) =
    if not (List.mem index !includes) then
    begin
      (*
      if List.length ts != 0 then
        print_endline ("INCLUDES, ts="^catmap "," (sbt bsym_table) ts)
      ;
      *)
      includes := index :: !includes;
      let env = mk_bare_env state bsym_table i in (* should have ts in .. *)
      let qns,sr,vs =
        match hfind "lookup" state.sym_table i with
        { Flx_sym.id=id; sr=sr; vs=vs; dirs=dirs } ->
(*
print_endline "---------------------";
print_endline ("Includes, id = " ^ id);
*)
(*
        if List.length (fst invs) > 0 then begin
        print_endline (id ^", Ints=" ^ catmap "," (sbt bsym_table) ts);
        print_endline (id ^", Raw vs = " ^ catmap "," (fun (n,k,_) -> n ^ "<" ^ si k ^ ">") (fst vs));
        print_endline (id ^", In vs = " ^ catmap "," (fun (n,k,_) -> n ^ "<" ^ si k ^ ">") (fst invs));
        end;
*)
        let _,incl_qns,_ = split_dirs [] dirs in
        let vs = List.map (fun (n,i,_) -> n,i) (fst vs) in
        incl_qns,sr,vs
      in
      List.iter (fun (invs2,qn) ->
          let rs2 = {rs with open_excludes = (invs,qn)::rs.open_excludes } in
          let {base_sym=j; spec_vs=vs'; sub_ts=ts'},ts'' =
            try lookup_qn_in_env' state bsym_table env rsground qn
            with Not_found -> failwith "QN NOT FOUND"
          in
            let mkenv i = mk_bare_env state bsym_table i in
            let args = List.map (fun (n,k,_) -> n,btyp_type_var (k,btyp_type 0)) (fst invs2) in
            let bt t = bind_type' state bsym_table env rs sr t args mkenv in
            let ts'' = List.map bt ts'' in
            let ts'' = List.map (tsubst sr vs ts) ts'' in
            let ts' = List.map (tsubst sr vs' ts'') ts' in
let i1 : plain_ivs_list_t  = fst invs in
let i2 : plain_ivs_list_t = fst invs2 in
let i3 : plain_ivs_list_t  = i1 @ i2 in
let i4 = i3, snd invs in
            get_includes' includes (i4,j,ts')
      )
      qns
    end
  in
  let includes = ref [] in
  List.iter (get_includes' includes) xs;
  (* list is unique due to check during construction *)
  !includes

and bind_dir
  state
  bsym_table
  env rs
  (vs,qn)
=
(*
  print_endline ("Try to bind dir " ^ string_of_qualified_name qn);
*)
  let nullmap=Hashtbl.create 3 in
  (* cheating stuff to add the type variables to the environment *)
  let cheat_table = Hashtbl.create 7 in
  List.iter
  (fun (n,i,_) ->
   let entry = NonFunctionEntry {base_sym=i; spec_vs=[]; sub_ts=[]} in
    Hashtbl.add cheat_table n entry;
    if not (Flx_sym_table.mem state.sym_table i) then
      Flx_sym_table.add state.sym_table i None {
        Flx_sym.id=n;
        sr=dummy_sr;
        vs=dfltvs;
        pubmap=nullmap;
        privmap=nullmap;
        dirs=[];
        symdef=SYMDEF_typevar TYP_type
      }
    ;
  )
  (fst vs)
  ;
  let cheat_env = (dummy_bid,"cheat",cheat_table,[],TYP_tuple []) in
  let rs2 = {rs with open_excludes = (vs,qn)::rs.open_excludes } in
  let {base_sym=i; spec_vs=spec_vs; sub_ts=ts}, ts' =
    try
      lookup_qn_in_env' state bsym_table env rs
      qn
    with Not_found -> failwith "QN NOT FOUND"
  in
  (* the vs is crap I think .. *)
  (*
  the ts' are part of the name and are bound in calling context
  the ts, if present, are part of a view we found if we
  happened to open a view, rather than a base module.
  At present this cannot happen because there is no way
  to actually name a view.
  *)
  (*
  assert (List.length vs = 0);
  assert (List.length ts = 0);
  *)
  let mkenv i = mk_bare_env state bsym_table i in
(*
  print_endline ("Binding ts=" ^ catmap "," string_of_typecode ts');
*)
  let ts' = List.map (fun t ->
    let btyp =  
      try (bind_type' state bsym_table (cheat_env::env) rs dummy_sr t [] mkenv) 
      with exn -> 
         print_endline ("[bind_dir] Type binding failed for " ^ string_of_typecode t); 
         raise exn
    in
    try
    beta_reduce "flx_lookup: bind_dir"
      state.counter
      bsym_table
      dummy_sr
      btyp
    with exn -> print_endline ("Beta-reduction failed, type " ^ sbt bsym_table btyp); raise exn
    ) ts' 
  in
  (*
  print_endline ("Ts bound = " ^ catmap "," (sbt bsym_table) ts');
  *)
  (*
  let ts' = List.map (fun t-> bind_type state env dummy_sr t) ts' in
  *)
  vs,i,ts'

and review_entry state bsym_table name sr vs ts {base_sym=i; spec_vs=vs'; sub_ts=ts'} : entry_kind_t =
   (* vs is the set of type variables at the call point,
     there are vs in the given ts,
     ts is the instantiation of another view,
     the number of these should agree with the view variables vs',
     we're going to plug these into formula got thru that view
     to form the next one.
     ts' may contain type variables of vs'.
     The ts' are ready to plug into the base objects type variables
     and should agree in number.

     SO .. we have to replace the vs' in each ts' using the given
     ts, and then record that the result contains vs variables
     to allow for the next composition .. whew!
   *)

   (* if vs' is has extra variables,
      (*
      tack them on to the ts
      *)
      synthesise a new vs/ts pair
      if vs' doesn't have enough variables, just drop the extra ts
   *)
    (*
    print_endline ("Review entry " ^ name ^ "<" ^ si i ^">");
    print_endline ("input vs="^catmap "," (fun (s,i)->s^"<"^si i^">") vs^
      ", input ts="^catmap "," (sbt bsym_table) ts);
    print_endline ("old vs="^catmap "," (fun (s,i)->s^"<"^si i^">") vs'^
      ", old ts="^catmap "," (sbt bsym_table) ts');
   *)
   let vs = ref (List.rev vs) in
   let vs',ts =
     let rec aux invs ints outvs outts =
       match invs,ints with
       | h::t,h'::t' -> aux t t' (h::outvs) (h'::outts)
       | h::t,[] ->
         let i = fresh_bid state.counter in
         let (name,_) = h in
         vs := (name,i)::!vs;
         (*
         print_endline ("SYNTHESISE FRESH VIEW VARIABLE "^si i^" for missing ts");
         *)
         let h' = btyp_type_var (i, btyp_type 0) in
         (*
         let h' = let (_,i) = h in btyp_type_var (i, btyp_type 0) in
         *)
         aux t [] (h::outvs) (h'::outts)
       | [],h::t -> 
         (* NOT seem to happen in practice .. *)
         print_endline ("Extra ts dropped, not enough vs");
         List.rev outvs, List.rev outts
       | [],[] -> List.rev outvs, List.rev outts
     in aux vs' ts [] []
   in
   let vs = List.rev !vs in
   let ts' = List.map (tsubst sr vs' ts) ts' in
   (*
   print_endline ("output vs="^catmap "," (fun (s,i)->s^"<"^si i^">") vs^
   ", output ts="^catmap "," (sbt bsym_table) ts');
   *)
   {base_sym=i; spec_vs=vs; sub_ts=ts'}

and review_entry_set state bsym_table k v sr vs ts : entry_set_t = match v with
  | NonFunctionEntry i -> NonFunctionEntry (review_entry state bsym_table k sr vs ts i)
  | FunctionEntry fs -> FunctionEntry (List.map (review_entry state bsym_table k sr vs ts) fs)

and make_view_table state bsym_table table sr vs ts : name_map_t =
  let h = Hashtbl.create 97 in
  Hashtbl.iter
  (fun k v ->
    let v = review_entry_set state bsym_table k v sr vs ts in
    Hashtbl.add h k v
  )
  table
  ;
  h

and pub_table_dir state bsym_table env (invs,i,ts) : name_map_t =
  let invs = List.map (fun (i,n,_)->i,n) (fst invs) in
  let sym = get_data state.sym_table i in
  match sym.Flx_sym.symdef with
  | SYMDEF_root _ 
  | SYMDEF_library 
  | SYMDEF_module ->
    let table = 
      if List.length ts = 0 
      then sym.Flx_sym.pubmap 
      else make_view_table state bsym_table sym.Flx_sym.pubmap (sym.Flx_sym.sr) invs ts 
    in
    table

  | SYMDEF_typeclass  ->
    let table = 
      if List.length ts = 0 
      then sym.Flx_sym.pubmap 
      else make_view_table state bsym_table sym.Flx_sym.pubmap (sym.Flx_sym.sr) invs ts 
    in
    (* a bit hacky .. add the type class specialisation view
       to its contents as an instance
    *)
    let inst = mkentry state sym.Flx_sym.vs i in
    let inst = review_entry state bsym_table sym.Flx_sym.id sym.Flx_sym.sr invs ts inst in
    let inst_name = "_inst_" ^ sym.Flx_sym.id in

    (* add inst thing to table *)
    Hashtbl.add table inst_name (FunctionEntry [inst]);
    table

  | _ ->
      clierrx "[flx_bind/flx_lookup.ml:6142: E228] " sym.Flx_sym.sr "[map_dir] Expected module"


and get_pub_tables state bsym_table env rs (dirs:sdir_t list) =
  let _,includes,_ = split_dirs rs.open_excludes dirs in
  let xs = uniq_list (List.map (bind_dir state bsym_table env rs) includes) in
  let includes = get_includes state bsym_table rs xs in
  let tables = List.map (pub_table_dir state bsym_table env ) includes in
  tables

and mk_bare_env state bsym_table index =
  let parent, sym = Flx_sym_table.find_with_parent state.sym_table index in
  (index, sym.Flx_sym.id, sym.Flx_sym.privmap, [], TYP_tuple []) ::
  match parent with
  | None -> []
  | Some index -> mk_bare_env state bsym_table index

and merge_directives state bsym_table rs env name dirs typeclasses =
  let env = ref env in
  let add table =
   env :=
     match !env with
     | (idx, id, nm, nms,con) :: tail ->
     (idx, id, nm,  table :: nms,con) :: tail
     | [] -> assert false
  in
  let use_map = Hashtbl.create 97 in
  add use_map;

  let add_qn (vs, qn) =
    if List.mem (vs,qn) rs.open_excludes then () else
    begin
      let u = [bind_dir state bsym_table !env rs (vs,qn)] in
      let u = get_includes state bsym_table rs u in
      let tables = List.map (pub_table_dir state bsym_table !env ) u in
      List.iter add tables
    end
  in
  List.iter
  (fun (sr,dir) -> match dir with
  | DIR_inject_module (vs,qn) -> add_qn (vs,qn)
  | DIR_use (n,qn) ->
    begin let entry,_ = lookup_qn_in_env2' state bsym_table !env rs qn in
    match entry with

    | NonFunctionEntry _ ->
      if Hashtbl.mem use_map n
      then failwith "Duplicate non function used"
      else Hashtbl.add use_map n entry

    | FunctionEntry ls ->
      let entry2 =
        try Hashtbl.find use_map  n
        with Not_found -> FunctionEntry []
      in
      match entry2 with
      | NonFunctionEntry _ ->
        failwith "Use function and non-function kinds"
      | FunctionEntry ls2 ->
        Hashtbl.replace use_map n (FunctionEntry (ls @ ls2))
    end

  | DIR_open (vs,qn) -> add_qn (vs,qn)
  )
  dirs;

 (* these should probably be done first not last, because this is
 the stuff passed through the function interface .. the other
 opens are merely in the body .. but typeclasses can't contain
 modules or types at the moment .. only functions .. so it
 probably doesn't matter
 *)
(* AHEM, this has changed! Type classes can contain anything now! *)
 List.iter add_qn typeclasses;
 !env

(* UNUSED .. HMM .. 
and merge_opens state bsym_table env rs (typeclasses,opens,includes,uses) =
  let use_map = Hashtbl.create 97 in
  List.iter
  (fun (n,qn) ->
    let entry,_ = lookup_qn_in_env2' state bsym_table env rs qn in
    match entry with

    | NonFunctionEntry _ ->
      if Hashtbl.mem use_map n
      then failwith "Duplicate non function used"
      else Hashtbl.add use_map n entry

    | FunctionEntry ls ->
      let entry2 =
        try Hashtbl.find use_map  n
        with Not_found -> FunctionEntry []
      in
      match entry2 with
      | NonFunctionEntry _ ->
        failwith "Use function and non-function kinds"
      | FunctionEntry ls2 ->
        Hashtbl.replace use_map n (FunctionEntry (ls @ ls2))
  )
  uses
  ;

  (* convert qualified names to i,ts format *)
  let btypeclasses = List.map (bind_dir state bsym_table env rs) typeclasses in
  let bopens = List.map (bind_dir state bsym_table env rs) opens in

  (* HERE! *)

  let bincludes = List.map (bind_dir state bsym_table env rs) includes in

  (*
  (* HACK to check open typeclass *)
  let _ =
    let xs = get_includes state rs bopens in
    let tables = List.map (pub_table_dir state env true) xs in
    ()
  in
  *)
  (* strip duplicates *)
  let u = uniq_cat [] btypeclasses in
  let u = uniq_cat u bopens in
  let u = uniq_cat u bincludes in

  (* add on any inherited modules *)
  let u = get_includes state bsym_table rs u in

  (* convert the i,ts list to a list of lookup tables *)
  let tables = List.map (pub_table_dir state bsym_table env ) u in

  (* return the list with the explicitly renamed symbols prefixed
     so they can be used for clash resolution
  *)
  use_map::tables
*)

and build_env'' state bsym_table rs index : env_t =
  let parent, sym = Flx_sym_table.find_with_parent state.sym_table index in
  let skip_merges = List.mem index rs.idx_fixlist in

  let rs = { rs with idx_fixlist = index :: rs.idx_fixlist } in
  let env = inner_build_env state bsym_table rs parent in
  
  (* Build temporary bare innermost environment with a full parent env. *)
  let typeclasses, constraints =
    let _, { raw_type_constraint=con; raw_typeclass_reqs=rtcr } =
      sym.Flx_sym.vs
    in
    rtcr,con
  in
  let env = (index, sym.Flx_sym.id, sym.Flx_sym.privmap, [], constraints) :: env in

  (* exit early if we don't need to do any merges *)
  if skip_merges then env else
  (*
  print_endline ("Build_env'' " ^ id ^":" ^ si index ^ " parent="^(match parent with None -> "None" | Some i -> si i));
  print_endline ("Privmap=");
  Hashtbl.iter (fun s _ ->  print_endline s) table ;
  *)

  (* use that env to process directives and type classes *)
  (*
  if typeclasses <> [] then
    print_endline ("Typeclass qns=" ^ catmap "," string_of_qualified_name typeclasses);
  *)
  let typeclasses = List.map (fun qn -> dfltvs,qn) typeclasses in

(*
  print_endline ("MERGE DIRECTIVES for " ^  sym.Flx_sym.id);
*)
  let env = merge_directives state bsym_table rs env sym.Flx_sym.id sym.Flx_sym.dirs typeclasses in
(*
  print_endline "Build_env'' complete, DIRECTIVES MERGED";
*)
  env

and inner_build_env state bsym_table rs parent : env_t =
  match parent with
  | None -> []
  | Some i ->
    try
      let env = Hashtbl.find state.env_cache i in
      env
    with
      Not_found ->
       let env = build_env'' state bsym_table rs i in
       Hashtbl.add state.env_cache i env;
       env

and build_env state bsym_table parent : env_t =
  (*
  print_endline ("Build env " ^ match parent with None -> "None" | Some i -> si i);
  *)
  inner_build_env state bsym_table rsground parent


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

and rebind_btype state bsym_table env sr ts t =
  let rbt t = rebind_btype state bsym_table env sr ts t in
  match t with
  | BTYP_hole -> assert false
  | BTYP_tuple_cons _ -> assert false
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
  | BTYP_array (t1,t2) -> btyp_array (rbt t1, rbt t2)

  | BTYP_int
  | BTYP_label
  | BTYP_unitsum _
  | BTYP_void
  | BTYP_fix _ -> t

  | BTYP_type_var (i,mt) -> clierrx "[flx_bind/flx_lookup.ml:6403: E229] " sr ("[rebind_type] Unexpected type variable " ^ sbt bsym_table t)
  | BTYP_type_apply _
  | BTYP_type_function _
  | BTYP_type _
  | BTYP_type_tuple _
  | BTYP_type_match _
    -> clierrx "[flx_bind/flx_lookup.ml:6409: E230] " sr ("[rebind_type] Unexpected metatype " ^ sbt bsym_table t)


and check_module state name sr entries ts =
    begin match entries with
    | NonFunctionEntry (index) ->
        let sym = get_data state.sym_table (sye index) in
        begin match sym.Flx_sym.symdef with
        | SYMDEF_root _
        | SYMDEF_library 
        | SYMDEF_module ->
            Flx_bind_deferred.Simple_module (sye index, ts, sym.Flx_sym.pubmap, sym.Flx_sym.dirs)
        | SYMDEF_typeclass ->
            Flx_bind_deferred.Simple_module (sye index, ts, sym.Flx_sym.pubmap, sym.Flx_sym.dirs)
        | _ ->
            clierrx "[flx_bind/flx_lookup.ml:6424: E231] " sr ("Expected '" ^ sym.Flx_sym.id ^ "' to be module in: " ^
            Flx_srcref.short_string_of_src sr ^ ", found: " ^
            Flx_srcref.short_string_of_src sym.Flx_sym.sr)
        end
    | _ ->
      failwith
      (
        "Expected non function entry for " ^ name
      )
    end

(* the top level table only has a single entry,
  the root module, which is the whole file

  returns the root name, table index, and environment
*)

and eval_module_expr state bsym_table env e : module_rep_t =
  (*
  print_endline ("Eval module expr " ^ string_of_expr e);
  *)
  match e with
  | EXPR_name (sr,name,ts) ->
    let entries = inner_lookup_name_in_env state bsym_table env rsground sr name in
    check_module state name sr entries ts

  | EXPR_lookup (sr,(e,name,ts)) ->
    let result = eval_module_expr state bsym_table env e in
    begin match result with
      | Flx_bind_deferred.Simple_module (index,ts',htab,dirs) ->
      let env' = mk_bare_env state bsym_table index in
      let tables = get_pub_tables state bsym_table env' rsground dirs in
      let result = lookup_name_in_table_dirs htab tables sr name in
        begin match result with
        | Some x ->
          check_module state name sr x (ts' @ ts)

        | None -> clierrx "[flx_bind/flx_lookup.ml:6461: E232] " sr
          (
            "Can't find " ^ name ^ " in module"
          )
        end

    end

  | _ ->
    let sr = src_of_expr e in
    clierrx "[flx_bind/flx_lookup.ml:6471: E233] " sr
    (
      "Invalid module expression " ^
      string_of_expr e
    )

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
  with Not_found -> failwith "xxxx bind expression raised Not_found [BUG]"
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





