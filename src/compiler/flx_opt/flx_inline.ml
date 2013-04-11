open List
open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_exceptions
open Flx_foldvars
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

let string_of_vs vs =
  "[" ^ catmap "," (fun (s,i)->s^"<"^si i^">") vs ^ "]"

(* varmap is the *typevariable* remapper,
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

let mk_label_map syms exes =
  let h = Hashtbl.create 97 in
  let aux = function
  | BEXE_label (sr,s) ->
    let n = fresh_bid syms.counter in
    let s' =  "_" ^ string_of_bid n in
    Hashtbl.add h s s'
  | _ -> ()
  in
    List.iter aux exes;
    h

let idt t = t

(*
(* APPEARS TO BE UNUSED .. *)
let is_simple_expr syms bsym_table e =
  print_endline ("Is " ^ sbe bsym_table e ^ " simple?");
  match e with
  | BEXPR_ref _,_ -> print_endline "YES"; true
  | _ -> print_endline "NO"; false
*)

(* CALL LIFTING. What this does is transform a call:

  call (f a) arg

  by replacing it with the body of f,
  in which every

  return x

  is replaced by

  call x arguemnt

  This converts  f from a function returning
  a procedure, to a procedure which executes that
  procedure.

  NOTE: this is a special case of the distributive law.

  f (if c then a else b) v => if c then f a v else f b v

*)

let call_lifting syms uses bsym_table caller caller_vs callee ts a argument =
  (*
  print_endline "DOING CALL LIFTING";
  *)
  let bsym = Flx_bsym_table.find bsym_table callee in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
    (*
    print_endline ("Found procedure "^id^": Inline it!");
    *)
    let relabel = mk_label_map syms exes in
    let varmap =
      try mk_varmap vs ts
      with Failure x ->
        print_endline "[call_lifting] FAIL mk_varmap";
        raise (Failure x)
    in
    let callee_vs_len = length vs in

    let revariable = reparent_children
      syms uses bsym_table
      caller_vs callee_vs_len callee (Some caller) relabel varmap false []
    in
    (* use the inliner to handle the heavy work *)
    let body = gen_body
      syms
      uses bsym_table
      (Flx_bsym.id bsym)
      varmap
      ps
      relabel
      revariable
      exes
      a
      (Flx_bsym.sr bsym)
      caller
      callee
      caller_vs
      callee_vs_len
      `Lazy
      props
    in

    (* replace all function returns with tailed calls *)
    let body2 = ref [] in
    let n = fresh_bid syms.counter in
    let end_label = "_end_call_lift_" ^ string_of_bid n in
    body2 := bexe_label (Flx_bsym.sr bsym,end_label) :: !body2;
    List.iter
      (function
      | BEXE_fun_return (sr,e) ->
        (* NOTE REVERSED ORDER *)
        let call_instr =
          (
          (*
          match e with
          | BEXPR_closure (i,ts),_ ->
            bexe_call_direct (sr,i,ts,argument)
          | _ ->
          *)
            bexe_call (sr,e,argument)
          )
        in
        body2 := bexe_goto (sr,end_label) :: !body2;
        body2 := call_instr :: !body2;
      | BEXE_yield _ ->
        syserr (Flx_bsym.sr bsym) "Attempt to inline generator containing a yield"
      | x -> body2 := x::!body2
      )
      body
    ;
    (*
    print_endline (
     catmap "\n" (string_of_bexe bsym_table 0) !body2
    )
    ;
    *)
    revariable,!body2 (* forward order *)

  | _ -> assert false

let inline_tail_apply syms uses bsym_table caller caller_vs callee ts a =
  (* TEMPORARY .. this should be allowed for unrolling but we do not do that yet *)
  assert (callee <> caller);
  let bsym = Flx_bsym_table.find bsym_table callee in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
    (*
    let id2,_,_,_ = hfind "inline-tail[function]" bsym_table caller in
    print_endline
    (
      "TAIL Inlining function "^id^
      "<"^si callee^">"^
      "[" ^ catmap "," (sbt bsym_table) ts ^ "] into " ^ id2 ^ "<" ^ si caller ^">"
    );
    *)
    let relabel = mk_label_map syms exes in
    let varmap =
      try mk_varmap vs ts
      with Failure x ->
        print_endline "[inline_tail_apply] FAIL mk_varmap";
        raise (Failure x)
    in
    let callee_vs_len = length vs in

    let revariable = reparent_children
      syms uses bsym_table
      caller_vs callee_vs_len callee (Some caller) relabel varmap false []
    in

    (* use the inliner to handle the heavy work *)
    let body = gen_body
      syms
      uses bsym_table
      (Flx_bsym.id bsym)
      varmap
      ps
      relabel
      revariable
      exes
      a
      (Flx_bsym.sr bsym)
      caller
      callee
      caller_vs
      callee_vs_len
      `Lazy
      props
    in
    revariable,rev body

  | _ -> assert false

let inline_function syms uses bsym_table caller caller_vs callee ts a varindex =
  (*
  print_endline ("Inline function: init var index " ^ si varindex);
  *)
  let bsym = Flx_bsym_table.find bsym_table callee in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
    (*
    print_endline
    (
      "Inlining function "^id^
      "<"^si callee^">"^
      "[" ^ catmap "," (sbt bsym_table) ts ^ "]"^
      " retvar="^ si varindex ^
      "\nvs = " ^ catmap "," (fun (s,i) -> s ^ "<" ^ si i ^ ">") vs
    );
    *)
    let relabel = mk_label_map syms exes in
    let varmap =
      try mk_varmap vs ts
      with Failure x ->
        print_endline "[inline_function] FAIL mk_varmap";
        raise (Failure x)
    in
    let callee_vs_len = length vs in

    let revariable = reparent_children
      syms uses bsym_table
      caller_vs callee_vs_len callee (Some caller) relabel varmap false []
    in

    (* use the inliner to handle the heavy work *)
    let body = gen_body
      syms
      uses bsym_table
      (Flx_bsym.id bsym)
      varmap
      ps
      relabel
      revariable
      exes
      a
      (Flx_bsym.sr bsym)
      caller
      callee
      caller_vs
      callee_vs_len
      `Lazy
      props
    in

    (*
    print_endline "Replace returns with inits";
    *)
    (* replace all function returns with variable initialisations *)
    let body2 = ref [] in
    let n = fresh_bid syms.counter in
    let end_label = "_end_inline_" ^ Flx_bsym.id bsym ^ "_" ^ string_of_bid n in
    let t = ref None in
    let end_label_used = ref false in
    List.iter
      (function
      | BEXE_fun_return (sr,((_,t') as e)) ->
        t := Some t';
        if not (!body2 == []) then begin
          body2 := bexe_goto (sr,end_label) :: !body2;
          end_label_used := true
        end
        ;
        let call_instr = bexe_init (sr,varindex,e) in
        (*
        print_endline ("Replacing return with init: " ^ string_of_bexe bsym_table 0 call_instr);
        *)
        body2 := call_instr :: !body2;

      | BEXE_yield _ ->
        syserr (Flx_bsym.sr bsym) "Attempt to inline generator with a yield"

      | x -> body2 := x::!body2
      )
      body
    ;
    (* Ugghhh *)
    if !end_label_used then
      body2 := !body2 @ [bexe_label (Flx_bsym.sr bsym,end_label)]
    ;
    (*
    print_endline (
     catmap "\n" (string_of_bexe bsym_table 0) !body2
    )
    ;
    *)
    revariable,!body2 (* forward order *)

  | _ -> assert false

(* note u sr e must return exes in reverse order, this
  function however returns exes in forward order
*)
let expand_exe syms bsym_table u exe =
  let xs =
    (*
    print_endline ("EXPAND EXE " ^ string_of_bexe bsym_table 0 exe);
    *)
    match exe with
    | BEXE_axiom_check _ -> assert false
    | BEXE_call_prim (sr,i,ts,e2) -> assert false
      (*
      let e,xs = u sr e2 in
      bexe_call_prim (sr,i,ts,e) :: xs
      *)

    | BEXE_call_stack (sr,i,ts,e2) -> assert false

    | BEXE_call_direct (sr,i,ts,e2) -> assert false
      (*
      let e,xs = u sr e2 in
      bexe_call_direct (sr,i,ts,e) :: xs
      *)

    | BEXE_jump_direct (sr,i,ts,e2) -> assert false
      (*
      let e,xs = u sr e2 in
      bexe_jump_direct (sr,i,ts,e) :: xs
      *)

    | BEXE_assign (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_assign (sr,e1,e2) :: xs2 @ xs1

    | BEXE_assert (sr,e) ->
      let e,xs = u sr e in
      bexe_assert (sr,e) :: xs

    | BEXE_assert2 (sr,sr2,e1,e2) ->
      let e1,xs1 =
        match e1 with Some e -> let a,b = u sr e in Some a,b
        | None -> None,[]
      in
      let e2,xs2 = u sr e2 in
      bexe_assert2 (sr,sr2,e1,e2) :: xs2 @ xs1

    | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
      let e1,xs1 =
        match e1 with Some e -> let a,b = u sr e in Some a,b
        | None -> None,[]
      in
      let e2,xs2 = u sr e2 in
      bexe_axiom_check2 (sr,sr2,e1,e2) :: xs2 @ xs1

    (* preserve call lift pattern ??*)
    | BEXE_call (sr,(BEXPR_apply((BEXPR_closure(i,ts),t'),e1),t),e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_call (sr,
        (bexpr_apply t ((bexpr_closure t' (i,ts)),e1)),
        e2) :: xs2 @ xs1

    | BEXE_call (sr,e1,e2) ->
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_call (sr,e1,e2) :: xs2 @ xs1

    | BEXE_jump (sr,e1,e2) -> 
      let e1,xs1 = u sr e1 in
      let e2,xs2 = u sr e2 in
      bexe_jump (sr,e1,e2) :: xs2 @ xs1

    | BEXE_ifgoto (sr,e,lab) ->
      let e,xs = u sr e in
      bexe_ifgoto (sr,e,lab) :: xs

    (* preserve tail call pattern -- used by both
       tail-rec eliminator
       and by call lifter (which converts returns to calls)
    *)
    | BEXE_fun_return (sr,(BEXPR_apply((BEXPR_closure(i,ts),t'),e),t)) ->
      let e,xs = u sr e in
      bexe_fun_return (sr,
        (bexpr_apply t ((bexpr_closure t' (i,ts)),e))) :: xs

    | BEXE_fun_return (sr,e) ->
      let e,xs = u sr e in
      bexe_fun_return (sr,e) :: xs

    | BEXE_yield (sr,e) ->
      let e,xs = u sr e in
      bexe_yield (sr,e) :: xs

    (* This case has to be handled specially, in case we already
       have a simplified form, and the unravelling introduces
       a gratuitous extra variable: for example

       x : = f a

       might expand to

       x' = f a
       x := x'

       which is rather pointless. There is, unfortunately,
       a duplicate of this check elsewhere ..
    *)

    | BEXE_init (sr,i,(BEXPR_apply((BEXPR_closure (j,ts),t'),e),t))
      (*
      when is_generator bsym_table j
      *)
      ->
      let e,xs = u sr e in
      bexe_init (sr, i,
        (bexpr_apply t ((bexpr_closure t' (j,ts)),e))) :: xs

    | BEXE_init (sr,i,e) ->
      let e,xs = u sr e in
      bexe_init (sr,i,e) :: xs

    | BEXE_svc _
    | BEXE_label _
    | BEXE_goto _
    | BEXE_code _
    | BEXE_nonreturn_code _
    | BEXE_proc_return _
    | BEXE_comment _
    | BEXE_nop _
    | BEXE_halt _
    | BEXE_trace _
    | BEXE_begin
    | BEXE_end
    | BEXE_try _
    | BEXE_catch _
    | BEXE_endtry _
      -> [exe]
  in
    let xs = rev xs in
    xs

let check_reductions syms bsym_table exes = 
  let exes = 
    try 
      Flx_reduce.reduce_exes syms bsym_table !(syms.reductions) exes 
    with Not_found -> assert false
  in
  exes

let heavy_inline_call syms uses bsym_table
  caller caller_vs callee ts argument id sr (props, vs, (ps,traint), exes)
=
  (*
  print_endline ("INLINING CALL to " ^ id ^"<"^ si callee^">("^sbe bsym_table argument^")");
  print_endline ("In procedure " ^ si caller ^ " with vs=" ^ string_of_vs caller_vs);
  print_endline ("Callee is " ^ id ^ "<"^si callee ^ "> with ts = " ^ catmap "," (sbt bsym_table) ts);
  print_endline ("Callee vs=" ^ string_of_vs vs);
  *)
  let caller_vs_len = length caller_vs in
  let callee_vs_len = length vs in
  (*
  print_endline ("In the callee and its children,");
  print_endline ("The callee vs are elided and replaced by the caller vs");
  print_endline ("ELIDE: first " ^ si callee_vs_len ^ ", PREPEND " ^ si caller_vs_len);
  print_endline ("This works by instantiating the callee vs with the calls ts");
  *)
  assert(length vs = length ts);

  (*
  print_endline ("Found procedure "^id^": Inline it!");
  *)
  let relabel = mk_label_map syms exes in
  let varmap =
    try mk_varmap vs ts
    with Failure x ->
      print_endline "[heavy_inline_call] FAIL mk_varmap";
      raise (Failure x)
  in
  let revariable = reparent_children
    syms uses bsym_table
    caller_vs callee_vs_len callee (Some caller) relabel varmap false []
  in
  let xs = gen_body
    syms
    uses bsym_table
    id
    varmap
    ps
    relabel
    revariable
    exes
    argument
    sr
    caller
    callee
    caller_vs
    callee_vs_len
    `Lazy
    props
  in
    revariable,rev xs (* forward order *)

let make_specialisation syms uses bsym_table
  caller caller_vs callee ts id sr parent props vs exes rescan_flag
=
  (*
  print_endline ("Specialising call " ^ id ^ "<"^si callee ^ ">[" ^ catmap "," (sbt bsym_table) ts ^"]");
  print_endline ("In procedure " ^ si caller ^ " with vs=" ^ string_of_vs caller_vs);
  print_endline ("Callee vs=" ^ string_of_vs vs);
  *)
  let caller_vs_len = length caller_vs in
  let callee_vs_len = length vs in

  (*
  print_endline ("In the callee and its children,");
  print_endline ("The callee vs are elided and replaced by the caller vs");
  print_endline ("ELIDE: first " ^ si callee_vs_len ^ ", PREPEND " ^ si caller_vs_len);
  print_endline ("This works by instantiating the callee vs with the calls ts");
  *)
  assert(length vs = length ts);

  (*
  print_endline ("Found procedure "^id^": Inline it!");
  *)
  let relabel = mk_label_map syms exes in
  let varmap =
    try mk_varmap vs ts
    with Failure x ->
      print_endline "[make_specialisation] FAIL mk_varmap";
      raise (Failure x)
  in
  let k,ts' =
    specialise_symbol
      syms uses bsym_table
      caller_vs callee_vs_len callee ts parent relabel varmap rescan_flag
   in
   (*
   print_endline ("Specialised to " ^ id ^ "<"^si k ^ "> with ts = " ^ catmap "," (sbt bsym_table) ts');
   *)
   k,ts'

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

let inlining_complete bsym_table i =
  match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_fun (props,_,_,_,_) -> mem `Inlining_complete props
  | BBDCL_external_fun _ -> true

  | _ -> assert false


(*


See post in felix-language. The problem is knowing
when to inline a function: typeclass virtual function
default methods can only be inlined if there is an instance
of the typeclass AND the virtual is not overridden in the
instance.

Proposed algorithm.

1. Check if the function is virtual.

2. If so, find its parent, which is the typeclass

3. strip the tail off the instantiating ts so it
   matches the length of the typeclass vs list
   (in case the function is polymorphic, the function's
    private type arguments will be the remaining ones)

3. Using a table of pairs:

	(typeclass, (instance, (vs,ts)))

discover if there is an instance. This is actually hard:
the check actually requires seeing if the given ts specialises
one of the ts in the above table for the given typeclass.
The instantiation's vs is required too, since the ts of the
typeclass have to be mapped to the instance view.

4. IF there is a match:

4a. try to find an instance  of the virtual function.

4b. If none is found, then inline the virtual function
    default body

4c.  otherwise inline the instance.

5. otherwise (no instance) leave the call alone.

IF we know the code is fully monorphised then 5 becomes
instead an error.

Note that Felix currently DOES NOT detect this error.
If the function has a default, it will be used even
if there is no instance. This will result in either
an infinite recursion at run time OR lead to another
virtual that has no body, resulting in an error diagnostic.

But the infinite recursion is also possible even if there
is an instance .. so nothing is lost here.. :)

*)

let virtual_check syms bsym_table sr i ts =
  let parent, bsym = Flx_bsym_table.find_with_parent bsym_table i in
  let name : string = Flx_bsym.id bsym in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_external_fun (props,_,_,_,_,_,_)
  | BBDCL_fun (props,_,_,_,_) when mem `Virtual props ->
    let parent =
      match Flx_bsym_table.find_parent bsym_table i with
      | Some p -> p
      | None -> syserr sr ("can't find parent of " ^ Flx_bsym.id bsym ^" " ^ si i ^ " but is it " ^ match parent with | Some k -> si k | None -> "None");
        assert false
    in
    let tcvslen =
      try
        match Flx_bsym_table.find_bbdcl bsym_table parent with
        | BBDCL_typeclass (_, vs) ->
          (*
          print_endline ("Found parent " ^ pid ^ "<" ^ si i ^ ">");
          *)
          List.length vs
        | _ ->
          print_endline "Woops, parent isn't typeclass?";
          assert false
      with Not_found ->
        print_endline ("Parent typeclass " ^ string_of_bid parent ^
          " not found!");
        assert false
    in
    let tslen = List.length ts in
    (*
    print_endline ("Vs len of parent = " ^ si tcvslen);
    print_endline ("ts len           = " ^ si tslen);
    *)
    if tcvslen > tslen then
      clierr sr "Not enough type arguments for typeclass"
    ;
    let fts = rev (list_prefix (rev ts) (tslen - tcvslen)) in
    let ts = list_prefix ts tcvslen in
(*
    let instances =
      try Hashtbl.find syms.instances_of_typeclass parent
      with Not_found ->
        (*
        print_endline "No instances of typeclass?";
        *)
        (*
        assert false
        *)
        []
    in
    (*
    print_endline "Found some instances!";
    print_endline ("ts = " ^ catmap "," (sbt bsym_table) ts);
    *)
    let matches = ref [] in
    List.iter (fun (j,(jvs,jcon,jts)) ->
      (*
      print_endline ("instance[" ^
        catmap "," (fun (s,i) -> s^ "<"^si i^">") jvs ^ "] " ^
        si j ^ "[" ^
        catmap "," (sbt bsym_table) jts ^ "]"
      );
      *)
      (* check if the call specialises the instance. *)
      let ok =
        Flx_typeclass.tcinst_chk syms bsym_table true i ts sr
          (jvs, jcon,jts, j)
      in
      begin match ok with
      | `MatchesNow,_,_ -> matches := j :: !matches

      | _ -> (* print_endline "Doesn't match"; *)  ()
      end
    )
    instances
    ;
    begin match !matches with
    | [_] ->
*)
      begin let res = Flx_typeclass.fixup_typeclass_instance' syms bsym_table true i ts in
      match res with
      | `MatchesNow,j,ts' -> (* print_endline (name ^ " Matches now, inline " ^ si j); *) true,j,ts' @ fts (* inline matching function *)
      | `MaybeMatchesLater,_,_ -> (* print_endline (name ^ " May match later, deref " ^ si i); *) false,i,ts@fts (* wait *)
      | `CannotMatch,_,_ -> (* print_endline (name ^ " No match use default " ^ si i); *) true,i,ts@fts (* inline default if any *)
      end
(*
      let i',ts' =
        Flx_typeclass.maybe_fixup_typeclass_instance syms bsym_table i ts
      in
      if i = i' then begin
        (*
        print_endline (id ^ " -- Dispatch to default");
        *)
        true,i',ts' @ fts
      end else begin
        (*
        print_endline (id ^ " -- Dispatch to instance");
        *)
        true,i',ts' @ fts
      end
    | _ ->
      (*
      print_endline (id ^ " -- Dispatch unknown");
      *)
      false,i,ts @ fts
    end
*)

  | BBDCL_fun (_,_,_,_,exes) ->
    let chk_yield acc exe = match exe with BEXE_yield _ -> false | _ -> acc in
    let can_inline = fold_left chk_yield true exes in
    can_inline,i,ts
    

  | _ -> (* print_endline (id ^ " -- Not virtual") *) true,i,ts

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

  This routine has to be called on an actual function, not a virtual.
*)

let inline_check syms bsym_table uses srcall caller callee props exes =
  let bsym = Flx_bsym_table.find bsym_table callee in
  let srdef = Flx_bsym.sr bsym in
  let id = Flx_bsym.id bsym in
  let bbdcl = Flx_bsym.bbdcl bsym in
  let kind = 
     match bbdcl with
     | BBDCL_fun (_,_,_,ret,_) -> (match ret with BTYP_void -> `Proc | _ -> `Fun)
     | _ -> assert false
  in

  let contains_svc = 
    (let chk_svc acc exe = match exe with BEXE_svc _ -> false | _ -> acc in
    not (fold_left chk_svc true exes))
  in
  let contains_yield = 
    (let chk_yield acc exe = match exe with BEXE_yield _ -> false | _ -> acc in
    not (fold_left chk_yield true exes))
  in

  (* This says the target function calls itself. That does not
     mean the call to the target is a recursive call.
  *)
  let target_is_recursive =
      Flx_call.is_recursive_call uses callee callee
  in

  (* This says that there is some call in the caller to the target,
     and some call in the target to the caller, implying both
     are recursive, and, mutually recursive.
  *)
  let call_is_recursive = 
      Flx_call.is_recursive_call uses caller callee
  in

  let target_is_child =
      Flx_bsym_table.is_child bsym_table caller callee
  in

  if mem `NoInline props && mem `Inline props 
  then begin
    clierr srdef ("Function " ^ id ^ ": Conflicting properties inline and noinline")
  end 
  ;
  if mem `Inline props &&
    Flx_call.is_recursive_call uses caller callee &&
    not (Flx_bsym_table.is_child bsym_table caller callee)
  then begin
    clierr2 srcall srdef ("Cannot inline recursive call to non-child inline function " ^ id)
  end
  ;
  if mem `Inline props && contains_yield
  then begin
    clierr2 srcall srdef ("Cannot inline function " ^ id ^ " containing a yield instruction")
  end
  ;

  if mem `NoInline props && kind = `Fun && contains_svc
  then begin
    clierr2 srcall srdef ("Must inline function " ^ id ^ " containing svc instruction")
  end
  ;
  let result =
    not (mem `NoInline props) &&
    (
        mem `Inline props || mem `GeneratedInline props ||
        length exes <= syms.compiler_options.max_inline_length
    ) &&
    (
      not target_is_recursive || call_is_recursive && target_is_child
    )
  in result

let rec special_inline syms uses bsym_table caller_vs caller hic excludes sr e =
  (*
  print_endline ("Special inline " ^ sbe bsym_table e);
  *)
  let exes' = ref [] in
  let id x = x in
  let rec aux e =
  (*
  print_endline (" ... Special inline subexpr: " ^ sbe bsym_table e);
  *)
  match Flx_bexpr.map ~f_bexpr:aux e with
  | BEXPR_get_n ((BEXPR_case (n,_),_),(BEXPR_tuple ls,_)),_ -> 
    nth ls n

  | BEXPR_get_n ((BEXPR_case (n,_),_),(BEXPR_record ls,_)),_ -> 
    snd (nth ls n)

  (* get_n on a struct apply to an explicit tuple .. *)
  | BEXPR_get_n (
      (BEXPR_case (n,_),_),
      (BEXPR_apply ((BEXPR_closure (bid,ts),_),(BEXPR_tuple ls,_)),_)
    ),_ -> 
    let bbdcl = Flx_bsym_table.find_bbdcl bsym_table bid in
    begin match bbdcl with 
    | BBDCL_struct _
    | BBDCL_cstruct _ -> nth ls n
    | _ ->  e
    end

  | BEXPR_closure (callee,_),_ as x ->
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      x

  | ((BEXPR_apply_prim (callee,ts,a),t) as e)
  | ((BEXPR_apply_stack (callee,ts,a),t) as e)
  | ((BEXPR_apply_direct (callee,ts,a),t) as e) -> assert false

  | (((BEXPR_apply(  (BEXPR_closure (callee,ts),_) ,a)),t) as e)
    ->
      let can_inline,callee,ts = virtual_check syms bsym_table sr callee ts in
      if not (mem callee excludes) then begin
        heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
        let bsym = Flx_bsym_table.find bsym_table callee in
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_external_fun (props,_,_,_,_,_,_)
        | BBDCL_fun (props,_,_,_,_)  
          when mem `Generator props
          ->
          (*
          print_endline ("Unravel generator " ^ id);
          *)

          (* create a new variable *)
          let urv = fresh_bid syms.counter in
          let urvid = "_genout_urv" ^ string_of_bid urv in
          add_use uses caller urv sr;
          Flx_bsym_table.add bsym_table urv (Some caller)
            (Flx_bsym.create ~sr urvid (bbdcl_val (caller_vs,t,`Var)));

          (* set variable to function appliction *)
          let cll = bexe_init (sr,urv,e) in
          exes' := cll :: !exes';


          (* replace application with the variable *)
          let ts = List.map
            (fun (_,i)-> btyp_type_var (i,btyp_type 0))
            caller_vs
          in
          bexpr_name t (urv,ts)

        | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
          (* TEMPORARY FIX! *)

          (*
          (* create a new variable *)
          let urv = !(syms.counter) in incr (syms.counter);
          let urvid = "_urv" ^ si urv in
          add_child caller urv;
          add_use uses caller urv sr;
          let entry = BBDCL_val (caller_vs,t) in
          Flx_bsym_table.update bsym_table urv (urvid,Some caller,sr,entry);

          (* set variable to function appliction *)
          let cll = BEXE_init (sr,urv,e) in
          exes' := cll :: !exes';


          (* replace application with the variable *)
          let ts = List.map (fun (_,i)-> btyp_type_var (i,btyp_type 0)) caller_vs in
          BEXPR_name (urv,ts),t
          *)



          (*
          print_endline ("Consider inlining " ^ id ^ "<" ^ si callee ^ "> into " ^ si caller);
          print_endline ("  Child? " ^
            if Flx_bsym_table.is_child bsym_table caller callee then "YES" else "NO")
          ;
          print_endline ("  Recursive? " ^
            if Flx_call.is_recursive_call uses callee callee then "YES" else "NO")
          ;
          print_endline ("  Recursive Call? " ^
            if Flx_call.is_recursive_call uses caller callee then "YES" else "NO")
          ;
          print_endline ("  Can_inline flag? " ^
            if can_inline then "YES" else "NO")
          ;
          print_endline ("  Short enough? " ^
            if length exes <= syms.compiler_options.max_inline_length then "YES" else "NO")
          ;
          print_endline ("  Noinline property? " ^
            if mem `NoInline props then "YES" else "NO")
          ;
          print_endline ("  Inline property? " ^
            if mem `Inline props then "YES" else "NO")
          ;
          *)
          let inline_choice =
            can_inline && inline_check syms bsym_table uses sr caller callee props exes 
          in
          (*
          print_endline ("  Inline decision: " ^
            if inline_choice then "YES" else "NO")
          ;
          *)
          if inline_choice
          then
              begin
                (*
                heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
                *)
                if not (inlining_complete bsym_table callee) then print_endline "Inlining isn't complete in callee ..??";

                if inlining_complete bsym_table callee then begin
                  (*
                  print_endline ("INLINE " ^ id ^ "<" ^ si callee ^ ">");
                  print_endline ("Special inline " ^ si caller ^" calls " ^ si callee);
                  *)
                  (* GENERAL CASE -- we need to add a variable *)
                  let urv = fresh_bid syms.counter in
                  (* inline the code, replacing returns with variable inits *)
                  let revariable,xs =
                     inline_function syms uses bsym_table caller caller_vs callee ts a urv
                  in
                  match rev xs with
                  (* SPECIAL CASE DETECTOR: if the inlined function
                    terminates with an initialisation of the new variable,
                    ignore the variable and use the value used to initialise
                    it instead. This is sure to be the result of the sole
                    trailing return. If there were another return, a
                    jump to the end of the function would be needed,
                    past this initialisation, which would require a label
                    at the end of the function

                    Note this is a bad form of 'apply lifting'.
                    We should be able to inline

                    f (g x)

                    by inlining g x, and replacing 'return e'
                    with 'v = f e' everywhere. instead we get
                    v = e in various places, then f v.

                    To do this right we need to see a double application.
                  *)
                  | [] -> assert false
                  | BEXE_init (sr,j,e') :: tail ->
                    assert (j==urv);
                    (*
                    print_endline "DETECTED SPECIAL CASE";
                    print_endline "Outputing tail:";
                    List.iter (fun x -> print_endline (string_of_bexe bsym_table 0 x)) (rev tail);
                    print_endline ("Expr: " ^ sbe bsym_table e');
                    *)
                    let tail = hic revariable callee (rev tail) in
                    exes' := rev tail @ !exes';
                    e'

                  | rxs ->
                    let urvid = "_urv" ^ string_of_bid urv in
                    add_use uses caller urv sr;
                    Flx_bsym_table.add bsym_table urv (Some caller)
                      (Flx_bsym.create ~sr urvid (bbdcl_val (caller_vs,t,`Val)));

                    let rxs = hic revariable callee xs in
                    exes' := rev rxs @ !exes';
                    let ts = List.map
                      (fun (_,i)-> btyp_type_var (i,btyp_type 0))
                      caller_vs
                    in
                    bexpr_name t (urv,ts)
                end
                else
                begin
                  (*
                  print_endline ("***> Didn't inline " ^ id);
                  *)
                  e
                end
              end
          else
          begin
            (*
            print_endline ("***> Didn't inline " ^ id);
            *)
            e
          end
        | _ -> e
        end
      end else e

  | x -> x
  in
   let e = aux e in (* we need left to right evaluation here ..*)
   e,!exes'

and heavy_inline_calls
  syms
  bsym_table
  uses
  caller_vs
  caller
  excludes
  exes
=
  (*
  print_endline ("HIC: Input excludes = " ^ catmap "," si excludes);
  *)
  let specialise_check caller callee ts props exes = false
    (*
    (* for the moment, don't specialise recursive calls *)
    ts <> [] &&
    not (Flx_call.is_recursive_call uses caller callee)
    *)
  in
  let hic revariable callee exes = if false then exes else
    (*
    print_endline "Rescanning ..";
    *)
    let excludes = fold_left
    (fun acc i ->
      i :: (try [Hashtbl.find revariable i] with Not_found -> []) @ acc
    )
    []
    (callee::excludes)
    in
    heavy_inline_calls
      syms
      bsym_table
      uses
      caller_vs
      caller
      excludes
      exes
  in

  (* The function ee applies the special inlining routine
    to all subexpressions of an expression, bottom up
    (that is, inside out).
  *)

  let sinl sr e = special_inline syms uses bsym_table caller_vs caller hic (caller::excludes) sr e in

  let ee exe = expand_exe syms bsym_table sinl exe in
  let exes' = ref [] in (* reverse order *)
  List.iter  (* each exe *)
  (fun exeIN ->
    (*
    print_endline ("EXE[in] =" ^ string_of_bexe bsym_table 0 exeIN);
    *)
    let xs = ee exeIN in
    (*
    List.iter (fun x -> print_endline ("EXE[out]=" ^ string_of_bexe bsym_table 0 x)) xs;
    print_endline "--";
    *)
    (*
      This code RESCANS the result of the special inliner.
      The special inliner only handles function applications,
      this code should NOT handle them because iteration might
      lead to infinite recurse ..??

      This means the 'special cases' handled must be
      disjoint.

      Unfortunately, when inlining a function, we first
      inline into the function, then dump the result and
      rescan it. Consequently the recursion stop applied
      which leaves a direct non-tail self call will be
      rescanned here, and the function will be unfolded
      again .. in that process we also redo the special
      inlining .. infinite recursion. This is stopped
      by the flag which prevents inlining into a function
      more than once .. but that doesn't work if the
      function is cloned.
    *)
    List.iter (fun exe ->
    match exe with
    | BEXE_call (sr,(BEXPR_closure(callee,ts),clt),argument)
    (*
    | BEXE_call_direct (sr,callee,ts,argument)
    *)
      when not (mem callee excludes)
      ->
      let can_inline,callee,ts = virtual_check syms bsym_table sr callee ts in
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      (*
      print_endline ("CALL DIRECT " ^ id ^ "<"^ si callee^">");
      *)
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,(ps,traint),BTYP_void,exes) ->
        if can_inline && inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if syms.compiler_options.print_flag then
          print_endline ("inlining direct call: " ^ string_of_bexe bsym_table 0 exe);
          let revariable,xs = heavy_inline_call
            syms
            uses bsym_table
            caller
            caller_vs
            callee
            ts
            argument
            (Flx_bsym.id bsym)
            sr
            (props,vs,(ps,traint),exes)
          in
          let xs = hic revariable callee xs in
          exes' := rev xs @ !exes'
        end
        else
          exes' := exe :: !exes'

      | _ ->  exes' := exe :: !exes'
      end

    | BEXE_call (sr,(BEXPR_apply_stack (callee,ts,a),_),argument)
    | BEXE_call (sr,(BEXPR_apply_prim (callee,ts,a),_),argument)
    | BEXE_call (sr,(BEXPR_apply_direct (callee,ts,a),_),argument)
      -> assert false

    | BEXE_call (sr,(BEXPR_apply((BEXPR_closure (callee,ts),_),a),_),argument)
      when not (mem callee excludes)
      ->
      (*
      print_endline "DETECTED CANDIDATE FOR CALL LIFTING ";
      print_endline ("In procedure " ^ si caller ^ " with vs=" ^ string_of_vs caller_vs);
      *)
      (*
      print_endline ("handling call lift: " ^ string_of_bexe bsym_table 0 exe);
      print_endline ("Callee is " ^ si callee ^ " with ts = " ^ catmap "," (sbt bsym_table) ts);
      *)
      let can_inline,callee,ts = virtual_check syms bsym_table sr callee ts in
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
        if can_inline && inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if syms.compiler_options.print_flag then
          print_endline ("Inline call lift: " ^ string_of_bexe bsym_table 0 exe);
          let revariable,xs =
            call_lifting syms uses bsym_table caller caller_vs callee ts a argument
          in
          let xs = hic revariable callee xs in
          exes' := rev xs @ !exes'
        end else
          exes' := exe :: !exes'
      | _ -> exes' := exe :: !exes'
      end

    | BEXE_init (sr,i,(BEXPR_apply_stack (callee,ts,a),_))
    | BEXE_init (sr,i,(BEXPR_apply_prim (callee,ts,a),_))
    | BEXE_init (sr,i,(BEXPR_apply_direct (callee,ts,a),_))
      -> assert false

    | BEXE_init (sr,i,(BEXPR_apply ((BEXPR_closure(callee,ts),_),a),_))
      when not (mem callee excludes)  ->
      let can_inline,callee,ts = virtual_check syms bsym_table sr callee ts in
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
        if can_inline && inline_check syms bsym_table uses sr caller callee props exes then
          begin
            let bsymv = Flx_bsym_table.find bsym_table i in
            begin match Flx_bsym.bbdcl bsymv with
            | BBDCL_val (vs,t,`Tmp) ->
                (* Downgrading temporary *)
                (* should this be a VAR or a VAL? *)
                Flx_bsym_table.update_bbdcl
                  bsym_table
                  i
                  (bbdcl_val (vs,t,`Var))
            | _ -> ()
            end;
            if syms.compiler_options.print_flag then
            print_endline ("Inline init: " ^ string_of_bexe bsym_table 0 exe);
            let revariable,xs =
              inline_function syms uses bsym_table caller caller_vs callee ts a i
            in
            let xs = hic revariable callee xs in
            exes' := rev xs @ !exes'
          end
        else
          exes' := exe :: !exes'
      | _ -> exes' := exe :: !exes'
      end

    | BEXE_fun_return (sr,(BEXPR_apply_direct (callee,ts,a),_))
    | BEXE_fun_return (sr,(BEXPR_apply_stack (callee,ts,a),_))
    | BEXE_fun_return (sr,(BEXPR_apply_prim (callee,ts,a),_))
     -> assert false

    | BEXE_fun_return (sr,(BEXPR_apply((BEXPR_closure(callee,ts),_),a),_))
      when not (mem callee excludes)  ->
      let can_inline,callee,ts = virtual_check syms bsym_table sr callee ts in
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
        if can_inline && inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if inlining_complete bsym_table callee then
          begin
            if syms.compiler_options.print_flag then
            print_endline ("Inline tail apply : " ^ string_of_bexe bsym_table 0 exe);
            let revariable,xs =
              inline_tail_apply syms uses bsym_table caller caller_vs callee ts a
            in
            let xs = hic revariable callee xs in
            exes' := rev xs @ !exes'
          end else
            exes' := exe :: !exes'
        end else
          exes' := exe :: !exes'
      | _ ->
        exes' := exe :: !exes'
      end
    | _ -> exes' := exe :: !exes'
    )
    xs
  )
  exes
  ;
  rev !exes'

and remove_unused_children syms uses bsym_table i =
  let desc = Flx_bsym_table.find_descendants bsym_table i in
  if desc <> BidSet.empty then begin
    (* all the descendants of a routine, excluding self *)
    (*
    print_endline "CANDIDATE FOR CHILD REMOVAL";
    print_function bsym_table i;
    print_endline ("Descendants of " ^ si i ^ " =" ^ BidSet.fold (fun j s -> s ^ " " ^ si j) desc "");
    BidSet.iter (fun i-> print_function bsym_table i) desc;
    *)


    (* everything used by this routine directly or indirectly *)
    let used = Flx_call.use_closure uses i in

    (*
    print_endline ("Usage closure of " ^ si i ^ " =" ^ BidSet.fold (fun j s -> s ^ " " ^ si j) used "");
    *)
    (* any desendants not used by this routine *)
    let unused_descendants = BidSet.diff desc used in

    (* remove the item *)
    BidSet.iter
    (fun i ->
      (* remove from symbol table, child map, and usage map *)
      Flx_bsym_table.remove bsym_table i;
      Hashtbl.remove uses i;
      if syms.compiler_options.print_flag then
        print_endline ("REMOVED CHILD SYMBOL " ^
          qualified_name_of_bindex bsym_table i ^ "=" ^ string_of_int i)
    )
    unused_descendants
  end

and heavily_inline_bbdcl syms uses bsym_table excludes i =
  let bsym =
    try Some (Flx_bsym_table.find bsym_table i)
    with Not_found -> None
  in
  match bsym with None -> () | Some bsym ->
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,(ps,traint),ret,exes) ->
    if not (mem `Inlining_started props) then begin
      let props = `Inlining_started :: props in
      let bbdcl = bbdcl_fun (props,vs,(ps,traint),ret,exes) in
      Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
      (* inline into all children first *)
      let children = Flx_bsym_table.find_children bsym_table i in
      BidSet.iter (fun i-> heavily_inline_bbdcl syms uses bsym_table excludes i) children;
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
      let xcls = Flx_tailit.exes_get_xclosures syms exes in
      BidSet.iter (fun i-> heavily_inline_bbdcl syms uses bsym_table excludes i) xcls;

      if syms.compiler_options.print_flag then
      print_endline ("HIB:Examining function " ^ Flx_bsym.id bsym ^ "<" ^
        string_of_bid i ^ "> for inlinable calls");
      
      let exes = List.map Flx_bexe.reduce exes in (* term reduction *)
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = fold_vars syms bsym_table uses i ps exes in
      let exes = check_reductions syms bsym_table exes in (* user reductions *)

      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = heavy_inline_calls
        syms
        bsym_table
        uses
        vs
        i
        excludes
        exes
      in
      
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = Flx_tailit.tailit
        syms
        bsym_table
        uses
        (Flx_bsym.id bsym)
        i
        (Flx_bsym.sr bsym)
        ps
        vs
        exes
      in
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
      let exes = List.map Flx_bexe.reduce exes in (* term reduction *)
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = fold_vars syms bsym_table uses i ps exes in
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      let exes = check_reductions syms bsym_table exes in (* user reductions *)
      let exes = Flx_cflow.chain_gotos syms exes in
      let exes = List.map Flx_bexe.reduce exes in
      let props = `Inlining_complete :: props in
      let bbdcl = bbdcl_fun (props,vs,(ps,traint),ret,exes) in
      Flx_bsym_table.update_bbdcl bsym_table i bbdcl;
      recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
      remove_unused_children syms uses bsym_table i;
    end
  | _ -> ()

let heavy_inlining syms bsym_table =
  let used = ref (!(syms.roots)) in
  let (uses,usedby) = Flx_call.call_data bsym_table in

  while not (BidSet.is_empty !used) do
    let i = BidSet.choose !used in
    used := BidSet.remove i !used;
    heavily_inline_bbdcl syms uses bsym_table [i] i
  done;

  (* This code is here to attempt to optimise closures (and clones?)
     which aren't handled by the above loop.
  *)
  Flx_bsym_table.iter begin fun bid _ _ ->
    try heavily_inline_bbdcl syms uses bsym_table [bid] bid
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
