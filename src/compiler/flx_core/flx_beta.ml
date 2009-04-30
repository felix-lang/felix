(* Meta typing and beta reduction *)

open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2

open Flx_print
open Flx_exceptions
open Flx_typing
open List
open Flx_srcref
open Flx_unify
open Flx_maps

exception BTfound of btypecode_t

let rec metatype syms sr term =
  (*
  print_endline ("Find Metatype  of: " ^ string_of_btypecode syms.dfns term);
  *)
  let t = metatype' syms sr term in
  (*
  print_endline ("Metatype  of: " ^ string_of_btypecode syms.dfns term ^ " is " ^ sbt syms.dfns t);
  print_endline "Done";
  *)
  t

and metatype' syms sr term =
  let st t = string_of_btypecode syms.dfns t in
  let mt t = metatype' syms sr t in
  match term with
  | `BTYP_lift t -> t

  | `BTYP_typefun (a,b,c) ->
    let ps = List.map snd a in
    let argt =
      match ps with
      | [x] -> x
      | _ -> `BTYP_tuple ps
    in
      let rt = metatype syms sr c in
      if b<>rt
      then
        clierr sr
        (
          "In abstraction\n" ^
          st term ^
          "\nFunction body metatype \n"^
          st rt^
          "\ndoesn't agree with declared type \n" ^
          st b
        )
      else `BTYP_function (argt,b)

  | `BTYP_type_tuple ts ->
    `BTYP_tuple (map mt ts)

  | `BTYP_apply (a,b) ->
    begin
      let ta = mt a
      and tb = mt b
      in match ta with
      | `BTYP_function (x,y) ->
        if x = tb then y
        else
          clierr sr (
            "Metatype error: function argument wrong metatype, expected:\n" ^
            sbt syms.dfns x ^
            "\nbut got:\n" ^
            sbt syms.dfns tb
          )

      | _ -> clierr sr
        (
          "Metatype error: function required for LHS of application:\n"^
          sbt syms.dfns term ^
          ", got metatype:\n" ^
          sbt syms.dfns ta
        )
    end
  | `BTYP_var (i,mt) ->
    (*
    print_endline ("Type variable " ^ si i^ " has encoded meta type " ^ sbt syms.dfns mt);
    (
      try
        let symdef = Hashtbl.find syms.dfns i in begin match symdef with
        | {symdef=`SYMDEF_typevar mt} -> print_endline ("Table shows metatype is " ^ string_of_typecode mt);
        | _ -> print_endline "Type variable isn't a type variable?"
        end
      with Not_found -> print_endline "Cannot find type variable in symbol table"
    );
    *)
    mt

  | `BTYP_type i -> `BTYP_type (i+1)
  | `BTYP_inst (index,ts) ->
    let {id=id; symdef=entry} =
      try Hashtbl.find syms.dfns index
      with Not_found -> failwith ("[metatype'] can't find type instance index " ^ si index)
    in
    (*
    print_endline ("Yup .. instance id=" ^ id);
    *)

    (* this is hacked: we should really bind the types and take
      the metatype of them but we don't have access to the
      bind type routine due to module factoring .. we could pass
      in the bind-type routine as an argument .. yuck ..
    *)
    begin match entry with
    | `SYMDEF_nonconst_ctor (_,ut,_,_,argt) ->
      `BTYP_function (`BTYP_type 0,`BTYP_type 0)

    | `SYMDEF_const_ctor (_,t,_,_) ->
      `BTYP_type 0

    | `SYMDEF_abs _ -> `BTYP_type 0

    | _ ->  clierr sr ("Unexpected argument to metatype: " ^ sbt syms.dfns term)
    end


  | _ ->
     print_endline ("Questionable meta typing of term: " ^ sbt syms.dfns term);
     `BTYP_type 0 (* THIS ISN'T RIGHT *)



(* fixpoint reduction: reduce
   Fix f. Lam x. e ==> Lam x. Fix z. e [f x -> z]
   to replace a recursive function
   with a recursive data structure.

   Example: consider:

   list t = t * list t

   which is

   list = fix f. lam t. t * f t

   We can apply list to int:

   list int = (fix f. lam t. t * f t) int

   unfolding:

   list int = (lam t. t * (fix f. lam t. t * f t)) int
            = int * (fix f. lam t. t * f t) int
            = int * list int

   which is just

   list int = fix z. int * z

  The rule ONLY works when a recursive function f
  is applied in its own definition to its own parameter.

  The rule traps in infinite expansion of a data type,
  and creates instead an recursive data type, eliminating
  the function.

  The normal beta reduction rule is

  (lam t. b) a => b [t->a]

  For a recursive function:

  (fix f. lam t. b) a => b[f-> fix f. lam t. b; t-> a]

  and the result must be reduced again.

*)

and fixup syms ps body =
 let param = match ps with
   | [] -> assert false
   | [i,mt] -> `BTYP_var (i,mt)
   | x -> `BTYP_type_tuple (List.map (fun (i,mt) -> `BTYP_var (i,mt)) x)
 in
 (*
 print_endline ("Body  = " ^ sbt syms.dfns body);
 print_endline ("Param = " ^ sbt syms.dfns param);
 *)
 let rec aux term depth =
   let fx t = aux t (depth+1) in
   match map_btype fx term with
   | `BTYP_apply (`BTYP_fix i, arg)
     when arg = param
     && i + depth +1  = 0 (* looking inside application, one more level *)
     -> print_endline "SPECIAL REDUCTION";
     `BTYP_fix (i+2) (* elide application AND skip under lambda abstraction *)

   | `BTYP_typefun (a,b,c) ->
      (* NOTE we have to add 2 to depth here, an extra
      level for the lambda binder.
      NOTE also: this is NOT a recusive call to fixup!
      It doesn't fixup this function.
      *)

      (*
      print_endline "OOPS >> no alpha conversion?";
      *)

      `BTYP_typefun (a, fx b, aux c (depth + 2))
   | x -> x
 in
   (* note depth 1: we seek a fix to an abstraction
   of which we're given only the body, that's an
   extra level in the term structure
   *)
   aux body 1

(* generic fixpoint adjuster: add amt to the fixpoint
   to make it span less deep term, to compensate
   for removing the top combinator of the term as a result
   of a one level adjustment eg: reduce a type match
*)

and adjust t =
  let rec adj depth t =
    let fx t = adj (depth + 1) t in
    match map_btype fx t with
    | `BTYP_fix i when i + depth < 0 -> `BTYP_fix (i+1)
    | x -> x
  in adj 0 t

and mk_prim_type_inst syms i args =
  print_endline "MK_PRIM_TYPE";
  let t = `BTYP_inst (i,args) in
  (*
  let _,t' = normalise_type t in
  let args = match t' with
    | `BTYP_inst (_,args) -> args
    | _ -> assert false
  in
  if not (Hashtbl.mem syms.prim_inst (i,args))
  then begin
    let n = !(syms.counter) in
    incr (syms.counter);
    Hashtbl.add syms.prim_inst (i, args) n;
    Hashtbl.add syms.rev_prim_inst n (i, args)
  end;
  *)
  t


and beta_reduce syms sr t1 =
  (*
  print_endline ("---------- Beta reduce " ^ sbt syms.dfns t1);
  *)
  let t2 =
  try
  beta_reduce' syms sr [] t1
  with
    | Not_found -> failwith  ("Beta reduce failed with Not_found in " ^ sbt syms.dfns t1)
    | Failure s -> failwith ("beta-reduce failed in " ^ sbt syms.dfns t1 ^ "\nmsg: " ^ s)
  in
  (*
  print_endline ("============  reduced= " ^ sbt syms.dfns t2);
  *)
  t2

and type_list_index syms ls t =
  (*
  print_endline ("Comparing : " ^ sbt syms.dfns t ^ " with ..");
  *)
  let rec aux ls n = match ls with
  | [] -> None
  | hd :: tl ->
    (*
    print_endline ("Candidate : " ^ sbt syms.dfns hd);
    *)
    if
      begin try type_eq syms.counter syms.dfns hd t
      with x ->
        print_endline ("Exception: " ^ Printexc.to_string x);
        false
      end
    then Some n
    else aux tl (n+1)
  in aux ls 0

and beta_reduce' syms sr termlist t =
  (*
  print_endline ("BETA REDUCE " ^ sbt syms.dfns t ^ "\ntrail length = " ^ si (length termlist));
  *)
  if length termlist > 20
  then begin
    print_endline ("Trail=" ^ catmap "\n" (sbt syms.dfns) termlist);
    failwith  ("Trail overflow, infinite expansion: BETA REDUCE " ^
    sbt syms.dfns t ^ "\ntrail length = " ^ si (length termlist))
  end;

  match type_list_index syms termlist t with
  | Some j ->
        (*
        print_endline "+++Trail:";
        let i = ref 0 in
        iter (fun t -> print_endline (
          "    " ^ si (!i) ^ " ---> " ^sbt syms.dfns t)
          ; decr i
        )
        (t::termlist)
        ;
        print_endline "++++End";
    print_endline ("Beta find fixpoint " ^ si (-j-1));
    print_endline ("Repeated term " ^ sbt syms.dfns t);
    *)
    `BTYP_fix (-j - 1)

  | None ->

  let br t' = beta_reduce' syms sr (t::termlist) t' in
  let st t = string_of_btypecode syms.dfns t in
  let mt t = metatype syms sr t in
  match t with
  | `BTYP_lift t -> `BTYP_lift (br t)
  | `BTYP_fix _ -> t
  | `BTYP_var (i,_) -> t

  | `BTYP_typefun (p,r,b) -> t
  (*
    let b = fixup syms p b in
    let b' = beta_reduce' syms sr (t::termlist) b in
    let t = `BTYP_typefun (p, br r, b') in
    t
  *)

  | `BTYP_inst (i,ts) ->
    let ts = map br ts in
    begin try match Hashtbl.find syms.dfns i with
    | {id=id; symdef=`SYMDEF_type_alias _ } ->
      failwith ("Beta reduce found a type instance of "^id^" to be an alias, which it can't handle")
    | _ -> `BTYP_inst (i,ts)
    with Not_found -> `BTYP_inst (i,ts) (* could be reparented class *)
    end

  | `BTYP_tuple ls -> `BTYP_tuple (map br ls)
  | `BTYP_array (i,t) -> `BTYP_array (i, br t)
  | `BTYP_sum ls -> `BTYP_sum (map br ls)
  | `BTYP_record ts ->
     let ss,ls = split ts in
     `BTYP_record (combine ss (map br ls))

  | `BTYP_variant ts ->
     let ss,ls = split ts in
     `BTYP_variant (combine ss (map br ls))

  (* Intersection type reduction rule: if any term is 0,
     the result is 0, otherwise the result is the intersection
     of the reduced terms with 1 terms removed: if there
     are no terms return 1, if a single term return it,
     otherwise return the intersection of non units
     (at least two)
  *)
  | `BTYP_intersect ls ->
    let ls = map br ls in
    if mem `BTYP_void ls then `BTYP_void
    else let ls = filter (fun i -> i <> `BTYP_tuple []) ls in
    begin match ls with
    | [] -> `BTYP_tuple []
    | [t] -> t
    | ls -> `BTYP_intersect ls
    end

  | `BTYP_typeset ls -> `BTYP_typeset (map br ls)

  | `BTYP_typesetunion ls ->
    let ls = rev_map br ls in
    (* split into explicit typesets and other terms
      at the moment, there shouldn't be any 'other'
      terms (since there are no typeset variables ..
    *)
    let rec aux ts ot ls  = match ls with
    | [] ->
      begin match ot with
      | [] -> `BTYP_typeset ts
      | _ ->
        (*
        print_endline "WARNING UNREDUCED TYPESET UNION";
        *)
        `BTYP_typesetunion (`BTYP_typeset ts :: ot)
      end

    | `BTYP_typeset xs :: t -> aux (xs @ ts) ot t
    | h :: t -> aux ts (h :: ot) t
    in aux [] [] ls

  (* NOTE: sets have no unique unit *)
  (* WARNING: this representation is dangerous:
     we can only calculate the real intersection
     of discrete types *without type variables*

     If there are pattern variables, we may be able
     to apply unification as a reduction. However
     we have to be very careful doing that: we can't
     unify variables bound by universal or lambda quantifiers
     or the environment: technically I think we can only
     unify existentials. For example the intersection

     'a * int & long & 'b

     may seem to be long * int, but only if 'a and 'b are
     pattern variables, i.e. dependent variables we're allowed
     to assign. If they're actually function parameters, or
     just names for types in the environment, we have to stop
     the unification algorithm from assigning them (since they're
     actually particular constants at that point).

     but the beta-reduction can be applied anywhere .. so I'm
     not at all confident of the right reduction rule yet.

     Bottom line: the rule below is a hack.
  *)
  | `BTYP_typesetintersection ls ->
    let ls = map br ls in
    if mem (`BTYP_typeset []) ls then `BTYP_typeset []
    else begin match ls with
    | [t] -> t
    | ls -> `BTYP_typesetintersection ls
    end


  | `BTYP_type_tuple ls -> `BTYP_type_tuple (map br ls)
  | `BTYP_function (a,b) -> `BTYP_function (br a, br b)
  | `BTYP_cfunction (a,b) -> `BTYP_cfunction (br a, br b)
  | `BTYP_pointer a -> `BTYP_pointer (br a)
(*  | `BTYP_lvalue a -> `BTYP_lvalue (br a) *)

  | `BTYP_void -> t
  | `BTYP_type _ -> t
  | `BTYP_unitsum _ -> t

  | `BTYP_apply (t1,t2) ->
    let t1 = br t1 in (* eager evaluation *)
    let t2 = br t2 in (* eager evaluation *)
    let t1 =
      match t1 with
      | `BTYP_fix j ->
        (*
        print_endline ("++++Fixpoint application " ^ si j);
        print_endline "+++Trail:";
        let i = ref 0 in
        iter (fun t -> print_endline (
          "    " ^ si (!i) ^ " ---> " ^sbt syms.dfns t)
          ; decr i
        )
        (t1::t::termlist)
        ;
        print_endline "++++End";
        *)
        let whole = nth termlist (-2-j) in
        (*
        print_endline ("Recfun = " ^ sbt syms.dfns whole);
        *)
        begin match whole with
        | `BTYP_typefun _ -> ()
        | _ -> assert false
        end;
        whole

      | _ -> t1
    in
    (*
    print_endline ("Function = " ^ sbt syms.dfns t1);
    print_endline ("Argument = " ^ sbt syms.dfns t2);
    print_endline ("Unfolded = " ^ sbt syms.dfns (unfold syms.dfns t1));
    *)
    begin match unfold syms.dfns t1 with
    | `BTYP_typefun (ps,r,body) ->
      let params' =
        match ps with
        | [] -> []
        | [i,_] -> [i,t2]
        | _ ->
          let ts = match t2 with
          | `BTYP_type_tuple ts -> ts
          | _ -> assert false
          in
            if List.length ps <> List.length ts
            then failwith "Wrong number of arguments to typefun"
            else List.map2 (fun (i,_) t -> i, t) ps ts
      in
      (*
      print_endline ("Body before subs    = " ^ sbt syms.dfns body);
      print_endline ("Parameters= " ^ catmap "," (fun (i,t) -> "T"^si i ^ "=>" ^ sbt syms.dfns t) params');
      *)
      let t' = list_subst syms.counter params' body in
      (*
      print_endline ("Body after subs     = " ^ sbt syms.dfns t');
      *)
      let t' = beta_reduce' syms sr (t::termlist) t' in
      (*
      print_endline ("Body after reduction = " ^ sbt syms.dfns t');
      *)
      let t' = adjust t' in
      t'

    | _ ->
      (*
      print_endline "Apply nonfunction .. can't reduce";
      *)
      `BTYP_apply (t1,t2)
    end

  | `BTYP_type_match (tt,pts) ->
    (*
    print_endline ("Typematch [before reduction] " ^ sbt syms.dfns t);
    *)
    let tt = br tt in
    let new_matches = ref [] in
    iter (fun ({pattern=p; pattern_vars=dvars; assignments=eqns}, t') ->
      (*
      print_endline (spc ^"Tring to unify argument with " ^ sbt syms.dfns p');
      *)
      let p =  br p in
      let x =
        {
          pattern=p;
          assignments= map (fun (j,t) -> j, br t) eqns;
          pattern_vars=dvars;
        }, t'
      in
      match maybe_unification syms.counter syms.dfns [p,tt] with
      | Some _ -> new_matches := x :: !new_matches
      | None ->
        (*
        print_endline (spc ^"Discarding pattern " ^ sbt syms.dfns p');
        *)
        ()
    )
    pts
    ;
    let pts = rev !new_matches in
    match pts with
    | [] ->
      failwith "[beta-reduce] typematch failure"
    | ({pattern=p';pattern_vars=dvars;assignments=eqns},t') :: _ ->
      try
        let mgu = unification false syms.counter syms.dfns [p', tt] dvars in
        (*
        print_endline "Typematch success";
        *)
        let t' = list_subst syms.counter (mgu @ eqns) t' in
        let t' = br t' in
        (*
        print_endline ("type match reduction result=" ^ sbt syms.dfns t');
        *)
        adjust t'
      with Not_found -> `BTYP_type_match (tt,pts)
