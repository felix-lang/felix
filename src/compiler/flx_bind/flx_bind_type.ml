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
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid

let debug = false 

let rec expand_typeset t =
  match t with
  | BTYP_type_tuple ls
  | BTYP_type_set ls
  | BTYP_type_set_union ls -> List.fold_left (fun ls t -> expand_typeset t @ ls) [] ls
  | x -> [x]

let handle_typeset state sr elt tset =
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
      pattern = btyp_type_var (fresh,Flx_kind.KIND_type);
      pattern_vars = BidSet.singleton fresh;
      assignments=[]
    },
    btyp_void ()
  in
  let lss = List.rev (dflt :: lss) in
  btyp_type_match (elt, lss)


let rec bind_type'
  bind_type_index
  bind_expression'
  lookup_type_qn_with_sig'
  lookup_qn_in_env'
  lookup_qn_with_sig'
  state
  bsym_table
  env
  rs
  sr t params
  mkenv
=
  let bind_type' env rs sr t params mkenv =
bind_type'
  bind_type_index
  bind_expression'
  lookup_type_qn_with_sig'
  lookup_qn_in_env'
  lookup_qn_with_sig'
  state
  bsym_table
  env
  rs
  sr t params
  mkenv
in
 
if debug then
print_endline ("Bind type " ^ string_of_typecode t ^ " params = " ^
  catmap "," (fun (s,t) -> s ^ "->" ^ Flx_btype.st t) params
);
  let btp t params = bind_type' 
    env
    {rs with depth = rs.depth+1}
    sr t params mkenv
  in
  let bt t = btp t params in
  let bi i ts = bind_type_index state bsym_table rs sr i ts mkenv in
  let bisub i ts = bind_type_index state bsym_table {rs with depth= rs.depth+1} sr i ts mkenv in
  let br t = Flx_beta.beta_reduce "flx_lookup: bind_type'" state.counter bsym_table sr t in

  let t =
  match t with
  | TYP_pclt (d,c) -> btyp_cltpointer (bt d) (bt c)
  | TYP_defer (sr, tor) -> 
    begin match !tor with
    | None -> print_endline ("Bind type: undefined defered type found"); assert false
    | Some t -> bt t
    end

  | TYP_rptsum (count, base) ->
    let n = bt count in
    let b = bt base in
    btyp_rptsum (n,b)

  | TYP_label -> btyp_label ()
  | TYP_patvar _ -> failwith "Not implemented patvar in typecode"
  | TYP_patany _ -> failwith "Not implemented patany in typecode"

  | TYP_intersect ts -> btyp_intersect (List.map bt ts)
  | TYP_union ts -> btyp_union (List.map bt ts)
  | TYP_record ts -> btyp_record (List.map (fun (s,t) -> s,bt t) ts)
  | TYP_polyrecord (ts,v) -> btyp_polyrecord (List.map (fun (s,t) -> s,bt t) ts) (bt v)
  | TYP_variant ts -> 
(*
print_endline ("\n******\nTrying to bind variant " ^ string_of_typecode t);
*)
     let flds = List.fold_left  (fun acc x ->  
       match x with 
       | `Ctor (s,t) -> 
(*
          print_endline ("Trying to bind constructor " ^ s ^ " argument type " ^ string_of_typecode t);
*)
          let t = bt t in
(*
          print_endline ("Bound bind constructor " ^ s ^ " argument type " ^ Flx_btype.st t);
*)
          `Ctor (s, t) :: acc
       | `Base t ->
(*
          print_endline ("Trying to bind base type " ^ string_of_typecode t);
*)
          let t = bt t in
(*
          print_endline ("Bound base type " ^ Flx_btype.st t);
*)
          `Base t :: acc
       )
       []
       ts 
     in 
(*
print_endline("Bound variant components = " ^ catmap "," (fun (s,t) -> "`" ^ s ^ " of " ^ Flx_btype.st t) flds);
*)
     let t = btyp_polyvariant flds in
(*
print_endline ("Bound variant = " ^ Flx_btype.st t);
*)
     t

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
        | BTYP_inst (i,ts,_) -> (* should only happen during typedef binding in nominal type mode *)
          begin try
            let bsym = Flx_bsym_table.find bsym_table i in
            let bbdcl = Flx_bsym.bbdcl bsym in
            match bbdcl with
            | BBDCL_nominal_type_alias (bvs,t)
            | BBDCL_structural_type_alias (bvs,t) -> 
              let t = Flx_btype_subst.tsubst sr bvs ts t in
              begin match t with
              | BTYP_record (fields) -> 
                new_fields := List.rev fields @ (!new_fields)
              | _ -> clierr sr ("Flx_bind_type.record extension: expected typedef of record type, got " ^
                   Flx_btype.st t
                )
              end
            | _ -> clierr sr ("Flx_bind_type.record extension: expected nominal type to be typedef, got" ^
               Flx_print.string_of_bbdcl bsym_table bbdcl i) 
          with _ ->
            clierr sr ("Flx_bind_type.record extension: can't find  nominal type, expected typedef .." ^ string_of_int i)
          end
          
        | _ -> clierrx "[flx_bind/flx_lookup.ml:802: E93] " sr ("Record extension requires bases be records too, got " ^ 
          Flx_btype.st t ^ "=" ^
          sbt bsym_table t)
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

  | TYP_subtype_match (t,ps) as ubt ->
    let t = bt t in
    bind_subtype_match bsym_table state.counter bt btp params sr t ps ubt 


  | TYP_dual t -> Flx_btype_dual.dual (bt t)

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
if i = 7141 then
print_endline ("Flx_bind_type TYP_var " ^ string_of_int i);
    begin try 
      let sym = Flx_sym_table.find state.sym_table i in
      match sym.Flx_sym.symdef with
      | SYMDEF_typevar mt -> 
        let k = Flx_btype.bmt "Flx_bind_type.TYP_var" mt in
        btyp_type_var (i, k)
      | _ -> raise Not_found 
    with Not_found ->
      (* HACK .. assume variable is type TYPE *)
(*
print_endline ("FUDGE: Binding TYP_var " ^ si i ^ ", HACKING KIND TO TYPE");
*)
      btyp_type_var (i, Flx_kind.KIND_type)
    end

  | TYP_as (t,s) ->
(*
print_endline ("\n\n+++++++++\nTrying to bind recursive type " ^ string_of_typecode t ^ " AS " ^ s);
*)
    let t = bind_type'
      env
      { rs with as_fixlist = (s,rs.depth)::rs.as_fixlist }
      sr
      t
      params
      mkenv
    in
(*
print_endline ("\n+++++++++Bound recursive type is " ^ Flx_btype.st t^"\n\n");
*)
    t

  | TYP_typeof e ->
    if get_structural_typedefs state then begin
      if debug then
      print_endline ("Flx_bind_type.TYP_typeof(" ^ string_of_expr e ^ ")");
      if List.mem_assq e rs.expr_fixlist
      then begin
        (* Typeof is recursive *)
        let outer_depth = List.assq e rs.expr_fixlist in
        let fixdepth = outer_depth -rs.depth in
(* HACK metatype guess *)
        btyp_fix fixdepth (Flx_kind.KIND_type)
      end else begin
        if debug then
        print_endline ("Flx_bind_type.TYP_typeof.Start tentative binding of typeof (" ^ string_of_expr e ^ ")");
        let t = snd (bind_expression' state bsym_table env rs e []) in
        if debug then
        print_endline ("Flx_bind_type.TYP_typeof.end tentative binding of typeof (" ^string_of_expr e^ ")");
        t
      end
    end else begin
      match env with
      | (parent,_,_,_,_)::_ ->
        btyp_typeof (parent, e)
      | [] -> 
        btyp_typeof (0,e)
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
  | TYP_tuple_snoc (_,t1,t2) -> btyp_tuple_snoc (bt t1) (bt t2)
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
  | TYP_rref t -> btyp_rref (bt t)
  | TYP_wref t -> btyp_wref (bt t)
  | TYP_uniq t -> btyp_uniq (bt t)

  | TYP_void _ -> btyp_void ()

  | TYP_typefun (ps,r,body) ->
(*
print_endline ("Binding type function " ^ Flx_print.string_of_typecode t);
*)
      let data = List.rev_map
        (fun (name, mt) -> name, bmt "Flx_bind_type.1" mt, fresh_bid state.counter)
        ps
      in
      (* reverse order .. *)
      let pnames = List.map
        (fun (n, mt, i) -> 
(*
           print_endline ("Flx_bind_type: Type function, parameter " ^ 
           si i ^ ",kind=" ^ Flx_kind.sk mt); 
*)
           (n, btyp_type_var (i, mt)))
        data
      in
      let bbody =
        bind_type'
          env
          { rs with depth=rs.depth + 1 }
          sr
          body
          (pnames@params)
          mkenv
      in
      (* order as written *)
      let bparams = List.rev_map (fun (n, t, i) -> (i, t)) data in

      btyp_type_function (bparams, bmt "Flx_bind_type.2" r, bbody)

  | TYP_apply (TYP_name (_,"_rev",[]),t2) ->
    let t2 = bt t2 in
    begin match t2 with
    | BTYP_tuple ts -> btyp_tuple (List.rev ts)
    | _ -> btyp_rev t2
    end

  | TYP_apply (TYP_apply (TYP_name (_,"_map",[]), funct), t2) ->
    let bfunct = bt funct in
    let bt2 = bt t2 in
(*
print_endline ("type _map functor = " ^ sbt bsym_table bfunct);
print_endline ("type _map datatype = " ^ sbt bsym_table bt2);
*)
    btyp_type_map (bfunct, bt2)

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
  | TYP_apply (t1,t2) -> 
(*
print_endline ("Binding TYP_apply " ^ string_of_typecode t);
*)
    let x = btyp_type_apply (bt t1, bt t2) in
(*
print_endline ("  ***** Bound TYP_apply: " ^ Flx_btype.st x );
*)
    x

  | TYP_type_tuple ts -> btyp_type_tuple (List.map bt ts)

  | TYP_name (sr,s,[]) when List.mem_assoc s rs.as_fixlist ->
(* HACK metatype guess *)
    btyp_fix ((List.assoc s rs.as_fixlist) - rs.depth) (Flx_kind.KIND_type)

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
            (fun (s,i,mt) -> btyp_type_var (i, Flx_btype.bmt "Flx_bind_type1" mt))
            (fst sym.Flx_sym.vs)
          in
          btyp_inst (index,ts,Flx_kind.KIND_type)
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
          string_of_bid entry_kind.base_sym);
        print_endline ("Kind=" ^ match t with | TYP_name (_,s,ts) -> "TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "TYP_*");
        print_endline ("spec_vs=" ^
          catmap ","
            (fun (s,j)-> s ^ "<" ^ string_of_bid j ^ ">")
            entry_kind.spec_vs);
        print_endline ("sub_ts=" ^
          catmap "," (sbt bsym_table) entry_kind.sub_ts);
end;
*)
      let ts = List.map bt ts in
(*
if string_of_qualified_name x = "digraph_t" then begin
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
end;
*)
      let baset = bi
        entry_kind.base_sym
        entry_kind.sub_ts
      in
(*
if string_of_qualified_name x = "digraph_t" then begin
      print_endline ("Base type bound with sub_ts replacing type variables " ^ sbt bsym_table baset);
end;
*)
      (* SHOULD BE CLIENT ERROR not assertion *)
      if List.length ts != List.length entry_kind.spec_vs then 
      begin
        print_endline ("bind_type': Type "^string_of_typecode t^"=Qualified name "^string_of_qualified_name x^" lookup finds index " ^
          string_of_bid entry_kind.base_sym);
        print_endline ("Kind=" ^ match t with | TYP_name (_,s,ts) -> "TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "TYP_*");
        print_endline ("spec_vs=" ^
          catmap ","
            (fun (s,j,mt)-> s ^ "<" ^ string_of_bid j ^ ">")
            entry_kind.spec_vs);
        print_endline ("spec_ts=" ^
          catmap "," (sbt bsym_table) entry_kind.sub_ts);
        print_endline ("input_ts=" ^ catmap "," (sbt bsym_table) ts);
        begin match
          hfind "lookup" state.sym_table entry_kind.base_sym
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
          si (List.length entry_kind.spec_vs) ^ ", but got " ^
          si (List.length ts))
      end;

      assert (List.length ts = List.length entry_kind.spec_vs);
      let t = tsubst sr entry_kind.spec_vs ts baset in
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


