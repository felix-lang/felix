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
open Flx_generic
open Flx_overload
open Flx_tpat
open Flx_lookup_state
open Flx_name_map
open Flx_btype_occurs
open Flx_btype_subst
open Flx_bid


(* NEW RULES. bind_type MAY NOT 
  lookup in the bound symbol table 
  beta reduce anything

  It is permitted ONLY to actually replace names with indices
  by lookup in the unbound symbol table, and,
  replace unbound terms with corresponding bound terms

  In particular, it is used to bind typedefs and type functions
  putting them in the bound symbol table, so that subsequent binding
  of non-type terms eg variables and functions, can find bound types
  in the bound symbol table: at THAT point only ALL the nominal types
  typedefs, and type functions must be in the bound symbol table.

  The types can then be beta-reduced.
*)

let debug = false 

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
  (lookup_qn_in_env': lookup_state_t -> 'a -> env_t -> 'b -> qualified_name_t -> entry_kind_t * typecode_t list)
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

  let t =
  match t with
  | `TYP_bool b -> bbool b
  | `TYP_typeop (sr,op,t,k) ->
    let t = bt t in
    let k = Flx_btype.bmt "Flx_bind_type.`TYP_var" k in
    btyp_typeop op t k

  | `TYP_pclt (d,c) -> 
(*
print_endline ("Binding pointer to clt: base " ^ string_of_typecode d ^ ", target " ^ string_of_typecode c);
    let d' = bt d in
    let c' = bt c in
print_endline (".. Bound base = " ^ sbt bsym_table d');
print_endline (".. Bound target = " ^ sbt bsym_table c');
*)
    let result = btyp_cltpointer (bt d) (bt c) in
(*
print_endline ("  .. result = " ^ sbt bsym_table result);
*)
    result

  | `TYP_rpclt (d,c) -> btyp_cltrref (bt d) (bt c)
  | `TYP_wpclt (d,c) -> btyp_cltwref (bt d) (bt c)

  | `TYP_rptsum (count, base) ->
    let n = bt count in
    let b = bt base in
    btyp_rptsum (n,b)


  | `TYP_compactrptsum (count, base) ->
    let n = bt count in
    let b = bt base in
    btyp_compactrptsum (n,b)

  | `TYP_label -> btyp_label ()
  | `TYP_patvar _ -> failwith "Not implemented patvar in typecode"
  | `TYP_patany _ -> failwith "Not implemented patany in typecode"

  | `TYP_record ts -> btyp_record (List.map (fun (s,t) -> s,bt t) ts)
  | `TYP_polyrecord (ts,s,v) -> btyp_polyrecord (List.map (fun (s,t) -> s,bt t) ts) s (bt v)
  | `TYP_variant ts -> 
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
     let t = btyp_polyvariant flds in
(*
print_endline ("[flx_bind_type] Bound polyvariant = " ^ Flx_btype.st t);
*)
     t

  | `TYP_type_extension (sr, ts, t') ->
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
        | BTYP_inst (_,i,ts,_) -> (* should only happen during typedef binding in nominal type mode *)
          begin try
            let bsym = Flx_bsym_table.find bsym_table i in
            let bbdcl = Flx_bsym.bbdcl bsym in
            match bbdcl with
            | BBDCL_type_alias (bvs,t) -> 
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
          
        | _ -> clierrx "[flx_bind_type.ml:247: E93] " sr ("Record extension requires bases be records too, got " ^ 
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
  | `TYP_type_match (t,ps) as ubt ->
    let t = bt t in
    bind_type_match bsym_table state.counter bt btp params sr t ps ubt 

  | `TYP_subtype_match (t,ps) as ubt ->
    let t = bt t in
    bind_subtype_match bsym_table state.counter bt btp params sr t ps ubt 


  | `TYP_dual t -> Flx_btype_dual.dual (bt t)

  | `TYP_ellipsis -> btyp_ellipsis
    (* failwith "Unexpected `TYP_ellipsis (...) in bind type" *)

  | `TYP_none ->
    failwith "Unexpected `TYP_none in bind type"

  | `TYP_typeset ts -> btyp_type_set (List.map bt ts)
  | `TYP_setunion ts -> btyp_type_set_union (List.map bt ts)
  | `TYP_setintersection ts -> btyp_type_set_intersection (List.map bt ts)
  | `TYP_isin (elt,tset) -> btyp_in (bt elt, bt tset)

  | `TYP_var i ->
(*
if i = 7141 then
print_endline ("Flx_bind_type `TYP_var " ^ string_of_int i);
*)
    begin try 
      let sym = Flx_sym_table.find state.sym_table i in
      match sym.Flx_sym.symdef with
      | SYMDEF_typevar mt -> 
        let k = Flx_btype.bmt "Flx_bind_type.`TYP_var" mt in
        btyp_type_var (i, k)
      | _ -> raise Not_found 
    with Not_found ->
      (* HACK .. assume variable is type TYPE *)
(*
print_endline ("FUDGE: Binding `TYP_var " ^ si i ^ ", HACKING KIND TO TYPE");
*)
      btyp_type_var (i, Flx_kind.KIND_var "AnyKind")
    end

  | `TYP_as (t,s,k) ->
(*
print_endline ("\n\n+++++++++\nTrying to bind recursive type " ^ string_of_typecode t ^ " AS " ^ s);
*)
    let k = Flx_btype.bmt "Flx_bind_type:TYP_as" k in
    let t = bind_type'
      env
      { rs with as_fixlist = (s,(rs.depth,k))::rs.as_fixlist }
      sr
      t
      params
      mkenv
    in
(*
print_endline ("\n+++++++++Bound recursive type is " ^ Flx_btype.st t^"\n\n");
*)
    t

  | `TYP_typeof e ->
      if List.mem_assq e rs.expr_fixlist
      then begin
        (* Typeof is recursive *)
        let outer_depth = List.assq e rs.expr_fixlist in
        let fixdepth = outer_depth -rs.depth in
(* HACK metatype guess : expressions generally ARE of kind TYPE, it just might be
an over-generalisation *)
(*
print_endline ("Flx_bind_type. structural mode: TYP_typeof fixpoint metatype hack! Expression " ^ string_of_expr e);
*)
        btyp_fix fixdepth (Flx_kind.KIND_type)
      end else begin
        let t = snd (bind_expression' state bsym_table env rs e []) in
        t
      end

  | `TYP_array (t1,t2)->
      let t1 = bt t1 in
      let t2 = bt t2 in
      if not (islinear_type t2) then 
        clierr sr ("Flx_bind_type.TYP_array] Array index must be compact linear, got " ^ sbt bsym_table t2);
      btyp_array (t1, t2)

  | `TYP_compactarray (t1,t2)->
      let t1 = bt t1 in
      let t2 = bt t2 in
      if not (islinear_type t2) then 
        clierr sr ("Flx_bind_type.TYP_compactarray] Compact Array index must be compact linear, got " ^ sbt bsym_table t2);
      if not (islinear_type t1) then 
        clierr sr ("Flx_bind_type.TYP_compactarray] Compact Array value type must be compact linear, got " ^ sbt bsym_table t1);
      btyp_compactarray (t1, t2)

  | `TYP_compacttuple ts -> btyp_compacttuple (List.map bt ts)
  | `TYP_tuple ts -> btyp_tuple (List.map bt ts)
  | `TYP_intersect ts -> btyp_intersect (List.map bt ts)
  | `TYP_union ts -> btyp_union (List.map bt ts)
  | `TYP_tuple_cons (_,t1,t2) -> btyp_tuple_cons (bt t1) (bt t2)
  | `TYP_tuple_snoc (_,t1,t2) -> btyp_tuple_snoc (bt t1) (bt t2)
  | `TYP_unitsum k ->
      begin match k with
      | 0 -> btyp_void ()
      | 1 -> btyp_tuple []
      | _ -> btyp_unitsum k
      end

  | `TYP_compactsum ts ->
      let ts' = List.map bt ts in
      if Flx_btype.all_units ts' then
        btyp_unitsum (List.length ts)
      else
        btyp_compactsum ts'

  | `TYP_sum ts ->
      let ts' = List.map bt ts in
      if Flx_btype.all_units ts' then
        btyp_unitsum (List.length ts)
      else
        btyp_sum ts'

  | `TYP_function (d,c) -> btyp_function (bt d, bt c)
  | `TYP_effector (d,e,c) -> btyp_effector (bt d, bt e,bt c)
  | `TYP_linearfunction (d,c) -> btyp_linearfunction (bt d, bt c)
  | `TYP_lineareffector (d,e,c) -> btyp_lineareffector (bt d, bt e,bt c)
  | `TYP_cfunction (d,c) -> btyp_cfunction (bt d, bt c)
  | `TYP_pointer t -> btyp_pointer (bt t)
  | `TYP_rref t -> btyp_rref (bt t)
  | `TYP_wref t -> btyp_wref (bt t)
  | `TYP_uniq t -> btyp_uniq (bt t)
  | `TYP_borrowed t -> btyp_borrowed (bt t)

  | `TYP_void _ -> btyp_void ()

  | `TYP_typefun (ps,r,body) ->
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

  | `TYP_apply (`TYP_name (_,"_rev",[]),t2) ->
    let t2 = bt t2 in
    begin match t2 with
    | BTYP_tuple ts -> btyp_tuple (List.rev ts)
    | _ -> btyp_rev t2
    end

  | `TYP_apply (`TYP_apply (`TYP_name (_,"_map",[]), funct), t2) ->
    let bfunct = bt funct in
    let bt2 = bt t2 in
(*
print_endline ("type _map functor = " ^ sbt bsym_table bfunct);
print_endline ("type _map datatype = " ^ sbt bsym_table bt2);
*)
    btyp_type_map (bfunct, bt2)

  | `TYP_apply (`TYP_name (_,"_flatten",[]),t2) ->
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
  | `TYP_apply (t1,t2) -> 
(*
print_endline ("Binding `TYP_apply " ^ string_of_typecode t);
*)
    let x = (* br *) (btyp_type_apply (bt t1, bt t2)) in
(*
print_endline ("  ***** Bound `TYP_apply: " ^ Flx_btype.st x );
*)
    x

  | `TYP_type_tuple ts -> btyp_type_tuple (List.map bt ts)

  | `TYP_name (sr,s,[]) when List.mem_assoc s rs.as_fixlist ->
    let level, kind = List.assoc s rs.as_fixlist in
    btyp_fix (level - rs.depth) kind 

  | `TYP_name (sr,s,[]) when List.mem_assoc s params ->
    let t = List.assoc s params in
(*
print_endline ("Binding `TYP_name " ^s^ " via params to " ^ sbt bsym_table t);
*)
    t

  | `TYP_index (sr,name,index) as x ->
      let sym =
        try hfind "lookup" state.sym_table index
        with Not_found ->
          syserr sr ("Synthetic name "^name ^ " not in symbol table!")
      in
      begin match sym.Flx_sym.symdef with
      | SYMDEF_struct (_,variance)
      | SYMDEF_cstruct (_,_,variance)
      | SYMDEF_union (_,variance)
      | SYMDEF_abs (_,_,_,variance) ->
          let ts = List.map
            (fun (s,i,mt) -> btyp_type_var (i, Flx_btype.bmt "Flx_bind_type1" mt))
            (fst sym.Flx_sym.vs)
          in
          btyp_inst (`Nominal variance,index,ts,Flx_kind.KIND_type)
      | SYMDEF_typevar _ ->
          print_endline ("Synthetic name "^name ^ " is a typevar!");
          syserr sr ("Synthetic name "^name ^ " is a typevar!")

      | _ ->
          print_endline ("Synthetic name "^name ^ " is not a nominal type!");
          syserr sr ("Synthetic name "^name ^ " is not a nominal type!")
      end

  | `TYP_name (sr,"instancetype",[]) -> 
(* print_endline ("Trying to bind instancetype"); *)
     btyp_instancetype sr

  | `TYP_fname (sr, name, ks) ->
(*
print_endline ("Lookup type function name " ^ name ^ " unbound ks=" ^ Flx_util.catmap ", " Flx_print.str_of_kindcode ks);
*)
    let hackname : qualified_name_t  = (`AST_name (sr, name, []) :> qualified_name_t) in
(*
print_endline ("Munged qualified name " ^ Flx_print.string_of_qualified_name hackname);
*)
    let {base_sym=index; spec_vs=spec_vs; sub_ts=sub_ts} , ts = lookup_qn_in_env' state bsym_table env rs hackname in
(*
print_endline ("Found it " ^ name ^ "="^ string_of_int index);
*)
    (* we have a problem now: we've found a view of the typefunction, this can happen
    if the type function is inside a polymorphic class which is opened. The substitution
    of ts in the view with the vs of the environment must be done, but it cannot be done
    right now because the type function may not have been bound yet, 
    but we cannot defer it either, because the BTYP_finst construction only allows
    for kinding substitutions. We will have to FIXME this later. The type function
    itself does not support type variable indices, only kind indices.
    *)
    let ks = List.map (Flx_btype.bmt "[Flx_bind_type:TYP_fname]") ks in
(*
print_endline ("Bound ks = " ^ Flx_util.catmap ", " Flx_kind.sk ks);
*)
    let sym = Flx_sym_table.find state.sym_table index in
    let dom, cod = match sym.symdef with
      | SYMDEF_type_function (iks, t) ->
        (* this constraint requires explicit specification of kind variable bindings.
           It will be removed when we do kind unification.
           This has to happen anyhow, in order to kind check the bindings specified
           produce the correct type function kindings
           and since this requires unification anyhow, we might as well use
           the KMGU produced to fix the ks if they're not specified,
           since we need the KMGU in any case to check, if they are specified,
           they agree with the infered ones. ... but for now .. hackery
        *)
        assert (List.length iks = List.length ks); 
        begin match t with
        | `TYP_typefun (kps, cod, body) -> 
          let dom = match kps with
          | [] -> KND_tuple [] (* kind unit .. *)
          | [_,k] -> k
          | kps -> KND_tuple (List.map snd kps)
          in
          let bks = List.map (fun (s,i,srt) -> s,i, Flx_kind.bind_sortcode srt) iks in
          let dom = Flx_btype.bmt "Flx_bind_type:bind_type TYP_fname(dom)" dom in 
          let cod = Flx_btype.bmt "Flx_bind_type:bind_type TYP_fname(cod)" cod in
          let dom = Flx_kind.ksubst sr bks ks dom in
          let cod = Flx_kind.ksubst sr bks ks cod in
          dom,cod
        | _ -> assert false
        end
      | _ -> assert false
    in 
    let t = btyp_finst (index, ks, dom, cod) in
(*
    print_endline ("Bound reference " ^ Flx_btype.st t);
*)
    t

  | `TYP_flookup _ ->
print_endline ("Lookup type function name here");
     assert false

  | `TYP_name _
  | `TYP_case_tag _
  | `TYP_lookup _
  | `TYP_callback _ as x ->
      let x : qualified_name_t =
        match qualified_name_of_typecode x with
        | Some q -> q
        | None -> assert false
      in
(*
if string_of_qualified_name x = "digraph_t" then begin
*)
(*
print_endline ("Bind type', name = " ^ string_of_qualified_name x);
*)
(*
end;
*)
      let sr2 = src_of_qualified_name x in
      let entry_kind, ts = lookup_qn_in_env' state bsym_table env rs x in
(*
print_endline ("Lookup qn done");
*)
(*
if string_of_qualified_name x = "td[int]" then begin
        print_endline ("bind_type': Type "^string_of_typecode t^"=Qualified name "^string_of_qualified_name x^" lookup finds index " ^
          string_of_bid entry_kind.base_sym);
        print_endline ("Kind=" ^ match t with | `TYP_name (_,s,ts) -> "`TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "`TYP_*");
        print_endline ("spec_vs=" ^
          catmap ","
            (fun (s,j,k)-> s ^ "<" ^ string_of_bid j ^ ":" ^ Flx_kind.sk k ^ ">")
            entry_kind.spec_vs);
        print_endline ("sub_ts=" ^
          catmap "," (sbt bsym_table) entry_kind.sub_ts);
end;
*)
      let ts = List.map bt ts in
(*
print_endline ("ts bound");
*)
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
*)
(*
      print_endline ("Base type bound with sub_ts replacing type variables " ^ sbt bsym_table baset);
*)
(*
end;
*)
      (* SHOULD BE CLIENT ERROR not assertion *)
      if List.length ts != List.length entry_kind.spec_vs then 
      begin
        print_endline ("bind_type': Type "^string_of_typecode t^"=Qualified name "^string_of_qualified_name x^" lookup finds index " ^
          string_of_bid entry_kind.base_sym);
        print_endline ("Kind=" ^ match t with | `TYP_name (_,s,ts) -> "`TYP_name ("^s^"["^catmap ","string_of_typecode ts^"])" | _ -> "`TYP_*");
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
      List.iter2 (fun (s,j,k) t ->   
        let kot = Flx_btype_kind.metatype sr t in
        if not (Flx_kind.kind_ge2 k kot) then begin 
          let sr2 = match hfind "lookup" state.sym_table entry_kind.base_sym with {sr=sr2} -> sr2 in
          clierr2 sr sr2 ("Kinding error binding type " ^ string_of_qualified_name x ^ "\n" ^
            "Argument type " ^ sbt bsym_table t ^ " has kind " ^ Flx_kind.sk kot ^ "\n" ^ 
            "which is not a subkind of required kind " ^ Flx_kind.sk k) 
         end
      ) entry_kind.spec_vs ts;
 
      let t = tsubst sr entry_kind.spec_vs ts baset in
(*
if string_of_qualified_name x = "digraph_t" then begin
*)
(*
      print_endline ("Base type bound with input ts replacing spec type variables " ^ sbt bsym_table t);
*)
(*
end;
*)
      t

  | `TYP_suffix (sr,(qn,t)) ->
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

