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

let rec lookup_qn_with_sig'
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
=
  let lookup_qn_with_sig'
  state
  bsym_table
  sra srn
  env rs
  qn
  signs
=
  lookup_qn_with_sig'
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
in
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
        let hvs = List.map (fun (id,index,kind) -> id,index,bmt "lookup_qn_with_sig.1" kind) hack_vs in
        let hts = List.map (fun (_,index,kind) -> Flx_btype.btyp_type_var (index,bmt "lookup_qn_with_sig.2" kind)) hack_vs in
        let hacked_entry = { base_sym=index; spec_vs=hvs; sub_ts=hts } in
        let ro = resolve_overload state bsym_table env rs sra [hacked_entry] id signs ts in
        let (_,t),bts =
          match ro with
          | Some (index,t,ret,mgu,ts) ->
            Flx_lookup_name_itdws.handle_function
              inner_type_of_index_with_ts
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
if debug then
print_endline ("flx_lookup.handle_nonfunction.bexpr_closure");
        bexpr_closure t (index,bts)

      | SYMDEF_newtype _
      | SYMDEF_union _
      | SYMDEF_abs _
      | SYMDEF_instance_type _ 
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
      | SYMDEF_once t
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
        let bvs = List.map (fun (s,i,tp) -> s,i,Flx_btype.bmt "lookup_qn_with_sig.5" tp) (fst vs) in
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

    | BTYP_rptsum (BTYP_unitsum k, vt) ->
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
      | Free_fixpoint _ as x -> 
(*
print_endline ("Free fixpoint");
*)
        raise x
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
print_endline ("ctor hack failed (client error)");
             clierr2 sr1 sr2
             (
             "attempting name lookup of " ^ name ^ " got Overload Kind ERROR1: " ^ s1 ^
             "\nattempting name lookup of _ctor_" ^ name ^ " got ERROR2: " ^ s2
             )
        end
 
      | ClientError (sr1,s1) as x ->
(*
print_endline ("Client Error (trying ctor hack)?");
    print_endline ("Client Error: Lookup simple name " ^ name ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "] sig=" ^ 
      catmap "," (sbt bsym_table) signs);
     print_endline ("Client error message: " ^ s1);
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
      | x -> 
(*
print_endline ("Error lookup name with sig .. " ^ Printexc.to_string x);
*)
        raise x
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
        (fun (name,i,kind) -> 
(*
print_endline ("AST_index(function): "^name^"=T<"^string_of_int i^">");
*)
          btyp_type_var (i,bmt "lookup_qn_with_sig.3" kind))
        (fst vs)
      in
if debug then
print_endline ("flx_lookup.Ast_index.bexpr_closure");
      let x = bexpr_closure
        (inner_type_of_index state bsym_table sr rs index)
        (index,ts)
      in
      x

    | _ ->
(*
      print_endline ("Non function (guess metatype) " ^ name);
*)
      let ts = List.map
        (fun (_,i,kind) ->
(*
print_endline ("AST_index (nonfunction): "^name^"=T<"^string_of_int i^">");
*)
 btyp_type_var (i,bmt "lookup_qn_with_sig.4" kind))
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
    let env' = Flx_name_lookup.mk_bare_env state.sym_table impl in
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
if debug then
print_endline ("flx_lookup.FunctionEntry.bexpr_closure");
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


