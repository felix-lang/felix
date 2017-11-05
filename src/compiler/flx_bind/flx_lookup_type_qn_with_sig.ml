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
open Flx_name_lookup

let debug = false

let rec lookup_type_qn_with_sig'
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
=
let lookup_type_qn_with_sig'
  sra srn
  env rs
  qn
  signs

= lookup_type_qn_with_sig'
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
in


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
        print_endline ("[lookup_type_qn_with_sig] Struct constructor found, type= " ^ sbt bsym_table t);
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
        lookup_type_qn_with_sig' 
        sra srn env rs qn signs

      | SYMDEF_const (_,t,_,_)
      | SYMDEF_once t
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
    print_endline ("lookup_type_qn_with_sig': AST_name " ^ name ^ " calling lookup_type_name_with_sig");
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
        (fun (name,i,kind) ->
print_endline ("AST_name: "^name^"=T<"^string_of_int i^">");
 btyp_type_var (i, bmt "Flx_lookup_type_qn_with_sig.1" kind))
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
        (fun (name,i,kind) ->
print_endline ("AST_name: "^name^"=T<"^string_of_int i^">");
 btyp_type_var (i, bmt "Flx_lookup_type_qn_with_sig.2" kind))
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
    let env' = Flx_name_lookup.mk_bare_env state.sym_table impl in
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


