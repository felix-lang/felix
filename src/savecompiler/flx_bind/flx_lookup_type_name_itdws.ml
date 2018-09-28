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

let handle_type 
  build_env
  bind_type_index
  state bsym_table rs sra srn name ts index =
  let sym = get_data state.sym_table index in
  match sym.Flx_sym.symdef with
  | SYMDEF_function _
  | SYMDEF_fun _
  | SYMDEF_struct _
  | SYMDEF_cstruct _
  | SYMDEF_nonconst_ctor _
  | SYMDEF_callback _ -> btyp_inst (index,ts,Flx_kind.KIND_type)
  | SYMDEF_instance_type _ ->
(*
print_endline ("Lookup_type_name_in_table_dirs_with_sig: Handle type " ^ name ^ " ... binding type index " ^ string_of_int index);
*)
      let mkenv i = build_env state bsym_table (Some i) in
      let t = bind_type_index state bsym_table rs sym.Flx_sym.sr index ts mkenv in
(*
print_endline ("Handle type " ^ name ^ " ... bound type is " ^ sbt bsym_table t);
*)
      t

  | SYMDEF_type_alias _ ->
(*
print_endline ("Lookup_type_name_in_table_dirs_with_sig: Handle type alias " ^ name ^ " ... binding type index " ^ string_of_int index);
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

let lookup_type_name_in_table_dirs_with_sig
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
=
  let mkenv i = build_env state bsym_table (Some i) in
  let bt sr t =
    bind_type' state bsym_table env rs sr t [] mkenv
  in

  let process_index index =
    match get_data state.sym_table (sye index) with
    { Flx_sym.id=id; sr=sr; vs=vs; symdef=entry }->
    (*
    print_endline ("FOUND " ^ id);
    *)
    match entry with
    | SYMDEF_inherit _ ->
      clierrx "[flx_bind/flx_lookup.ml:3607: E158] " sra "Woops found inherit in lookup_type_name_in_table_dirs_with_sig"
    | SYMDEF_inherit_fun _ ->
      clierrx "[flx_bind/flx_lookup.ml:3609: E159] " sra "Woops found inherit function in lookup_type_name_in_table_dirs_with_sig"

    | SYMDEF_struct _
    | SYMDEF_cstruct _
    | SYMDEF_nonconst_ctor _
      ->
        print_endline "lookup_name_in_table_dirs_with_sig finds struct constructor";
        assert false;
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
              build_env
              bind_type_index
              state
              bsym_table
              rs
              sra srn name ts index
            in
              Some tb
          | None -> None
          end

(* This routine can't work, because the "sigs" required are types,
   and the check needed here is on kinds. The only real solution is
   a special lookup_type_function_name_itdws routine
*)
    | SYMDEF_typevar mt ->
      let mt = bmt "lookup_type_name_itdws" mt in
(*
      (* match function a -> b -> c -> d with sigs a b c *)
      let rec m f s = match f,s with
      | BTYP_function (d,c),h::t when d = h -> m c t
      | BTYP_type_function _,_ -> failwith "Can't handle actual lambda form yet"
      | _,[] -> true
      | _ -> false
      in
      if m mt t2
      then 
*)
      Some (btyp_type_var (sye index,mt))
(*
      else
      (print_endline
      (
        "Typevariable has wrong meta-type" ^
        "\nexpected domains " ^ catmap ", " (sk) t2 ^
        "\ngot " ^ sk mt
      ); None)
*)


    | SYMDEF_virtual_type ->
      print_endline "Found virtual type";
      Some (btyp_vinst (sye index, ts, Flx_kind.KIND_type))

    | SYMDEF_newtype _
    | SYMDEF_abs _
    | SYMDEF_union _ ->
      (* print_endline "Found abs,union,or newtype"; *)
      Some (btyp_inst (sye index, ts, Flx_kind.KIND_type))

    (* an instance type is just like a type alias in phase 1 *)
    | SYMDEF_instance_type t ->
      Some (bt sr t)

    (* the effect of the binding depends on the mode for aliases, nominal or structural *)
    | SYMDEF_type_alias t -> 
(*
      let modes = if get_structural_typedefs state then "structural" else "nominal" in
print_endline ("lookup_type_name_in_table_dirs_with_sig: Binding reference to type alias " ^ name ^ " mode=" ^ modes);
*)
      Some (bt sr t)

    | SYMDEF_label _
    | SYMDEF_const_ctor _
    | SYMDEF_const _
    | SYMDEF_var _
    | SYMDEF_ref _
    | SYMDEF_val _
    | SYMDEF_once _
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
  in
(*
  print_endline
  (
    "LOOKUP TYPE NAME "^name ^"["^
    catmap "," (sbt bsym_table) ts ^
    "] IN TABLE DIRS WITH SIG " ^ catmap "," (sbt bsym_table) t2
  );
  begin match Flx_name_lookup.lookup_name_in_htab table name  with
  | None -> print_endline ("Name " ^ name ^ " not found!");
  | Some _ -> print_endline ("Name " ^ name ^ " found!");
  end;
*)

  match Flx_name_lookup.lookup_name_in_htab table name with
  | Some (NonFunctionEntry (index)) ->
    process_index index

  | Some (FunctionEntry fs) ->
(*
    print_endline ("Lookup type name in table dirs with sig Found function set size " ^ si (List.length fs));
    print_endline ("Not allowed for types now .. ");
*)
    None

  | None ->
(*
    print_endline "Can't find in primary table, Trying opens";
*)
    let opens : entry_set_t list =
      List.concat
      (
        List.map
        (fun table ->
          match Flx_name_lookup.lookup_name_in_htab table name with
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
    | [NonFunctionEntry index] ->
      process_index index
    | _ -> None
 

