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

type module_rep_t = Flx_bind_deferred.module_rep_t

let check_module state name sr entries ts =
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

let rec eval_module_expr 
  inner_lookup_name_in_env
  get_pub_tables
  lookup_name_in_table_dirs
  state bsym_table env e : module_rep_t 
=
  let eval_module_expr state bsym_table env e =
    eval_module_expr 
      inner_lookup_name_in_env
      get_pub_tables
      lookup_name_in_table_dirs
      state bsym_table env e 
  in
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
      let env' = Flx_name_lookup.mk_bare_env state.sym_table index in
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


