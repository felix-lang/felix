open Flx_ast
open Flx_btype
open Flx_bexpr
open Flx_lookup_state
open Flx_types
open Flx_name_map
open Flx_bid

type module_rep_t =
  | Simple_module of bid_t * typecode_t list * name_map_t * sdir_t list

let typecode_of_btype sr x = Flx_typecode_of_btype.typecode_of_btype sr x

(* THIS IS SPECIAL CODE SUGGESTED BY GARRET BLUMA *)
(* It is designed to handle cases like

   (1,2,3) . map (fun x -> ... )

   so we don't have to write the type of x as int
*)
let set_deferred_type 
  bsym_table state env inner_lookup_name_in_env 
  eval_module_expr get_pub_tables
  rs 
  sr f' (ea, ta as a)
=
  let pss = match f' with
    | EXPR_name (sr,name,[]) ->
      let entries = 
        try inner_lookup_name_in_env state bsym_table env rs sr name
        with _ -> raise Flx_dot.OverloadResolutionError
      in 
      begin match entries with
      | FunctionEntry [{base_sym=idx; spec_vs=[]; sub_ts=[]}] ->
        begin match hfind "lookup(defered?)" state.sym_table idx with
        | { Flx_sym.symdef=SYMDEF_function (pss, ret, effects,props,exes) } -> pss
        | _ -> raise Flx_dot.OverloadResolutionError 
        end
      | _ -> 
       raise Flx_dot.OverloadResolutionError
      end 

    | EXPR_lookup (sr, (e,name,[]))  -> 
      let entry =
        match
          eval_module_expr
          state
          bsym_table
          env
          e
        with
        | (Simple_module (impl, ts, htab,dirs)) ->
          let env' = Flx_name_lookup.mk_bare_env state.sym_table impl in
          let tables = get_pub_tables state bsym_table env' rs dirs in
          let result = Flx_name_lookup.lookup_name_in_table_dirs htab tables sr name in
          result
      in
      begin match entry with
      | Some entry ->
        begin match entry with
        | FunctionEntry [{base_sym=idx; spec_vs=[]; sub_ts=[]}] -> 
          begin match hfind "lookup(defered?)" state.sym_table idx with
          | { Flx_sym.symdef=SYMDEF_function (pss, ret, effects,props,exes) } -> pss
          | _ -> raise Flx_dot.OverloadResolutionError 
          end
        | _ -> raise Flx_dot.OverloadResolutionError
        end
      | None -> raise Flx_dot.OverloadResolutionError
      end

    | EXPR_index (sr,name,idx) ->
      begin match hfind "lookup(defered?)" state.sym_table idx with
      | { Flx_sym.symdef=SYMDEF_function (pss, ret, effects,props,exes) } -> pss
      | _ -> raise Flx_dot.OverloadResolutionError 
      end 
    | EXPR_suffix (sr,(qn,TYP_defer (sr2,dt))) -> 
      Satom (sr,`PVal,"defered-lambda-param",TYP_defer (sr2,dt),None),None
    | EXPR_suffix _ -> raise Flx_dot.OverloadResolutionError 
    | _ -> 
      raise Flx_dot.OverloadResolutionError
  in 

  match pss with
  | Satom (sr,kind,pid,TYP_defer (_,tref),_),None -> 
    begin match !tref with
    | Some t -> print_endline ("DEFERED TYPE IS ALREADY SET")
    | None -> 
      print_endline ("DEFERED TYPE IS NOT SET");
      let tc = typecode_of_btype bsym_table state.counter sr ta in
      tref := Some tc;
      print_endline ("DEFERED TYPE SET NOW TO " ^ Flx_print.string_of_typecode tc)
    end;
    raise Flx_dot.OverloadResolutionError
  | _ -> raise Flx_dot.OverloadResolutionError


