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

module EntrySet = Set.Make(
  struct
    type t = entry_kind_t
    let compare = compare
  end
)

let rec trclose 
  build_env
  lookup_qn_in_env2'
  state bsym_table rs sr fs =
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

and resolve_inherits 
  build_env
  inner_build_env
  lookup_qn_in_env2'
  state bsym_table rs sr x =
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
  | FunctionEntry fs -> FunctionEntry (
    trclose 
      build_env
      lookup_qn_in_env2'
      state bsym_table rs sr fs
    )


