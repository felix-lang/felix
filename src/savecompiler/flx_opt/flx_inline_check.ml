open List
open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_btype
open Flx_exceptions
open Flx_foldvars
(* open Flx_foldvars2 *)
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
open Flx_bid

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

*)

let inline_check syms bsym_table uses srcall caller callee props exes =
  let bsym = Flx_bsym_table.find bsym_table callee in
  let srdef = Flx_bsym.sr bsym in
  let id = Flx_bsym.id bsym in
  let bbdcl = Flx_bsym.bbdcl bsym in
  let kind = 
     match bbdcl with
     | BBDCL_fun (_,_,_,ret,effects,_) -> (match ret with BTYP_void -> `Proc | _ -> `Fun)
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
    clierrx "[flx_opt/flx_inline.ml:647: E371] " srdef ("Function " ^ id ^ ": Conflicting properties inline and noinline")
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


