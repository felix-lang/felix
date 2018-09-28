open Flx_version
open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_typing2
open Flx_pat
open Flx_exceptions

module CS = Flx_code_spec


(* Here, "name" is the name of the containing parent, eg if the statement being
 * processed is in a module X, then name will be "X". The name always exists.
 * If it is a top level thing, the name is a munged version of the program filename.
 *
 * The idea here is: if you write "requires fred" in a module X, then "_rqs_X"
 * will be an empty insertion with requirement "fred". We then make every symbol
 * in X depend on "_rqs_X" and thus propagate the dependency on "fred".
 *
 * If a module Y is nested in a module X, then "_rqs_Y" will have a requirement
 * on "_rqs_X", so the symbols in a nested module will inherit any requirements
 * of the parent of the module which is their parent.
 *
 * Adding the dependency of Y on X is called making a bridge.
 *
 * BUG: TO BE FIXED: The top level "module" never gets an insertion!
 * So the bridges built to that module fail. This only happens if
 * we're processing a nested scope for which a bridge is generated,
 * some some of our regression tests pass, but any with a function in them
 * fail (since function have scopes and therefore generate bridges)
 *
 * The root rqs thing has to be manually inserted by the top level caller
 * of desugar, which is the one inventing the top level module name from
 * the program
 *)
let bridge n sr parent_vs rqname' name : asm_t =
(*
    print_endline ("Making bridge for " ^ n ^ " -> " ^ name ^ Flx_print.string_of_vs parent_vs);
*)
  let ts = List.map (fun (s,_)-> TYP_name (sr,s,[])) (fst parent_vs) in
  let us = NREQ_atom (`AST_name (sr, "_rqs_" ^ name, ts)) in
  let body = DCL_insert (CS.Str "", `Body, us) in
  Dcl (sr, "_rqs_" ^ n, None, `Public, dfltvs, body)

let map_req name n = if n = "_root" then "_rqs_" ^ name else n 

let map_reqs rqname' sr (reqs : named_req_expr_t) : named_req_expr_t =
    NREQ_and (NREQ_atom (rqname' sr), reqs)

  (* name literal requirements *)
let mkprop sr s = match s with
    | "heap_closure" -> `Heap_closure
    | "needs_gc" -> `Uses_gc
    | "needs_ptf" -> `Requires_ptf
    | "pure" -> `Pure
    | "generator" -> `Generator
    | "virtual" -> `Virtual
    | x -> clierrx "[flx_desugar/flx_reqs.ml:62: E353] " sr ("Unknown property " ^ x)

let mkreqs state access parent_ts sr (rqs :raw_req_expr_t) : 
  int option * 
  type_qual_t list *
  property_t list * 
  asm_t list * 
  named_req_expr_t 
=
    let ix = None in
    let quals = ref [] in
    let props = ref [] in
    let decls = ref [] in
    let mkreq s kind =
      let n = state.Flx_desugar_expr.fresh_bid () in
      let n = "_req_" ^ string_of_bid n in
      let dcl = Dcl (sr,n,ix,access,dfltvs,DCL_insert (s,kind,NREQ_true)) in
      decls := dcl :: !decls;
      NREQ_atom (`AST_name (sr,n,parent_ts sr))
    in
    let index = ref None in
    let rec aux rqs = match rqs with
    | RREQ_or (a,b) -> NREQ_or (aux a, aux b)
    | RREQ_and (a,b) -> NREQ_and (aux a, aux b)
    | RREQ_true -> NREQ_true
    | RREQ_false -> NREQ_false
    | RREQ_atom x -> match x with
      | Body_req s -> mkreq s `Body
      | Header_req s -> mkreq s `Header
      | Package_req s -> mkreq s `Package

      | Named_req n -> NREQ_atom n

      | Property_req "generator" ->
        props := `Generator :: !props;
        NREQ_true

      | Subtype_req ->
        props := `Subtype :: !props;
        NREQ_true


      | Property_req "virtual" ->
        props := `Virtual:: !props;
        NREQ_true

      | Property_req s ->
        props := mkprop sr s :: !props;
        NREQ_true
      | Scanner_req s ->
        quals := `Scanner s :: !quals;
        NREQ_true

      | Finaliser_req s ->
        quals := `Finaliser s :: !quals;
        NREQ_true

      | Encoder_req s ->
        quals := `Encoder s :: !quals;
        NREQ_true

      | Decoder_req s ->
        quals := `Decoder s :: !quals;
        NREQ_true

      | Index_req i ->
        index := Some i;
        NREQ_true

      | Named_index_req s ->
        print_endline ("Named index requirement " ^ s ^ 
        " for symbol should have be removed by macro processor.\n" ^
        "A macro with that name defined as an integer\n" ^
        "is required for the concordance which allows the compiler\n"  ^
        "to refer directly to symbols defined in the library\n");
        clierr sr ("Unspecified value for concordance macro " ^ s);

    in
    let r = aux rqs in
    !index, !quals, !props, !decls, r


