open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bparameter
open Flx_bbdcl
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_typing
open Flx_unify
open Flx_exceptions
open List
open Flx_generic
open Flx_tpat
open Flx_name_map
open Flx_bid
open Flx_bbind_state

let bind_req state bsym_table env sr tag =
  Flx_lookup.lookup_code_in_env
    state.lookup_state
    bsym_table
    env
    sr
    tag


(* this routine converts a requirements expression into a list
  of requirements. Requirements must be satisfied.
*)

type tmp_req_t = 
  | Satisfied of int * Flx_btype.t list 
  | Fail of Flx_ast.qualified_name_t option

let pr_tmp_req_t bsym_table = function
  | Satisfied (i,ts) -> "Satisfied " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
  | Fail None -> "Fail Logic"
  | Fail (Some qn) -> "Fail qn=" ^ string_of_qualified_name qn

let bind_reqs bt state bsym_table env sr reqs =
  let add lst i =
    if mem i lst then lst else begin 
      (*
      if state.print_flag then
        print_endline ("// Adding requirement " ^ pr_tmp_req_t bsym_table i);
      *)
      i :: lst
    end
  in
  let merge a b = fold_left add a b in
  let rec aux reqs = match reqs with
  | NREQ_true -> []
  | NREQ_false -> [Fail None]
  | NREQ_and (a,b) -> merge (aux a) (aux b)
  | NREQ_or (a,b) ->
    let check a = 
      try 
        List.iter (fun x -> match x with 
          | Satisfied _ -> () 
          | Fail _ -> raise Not_found
        )
        a;
        true
      with Not_found -> false
    in
    let a = aux a and b = aux b in
    (* Note: we don't check b here, because if we found a failure, what would we do?
       We can't report an error, because the alternative might be nested in another.
       We return b so that at least we can get *some* kind of diagnostic on the final
       check: it will only list the failure of the second alternative, but that's 
       better than nothing
    *)
    if check a then a else b

  | NREQ_atom tag ->
    match bind_req state bsym_table env sr tag with
    | None -> [Fail (Some tag)]
    | Some (entries, ts) ->
      let ts = map bt ts in
      fold_left (fun lst index ->
        let index = sye index in
        try
          let ts = adjust_ts state.sym_table bsym_table sr index ts in
          add lst (Satisfied (index,ts))
        with x ->
          print_endline "** Bind_req failed due to vs/ts mismatch";
          print_endline "** IGNORING! (HACK!!)";
          lst
      ) [] entries
  in
    let res = aux reqs in
    let res = fold_left (fun acc r -> match r with 
      | Satisfied (i,ts) -> (i,ts)::acc
      | Fail None -> clierrx "[flx_bind/flx_bbind.ml:166: E0] " sr "Explicit requirements failure"
      | Fail (Some q) -> clierrx "[flx_bind/flx_bbind.ml:167: E1] " sr ("Cannot find requirement for " ^ Flx_print.string_of_qualified_name q)
      ) 
      [] res 
    in
    res

