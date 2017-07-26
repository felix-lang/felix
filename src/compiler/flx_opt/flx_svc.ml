
open List

open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_options
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_spexes
open Flx_args
open Flx_bid

let has_svc_field es = 
  try
    List.iter (fun (s,_) -> if s = "SVC" then raise Not_found) es;
    false
  with 
    Not_found -> true

let merge_svc_field es =
  if has_svc_field es then es else ("SVC",btyp_unit()) :: es

let get_polyfields t =
  match t with
  | BTYP_record es -> es, btyp_unit ()
  | BTYP_polyrecord (es,v) -> es, v
  | _ -> [], t

let merge_svc effects = 
  let es, v = get_polyfields effects in
  let es = merge_svc_field es in
  btyp_polyrecord es v

let does_svc exes = try List.iter (fun exe -> 
   match exe with | BEXE_svc _ -> raise Not_found | _ -> ()) exes; false
   with Not_found -> true 

let direct_apply_svc svcs e =
  match e with
  | BEXPR_apply_direct (i,_,_),_
  | BEXPR_apply_stack (i,_,_),_ ->
    if BidSet.mem i svcs then raise Not_found
  | _ -> ()

let direct_call_svc exes svcs = 
  try 
    List.iter (fun exe -> 
      let f_bexpr e = Flx_bexpr.iter ~f_bexpr:(direct_apply_svc svcs) e in
      Flx_bexe.iter ~f_bexpr exe;
      match exe with
      | BEXE_call_direct (_,i,_,_)
      | BEXE_jump_direct (_,i,_,_)
      | BEXE_call_stack (_,i,_,_) ->
        if BidSet.mem i svcs then raise Not_found
      | _ -> ()
    )
    exes;
    false
  with Not_found -> true

let svc_leaf_check syms bsym_table svcs =
  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
      let effects' = if does_svc exes then merge_svc effects else effects in
      if effects <> effects' then begin
(*
        print_endline ("Detected SVC in " ^ Flx_bsym.id bsym);
*)
        svcs := BidSet.add i !svcs 
      end
    | _ -> ()
  end bsym_table

let direct_call_check syms bsym_table svcs =
  let counter = ref 0 in
  Flx_bsym_table.iter begin fun i _ bsym ->
    if not (BidSet.mem i !svcs) then (* already analysed *)
      match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
        if direct_call_svc exes !svcs then begin 
(*
          print_endline ("Detected direct call to SVC containing fn in " ^ Flx_bsym.id bsym);
*)
          incr counter;
          svcs := BidSet.add i !svcs 
        end

      | _ -> ()
  end bsym_table;
  !counter


let find_svcs syms bsym_table =
  let svcs = ref BidSet.empty in
  svc_leaf_check syms bsym_table svcs;
  let iterations = ref 0 in
  begin
    (* easy but slow way! should use recursive descent *)
    let counter = ref 1 in 
    while !counter > 0 do
      counter := direct_call_check syms bsym_table svcs; 
      incr iterations;
    done
  end;
(*
  print_endline ("Done svcs in " ^ string_of_int !iterations);
*)
  !svcs

let svc_set_inline syms bsym_table =
  let svcs = find_svcs syms bsym_table in
  BidSet.iter (fun i ->
    let bsym = Flx_bsym_table.find bsym_table i in
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
      if ret <> btyp_void () then
      if List.mem `NoInline  props then begin
        print_endline ("WARNING: SVC propagates to noinline function " ^ Flx_bsym.id bsym ^ "<"^string_of_int i^">")
      end
      else if not (List.mem `Inline props) then
        let props = `Inline :: props in
        let bbdcl = Flx_bbdcl.bbdcl_fun (props,vs,(ps,traint),ret,effects,exes) in 
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl
      else ()

    | _ -> assert false
  ) svcs



let svc_check syms bsym_table =
  let svcs = find_svcs syms bsym_table in
  BidSet.iter (fun i ->
    let bsym = Flx_bsym_table.find bsym_table i in
    let bbdcl = Flx_bsym.bbdcl bsym in
    match bbdcl with
    | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
      if ret <> btyp_void () then
        print_endline ("Warning: SVC propagates to function " ^
          Flx_bsym.id bsym ^ "<"^ string_of_int i ^">");
    | _ -> assert false
  ) svcs;



