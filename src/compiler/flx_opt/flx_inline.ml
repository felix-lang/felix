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

let print_time syms msg f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  if syms.Flx_mtypes2.compiler_options.Flx_options.showtime
  then print_endline (String.sub (msg ^ "                                        ") 0 40
        ^ string_of_int (int_of_float elapsed) ^ "s");
  result

let ident x = x

let idt t = t

(*
let check_reductions syms bsym_table exes = 
  let exes = 
    try 
      Flx_reduce.reduce_exes syms bsym_table !(syms.reductions) exes 
    with Not_found -> assert false
  in
  exes

let make_specialisation syms uses bsym_table
  caller callee id sr parent props exes rescan_flag
=
  (*
  print_endline ("Specialising call " ^ id ^ "<"^si callee ^ ">");
  print_endline ("In procedure " ^ si caller );
  *)
  (*
  print_endline ("Found procedure "^id^": Inline it!");
  *)
  let k=
    specialise_symbol
      syms uses bsym_table
      callee parent rescan_flag
   in
   (*
   print_endline ("Specialised to " ^ id ^ "<"^si k ^ ">" );
   *)
   k
*)


let heavy_inlining syms bsym_table = (* if true then () else  *)
  let used = ref (!(syms.roots)) in
  let (uses,usedby) = Flx_call.call_data bsym_table in

  while not (BidSet.is_empty !used) do
    let i = BidSet.choose !used in
    used := BidSet.remove i !used;
    Flx_inline_bbdcl.heavily_inline_bbdcl syms uses bsym_table [i] i
  done;

  (* This code is here to attempt to optimise closures (and clones?)
     which aren't handled by the above loop.
  *)
  Flx_bsym_table.iter begin fun bid _ _ ->
    try Flx_inline_bbdcl.heavily_inline_bbdcl syms uses bsym_table [bid] bid
    with exn -> ()
    (*
      print_endline ("*** ERROR OPTIMISING [ignored?] " ^ si i);
      print_endline (Printexc.to_string exn);
      raise exn
    *)
  end bsym_table


