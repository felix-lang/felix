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
open Flx_set
open Flx_spexes
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util
open Flx_bid

let heavy_inline_call syms uses bsym_table
  caller callee argument id sr (props, vs, ps, exes)
=
  (*
  print_endline ("INLINING CALL to " ^ id ^"<"^ si callee^">("^sbe bsym_table argument^")");
  print_endline ("In procedure " ^ si caller );
  print_endline ("Callee is " ^ id ^ "<"^si callee ^ ">" );
  *)
  (*
  print_endline ("Found procedure "^id^": Inline it!");
  *)
  let revariable = Flx_reparent.reparent_children
    syms uses bsym_table
    callee (Some caller) false []
  in
  let xs = gen_body
    syms
    uses bsym_table
    id
    ps
    revariable
    exes
    argument
    sr
    caller
    callee
    `Lazy
    props
  in
    revariable,rev xs (* forward order *)


