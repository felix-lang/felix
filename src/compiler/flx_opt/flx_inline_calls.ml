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

(* this function stacks up exes in reverse order, then reverses the
result so it returns exes in forward order
*)

let rec heavy_inline_calls
  syms
  bsym_table
  uses
  inline_bbdcl
  caller
  excludes
  exes
=
  (*
  print_endline ("HIC: Input excludes = " ^ catmap "," si excludes);
  *)
  let specialise_check caller callee props exes = false
    (*
    (* for the moment, don't specialise recursive calls *)
    ts <> [] &&
    not (Flx_call.is_recursive_call uses caller callee)
    *)
  in

  (* Rescanning currently enabled, set true to disable *)
  let hic revariable callee exes = if false then exes else
    (*
    print_endline "Rescanning ..";
    *)
    let excludes = fold_left
    (fun acc i ->
      i :: (try [Hashtbl.find revariable i] with Not_found -> []) @ acc
    )
    []
    (callee::excludes)
    in
    heavy_inline_calls
      syms
      bsym_table
      uses
      inline_bbdcl
      caller
      excludes
      exes
  in

  (* The function ee applies the special inlining routine
    to all subexpressions of an expression, bottom up
    (that is, inside out).
  *)

  let sinl sr e = Flx_special_inline.special_inline 
    syms uses bsym_table caller inline_bbdcl hic (caller::excludes) sr e 
  in

  let ee exe = Flx_expand_exe.expand_exe syms bsym_table sinl exe in
  let exes' = ref [] in (* reverse order *)
  List.iter  (* each exe *)
  (fun exeIN ->
    (*
    print_endline ("EXE[in] =" ^ string_of_bexe bsym_table 0 exeIN);
    *)
    let xs = ee exeIN in
    (*
    List.iter (fun x -> print_endline ("EXE[out]=" ^ string_of_bexe bsym_table 0 x)) xs;
    print_endline "--";
    *)
    (*
      This code RESCANS the result of the special inliner.
      The special inliner only handles function applications,
      this code should NOT handle them because iteration might
      lead to infinite recurse ..??

      This means the 'special cases' handled must be
      disjoint.

      Unfortunately, when inlining a function, we first
      inline into the function, then dump the result and
      rescan it. Consequently the recursion stop applied
      which leaves a direct non-tail self call will be
      rescanned here, and the function will be unfolded
      again .. in that process we also redo the special
      inlining .. infinite recursion. This is stopped
      by the flag which prevents inlining into a function
      more than once .. but that doesn't work if the
      function is cloned.
    *)
    List.iter (fun exe ->
    match exe with
    | BEXE_call (sr,(BEXPR_closure(callee,ts),_),argument)
    | BEXE_call_direct (sr,callee,ts,argument)
      when not (mem callee excludes)
      ->
      assert (ts=[]);
      inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      (*
      print_endline ("CALL DIRECT " ^ id ^ "<"^ si callee^">");
      *)
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,ps,BTYP_void,effects,exes) ->
        assert (vs = []);
        if Flx_inline_check.inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if syms.compiler_options.print_flag then
          print_endline ("inlining direct call: " ^ string_of_bexe bsym_table 0 exe);
          let revariable,xs = Flx_inline_call.heavy_inline_call
            syms
            uses bsym_table
            caller
            callee
            argument
            (Flx_bsym.id bsym)
            sr
            (props,vs,ps,exes)
          in
          let xs = hic revariable callee xs in
          exes' := rev xs @ !exes'
        end
        else
          exes' := exe :: !exes'

      | _ ->  assert false (* exes' := exe :: !exes' *)
      end

    | BEXE_call (sr,(BEXPR_apply_stack (callee,ts,a),_),argument)
      -> assert false

    | BEXE_call (sr,(BEXPR_apply((BEXPR_closure (callee,ts),_),a),_),argument)
    | BEXE_call (sr,(BEXPR_apply_direct (callee,ts,a),_),argument)
      when not (mem callee excludes)
      ->
      assert (ts=[]);
      (*
      print_endline "DETECTED CANDIDATE FOR CALL LIFTING ";
      print_endline ("In procedure " ^ si caller ^ " with vs=" ^ string_of_vs caller_vs);
      *)
      (*
      print_endline ("handling call lift: " ^ string_of_bexe bsym_table 0 exe);
      print_endline ("Callee is " ^ si callee ^ " with ts = " ^ catmap "," (sbt bsym_table) ts);
      *)
      inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
        assert (vs=[]);
        if Flx_inline_check.inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if syms.compiler_options.print_flag then
          print_endline ("Inline call lift: " ^ string_of_bexe bsym_table 0 exe);
          let revariable,xs =
            Flx_call_lifting.call_lifting syms uses bsym_table caller callee a argument
          in
          let xs = hic revariable callee xs in
          exes' := rev xs @ !exes'
        end else
          exes' := exe :: !exes'
      | _ -> assert false (* exes' := exe :: !exes' *)
      end

    | BEXE_init (sr,i,(BEXPR_apply_stack (callee,ts,a),_))
      -> assert false

    | BEXE_init (sr,i,(BEXPR_apply ((BEXPR_closure(callee,ts),_),a),_))
    | BEXE_init (sr,i,(BEXPR_apply_direct (callee,ts,a),_))
      when not (mem callee excludes)  ->
(*
print_endline "init apply closure/direct";
*)
      assert (ts=[]);
      inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
        if Flx_inline_check.inline_check syms bsym_table uses sr caller callee props exes then
          begin
            let bsymv = Flx_bsym_table.find bsym_table i in
            begin match Flx_bsym.bbdcl bsymv with
            | BBDCL_val (vs,t,`Tmp) ->
                assert (vs=[]);
print_endline ("Downgrading temporary to val: " ^ Flx_bsym.id bsymv);
                (* Downgrading temporary *)
                (* should this be a VAR or a VAL? *)
                Flx_bsym_table.update_bbdcl
                  bsym_table
                  i
                  (bbdcl_val ([],t,`Var))
            | _ -> ()
            end;
            if syms.compiler_options.print_flag then
            print_endline ("Inline init: " ^ string_of_bexe bsym_table 0 exe);
            let revariable,xs =
              Flx_inline_function.inline_function syms uses bsym_table caller callee a i
            in
            let xs = hic revariable callee xs in
            exes' := rev xs @ !exes'
          end
        else
          exes' := exe :: !exes'
      | _ -> assert false (* exes' := exe :: !exes' *)
      end

    | BEXE_fun_return (sr,(BEXPR_apply_stack (callee,ts,a),_))
     -> assert false

    | BEXE_fun_return (sr,(BEXPR_apply((BEXPR_closure(callee,ts),_),a),_))
    | BEXE_fun_return (sr,(BEXPR_apply_direct (callee,ts,a),_))
      when not (mem callee excludes)  ->
(*
print_endline "return apply/closure/direct";
*)
      assert (ts=[]);
      inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      let bsym = Flx_bsym_table.find bsym_table callee in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
        assert (vs=[]);
        if Flx_inline_check.inline_check syms bsym_table uses sr caller callee props exes then
        begin
          if Flx_inlining_complete.inlining_complete bsym_table callee then
          begin
            if syms.compiler_options.print_flag then
            print_endline ("Inline tail apply : " ^ string_of_bexe bsym_table 0 exe);
            let revariable,xs =
              Flx_inline_tail_apply.inline_tail_apply syms uses bsym_table caller callee a
            in
            let xs = hic revariable callee xs in
            exes' := rev xs @ !exes'
          end else
            exes' := exe :: !exes'
        end else
          exes' := exe :: !exes'
      | _ -> assert false (* exes' := exe :: !exes' *)
      end
    | _ -> exes' := exe :: !exes'
    )
    xs
  )
  exes
  ;
  rev !exes'


