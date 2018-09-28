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

(* Returns and expression e in which subexpressions are replaced by variables,
  and the variables are initialised in the executable instructions,
  so the exe containing this expression, after replacement, has to run
  after the exes it returns.

  The exes returned are in reverse order. Called by Flx_inline_calls.
*)
let special_inline syms uses bsym_table caller heavily_inline_bbdcl hic excludes sr e =
  let nth ls n = try List.nth ls n with _ -> 
    failwith ("special_inline, n'th failure listlen=" ^ string_of_int n ^
    ", index=" ^ string_of_int (List.length ls))
  in
(*
  print_endline ("Special inline " ^ sbe bsym_table e ^ " type " ^ sbt bsym_table (snd e));
*)
  let exes' = ref [] in
  let id x = x in
  let rec aux e =
(*
  print_endline (" ... Special inline subexpr: " ^ sbe bsym_table e);
*)
  let result = 

(* Here is our problem!

   This is a bottom up scan. This means the first things pushed are leaves,
   and we work upwards.  After the caller reverses this list of exes
   the leaf initialisations happen first, which is essential, since node
   initialisations dependent on their kids contains variables that
   must be initialised first.

   The PROBLEM is we have to use a top down scan to handle BEXPR_cond,
   so we can skip initialising variables for expressions in the branches,
   since these initialisation can fail: we have to evaluated them
   on demand, i.e. lazily.


   FIRST: change the ordering so the analysis is FLAT, i.e. it only
   applies to the top level expression which is a special case.
   Non-special cases are analysed by mapping their subterms.
*)
  match e with
  | BEXPR_cond (c,tr,fa),_  -> 
    let c = Flx_bexpr.map ~f_bexpr:aux c in bexpr_cond c tr fa
  | _ ->

  match Flx_bexpr.map ~f_bexpr:aux e with
  | BEXPR_apply ((BEXPR_prj (n,_,_),_),(BEXPR_tuple ls,_)),_ -> 
(*
print_endline "Apply prj/tuple";
*)
    let e = try nth ls n with exn ->
      print_endline "projection of tuple";
      raise exn
    in e

  | BEXPR_apply ((BEXPR_prj (n,_,_),_),(BEXPR_record ls,_)),_ -> 
(*
print_endline "Apply prj/record";
*)
    let e = try snd (nth ls n) with exn ->
      print_endline "projection of record";
      raise exn
    in e

  (* get_n on a struct apply to an explicit tuple .. *)
  | BEXPR_apply (
      (BEXPR_prj (n,_,_),_),
      (BEXPR_apply_struct (bid,ts,(BEXPR_tuple ls,_)),_)
    ),_ as x -> 
(*
print_endline "Apply prj/struct/tuple";
*)
    let bbdcl = Flx_bsym_table.find_bbdcl bsym_table bid in
    begin match bbdcl with 
    | BBDCL_struct _
    | BBDCL_cstruct _ -> let e = nth ls n in e
    | _ -> 
       print_endline ("Apply projection to apply of struct constructor to tuple: index not struct!"); 
       assert false (* Flx_bexpr.map ~f_bexpr:aux x *)
    end

  | BEXPR_closure (callee,_),_ as x ->
      heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
      x

  | ((BEXPR_apply_stack (callee,ts,a),t) as e)
    -> assert false

  | ((BEXPR_apply_prim (callee,ts,a),t) as e) ->
    let bsym = Flx_bsym_table.find bsym_table callee in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_external_fun (props,_,_,_,_,_,_) ->
      if mem `Generator props then begin
        (* create a new variable *)
        let urv = fresh_bid syms.counter in
        let urvid = "_genout_urv" ^ string_of_bid urv in
        add_use uses caller urv sr;
        Flx_bsym_table.add bsym_table urv (Some caller)
          (Flx_bsym.create ~sr urvid (bbdcl_val ([],t,`Var)));

        (* set variable to function appliction *)
        let cll = bexe_init (sr,urv,e) in
        exes' := cll :: !exes';


        (* replace application with the variable *)
        bexpr_varname t (urv,[])

      end else begin
      e
      end
    | _ -> assert false
    end

  | (((BEXPR_apply(  (BEXPR_closure (callee,ts),_) ,a)),t) as e)
  | ((BEXPR_apply_direct (callee,ts,a),t) as e) 
    ->
(*
print_endline ("apply closure/direct callee=" ^ string_of_int callee ^ " arg=" ^ sbe bsym_table a);
*)
      assert (ts=[]);
      if not (mem callee excludes) then begin
        heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
        let bsym = Flx_bsym_table.find bsym_table callee in
        let id = Flx_bsym.id bsym in
(*
print_endline ("Callee=" ^ id);
*)
        begin match Flx_bsym.bbdcl bsym with
        | BBDCL_fun (props,_,_,_,_,_)  
          when mem `Generator props && not (mem `Inline props)
          ->
          (*
          print_endline ("Unravel generator " ^ id);
          *)

          (* create a new variable *)
          let urv = fresh_bid syms.counter in
          let urvid = "_genout_urv" ^ string_of_bid urv in
          add_use uses caller urv sr;
          Flx_bsym_table.add bsym_table urv (Some caller)
            (Flx_bsym.create ~sr urvid (bbdcl_val ([],t,`Var)));

          (* set variable to function appliction *)
          let cll = bexe_init (sr,urv,e) in
          exes' := cll :: !exes';


(*
print_endline ("fresh variable type " ^ sbt bsym_table t);
*)
          (* replace application with the variable *)
          bexpr_varname t (urv,[])

        | BBDCL_fun (props,vs,(ps,traint),ret,effects,exes) ->
          (* TEMPORARY FIX! *)

          (*
          (* create a new variable *)
          let urv = !(syms.counter) in incr (syms.counter);
          let urvid = "_urv" ^ si urv in
          add_child caller urv;
          add_use uses caller urv sr;
          let entry = BBDCL_val (caller_vs,t) in
          Flx_bsym_table.update bsym_table urv (urvid,Some caller,sr,entry);

          (* set variable to function appliction *)
          let cll = BEXE_init (sr,urv,e) in
          exes' := cll :: !exes';


          (* replace application with the variable *)
          let ts = List.map (fun (_,i)-> btyp_type_var (i,btyp_type 0)) caller_vs in
          BEXPR_varname (urv,ts),t
          *)



          (*
          print_endline ("Consider inlining " ^ id ^ "<" ^ si callee ^ "> into " ^ si caller);
          print_endline ("  Child? " ^
            if Flx_bsym_table.is_child bsym_table caller callee then "YES" else "NO")
          ;
          print_endline ("  Recursive? " ^
            if Flx_call.is_recursive_call uses callee callee then "YES" else "NO")
          ;
          print_endline ("  Recursive Call? " ^
            if Flx_call.is_recursive_call uses caller callee then "YES" else "NO")
          ;
          print_endline ("  Can_inline flag? " ^
            if can_inline then "YES" else "NO")
          ;
          print_endline ("  Short enough? " ^
            if length exes <= syms.compiler_options.max_inline_length then "YES" else "NO")
          ;
          print_endline ("  Noinline property? " ^
            if mem `NoInline props then "YES" else "NO")
          ;
          print_endline ("  Inline property? " ^
            if mem `Inline props then "YES" else "NO")
          ;
          *)
          let inline_choice =
            Flx_inline_check.inline_check syms bsym_table uses sr caller callee props exes 
          in
          (*
          print_endline ("  Inline decision: " ^
            if inline_choice then "YES" else "NO")
          ;
          *)
          if inline_choice
          then
              begin
                (*
                heavily_inline_bbdcl syms uses bsym_table (callee::excludes) callee;
                *)
                if not (Flx_inlining_complete.inlining_complete bsym_table callee) then print_endline "Inlining isn't complete in callee ..??";

                if Flx_inlining_complete.inlining_complete bsym_table callee then begin
                  (*
                  print_endline ("INLINE " ^ id ^ "<" ^ si callee ^ ">");
                  print_endline ("Special inline " ^ si caller ^" calls " ^ si callee);
                  *)
                  (* GENERAL CASE -- we need to add a variable *)
                  let urv = fresh_bid syms.counter in
                  (* inline the code, replacing returns with variable inits *)
                  let revariable,xs =
                     Flx_inline_function.inline_function syms uses bsym_table caller callee a urv
                  in
                  match rev xs with
                  (* SPECIAL CASE DETECTOR: if the inlined function
                    terminates with an initialisation of the new variable,
                    ignore the variable and use the value used to initialise
                    it instead. This is sure to be the result of the sole
                    trailing return. If there were another return, a
                    jump to the end of the function would be needed,
                    past this initialisation, which would require a label
                    at the end of the function

                    Note this is a bad form of 'apply lifting'.
                    We should be able to inline

                    f (g x)

                    by inlining g x, and replacing 'return e'
                    with 'v = f e' everywhere. instead we get
                    v = e in various places, then f v.

                    To do this right we need to see a double application.
                  *)
                  | [] -> assert false
                  | BEXE_init (sr,j,e') :: rev_tail ->
                    assert (j==urv);
                    (*
                    print_endline "DETECTED SPECIAL CASE";
                    print_endline "Outputing tail:";
                    List.iter (fun x -> print_endline (string_of_bexe bsym_table 0 x)) (rev tail);
                    print_endline ("Expr: " ^ sbe bsym_table e');
                    *)
                    let tail = hic revariable callee (rev rev_tail) in
                    exes' := rev tail @ !exes';
(*
print_endline ("New expr = " ^ sbe bsym_table e' ^ " type " ^ sbt bsym_table (snd e'));
*)
                    e'

                  | rxs ->
                    let urvid = "_urv" ^ string_of_bid urv in
                    add_use uses caller urv sr;
                    Flx_bsym_table.add bsym_table urv (Some caller)
                      (Flx_bsym.create ~sr urvid (bbdcl_val ([],t,`Val)));

                    let rxs = hic revariable callee xs in
                    exes' := rev rxs @ !exes';
(*
print_endline ("new var type " ^ sbt bsym_table t);
*)
                    bexpr_varname t (urv,[])
                end
                else
                begin
(*
                  print_endline ("***> Didn't inline " ^ id);
*)
                  e
                end
              end
          else
          begin
(*
            print_endline ("***> Didn't inline " ^ id);
*)
            e
          end
        | _ -> assert false (* e *)
        end
      end 
      else e

  | x -> x
  in
(*
  print_endline (" ... Special inline result: " ^ sbe bsym_table result);
*)
  result
  in
   let e = aux e in (* we need left to right evaluation here ..*)
   e,!exes'


