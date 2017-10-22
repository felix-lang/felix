(* Convenience function for printing debug statements. *)
let print_debug syms msg =
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
  then print_endline msg

let print_time syms msg f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  if syms.Flx_mtypes2.compiler_options.Flx_options.showtime
  then print_endline (String.sub (msg ^ "                                        ") 0 40
        ^ string_of_int (int_of_float elapsed) ^ "s");
  result


(* Convert curried functions to uncurried functions so they can ba applied
 * directly instead of requiring closures. *)
let uncurry_functions syms bsym_table =
  let bsym_table = ref bsym_table in
  let counter = ref 0 in

  while Flx_uncurry.uncurry_gen syms !bsym_table > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";

    (* Remove unused symbols. *)
    bsym_table := Flx_use.copy_used syms !bsym_table
  done;

  !bsym_table

let mkproc syms bsym_table =
  (* Clean up the symbol table. *)
  let bsym_table = Flx_use.copy_used syms bsym_table in
  let bsym_table = ref bsym_table in
  let counter = ref 1 in
  let ncvt = ref (Flx_mkproc.mkproc_gen syms !bsym_table  ) in
(*
print_endline ("\n\n++++++ Ran mkproc, pass = " ^ string_of_int (!counter) ^ ", converted " ^ string_of_int !ncvt);
*)
  while !ncvt > 0 do
    (* Clean up the symbol table. *)
    bsym_table := Flx_use.copy_used syms !bsym_table;
    ncvt := Flx_mkproc.mkproc_gen syms !bsym_table;
(*
print_endline ("\n\n++++++ Ran mkproc, pass = " ^ string_of_int (!counter) ^ ", converted " ^ string_of_int !ncvt);
*)
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";
  done;
  Flx_use.copy_used syms !bsym_table (* final cleanup *)


(* Convert functions into stack calls. *)
let stack_calls syms bsym_table =
  print_debug syms "//Calculating stackable calls";

  let label_info = Flx_label.create_label_info bsym_table in
  Flx_stack_calls.make_stack_calls syms bsym_table label_info;

  print_debug syms "//stackable calls done";

  bsym_table


(* Do some platform independent optimizations of the code. *)
let optimize_bsym_table' syms bsym_table (root_proc: int option) =
  print_debug syms "//OPTIMISING";
(*
  print_time syms "[flx_opt]; Performing reductions" begin fun () ->
  Flx_reduce.reduce_all syms bsym_table end; 
*)
  print_time syms "[flx_opt]; Finding roots" begin fun () ->
  (* Find the root and exported functions and types. *)
  Flx_use.find_roots syms bsym_table root_proc syms.Flx_mtypes2.bifaces end;

  let bsym_table = 
   print_time syms "[flx_opt]; Monomorphising" begin fun () ->
  (* monomorphise *)
  Flx_numono.monomorphise2 true syms bsym_table end
  in

  print_time syms "[flx_opt]; Verifying typeclass elimination" begin fun () ->
  (* check no typeclasses are left *)
  Flx_bsym_table.iter
  (fun id pa sym -> 
     match sym.Flx_bsym.bbdcl with 
     | Flx_bbdcl.BBDCL_axiom
     | Flx_bbdcl.BBDCL_lemma
     | Flx_bbdcl.BBDCL_reduce
     | Flx_bbdcl.BBDCL_invalid  
     | Flx_bbdcl.BBDCL_module 
     | Flx_bbdcl.BBDCL_instance _ 
     | Flx_bbdcl.BBDCL_typeclass _ 
       -> assert false 
     | _ -> () 
  )
  bsym_table
  end;

  print_time syms "[flx_opt]; Simplify requirements" begin fun () ->
  Flx_breqs.simplify_reqs bsym_table end;

 
  (* eliminate funprods, replace by calls to generated funs *)
  let bsym_table = Flx_funprod.elim_funprods syms bsym_table in
  (* eliminate funsums , replace by calls to generated funs *)
  let bsym_table = Flx_funsum.elim_funsums syms bsym_table in
  (* eliminate lrangles, replace by calls to generated funs *)
  let bsym_table = Flx_lrangle.elim_lrangles syms bsym_table in
  (* eliminate lrbracks, replace by calls to generated funs *)
  let bsym_table = Flx_lrbrack.elim_lrbracks syms bsym_table in


  let bsym_table = 
  print_time syms "[flx_opt]; Downgrading abstract types to representations" begin fun () ->
  (* Downgrade abstract types now. *)
  Flx_strabs.strabs bsym_table end 
  in

  print_time syms "[flx_opt]; Verifying abstract type elimination" begin fun () -> 
  (* check no abstract types are left *)
  Flx_bsym_table.iter
  (fun id pa sym -> 
     match sym.Flx_bsym.bbdcl with 
     | Flx_bbdcl.BBDCL_newtype _ -> assert false 
     | _ -> () 
  )
  bsym_table
  end ;

  let bsym_table = print_time syms "[flx_opt]; Removing unused symbols" begin fun () ->
  (* Clean up the symbol table. *)
  Flx_use.copy_used syms bsym_table end
  in

  let bsym_table = print_time syms "[flx_opt]; Uncurrying curried function" begin fun () -> 
  (* Uncurry curried functions. *)
  uncurry_functions syms bsym_table end
  in

(*
print_endline "Uncurrying DONE";
*)
  let bsym_table = print_time syms "[flx_opt]; Generating wrappers (new)" begin fun () ->
  (* make wrappers for non-function functional values *)
  Flx_mkcls2.make_wrappers syms bsym_table end
  in
(*
print_endline "Wrapper generation DONE";
*)

  print_time syms "[flx_opt]; Set SVC funs inline " begin fun () -> 
  Flx_svc.svc_set_inline syms bsym_table end;

  print_time syms "[flx_opt]; Inlining" begin fun () -> 
  (* Perform the inlining. *)
  Flx_inline.heavy_inlining syms bsym_table end;

  let bsym_table = print_time syms "[flx_opt]; Remove unused symbols" begin fun () -> 
  (* Clean up the symbol table. *)
  Flx_use.copy_used syms bsym_table end
  in

  let bsym_table = print_time syms "[flx_opt]; Expanding Coercions (new)" begin fun () ->
  (* make wrappers for non-function functional values *)
  Flx_xcoerce.expand_coercions syms bsym_table end
  in

  let bsym_table = print_time syms "[flx_opt]; Stripping Lambdas (new)" begin fun () ->
  (* make wrappers for non-function functional values *)
  Flx_lambda.strip_lambdas syms bsym_table end
  in


(*
print_endline "Unused symbols removed";
*)
(*
  let bsym_table = print_time syms "[flx_opt]; Converting functions to procedures" begin fun () ->
  (* convert functions to procedures *)
  mkproc syms bsym_table end
  in
*)

(*
print_endline "Dead code elim";
*)
  print_time syms "[flx_opt]; Eliminate dead code" begin fun () ->
  (* Eliminate dead code. *)
  let elim_state = Flx_elim.make_elim_state syms bsym_table in
  Flx_elim.eliminate_unused elim_state end;
(*
print_endline "DONE Dead code elim";
*)
  Flx_svc.svc_check syms bsym_table;

  let bsym_table = print_time syms "[flx_opt]; Do stack call optimisation" begin fun () ->
  (* Convert functions into stack calls. *)
  stack_calls syms bsym_table end
  in

  print_time syms "[flx_opt]; Mark heap closures" begin fun () ->
  Flx_mkcls.mark_heap_closures syms bsym_table end;

  (* new once check by control flow analysis *)
  Flx_once.once_bsym_table bsym_table syms.Flx_mtypes2.counter;

  bsym_table


let optimize_bsym_table syms bsym_table root_proc =
  print_time syms "[flx_opt]; optimisation pass complete" begin fun () -> 
  optimize_bsym_table' syms bsym_table root_proc 
end


