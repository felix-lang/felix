(* code generation driver *)

open Flx_util
open Flx_ast
open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_bbind
open Flx_name
open Flx_tgen
open Flx_gen
open Flx_symtab
open Flx_getopt
open Flx_version
open Flx_exceptions
open Flx_flxopt
open Flx_ogen
open Flx_typing
open List
;;
Flx_version_hook.set_version ();;

let print_help () = print_options(); exit(0)
;;

let reverse_return_parity = ref false
;;

let last_time = ref 0.0
;;
let tim() =
  let now = (Unix.times()).Unix.tms_utime in
  let elapsed = now -. !last_time in
  last_time := now;
  elapsed
;;

let format_time tm =
  si (tm.Unix.tm_year + 1900) ^ "/" ^
  si (tm.Unix.tm_mon + 1) ^ "/" ^
  si tm.Unix.tm_mday ^ " " ^
  si tm.Unix.tm_hour ^ ":" ^
  si tm.Unix.tm_min ^ ":" ^
  si tm.Unix.tm_sec
;;

type file_state_t = [`NeverOpened | `Open of out_channel | `Closed ]

type out_file_t = {
  out_filename : string;
  mutable out_chan: file_state_t;
}

let force_open f =
  match f.out_chan with
  | `Open chan -> chan
  | _ ->
    let chan = open_out f.out_filename in
    f.out_chan <- `Open chan;
    chan

let ensure_closed f =
  match f.out_chan with
  | `NeverOpened | `Closed -> ()
  | `Open chan ->
    close_out chan;
    f.out_chan <- `Closed

let was_used f =
  match f.out_chan with
  | `NeverOpened -> false
  | _ -> true

let ws f x =
  let chan = force_open f in
  output_string chan x

let wl f x = ws f (x ^ "\n")

let mkf f = { out_filename=f; out_chan=`NeverOpened }

;;

try
  (* Time initialisation *)
  let compile_start = Unix.time () in
  let compile_start_gm = Unix.gmtime compile_start in
  let compile_start_local = Unix.localtime compile_start in
  let compile_start_gm_string = format_time compile_start_gm ^ " UTC" in
  let compile_start_local_string = format_time compile_start_local ^ " (local)" in


  (* Argument parsing *)
  let argc = Array.length Sys.argv in
  if argc <= 1
  then begin
    print_endline "usage: flxg --key=value ... filename; -h for help";
    exit 0
  end
  ;
  let raw_options = parse_options Sys.argv in
  let compiler_options = get_felix_options raw_options in
  reverse_return_parity := compiler_options.reverse_return_parity
  ;
  let syms = make_syms compiler_options in
  if check_keys raw_options ["h"; "help"]
  then print_help ()
  ;
  if check_key raw_options "version"
  then (print_endline ("Felix Version " ^ !version_data.version_string))
  ;
  let print_debug msg =
    if compiler_options.print_flag
    then print_endline msg
  in

  if compiler_options.print_flag then begin
    print_string "//Include directories = ";
    List.iter (fun d -> print_string (d ^ " "))
    compiler_options.include_dirs;
    print_endline ""
  end
  ;

  (* main filename processing *)
  (*
  List.iter (fun f ->
  print_endline ("File " ^ f))
  compiler_options.files
  ;
  *)
  let files = compiler_options.files in
  if length files = 0 then print_help();

  let filename = hd (rev files) in
  let inbase = filename in

  let input_file_name = inbase ^ ".flx" in
  (*
  and iface_file_name = filebase ^ ".fix"
  *)
  let outbase =
    match compiler_options.output_dir with
    | None -> filename
    | Some d -> Filename.concat d (Filename.basename filename)
  in
  let why_file_name = mkf (outbase ^ ".why")

  and header_file = mkf (outbase ^ ".hpp")
  and body_file = mkf (outbase ^ ".cpp")
  and ctors_file = mkf (outbase ^ "_ctors.cpp")
  and package_file = mkf (outbase ^ ".resh")
  and rtti_file = mkf (outbase ^ ".rtti")
  and report_file = mkf (outbase ^ ".xref")

  and module_name =
    let n = String.length inbase in
    let i = ref (n-1) in
    while !i <> -1 && inbase.[!i] <> '/' && inbase.[!i] <> '\\' do decr i done;
    String.sub inbase (!i+1) (n - !i - 1)
  in

  let include_dirs =  Filename.current_dir_name :: compiler_options.include_dirs in
  let compiler_options = { compiler_options with include_dirs = include_dirs } in
  let syms = { syms with compiler_options = compiler_options } in

  (* PARSE THE IMPLEMENTATION FILE *)

  let parse_tree =
    fold_left (fun tree file ->
      let file_name =
        if Filename.check_suffix file ".flx" then file else file ^ ".flx"
      in
      print_debug ("//Parsing Implementation " ^ file_name);
      let sts = Flx_colns.include_file syms file_name in
      concat [tree; sts]
    )
    []
    files
  in
  print_debug (Flx_print.string_of_compilation_unit parse_tree);

  let parse_time = tim() in
  print_debug ("//PARSE OK time " ^ string_of_float parse_time);

  print_debug "//DESUGARING";

  let desugar_state = Flx_desugar.make_desugar_state module_name syms in
  let asms = Flx_desugar.desugar_compilation_unit desugar_state parse_tree in
  let desugar_time = tim() in
  print_debug ("//DESUGAR time " ^ string_of_float desugar_time);

  (* THIS IS A HACK! *)
  let root = !(syms.counter) in
  print_debug ("//Top level module '" ^ module_name ^ "' has index " ^ si root);


  print_debug "//BUILDING TABLES";

  let _, _, exes, ifaces, _ = build_tables syms root asms in
  let build_table_time = tim() in
  print_debug ("//BUILDING TABLES time " ^ string_of_float build_table_time);


  print_debug "//BINDING EXECUTABLE CODE";
  let bbind_state = Flx_bbind.make_bbind_state syms in
  let bbdfns = bbind bbind_state in

  print_debug "//DOWNGRADING ABSTRACT TYPES";
  let bbdfns = Flx_strabs.strabs syms bbdfns in

  let child_map = Flx_child.cal_children syms bbdfns in
  Flx_typeclass.typeclass_instance_check syms bbdfns child_map;

  (* generate axiom checks *)
  if compiler_options.generate_axiom_checks then
  Flx_axiom.axiom_check syms bbdfns;

  (* generate why file *)
  Flx_why.emit_whycode why_file_name.out_filename syms bbdfns root
  ;


  syms.bifaces <- bind_ifaces bbind_state ifaces;
  Hashtbl.clear syms.ticache;

  let binding_time = tim() in

  print_debug ("//Binding complete time " ^ string_of_float binding_time);

  print_debug "//CHECKING ROOT";

  let root_proc =
    match
      try Hashtbl.find syms.dfns root
      with Not_found ->
        failwith
        (
          "Can't find root module " ^ si root ^
          " in symbol table?"
        )
    with {id=id; sr=sr; parent=parent;vs=vs;pubmap=name_map;symdef=entry} ->
    begin match entry with
      | `SYMDEF_module -> ()
      | _ -> failwith "Expected to find top level module ''"
    end
    ;
    let entry =
      try Hashtbl.find name_map "_init_"
      with Not_found ->
        failwith "Can't find name _init_ in top level module's name map"
    in
    let index = match entry with
      | `FunctionEntry [x] -> sye x
      | `FunctionEntry [] -> failwith "Couldn't find '_init_'"
      | `FunctionEntry _ -> failwith "Too many top level procedures called '_init_'"
      | `NonFunctionEntry _ -> failwith "_init_ found but not procedure"
    in
    print_debug ("//root module's init procedure has index " ^ si index);
    index
  in

  print_debug "//OPTIMISING";
  let () = Flx_use.find_roots syms bbdfns root_proc syms.bifaces in
  let bbdfns = Flx_use.copy_used syms bbdfns in
  let child_map = Flx_child.cal_children syms bbdfns in
  (*
  DISABLE TEMPORARILY DUE TO BUG
  *)
  let bbdfns = ref bbdfns in
  let child_map = ref child_map in
  let counter = ref 0 in
  while Flx_uncurry.uncurry_gen syms (!child_map,!bbdfns) > 0 do
    incr counter;
    if !counter > 10 then failwith "uncurry exceeded 10 passes";
    bbdfns := Flx_use.copy_used syms !bbdfns;
    child_map := Flx_child.cal_children syms !bbdfns;
  done;
  let bbdfns = !bbdfns and child_map = !child_map in


  let bbdfns = if compiler_options.max_inline_length > 0 then
  begin
    if compiler_options.print_flag then begin
      print_endline "";
      print_endline "---------------------------";
      print_endline "INPUT TO OPTIMISATION PASS";
      print_endline "---------------------------";
      print_endline "";
      print_symbols syms.dfns bbdfns
   end;

    syms.reductions <- Flx_reduce.remove_useless_reductions syms bbdfns syms.reductions;
    Flx_typeclass.fixup_typeclass_instances syms bbdfns;
    let bbdfns = Flx_use.copy_used syms bbdfns in
    let child_map = Flx_child.cal_children syms bbdfns in
    Flx_inline.heavy_inlining syms (child_map,bbdfns);
    let bbdfns = Flx_use.copy_used syms bbdfns in
    let child_map = Flx_child.cal_children syms bbdfns in

    print_debug "PHASE 1 INLINING COMPLETE";
    if compiler_options.print_flag then begin
      print_endline "";
      print_endline "---------------------------";
      print_endline "POST PHASE 1 FUNCTION SET";
      print_endline "---------------------------";
      print_endline "";
      print_symbols syms.dfns bbdfns
    end;

    Hashtbl.iter
    (fun i _ ->
      Flx_prop.rem_prop bbdfns `Inlining_started i;
      Flx_prop.rem_prop bbdfns `Inlining_complete i;
    )
    bbdfns
    ;

    Flx_inst.instantiate syms bbdfns true root_proc syms.bifaces;
    (* EXPERIMENTAL!
      Adds monomorphic versions of all symbols.
      This will do nothing, because they're not
      actually instantiated!
    *)
    print_debug "//MONOMORPHISING";
    Flx_mono.monomorphise syms bbdfns;
    print_debug "//MONOMORPHISING DONE";

    let bbdfns = Flx_use.copy_used syms bbdfns in
    let child_map = Flx_child.cal_children syms bbdfns in

    if compiler_options.print_flag then begin
      print_endline "";
      print_endline "---------------------------";
      print_endline "POST MONOMORPHISATION FUNCTION SET";
      print_endline "---------------------------";
      print_endline "";
      print_symbols syms.dfns bbdfns
    end;

    print_debug "//Removing useless reductions";

    syms.reductions <- Flx_reduce.remove_useless_reductions syms bbdfns syms.reductions;

    print_debug "//INLINING";

    Flx_typeclass.fixup_typeclass_instances syms bbdfns;
    let bbdfns = Flx_use.copy_used syms bbdfns in
    let child_map = Flx_child.cal_children syms bbdfns in
    Flx_inline.heavy_inlining syms (child_map,bbdfns);
    (*
    print_endline "INLINING DONE: RESULT:";
    print_symbols syms.dfns bbdfns;
    *)
    bbdfns
  end
  else bbdfns
  in

  let bbdfns = Flx_use.copy_used syms bbdfns in
  let child_map = Flx_child.cal_children syms bbdfns in

  let bbdfns = ref bbdfns in
  let child_map = ref child_map in
  let counter = ref 0 in
  while Flx_mkproc.mkproc_gen syms (!child_map,!bbdfns) > 0 do
    incr counter;
    if !counter > 10 then failwith "mkproc exceeded 10 passes";
    bbdfns := Flx_use.copy_used syms !bbdfns;
    child_map := Flx_child.cal_children syms !bbdfns;
  done;
  let bbdfns = !bbdfns and child_map = !child_map in

  (*
  print_endline "Discarding crud .. left with:";
  print_symbols syms.dfns bbdfns;
  *)


  let rec lvof x = match x with
    | `BEXPR_name (i,_),_ -> i
    | `BEXPR_get_n (_,e),_ -> lvof e
    | _ -> 0 (* assume 0 isn't the index of any variable *)
  in
  let elim_init maybe_unused exes =
    List.filter (function
      | `BEXE_init (_,i,_) -> not (IntSet.mem i maybe_unused)
      | `BEXE_assign (_,x,_) -> not (IntSet.mem (lvof x) maybe_unused)
      | _ -> true
    )
    exes
  in
  let elim_pass () =
    print_debug "Elim pass";
    (* check for unused things .. possible, just a diagnostic for now *)
    let full_use = Flx_use.full_use_closure syms bbdfns in
    let partial_use = Flx_use.cal_use_closure syms bbdfns false in
    let maybe_unused = IntSet.diff full_use partial_use in

    Hashtbl.iter
    (fun i (id,parent,sr,entry) -> match entry with
    | `BBDCL_procedure (props ,bvs,(ps,tr),exes) ->
      let exes = elim_init maybe_unused exes in
      let entry = `BBDCL_procedure (props,bvs,(ps,tr),exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry)

    | `BBDCL_function (props,bvs,(ps,rt),ret,exes) ->
      let exes = elim_init maybe_unused exes in
      let entry = `BBDCL_function (props,bvs,(ps,rt),ret,exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry)

    | _ -> ()
    )
    bbdfns
    ;

    IntSet.iter
    (fun i->
      let id,_,_,_ = Hashtbl.find bbdfns i in
      print_debug ("Removing unused " ^ id ^ "<" ^ si i ^ ">");
      Hashtbl.remove bbdfns i
    )
    maybe_unused
    ;
    IntSet.is_empty maybe_unused
  in

  while not (elim_pass ()) do () done;


  (*
  print_symbols syms.dfns bbdfns;
  *)

  Flx_typeclass.fixup_typeclass_instances syms bbdfns;
  print_debug "//Calculating stackable calls";
  let label_map = Flx_label.create_label_map bbdfns syms.counter in
  let label_usage = Flx_label.create_label_usage syms bbdfns label_map in
  let label_info = label_map, label_usage in

  Flx_stack_calls.make_stack_calls syms (child_map,bbdfns) label_map label_usage;

  let opt_time = tim() in

  print_debug ("//Optimisation complete time " ^ string_of_float opt_time);


  print_debug "//Generating primitive wrapper closures";
  Flx_mkcls.make_closures syms bbdfns;
  let child_map = Flx_child.cal_children syms bbdfns in

  if compiler_options.print_flag then
  begin
    let f = force_open report_file in
    Flx_call.print_call_report syms bbdfns f;
    ensure_closed report_file
  end
  ;

  print_debug "//Finding which functions use globals";
  let bbdfns = Flx_use.copy_used syms bbdfns in
  Flx_global.set_globals syms bbdfns;
  let child_map = Flx_child.cal_children syms bbdfns in

  (*
  print_symbols syms.dfns bbdfns;
  *)

  print_debug "//instantiating";

  Flx_inst.instantiate syms bbdfns false root_proc syms.bifaces;

  let label_map = Flx_label.create_label_map bbdfns syms.counter in
  let label_usage = Flx_label.create_label_usage syms bbdfns label_map in
  let label_info = label_map, label_usage in


  let top_class =
    try cpp_instance_name syms bbdfns root_proc []
    with Not_found ->
      failwith ("can't name instance of root _init_ procedure index " ^ si root_proc)
  in

  (* fix up root procedures so if they're not stackable,
     then they need a heap closure -- wrappers require
     one or the other
  *)
  IntSet.iter (fun i ->
    let id,parent,sr,entry = Hashtbl.find bbdfns i in
    match entry with
    | `BBDCL_procedure (props,vs,p,exes) ->
      let props = ref props in
      if List.mem `Stackable !props then begin
        if not (List.mem `Stack_closure !props)
        then props := `Stack_closure :: !props
      end else begin
        if not (List.mem `Heap_closure !props)
        then props := `Heap_closure :: !props
      end
      ;
      if not (List.mem `Requires_ptf !props)
      then props := `Requires_ptf :: !props
      ;
      let entry = `BBDCL_procedure (!props, vs,p,exes) in
      Hashtbl.replace bbdfns i (id,parent,sr,entry)
    | _ -> ()

  )
  !(syms.roots)
  ;
  (* FUDGE the init procedure to make interfacing a bit simpler *)
  let topclass_props =
    let id,parent,sr,entry = Hashtbl.find bbdfns root_proc in
    match entry with
    | `BBDCL_procedure (props,vs,p,exes) -> props
    | _ -> syserr sr "Expected root to be procedure"
  in
  print_debug ("//root module's init procedure has name " ^ top_class);

  let instantiation_time = tim() in

  print_debug ("//instantiation time " ^ string_of_float instantiation_time);

  if compiler_options.compile_only
  then exit (if compiler_options.reverse_return_parity then 1 else 0)
  ;

  begin
  let find_parsers this sr e = match e with
    | _ -> ()
  in

  let nul x = () in
  Hashtbl.iter
  (fun i (_,_,_,entry) -> match entry with
  | `BBDCL_function (_,_,_,_,exes)
  | `BBDCL_procedure (_,_,_,exes) ->
    List.iter
      (fun exe ->
         let sr = src_of_bexe exe in
         Flx_maps.iter_bexe nul (find_parsers i sr) nul nul nul exe
      )
    exes
  | _ -> ()
  )
  bbdfns
  end
  ;

  let sr = ("unknown",0,0,0,0) in

  let psh s = ws header_file s in
  let psb s = ws body_file s in
  let psp s = ws package_file s in
  let psr s = ws rtti_file s in

  let plh s = psh s; psh "\n" in
  let plb s = psb s; psb "\n" in
  let plr s = psr s; psr "\n" in
  let plp s = psp s; psp "\n" in

  print_debug "//GENERATING Package Requirements";

  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      match
        try Hashtbl.find bbdfns i
        with Not_found -> failwith ("[package] can't find index " ^ si i)
      with (id,parent,sr,entry) ->
      match entry with
      | `BBDCL_insert (_,s,`Package,_) ->
        begin match s with
        | `Identity | `Str "" | `StrTemplate "" -> ()
        | _ ->
          let s =
            match s with
            | `Identity -> assert false (* covered above *)
            | `Virtual -> clierr sr "Instantiate virtual insertion!"
            | `Str s -> Flx_cexpr.ce_expr "atom" s
            | `StrTemplate s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plp s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;


  print_debug "//GENERATING C++: user headers";

  plh ("#ifndef _FLX_GUARD_" ^ cid_of_flxid module_name);
  plh ("#define _FLX_GUARD_" ^ cid_of_flxid module_name);
  plh ("//Input file: " ^ input_file_name);
  plh ("//Generated by Felix Version " ^ !version_data.version_string);
  plh ("//Timestamp: " ^ compile_start_gm_string);
  plh ("//Timestamp: " ^ compile_start_local_string);
  plh "";
  plh "#ifndef FLX_NO_INCLUDES";
  plh ("#include \"" ^ module_name ^ ".includes\"");
  plh "#endif";
  plh "//FELIX RUNTIME";
  (* plh "#include \"flx_rtl.hpp\""; *)
  plh "using namespace flx::rtl;";
  (* plh "#include \"flx_gc.hpp\""; *)
  plh "using namespace flx::gc::generic;";
  plh "";

  plh "\n//-----------------------------------------";
  plh "//USER HEADERS";
  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts)->
      match
        try Hashtbl.find bbdfns i
        with Not_found -> failwith ("[user header] can't find index " ^ si i)
      with (id,parent,sr,entry) ->
      match entry with
      | `BBDCL_insert (_,s,`Header,_) ->
        begin match s with
        | `Identity | `Str "" | `StrTemplate "" -> ()
        | _ ->
          let s =
            match s with
            | `Identity -> assert false
            | `Virtual -> clierr sr "Instantiate virtual insertion!"
            | `Str s -> Flx_cexpr.ce_expr "atom" s
            | `StrTemplate s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plh s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;

  (* HACKERY FOR ELKHOUND -- we force include library files
    into the global namespace, macro guards should prevent
    subsequent inclusion in the module namespace
  *)
  if Hashtbl.length syms.lexers <> 0 then begin
    plh "#include \"elk_lexerint.h\""
  end
  ;

  if Hashtbl.length syms.parsers <> 0 then begin
    plh "#include \"elk_useract.h\""
  end
  ;

  plh "\n//-----------------------------------------";
  List.iter plh [
  "//FELIX SYSTEM";
  "namespace flxusr { namespace " ^ cid_of_flxid module_name ^ " {";
  "struct thread_frame_t;"
  ]
  ;
  print_debug "//GENERATING C++: collect types";
  let types = ref [] in
    Hashtbl.iter
    (fun t index-> types := (index, t) :: !types)
    syms.registry
  ;
  let types =
    List.sort
    (
      fun a1 a2 -> compare (fst a1) (fst a2)
    )
    !types
  in
  (*
  List.iter
  (fun (_,t) -> print_endline (string_of_btypecode dfns t))
  types
  ;
  *)

  print_debug "//GENERATING C++: type class names";
  plh "\n//-----------------------------------------";
  plh "//NAME THE TYPES";
  plh  (gen_type_names syms bbdfns types);

  print_debug "//GENERATING C++: type class definitions";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE THE TYPES";
  plh  (gen_types syms bbdfns types);

  if not (Hashtbl.length syms.parsers + Hashtbl.length syms.lexers = 0) then begin
  plp "elk";
  plh "\n//-----------------------------------------";
  plh  "//ELKHOUND OBJECTS, forward declaration";
  Hashtbl.iter
  (fun _ n -> plh ("struct ElkLex_"^si n^";"))
  syms.lexers
  ;
  Hashtbl.iter
  (fun _ n -> plh ("struct Elk_"^si n^";"))
  syms.parsers
  end
  ;
  print_debug "//GENERATING C++: function and procedure classes";
  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASS NAMES";
  plh  (gen_function_names syms (child_map,bbdfns));

  plh "\n//-----------------------------------------";
  plh  "//DEFINE FUNCTION CLASSES";
  plh  (gen_functions syms (child_map,bbdfns));

  if not (Hashtbl.length syms.parsers + Hashtbl.length syms.lexers = 0) then begin
  plh "\n//-----------------------------------------";
  plh  "//INCLUDE ELKHOUND PARSERS";
  Hashtbl.iter
  (fun _ n -> plh ("#include \""^module_name^"_lexer_"^si n^".hpp\""))
  syms.lexers
  ;
  Hashtbl.iter
  (fun _ n -> plh ("#include \""^module_name^"_parser_"^si n^".h\""))
  syms.parsers
  end
  ;

  let topvars_with_type = find_thread_vars_with_type bbdfns in
  let topvars = List.map fst topvars_with_type in
  List.iter plh
  [
  "struct thread_frame_t {";
  "  int argc;";
  "  char **argv;";
  "  FILE *flx_stdin;";
  "  FILE *flx_stdout;";
  "  FILE *flx_stderr;";
  "  gc_profile_t *gcp;";
  "  thread_frame_t(";
  "  );";
  ]
  ;
  plh (format_vars syms bbdfns topvars []);
  plh "};";
  plh "";
  plh "FLX_DCL_THREAD_FRAME";
  plh "";
  plh ("}} // namespace flxusr::" ^ cid_of_flxid module_name);

  (* BODY *)
  print_debug "//GENERATING C++: GC ptr maps & offsets";

  plb ("//Input file: " ^ input_file_name);
  plb ("//Generated by Felix Version " ^ !version_data.version_string);
  plb ("//Timestamp: " ^ compile_start_gm_string);
  plb ("//Timestamp: " ^ compile_start_local_string);

  plb ("#include \"" ^ module_name ^ ".hpp\"");
  plb "#include <stdio.h>"; (* for diagnostics *)

  if Hashtbl.length syms.parsers <> 0 then begin
    plb "#include \"elk_glr.h\""
  end
  ;

  plb "#define comma ,";
  plb "\n//-----------------------------------------";
  plb "//EMIT USER BODY CODE";
  (* These must be in order: build a list and sort it *)
  begin
    let dfnlist = ref [] in
    Hashtbl.iter
    (fun (i,ts) _ -> dfnlist := (i,ts) :: !dfnlist)
    syms.instances
    ;
    let insts = Hashtbl.create 97 in
    List.iter
    (fun (i,ts) ->
      match
        try Hashtbl.find bbdfns i
        with Not_found -> failwith ("[user body] can't find index " ^ si i)
      with (id,parent,sr,entry) ->
      match entry with
      | `BBDCL_insert (_,s,`Body,_) ->
        begin match s with
        | `Identity | `Str "" | `StrTemplate "" -> ()
        | _ ->
          let s =
            match s with
            | `Identity -> assert false
            | `Virtual -> clierr sr "Instantiate virtual insertion!"
            | `Str s -> Flx_cexpr.ce_expr "atom" s
            | `StrTemplate s ->
              (* do we need tsubst vs ts t? *)
              let tn t = cpp_typename syms t in
              let ts = List.map tn ts in
              Flx_csubst.csubst sr sr s (Flx_cexpr.ce_atom "Error") [] [] "Error" "Error" ts "atom" "Error" ["Error"] ["Error"] ["Error"]
          in
          let s = Flx_cexpr.sc "expr" s in
          if not (Hashtbl.mem insts s) then
          begin
            Hashtbl.add insts s ();
            plb s
          end
        end
      | _ -> ()
    )
    (List.sort compare !dfnlist)
  end
  ;

  plb "\n//-----------------------------------------";
  plb ("namespace flxusr { namespace " ^ cid_of_flxid module_name ^ " {");

  plb "FLX_DEF_THREAD_FRAME";
  plb "//Thread Frame Constructor";

  let sr = Flx_srcref.make_dummy "Thread Frame" in
  let topfuns = List.filter (fun (_,t) -> is_gc_pointer syms bbdfns sr t) topvars_with_type in
  let topfuns = List.map fst topfuns in
  let topinits =
    [
      "  gcp(0)"
    ]
    @
    List.map
    (fun index ->
      "  " ^
      cpp_instance_name syms bbdfns index [] ^
      "(0)"
    )
    topfuns
  in
  let topinits = String.concat ",\n" topinits in
  List.iter plb
  [
  "thread_frame_t::thread_frame_t(";
  ") :";
  topinits;
  "{}"
  ];

  plr (Flx_ogen.gen_offset_tables syms (child_map,bbdfns) module_name);
  ensure_closed rtti_file;
  if was_used rtti_file then begin
    plb "\n//-----------------------------------------";
    plb "//DEFINE OFFSET tables for GC";
    plb ("#include \""^module_name^".rtti\"");
  end;

  begin
    let header_emitted = ref false in
    Hashtbl.iter
    (fun (fno,_) inst ->
      try
        let labels = Hashtbl.find label_map fno in
        Hashtbl.iter
        (fun lab lno ->
          match Flx_label.get_label_kind_from_index label_usage lno with
          | `Far ->
            if not !header_emitted then begin
              plb "\n//-----------------------------------------";
              plb "#if FLX_CGOTO";
              plb "//DEFINE LABELS for GNUC ASSEMBLER LABEL HACK";
              header_emitted := true;
            end
            ;
            plb ("FLX_DECLARE_LABEL(" ^ si lno ^ ","^ si inst ^ "," ^ lab^")")
          | `Near -> ()
          | `Unused -> ()
        )
        labels
      with Not_found -> ()
    )
    syms.instances
    ;
    if !header_emitted then plb "#endif";
  end
  ;
  if not (Hashtbl.length syms.parsers + Hashtbl.length syms.lexers = 0) then begin
  plb "\n//-----------------------------------------";
  plb  "//INCLUDE ELKHOUND PARSERS";
  Hashtbl.iter
  (fun _ n -> plb ("#include \""^module_name^"_lexer_"^si n^".cpp\""))
  syms.lexers
  ;

  plb "#include \"elk_glr.h\"";
  Hashtbl.iter
  (fun _ n -> plb ("#include \""^module_name^"_parser_"^si n^".cc\""))
  syms.parsers
  end
  ;

  print_debug "//GENERATING C++: method bodies";

  plb "\n//-----------------------------------------";
  plb "//DEFINE FUNCTION CLASS METHODS";
  plb ("#include \"" ^module_name ^ "_ctors.cpp\"");
  gen_execute_methods body_file.out_filename syms (child_map,bbdfns) label_info syms.counter (force_open body_file) (force_open ctors_file);

  print_debug "//GENERATING C++: interface";
  plb "\n//-----------------------------------------";
  plb ("}} // namespace flxusr::" ^ cid_of_flxid module_name);

  plb "//CREATE STANDARD EXTERNAL INTERFACE";
  plb ("FLX_FRAME_WRAPPERS(flxusr::" ^ cid_of_flxid module_name ^ ")");
  (if List.mem `Pure topclass_props then
    plb ("FLX_C_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  else if List.mem `Stackable topclass_props then
    plb ("FLX_STACK_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  else
    plb ("FLX_START_WRAPPER(flxusr::" ^ cid_of_flxid module_name ^ "," ^ top_class ^ ")")
  );
  plb "\n//-----------------------------------------";

  plh ("using namespace flxusr::" ^ cid_of_flxid module_name ^ ";");
  if List.length syms.bifaces > 0 then begin
    plh "//DECLARE USER EXPORTS";
    plh (gen_biface_headers syms bbdfns syms.bifaces);
    plb "//DEFINE EXPORTS";
    plb (gen_biface_bodies syms bbdfns syms.bifaces);
    plb (gen_python_module module_name syms bbdfns syms.bifaces);
  end
  ;

  (* rather late: generate variant remapping tables *)
  if Hashtbl.length syms.variant_map > 0 then begin
    plr "// VARIANT REMAP ARRAYS";
    Hashtbl.iter
    (fun (srct,dstt) vidx ->
      match srct,dstt with
      | `BTYP_variant srcls, `BTYP_variant dstls ->
        begin
          let rcmp (s,_) (s',_) = compare s s' in
          let srcls = List.sort rcmp srcls in
          let dstls = List.sort rcmp dstls in
          let n = List.length srcls in
          let remap =
            List.map
            (fun (s,_) ->
              match Flx_list.list_assoc_index dstls s with
              | Some i -> i
              | None -> assert false
            )
            srcls
          in
          plr ("static int vmap_" ^ si vidx^ "["^si n^"]={" ^
            catmap "," (fun i -> si i) remap ^
          "};")
        end
      | _ -> failwith "Remap non variant types??"
    )
    syms.variant_map
  end
  ;
  plh "//header complete";
  plh "#endif";
  plb "//body complete";
  ensure_closed header_file;
  ensure_closed body_file;
  ensure_closed ctors_file;
  plp "flx";
  plp "flx_gc";  (* RF: flx apps now need flx_gc. is this the way to do it? *)
  ensure_closed package_file;
  let code_generation_time = tim() in
  print_debug ("//code generation time " ^ string_of_float code_generation_time);

  let total_time =
    parse_time +.
    desugar_time +.
    build_table_time +.
    binding_time +.
    opt_time +.
    instantiation_time +.
    code_generation_time
  in
  print_debug ("//Felix compiler time " ^ string_of_float total_time);
  let fname = "flxg_stats.txt" in
  let
    old_parse_time,
    old_desugar_time,
    old_build_table_time,
    old_binding_time,
    old_opt_time,
    old_instantiation_time,
    old_code_generation_time,
    old_total_time
  =
  let zeroes = 0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0 in
  let f = try Some (open_in fname) with _ -> None in
  begin match f with
  | None -> zeroes
  | Some f ->
    let x =
      try
        let id x1 x2 x3 x4 x5 x6 x7 x8 = x1, x2, x3, x4, x5, x6, x7, x8 in
        Scanf.fscanf f
        "parse=%f desugar=%f build=%f bind=%f opt=%f inst=%f gen=%f tot=%f"
        id
      with _ -> zeroes
    in close_in f; x
  end
  in
    try (* failure to save stats isn't fatal *)
      let f = open_out fname in
      Printf.fprintf
        f
        "parse=%f\ndesugar=%f\nbuild=%f\nbind=%f\nopt=%f\ninst=%f\ngen=%f\ntot=%f\n"
        (old_parse_time +. parse_time)
        (old_desugar_time +. desugar_time)
        (old_build_table_time +. build_table_time)
        (old_binding_time +. binding_time)
        (old_opt_time +. opt_time)
        (old_instantiation_time +. instantiation_time)
        (old_code_generation_time +. code_generation_time)
        (old_total_time +. total_time)
      ;
      close_out f
    with _ -> ()
    ;
  exit (if compiler_options.reverse_return_parity then 1 else 0)

with x ->
  Flx_terminate.terminate !reverse_return_parity x
;;
