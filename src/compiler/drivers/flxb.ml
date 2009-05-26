(* name binding test harness *)

open Flx_ast
open Flx_util
open Flx_desugar
open Flx_bbind
open Flx_print
open Flx_types
open Flx_symtab
open Flx_getopt
open Flx_version
open Flx_flxopt
open Flx_exceptions
open Flx_set
open Flx_mtypes2
open Flx_use
open Flx_typing
;;
Flx_version_hook.set_version ();;

let dfltvs_aux = { raw_type_constraint=`TYP_tuple []; raw_typeclass_reqs=[]}
let dfltvs = [],dfltvs_aux

let print_help () = print_options(); exit(0)
;;
let reverse_return_parity = ref false
;;

try
  let argc = Array.length Sys.argv in
  if argc <= 1
  then begin
    print_endline "usage: flxg --key=value ... filename; -h for help";
    raise (Exit 0)
  end
  ;
  let raw_options = parse_options Sys.argv in
  let compiler_options = get_felix_options raw_options in
  reverse_return_parity :=  compiler_options.reverse_return_parity
  ;
  let syms = make_syms compiler_options in

  if check_keys raw_options ["h"; "help"]
  then print_help ()
  ;
  if check_key raw_options "version"
  then (print_endline ("Felix Version " ^ !version_data.version_string))
  ;
  if compiler_options.print_flag then begin
    print_string "//Include directories = ";
    List.iter (fun d -> print_string (d ^ " "))
    compiler_options.include_dirs;
    print_endline ""
  end
  ;

  let filename =
    match get_key_value raw_options "" with
    | Some s -> s
    | None -> exit 0
  in
  let filebase = filename in
  let input_file_name = filebase ^ ".flx"
  and iface_file_name = filebase ^ ".fix"
  and module_name =
    let n = String.length filebase in
    let i = ref (n-1) in
    while !i <> -1 && filebase.[!i] <> '/' do decr i done;
    String.sub filebase (!i+1) (n - !i - 1)
  in

  (* PARSE THE IMPLEMENTATION FILE *)
  print_endline ("//Parsing Implementation " ^ input_file_name);
  let parser_state = List.fold_left
    (Flx_parse.parse_file ~include_dirs:compiler_options.include_dirs)
    (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
    (compiler_options.auto_imports @ [input_file_name])
  in
  let parse_tree = List.rev (Flx_parse.parser_data parser_state) in
  let have_interface = Sys.file_exists iface_file_name in
  print_endline (Flx_print.string_of_compilation_unit parse_tree);
  print_endline "//PARSE OK";

  let include_dirs =  (* (Filename.dirname input_file_name) :: *) compiler_options.include_dirs in
  let compiler_options = { compiler_options with include_dirs = include_dirs } in
  let syms = { syms with compiler_options = compiler_options } in
  let deblocked = desugar_program syms module_name parse_tree in

  let root = !(syms.counter) in
  print_endline ("//Top level module '"^module_name^"' has index " ^ si root);

  let table, _, exes, ifaces,dirs =
    build_tables syms "root" dfltvs 0 None None root deblocked
  in
    print_endline "//BINDING EXECUTABLE CODE";
    print_endline "//-----------------------";
    let bbdfns = bbind syms in
    let child_map = Flx_child.cal_children syms bbdfns in
    let bifaces = bind_ifaces syms ifaces in
    print_endline "//Binding complete";

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
      if compiler_options.print_flag
      then print_endline ("//root module's init procedure has index " ^ si index);
      index
    in

    Hashtbl.iter
    (fun index (name,parent,sr,entry) -> print_endline
      (
        si index ^ " --> " ^
        string_of_bbdcl syms.dfns bbdfns entry index
      )
    )
    bbdfns

with x -> Flx_terminate.terminate !reverse_return_parity x
;;
