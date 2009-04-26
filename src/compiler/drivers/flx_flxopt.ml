open Flx_set
open Flx_mtypes2
open Flx_getopt

let print_chosen options =
  print_endline
  (String.concat ", "
    (List.map
      (fun (a, b) ->
        a ^ "=" ^ b
      )
      options
    )
  )

let get_felix_options options =
  {
    optimise    = check_keys options ["opt"; "optimise"];
    debug       = check_key options "debug";
    document_grammar = check_key options "document-grammar";
    document_typeclass = check_key options "document-typeclass";
    with_comments = check_key options "with-comments";
    mangle_names = check_key options "mangle-names";
    include_dirs= get_keys_values options ["I"; "include"];
    print_flag  = check_keys options ["v"; "verbose"];
    generate_axiom_checks  = not (check_keys options ["no-check-axioms"]);
    trace       = check_keys options ["trace" ];
    files       = get_key_values options "";
    raw_options = options;
    reverse_return_parity = check_key options "e";
    force_recompile = check_keys options ["force"];
    cache_dir = get_key_value options "cache_dir";
    output_dir = get_key_value options "output_dir";
    max_inline_length =
      begin
        let inline =
          match get_key_value options "inline" with
          | Some i ->
            (
              if i = "none" then 0 else
              if i = "" then 50 else
                try
                  int_of_string i
                with _ ->
                  failwith ("Invalid value for inline: '" ^ i^"'")
            )
          | None ->
            begin match check_key options "noinline" with
            | true -> 0
            | false ->
              begin match check_keys options ["inline";"opt";"optimise"] with
              | true -> 50
              | false -> 5
              end
            end
        in
          (* we need to at least inline a little for typeclasses *)
          if inline < 1 then 1 else inline
      end
    ;
    auto_imports = get_key_values options "import"
    ;

    compile_only = check_keys options ["c";"compile-only"]
  }

let print_options () =
  print_endline "options:";
  print_endline "  -h, --help : print this help";
  print_endline "  --version: print version info";
  print_endline "  -v, --verbose: print symbol table";
  print_endline "  -c, --compile-only: no code generation";
  print_endline "  -Idir, --include=dir : append dir to include path";
  print_endline "  --import=file.flxh : automatically #import <file.flxh>";
  print_endline "  --inline, --noinline, --optimise";
  print_endline "  --cache_dir=<none>: .par and .syncache directory";
  print_endline "  --output_dir=<none>: .cpp, .hpp, .why etc directory";
  print_endline "  --force : force recompilation";
  print_endline "  --with-comments : generate code with comments";
  print_endline "  --mangle-names : generate code with fully mangled names"

let make_syms options =
  {
    registry = Hashtbl.create 97;
    counter = ref 1;
    dfns = Hashtbl.create 97;
    varmap = Hashtbl.create 97;
    ticache = Hashtbl.create 97;
    glr_cache = Hashtbl.create 97;
    env_cache = Hashtbl.create 97;
    compiler_options = options;
    instances = Hashtbl.create 97;
    include_files = ref [];
    roots = ref IntSet.empty;
    wrappers = Hashtbl.create 97;
    lexers = Hashtbl.create 7;
    parsers = Hashtbl.create 7;
    quick_names = Hashtbl.create 97;
    bifaces = [];
    reductions = [];
    axioms = [];
    variant_map = Hashtbl.create 97;
    typeclass_to_instance = Hashtbl.create 97;
    instances_of_typeclass = Hashtbl.create 97;
    transient_specialisation_cache = Hashtbl.create 97;
  }
