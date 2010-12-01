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

(* this stupid routine gets rid of all duplicate // in a filename, and also
 * any trailing /, since for some reason this confuses the include file
 * exclusion checks
 *)
let fixit file =
  let n = String.length file in
  let s = ref "" in
  let copy = ref true in
  for i = 0 to n-1 do
    let ch = String.sub file i 1 in
    if ch <> Flx_filesys.dir_sep then copy := true;
    if !copy then s := !s ^ ch;
    copy := ch <> Flx_filesys.dir_sep 
  done;
  let s = !s in
  let n = String.length s in
  let s = 
    if n>1 then
      if String.sub s (n-1) 1 = Flx_filesys.dir_sep 
      then String.sub s (n-1) 1 
      else s
    else s
  in 
  s

let fixup files = List.map fixit files

let get_felix_options options =
  {
    optimise    = check_keys options ["opt"; "optimise"];
    debug       = check_key options "debug";
    document_grammar = check_key options "document-grammar";
    document_typeclass = check_key options "document-typeclass";
    with_comments = check_key options "with-comments";
    mangle_names = check_key options "mangle-names";
    include_dirs= fixup (get_keys_values options ["I"; "include"]);
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
              if i = "" then 150 else
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
              | true -> 250
              | false -> 15
              end
            end
        in
          (* we need to at least inline a little for typeclasses *)
          if inline < 10 then 10 else inline
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
