(* Desugaring
 *
 * Two routines: one to build interfaces from modules, and one to lift lambdas
 * and also blocks.
 *)

open Flx_ast
open Flx_types
open Flx_mtypes2
open List
open Flx_exceptions
open Flx_util
open Flx_version

type nsrec = {
  name:string;
  sr:Flx_srcref.t;
  vs:vs_list_t;
  fudges:string list ref
}

let include_file syms inspec =
  let force = syms.compiler_options.force_recompile in
  let this_version = !Flx_version.version_data in
  let basename =
    let n = String.length inspec in
    if n <= 3 then inspec
    else
      let x = String.sub inspec (n-4) 4 in
      match x with
      | ".flx" | ".par" -> String.sub inspec 0 (n-4)
      | _ -> inspec

  in
  let include_dirs = syms.compiler_options.include_dirs in
  let tf = Flx_filesys.find_file ~include_dirs (basename ^ ".flx") in
  let pf =
    try
      Flx_filesys.find_file
        ~include_dirs:(match syms.compiler_options.cache_dir with
        | None -> include_dirs
        | Some d -> d::include_dirs
        )
        (basename ^ ".par")
    with Flx_filesys.Missing_path _ ->
      (* It's okay if the .par file doesn't exist. *)
      ""
  in
  let tf_mt = Flx_filesys.filetime tf in
  let pf_mt = Flx_filesys.filetime pf in
  let cbt = this_version.build_time_float in
  let saveit sts =
      let pf =
        match syms.compiler_options.cache_dir with
        | None ->
          if pf = "" then
            (try Filename.chop_extension tf with | _ -> tf) ^ ".par"
          else pf
        | Some d -> (Filename.concat d
          (try Filename.chop_extension basename with _ -> basename)
          ) ^ ".par"
      in
        let x = try Some (open_out_bin pf) with _ -> None in
        match x with
        | Some x ->
          if syms.compiler_options.print_flag then
          print_endline ("Written " ^ pf);
          Marshal.to_channel x this_version [];
          Marshal.to_channel x sts [];
          close_out x
        | None -> () (* can't write, don't worry *)
  in
  let parseit () =
    if syms.compiler_options.print_flag then print_endline ("Parsing " ^ tf);
    let parser_state = List.fold_left
      (Flx_parse.parse_file ~include_dirs)
      (Flx_parse.make_parser_state (fun stmt stmts -> stmt :: stmts) [])
      (syms.compiler_options.auto_imports @ [tf])
    in
    let tree = List.rev (Flx_parse.parser_data parser_state) in
    let local_prefix = Filename.basename basename in
    let macro_state = Flx_macro.make_macro_state local_prefix in
    let tree = Flx_macro.expand_macros macro_state tree in
    tree
  in
  let sts =
      (* -- no file ----------------------------------------- *)
    if tf_mt = 0.0 && pf_mt = 0.0 then
        failwith
        (
          "No .flx or .par file for name " ^
          basename ^
          " found in path:\n" ^
          String.concat "; " include_dirs
        )

      (* -- parsed file is newer or text doesn't exist ------- *)
    else
    let include_name =
      Filename.chop_extension
      (if tf <> "" then tf else pf)
    in
      if mem include_name !(syms.include_files) then [] else
      begin (* file not already included *)
        syms.include_files := include_name :: !(syms.include_files)
        ;
        if cbt < pf_mt && (not force) && tf_mt < pf_mt then
        begin (* top level time stamps OK *)
          let x = open_in_bin pf in
          let that_version = Marshal.from_channel x in
          if this_version = that_version then begin
            let tree = Marshal.from_channel x in
            close_in x;
            if syms.compiler_options.print_flag then
            print_endline ("Loaded " ^ pf);
            tree
          end (* right version of compiler *)
          else
          begin (* wrong version of compiler *)
            close_in x;
            let sts = parseit() in
            saveit sts;
            sts
          end
        end
        else
        begin (* time stamps wrong *)
          let sts = parseit() in
          saveit sts;
          sts
        end
      end (* process inclusion first time *)
  in
    sts

(* very inefficient .. fixme! *)
let rev_concat lss = rev (concat lss)

let map_vs sr (vs,_) = map (fun (s,_)->TYP_name (sr,s,[])) vs

let rec collate_namespaces syms sts =
 let counter = syms.counter in
 let rec cn stsin stsout nslist = match stsin with
 | [] ->
   rev_map (* faster, order doesn't matter here *)
   (fun (_,{name=name; sr=sr; vs=vs; fudges=fudges}) ->
     let sts =
       rev_map
       (fun fudge -> `AST_inject_module (sr,`AST_name (sr,fudge,map_vs sr vs)))
       !fudges
     in
     `AST_untyped_module (sr,name,vs,sts)
   )
   nslist
   @
   rev stsout

 | `AST_include (sr,inspec) :: tail ->
    let sts = include_file syms inspec in
    cn (sts @ tail) stsout nslist

 | head:: tail ->
   cn tail (head::stsout) nslist

 in cn sts [] []

