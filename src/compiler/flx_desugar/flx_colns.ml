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
open Flx_srcref
open Flx_util
open Flx_filesys
open Flx_version
open Flx_macro

type nsrec = {
  name:string;
  sr:range_srcref;
  vs:vs_list_t;
  fudges:string list ref
}

let dfltvs_aux = { raw_type_constraint=`TYP_tuple []; raw_typeclass_reqs=[]}
let dfltvs = [],dfltvs_aux

let include_file syms inspec lookup =
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
  let tf = find_file lookup include_dirs (basename ^ ".flx") in
  let pf = find_file lookup
    (match syms.compiler_options.cache_dir with
    | None -> include_dirs
    | Some d -> d::include_dirs
    )
    (basename ^ ".par")
  in
  let tf_mt = filetime tf in
  let pf_mt = filetime pf in
  let cbt = this_version.build_time_float in
  let saveit hash_include_files sts =
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
          Marshal.to_channel x (hash_include_files,sts) [];
          close_out x
        | None -> () (* can't write, don't worry *)
  in
  let parseit() =
    let hash_include_files, sts =
      if syms.compiler_options.print_flag then
      print_endline ("Parsing " ^ tf);
      Flx_parse_ctrl.parse_file
        tf
        (Filename.dirname tf)
        include_dirs
        syms.compiler_options.cache_dir
        Flx_macro.expand_expression
        syms.compiler_options.auto_imports
    in
      let local_prefix = Filename.basename basename in
      let tree = expand_macros local_prefix 5000 sts in
      hash_include_files, tree
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
            let (hash_include_files,tree) = Marshal.from_channel x in
            close_in x;

            let hash_includes_agree = fold_left
              (fun acc f ->
                let ft = filetime f in
                acc && ft <> 0.0 && ft < pf_mt
              )
              true
              hash_include_files
            in
            if hash_includes_agree then begin (* all time stamps OK *)
              if syms.compiler_options.print_flag then
              print_endline ("Loaded " ^ pf);
              tree
            end else begin (* include file timestamps wrong *)
              let hash_include_files, sts = parseit() in
              saveit hash_include_files sts;
              sts
            end
          end (* right version of compiler *)
          else
          begin (* wrong version of compiler *)
            close_in x;
            let hash_include_files, sts = parseit() in
            saveit hash_include_files sts;
            sts
          end
        end
        else
        begin (* time stamps wrong *)
          let hash_include_files,sts = parseit() in
          saveit hash_include_files sts;
          sts
        end
      end (* process inclusion first time *)
  in
    sts


let appns sr name vs fudge_name nslist =
  try
    match assoc name nslist with
    { sr=sr'; vs=vs'; fudges=fudges' } ->
    if vs <> vs' then
      clierr2 sr' sr "namespace type variables/constraints don't agree"
    ;
    fudges' := fudge_name :: !fudges'
    ;
    nslist


  with Not_found ->
   ( name,{name=name; sr=sr; vs=vs; fudges=ref [fudge_name] }) :: nslist

(* very inefficient .. fixme! *)
let rev_concat lss = rev (concat lss)

let map_vs sr (vs,_) = map (fun (s,_)->`AST_name (sr,s,[])) vs

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

 | `AST_namespace (sr,name,vs,sts) :: tail ->
   let n = !counter in incr counter;
   let fudge_name = "_" ^ name ^ "_" ^ si n in
   let inh = `AST_inject_module (sr, `AST_name (sr,name, map_vs sr vs)) in
   let sts = inh :: sts in
   let fudge_module = `AST_untyped_module (sr,fudge_name,vs,sts) in
   let nslist = appns sr name vs fudge_name nslist in
   cn tail (fudge_module::stsout) nslist

 | `AST_include (sr,inspec) :: tail ->
    let sts = include_file syms inspec true in
    cn (sts @ tail) stsout nslist

 | head:: tail ->
   cn tail (head::stsout) nslist

 in cn sts [] []

(* The namespace munging replaces this:

    namespace A { .. }
    namespace A { .. }

 with this:

   module A1 { inherit A; ... }
   module A2 { inherit A; .. }
   module A { inherit A1; inherit A2; }

The effect is to make all the definitions visible in each namespace
section, and be able to grab the lot, collated, by the original 
name, whilst preserving order of writing within each namespace
*)

