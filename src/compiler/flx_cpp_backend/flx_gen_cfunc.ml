open List

open Flx_bbdcl
open Flx_beta
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_cexpr
open Flx_ctorgen
open Flx_ctypes
open Flx_display
open Flx_egen
open Flx_exceptions
open Flx_label
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_name
open Flx_ogen
open Flx_options
open Flx_pgen
open Flx_print
open Flx_types
open Flx_typing
open Flx_unify
open Flx_util
open Flx_gen_helper
open Flx_btype_subst
open Flx_bid

let gen_C_function syms bsym_table (shapes:Flx_set.StringSet.t ref) shape_map props index id sr vs bps ret' ts instance_no =
  let rt vs t = beta_reduce "flx_gen_cfunc" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let requires_ptf = mem `Requires_ptf props in
  (*
  print_endline ("C Function " ^ id ^ " " ^ if requires_ptf then "requires ptf" else "does NOT require ptf");
  *)
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating C function inst " ^
    string_of_bid instance_no ^ "=" ^
    id ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  let argtype = Flx_bparams.get_btype bps in
  if length ts <> length vs then
  failwith
  (
    "[gen_function} wrong number of args, expected vs = " ^
    si (length vs) ^
    ", got ts=" ^
    si (length ts)
  );
  let argtype = rt vs argtype in
  let rt' vs t = beta_reduce "flx_gen_cfunc2" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let ret = rt' vs ret' in
  if ret = btyp_tuple [] then "// elided (returns unit)\n" else

  let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, ret)) in
  (* let argtypename = cpp_typename syms bsym_table argtype in *)
  let display = get_display_list bsym_table index in
  assert (length display = 0);
  let name = cpp_instance_name syms bsym_table index ts in
  let rettypename = cpp_typename syms bsym_table ret in
  let pidxs = Flx_bparams.get_bids bps in
  rettypename ^ " " ^
  name ^ "(" ^
  (
    let s =
(*
      match length pidxs with
      | 0 -> ""
      | 1 ->
        let ix = hd pidxs in
        if Hashtbl.mem syms.instances (ix, ts)
        && not (argtype = btyp_tuple [] || argtype = btyp_void ())
        then cpp_typename syms bsym_table argtype else ""
      | _ ->
*)
        fold_left
        (fun s {pindex=i; ptyp=t} ->
          let t = rt vs t in
          if Hashtbl.mem syms.instances (i,ts) && not (t = btyp_tuple [])
          then s ^
            (if String.length s > 0 then ", " else " ") ^
            cpp_typename syms bsym_table t
          else s (* elide initialisation of elided variable *)
        )
        ""
        (Flx_bparams.get_params bps)

    in
      (
        if (not (mem `Cfun props)) then
        (
          if String.length s > 0
          then (if requires_ptf then "FLX_FPAR_DECL " else "") ^s
          else (if requires_ptf then "FLX_FPAR_DECL_ONLY" else "")
        ) else s
      )
  ) ^
  ");\n"

let gen_C_function_body filename syms bsym_table
  (shapes: Flx_set.StringSet.t ref) shape_map
  label_info counter index ts sr instance_no
=
  let rt vs t = beta_reduce "flx_gen_cfunc: gen_C_function_body" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("gen_C_function_body] can't find " ^ string_of_bid index)
  in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating C function body inst " ^
    string_of_bid instance_no ^ "=" ^
    Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,bps,ret',effects,exes) ->
    (*
    print_endline ("Properties=" ^ catmap "," (fun x->st (x:>felix_term_t)) props);
    *)
    let requires_ptf = mem `Requires_ptf props in
    if length ts <> length vs then
    failwith
    (
      "[get_function_methods] wrong number of type args, expected vs = " ^
      si (length vs) ^
      ", got ts=" ^
      si (length ts)
    );
    let name = cpp_instance_name syms bsym_table index ts in

    "//C FUNC <" ^ string_of_bid index ^ ">: " ^ name ^ "\n" ^

    let argtype = Flx_bparams.get_btype bps in
    let argtype = rt vs argtype in
    let rt' vs t = beta_reduce "flx_gen_cfunc: gen_C_function_body2"  syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
    let ret = rt' vs ret' in
    if ret = btyp_tuple [] then "// elided (returns unit)\n\n" else


    let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, ret)) in
    (* let argtypename = cpp_typename syms bsym_table argtype in *)
    let rettypename = cpp_typename syms bsym_table ret in

    let params = Flx_bparams.get_bids bps in
    let exe_string,_ =
      try
        Flx_gen_exe.gen_exes filename name syms bsym_table 
          shapes shape_map [] label_info counter index 
          exes vs ts instance_no true
      with x ->
        (*
        print_endline (Printexc.to_string x);
        print_endline (catmap "\n" (string_of_bexe bsym_table 1) exes);
        print_endline "Can't gen exes ..";
        *)
        raise x
    in
    let dcl_vars =
      let kids = Flx_bsym_table.find_children bsym_table index in
      let kids =
        BidSet.fold begin fun bid lst ->
          let bsym =
            try Flx_bsym_table.find bsym_table bid with Not_found ->
              failwith ("[C func body, vars] Can't find index " ^
                string_of_bid bid);
          in
          match Flx_bsym.bbdcl bsym with
          | BBDCL_val (vs,t,(`Val | `Var | `Once)) when not (List.mem bid params) ->
              (bid, rt vs t) :: lst
          | BBDCL_val (vs,t,`Ref) when not (List.mem bid params) ->
              (bid, btyp_pointer (rt vs t)) :: lst
          | _ -> lst
        end kids []
      in
      fold_left
      (fun s (i,t) -> s ^ "  " ^
        cpp_typename syms bsym_table t ^ " " ^
        cpp_instance_name syms bsym_table i ts ^ ";\n"
      )
      "" kids
    in
      rettypename ^ " " ^
      name ^ "(" ^
      (
        let s =
(*
          match bps with
          | [] -> ""
          | [{pkind=k; pindex=i; ptyp=t}] ->
            if Hashtbl.mem syms.instances (i, ts)
            && not (argtype = btyp_tuple [] || argtype = btyp_void ())
            then
              let t = rt vs t in
              cpp_typename syms bsym_table t ^ " " ^
              cpp_instance_name syms bsym_table i ts
            else ""
          | _ ->
*)
              fold_left
              (fun s {pkind=k; pindex=i; ptyp=t} ->
                let t = rt vs t in
                if Hashtbl.mem syms.instances (i,ts) && not (t = btyp_tuple [])
                then s ^
                  (if String.length s > 0 then ", " else " ") ^
                  cpp_typename syms bsym_table t ^ " " ^
                  cpp_instance_name syms bsym_table i ts
                else s (* elide initialisation of elided variable *)
              )
              ""
              (Flx_bparams.get_params bps)
        in
          (
            if not (mem `Cfun props) &&
            requires_ptf then
              if String.length s > 0
              then "FLX_APAR_DECL " ^ s
              else "FLX_APAR_DECL_ONLY"
            else s
          )
      )^
      "){\n" ^
      dcl_vars ^
      exe_string ^
      "}\n"

  | _ -> failwith "function expected"

let gen_C_procedure_body filename syms bsym_table 
  (shapes: Flx_set.StringSet.t ref) shape_map
  label_info counter index ts sr instance_no
=
  let rt vs t = beta_reduce "flx_gen_cfunc: gen_C_procedure_body" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("gen_C_function_body] can't find " ^ string_of_bid index)
  in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating C procedure body inst " ^
    string_of_bid instance_no ^ "=" ^
    Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,bps,BTYP_fix (0,_),effects,exes) 
  | BBDCL_fun (props,vs,bps,BTYP_void,effects,exes) ->
    let requires_ptf = mem `Requires_ptf props in
    if length ts <> length vs then
    failwith
    (
      "[get_function_methods] wrong number of type args, expected vs = " ^
      si (length vs) ^
      ", got ts=" ^
      si (length ts)
    );
    let name = cpp_instance_name syms bsym_table index ts in
    let argtype = Flx_bparams.get_btype bps in
    let argtype = rt vs argtype in

    let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, btyp_void ())) in
    (* let argtypename = cpp_typename syms bsym_table argtype in *)

    let params = Flx_bparams.get_bids bps in
    let exe_string,_ =
      try
        Flx_gen_exe.gen_exes filename name syms bsym_table 
          shapes shape_map [] label_info counter index exes vs ts instance_no true
      with x ->
        (*
        print_endline (Printexc.to_string x);
        print_endline (catmap "\n" (string_of_bexe bsym_table 1) exes);
        print_endline "Can't gen exes ..";
        *)
        raise x
    in
    let dcl_vars =
      let kids = Flx_bsym_table.find_children bsym_table index in
      let kids =
        BidSet.fold begin fun bid lst ->
          let bsym =
            try Flx_bsym_table.find bsym_table bid with Not_found ->
              failwith ("[C func body, vars] Can't find index " ^
                string_of_bid bid);
          in
          match Flx_bsym.bbdcl bsym with
          | BBDCL_val (vs,t,(`Val | `Var | `Once)) when not (mem bid params) ->
              (bid, rt vs t) :: lst
          | BBDCL_val (vs,t,`Ref) when not (mem bid params) ->
              (bid, btyp_pointer (rt vs t)) :: lst
          | _ -> lst
        end kids []
      in
      fold_left
      (fun s (i,t) -> s ^ "  " ^
        cpp_typename syms bsym_table t ^ " " ^
        cpp_instance_name syms bsym_table i ts ^ ";\n"
      )
      "" kids
    in
    let output =
      "//C PROC <" ^ string_of_bid index ^ ">: " ^ name ^ "\n" ^
      "void " ^
      name ^ "(" ^
      (
        let s =
(*
          match bps with
          | [] -> ""
          | [{pkind=k; pindex=i; ptyp=t}] ->
            if Hashtbl.mem syms.instances (i, ts)
            && not (argtype = btyp_tuple [] || argtype = btyp_void ())
            then
              let t = rt vs t in
              cpp_typename syms bsym_table t ^ " " ^
              cpp_instance_name syms bsym_table i ts
            else ""
          | _ ->
*)
              fold_left
              (fun s {pkind=k; pindex=i; ptyp=t} ->
                let t = rt vs t in
                if Hashtbl.mem syms.instances (i,ts) && not (t = btyp_tuple [])
                then s ^
                  (if String.length s > 0 then ", " else " ") ^
                  cpp_typename syms bsym_table t ^ " " ^
                  cpp_instance_name syms bsym_table i ts
                else s (* elide initialisation of elided variable *)
              )
              ""
              (Flx_bparams.get_params bps)
        in
          (
            if (not (mem `Cfun props)) && requires_ptf then
              if String.length s > 0
              then "FLX_APAR_DECL " ^ s
              else "FLX_APAR_DECL_ONLY"
            else s
          )
      )^
      "){\n" ^
      dcl_vars ^
      exe_string ^
      "}\n"
      in 
      output

  | _ -> failwith "procedure expected"




