open Flx_ast
open Flx_typing

let debugid = 
  try Sys.getenv "FLX_COMPILER_OVERLOAD_DEBUG_ID"   
  with Not_found -> ""


let strtyp t = Flx_print.string_of_typecode t

(* see also "reorder" function .. hmm .. *)
let fixup_argtypes be bid pnames base_domain argt rs =
  match pnames with
  | None -> argt
  | Some ps ->
      match
        try
          List.iter (fun (name,_) -> ignore (List.assoc name ps)) rs;
          true
        with Not_found -> false
      with
      | false -> argt
      | true ->
          match base_domain with
          | `TYP_record _ -> argt
          | `TYP_tuple [] -> argt (* lazy *)
          | _ ->
              let ps =
                List.map begin fun (name,e) ->
                  name,
                  match e with
                  | None -> None
                  | Some e -> Some (be bid e)
                end ps
              in
              begin
                try
                  let ats =
                    List.map begin fun (name,d) ->
                      try List.assoc name rs
                      with Not_found ->
                        match d with (* ok to skip if there is a default *)
                        | Some (e,t) -> t
                        | None -> raise Not_found
                    end ps
                  in
                  let t =
                    match ats with
                    | [] -> assert false
                    | [x] -> x
                    | _ -> Flx_btype.btyp_tuple ats
                  in
                  t
                with Not_found -> argt
              end


let resolve sym_table bsym_table base_sym bt be arg_types =
  let sym = Flx_sym_table.find sym_table base_sym in
  let name = sym.Flx_sym.id in
(*
  if name = debugid then 
  print_endline ("Attempting to resolve " ^ name);
*)
  let opt_bsym = try Some (Flx_bsym_table.find bsym_table base_sym) with Not_found -> None in

  let pvs, vs, { raw_type_constraint=con } =
    Flx_generic.find_split_vs sym_table bsym_table base_sym
  in
(*
print_endline ("Flx_overload: resolve: split_vs finds constraint " ^ strtyp con);
    print_endline ("SPLITVS: PARENT VS=" ^ catmap "," (fun (s,i,_)->s^"<"^si i^">") pvs);
    print_endline ("SPLITVS: base   VS=" ^ catmap "," (fun (s,i,_)->s^"<"^si i^">") vs);
*)
  let base_domain, base_result, pnames = Flx_sig_of_symdef.sig_of_symdef
    sym.Flx_sym.symdef
    sym.Flx_sym.sr
    sym.Flx_sym.id
    base_sym
  in
(*
if name = debugid then begin
  print_endline ("Base_sym=" ^si base_sym^ ", base domain="^strtyp base_domain ^
   ", base result="^strtyp base_result^", pnames from sig_of_symdef");
end;
*)
  let arg_types =
    match arg_types with
    | Flx_btype.BTYP_record (rs) as argt :: tail ->
        fixup_argtypes be base_sym pnames base_domain argt rs :: tail

    | Flx_btype.BTYP_tuple [] as argt :: tail ->
        fixup_argtypes be base_sym pnames base_domain argt [] :: tail

    | _ ->
        arg_types
  in
(*
if name = debugid then 
  print_endline ("Arg types = " ^ catmap "," (sbt bsym_table) arg_types);
*)
  (* bind type in base context, then translate it to view context:
   * thus, base type variables are eliminated and specialisation
   * type variables introduced *)

(*
  let con = match con with | `TYP_tuple [] -> Flx_btype.btyp_tuple [] | _ -> bt sym.Flx_sym.sr con in
*)

(*
print_endline ("UNBOUND Constraint2 = " ^ strtyp con);
*)
  let con = bt sym.Flx_sym.sr con in
(*
print_endline ("BOUND Constraint2 = " ^ Flx_btype.st con);
*)
  let domain,base_result = 
  (* this is primarily an optimisation to save recursive overload resolution
   * to find the return type of a function, which may itself involve a chain
   * of overload resolutions. However it also helps if a function isn't declared
   * with a return type, to find the computed return type: however this will ONLY
   * WORK if the function is already bound, so it can't be relied on. This needs
   * to be fixed! Because the results of a call with multiple arguments depend
   * on the return type, and we can't have the success of overloading depend on
   * the order of binding the compiler happens to pick! FIX IT!
   *)
  match opt_bsym with
  | Some {Flx_bsym.id=id;sr=sr;bbdcl=Flx_bbdcl.BBDCL_fun (props,base_bvs,ps,rt,effects,_)} ->
(*
if name = debugid then print_endline ("Found function binding for " ^ id);
*)
    let domain = Flx_bparams.get_btype ps in
    let base_result = rt in
    domain, base_result

  | _ -> 
(*
print_endline ("Warning: didn't find function binding for " ^ sym.Flx_sym.id);
*)
(*
if name = debugid then print_endline ("Can't find bound symbol table entry, binding:");
*)
    let domain = 
      try bt sym.Flx_sym.sr base_domain 
      with exn -> 
       print_endline ("[Flx_resolve] Binding: " ^ name ^ ": Can't bind base domain type " ^ strtyp base_domain);
       print_endline ("[Flx_resolve] " ^ Flx_srcref.long_string_of_src sym.Flx_sym.sr); 
       print_endline ("[Flx_resolve] Exception: " ^ Printexc.to_string exn);
       print_endline ("{Flx_resolve] TERMINATING");
       assert false
    in
    let base_result = 
     try bt sym.Flx_sym.sr base_result 
     with exn -> 
       print_endline ("[Flx_resolve] WARNING: Can't bind base result type " ^ strtyp base_result); 
       print_endline ("[Flx_resolve] Exception: " ^ Printexc.to_string exn);
       print_endline ("[Flx_resolve] Returning btyp_none()");
       Flx_btype.btyp_none()
    in
    domain,base_result
  in
(*
if name = debugid then print_endline "Resolve complete";
*)
  sym.Flx_sym.id, sym.Flx_sym.sr, vs, pvs, con, domain, base_result, arg_types


