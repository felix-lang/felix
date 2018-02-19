(* scan the unbound symbol table for functions claiming to be subtype
coercions and just print them for now
*)
open Flx_sym
open Flx_types
open Flx_print

let report_subtypes syms =
  Flx_sym_table.iter (fun bid parent sym ->
    let vs = fst sym.vs in
    match sym.symdef with
    | SYMDEF_function (params,ret,effect,props,_) when List.mem `Subtype props ->
(*
      print_endline ("Felix  Function: Symbol table " ^ string_of_int bid ^ " -> " ^ sym.id);
*)
      if List.length vs <> 0 then
        print_endline ("   Improper subtype, no type variables allowed, got " ^
         string_of_int (List.length vs))
      else
      let ps = fst params in
      begin match ps with
      | Satom (sr,kind,id,typ,initopt) -> () 
(*
         print_endline ("      " ^ string_of_typecode typ ^ " -> " 
         ^ string_of_typecode ret)
*)
      | _ ->
         print_endline ("Improper subtype, only one parameter allowed, got " ^ 
           string_of_paramspec_t ps)
      end
      
    | SYMDEF_fun (props,ps,ret,_,_,_) when List.mem `Subtype props ->
(*
      print_endline ("Extern Function: Symbol table " ^ string_of_int bid ^ " -> " ^ sym.id);
*)
      if List.length vs <> 0 then
        print_endline ("   Improper subtype, no type variables allowed, got " ^
         string_of_int (List.length vs))
      else
      begin match ps with
      | [typ] -> ()
(*
         print_endline ("      " ^ string_of_typecode typ ^ " -> " 
         ^ string_of_typecode ret)
*)
      | _ ->
         print_endline ("Improper subtype, only one parameter allowed, got " ^ 
           string_of_int (List.length ps))
      end
 
    | _ -> ()
  ) 
  syms

