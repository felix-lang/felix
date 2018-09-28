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
open Flx_bid
open Flx_btype_subst

let find_variable_indices syms bsym_table index =
  let children = Flx_bsym_table.find_children bsym_table index in
  BidSet.fold begin fun bid bids ->
    try match Flx_bsym_table.find_bbdcl bsym_table bid with
      | BBDCL_val (_,_,(`Val | `Var | `Ref | `Once)) -> bid :: bids
      | _ -> bids
    with Not_found -> bids
  end children []

(*
let is_gadt cps = 
 List.fold_left (fun acc (id,idx,evs,d,c) -> match c with BTYP_void -> true | _ -> acc) false cps
*)

let get_variable_typename syms bsym_table i ts =
  let bsym =
    try Flx_bsym_table.find bsym_table i with Not_found ->
      failwith ("[get_variable_typename] can't find index " ^ string_of_bid i)
  in
  let sr = Flx_bsym.sr bsym in
  let rt vs t = beta_reduce "flx_gen_helper" 
    syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_val (vs,t,_) ->
      if length ts <> length vs then begin
        failwith
        (
          "[get_variable_typename} wrong number of args, expected vs = " ^
          si (length vs) ^
          ", got ts=" ^
          si (length ts)
        )
      end;
(*
print_endline ("--- variable generator --- " ^ Flx_bsym.id bsym);
assert (List.length vs = 0);
assert (List.length ts = 0);
print_endline ("Original type " ^ sbt bsym_table t);
begin match t with 
| BTYP_inst (i,_) -> 
  print_endline ("Nominal type index " ^ string_of_int i);
  begin match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_union (_,cps) -> 
    print_endline "Nominal type is union";
    print_endline (if is_gadt cps then " ** GADT" else "  ** not GADT")
  | _ -> ()
  end
| _ -> ()
end;
*)
      let t = rt vs t in
(*
print_endline ("Reduced type " ^ sbt bsym_table t);
begin match t with 
| BTYP_inst (i,_) -> 
  print_endline ("Nominal type index " ^ string_of_int i);
  begin match Flx_bsym_table.find_bbdcl bsym_table i with
  | BBDCL_union (_,cps) -> 
    print_endline "Nominal type is union";
    print_endline (if is_gadt cps then " ** GADT" else "  ** not GADT")
  | _ -> ()
  end
| _ -> ()
end;
*)

      let tname = cpp_typename syms bsym_table t in
(*
print_endline ("Variable generator: variable " ^ 
  Flx_bsym.id bsym ^ ", type=" ^ sbt bsym_table t ^ ", C++type=" ^ tname
);
*)
      tname

  | _ ->
      failwith "[get_variable_typename] Expected variable"

let format_vars syms bsym_table vars ts =
  catmap  ""
  (fun idx ->
    let instname =
      try Some (cpp_instance_name syms bsym_table idx ts)
      with _ -> None
    in
      match instname with
      | Some instname ->
        let typename = get_variable_typename syms bsym_table idx ts in
        "  " ^ typename ^ " " ^ instname ^ ";\n"
      | None -> "" (* ignore unused variables *)
  )
  vars

let find_members syms bsym_table index ts =
  let variables = find_variable_indices syms bsym_table index in
  match format_vars syms bsym_table variables ts with
  | "" -> ""
  | x ->
  (*
  "  //variables\n" ^
  *)
  x
let get_type bsym_table index =
  let bsym =
    try Flx_bsym_table.find bsym_table index
    with _ -> failwith ("[get_type] Can't find index " ^ si index)
  in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,ps,ret,effects,_) ->
      btyp_function (Flx_bparams.get_btype ps,ret)
  | _ -> failwith "Only function and procedure types handles by get_type"


let is_gc_pointer syms bsym_table sr t =
  (*
  print_endline ("[is_gc_ptr] Checking type " ^ sbt bsym_table t);
  *)
  match t with
  | BTYP_function _ -> true
  | BTYP_pointer _ -> true
  | BTYP_inst (i,_,_) ->
    let bsym =
      try Flx_bsym_table.find bsym_table i with Not_found ->
        clierrx "[flx_cpp_backend/flx_gen_helper.ml:112: E310] " sr ("[is_gc_pointer] Can't find nominal type " ^
          string_of_bid i);
    in
    begin match Flx_bsym.bbdcl bsym with
    | BBDCL_external_type (_,tqs,_,_) -> mem `GC_pointer tqs
    | _ -> false
    end
  | _ -> false

let is_closure_var bsym_table index =
  let var_type bsym_table index =
    let id,_,entry =
      try Hashtbl.find bsym_table index
      with Not_found -> failwith ("[var_type] ]Can't get index " ^ si index)
    in match entry with
    | BBDCL_val (_,t,(`Val | `Var | `Ref | `Once)) -> t
    | _ -> failwith ("[var_type] expected "^id^" to be variable")
  in
  match var_type bsym_table index with
  | BTYP_function _ -> true
  | _ -> false


