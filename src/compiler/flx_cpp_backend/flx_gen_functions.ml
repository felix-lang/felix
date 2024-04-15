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
open Flx_bid
open Flx_btype_subst

(* This code generates the class declarations *)
let gen_functions syms bsym_table (shapes: Flx_set.StringSet.t ref) shape_table =
  let xxsym_table = ref [] in
  Hashtbl.iter
  (fun x i ->
    (* if proper_descendant parent then  *)
    xxsym_table := (i,x) :: !xxsym_table
  )
  syms.instances
  ;

  let s = Buffer.create 2000 in
  List.iter
  (fun ((i:bid_t),(index,ts)) ->
    let tss =
      if length ts = 0 then "" else
      "[" ^ catmap "," (sbt bsym_table) ts^ "]"
    in
    let parent, bsym =
      try Flx_bsym_table.find_with_parent bsym_table index with Not_found ->
        failwith ("[gen_functions] can't find index " ^ string_of_bid index)
    in
    let sr = Flx_bsym.sr bsym in
    let parent_name = 
      match parent with
      | None -> "None"
      | Some p -> 
       let parent_bsym = Flx_bsym_table.find bsym_table p 
       in Flx_bsym.id parent_bsym ^ "<" ^ string_of_int p ^ ">"
    in
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,ps,ret,effects,_) ->
if List.mem `Csp props then print_endline ("Generating rt function " ^ bsym.id);
      let is_proc = match ret with | BTYP_void -> true | _ -> false in
      let is_escape = match ret with | BTYP_fix(0,_) -> true | _ -> false in
      let name = 
         (if List.mem `Csp props then "CSP " else "") ^ 
         (if is_escape then "ESCAPE " else "") ^ 
         (if is_proc then "PROCEDURE" else "FUNCTION") 
      in
      bcat s ("\n//------------------------------\n");
      let ft = btyp_effector (Flx_bparams.get_btype ps,effects,ret) in
      if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then begin
        bcat s ("//PURE C " ^ name ^ " <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^ " " ^ sbt bsym_table ft ^
          "\n");
        bcat s
        (Flx_gen_cfunc.gen_C_function
          syms
          bsym_table
          shapes shape_table
          props
          index
          (Flx_bsym.id bsym)
          (Flx_bsym.sr bsym)
          vs
          ps
          ret
          ts
          i)
      end else begin
        bcat s ("//" ^ name ^ " <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^ " " ^ sbt bsym_table ft ^
          "\n//    parent = " ^ parent_name ^
          "\n");
        bcat s
        (Flx_gen_func.gen_function
          syms
          bsym_table
          props
          index
          (Flx_bsym.id bsym)
          (Flx_bsym.sr bsym)
          vs
          ps
          ret
          ts
          i)
      end

    | BBDCL_external_fun (_,vs,ps_cf,ret',_,_,`Callback (ps_c,_)) ->
      let instance_no = i in
      bcat s ("\n//------------------------------\n");
      if ret' = btyp_void () then begin
        bcat s ("//CALLBACK C PROC <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^
          "\n");
      end else begin
        bcat s ("//CALLBACK C FUNCTION <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^
          "\n");
      end
      ;
      let rt vs t =
        beta_reduce "flx_gen1" syms.Flx_mtypes2.counter bsym_table (Flx_bsym.sr bsym) (tsubst sr vs ts t)
      in
      if syms.compiler_options.print_flag then
      print_endline
      (
        "//Generating C callback function inst " ^
        string_of_bid instance_no ^ "=" ^
        Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
        (
          if length ts = 0 then ""
          else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
        )
      );
      if length ts <> length vs then
      failwith
      (
        "[gen_function} wrong number of args, expected vs = " ^
        si (length vs) ^
        ", got ts=" ^
        si (length ts)
      );
      let ret = rt vs ret' in
      (*
      let name = cpp_instance_name syms bsym_table index ts in
      *)
      let name = Flx_bsym.id bsym in (* callbacks can't be polymorphic .. for now anyhow *)
      let rettypename = cpp_typename syms bsym_table ret in
      let sss =
        "extern \"C\" " ^
        rettypename ^ " " ^
        name ^ "(" ^
        (
          match length ps_c with
          | 0 -> ""
          | 1 -> cpp_typename syms bsym_table (hd ps_c)
          | _ ->
            fold_left
            (fun s t ->
              let t = rt vs t in
              s ^
              (if String.length s > 0 then ", " else "") ^
              cpp_typename syms bsym_table t
            )
            ""
            ps_c
        ) ^
        ");\n"
      in bcat s sss

    | _ -> () (* bcat s ("//SKIPPING " ^ id ^ "\n") *)
  )
  (sort Stdlib.compare !xxsym_table)
  ;
  Buffer.contents s

