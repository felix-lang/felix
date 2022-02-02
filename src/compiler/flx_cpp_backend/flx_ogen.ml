(* This module is responsible for generating shape tables for all dynamically allocated
 * objects
 *)

(* Convenience function for printing debug statements. *)
let print_debug syms msg =
  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag
  then print_endline msg


open Flx_util
open Flx_list
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bbdcl
open Flx_mtypes2
open Flx_name
open Flx_unify
open Flx_typing
open List
open Flx_print
open Flx_exceptions
open Flx_maps
open Flx_cal_type_offsets
open Flx_cal_fun_offsets
open Flx_cal_threadframe_offsets
open Flx_findvars
open Flx_gen_shape
open Flx_btype_subst


let comma_sub s =
  let rec aux l r =
    try   (* note: breaks tail recursion optimisation *)
      let i = String.index r ',' in
      let n = String.length r in
      aux (l ^ String.sub r 0 i ^ " comma ") (String.sub r (i+1) (n-i-1))
    with Not_found -> l ^ r
  in
  aux "" s
let id x = ()

let scan_bexpr syms bsym_table allocable_types e : unit =
  let rec aux e = match e with
    | BEXPR_new (_,t),_ when t <> Flx_btype.btyp_tuple [] ->
      (* print_endline ("FOUND A NEW " ^ sbt bsym_table t); *)
      let index =
        try Flx_treg.find_type_index syms bsym_table t
        with Not_found -> failwith ("new: Can't find type in registry " ^ sbt bsym_table t)
      in
      Hashtbl.replace allocable_types t index;

      | BEXPR_class_new (cls, _),_ ->
      (*
      print_endline ("FOUND A CLASS NEW " ^ sbt bsym_table t);
      *)
      let index =
        try Flx_treg.find_type_index syms bsym_table cls
        with Not_found -> failwith ("class new: Can't find type in registry " ^ sbt bsym_table cls)
      in
      Hashtbl.replace allocable_types cls index;

    | BEXPR_identity_function t,ft -> 
      let index = 
        try Flx_treg.find_type_index syms bsym_table ft
        with Not_found -> failwith ("[scan_expr] Can't find identity function of type " ^
         sbt bsym_table ft ^ " in registry")
      in
      Hashtbl.replace allocable_types ft index;

    | x -> ()
  in
  Flx_bexpr.iter ~f_bexpr:aux e

let scan_exe syms bsym_table allocable_types exe : unit =
  Flx_bexe.iter ~f_bexpr:(scan_bexpr syms bsym_table allocable_types) exe

let scan_exes syms bsym_table allocable_types exes : unit =
  iter (scan_exe syms bsym_table allocable_types) exes

let gen_offset_tables syms bsym_table extras module_name =
  print_debug syms "GEN OFFSET TABLES";
  let allocable_types = Hashtbl.create 97 in
  let primitive_shapes = Hashtbl.create 97 in
  let s = Buffer.create 20000 in
  let h = Buffer.create 20000 in
  bcat h ("\n//**************************************\n");
  bcat h "// Shape decls\n";
  bcat h ("//**************************************\n");
  bcat h "\n";
 
  (* Make a shape for every non-C style function with the property `Heap_closure *)
  (* last arg, allocable_types, is used for dependent types found in process *)
  print_debug syms "Make fun shapes";
  gen_all_fun_shapes module_name (scan_exes syms bsym_table allocable_types) s h syms bsym_table allocable_types;

  (* generate offsets for all pointers store in the thread_frame *)
  print_debug syms "Make thread frame offsets";
  gen_thread_frame_offsets module_name s h syms bsym_table allocable_types;

  (* We're not finished: we need offsets dynamically allocated types too *)

  print_debug syms "Make shapes for dynamically allocated types";

  (* extra shapes required by primitives *)
  List.iter (fun t ->
    let index = Flx_treg.find_type_index syms bsym_table t in
    Hashtbl.replace allocable_types t index
  )
  extras;

  (* Run through type registry *)
  List.iter
  (fun (btyp, index) ->
print_debug syms ("Handle type " ^ sbt bsym_table btyp ^ " instance " ^ si index);
    match unfold "flx_ogen: gen_shape_tables" btyp with
    | BTYP_sum args ->
      iter begin fun t ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Flx_treg.find_type_index syms bsym_table t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_rptsum (n,t) ->
      let index = Flx_treg.find_type_index syms bsym_table t in
      Hashtbl.replace allocable_types t index

    | BTYP_variant args ->
      iter begin fun (_,t) ->
        match t with
        | BTYP_tuple []
        | BTYP_void -> ()
        | _ ->
          try
            let index = Flx_treg.find_type_index syms bsym_table t in
            Hashtbl.replace allocable_types t index
          with Not_found -> ()
      end args

    | BTYP_inst (`Nominal, i,ts,_) ->
      print_debug syms ("Thinking about type index " ^ si i ^ " ts = " ^ catmap "," (sbt bsym_table) ts);
      let bsym =
        try Flx_bsym_table.find bsym_table i
        with Not_found ->
          failwith ("[gen_offset_tables:BTYP_inst] can't find index " ^
            string_of_bid i)
      in
      begin match Flx_bsym.bbdcl bsym with
      | BBDCL_external_type (vs,bquals,_,breqs) ->
        if List.length vs > 0 && List.mem (`TypeTag "functor") bquals then
        print_endline ("functor primitive: abstract type "^Flx_bsym.id bsym ^ "["^catmap "," (sbt bsym_table) ts^"]");
        (*
        print_endline ("abstract type "^Flx_bsym.id bsym ^ "["^catmap "," (sbt bsym_table) ts^"]");
        print_endline ("  properties: " ^ string_of_bquals bsym_table bquals);
        print_endline ("  requirements: " ^ string_of_breqs bsym_table breqs);
        *)
        let handle_qual bqual = match bqual with
        | `Bound_needs_shape t ->
          (*
          print_endline ("Needs shape (uninstantiated) " ^ sbt bsym_table t);
          *)
          let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
          let t = varmap_subst varmap t in
          (*
          print_endline ("Needs shape (instantiated) " ^ sbt bsym_table t);
          *)
          begin try
            let index = Flx_treg.find_type_index syms bsym_table t in
            Hashtbl.replace allocable_types t index
          with
          | Not_found -> failwith ("[gen_offset_tables] Woops, type "^si i^ " = " ^ 
             sbt bsym_table t ^" isn't in registry! Required shape for " ^ bsym.Flx_bsym.id)
          end

        | _ -> ()
        in
        let rec aux quals = match quals with
        | [] -> ()
        | h :: t -> handle_qual h; aux t
        in aux bquals


      (* this routine assumes any use of a union component is
         allocable .. this is quite wrong but safe. This SHOULD
         be drived by detecting constructor expressions

         We don't need to worry about pattern matches .. if
         we didn't construct it, perhaps a foreigner did,
         in which case THEY needed to create the shape object
      *)
      | BBDCL_union (vs,args) ->
        let varmap = mk_varmap (Flx_bsym.sr bsym) vs ts in
        let args = map (fun (_,_,evs,t,_,_)->t) args in
        let args = map (varmap_subst varmap) args in
        iter begin fun t ->
          match t with
          | BTYP_tuple []
          | BTYP_void -> ()
          | _ ->
            try
              let index = Flx_treg.find_type_index syms bsym_table t in
              Hashtbl.replace allocable_types t index
            with Not_found -> ()
        end args

      | _ -> ()
      end
    | _ -> ()
  )
  syms.registry
  ;
  let need_int = ref false in

  (* somehow, we can get duplicates, probably because the types are not uniquely represented *)
  let generated = Hashtbl.create 97 in
  let process_table table =
    let new_table = Hashtbl.create 97 in
    Hashtbl.iter
    (fun btyp index -> 
       if not (Hashtbl.mem generated index) then begin
         Hashtbl.add generated index ();
         Flx_gen_type_shape.gen_type_shape module_name s h syms bsym_table need_int primitive_shapes btyp index new_table
       end
    )
    table;
    new_table
  in
  let table = ref allocable_types in
  while Hashtbl.length !table > 0 do
     table := process_table !table
  done;

  bcat s ("\n");

 
  if !need_int then
  bcat h ("static ::flx::gc::generic::gc_shape_t &int_ptr_map = ::flx::rtl::_int_ptr_map;\n");
  bcat h ("\n//**************************************\n");

  bcat s "";
  Buffer.contents h ^
  Buffer.contents s

