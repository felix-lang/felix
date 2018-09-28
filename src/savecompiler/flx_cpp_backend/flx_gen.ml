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

(*
let gen_class syms bsym_table props index id sr vs ts instance_no =
  let rt vs t = beta_reduce syms.Flx_mtypes2.counter bsym_table sr (tsubst vs ts t) in
  let requires_ptf = mem `Requires_ptf props in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating class inst " ^
    si instance_no ^ "=" ^
    id ^ "<" ^ string_of_bid index ^ ">" ^
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
  let display = get_display_list bsym_table index in
  let frame_dcls =
    if requires_ptf then
    "  FLX_FMEM_DECL\n"
    else ""
  in
  let display_string = match display with
    | [] -> ""
    | display ->
      cat ""
      (
        List.map
        (fun (i, vslen) ->
         try
         let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
         "  " ^ instname ^ " *ptr" ^ instname ^ ";\n"
         with _ -> failwith "Can't cal display name"
         )
        display
      )
  and ctor_dcl name =
    "  " ^name^
    (if length display = 0
    then (if requires_ptf then "(FLX_FPAR_DECL_ONLY);\n" else "();\n")
    else (
    "  (" ^
    (if requires_ptf then
    "FLX_FPAR_DECL "
    else ""
    )
    ^
    cat ","
      (
        List.map
        (
          fun (i,vslen) ->
          let instname = cpp_instance_name syms bsym_table i (list_prefix ts vslen) in
          instname ^ "*"
        )
        display
      )^
      ");\n"
    ))
  (*
  and dtor_dcl name =
    "  ~" ^ name ^"();\n"
  *)
  in
  let members = find_members syms bsym_table index ts in
  let name = cpp_instance_name syms bsym_table index ts in
    let ctor = ctor_dcl name in
  "struct " ^ name ^
  " {\n" ^
  (*
  "  //os frames\n" ^
  *)
  frame_dcls ^
  (*
  "  //display\n" ^
  *)
  (
    if String.length display_string = 0 then "" else
    display_string ^ "\n"
  )
  ^
  members ^
  (*
  "  //constructor\n" ^
  *)
  ctor ^
  (
    if mem `Heap_closure props then
    (*
    "  //clone\n" ^
    *)
    "  " ^name^"* clone();\n"
    else ""
  )
  ^
  (*
  "  //call\n" ^
  *)
  "};\n"

*)

let gen_function_names syms bsym_table =
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
  (fun (i,(index,ts)) ->
    let tss =
      if length ts = 0 then "" else
      "[" ^ catmap "," (sbt bsym_table) ts^ "]"
    in
    let bsym =
      try Flx_bsym_table.find bsym_table index with Not_found ->
        failwith ("[gen_functions] can't find index " ^ string_of_bid index)
    in
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,(ps,traint), _, effects,_) ->
      if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then begin
      end else begin
        let name = cpp_instance_name syms bsym_table index ts in
        bcat s ("struct " ^ name ^ ";\n");
      end

    | _ -> () (* bcat s ("//SKIPPING " ^ id ^ "\n") *)
  )
  (sort compare !xxsym_table)
  ;
  Buffer.contents s

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
      let is_proc = match ret with | BTYP_void | BTYP_fix (0,_) -> true | _ -> false in
      let name = if is_proc then "PROCEDURE" else "FUNCTION" in
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
  (sort compare !xxsym_table)
  ;
  Buffer.contents s

(*
let gen_dtor syms bsym_table name display ts =
  name^"::~"^name^"(){}\n"
*)

let unpack_args syms bsym_table shapes shape_table label_info index vs ts sr argtype bps params =
  assert (ts = []);
  let xps,_ = bps in
  match xps with
  | Flx_ast.Slist [] -> ""
  | Flx_ast.Satom {pindex=i} ->
    if Hashtbl.mem syms.instances (i, ts)
    && not (argtype = btyp_tuple [] || argtype = btyp_void ())
    then
      "  " ^ cpp_instance_name syms bsym_table i ts ^ " = _arg;\n"
    else ""
  | _ ->
    let ge' e : Flx_ctypes.cexpr_t = 
      Flx_egen.gen_expr' syms bsym_table shapes shape_table label_info index vs ts sr e 
    in
    let arg = bexpr_literal argtype {Flx_literal.felix_type="";internal_value=""; c_value="_arg"} in 

    if Flx_btype.islinear_type bsym_table argtype then
      let counter = ref 0 in
      List.fold_left begin fun s i ->
        let n = !counter in incr counter;
        if Hashtbl.mem syms.instances (i,ts)
        then
            let component = match argtype with
            | BTYP_array (v,idxt) -> 
              let index = bexpr_const_case (n,idxt) in
              Flx_ixgen.handle_get_n_array_clt syms bsym_table ge' index idxt v idxt argtype arg  
            | BTYP_tuple ls -> 
              let rt = List.nth ls n in
              let dummy = arg in (* dont care doesn't seem to be used *)
              Flx_ixgen.handle_get_n syms bsym_table ls rt ge' dummy argtype n arg
            | _ -> assert false
            in
            let component = Flx_cexpr.string_of_cexpr component in
            s ^ "  " ^ cpp_instance_name syms bsym_table i ts ^ " = " ^ component ^";\n"
        else s (* elide initialisation of elided variable *)
      end "" params
    else (* not clt *)
      let inits = List.map (fun ({pindex=pindex; ptyp=ptyp},prj) -> 
        let init = 
          match prj with
          | None -> arg
          | Some (_,BTYP_function (_,c) as prj) -> bexpr_apply c (prj,arg)
          | _ -> assert false
        in
        let init = Flx_cexpr.string_of_cexpr (ge' init) in
          "  " ^ cpp_instance_name syms bsym_table pindex ts ^ " = " ^ init ^ ";\n"
      )
      (Flx_bparams.get_prjs bps)
      in
      String.concat "" inits

(* PROCEDURES are implemented by continuations.
   The constructor accepts the display vector to
   form the closure object. The call method accepts
   the callers continuation object as a return address,
   and the procedure argument, and returns a continuation.
   The resume method runs the continuation until
   it returns a continuation to some object, possibly
   the same object. A flag in the continuation object
   determines whether the yield of control is a request
   for data or not (if so, the dispatcher must place the data
   in the nominated place before calling the resume method again.
*)

(* FUNCTIONS are implemented as functoids:
  the constructor accepts the display vector so as
  to form a closure object, the apply method
  accepts the argument and runs the function.
  The machine stack is used for functions.
*)
let gen_function_methods filename syms bsym_table (
   shapes: Flx_set.StringSet.t ref) shape_table
  label_info counter index ts instance_no : string * string
=
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("[gen_function_methods] can't find " ^ string_of_bid index)
  in
  let sr = Flx_bsym.sr bsym in
  let rt vs t = beta_reduce "flx_gen2" syms.Flx_mtypes2.counter bsym_table (Flx_bsym.sr bsym) (tsubst sr vs ts t) in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating function body inst " ^
    string_of_bid instance_no ^ "=" ^
    Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  let cxx_name = cid_of_flxid (Flx_bsym.id bsym) in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,bps,ret',effects,exes) ->
    let tailsr = Flx_bsym.sr bsym in
    if length ts <> length vs then
    failwith
    (
      "[get_function_methods} wrong number of args, expected vs = " ^
      si (length vs) ^
      ", got ts=" ^
      si (length ts)
    );
    let argtype = Flx_bparams.get_btype bps in
    let argtype = rt vs argtype in
    let rt' vs t = beta_reduce "flx_gen3" syms.Flx_mtypes2.counter bsym_table (Flx_bsym.sr bsym) (tsubst sr vs ts t) in
    let ret = rt' vs ret' in
    if ret = btyp_tuple [] then "// elided (returns unit)\n","" else

    let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, ret)) in

    let argtypename = cpp_typename syms bsym_table argtype in
    let name = cpp_instance_name syms bsym_table index ts in

    let display = get_display_list bsym_table index in

    let rettypename = cpp_typename syms bsym_table ret in

    let ctor =
      let vars = Flx_findvars.find_references syms bsym_table index ts in
      let funs = filter (fun (_,t) -> is_gc_pointer syms bsym_table (Flx_bsym.sr bsym) t) vars in
      gen_ctor syms bsym_table name display funs [] [] ts props
    in
    let params = Flx_bparams.get_bids bps in
    let exe_string,needs_switch =
      try
        Flx_gen_exe.gen_exes filename cxx_name syms bsym_table shapes shape_table
          display label_info counter index exes vs ts instance_no false
      with x ->
        (*
        print_endline (Printexc.to_string x);
        print_endline (catmap "\n" (string_of_bexe bsym_table 1) exes);
        print_endline "Can't gen exes ..";
        *)
        raise x
    in
    let cont = "::flx::rtl::con_t *" in
    let apply =
      rettypename^ " " ^name^
      "::apply("^
      (if argtype = btyp_tuple [] || argtype = btyp_void ()
      then ""
      else argtypename ^" const &_arg ")^
      "){\n" ^
      (*
      (if mem `Uses_gc props then
      "  gc_profile_t &gc = *PTF gcp;\n"
      else ""
      )
      ^
      *)
      ( unpack_args syms bsym_table shapes shape_table label_info index vs ts sr argtype bps params
      )^
        (if needs_switch then
        "  FLX_START_SWITCH\n" else ""
        ) ^
        exe_string ^
        (let f, sl, sc, el, ec = Flx_srcref.to_tuple tailsr in
         let s = string_of_string f ^ "," ^
         si sl ^ "," ^ si sc ^ "," ^
         si el ^ "," ^ si ec
        in
        "    FLX_DROPTHRU_FAILURE("^s^"); // HACK! \n") ^ (* HACK .. should be in exe_string .. *)
        (if needs_switch then
        "  FLX_END_SWITCH\n" else ""
        )
      ^
      "}\n"
    and clone =
      "  " ^ name ^ "* "^name^"::clone(){\n"^
      (if mem `Generator props then
      "  return this;\n"
      else
      "  return new(*PTF gcp,"^name^"_ptr_map,true) "^name^"(*this);\n"
      )^
      "}\n"
    in
      let q = qualified_name_of_bindex bsym_table index in
      let ctor =
      "//FUNCTION <" ^ string_of_bid index ^ ">: " ^ q ^ ": Constructor\n" ^
      ctor^ "\n" ^
      (
        if mem `Heap_closure props then
        "\n//FUNCTION <" ^ string_of_bid index ^ ">: " ^ q ^ ": Clone method\n" ^
        clone^ "\n"
        else ""
      )
      and apply =
      "//FUNCTION <" ^ string_of_bid index ^">: "  ^ q ^ ": Apply method\n" ^
      apply^ "\n"
      in apply,ctor


  | _ -> failwith "function expected"

let gen_procedure_methods filename syms bsym_table  
  (shapes: Flx_set.StringSet.t ref) shape_table
  label_info counter index ts instance_no : string * string
=
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("[gen_procedure_methods] Can't find index " ^
        string_of_bid index)
  in (* can't fail *)
  let sr = Flx_bsym.sr bsym in
  let rt vs t = beta_reduce "flx_gen4" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
  if syms.compiler_options.print_flag then
  print_endline
  (
    "//Generating procedure body inst " ^
    string_of_bid instance_no ^ "=" ^
    Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
    (
      if length ts = 0 then ""
      else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
    )
  );
  let cxx_name = cid_of_flxid (Flx_bsym.id bsym) in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,bps,BTYP_fix (0,_),effects,exes)
  | BBDCL_fun (props,vs,bps,BTYP_void,effects,exes) ->
    if length ts <> length vs then
    failwith
    (
      "[get_procedure_methods} wrong number of args, expected vs = " ^
      si (length vs) ^
      ", got ts=" ^
      si (length ts)
    );
    let stackable = mem `Stack_closure props in
    let heapable = mem `Heap_closure props in
    (*
    let heapable = not stackable or heapable in
    *)
    let argtype = Flx_bparams.get_btype bps in
    let argtype = rt vs argtype in
    let funtype = Flx_fold.fold bsym_table syms.counter (btyp_function (argtype, btyp_void ())) in

    let argtypename = cpp_typename syms bsym_table argtype in
    let name = cpp_instance_name syms bsym_table index ts in

    let display = get_display_list bsym_table index in

    let ctor =
      let vars = Flx_findvars.find_references syms bsym_table index ts in
      let funs = filter (fun (i,t) -> is_gc_pointer syms bsym_table (Flx_bsym.sr bsym) t) vars in
      gen_ctor syms bsym_table name display funs [] [] ts props
    in

    (*
    let dtor = gen_dtor syms bsym_table name display ts in
    *)
    let ps = List.map (fun {pid=id; pindex=ix; ptyp=t} -> id,t) (Flx_bparams.get_params bps) in
    let params = Flx_bparams.get_bids bps in
    let exe_string,needs_switch =
      Flx_gen_exe.gen_exes filename cxx_name syms bsym_table shapes shape_table display label_info counter index exes vs ts instance_no (stackable && not heapable)
(*
      Flx_gen_exe.gen_exes filename syms bsym_table display label_info counter index exes vs ts instance_no stackable
*)
    in

    let cont = "::flx::rtl::con_t *" in
    let heap_call_arg_sig, heap_call_arg =
      match argtype with
      | BTYP_tuple [] -> cont ^ "_ptr_caller","0"
      | _ -> cont ^ "_ptr_caller, " ^ argtypename ^" const &_arg","0,_arg"
    and stack_call_arg_sig =
      match argtype with
      | BTYP_tuple [] -> ""
      | _ -> argtypename ^" const &_arg"
    in
    let unpacked_args =
      unpack_args syms bsym_table shapes shape_table label_info index vs ts sr argtype bps params
    in
(*
      match bps with
      | [] -> ""
      | [{pindex=i}] ->
          if Hashtbl.mem syms.instances (i,ts)
          && not (argtype = btyp_tuple [] || argtype = btyp_void ())
          then
            "  " ^ cpp_instance_name syms bsym_table i ts ^ " = _arg;\n"
          else ""

      | _ ->
          let counter = ref 0 in
          List.fold_left begin fun s i ->
            let n = !counter in incr counter;
            if Hashtbl.mem syms.instances (i,ts)
            then
              let memexpr =
                match argtype with
                | BTYP_array _ -> ".data["^si n^"]"
                | BTYP_tuple _ -> ".mem_"^ si n
                | _ -> assert false
              in
              s ^ "  " ^ cpp_instance_name syms bsym_table i ts ^ " = _arg" ^ memexpr ^";\n"
            else s (* elide initialisation of elided variables *)
          end "" params
*)
    let stack_call =
        "void " ^name^ "::stack_call(" ^ stack_call_arg_sig ^ "){\n" ^
        (
          if not heapable
          then unpacked_args ^ exe_string
          else
            "  ::flx::rtl::con_t *cc = call("^heap_call_arg^");\n" ^
            "  while(cc) cc = cc->resume();\n"
        ) ^ "\n}\n"
    and heap_call =
        cont ^ " " ^ name ^ "::call(" ^ heap_call_arg_sig ^ "){\n" ^
        "  _caller = _ptr_caller;\n" ^
        unpacked_args ^
        "  INIT_PC\n" ^
        "  return this;\n}\n"
    and resume =
      if exes = []
      then
        cont^name^"::resume(){//empty\n"^
        "     FLX_RETURN\n" ^
        "}\n"
      else
        cont^name^"::resume(){\n"^
        (if needs_switch then
        "  FLX_START_SWITCH\n" else ""
        ) ^
        exe_string ^
        (if needs_switch then 
        "      FLX_KILLPC\n" 
        else "") ^
        "    FLX_RETURN\n" ^ (* HACK .. should be in exe_string .. *)
        (if needs_switch then
        "  FLX_END_SWITCH\n" else ""
        )^
        "}\n"
    and clone =
      "  " ^name^"* "^name^"::clone(){\n" ^
        "  return new(*PTF gcp,"^name^"_ptr_map,true) "^name^"(*this);\n" ^
        "}\n"
    in
      let q =
        try qualified_name_of_bindex bsym_table index
        with Not_found ->
          string_of_bid instance_no ^ "=" ^
          Flx_bsym.id bsym ^ "<" ^ string_of_bid index ^ ">" ^
          (
            if length ts = 0 then ""
            else "[" ^ catmap "," (sbt bsym_table) ts ^ "]"
          )
      in
      let ctor =
      "//PROCEDURE <" ^ string_of_bid index ^ ":> " ^ q ^ ": Constructor\n" ^
      ctor^
      (
        if mem `Heap_closure props then
        "\n//PROCEDURE <" ^ string_of_bid index ^ ":> " ^ q ^ ": Clone method\n" ^
        clone
        else ""
      )
      and call =
      "\n//PROCEDURE <" ^ string_of_bid index ^ ":> " ^ q ^ ": Call method\n" ^
      (if stackable then stack_call else "") ^
      (if heapable then heap_call else "") ^
      (if heapable then
        "\n//PROCEDURE <" ^ string_of_bid index ^ ":> " ^ q ^ ": Resume method\n" ^
        resume
        else ""
      )
      in call,ctor

  | _ -> failwith "procedure expected"


let gen_execute_methods filename syms bsym_table 
  (shapes: Flx_set.StringSet.t ref) shape_table
  label_info counter bf bf2 
=
  let s = Buffer.create 2000 in
  let s2 = Buffer.create 2000 in
  Hashtbl.iter
  (fun (index,ts) instance_no ->
  let bsym =
    try Flx_bsym_table.find bsym_table index with Not_found ->
      failwith ("[gen_execute_methods] Can't find index " ^ string_of_bid index)
  in
  let sr = Flx_bsym.sr bsym in
  let cxx_name = cid_of_flxid (Flx_bsym.id bsym) in

  begin match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (props,vs,(ps,traint),BTYP_fix (0,_),effects,_)
  | BBDCL_fun (props,vs,(ps,traint),BTYP_void,effects,_) ->
    bcat s ("//------------------------------\n");
    if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then
      bcat s (
        Flx_gen_cfunc.gen_C_procedure_body filename syms bsym_table shapes shape_table
        label_info counter index ts (Flx_bsym.sr bsym) instance_no
      )
    else
      let call,ctor =
        gen_procedure_methods filename syms bsym_table shapes shape_table
        label_info counter index ts instance_no
      in
      bcat s call;
      bcat s2 ctor

  | BBDCL_fun (props,vs,(ps,traint),ret,effects,_) ->
    bcat s ("//------------------------------\n");
    if mem `Cfun props || mem `Pure props && not (mem `Heap_closure props) then
      bcat s (
        Flx_gen_cfunc.gen_C_function_body filename syms bsym_table shapes shape_table
        label_info counter index ts (Flx_bsym.sr bsym) instance_no
      )
    else
      let apply,ctor =
        gen_function_methods filename syms bsym_table shapes shape_table
        label_info counter index ts instance_no
      in
      bcat s2 ctor;
      bcat s apply

  | BBDCL_external_fun (_,vs,ps_cf,ret',_,_,`Callback (ps_c,client_data_pos)) ->
      let tss =
        if length ts = 0 then "" else
        "[" ^ catmap "," (sbt bsym_table) ts^ "]"
      in
      bcat s ("\n//------------------------------\n");
      if ret' = btyp_void () then begin
        bcat s ("//CALLBACK C PROCEDURE <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^ "\n");
      end else begin
        bcat s ("//CALLBACK C FUNCTION <" ^ string_of_bid index ^ ">: " ^
          qualified_name_of_bindex bsym_table index ^ tss ^ "\n");
      end
      ;
      let rt vs t = beta_reduce "flx_gen5" syms.Flx_mtypes2.counter bsym_table sr (tsubst sr vs ts t) in
      let ps_c = List.map (rt vs) ps_c in
      let ps_cf = List.map (rt vs) ps_cf in
      let ret = rt vs ret' in
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
        "[gen_function] wrong number of args, expected vs = " ^
        si (length vs) ^
        ", got ts=" ^
        si (length ts)
      );
      (*
      let name = cpp_instance_name syms bsym_table index ts in
      *)
      let name = Flx_bsym.id bsym in (* callbacks can't be polymorphic .. for now anyhow *)
      let rettypename = cpp_typename syms bsym_table ret in
      let n = length ps_c in
      let flx_fun_atypes =
        rev
        (
          fold_left
          (fun lst (t,i) ->
            if i = client_data_pos
            then lst
            else (t,i)::lst
          )
          []
          (combine ps_c (nlist n))
        )
      in
      let flx_fun_atype =
        if length flx_fun_atypes = 1 then fst (hd flx_fun_atypes)
        else btyp_tuple (List.map fst flx_fun_atypes)
      in
      let flx_fun_reduced_atype = rt vs flx_fun_atype in
      let flx_fun_atype_name = cpp_typename syms bsym_table flx_fun_atype in
      let flx_fun_reduced_atype_name = cpp_typename syms bsym_table flx_fun_reduced_atype in
      let flx_fun_args = List.map (fun (_,i) -> "_a" ^ si i) flx_fun_atypes in
      let flx_fun_arg = match length flx_fun_args with
        | 0 -> ""
        | 1 -> hd flx_fun_args
        | _ ->
          (* argument tuple *)
          let a = flx_fun_atype_name ^ "(" ^ String.concat "," flx_fun_args ^")" in
          if flx_fun_reduced_atype_name <> flx_fun_atype_name
          then "reinterpret<" ^ flx_fun_reduced_atype_name ^ ">("^a^")/*flx_gen*/"
          else a

      in
      let sss =
        (* return type *)
        rettypename ^ " " ^

        (* function name *)
        name ^ "(" ^
        (
          (* parameter list *)
          match length ps_c with
          | 0 -> ""
          | 1 -> cpp_typename syms bsym_table (hd ps_c) ^ " _a0"
          | _ ->
            fold_left
            (fun s (t,j) ->
              s ^
              (if String.length s > 0 then ", " else "") ^
              cpp_typename syms bsym_table t ^ " _a" ^ si j
            )
            ""
            (combine ps_c (nlist n))
        ) ^
        "){\n"^
        (
          (* body *)
          let flx_fun_type = nth ps_cf client_data_pos in
          let flx_fun_type_name = cpp_typename syms bsym_table flx_fun_type in
          (* cast *)
          "  " ^ flx_fun_type_name ^ " callback = ("^flx_fun_type_name^")_a" ^ si client_data_pos ^ ";\n" ^
          (
            if ret = btyp_void () then begin
              "  ::flx::rtl::con_t *p = callback->call(0" ^
              (if String.length flx_fun_arg > 0 then "," ^ flx_fun_arg else "") ^
              ");\n" ^
              "  while(p)p = p->resume();\n"
            end else begin
              "  return callback->apply(" ^ flx_fun_arg ^ ");\n";
            end
          )
        )^
        "  }\n"
      in bcat s sss

  | _ -> ()
  end
  ;
  output_string bf (Buffer.contents s);
  output_string bf2 (Buffer.contents s2);
  Buffer.clear s;
  Buffer.clear s2;
  )
  syms.instances
(*  Generate Python module initialisation entry point
if a Python module function is detected as an export
*)


