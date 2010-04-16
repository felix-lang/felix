type codegen_state_t =
  {
    syms: Flx_mtypes2.sym_state_t;
    context: Llvm.llcontext;
    the_module: Llvm.llmodule;
    the_fpm: [`Function] Llvm.PassManager.t;
    the_ee: Llvm_executionengine.ExecutionEngine.t;
    type_bindings: (Flx_types.bid_t, Llvm.lltype) Hashtbl.t;
    call_bindings: (Flx_types.bid_t, call_t) Hashtbl.t;
    value_bindings: (Flx_types.bid_t, Llvm.llvalue) Hashtbl.t;
    label_bindings: (string, Llvm.llbasicblock) Hashtbl.t;
    closure_type_bindings: (Flx_types.bid_t, Llvm.lltype) Hashtbl.t;
    closure_bindings: (Flx_types.bid_t, Llvm.llvalue) Hashtbl.t;
    name_bindings:
      (string, Flx_types.bid_t * Flx_btype.t list) Hashtbl.t;
  }
and call_t =
  codegen_state_t ->
  Flx_bsym_table.t ->
  Llvm.llbuilder ->
  Flx_srcref.t ->
  Flx_bexpr.t list ->
  Llvm.llvalue

(* Used to represent what kind of closure to use to store values. *)
type closure_kind_t =
  | Global
  | Stack of Llvm.llvalue * Llvm.llbuilder
  | Stack_closure of Flx_types.bid_t * Llvm.llvalue * Llvm.llbuilder


let make_codegen_state syms optimization_level =
  let context = Llvm.create_context () in
  let the_module = Llvm.create_module context "__root__" in

  (* Set up the llvm optimizer and execution engine *)
  let the_ee = Llvm_executionengine.ExecutionEngine.create the_module in
  let the_fpm = Llvm.PassManager.create_function the_module in

  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add
  (Llvm_executionengine.ExecutionEngine.target_data the_ee)
  the_fpm;

  if optimization_level >= 1 then begin
    (* Promote allocas to registers. *)
    Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;

    (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
    Llvm_scalar_opts.add_instruction_combination the_fpm;

    (* reassociate expressions. *)
    Llvm_scalar_opts.add_reassociation the_fpm;

    (* Eliminate Common SubExpressions. *)
    Llvm_scalar_opts.add_gvn the_fpm;

    (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
    Llvm_scalar_opts.add_cfg_simplification the_fpm;
  end;

  ignore (Llvm.PassManager.initialize the_fpm);

  {
    syms = syms;
    context = context;
    the_module = the_module;
    the_fpm = the_fpm;
    the_ee = the_ee;
    type_bindings = Hashtbl.create 97;
    call_bindings = Hashtbl.create 97;
    value_bindings = Hashtbl.create 97;
    label_bindings = Hashtbl.create 97;
    closure_bindings = Hashtbl.create 97;
    closure_type_bindings = Hashtbl.create 97;
    name_bindings = Hashtbl.create 97;
  }


(* Convert a literal suffix into an llvm type *)
let lltype_of_suffix state suffix =
  match suffix with
  | "tiny" | "utiny" | "int8"  | "uint8"  ->
      Llvm.i8_type state.context
  | "short" | "ushort" | "int16" | "uint16" ->
      Llvm.i16_type state.context
  | "int" | "uint" | "long" | "ulong" | "int32" | "uint32" ->
      Llvm.i32_type state.context
  | "vlong" | "uvlong" | "int64" | "uint64" ->
      Llvm.i64_type state.context
  | "float" ->
      Llvm.float_type state.context
  | "double" ->
      Llvm.double_type state.context
  | "ldouble" ->
      Llvm.fp128_type state.context
  | _ ->
      failwith ("[lltype_of_suffix] Unexpected Type " ^ suffix)


(* Convenience function to look up the name of an index *)
let name_of_index state bsym_table bid ts =
  let rec aux bid ts =
    (* Recursively prepend the name of the parent to *)
    let name, ts =
      let bsym = Flx_bsym_table.find bsym_table bid in
      let bsym_parent = Flx_bsym_table.find_parent bsym_table bid in
      let ts = Flx_bbdcl.get_ts (Flx_bsym.bbdcl bsym) in
      match bsym_parent with
      | None -> Flx_bsym.id bsym, ts
      | Some parent ->
          let name = aux parent ts in
          let name =
            if String.length name = 0
            then Flx_bsym.id bsym
            else name ^ "." ^ Flx_bsym.id bsym
          in
          name, ts
    in
    (* Check our name cache if we need to mangle the function name. *)
    match Flx_hashtbl.find_opt state.name_bindings name with
    | None ->
        (* It's not in the cache, so claim the name and return it. *)
        Hashtbl.add state.name_bindings name (bid, ts);
        name

    | Some (bid', ts') ->
        (* Uh oh, someone else has this name, so lets mangle the name. *)
        name ^ "$_i" ^ Flx_print.string_of_bid bid
  in
  (* Prefix '_Z' in order to not conflict with any exported symbols. *)
  "_Z" ^ (aux bid ts)


(* Convenience function to get the string value of an Llvm.TypeKind. *)
let name_of_typekind = function
  | Llvm.TypeKind.Void -> "void"
  | Llvm.TypeKind.Float -> "float"
  | Llvm.TypeKind.Double -> "double"
  | Llvm.TypeKind.X86fp80 -> "x86fp80"
  | Llvm.TypeKind.Fp128 -> "fp128"
  | Llvm.TypeKind.Ppc_fp128 -> "ppc_fp128"
  | Llvm.TypeKind.Label -> "label"
  | Llvm.TypeKind.Integer -> "integer"
  | Llvm.TypeKind.Function -> "function"
  | Llvm.TypeKind.Struct -> "struct"
  | Llvm.TypeKind.Array -> "array"
  | Llvm.TypeKind.Pointer -> "pointer"
  | Llvm.TypeKind.Opaque -> "opaque"
  | Llvm.TypeKind.Vector -> "vector"
  | Llvm.TypeKind.Metadata -> "metadata"
  | Llvm.TypeKind.Union -> "union"


(* Convenience function to check we're dealing with the right types. *)
let check_type sr typ expected_type =
  if typ != expected_type then
    Flx_exceptions.clierr sr ("invalid type. got " ^
      Llvm.string_of_lltype typ ^
      " expected " ^
      Llvm.string_of_lltype expected_type)


(* Convenience function to check we're dealing with the right typekind. *)
let check_typekind sr value expected_typekind =
  let typekind = Llvm.classify_type (Llvm.type_of value) in
  if typekind != expected_typekind then
    Flx_exceptions.clierr sr ("invalid type. got " ^
      name_of_typekind typekind ^
      " expected " ^
      name_of_typekind expected_typekind)


(* Convenience wrapper to creating a gep. *)
let codegen_gep state value args name builder =
  let args = Array.map
    (fun i -> Llvm.const_int (Llvm.i32_type state.context) i)
    args
  in
  Llvm.build_gep value args name builder


(* Convert a felix type to an llvm type. *)
let rec lltype_of_btype state btype =
  match btype with
  | Flx_btype.BTYP_none -> assert false

  | Flx_btype.BTYP_inst (index, ts) ->
      begin try Hashtbl.find state.type_bindings index with Not_found ->
        failwith ("[lltype_of_btype:BTYP_inst] unable to find index " ^
          Flx_print.string_of_bid index)
      end

  | Flx_btype.BTYP_tuple ls ->
      let ls = List.map (lltype_of_btype state) ls in
      Llvm.struct_type state.context (Array.of_list ls)

  | Flx_btype.BTYP_record ls -> assert false
  | Flx_btype.BTYP_variant ls -> assert false
  | Flx_btype.BTYP_unitsum k -> Llvm.integer_type state.context k
  | Flx_btype.BTYP_sum ls -> assert false

  | Flx_btype.BTYP_function (args, ret_type) ->
      let args =
        match args with
        | Flx_btype.BTYP_tuple args -> List.map (lltype_of_btype state) args
        | _ -> [lltype_of_btype state args]
      in
      let ret_type = lltype_of_btype state ret_type in
      Llvm.function_type ret_type (Array.of_list args)

  | Flx_btype.BTYP_cfunction (args, result) -> assert false
  | Flx_btype.BTYP_pointer t -> Llvm.pointer_type (lltype_of_btype state t)
  | Flx_btype.BTYP_array (t1, Flx_btype.BTYP_unitsum k) ->
      let t1 = lltype_of_btype state t1 in
      Llvm.array_type t1 k

  | Flx_btype.BTYP_array (t1, t2) -> assert false
  | Flx_btype.BTYP_void -> Llvm.void_type state.context
  | Flx_btype.BTYP_fix i -> assert false
  | Flx_btype.BTYP_intersect ls -> assert false
  | Flx_btype.BTYP_type_var (i, mt) -> assert false
  | Flx_btype.BTYP_type_apply (t1, t2) -> assert false
  | Flx_btype.BTYP_type_function (args, result, body) -> assert false
  | Flx_btype.BTYP_type i -> assert false
  | Flx_btype.BTYP_type_tuple ls -> assert false
  | Flx_btype.BTYP_type_match (t, ps) -> assert false
  | Flx_btype.BTYP_type_set ls -> assert false
  | Flx_btype.BTYP_type_set_union ls -> assert false
  | Flx_btype.BTYP_type_set_intersection ls -> assert false


(* Convenience function to find the parent of the builder. *)
let builder_parent builder =
  (* First we need to get the current basic block. *)
  let bb = Llvm.insertion_block builder in

  (* Then, return the basic block's parent. *)
  Llvm.block_parent bb


(* Generate code for a literal *)
let codegen_literal state builder sr literal =
  match literal with
  | Flx_ast.AST_float (suffix, f) ->
      Llvm.const_float_of_string (lltype_of_suffix state suffix) f
  | Flx_ast.AST_int (suffix, i) ->
      Llvm.const_int_of_string
        (lltype_of_suffix state suffix)
        (Big_int.string_of_big_int i)
        10
  | Flx_ast.AST_cstring s ->
      (* Create a global constant string. *)
      let c = Llvm.const_stringz state.context s in
      let g = Llvm.define_global "" c state.the_module in
      Llvm.set_linkage Llvm.Linkage.Internal g;

      (* Return a gep to the value in order to be the right type. *)
      let zero = Llvm.const_int (Llvm.i32_type state.context) 0 in
      Llvm.build_gep g [| zero; zero |] "" builder

  | _ ->
      assert false


(* Create an alloca instruction in the entry block of the function. This
 * is used for mutable variables etc. *)
let create_entry_block_alloca state builder btype name =
  (* Get the builder's function. *)
  let the_function = builder_parent builder in

  (* Get a builder at the entry block. *)
  let builder = Llvm.builder_at
    state.context
    (Llvm.instr_begin (Llvm.entry_block the_function))
  in
  Llvm.build_alloca (lltype_of_btype state btype) name builder


(* Generate call for an expression *)
let rec codegen_expr state bsym_table builder sr tbexpr =
  print_endline ("codegen_expr: " ^ Flx_print.string_of_bound_expression
    bsym_table tbexpr);

  (* See if there are any simple reductions we can apply to the expression. *)
  let bexpr, btype = Flx_bexpr.reduce tbexpr in

  match bexpr with
  | Flx_bexpr.BEXPR_deref e ->
      print_endline "BEXPR_deref";
      codegen_deref state bsym_table builder sr e

  | Flx_bexpr.BEXPR_name (index, _) ->
      print_endline "BEXPR_name";
      begin try Hashtbl.find state.value_bindings index with Not_found ->
        Flx_exceptions.clierr sr ("Unable to find index " ^
          Flx_print.string_of_bid index)
      end

  | Flx_bexpr.BEXPR_ref (index, btype) ->
      print_endline "BEXPR_ref";
      assert false

  | Flx_bexpr.BEXPR_likely e ->
      print_endline "BEXPR_likely";
      (* Do nothing for now *)
      codegen_expr state bsym_table builder sr e

  | Flx_bexpr.BEXPR_unlikely e ->
      print_endline "BEXPR_unlikely";
      (* Do nothing for now *)
      codegen_expr state bsym_table builder sr e

  | Flx_bexpr.BEXPR_address e ->
      print_endline "BEXPR_address";

      let e = codegen_expr state bsym_table builder sr e in

      (* Make sure we've got a pointer. *)
      check_typekind sr e Llvm.TypeKind.Pointer;

      (* Expressions can only have their address taken if they're on the stack.
       * So, we shouldn't need to do any work. *)
      e

  | Flx_bexpr.BEXPR_new e ->
      print_endline "BEXPR_new";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_literal literal ->
      print_endline "BEXPR_literal";
      codegen_literal state builder sr literal

  | Flx_bexpr.BEXPR_apply (f, e) ->
      print_endline "BEXPR_apply";
      assert false

  | Flx_bexpr.BEXPR_apply_direct (bid, _, e)
  | Flx_bexpr.BEXPR_apply_prim (bid, _, e) ->
      print_endline "BEXPR_apply_{direct,prim}";
      codegen_apply_direct state bsym_table builder sr bid e

  | Flx_bexpr.BEXPR_apply_stack (bid, _, e) ->
      print_endline "BEXPR_apply_stack";
      codegen_apply_stack state bsym_table builder sr bid e

  | Flx_bexpr.BEXPR_apply_struct (index, _, e) ->
      print_endline "BEXPR_apply_struct";
      assert false

  | Flx_bexpr.BEXPR_tuple es ->
      print_endline "BEXPR_tuple";
      codegen_struct state bsym_table builder sr es btype

  | Flx_bexpr.BEXPR_record es ->
      print_endline "BEXPR_record";
      codegen_struct state bsym_table builder sr (List.map snd es) btype

  | Flx_bexpr.BEXPR_variant (string, e) ->
      print_endline "BEXPR_variant";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_get_n (n, e) ->
      print_endline "BEXPR_get_n";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_closure (bid, ts) ->
      print_endline ("BEXPR_closure: " ^ name_of_index state bsym_table bid ts);
      begin try Hashtbl.find state.value_bindings bid with Not_found ->
        Flx_exceptions.clierr sr ("Unable to find index " ^
          Flx_print.string_of_bid bid)
      end

  | Flx_bexpr.BEXPR_case (index, btype) ->
      print_endline "BEXPR_case";
      begin match btype with
      | Flx_btype.BTYP_sum _
      | Flx_btype.BTYP_unitsum _
      | Flx_btype.BTYP_variant _ ->
          if Flx_btype.is_unitsum btype then
            (* Construct a constant value of the same type as the unitsum. *)
            let t = lltype_of_btype state btype in
            Llvm.const_int t index
          else
            assert false
      | _ ->
          assert false
      end

  | Flx_bexpr.BEXPR_match_case (int, e) ->
      print_endline "BEXPR_match_case";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_case_arg (int, e) ->
      print_endline "BEXPR_case_arg";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_case_index e ->
      print_endline "BEXPR_case_index";
      let _ = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexpr.BEXPR_expr (string, btype) ->
      print_endline "BEXPR_expr";
      assert false

  | Flx_bexpr.BEXPR_range_check (e1, e2, e3) ->
      print_endline "BEXPR_range_check";
      let _ = codegen_expr state bsym_table builder sr e1 in
      let _ = codegen_expr state bsym_table builder sr e2 in
      let _ = codegen_expr state bsym_table builder sr e3 in
      assert false

  | Flx_bexpr.BEXPR_coerce (tbexpr, btype) ->
      print_endline "BEXPR_coerce";
      assert false

(* Generate code for an llvm struct type. *)
and codegen_struct state bsym_table builder sr es btype =
  let the_struct = create_entry_block_alloca state builder btype "" in
  load_struct state bsym_table builder sr the_struct es


and load_struct state bsym_table builder sr the_struct es =
  check_typekind sr the_struct Llvm.TypeKind.Pointer;

  (* Add the values to the struct. *)
  let _ =
    List.fold_left begin fun i e ->
      let lhs = codegen_gep state the_struct [| 0; i |] "" builder in
      let rhs = codegen_expr state bsym_table builder sr e in

      (* Make sure we've got the right types. *)
      check_type sr (Llvm.type_of lhs) (Llvm.pointer_type (Llvm.type_of rhs));

      ignore (Llvm.build_store rhs lhs builder);

      i + 1
    end 0 es
  in
  the_struct


(* Optionally dereference a value if it's a pointer. *)
and codegen_deref state bsym_table builder sr ((bexpr,_) as e) =
  let e = codegen_expr state bsym_table builder sr e in

  (* Literal values aren't dereferenced. *)
  match bexpr with
  | Flx_bexpr.BEXPR_literal _ -> e
  | _ ->
      (* Dereference only if we've gotten a pointer *)
      match Llvm.classify_type (Llvm.type_of e) with
      | Llvm.TypeKind.Pointer -> Llvm.build_load e "" builder
      | _ -> e


and codegen_apply sr f args name builder =
  (* Directly call the function. *)
  let args = Array.of_list args in
  let params = Llvm.params f in

  (* Make sure the number of arguments equals the function arguments. *)
  if Array.length args != Array.length params then
    failwith ("Not enough arguments for " ^ Llvm.value_name f);

  (* Make sure that the types are the same. *)
  Array.iteri begin fun i arg ->
    check_type sr (Llvm.type_of arg) (Llvm.type_of params.(i))
  end args;

  Llvm.build_call f args name builder


and codegen_apply_direct state bsym_table builder sr bid e =
  let es =
    match e with
    | Flx_bexpr.BEXPR_tuple es, _ -> es
    | _ -> [e]
  in

  let f =
    try Hashtbl.find state.call_bindings bid with Not_found ->
      Flx_exceptions.clierr sr ("Unable to find bid " ^
        Flx_print.string_of_bid bid)
  in
  f state bsym_table builder sr es


and codegen_apply_stack state bsym_table builder sr bid e =
  let f =
    try Hashtbl.find state.value_bindings bid with Not_found ->
      Flx_exceptions.clierr sr ("Unable to find bid " ^
        Flx_print.string_of_bid bid)
  in

  Llvm.dump_value f;

  (* First get all the arguments. *)
  let es =
    match e with
    | Flx_bexpr.BEXPR_tuple es, _ -> es
    | _ -> [e]
  in
  let es = List.map (codegen_deref state bsym_table builder sr) es in

  (* Now, add all of the closures it needs. *)
  let display = Flx_display.get_display_list bsym_table bid in
  let es = List.fold_left
    (fun es (bid,_) -> Hashtbl.find state.closure_bindings bid :: es)
    es display
  in

  codegen_apply sr f es "" builder


let codegen_call_direct state bsym_table builder sr f args =
  let args = List.map (codegen_deref state bsym_table builder sr) args in
  codegen_apply sr f args "" builder


let create_unary_llvm_inst f typekind =
  fun state bsym_table builder sr e ->
    let e = codegen_deref state bsym_table builder sr e in
    check_typekind sr e typekind;

    f e "" builder


let codegen_lnot = create_unary_llvm_inst
  begin fun e name builder ->
    let t = Llvm.type_of e in

    (* Compare the integer to zero. *)
    let e = Llvm.build_icmp Llvm.Icmp.Eq e (Llvm.const_int t 0) "" builder in

    (* 0-extend the result to the expected integer type. *)
    Llvm.build_zext e t name builder
  end
  Llvm.TypeKind.Integer


let create_binary_llvm_inst f lhs_typekind rhs_typekind =
  fun state bsym_table builder sr lhs rhs ->
    let lhs = codegen_deref state bsym_table builder sr lhs in
    check_typekind sr lhs lhs_typekind;

    let rhs = codegen_deref state bsym_table builder sr rhs in
    check_typekind sr rhs rhs_typekind;

    f lhs rhs "" builder


let codegen_add = create_binary_llvm_inst
  Llvm.build_add
  Llvm.TypeKind.Integer
  Llvm.TypeKind.Integer

let codegen_sub = create_binary_llvm_inst
  Llvm.build_sub
  Llvm.TypeKind.Integer
  Llvm.TypeKind.Integer

let codegen_eq = create_binary_llvm_inst
  (Llvm.build_icmp Llvm.Icmp.Eq)
  Llvm.TypeKind.Integer
  Llvm.TypeKind.Integer

let codegen_ne = create_binary_llvm_inst
  (Llvm.build_icmp Llvm.Icmp.Ne)
  Llvm.TypeKind.Integer
  Llvm.TypeKind.Integer

let codegen_subscript state bsym_table builder sr lhs rhs =
  let lhs = codegen_expr state bsym_table builder sr lhs in
  check_typekind sr lhs Llvm.TypeKind.Pointer;

  let rhs = codegen_deref state bsym_table builder sr rhs in
  check_typekind sr rhs Llvm.TypeKind.Integer;

  let zero = Llvm.const_int (Llvm.i32_type state.context) 0 in
  let gep = Llvm.build_gep lhs [| zero; rhs |] "" builder in
  Llvm.build_load gep "" builder


(* Generate code for a bound statement. *)
let codegen_bexe state bsym_table builder bexe =
  print_endline ("codegen_bexe: " ^ Flx_print.string_of_bexe
    bsym_table 0 bexe);

  (* See if there are any simple reductions we can apply to the exe. *)
  let bexe = Flx_bexe.reduce bexe in

  match bexe with
  | Flx_bexe.BEXE_label (sr, label) ->
      print_endline "BEXE_label";

      (* Find or create the basic block of the label *)
      let bb =
        try Hashtbl.find state.label_bindings label with Not_found ->
          (* The label doesn't exist yet, so let's make it. *)
          let the_function = builder_parent builder in
          let bb = Llvm.append_block state.context label the_function in

          (* Unconditional branch to the basic block. *)
          ignore (Llvm.build_br bb builder);

          Hashtbl.add state.label_bindings label bb;
          bb
      in

      (* Set the builder to start generating code on that basic block. *)
      Llvm.position_at_end bb builder

  | Flx_bexe.BEXE_comment (sr, string) ->
      (* Ignore the comment. *)
      ()

  | Flx_bexe.BEXE_halt (sr, string) ->
      print_endline "BEXE_halt";
      assert false

  | Flx_bexe.BEXE_trace (sr, s1, s2) ->
      print_endline "BEXE_trace";
      assert false

  | Flx_bexe.BEXE_goto (sr, label) ->
      print_endline "BEXE_goto";

      (* Find the basic block of the label. *)
      let bb =
        try Hashtbl.find state.label_bindings label with Not_found ->
          (* The label doesn't exist yet, so let's make it. *)
          let the_function = builder_parent builder in
          let bb = Llvm.append_block state.context label the_function in
          Hashtbl.add state.label_bindings label bb;
          bb
      in

      (* Branch to that basic block. *)
      ignore (Llvm.build_br bb builder)

  | Flx_bexe.BEXE_ifgoto (sr, e, label) ->
      print_endline "BEXE_ifgoto";
      let e = codegen_expr state bsym_table builder sr e in

      (* Get the builder's current function. *)
      let the_function = builder_parent builder in

      (* Find the basic block of the label. *)
      let then_bb =
        try Hashtbl.find state.label_bindings label with Not_found ->
          (* The label doesn't exist yet, so let's make it. *)
          let bb = Llvm.append_block state.context label the_function in
          Hashtbl.add state.label_bindings label bb;
          bb
      in

      (* Create another basic block if the comparison fails. *)
      let else_bb = Llvm.append_block state.context "else" the_function in

      (* Emit the branch. *)
      ignore (Llvm.build_cond_br e then_bb else_bb builder);

      (* Continue with the else branch. *)
      Llvm.position_at_end else_bb builder

  | Flx_bexe.BEXE_call (sr, p, a) ->
      let e1 = codegen_expr state bsym_table builder sr p in
      let e2 = codegen_expr state bsym_table builder sr a in
      assert false

  | Flx_bexe.BEXE_call_direct (sr, bid, _, e)
  | Flx_bexe.BEXE_call_prim (sr, bid, _, e) ->
      print_endline "BEXE_call_{direct,prim}";
      ignore (codegen_apply_direct state bsym_table builder sr bid e)

  | Flx_bexe.BEXE_call_stack (sr, bid, _, e) ->
      print_endline "BEXE_call_stack";
      ignore (codegen_apply_stack state bsym_table builder sr bid e)

  | Flx_bexe.BEXE_jump (sr, e1, e2) ->
      print_endline "BEXE_jump";
      assert false

  | Flx_bexe.BEXE_jump_direct (sr, index, _, e) ->
      print_endline "BEXE_jump_direct";
      let e = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexe.BEXE_svc (sr, index) ->
      print_endline "BEXE_svc";
      assert false

  | Flx_bexe.BEXE_fun_return (sr, e) ->
      print_endline "BEXE_fun_return";
      let e = codegen_deref state bsym_table builder sr e in

      ignore (Llvm.build_ret e builder);

  | Flx_bexe.BEXE_yield (sr, e) ->
      print_endline "BEXE_yield";
      let e = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexe.BEXE_proc_return sr ->
      print_endline "BEXE_proc_return";
      ignore (Llvm.build_ret_void builder);

  | Flx_bexe.BEXE_nop (sr, string) ->
      print_endline "BEXE_nop";
      assert false

  | Flx_bexe.BEXE_code (sr, code_spec_t) ->
      print_endline "BEXE_code";
      assert false

  | Flx_bexe.BEXE_nonreturn_code (sr, code_spec_t) ->
      print_endline "BEXE_nonreturn_code";
      assert false

  | Flx_bexe.BEXE_assign (sr, lhs, rhs) ->
      print_endline "BEXE_assign";

      (* We can only assign to a name *)
      let lhs =
        match lhs with
        | Flx_bexpr.BEXPR_name (index, _), _ ->
            begin try Hashtbl.find state.value_bindings index with Not_found ->
              Flx_exceptions.clierr sr ("Unable to find index " ^
                Flx_print.string_of_bid index)
            end
        | _ ->
            Flx_exceptions.clierr sr ("invalid lvalue")
      in
      let rhs = codegen_expr state bsym_table builder sr rhs in

      (* Check to make sure we're dealing with the right types. *)
      check_type
        sr
        (Llvm.type_of lhs)
        (Llvm.pointer_type (Llvm.type_of rhs));

      ignore (Llvm.build_store rhs lhs builder)

  | Flx_bexe.BEXE_init (sr, index, e) ->
      print_endline "BEXE_init";

      let lhs = Hashtbl.find state.value_bindings index in

      begin match e with
      | Flx_bexpr.BEXPR_tuple es, _ ->
          (* If the rhs is a tuple, load it directly. *)
          ignore (load_struct state bsym_table builder sr lhs es)
      | _ ->
          (* Otherwise, just do normal codegen. *)
          let rhs = codegen_deref state bsym_table builder sr e in

          Llvm.dump_module state.the_module;

          (* Check to make sure we're dealing with the right types. *)
          check_type
            sr
            (Llvm.type_of lhs)
            (Llvm.pointer_type (Llvm.type_of rhs));

          ignore (Llvm.build_store rhs lhs builder)
      end

  | Flx_bexe.BEXE_begin ->
      print_endline "BEXE_begin";
      assert false

  | Flx_bexe.BEXE_end ->
      print_endline "BEXE_end";
      assert false

  | Flx_bexe.BEXE_assert (sr, e) ->
      print_endline "BEXE_assert";
      let e = codegen_expr state bsym_table builder sr e in
      assert false

  | Flx_bexe.BEXE_assert2 (sr1, sr2, e1, e2) ->
      print_endline "BEXE_assert2";
      begin
        match e1 with
        | Some e1 ->
            let e1 = codegen_expr state bsym_table builder sr1 e1 in
            ()
        | None -> ()
      end;
      let e2 = codegen_expr state bsym_table builder sr2 e2 in
      assert false

  | Flx_bexe.BEXE_axiom_check (sr, e) ->
      print_endline "BEXE_axiom_check";
      let e = codegen_expr state bsym_table builder sr e in
      assert false


(* Convenience function to find all the values in a function. *)
let find_value_indicies state bsym_table bid =
  Flx_types.BidSet.filter begin fun bid ->
    try match Flx_bsym_table.find_bbdcl bsym_table bid with
    | Flx_bbdcl.BBDCL_var _
    | Flx_bbdcl.BBDCL_ref _
    | Flx_bbdcl.BBDCL_val _ -> true
    | _ -> false
    with Not_found -> false
  end (Flx_bsym_table.find_children bsym_table bid)


(* Convenience function to create the closure type of a function. *)
let find_closure_type state bsym_table bid =
  let ts =
    Flx_types.BidSet.fold begin fun bid ts ->
      try match Flx_bsym_table.find_bbdcl bsym_table bid with
      | Flx_bbdcl.BBDCL_var (_,btype)
      | Flx_bbdcl.BBDCL_ref (_,btype)
      | Flx_bbdcl.BBDCL_val (_,btype) -> (lltype_of_btype state btype) :: ts
      | _ -> ts
      with Not_found -> ts
    end (find_value_indicies state bsym_table bid) []
  in
  Llvm.struct_type state.context (Array.of_list ts)


let codegen_proto state bsym_table bid name parameters ret_type =
  (* Register the function's closure type. *)
  let closure_type = find_closure_type state bsym_table bid in
  Hashtbl.add
    state.closure_type_bindings
    bid
    (Llvm.pointer_type closure_type);

  (* Now let's build up the parameter types. For a function like:
   *
   * fun f () = {
   *  val x = 1;
   *  fun g () = {
   *    val y = 2;
   *    fun h (a:int, b:int, c:int) = {
   *      return x + y + a + b + c;
   *    }
   *  }
   * }
   *
   * We want to create an llvm function for `h` to look like this:
   *
   * %0 = type { i32 }
   * %1 = type { i32, i32 }
   *
   * define i32 @f.g.h(%0 %f-closure, %1 %f.g-closure, i32 %a, i32 %b, i32 %c)
   *
   * In order to do this, we'll build up the argument list in reverse, first by
   * creating the parameters for the regular parameters. *)
  let ts = List.map
    (fun p -> lltype_of_btype state p.Flx_bparameter.ptyp)
    parameters
  in

  (* Next we'll handle the parent closure types. First, find the parent
   * closures. *)
  let display = Flx_display.get_display_list bsym_table bid in

  (* Then grab the closure types and add them to our paramter type list. *)
  let ts = List.fold_left
    (fun ts (bid, _) -> Hashtbl.find state.closure_type_bindings bid :: ts)
    ts display
  in

  (* We've got all the types we need, so make the function. *)
  let the_function_type = Llvm.function_type
    (lltype_of_btype state ret_type)
    (Array.of_list ts)
  in

  let the_function =
    match Llvm.lookup_function name state.the_module with
    | None -> Llvm.declare_function name the_function_type state.the_module
    | Some f -> assert false
  in

  (* Let's now set the names for the parameters. We'll do this in the reverse
   * order as above in order to have proper array indexing. So, let's set the
   * closure names in order. *)
  let i =
    List.fold_right begin fun (bid, _) i ->
      let f = Hashtbl.find state.value_bindings bid in
      Llvm.set_value_name
        (Llvm.value_name f ^ "-closure")
        (Llvm.param the_function i);
      i + 1
    end display 0
  in

  (* Then set the regular parameter names in order. *)
  let _ =
    List.fold_left begin fun i p ->
      Llvm.set_value_name
        p.Flx_bparameter.pid
        (Llvm.param the_function i);
      i + 1
    end i parameters
  in

  (* Register the function value. *)
  Hashtbl.add state.value_bindings bid the_function;

  (* And add the callback function. *)
  Hashtbl.add state.call_bindings bid
    begin fun state bsym_table builder sr args ->
      codegen_call_direct state bsym_table builder sr the_function args
    end;

  Llvm.dump_module state.the_module;

  the_function, display


let codegen_closure state closure closure_kind =
  (* Exit early if we don't have any values to generate. *)
  if closure = [] then () else

  match closure_kind with
  | Global ->
      List.iter begin fun (bid, name, btype) ->
        (* Create the global. *)
        let e = Llvm.define_global
          name
          (Llvm.undef (lltype_of_btype state btype))
          state.the_module
        in

        (* Don't export the global variable. *)
        Llvm.set_linkage Llvm.Linkage.Internal e;

        Hashtbl.add state.value_bindings bid e;
      end closure

  | Stack (the_function, builder) ->
      (* ... Add the allocas. *)
      List.iter begin fun (bid, name, btype) ->
        Hashtbl.add
          state.value_bindings
          bid
          (Llvm.build_alloca (lltype_of_btype state btype) name builder)
      end closure

  | Stack_closure (bid, the_function, builder) ->
      let closure = Array.of_list closure in

      (* First build up the types for the struct. *)
      let ts = Array.map
        (fun (_,_,btype) -> lltype_of_btype state btype)
        closure
      in

      (* Then build the closure struct. *)
      let the_struct = Llvm.build_alloca
        (Llvm.struct_type state.context ts)
        ((Llvm.value_name the_function) ^ "-closure")
        builder
      in

      (* Add a closure loop. *)
      Hashtbl.add state.closure_bindings bid the_struct;

      (* Step through all the closed values and get the addresses for them and
       * register them to the value bindings. *)
      Array.iteri begin fun i (bid, name, btype) ->
        let e = codegen_gep state the_struct [| 0; i |] name builder in

        (* Register the gep as the value. *)
        Hashtbl.add state.value_bindings bid e
      end closure


let rec codegen_function
  state
  bsym_table
  sr
  bid
  name
  props
  parameters
  ret_type
  es
=
  (* Declare the function *)
  let the_function, display = codegen_proto
    state
    bsym_table
    bid
    name
    parameters
    ret_type
  in

  (* Create the initial basic block *)
  let bb = Llvm.append_block state.context "entry" the_function in
  let builder = Llvm.builder_at_end state.context bb in

  try
    (* Codegen the sub-expressions inside our function *)
    let state =
      { state with
        type_bindings = Hashtbl.copy state.type_bindings;
        call_bindings = Hashtbl.copy state.call_bindings;
        value_bindings = Hashtbl.copy state.value_bindings;
        label_bindings = Hashtbl.copy state.label_bindings;
        closure_type_bindings = Hashtbl.copy state.closure_type_bindings;
        closure_bindings = Hashtbl.copy state.closure_bindings;
      }
    in

    (* Build our stack frame. *)
    let closure = ref [] in

    (* Generate the symbols for the children. *)
    Flx_types.BidSet.iter begin fun i ->
      ignore (codegen_symbol
        state
        bsym_table
        closure
        i
        (Flx_bsym_table.find bsym_table i))
    end (Flx_bsym_table.find_children bsym_table bid);

    (* Create local bindings for the closures. *)
    let i =
      List.fold_right begin fun (bid,_) i ->
        (* Grab the closure argument. *)
        let closure = Llvm.param the_function i in

        (* Add the closure to the closure lookup binding. *)
        Hashtbl.add state.closure_bindings bid closure;

        (* Now step through all the items in the closure and bind them locally. *)
        let children = find_value_indicies state bsym_table bid in

        Flx_list.iteri begin fun i bid ->
          let gep = codegen_gep
            state
            closure
            [| 0; i |]
            (name_of_index state bsym_table bid [])
            builder
          in

          Hashtbl.add state.value_bindings bid gep
        end (List.rev (Flx_types.BidSet.elements children));

        i + 1
      end display 0
    in

    (* Next, make the closure to store our local values in. *)
    let closure_kind =
      if List.mem `Cfun props ||
         List.mem `Pure props &&
         not (List.mem `Heap_closure props)
      then Stack (the_function, builder)
      else Stack_closure (bid, the_function, builder)
    in
    codegen_closure state !closure closure_kind;

    (* Finally, create allocas for each of the regular arguments. *)
    let _ =
      List.fold_left begin fun i p ->
        let rhs = Llvm.param the_function i in

        (* Find the local symbol that corresponds with this parameter. *)
        let lhs = Hashtbl.find state.value_bindings p.Flx_bparameter.pindex in

        (* Make sure that we're dealing with the right types. *)
        check_type sr (Llvm.type_of lhs) (Llvm.pointer_type (Llvm.type_of rhs));

        (* Store the argument in the alloca. *)
        ignore (Llvm.build_store rhs lhs builder);

        Hashtbl.add state.value_bindings p.Flx_bparameter.pindex lhs;

        i + 1
      end i parameters
    in

    (* Generate code for the sub-statements. *)
    List.iter (codegen_bexe state bsym_table builder) es;

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Optimize the function. *)
    ignore (Llvm.PassManager.run_function the_function state.the_fpm);

    (* Return the function *)
    the_function
  with e ->
    Llvm.delete_function the_function;
    raise e


(* Create an llvm function from a felix function *)
and codegen_fun state index props vs ps ret_type code reqs prec =
  (* Convenience function for converting list to unary args. *)
  let call_unary f =
    fun state bsym_table builder sr args ->
      match args with
      | [ e ] -> f state bsym_table builder sr e
      | _ ->
          failwith ("1 argument required, provided " ^
            string_of_int (List.length args))
  in

  (* Convenience function for converting list to binary args. *)
  let call_binary f =
    fun state bsym_table builder sr args ->
      match args with
      | [ lhs; rhs ] -> f state bsym_table builder sr lhs rhs
      | _ ->
          failwith ("2 arguments required, provided " ^
            string_of_int (List.length args))
  in

  let f =
    match code with
    | Flx_ast.CS_str_template s ->
        (* We found an external function. Lets check if it's a native
         * instruction. Those start with a '%'. Otherwise, it must be the name
         * of an external function. *)
        begin match s with
        | "%add" -> call_binary codegen_add
        | "%sub" -> call_binary codegen_sub
        | "%subscript" -> call_binary codegen_subscript
        | "%eq" -> call_binary codegen_eq
        | "%ne" -> call_binary codegen_ne
        | "%lnot" -> call_unary codegen_lnot
        | "" -> failwith ("External function has no name");
        | name ->
            (* Handle some error cases *)
            if name.[0] == '%' then
              failwith ("Unknown instruction " ^ s);

            (* Assume then that we're declaring an external function. So, let's
             * first register the function with llvm. *)
            let ft = Llvm.function_type
              (lltype_of_btype state ret_type)
              (Array.map (lltype_of_btype state) (Array.of_list ps))
            in

            let the_function =
              match Llvm.lookup_function s state.the_module with
              | None -> Llvm.declare_function name ft state.the_module
              | Some f -> assert false
            in

            (* Use the C calling convention. *)
            Llvm.set_function_call_conv Llvm.CallConv.c the_function;

            (* ... and then return the call instruction. *)
            begin fun state bsym_table builder sr args ->
              codegen_call_direct state bsym_table builder sr the_function args
            end
        end

    | Flx_ast.CS_str s ->
        print_endline ("CS_str: " ^ s);
        assert false

    | Flx_ast.CS_virtual ->
        print_endline "CS_virtual";
        assert false

    | Flx_ast.CS_identity ->
        print_endline "CS_identity";
        assert false
  in
  Hashtbl.add state.call_bindings index f


(* Convert an external felix type into an llvm type. *)
and codegen_abs state index vs quals code reqs =
  let t =
    match code with
    | Flx_ast.CS_str_template s ->
        (* We found an external type. Lets check if it's a native llvm type.
         * These start with a '%'. *)
        let t =
          match s with
          | "%i1" -> Llvm.i1_type state.context
          | "%i8" -> Llvm.i8_type state.context
          | "%i16" -> Llvm.i16_type state.context
          | "%i32" -> Llvm.i32_type state.context
          | "%i64" -> Llvm.i64_type state.context
          | "%float" -> Llvm.float_type state.context
          | "%double" -> Llvm.double_type state.context
          | "%void" -> Llvm.void_type state.context
          | s -> failwith ("Unknown type " ^ s)
        in
        t

    | Flx_ast.CS_str s ->
        print_endline ("CS_str: " ^ s);
        assert false

    | Flx_ast.CS_virtual ->
        print_endline "CS_virtual";
        assert false

    | Flx_ast.CS_identity ->
        print_endline "CS_identity";
        assert false
  in
  Hashtbl.add state.type_bindings index t


and codegen_symbol state bsym_table closure index bsym =
  let bsym_parent = Flx_bsym_table.find_parent bsym_table index in
  print_endline ("codegen_symbol: " ^
    "parent=" ^ (match bsym_parent with
      | Some p -> Flx_print.string_of_bid p
      | None -> "None") ^
    " " ^
    (Flx_print.string_of_bbdcl bsym_table (Flx_bsym.bbdcl bsym) index));

  match Flx_bsym.bbdcl bsym with
  | Flx_bbdcl.BBDCL_invalid ->
      print_endline "BBDCL_invalid";
      assert false

  | Flx_bbdcl.BBDCL_module ->
      print_endline "BBDCL_module";
      assert false

  | Flx_bbdcl.BBDCL_function (props, _, (ps, _), ret_type, es) ->
      ignore (codegen_function
        state
        bsym_table
        (Flx_bsym.sr bsym)
        index
        (name_of_index state bsym_table index [])
        props
        ps
        ret_type
        es)

  | Flx_bbdcl.BBDCL_procedure (props, _, (ps, _), es) ->
      ignore (codegen_function
        state
        bsym_table
        (Flx_bsym.sr bsym)
        index
        (name_of_index state bsym_table index [])
        props
        ps
        (Flx_btype.btyp_void ())
        es)

  | Flx_bbdcl.BBDCL_val (_, btype)
  | Flx_bbdcl.BBDCL_var (_, btype)
  | Flx_bbdcl.BBDCL_ref (_, btype)
  | Flx_bbdcl.BBDCL_tmp (_, btype) ->
      let name = name_of_index state bsym_table index [] in
      closure := (index, name, btype) :: !closure

  | Flx_bbdcl.BBDCL_newtype (vs, ty) ->
      print_endline "BBDCL_newtype";
      assert false

  | Flx_bbdcl.BBDCL_abs (vs, quals, code, reqs) ->
      codegen_abs state index vs quals code reqs

  | Flx_bbdcl.BBDCL_const (props, vs, ty, code, reqs) ->
      print_endline "BBDCL_const";
      assert false

  | Flx_bbdcl.BBDCL_fun (props, vs, ps, ret_type, code, reqs, prec) ->
      codegen_fun state index props vs ps ret_type code reqs prec

  | Flx_bbdcl.BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec) ->
      print_endline "BBDCL_callback";
      assert false

  | Flx_bbdcl.BBDCL_proc (props, vs, ps, code, reqs) ->
      codegen_fun state index props vs ps (Flx_btype.btyp_void ()) code reqs ""

  | Flx_bbdcl.BBDCL_insert (vs, s, ikind, reqs) ->
      print_endline "BBDCL_insert";
      (* FIXME: ignore for now.
      assert false
      *)

  | Flx_bbdcl.BBDCL_union (vs, cs) ->
      print_endline "BBDCL_union";
      assert false

  | Flx_bbdcl.BBDCL_struct (vs, cs) ->
      print_endline "BBDCL_struct";
      assert false

  | Flx_bbdcl.BBDCL_cstruct (vs, cs) ->
      print_endline "BBDCL_cstruct";
      assert false

  | Flx_bbdcl.BBDCL_typeclass (props, vs) ->
      print_endline "BBDCL_typeclass";
      assert false

  | Flx_bbdcl.BBDCL_instance (props, vs, cons, index, ts) ->
      print_endline "BBDCL_instance";
      assert false

  | Flx_bbdcl.BBDCL_nonconst_ctor
    (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) ->
      print_endline "BBDCL_nonconst_ctor";
      assert false

  | Flx_bbdcl.BBDCL_axiom ->
      print_endline "BBDCL_axiom";
      assert false

  | Flx_bbdcl.BBDCL_lemma ->
      print_endline "BBDCL_lemma";
      assert false

  | Flx_bbdcl.BBDCL_reduce ->
      print_endline "BBDCL_reduce";
      assert false



let codegen state bsym_table bids bexes =
  (* First we'll generate the symbols. *)
  let global_closure = ref [] in

  List.iter begin fun bid ->
    (* Try to find the bsym corresponding with the bid. *)
    let bsym = Flx_bsym_table.find bsym_table bid in
    let bsym_parent = Flx_bsym_table.find_parent bsym_table bid in
    (* Only codegen top-level symbols, since that'll be handled by the code
     * generator. *)
    match bsym_parent with
    | Some parent -> ()
    | None -> codegen_symbol state bsym_table global_closure bid bsym
  end bids;

  (* Define all the global values. *)
  codegen_closure state !global_closure Global;

  (* If we don't have any executables, don't make a function. *)
  if bexes = [] then None else

  (* Make a new function to execute the statement in. We use an opaque
   * type so that we can later refine it to the actual value of the
   * returned expression. *)
  let the_function = Llvm.declare_function
    ""
    (Llvm.function_type (Llvm.void_type state.context) [||])
    state.the_module
  in

  (* Create the initial basic block *)
  let bb = Llvm.append_block state.context "entry" the_function in
  let builder = Llvm.builder_at_end state.context bb in

  (* Finally, generate code for the executions. *)
  List.iter begin fun bexe ->
    ignore (codegen_bexe state bsym_table builder bexe)
  end bexes;

  (* Make sure we have a return at the end of the function. *)
  ignore (Llvm.build_ret_void builder);

  (* Make sure the function is valid. *)
  Llvm_analysis.assert_valid_function the_function;

  (* Optimize the function. *)
  ignore (Llvm.PassManager.run_function the_function state.the_fpm);

  Llvm.dump_module state.the_module;

  Some the_function

let codegen_and_run state bsym_table bids bexes =
  let the_function = codegen state bsym_table bids bexes in

  (* Run the function. *)
  begin match the_function with
  | None -> ()
  | Some the_function ->
      (* Execute the function. *)
      ignore (Llvm_executionengine.ExecutionEngine.run_function
        the_function
        [||]
        state.the_ee)
  end;

  the_function
