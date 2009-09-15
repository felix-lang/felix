type call_t = Llvm.llvalue array -> string -> Llvm.llbuilder -> Llvm.llvalue

type codegen_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
  context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  the_fpm: [`Function] Llvm.PassManager.t;
  the_ee: Llvm_executionengine.ExecutionEngine.t;
  type_bindings: (Flx_ast.bid_t, Llvm.lltype) Hashtbl.t;
  call_bindings: (Flx_ast.bid_t, call_t) Hashtbl.t;
  value_bindings: (Flx_ast.bid_t, Llvm.llvalue) Hashtbl.t;
  label_bindings: (string, Llvm.llbasicblock) Hashtbl.t;
}


let make_codegen_state syms bbdfns context the_module the_fpm the_ee =
  {
    syms = syms;
    bbdfns = bbdfns;
    context = context;
    the_module = the_module;
    the_fpm = the_fpm;
    the_ee = the_ee;
    type_bindings = Hashtbl.create 97;
    call_bindings = Hashtbl.create 97;
    value_bindings = Hashtbl.create 97;
    label_bindings = Hashtbl.create 97;
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
let name_of_index state index =
  try
    match Hashtbl.find state.syms.Flx_mtypes2.dfns index with
    | { Flx_types.id=id } -> id
  with Not_found ->
    try
      match Hashtbl.find state.bbdfns index with id, _, _, _ -> id
    with Not_found ->
      "index_" ^ string_of_int index


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


(* Convenience function to check we're dealing with the right types. *)
let check_type value typekind =
  if Llvm.classify_type (Llvm.type_of value) != typekind then
    failwith ("invalid type, expected " ^ name_of_typekind typekind)


(* Convert a felix type to an llvm type. *)
let rec lltype_of_btype state btypecode =
  print_endline
    (Flx_print.string_of_btypecode state.syms.Flx_mtypes2.dfns btypecode);

  match Flx_maps.reduce_type btypecode with
  | Flx_types.BTYP_inst (index, ts) -> Hashtbl.find state.type_bindings index
  | Flx_types.BTYP_tuple ls ->
      let ls = List.map (lltype_of_btype state) ls in
      Llvm.struct_type state.context (Array.of_list ls)
  | Flx_types.BTYP_record ls -> assert false
  | Flx_types.BTYP_variant ls -> assert false
  | Flx_types.BTYP_unitsum k -> assert false
  | Flx_types.BTYP_sum ls -> assert false
  | Flx_types.BTYP_function (args, result) -> assert false
  | Flx_types.BTYP_cfunction (args, result) -> assert false
  | Flx_types.BTYP_pointer t -> assert false
  | Flx_types.BTYP_array (t1, Flx_types.BTYP_unitsum k) ->
      let t1 = lltype_of_btype state t1 in
      Llvm.array_type t1 k
  | Flx_types.BTYP_array (t1, t2) -> assert false
  | Flx_types.BTYP_void -> Llvm.void_type state.context
  | Flx_types.BTYP_fix i -> assert false
  | Flx_types.BTYP_intersect ls -> assert false
  | Flx_types.BTYP_var (i, mt) -> assert false
  | Flx_types.BTYP_apply (t1, t2) -> assert false
  | Flx_types.BTYP_typefun (args, result, body) -> assert false
  | Flx_types.BTYP_type i -> assert false
  | Flx_types.BTYP_type_tuple ls -> assert false
  | Flx_types.BTYP_type_match (t, ps) -> assert false
  | Flx_types.BTYP_typeset ls -> assert false
  | Flx_types.BTYP_typesetunion ls -> assert false
  | Flx_types.BTYP_typesetintersection ls -> assert false


(* Convenience function to find the parent of the builder. *)
let builder_parent builder =
  (* First we need to get the current basic block. *)
  let bb = Llvm.insertion_block builder in

  (* Then, return the basic block's parent. *)
  Llvm.block_parent bb


(* Generate code for a literal *)
let codegen_literal state sr literal =
  match literal with
  | Flx_ast.AST_float (suffix, f) ->
      Llvm.const_float_of_string (lltype_of_suffix state suffix) f
  | Flx_ast.AST_int (suffix, i) ->
      Llvm.const_int_of_string
        (lltype_of_suffix state suffix)
        (Big_int.string_of_big_int i)
        10
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
let rec codegen_expr state builder sr tbexpr =
  print_endline ("codegen_expr: " ^ Flx_print.string_of_bound_expression
    state.syms.Flx_mtypes2.dfns state.bbdfns tbexpr);

  (* See if there are any simple reductions we can apply to the expression. *)
  let bexpr, btypecode = Flx_maps.reduce_tbexpr state.bbdfns tbexpr in

  match bexpr with
  | Flx_types.BEXPR_deref e ->
      print_endline "BEXPR_deref";

      (* Make sure we've got a pointer. *)
      let e = codegen_expr state builder sr e in
      check_type e Llvm.TypeKind.Pointer;

      Llvm.build_load e "" builder

  | Flx_types.BEXPR_name (index, _) ->
      print_endline "BEXPR_name";
      let e = Hashtbl.find state.value_bindings index in
      let t = lltype_of_btype state btypecode in

      (* Arrays and structures are passed around by reference *)
      begin match Llvm.classify_type t with
      | Llvm.TypeKind.Array | Llvm.TypeKind.Struct -> e
      | _ -> Llvm.build_load e (name_of_index state index) builder
      end

  | Flx_types.BEXPR_ref (index, btypecode) ->
      print_endline "BEXPR_ref";
      assert false

  | Flx_types.BEXPR_likely e ->
      print_endline "BEXPR_likely";
      (* Do nothing for now *)
      codegen_expr state builder sr e

  | Flx_types.BEXPR_unlikely e ->
      print_endline "BEXPR_unlikely";
      (* Do nothing for now *)
      codegen_expr state builder sr e

  | Flx_types.BEXPR_address e ->
      print_endline "BEXPR_address";

      let e = codegen_expr state builder sr e in

      (* Make sure we've got a pointer. *)
      check_type e Llvm.TypeKind.Pointer;

      (* Expressions can only have their address taken if they're on the stack.
       * So, we shouldn't need to do any work. *)
      e

  | Flx_types.BEXPR_new e ->
      print_endline "BEXPR_new";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_not e ->
      print_endline "BEXPR_not";
      let e = codegen_expr state builder sr e in
      let ty = Llvm.type_of e in
      let e =
        Llvm.build_icmp Llvm.Icmp.Eq e (Llvm.const_int ty 0) "not" builder
      in
      Llvm.build_zext e ty "not" builder

  | Flx_types.BEXPR_literal literal ->
      print_endline "BEXPR_literal";
      codegen_literal state sr literal

  | Flx_types.BEXPR_apply (f, e) ->
      print_endline "BEXPR_apply";
      assert false

  | Flx_types.BEXPR_apply_direct (index, _, e) ->
      print_endline "BEXPR_apply_{prim,direct,stack_struct}";

      let f = Hashtbl.find state.call_bindings index in
      let args =
        match e with
        | Flx_types.BEXPR_tuple es, _ ->
            Array.map
              (codegen_expr state builder sr) (Array.of_list es)
        | _ ->
            [| codegen_expr state builder sr e |]
      in

      f args "" builder

  | Flx_types.BEXPR_apply_prim (index, _, e)
  | Flx_types.BEXPR_apply_stack (index, _, e)
  | Flx_types.BEXPR_apply_struct (index, _, e) ->
      print_endline "BEXPR_apply_{prim,direct,stack_struct}";

      let f = Hashtbl.find state.call_bindings index in
      let e = codegen_expr state builder sr e in

      f [|e|] "apply" builder

  | Flx_types.BEXPR_tuple es ->
      print_endline "BEXPR_tuple";
      codegen_struct state builder sr es btypecode

  | Flx_types.BEXPR_record es ->
      print_endline "BEXPR_record";
      codegen_struct state builder sr (List.map snd es) btypecode

  | Flx_types.BEXPR_variant (string, e) ->
      print_endline "BEXPR_variant";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_get_n (n, e) ->
      print_endline "BEXPR_get_n";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_closure (index, btypecode) ->
      print_endline ("BEXPR_closure: " ^ name_of_index state index);
      Hashtbl.find state.value_bindings index

  | Flx_types.BEXPR_case (int, btypecode) ->
      print_endline "BEXPR_case";
      assert false

  | Flx_types.BEXPR_match_case (int, e) ->
      print_endline "BEXPR_match_case";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_case_arg (int, e) ->
      print_endline "BEXPR_case_arg";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_case_index e ->
      print_endline "BEXPR_case_index";
      let _ = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXPR_expr (string, btypecode) ->
      print_endline "BEXPR_expr";
      assert false

  | Flx_types.BEXPR_range_check (e1, e2, e3) ->
      print_endline "BEXPR_range_check";
      let _ = codegen_expr state builder sr e1 in
      let _ = codegen_expr state builder sr e2 in
      let _ = codegen_expr state builder sr e3 in
      assert false

  | Flx_types.BEXPR_coerce (tbexpr_t, btypecode) ->
      print_endline "BEXPR_coerce";
      assert false


(* Generate code for an llvm struct type. *)
and codegen_struct state builder sr es btype =
  let the_struct = create_entry_block_alloca state builder btype "" in
  load_struct state builder sr the_struct es


and load_struct state builder sr the_struct es =
  (* Add the values to the struct. *)
  let zero = Llvm.const_int (Llvm.i32_type state.context) 0 in
  let _ =
    List.fold_left begin fun i e ->
      let gep = Llvm.build_gep
        the_struct
        [| zero; Llvm.const_int (Llvm.i32_type state.context) i |]
        "foo"
        builder
      in

      let e = codegen_expr state builder sr e in
      ignore (Llvm.build_store e gep builder);

      i + 1
    end 0 es
  in
  the_struct


let codegen_bexe state builder bexe =
  print_endline ("codegen_bexe: " ^ Flx_print.string_of_bexe
    state.syms.Flx_mtypes2.dfns state.bbdfns 0 bexe);

  (* See if there are any simple reductions we can apply to the exe. *)
  let bexe = Flx_maps.reduce_bexe state.bbdfns bexe in

  match bexe with
  | Flx_types.BEXE_label (sr, label) ->
      print_endline "BEXE_label";

      (* Find or create the basic block of the label *)
      let bb =
        try Hashtbl.find state.label_bindings label with Not_found ->
          (* The label doesn't exist yet, so let's make it. *)
          let the_function = builder_parent builder in
          let bb = Llvm.append_block state.context label the_function in
          Hashtbl.add state.label_bindings label bb;
          bb
      in

      (* Set the builder to start generating code on that basic block. *)
      Llvm.position_at_end bb builder

  | Flx_types.BEXE_comment (sr, string) ->
      (* Ignore the comment. *)
      ()

  | Flx_types.BEXE_halt (sr, string) ->
      print_endline "BEXE_halt";
      assert false

  | Flx_types.BEXE_trace (sr, s1, s2) ->
      print_endline "BEXE_trace";
      assert false

  | Flx_types.BEXE_goto (sr, label) ->
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

  | Flx_types.BEXE_ifgoto (sr, e, label) ->
      print_endline "BEXE_ifgoto";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_call (sr, p, a) ->
      let e1 = codegen_expr state builder sr p in
      let e2 = codegen_expr state builder sr a in
      assert false

  | Flx_types.BEXE_call_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_call_direct";
      let f = Hashtbl.find state.call_bindings index in
      let args =
        match e with
        | Flx_types.BEXPR_tuple es, _ ->
            Array.map
              (codegen_expr state builder sr)
              (Array.of_list es)
        | _ ->
            [| codegen_expr state builder sr e |]
      in
      ignore (f args "" builder)

  | Flx_types.BEXE_call_stack (sr, index, btypecode, e) ->
      print_endline "BEXE_call_stack";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_call_prim (sr, index, btypecode, e) ->
      print_endline "BEXE_call_prim";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_jump (sr, e1, e2) ->
      print_endline "BEXE_jump";
      assert false

  | Flx_types.BEXE_jump_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_jump_direct";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_loop (sr, int, e) ->
      print_endline "BEXE_loop";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_svc (sr, index) ->
      print_endline "BEXE_svc";
      assert false

  | Flx_types.BEXE_fun_return (sr, e) ->
      print_endline "BEXE_fun_return";
      let e = codegen_expr state builder sr e in
      ignore (Llvm.build_ret e builder);

  | Flx_types.BEXE_yield (sr, e) ->
      print_endline "BEXE_yield";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_proc_return sr ->
      print_endline "BEXE_proc_return";
      ignore (Llvm.build_ret_void builder);

  | Flx_types.BEXE_nop (sr, string) ->
      print_endline "BEXE_nop";
      assert false

  | Flx_types.BEXE_code (sr, code_spec_t) ->
      print_endline "BEXE_code";
      assert false

  | Flx_types.BEXE_nonreturn_code (sr, code_spec_t) ->
      print_endline "BEXE_nonreturn_code";
      assert false

  | Flx_types.BEXE_assign (sr, lhs, rhs) ->
      print_endline "BEXE_assign";

      (* We can only assign to a name *)
      let lhs =
        match lhs with
        | Flx_types.BEXPR_name (index, _), _ ->
            Hashtbl.find state.value_bindings index
        | _ ->
            failwith ("invalid lvalue")
      in
      let rhs = codegen_expr state builder sr rhs in

      check_type lhs Llvm.TypeKind.Pointer;
      check_type
        rhs
        (Llvm.classify_type (Llvm.element_type (Llvm.type_of lhs)));

      ignore (Llvm.build_store rhs lhs builder)

  | Flx_types.BEXE_init (sr, index, ((_, btype) as e)) ->
      print_endline "BEXE_init";

      let lhs =
        (* If the variable doesn't exist, let's define it now. *)
        try Hashtbl.find state.value_bindings index with Not_found ->
        let e = create_entry_block_alloca
          state
          builder
          btype
          (name_of_index state index)
        in
        Hashtbl.add state.value_bindings index e;
        e
      in

      (* Make sure the lhs is a pointer. *)
      check_type lhs Llvm.TypeKind.Pointer;

      begin match e with
      | Flx_types.BEXPR_tuple es, _ ->
          (* If the rhs is a tuple, load it directly. *)
          ignore (load_struct state builder sr lhs es)
      | _ ->
          (* Otherwise, just do normal codegen. *)
          let rhs = codegen_expr state builder sr e in

          (* Make sure the rhs is of the right type. *)
          check_type rhs
            (Llvm.classify_type (Llvm.element_type (Llvm.type_of lhs)));

          ignore (Llvm.build_store rhs lhs builder)
      end

  | Flx_types.BEXE_begin ->
      print_endline "BEXE_begin";
      assert false

  | Flx_types.BEXE_end ->
      print_endline "BEXE_end";
      assert false

  | Flx_types.BEXE_assert (sr, e) ->
      print_endline "BEXE_assert";
      let e = codegen_expr state builder sr e in
      assert false

  | Flx_types.BEXE_assert2 (sr1, sr2, e1, e2) ->
      print_endline "BEXE_assert2";
      begin
        match e1 with
        | Some e1 ->
            let e1 = codegen_expr state builder sr1 e1 in
            ()
        | None -> ()
      end;
      let e2 = codegen_expr state builder sr2 e2 in
      assert false

  | Flx_types.BEXE_axiom_check (sr, e) ->
      print_endline "BEXE_axiom_check";
      let e = codegen_expr state builder sr e in
      assert false


let codegen_proto state index name parameters ret_type =
  let parameters = Array.of_list parameters in

  (* Make the function type *)
  let ft = Llvm.function_type
    (lltype_of_btype state ret_type)
    (Array.map (fun p -> lltype_of_btype state p.Flx_types.ptyp) parameters)
  in

  (* Make the function *)
  let f = Llvm.declare_function name ft state.the_module in

  (* Set the names for all the arguments *)
  Array.iteri begin fun i a ->
    let bparam = parameters.(i) in
    Llvm.set_value_name bparam.Flx_types.pid a;
    Hashtbl.add state.value_bindings bparam.Flx_types.pindex a;
  end (Llvm.params f);

  (* Register the function. *)
  Hashtbl.add state.call_bindings index (Llvm.build_call f);

  f


let codegen_function state index name parameters ret_type es =
  (* Declare the function *)
  let the_function = codegen_proto state index name parameters ret_type in

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
      }
    in

    (* Convert the parameters into an array so we can index into it. *)
    let parameters = Array.of_list parameters in

    (* Create allocas for each of the arguments. *)
    Array.iteri begin fun i rhs ->
      let lhs = create_entry_block_alloca
        state
        builder
        parameters.(i).Flx_types.ptyp
        parameters.(i).Flx_types.pid
      in
      ignore (Llvm.build_store rhs lhs builder);
      Hashtbl.add state.value_bindings index lhs;
    end (Llvm.params the_function);

    (* Generate code for the sub-statements. *)
    List.iter (codegen_bexe state builder) es;

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
let codegen_fun state index props vs ps ret_type code reqs prec =
  let f =
    match code with
    | Flx_ast.CS_str_template s ->
        (* We found an external function. Lets check if it's a native
         * instruction. Those start with a '%'. Otherwise, it must be the name
         * of an external function. *)
        begin match s with
        | "%add" ->
            begin function
            | [| lhs; rhs |] ->
                check_type lhs Llvm.TypeKind.Integer;
                check_type rhs Llvm.TypeKind.Integer;
                Llvm.build_add lhs rhs
            | _ ->
                failwith ("Invalid arguments for " ^ name_of_index state index)
            end

        | "%sub" ->
            begin function
            | [| lhs; rhs |] ->
                check_type lhs Llvm.TypeKind.Integer;
                check_type rhs Llvm.TypeKind.Integer;
                Llvm.build_sub lhs rhs
            | _ ->
                failwith ("Invalid arguments for " ^ name_of_index state index)
            end

        | "%subscript" ->
            begin fun args name builder ->
              match args with
              | [| lhs; rhs |] ->
                  check_type lhs Llvm.TypeKind.Pointer;
                  check_type rhs Llvm.TypeKind.Integer;

                  let zero = Llvm.const_int (Llvm.i32_type state.context) 0 in
                  let gep = Llvm.build_gep lhs [| zero; rhs |] "" builder in
                  Llvm.build_load gep name builder
              | _ ->
                  failwith ("Invalid arguments for " ^
                    name_of_index state index)
            end

        | "%eq" ->
            begin function
            | [| lhs; rhs |] ->
                check_type lhs Llvm.TypeKind.Integer;
                check_type rhs Llvm.TypeKind.Integer;
                Llvm.build_icmp Llvm.Icmp.Eq lhs rhs
            | _ ->
                failwith ("Invalid arguments for " ^ name_of_index state index)
            end

        | "%ne" ->
            begin function
            | [| lhs; rhs |] ->
                check_type lhs Llvm.TypeKind.Integer;
                check_type rhs Llvm.TypeKind.Integer;
                Llvm.build_icmp Llvm.Icmp.Ne lhs rhs
            | _ ->
                failwith ("Invalid arguments for " ^ name_of_index state index)
            end

        | "%lnot" ->
            begin fun args name builder ->
              match args with
              | [| e |] ->
                  check_type e Llvm.TypeKind.Integer;

                  let ty = Llvm.type_of e in
                  let e = Llvm.build_icmp
                    Llvm.Icmp.Eq
                    e
                    (Llvm.const_int ty 0)
                    ""
                    builder
                  in
                  Llvm.build_zext e ty name builder
              | _ ->
                  failwith ("Invalid arguments for " ^
                    name_of_index state index)
            end

        | s ->
            (* Handle some error cases *)
            if String.length s == 0 then
              failwith ("External function has no name");

            if s.[0] == '%' then
              failwith ("Unknown instruction " ^ s);

            (* Assume then that we're declaring an external function. So, let's
             * first register the function with llvm. *)
            let ft = Llvm.function_type
              (lltype_of_btype state ret_type)
              (Array.map (lltype_of_btype state) (Array.of_list ps))
            in
            let the_function = Llvm.declare_function s ft state.the_module in

            (* Use the C calling convention. *)
            Llvm.set_function_call_conv Llvm.CallConv.c the_function;

            (* ... and then return the call instruction. *)
            Llvm.build_call the_function
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
let codegen_abs state index vs quals code reqs =
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


let codegen_symbol state index ((name, parent, sr, bbdcl) as symbol) =
  print_endline ("codegen_symbol: " ^ name);

  match bbdcl with
  | Flx_types.BBDCL_function (_, _, (ps, _), ret_type, es) ->
      let f = codegen_function state index name ps ret_type es in
      Hashtbl.add state.value_bindings index f

  | Flx_types.BBDCL_procedure (_, _, (ps, _), es) ->
      let f = codegen_function state index name ps Flx_types.BTYP_void es in
      Hashtbl.add state.value_bindings index f

  | Flx_types.BBDCL_val (vs, btype)
  | Flx_types.BBDCL_var (vs, btype)
  | Flx_types.BBDCL_ref (vs, btype)
  | Flx_types.BBDCL_tmp (vs, btype) ->
      let e = Llvm.define_global
        (name_of_index state index)
        (Llvm.undef (lltype_of_btype state btype))
        (state.the_module)
      in
      (* Don't export the global variable. *)
      Llvm.set_linkage Llvm.Linkage.Internal e;
      Hashtbl.add state.value_bindings index e

  | Flx_types.BBDCL_newtype (vs, ty) ->
      print_endline "BBDCL_newtype";
      assert false

  | Flx_types.BBDCL_abs (vs, quals, code, reqs) ->
      codegen_abs state index vs quals code reqs

  | Flx_types.BBDCL_const (props, vs, ty, code, reqs) ->
      print_endline "BBDCL_const";
      assert false

  | Flx_types.BBDCL_fun (props, vs, ps, ret_type, code, reqs, prec) ->
      codegen_fun state index props vs ps ret_type code reqs prec

  | Flx_types.BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec) ->
      print_endline "BBDCL_callback";
      assert false

  | Flx_types.BBDCL_proc (props, vs, ps, code, reqs) ->
      codegen_fun state index props vs ps Flx_types.BTYP_void code reqs []

  | Flx_types.BBDCL_insert (vs, s, ikind, reqs) ->
      print_endline "BBDCL_insert";
      assert false

  | Flx_types.BBDCL_union (vs, cs) ->
      print_endline "BBDCL_union";
      assert false

  | Flx_types.BBDCL_struct (vs, cs) ->
      print_endline "BBDCL_struct";
      assert false

  | Flx_types.BBDCL_cstruct (vs, cs) ->
      print_endline "BBDCL_cstruct";
      assert false

  | Flx_types.BBDCL_typeclass (props, vs) ->
      print_endline "BBDCL_typeclass";
      assert false

  | Flx_types.BBDCL_instance (props, vs, cons, index, ts) ->
      print_endline "BBDCL_instance";
      assert false

  | Flx_types.BBDCL_nonconst_ctor
    (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) ->
      print_endline "BBDCL_nonconst_ctor";
      assert false
