type call_t =
  | Inst_add
  | Inst_sub
  | Function of Llvm.llvalue


type codegen_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
  context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  call_bindings: (Flx_ast.bid_t, call_t) Hashtbl.t;
  value_bindings: (Flx_ast.bid_t, Llvm.llvalue) Hashtbl.t;
}


let make_codegen_state syms bbdfns context the_module builder =
  {
    syms = syms;
    bbdfns = bbdfns;
    context = context;
    the_module = the_module;
    builder = builder;
    call_bindings = Hashtbl.create 97;
    value_bindings = Hashtbl.create 97;
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
let name_of_index dfns bbdfns index =
  try
    match Hashtbl.find dfns index with
    | { Flx_types.id=id } -> id
  with Not_found ->
    try
      match Hashtbl.find bbdfns index with id, _, _, _ -> id
    with Not_found ->
      "index_" ^ string_of_int index


let is_instruction call_bindings index =
  match Hashtbl.find call_bindings index with
  | Inst_add | Inst_sub -> true
  | _ -> false


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


(* Generate call for an expression *)
let rec codegen_expr state sr ((bexpr, typecode) as tbexpr): Llvm.llvalue =
  print_endline ("codegen_expr: " ^ Flx_print.string_of_bound_expression
    state.syms.Flx_mtypes2.dfns state.bbdfns tbexpr);

  let name_of_index = name_of_index state.syms.Flx_mtypes2.dfns state.bbdfns in

  match bexpr with
  | Flx_types.BEXPR_deref e ->
      print_endline "BEXPR_deref";
      codegen_expr state sr e

  | Flx_types.BEXPR_name (index, btypecode) ->
      print_endline "BEXPR_name";
      Hashtbl.find state.value_bindings index

  | Flx_types.BEXPR_ref (index, btypecode) ->
      print_endline "BEXPR_ref";
      assert false

  | Flx_types.BEXPR_likely e ->
      print_endline "BEXPR_likely";
      codegen_expr state sr e

  | Flx_types.BEXPR_unlikely e ->
      print_endline "BEXPR_unlikely";
      codegen_expr state sr e

  | Flx_types.BEXPR_address e ->
      print_endline "BEXPR_address";
      codegen_expr state sr e

  | Flx_types.BEXPR_new e ->
      print_endline "BEXPR_new";
      codegen_expr state sr e

  | Flx_types.BEXPR_not e ->
      print_endline "BEXPR_not";
      codegen_expr state sr e

  | Flx_types.BEXPR_literal literal ->
      print_endline "BEXPR_literal";
      codegen_literal state sr literal

  (* Handle the case where we're calling a binary operator. *)
  | Flx_types.BEXPR_apply (
    (Flx_types.BEXPR_closure (index, _), _),
    (Flx_types.BEXPR_tuple [lhs; rhs], _)) ->
      print_endline ("BEXPR_apply: " ^ name_of_index index);

      let name = name_of_index index in
      let lhs = codegen_expr state sr lhs in
      let rhs = codegen_expr state sr rhs in

      begin match Hashtbl.find state.call_bindings index with
      | Function f -> Llvm.build_call f [|lhs; rhs|] name state.builder
      | Inst_add -> Llvm.build_add lhs rhs name state.builder
      | Inst_sub -> Llvm.build_sub lhs rhs name state.builder
      end

  | Flx_types.BEXPR_apply (
    (Flx_types.BEXPR_closure (index, _), _),
    (Flx_types.BEXPR_tuple es, _)) ->
      print_endline ("BEXPR_apply: " ^ name_of_index index);

      let name = name_of_index index in
      let es = List.map (codegen_expr state sr) es in

      begin match Hashtbl.find state.call_bindings index with
      | Function f -> Llvm.build_call f (Array.of_list es) name state.builder
      | Inst_add
      | Inst_sub ->
          failwith ("Invalid argument count for instruction!")
      end

  | Flx_types.BEXPR_apply (e1, e2) ->
      print_endline "BEXPR_apply";

      let _ = codegen_expr state sr e1 in
      let _ = codegen_expr state sr e2 in
      assert false

  | Flx_types.BEXPR_apply_prim (index, btypecode, e) ->
      print_endline "BEXPR_apply_prim";
      codegen_expr state sr e

  | Flx_types.BEXPR_apply_direct (index, btypecode, e) ->
      print_endline "BEXPR_apply_direct";
      codegen_expr state sr e

  | Flx_types.BEXPR_apply_stack (index, btypecode, e) ->
      print_endline "BEXPR_apply_stack";
      codegen_expr state sr e

  | Flx_types.BEXPR_apply_struct (index, btypecode, e) ->
      print_endline "BEXPR_apply_struct";
      codegen_expr state sr e

  | Flx_types.BEXPR_tuple fields ->
      print_endline "BEXPR_tuple";
      let _ =
        List.map begin fun e ->
          codegen_expr state sr e;
        end fields
      in
      assert false

  | Flx_types.BEXPR_record fields ->
      print_endline "BEXPR_record";
      let _ =
        List.map begin fun (name, e) ->
          codegen_expr state sr e;
        end fields
      in
      assert false

  | Flx_types.BEXPR_variant (string, e) ->
      print_endline "BEXPR_variant";
      codegen_expr state sr e

  | Flx_types.BEXPR_get_n (n, e) ->
      print_endline "BEXPR_get_n";
      codegen_expr state sr e

  | Flx_types.BEXPR_closure (index, btypecode) ->
      print_endline ("BEXPR_closure: " ^ name_of_index index);
      Hashtbl.find state.value_bindings index

  | Flx_types.BEXPR_case (int, btypecode) ->
      print_endline "BEXPR_case";
      assert false

  | Flx_types.BEXPR_match_case (int, e) ->
      print_endline "BEXPR_match_case";
      codegen_expr state sr e

  | Flx_types.BEXPR_case_arg (int, e) ->
      print_endline "BEXPR_case_arg";
      codegen_expr state sr e

  | Flx_types.BEXPR_case_index e ->
      print_endline "BEXPR_case_index";
      codegen_expr state sr e

  | Flx_types.BEXPR_expr (string, btypecode) ->
      print_endline "BEXPR_expr";
      assert false

  | Flx_types.BEXPR_range_check (e1, e2, e3) ->
      print_endline "BEXPR_range_check";
      let _ = codegen_expr state sr e1 in
      let _ = codegen_expr state sr e2 in
      let _ = codegen_expr state sr e3 in
      assert false

  | Flx_types.BEXPR_coerce (tbexpr_t, btypecode) ->
      print_endline "BEXPR_coerce";
      assert false


let codegen_bexe state bexe =
  print_endline ("codegen_bexe: " ^ Flx_print.string_of_bexe
    state.syms.Flx_mtypes2.dfns state.bbdfns 0 bexe);

  let name_of_index = name_of_index state.syms.Flx_mtypes2.dfns state.bbdfns in

  match bexe with
  | Flx_types.BEXE_label (sr, string) ->
      print_endline "BEXE_label";
      None

  | Flx_types.BEXE_comment (sr, string) ->
      print_endline "BEXE_comment";
      None

  | Flx_types.BEXE_halt (sr, string) ->
      print_endline "BEXE_halt";
      None

  | Flx_types.BEXE_trace (sr, s1, s2) ->
      print_endline "BEXE_trace";
      None

  | Flx_types.BEXE_goto (sr, string) ->
      print_endline "BEXE_goto";
      None

  | Flx_types.BEXE_ifgoto (sr, e, string) ->
      print_endline "BEXE_ifgoto";
      let expr = codegen_expr state sr e in
      None

  | Flx_types.BEXE_call (sr, p, a) ->
      let e1 = codegen_expr state sr p in
      let e2 = codegen_expr state sr a in
      None

  | Flx_types.BEXE_call_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_call_direct";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_call_stack (sr, index, btypecode, e) ->
      print_endline "BEXE_call_stack";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_call_prim (sr, index, btypecode, e) ->
      print_endline "BEXE_call_prim";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_jump (sr, e1, e2) ->
      print_endline "BEXE_jump";
      None

  | Flx_types.BEXE_jump_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_jump_direct";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_loop (sr, int, e) ->
      print_endline "BEXE_loop";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_svc (sr, index) ->
      print_endline "BEXE_svc";
      None

  | Flx_types.BEXE_fun_return (sr, e) ->
      print_endline "BEXE_fun_return";
      Some (codegen_expr state sr e)

  | Flx_types.BEXE_yield (sr, e) ->
      print_endline "BEXE_yield";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_proc_return (sr) ->
      print_endline "BEXE_proc_return";
      None

  | Flx_types.BEXE_nop (sr, string) ->
      print_endline "BEXE_nop";
      None

  | Flx_types.BEXE_code (sr, code_spec_t) ->
      print_endline "BEXE_code";
      None

  | Flx_types.BEXE_nonreturn_code (sr, code_spec_t) ->
      print_endline "BEXE_nonreturn_code";
      None

  | Flx_types.BEXE_assign (sr, e1, e2) ->
      print_endline "BEXE_assign";
      let e1 = codegen_expr state sr e1 in
      let e2 = codegen_expr state sr e2 in
      None

  | Flx_types.BEXE_init (sr, index, init) ->
      print_endline "BEXE_init";

      let the_function = Llvm.block_parent
        (Llvm.insertion_block state.builder)
      in

      let expr = codegen_expr state sr init in
      let alloca =
        let b = Llvm.builder_at state.context
          (Llvm.instr_begin (Llvm.entry_block the_function))
        in
        Llvm.build_alloca (Llvm.type_of expr) (name_of_index index) b
      in

      Hashtbl.add state.value_bindings index expr;

      let _ = Llvm.build_store expr alloca state.builder in
      None

  | Flx_types.BEXE_begin ->
      print_endline "BEXE_begin";
      None

  | Flx_types.BEXE_end ->
      print_endline "BEXE_end";
      None

  | Flx_types.BEXE_assert (sr, e) ->
      print_endline "BEXE_assert";
      let e = codegen_expr state sr e in
      None

  | Flx_types.BEXE_assert2 (sr1, sr2, e1, e2) ->
      print_endline "BEXE_assert2";
      begin
        match e1 with
        | Some e1 ->
            let e1 = codegen_expr state sr1 e1 in
            ()
        | None -> ()
      end;
      let e2 = codegen_expr state sr2 e2 in
      None

  | Flx_types.BEXE_axiom_check (sr, e) ->
      print_endline "BEXE_axiom_check";
      let e = codegen_expr state sr e in
      None


let codegen_proto state index name parameters =
  let parameters = Array.of_list parameters in
  let i32_type = Llvm.i32_type state.context in

  (* Make the function type *)
  let ft = Llvm.function_type
    i32_type
    (Array.map (fun _ -> i32_type) parameters)
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
  Hashtbl.add state.call_bindings index (Function f);

  f


let codegen_function state index name ps es =
  (* Declare the function *)
  let the_function = codegen_proto state index name ps in

  (* Create the initial basic block *)
  let bb = Llvm.append_block state.context "entry" the_function in
  let builder = Llvm.builder_at_end state.context bb in

  (* Codegen the sub-expressions inside our function *)
  let state = { state with builder = builder } in
  let e =
    List.fold_left begin fun _ e ->
      codegen_bexe state e
    end None es
  in
  match e with
  | None -> Llvm.build_ret_void builder
  | Some e -> Llvm.build_ret e builder


let codegen_symbol state index ((name, parent, sr, bbdcl) as symbol) =
  print_endline ("codegen_symbol: " ^ name);

  match bbdcl with
  | Flx_types.BBDCL_function (_, _, (ps, _), _, es) ->
      print_endline "BBDCL_function";
      let _ = codegen_function state index name ps es in
      ()

  | Flx_types.BBDCL_procedure (_, _, (ps, _), es) ->
      print_endline "BBDCL_procedure";
      let _ = codegen_function state index name ps es in
      ()

  | Flx_types.BBDCL_val (vs, ty) ->
      print_endline "BBDCL_val";

  | Flx_types.BBDCL_var (vs, ty) ->
      print_endline "BBDCL_var";

  | Flx_types.BBDCL_ref (vs, ty) ->
      print_endline "BBDCL_ref";

  | Flx_types.BBDCL_tmp (vs, ty) ->
      print_endline "BBDCL_tmp";

  | Flx_types.BBDCL_newtype (vs, ty) ->
      print_endline "BBDCL_newtype";

  | Flx_types.BBDCL_abs (vs, quals, code, reqs) ->
      print_endline "BBDCL_abs";

  | Flx_types.BBDCL_const (props, vs, ty, code, reqs) ->
      print_endline "BBDCL_const";

  | Flx_types.BBDCL_fun (props, vs, ps, ret_type, code, reqs, prec) ->
      print_endline "BBDCL_fun";

      begin match code with
      | Flx_ast.CS_str_template s ->
          print_endline ("CS_str_template: " ^ s);

          (* We found an external function. Lets check if it's a native
           * instruction. Those start with a '%'. Otherwise, it must be the name
           * of an external function. *)
          begin match s with
          | "%add" -> Hashtbl.add state.call_bindings index Inst_add
          | "%sub" -> Hashtbl.add state.call_bindings index Inst_sub
          | _ -> assert false
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
      end

  | Flx_types.BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec) ->
      print_endline "BBDCL_callback";

  | Flx_types.BBDCL_proc (props, vs, ps, code, reqs) ->
      print_endline "BBDCL_proc";

  | Flx_types.BBDCL_insert (vs, s, ikind, reqs) ->
      print_endline "BBDCL_insert";

  | Flx_types.BBDCL_union (vs, cs) ->
      print_endline "BBDCL_union";

  | Flx_types.BBDCL_struct (vs, cs) ->
      print_endline "BBDCL_struct";

  | Flx_types.BBDCL_cstruct (vs, cs) ->
      print_endline "BBDCL_cstruct";

  | Flx_types.BBDCL_typeclass (props, vs) ->
      print_endline "BBDCL_typeclass";

  | Flx_types.BBDCL_instance (props, vs, cons, index, ts) ->
      print_endline "BBDCL_instance";

  | Flx_types.BBDCL_nonconst_ctor
    (vs, uidx, ut, ctor_idx, ctor_argt, evs, etraint) ->
      print_endline "BBDCL_nonconst_ctor";
