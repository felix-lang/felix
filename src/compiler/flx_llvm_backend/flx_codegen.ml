type call_t = Llvm.llvalue array -> string -> Llvm.llbuilder -> Llvm.llvalue

type codegen_state_t = {
  syms: Flx_mtypes2.sym_state_t;
  bbdfns: Flx_types.fully_bound_symbol_table_t;
  context: Llvm.llcontext;
  the_module: Llvm.llmodule;
  builder: Llvm.llbuilder;
  type_bindings: (Flx_ast.bid_t, Llvm.lltype) Hashtbl.t;
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
    type_bindings = Hashtbl.create 97;
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
let rec codegen_expr state sr tbexpr =
  print_endline ("codegen_expr: " ^ Flx_print.string_of_bound_expression
    state.syms.Flx_mtypes2.dfns state.bbdfns tbexpr);

  (* See if there are any simple reductions we can apply to the expression. *)
  let bexpr, btypecode = Flx_maps.reduce_tbexpr state.bbdfns tbexpr in

  (* Helper function to look up the name of an index. *)
  let name_of_index = name_of_index state.syms.Flx_mtypes2.dfns state.bbdfns in

  match bexpr with
  | Flx_types.BEXPR_deref e ->
      print_endline "BEXPR_deref";
      let value = codegen_expr state sr e in
      Llvm.build_load value "deref" state.builder

  | Flx_types.BEXPR_name (index, btypecode) ->
      print_endline "BEXPR_name";
      Hashtbl.find state.value_bindings index

  | Flx_types.BEXPR_ref (index, btypecode) ->
      print_endline "BEXPR_ref";
      assert false

  | Flx_types.BEXPR_likely e ->
      print_endline "BEXPR_likely";
      (* Do nothing for now *)
      codegen_expr state sr e

  | Flx_types.BEXPR_unlikely e ->
      print_endline "BEXPR_unlikely";
      (* Do nothing for now *)
      codegen_expr state sr e

  | Flx_types.BEXPR_address e ->
      print_endline "BEXPR_address";
      (* Expressions can only have their address taken if they're on the stack.
       * So, we shouldn't need to do any work. *)

      (* FIXME: Add a check to make sure we're returning the right type *)
      codegen_expr state sr e

  | Flx_types.BEXPR_new e ->
      print_endline "BEXPR_new";
      let _ = codegen_expr state sr e in
      assert false

  | Flx_types.BEXPR_not e ->
      print_endline "BEXPR_not";
      let e = codegen_expr state sr e in
      let ty = Llvm.type_of e in
      let e =
        Llvm.build_icmp Llvm.Icmp.Eq e (Llvm.const_int ty 0) "not" state.builder
      in
      Llvm.build_zext e ty "not" state.builder

  | Flx_types.BEXPR_literal literal ->
      print_endline "BEXPR_literal";
      codegen_literal state sr literal

  | Flx_types.BEXPR_apply (f, e) ->
      print_endline "BEXPR_apply";
      assert false

  | Flx_types.BEXPR_apply_prim (index, _, (Flx_types.BEXPR_tuple es, _))
  | Flx_types.BEXPR_apply_direct (index, _, (Flx_types.BEXPR_tuple es, _))
  | Flx_types.BEXPR_apply_stack (index, _, (Flx_types.BEXPR_tuple es, _))
  | Flx_types.BEXPR_apply_struct (index, _, (Flx_types.BEXPR_tuple es, _)) ->
      print_endline "BEXPR_apply_{prim,direct,stack_struct}";

      let f = Hashtbl.find state.call_bindings index in
      let es = List.map (codegen_expr state sr) es in

      f (Array.of_list es) "apply" state.builder

  | Flx_types.BEXPR_apply_prim (index, _, e)
  | Flx_types.BEXPR_apply_direct (index, _, e)
  | Flx_types.BEXPR_apply_stack (index, _, e)
  | Flx_types.BEXPR_apply_struct (index, _, e) ->
      print_endline "BEXPR_apply_{prim,direct,stack_struct}";

      let f = Hashtbl.find state.call_bindings index in
      let e = codegen_expr state sr e in

      f [|e|] "apply" state.builder

  | Flx_types.BEXPR_tuple es ->
      print_endline "BEXPR_tuple";
      codegen_struct state sr es btypecode

  | Flx_types.BEXPR_record es ->
      print_endline "BEXPR_record";
      codegen_struct state sr (List.map snd es) btypecode

  | Flx_types.BEXPR_variant (string, e) ->
      print_endline "BEXPR_variant";
      let _ = codegen_expr state sr e in
      assert false

  | Flx_types.BEXPR_get_n (n, e) ->
      print_endline "BEXPR_get_n";
      let _ = codegen_expr state sr e in
      assert false

  | Flx_types.BEXPR_closure (index, btypecode) ->
      print_endline ("BEXPR_closure: " ^ name_of_index index);
      Hashtbl.find state.value_bindings index

  | Flx_types.BEXPR_case (int, btypecode) ->
      print_endline "BEXPR_case";
      assert false

  | Flx_types.BEXPR_match_case (int, e) ->
      print_endline "BEXPR_match_case";
      let _ = codegen_expr state sr e in
      assert false

  | Flx_types.BEXPR_case_arg (int, e) ->
      print_endline "BEXPR_case_arg";
      let _ = codegen_expr state sr e in
      assert false

  | Flx_types.BEXPR_case_index e ->
      print_endline "BEXPR_case_index";
      let _ = codegen_expr state sr e in
      assert false

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

(* Generate code for an llvm struct type. *)
and codegen_struct state sr es btypecode =
  let es = List.map (codegen_expr state sr) es in
  let ts = List.map Llvm.type_of es in
  let t = lltype_of_btype state btypecode in
  let _, s =
    List.fold_left begin fun (i, s) e ->
      i + 1, Llvm.build_insertvalue s e i "" state.builder
    end (0, Llvm.undef t) es
  in
  s


let codegen_bexe state bexe =
  print_endline ("codegen_bexe: " ^ Flx_print.string_of_bexe
    state.syms.Flx_mtypes2.dfns state.bbdfns 0 bexe);

  (* See if there are any simple reductions we can apply to the exe. *)
  let bexe = Flx_maps.reduce_bexe state.bbdfns bexe in

  (* Helper function to look up the name of an index. *)
  let name_of_index = name_of_index state.syms.Flx_mtypes2.dfns state.bbdfns in

  match bexe with
  | Flx_types.BEXE_label (sr, string) ->
      print_endline "BEXE_label";
      assert false

  | Flx_types.BEXE_comment (sr, string) ->
      (* Ignore the comment. *)
      None

  | Flx_types.BEXE_halt (sr, string) ->
      print_endline "BEXE_halt";
      assert false

  | Flx_types.BEXE_trace (sr, s1, s2) ->
      print_endline "BEXE_trace";
      assert false

  | Flx_types.BEXE_goto (sr, string) ->
      print_endline "BEXE_goto";
      assert false

  | Flx_types.BEXE_ifgoto (sr, e, string) ->
      print_endline "BEXE_ifgoto";
      let expr = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_call (sr, p, a) ->
      let e1 = codegen_expr state sr p in
      let e2 = codegen_expr state sr a in
      assert false

  | Flx_types.BEXE_call_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_call_direct";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_call_stack (sr, index, btypecode, e) ->
      print_endline "BEXE_call_stack";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_call_prim (sr, index, btypecode, e) ->
      print_endline "BEXE_call_prim";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_jump (sr, e1, e2) ->
      print_endline "BEXE_jump";
      assert false

  | Flx_types.BEXE_jump_direct (sr, index, btypecode, e) ->
      print_endline "BEXE_jump_direct";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_loop (sr, int, e) ->
      print_endline "BEXE_loop";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_svc (sr, index) ->
      print_endline "BEXE_svc";
      assert false

  | Flx_types.BEXE_fun_return (sr, e) ->
      print_endline "BEXE_fun_return";
      Some (codegen_expr state sr e)

  | Flx_types.BEXE_yield (sr, e) ->
      print_endline "BEXE_yield";
      let e = codegen_expr state sr e in
      assert false

  | Flx_types.BEXE_proc_return sr ->
      print_endline "BEXE_proc_return";
      None

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
      let lhs = codegen_expr state sr lhs in
      let rhs = codegen_expr state sr rhs in
      Some (Llvm.build_store rhs lhs state.builder)

  | Flx_types.BEXE_init (sr, index, e) ->
      print_endline "BEXE_init";

      let e = codegen_expr state sr e in
      Llvm.set_value_name (name_of_index index) e;
      Hashtbl.add state.value_bindings index e;
      None

  | Flx_types.BEXE_begin ->
      print_endline "BEXE_begin";
      assert false

  | Flx_types.BEXE_end ->
      print_endline "BEXE_end";
      assert false

  | Flx_types.BEXE_assert (sr, e) ->
      print_endline "BEXE_assert";
      let e = codegen_expr state sr e in
      assert false

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
      assert false

  | Flx_types.BEXE_axiom_check (sr, e) ->
      print_endline "BEXE_axiom_check";
      let e = codegen_expr state sr e in
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


let codegen_function state index name ps ret_type es =
  (* Declare the function *)
  let the_function = codegen_proto state index name ps ret_type in

  (* Create the initial basic block *)
  let bb = Llvm.append_block state.context "entry" the_function in
  let builder = Llvm.builder_at_end state.context bb in

  try
    (* Codegen the sub-expressions inside our function *)
    let state = { state with builder = builder } in
    let e =
      List.fold_left begin fun _ e ->
        codegen_bexe state e
      end None es
    in
    let _ =
      match e with
      | None -> Llvm.build_ret_void builder
      | Some e -> Llvm.build_ret e builder
    in

    (* Validate the generated code, checking for consistency. *)
    Llvm_analysis.assert_valid_function the_function;

    (* Return the function *)
    the_function
  with e ->
    Llvm.delete_function the_function;
    raise e


(* Create an llvm function from a felix function *)
let codegen_fun state index props vs ps ret_type code reqs prec =
  (* Helper function to look up the name of an index. *)
  let name_of_index = name_of_index state.syms.Flx_mtypes2.dfns state.bbdfns in

  let f =
    match code with
    | Flx_ast.CS_str_template s ->
        print_endline ("CS_str_template: " ^ s);

        (* We found an external function. Lets check if it's a native
         * instruction. Those start with a '%'. Otherwise, it must be the name
         * of an external function. *)
        let f =
          match s with
          | "%add" -> Llvm.build_add
          | "%sub" -> Llvm.build_sub
          | "%subscript" ->
              fun lhs rhs name builder ->
                let idx = Llvm.const_int (Llvm.i32_type state.context) 0 in
                Llvm.build_gep lhs [| idx; rhs |] name builder
          | s ->
              failwith ("Unknown instruction " ^ s)
        in

        begin function
        | [| lhs; rhs |] -> f lhs rhs
        | [||] | [| _ |] ->
            failwith ("Not enough arguments for " ^ (name_of_index index))
        | _ ->
            failwith ("Too many arguments for " ^ (name_of_index index))
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
        print_endline ("CS_str_template: " ^ s);

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
      print_endline "BBDCL_function";
      let f = codegen_function state index name ps ret_type es in
      Hashtbl.add state.value_bindings index f;
      ()

  | Flx_types.BBDCL_procedure (_, _, (ps, _), es) ->
      print_endline "BBDCL_procedure";
      let f = codegen_function state index name ps Flx_types.BTYP_void es in
      Hashtbl.add state.value_bindings index f;
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
      assert false

  | Flx_types.BBDCL_abs (vs, quals, code, reqs) ->
      print_endline "BBDCL_abs";
      codegen_abs state index vs quals code reqs

  | Flx_types.BBDCL_const (props, vs, ty, code, reqs) ->
      print_endline "BBDCL_const";
      assert false

  | Flx_types.BBDCL_fun (props, vs, ps, ret_type, code, reqs, prec) ->
      print_endline "BBDCL_fun";
      codegen_fun state index props vs ps ret_type code reqs prec

  | Flx_types.BBDCL_callback (props, vs, ps_cf, ps_c, k, rt, reqs, prec) ->
      print_endline "BBDCL_callback";
      assert false

  | Flx_types.BBDCL_proc (props, vs, ps, code, reqs) ->
      print_endline "BBDCL_proc";
      assert false

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
