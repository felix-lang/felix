(** Generic source reference manipulation.
 *
 * Note the special hack of forgetting the second filename when creating a
 * range: the alternative would be to record a complete list of lines. *)

let dummy_sr = ("generated",0,0,0,0)

(** axiom: rstoken a b = rsrange (lift a) (lift b) *)

(** get source range from source references of first
   and last tokens
*)
let rstoken (f1,l1,s1,e1) (f2,l2,s2,e2) = (f1,l1,s1,l2,e2)

(** get range from first and last ranges *)
let rsrange (f1,sl1,sc1,el1,ec1) (f2,sl2,sc2,el2,ec2) =
  (f1,sl1,sc1,el2,ec2)

(** lift token source to range for token without attribute*)
let slift (f,l,s,e) = (f,l,s,l,e)

(** lift token source to range for tokens with attribute*)
let sliftfst x = slift (fst x)

(** {6 Type specific operations} *)

open Flx_util
open Flx_list
open Flx_ast
open Flx_types

let src_of_bexe = function
  | `BEXE_goto (sr,_)
  | `BEXE_assert (sr,_)
  | `BEXE_assert2 (sr,_,_,_)
  | `BEXE_axiom_check (sr,_)
  | `BEXE_halt (sr,_)
  | `BEXE_trace (sr,_,_)
  | `BEXE_ifgoto (sr,_,_)
  | `BEXE_label (sr,_)
  | `BEXE_comment (sr,_)
  | `BEXE_call (sr,_,_)
  | `BEXE_call_direct (sr,_,_,_)
  | `BEXE_jump_direct (sr,_,_,_)
  | `BEXE_call_stack (sr,_,_,_)
  | `BEXE_call_prim (sr,_,_,_)
  | `BEXE_jump (sr,_,_)
  | `BEXE_loop (sr,_,_)
  | `BEXE_svc (sr,_)
  | `BEXE_fun_return (sr,_)
  | `BEXE_yield (sr,_)
  | `BEXE_proc_return sr
  | `BEXE_nop (sr,_)
  | `BEXE_code (sr,_)
  | `BEXE_nonreturn_code (sr,_)
  | `BEXE_assign (sr,_,_)
  | `BEXE_init (sr,_,_)
    -> sr

  | `BEXE_begin
  | `BEXE_end -> dummy_sr

let src_of_qualified_name (e : qualified_name_t) = match e with
  | `AST_void s
  | `AST_name  (s,_,_)
  | `AST_case_tag (s,_)
  | `AST_typed_case (s,_,_)
  | `AST_lookup (s,_)
  | `AST_the (s,_)
  | `AST_index (s,_,_)
  | `AST_callback (s,_)
    -> s

let src_of_suffixed_name (e : suffixed_name_t) = match e with
  | #qualified_name_t as x -> src_of_qualified_name x
  | `AST_suffix (s,_)
    -> s

let src_of_expr (e : expr_t) = match e with
  | #suffixed_name_t as x -> src_of_suffixed_name x
  | `AST_interpolate (s,_)
  | `AST_vsprintf (s,_)
  | `AST_ellipsis (s)
  | `AST_noexpand (s,_)
  | `AST_product (s,_)
  | `AST_sum (s,_)
  | `AST_setunion (s,_)
  | `AST_intersect (s,_)
  | `AST_isin (s,_)
  | `AST_setintersection (s,_)
  | `AST_orlist (s,_)
  | `AST_andlist (s,_)
  | `AST_arrow (s,_)
  | `AST_longarrow (s,_)
  | `AST_superscript (s,_)
  | `AST_patvar (s,_)
  | `AST_patany s

  | `AST_map (s,_,_)
  | `AST_apply  (s,_)
  | `AST_deref (s,_)
  | `AST_new (s,_)
  | `AST_ref  (s,_)
  | `AST_likely (s,_)
  | `AST_unlikely (s,_)
  | `AST_lift (s,_)
  | `AST_literal  (s,_)
  | `AST_tuple  (s,_)
  | `AST_record (s,_)
  | `AST_variant (s,_)
  | `AST_record_type (s,_)
  | `AST_variant_type (s,_)
  | `AST_arrayof (s,_)
  | `AST_dot  (s,_)
  | `AST_lambda  (s,_)
  | `AST_match_ctor  (s,_)
  | `AST_match_case (s,_)
  | `AST_ctor_arg  (s,_)
  | `AST_case_arg  (s,_)
  | `AST_case_index (s,_)
  | `AST_get_n  (s,_)
  | `AST_get_named_variable  (s,_)
  | `AST_coercion (s,_)
  | `AST_as (s,_)
  | `AST_match (s, _)
  | `AST_type_match (s, _)
  | `AST_cond (s,_)
  | `AST_expr (s,_,_)
  | `AST_letin (s,_)
  | `AST_typeof (s,_)
  | `AST_macro_ctor (s,_)
  | `AST_macro_statements (s,_)
  | `AST_case (s,_,_,_)
  | `AST_user_expr (s,_,_)
    -> s

let src_of_stmt e = match e with
  (*
  | `AST_public (s,_,_)
  *)
  | `AST_private (s,_)
  | `AST_label (s,_)
  | `AST_goto (s,_)
  | `AST_assert (s,_)
  | `AST_init (s,_,_)
  | `AST_function (s,_, _, _ , _, _, _)
  | `AST_reduce (s,_, _, _ , _, _)
  | `AST_axiom (s,_, _, _ , _)
  | `AST_lemma (s,_, _, _ , _)
  | `AST_curry (s,_, _, _ , _, _,_)
  | `AST_macro_name (s, _,_)
  | `AST_macro_names (s, _,_)
  | `AST_expr_macro (s,_, _,_)
  | `AST_stmt_macro (s,_, _,_)
  | `AST_macro_block (s,_)
  | `AST_macro_val (s,_,_)
  | `AST_macro_vals (s,_,_)
  | `AST_macro_var (s, _,_)
  | `AST_macro_assign (s,_,_)
  | `AST_macro_forget (s,_)
  | `AST_macro_label (s,_)
  | `AST_macro_goto (s,_)
  | `AST_macro_ifgoto (s,_,_)
  | `AST_macro_proc_return s
  | `AST_macro_ifor (s,_,_,_)
  | `AST_macro_vfor (s,_,_,_)

  | `AST_val_decl (s,_,_,_,_)
  | `AST_lazy_decl (s,_,_,_,_)
  | `AST_var_decl (s,_,_,_,_)
  | `AST_ref_decl (s,_,_,_,_)


  | `AST_type_alias (s,_,_,_)
  | `AST_inherit (s,_,_,_)
  | `AST_inherit_fun (s,_,_,_)
  | `AST_nop (s, _)

  | `AST_assign (s, _, _,_ )
  | `AST_cassign (s, _,_ )
  | `AST_call (s, _, _ )
  | `AST_jump (s, _, _ )
  | `AST_loop (s, _, _ )
  | `AST_svc (s, _)
  | `AST_fun_return (s, _)
  | `AST_yield (s, _)
  | `AST_proc_return s
  | `AST_halt (s,_)
  | `AST_trace (s,_,_)
  | `AST_ifgoto (s,_,_)
  | `AST_ifreturn (s,_)
  | `AST_ifdo (s,_,_,_)
  (*
  | `AST_whilst (s,_,_)
  | `AST_until (s,_,_)
  *)
  | `AST_abs_decl (s,_,_, _,_,_)
  | `AST_newtype (s,_,_,_)
  | `AST_ctypes (s,_,_,_)
  | `AST_const_decl (s,_,_,_,_,_)
  | `AST_fun_decl (s,_,_,_,_,_,_,_ )
  | `AST_callback_decl (s,_,_,_,_)
  | `AST_insert (s,_,_,_,_,_)
  | `AST_code (s, _)
  | `AST_noreturn_code (s, _)
  | `AST_union (s, _,_, _ )
  | `AST_struct (s,_, _, _)
  | `AST_typeclass (s,_, _, _)
  | `AST_instance (s,_, _,_)
  | `AST_untyped_module (s,_,_,_)
  | `AST_namespace (s,_,_,_)
  | `AST_export_fun (s, _,_)
  | `AST_export_python_fun (s, _,_)
  | `AST_export_type (s, _,_)
  | `AST_type (s,_,_)
  | `AST_open (s,_,_)
  | `AST_inject_module (s,_)
  | `AST_include (s,_)
  | `AST_use (s,_,_)
  | `AST_seq (s,_)
  | `AST_user_statement (s,_,_)
  | `AST_scheme_string (s,_)
    -> s
  | `AST_comment (s,_) -> s


let src_of_pat e = match e with
  | `PAT_coercion (s,_,_)
  | `PAT_nan s
  | `PAT_none s
  | `PAT_int (s,_,_)
  | `PAT_string (s, _)
  | `PAT_int_range (s,_,_,_,_)
  | `PAT_string_range (s, _, _)
  | `PAT_float_range (s, _,_)
  | `PAT_name (s, _)
  | `PAT_tuple (s, _)
  | `PAT_any s
  | `PAT_const_ctor (s, _)
  | `PAT_nonconst_ctor (s, _, _)
  | `PAT_as (s, _, _)
  | `PAT_when (s, _, _)
  | `PAT_record (s, _)
    -> s

(* get range from first and last expressions *)
let rsexpr a b = rsrange (src_of_expr a) (src_of_expr b)

(* get source range of non-empty list of expressions *)
let rslist lst =
  rsexpr (List.hd lst) (list_last lst)


let short_string_of_src (f,l1,c1,l2,c2) =
  if l1 = l2
  then
    f ^ ": line " ^ si l1 ^
    ", cols " ^ si c1 ^ " to " ^ si c2
  else
    f ^ ": line " ^ si l1 ^
    " col " ^ si c1 ^ " to " ^
    " line " ^ si l2 ^ " col " ^ si c2

let get_lines f context l1' l2' c1 c2 = (* first line is line 1 *)
  let l1 = max 1 (l1'-context) in
  let l2 = l2' + context in
  let n = String.length (si l2) in
  let fmt i =
    let s ="    " ^ si i in
    let m = String.length s in
    String.sub s (m-n) n
  in
  try
    let buf = Buffer.create ((l2-l1+4) * 80) in
    let spc () = Buffer.add_char buf ' ' in
    let star() = Buffer.add_char buf '*' in
    let nl() = Buffer.add_char buf '\n' in
    let f = open_in f in
    for i = 1 to l1-1 do ignore(input_line f) done;
    let too_long = l2'-l1' > 20 in
    begin
      try
        for i = l1 to l2 do
          let s = input_line f in
          if too_long && i = l1'+3 then
            Buffer.add_string buf ("...\n")
          else if too_long && i > l1'+3 && i< l2'-3 then () else
          begin
            Buffer.add_string buf (fmt i ^": ");
            Buffer.add_string buf s;
            nl();
            if i = l1' && l1' = l2' then
            begin
              for i = 1 to n + 2 do spc() done;
              for i = 1 to c1 - 1 do spc() done;
              for i = c1 to c2 do star() done;
              nl()
            end
          end
        done
      with _ -> Buffer.add_string buf "<eof>\n"
    end
    ;
    close_in f;
    Buffer.contents buf
  with _ ->
    "*** Can't read file " ^ f ^
    " lines " ^ fmt l1 ^ " thru " ^ fmt l2 ^ "\n"

let long_string_of_src (f,l1,c1,l2,c2) =
  short_string_of_src (f,l1,c1,l2,c2) ^
  "\n" ^
  get_lines f 1 l1 l2 c1 c2
