open Flx_ast
open Flx_btype
open Flx_types
open Flx_name_map

let apl2 (sr:Flx_srcref.t) (fn : string) (tup:expr_t list) =
  EXPR_apply
  (
    sr,
    (
      EXPR_name (sr,fn,[]),
      EXPR_tuple (sr,tup)
    )
  )

let land2 sr x y = apl2 sr "land" [x;y]

let truth sr = EXPR_typed_case (sr,1,TYP_unitsum 2) 
let falsity sr = EXPR_typed_case (sr,0,TYP_unitsum 2) 

let landn eq sr xs ys = 
  assert (List.length xs = List.length ys);
  match xs,ys with
  | hdx::tlx, hdy::tly ->
    List.fold_left (fun acc (x,y) -> land2 sr acc (eq sr x y)) (eq sr hdx hdy) (List.combine tlx tly)
  | _ -> truth sr   

let equal' bsym_table sym_table counter be rs sr a b t =
  let eq sr x y = apl2 sr "_eq" [x;y] in (* ourself *)
  match t with
  | BTYP_type_var _ -> 
    print_endline "Type variable?";
    assert false

  | BTYP_record flds ->
    let xas = List.map (fun (s,t) ->  EXPR_get_named_variable (sr,(s,a))) flds in
    let yas = List.map (fun (s,t) ->  EXPR_get_named_variable (sr,(s,b))) flds in
    landn eq sr xas yas

  | BTYP_tuple ts ->
    let ints = Flx_list.nlist (List.length ts) in
    let xas = List.map (fun i ->  EXPR_get_n (sr,(i,a))) ints in
    let yas = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    landn eq sr xas yas
 
  | BTYP_pointer _
  | BTYP_array _ 
  | BTYP_unitsum _ 
  | BTYP_sum _ 
  | BTYP_variant _
  | BTYP_inst _ 
  | _ -> (* print_endline ("Using typeclass =="); *) apl2 sr "==" [a;b]
 
let equal bsym_table sym_table counter be rs sr a b =
  equal' bsym_table sym_table counter be rs sr a b

let bind_eq bsym_table state inner_lookup_name_in_env be rs sr env pair =
(*
    print_endline ("Processing application of _eq");
*)
    let a,b = 
      match pair with
      | EXPR_tuple (sr,[a;b]) -> a,b
      | _ -> Flx_exceptions.clierrx "[flx_bind/flx_eq.ml:63: E75] " sr ("polyadic _eq function requires explicit argument pair")
    in
    begin try 
      let result = be rs (EXPR_apply (sr, (EXPR_name (sr,"__eq",[]), pair))) in
(*
      print_endline ("Found binding of __eq, using that");
*)
      result
    with _ ->
(*
      print_endline ("Failed to find binding of __eq, generating it instead");
*)
      let (_,ta) as ba = be rs a in
      let (_,tb) as bb = be rs b in
      let same_type = Flx_unify.type_eq bsym_table state.Flx_lookup_state.counter ta tb in 
      if not same_type then
         Flx_exceptions.clierrx "[flx_bind/flx_eq.ml:79: E76] " sr ("builtin equality requires arguments to be the same type\n" ^
           "got a=" ^ Flx_print.sbt bsym_table ta ^ "\n" ^
           "and b=" ^ Flx_print.sbt bsym_table tb)
      ;
(*
print_endline ("_eq of type " ^ sbt bsym_table ta);
*)
      let v1 = EXPR_name (sr,"_a",[]) in 
      let v2 = EXPR_name (sr,"_b",[]) in 
      let retexpr = equal bsym_table state.Flx_lookup_state.sym_table state.Flx_lookup_state.counter be rs sr v1 v2 ta in
(*
print_endline ("Got return expression " ^ Flx_print.string_of_expr retexpr);
*)
      let ubt = Flx_typecode_of_btype.typecode_of_btype bsym_table state.Flx_lookup_state.counter sr ta in
(*
print_endline ("Unbound type = " ^ string_of_typecode ubt);
*)
      let p1 = sr,`PVal,"_a",ubt,None in
      let p2 = sr,`PVal,"_b",ubt,None in
      let params = Slist [Satom p1; Satom p2], None in 
      let typecode = TYP_unitsum 2 in
      let properties = [] in
      let asms = [Exe (sr,EXE_fun_return retexpr)] in
      let dcl = DCL_function (params, typecode, Flx_typing.flx_unit,properties, asms) in
      let sdcl = sr,"__eq",None,`Public,dfltvs,dcl in
      let interfaces = ref [] in
      let inits = ref [] in
      let rootsym = Flx_sym_table.find state.Flx_lookup_state.sym_table 0 in 
(*
print_endline ("BEFORE: Name table has " ^ si (Hashtbl.length rootsym.Flx_sym.pubmap) ^ " entries");
print_endline ("BEFORE: Symbol table has " ^ si (Hashtbl.length state.sym_table) ^ " entries");
*)
      let new_index =
        Flx_symtab.build_table_for_dcl
          state.Flx_lookup_state.print_flag
          state.Flx_lookup_state.counter
          state.Flx_lookup_state.sym_table
          "__eq"
          Flx_types.dfltivs
          0 
          (Some 0)
          rootsym.Flx_sym.pubmap
          rootsym.Flx_sym.privmap
          interfaces
          inits
          sdcl
      in
(*
print_endline ("AFTER: Name table has " ^ si (Hashtbl.length rootsym.Flx_sym.pubmap) ^ " entries");
print_endline ("AFTER: Symbol table has " ^ si (Hashtbl.length state.sym_table) ^ " entries");
print_endline ("Assigning index " ^ si new_index);
print_endline ("Added overload of __eq to lookup table!");
*)
      let result = 
        try be rs (EXPR_apply (sr, (EXPR_name (sr,"__eq",[]), pair))) 
        with _ -> 
          print_endline ("Failed to bind application of __eq of "^Flx_print.sbt bsym_table ta^" we just added!!");
          let entries = inner_lookup_name_in_env state bsym_table env Flx_lookup_state.rsground sr "__eq" in
          print_endline ("Found " ^ Flx_print.string_of_entry_set entries);
          begin match entries with
          | NonFunctionEntry _ -> assert false
          | FunctionEntry fs ->
            List.iter 
              (fun entry -> print_endline (Flx_print.full_string_of_entry_kind state.Flx_lookup_state.sym_table bsym_table entry))
              fs
          end;
          assert false
      in
(*
      print_endline ("Found new binding of __eq!");
*)
      result
    end




