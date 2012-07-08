open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_exceptions
open Flx_srcref

open Flx_lookup_state

let type_eq = Flx_unify.type_eq
let sbt = Flx_print.sbt
let sbe = Flx_print.sbe
let si = string_of_int

let record_coercion state bsym_table sr x' n t' t'' ls' ls'' = 
  try
  bexpr_record t''
  (
    List.map
    (fun (s,t)->
      match Flx_list.list_assoc_index ls' s with
      | Some j ->
        let tt = List.assoc s ls' in
        if type_eq state.Flx_lookup_state.counter t tt then
          s,(bexpr_get_n t (bexpr_unitsum_case j n,x'))
        else clierr sr (
          "Source Record field '" ^ s ^ "' has type:\n" ^
          sbt bsym_table tt ^ "\n" ^
          "but coercion target has the different type:\n" ^
          sbt bsym_table t ^"\n" ^
          "The types must be the same!"
        )
      | None -> raise Not_found
    )
    ls''
  )
  with Not_found ->
    clierr sr
     (
     "Record coercion dst requires subset of fields of src:\n" ^
     sbe bsym_table x' ^ " has type " ^ sbt bsym_table t' ^
    "\nwhereas annotation requires " ^ sbt bsym_table t''
    )

let variant_coercion state bsym_table sr x' t' t'' lhs rhs = 
  try
    List.iter
    (fun (s,t)->
      match Flx_list.list_assoc_index rhs s with
      | Some j ->
        let tt = List.assoc s rhs in
        if not (type_eq state.counter t tt) then
        clierr sr (
          "Source Variant field '" ^ s ^ "' has type:\n" ^
          sbt bsym_table t ^ "\n" ^
          "but coercion target has the different type:\n" ^
          sbt bsym_table tt ^"\n" ^
          "The types must be the same!"
        )
      | None -> raise Not_found
    )
    lhs
    ;
    print_endline ("Coercion of variant to type " ^ sbt bsym_table t'');
    bexpr_coerce (x',t'')
  with Not_found ->
    clierr sr
     (
     "Variant coercion src requires subset of fields of dst:\n" ^
     sbe bsym_table x' ^ " has type " ^ sbt bsym_table t' ^
    "\nwhereas annotation requires " ^ sbt bsym_table t''
    )

let coerce (state:Flx_lookup_state.lookup_state_t) bsym_table sr ((e',t') as x') t'' =
(*
print_endline ("Binding coercion " ^ sbe bsym_table x' ^ ": " ^ sbt bsym_table t' ^ " to " ^ sbt bsym_table t'');
*)
    if type_eq state.Flx_lookup_state.counter t' t'' then x'
    else
    begin match t',t'' with
    | BTYP_inst (i,[]),t when Flx_btype.islinear_type bsym_table t->
      let n = Flx_btype.sizeof_linear_type bsym_table  t in
      begin match hfind "lookup" state.sym_table i with
      | { Flx_sym.id="int";
          symdef=SYMDEF_abs (_, Flx_code_spec.Str_template "int", _) }  ->
        begin match e' with
        | BEXPR_literal {Flx_literal.felix_type="int"; internal_value=big} ->
(*
print_endline "Coercion from int literal";
*)
          let m =
            try int_of_string big
            with _ -> clierr sr "Integer is too large for unitsum"
          in
          if m >=0 && m < n then
            bexpr_case t'' (m,t'')
          else
            clierr sr "Integer is out of range for unitsum"
        | _ ->
(*
print_endline "Coercion from int expression ";
*)
          let inttype = t' in
          let zero =
            bexpr_literal t' {Flx_literal.felix_type="int"; internal_value="0"; c_value="0"}
          in
          let xn =
            bexpr_literal t' {Flx_literal.felix_type="int"; internal_value=string_of_int n; c_value=string_of_int n}
          in
          let r = bexpr_coerce (bexpr_range_check t' (zero,x',xn),t'') in
(*
print_endline ("Coercion from int expression result is " ^ sbe bsym_table r);
*)
          r
        end
      | _ ->
        clierr sr ("Attempt to to coerce type:\n"^
        sbt bsym_table t'
        ^"to unitsum " ^ si n)
      end

    | t,(BTYP_inst (i,[]) as inttype) when Flx_btype.islinear_type bsym_table t->
      let n = Flx_btype.sizeof_linear_type bsym_table  t in
      begin match hfind "lookup" state.sym_table i with
      | { Flx_sym.id="int";
          symdef=SYMDEF_abs (_, Flx_code_spec.Str_template "int", _) }  ->
        Flx_bexpr.bexpr_coerce (x',inttype)

      | _ ->
        clierr sr ("Attempt to to coerce unitsum "^si n^" to type:\n"^
        sbt bsym_table t')
      end

    | BTYP_record (n',ls'),BTYP_record (n'',ls'') when n' = n''->
      let n = List.length ls' in
      record_coercion state bsym_table sr x' n t' t'' ls' ls'' 

    | BTYP_variant lhs,BTYP_variant rhs ->
      variant_coercion state bsym_table sr x' t' t'' lhs rhs 

    | BTYP_array (BTYP_array (v,ix1),ix2),BTYP_array(v',BTYP_tuple[ix1';ix2']) ->
      if v <> v' then
        clierr sr ("Coercion: source array of arrays value type " ^ sbt bsym_table v ^
         " not equal to target value type " ^ sbt bsym_table v')
      ;
      if ix1 <> ix1' || ix2 <> ix2' then
        clierr sr ("Coercion: source array of arrays value index types " ^ 
          sbt bsym_table ix1 ^ " and " ^ sbt bsym_table ix2 ^ 
          " not equal to target index types " ^ 
          sbt bsym_table ix2' ^ " and " ^ sbt bsym_table ix2')
      ; 
      bexpr_coerce (x',t'')

    | BTYP_array (BTYP_array (v,ix1),ix2),BTYP_array(v',BTYP_array(ix',BTYP_unitsum 2)) ->
      if v <> v' then
        clierr sr ("Coercion: source array of arrays value type " ^ sbt bsym_table v ^
         " not equal to target value type " ^ sbt bsym_table v')
      ;
      if ix1 <> ix' || ix2 <> ix' then
        clierr sr ("Coercion: source array of arrays value index types " ^ 
          sbt bsym_table ix1 ^ " and " ^ sbt bsym_table ix2 ^ 
          " not equal to target index types " ^ 
          sbt bsym_table ix' ^ " and " ^ sbt bsym_table ix')
      ; 
      bexpr_coerce (x',t'')


    | BTYP_array(v',BTYP_tuple[ix1';ix2']), BTYP_array (BTYP_array (v,ix1),ix2)->
      if v <> v' then
        clierr sr ("Coercion: source array value type " ^ sbt bsym_table v ^
         " not equal to target value type " ^ sbt bsym_table v')
      ;
      if ix1 <> ix1' || ix2 <> ix2' then
        clierr sr ("Coercion: source array value index types " ^ 
          sbt bsym_table ix1' ^ " and " ^ sbt bsym_table ix2' ^ 
          " not equal to target index types " ^ 
          sbt bsym_table ix1 ^ " and " ^ sbt bsym_table ix2)
      ;
      bexpr_coerce (x',t'')

    | BTYP_array(v',BTYP_array(ix',BTYP_unitsum 2)), BTYP_array (BTYP_array (v,ix1),ix2)->
      if v <> v' then
        clierr sr ("Coercion: source array value type " ^ sbt bsym_table v ^
         " not equal to target value type " ^ sbt bsym_table v')
      ;
      if ix1 <> ix' || ix2 <> ix' then
        clierr sr ("Coercion: source array value index types " ^ 
          sbt bsym_table ix' ^ " and " ^ sbt bsym_table ix' ^ 
          " not equal to target index types " ^ 
          sbt bsym_table ix1 ^ " and " ^ sbt bsym_table ix2)
      ;
      bexpr_coerce (x',t'')



    | _ ->
      clierr sr
      (
        "Wrong type in coercion:\n" ^
        sbe bsym_table x' ^ " has type " ^ sbt bsym_table t' ^
        "\nwhereas annotation requires " ^ sbt bsym_table t''
      )
    end


