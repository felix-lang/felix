open Flx_btype
open Flx_bexpr
open Flx_exceptions
open Flx_print


let bind_projection bsym_table sr v t = 
  let si i = string_of_int i in
  match t with
  (* Tuple Value *)
  | BTYP_tuple ts ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      bexpr_prj v t c

  (* Array value *)
  | BTYP_array (base, _) ->
    let c = base  in
    bexpr_prj v t c

  (* Pointer projections. NOTE: if the pointed at type is a compact linear type,
     the codomain of the projection is a compact linear pointer not an ordinary pointer
  *) 
  (* RW pointer to tuple *)
  | BTYP_pointer (BTYP_tuple ts as vt) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184p] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = if iscompact_linear_product vt then btyp_cltpointer vt c else btyp_pointer c in
      bexpr_prj v t c

  (* RW pointer to array *)
  | BTYP_pointer (BTYP_array (base, _) as vt) ->
      let c = base in
      let c = if iscompact_linear_product vt then btyp_cltpointer vt c else btyp_pointer c in
      bexpr_prj v t c

  (* RO pointer to tuple *)
  | BTYP_rref (BTYP_tuple ts as vt) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184r] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = if iscompact_linear_product vt then btyp_cltrref vt c else btyp_rref c in
      bexpr_prj v t c

  (* RO pointer to array *)
  | BTYP_rref (BTYP_array (base, _) as vt) ->
      let c = base in 
      let c = if iscompact_linear_product vt then btyp_cltrref vt c else btyp_rref c in
      bexpr_prj v t c

  (* WO pointer to tuple *)
  | BTYP_wref (BTYP_tuple ts as vt) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184w] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = if iscompact_linear_product vt then btyp_cltwref vt c else btyp_wref c in
      bexpr_prj v t c

  (* WO pointer to array *)
  | BTYP_wref (BTYP_array (base,_) as vt) ->
      let c = base in
      let c = if iscompact_linear_product vt then btyp_cltwref vt c else btyp_wref c  in
      bexpr_prj v t c

  (* Compact Linear Type Pointers *)
  (* RW pointer to tuple *)
  | BTYP_cltpointer (mach,BTYP_tuple ts) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184p] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_cltpointer mach c in
      bexpr_prj v t c

  (* RW pointer to array *)
  | BTYP_cltpointer (mach,BTYP_array (base, _)) ->
      let c = btyp_cltpointer mach base  in
      bexpr_prj v t c

  (* RO pointer to tuple *)
  | BTYP_cltrref (mach,BTYP_tuple ts) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184r] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_cltrref mach c in
      bexpr_prj v t c

  (* RO pointer to array *)
  | BTYP_cltrref (mach,BTYP_array (base, _)) ->
      let c = btyp_cltrref mach base  in
      bexpr_prj v t c

  (* WO pointer to tuple *)
  | BTYP_cltwref (mach,BTYP_tuple ts) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184w] " sr ("[Flx_lookup.bind_expression] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_cltwref mach c in
      bexpr_prj v t c

  (* WO pointer to array *)
  | BTYP_cltwref (mach,BTYP_array (base,_)) ->
      let c = btyp_cltwref mach base  in
      bexpr_prj v t c

  | _ ->
    clierrx "[flx_bind/flx_lookup.ml:4249: E186] " sr 
    ("[Flx_lookup.bind_expression] projection requires \n" ^ 
    "tuple or array type, or pointer thereto, got\n " ^ 
    sbt bsym_table t ^"\n= " ^ str_of_btype t)

let bind_array_projection counter bsym_table sr (_,ix_t as v) t = 
  let ate t1 t2 = 
    if not (Flx_typeeq.type_eq (sbt bsym_table) counter t1 t2) then
    clierr sr ("bind_array_projection: index type must equal array index type, got:\n" ^
     "index type       = " ^ sbt bsym_table t1 ^"\n" ^
     "array index type = " ^ sbt bsym_table t2)
  in 
  match t with
  (* Array value *)
  | BTYP_array (base, supt) ->
    ate ix_t supt;
    let c = base  in
    bexpr_aprj v t c

  (* RW pointer to array *)
  | BTYP_pointer (BTYP_array (base,  supt) as vt) ->
    ate ix_t supt;
    let c = base in
    let c = if iscompact_linear_product vt then btyp_cltpointer vt c else btyp_pointer c in
    bexpr_aprj v t c

  (* RO pointer to array *)
  | BTYP_rref (BTYP_array (base,  supt) as vt) ->
    ate ix_t supt;
    let c = base in 
    let c = if iscompact_linear_product vt then btyp_cltrref vt c else btyp_rref c in
    bexpr_aprj v t c

  (* Compact Linear Type Pointers *)
  (* RW pointer to array *)
  | BTYP_cltpointer (mach,BTYP_array (base, supt)) ->
    ate ix_t supt;
    let c = btyp_cltpointer mach base  in
    bexpr_aprj v t c

  (* RO pointer to array *)
  | BTYP_cltrref (mach,BTYP_array (base, ( supt))) ->
    ate ix_t supt;
    let c = btyp_cltrref mach base  in
    bexpr_aprj v t c

 (* WO pointer to array *)
  | BTYP_cltwref (mach,BTYP_array (base,supt)) ->
    ate ix_t supt;
    let c = btyp_cltwref mach base  in
    bexpr_aprj v t c

  | _ ->
    clierrx "[flx_bind/flx_lookup.ml:4249: E186] " sr 
    ("[Flx_lookup.bind_expression] array projection requires \n" ^ 
    "array type, or pointer thereto, got\n " ^ 
    sbt bsym_table t ^"\n= " ^ str_of_btype t)


