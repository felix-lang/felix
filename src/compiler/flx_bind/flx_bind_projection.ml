open Flx_btype
open Flx_bexpr
open Flx_exceptions
open Flx_print

(* This routine binds STANDALONE constant tuple and array value and pointer projections,
 * including all pointer kinds, including compact linear type pointers.
 *)
let bind_projection bsym_table sr v t = 
(*
print_endline ("bind projection type " ^ sbt bsym_table t ^ " index " ^ string_of_int v);
*)
  let si i = string_of_int i in
  (* Constant tuple value projection *)
  match t with
  (* Tuple Value *)
  | BTYP_compacttuple ts 
  | BTYP_tuple ts ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184] " sr ("[Flx_lookup.bind_projection] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      bexpr_prj v t c

  (* Constant array value projection *)
  | BTYP_compactarray (base, _) 
  | BTYP_array (base, _) ->
    let c = base  in
    bexpr_prj v t c

  (* Pointer projections. NOTE: if the pointed at type is a compact linear type,
     the codomain of the projection is a compact linear pointer not an ordinary pointer
  *) 
  (* Constant pointer to tuple projection *)
  | BTYP_ptr (mode,(BTYP_tuple ts as vt),[]) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184p] " sr ("[Flx_lookup.bind_projection] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_ptr mode c [] in
      bexpr_prj v t c

  | BTYP_ptr (mode,(BTYP_compacttuple ts as vt),[]) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_lookup.ml:4235: E184p] " sr ("[Flx_lookup.bind_projection] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_ptr mode c [vt] in
      bexpr_prj v t c


  (* Constant pointer to array projection *)
  | BTYP_ptr (mode,(BTYP_array (base, _) as vt),[]) ->
      let c = base in
      let c = btyp_ptr mode c [] in
      bexpr_prj v t c

  | BTYP_ptr (mode,(BTYP_compactarray (base, _) as vt),[]) ->
      let c = base in
      let c = btyp_ptr mode c [vt] in
      bexpr_prj v t c

  (* Compact Linear Type Pointers *)
  (* Constant pointer to CLT tuple projection *)
  | BTYP_ptr (mode,BTYP_compacttuple ts,[mach]) ->
    let n = List.length ts in
    if v < 0 || v >= n then
      clierrx "[flx_bind/flx_bind_projection.ml:4235: E184p] " sr ("[Flx_lookup.bind_bind_projection] projection index " ^ si v ^ 
        " negative or >= " ^ si n ^ "for tuple type " ^ sbt bsym_table t)
    else
      let c = List.nth ts v in
      let c = btyp_ptr mode c [mach] in
      bexpr_prj v t c

  (* Constant pointer to CLT array projection *)
  | BTYP_ptr (mode,BTYP_compactarray (base, _),[mach]) ->
      let c = btyp_ptr mode base [mach] in
      bexpr_prj v t c

  | _ ->
    clierrx "[flx_bind/flx_lookup.ml:4249: E186] " sr 
    ("[Flx_lookup.bind_projection] projection requires \n" ^ 
    "tuple or array type, or pointer thereto, got\n " ^ 
    sbt bsym_table t ^"\n= " ^ str_of_btype t)

(* Array projections, non-constant projection index *)
let bind_array_projection counter bsym_table sr (_,ix_t as v) t = 
(*
print_endline ("binding array projection, array type = " ^ sbt bsym_table t);
*)
  let ate t1 t2 = 
    if not (Flx_typeeq.type_eq (sbt bsym_table) counter t1 t2) then
    clierr sr ("Flx_bind_projection:bind_array_projection]: index type must equal array index type, got:\n" ^
     "index type       = " ^ sbt bsym_table t1 ^"\n" ^
     "array index type = " ^ sbt bsym_table t2)
  in 
  match t with
  (* Array value *)
  | BTYP_compactarray (base, supt)
  | BTYP_array (base, supt) ->
    ate ix_t supt;
    let c = base  in
    bexpr_aprj v t c

  (* RW pointer to array *)
  | BTYP_ptr (mode,(BTYP_array (base,  supt) as vt),[]) ->
    ate ix_t supt;
    let c = base in
    let c = btyp_ptr mode c [] in
    let (_,pt as p) = bexpr_aprj v t c in
(*
print_endline ("Projection type " ^ sbt bsym_table pt);
*)
    p

  | BTYP_ptr (mode,(BTYP_compactarray (base,  supt) as vt),[]) ->
    ate ix_t supt;
    let c = base in
    let c = btyp_ptr mode c [vt]  in
    let (_,pt as p) = bexpr_aprj v t c in
(*
print_endline ("Projection type " ^ sbt bsym_table pt);
*)
    p

  (* Compact Linear Type Pointers *)
  (* RW pointer to array *)
  | BTYP_ptr (mode,BTYP_compactarray (base, supt),[mach]) ->
    ate ix_t supt;
    let c = btyp_ptr mode base [mach]  in
    bexpr_aprj v t c

  | _ ->
    clierrx "[flx_bind/flx_bind_prohection.ml:111: E186] " sr 
    ("[Flx_lookup.bind_projection] array projection requires \n" ^ 
    "array type, or pointer thereto, got\n " ^ 
    sbt bsym_table t ^"\n= " ^ str_of_btype t)


