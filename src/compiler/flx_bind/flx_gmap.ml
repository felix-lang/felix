open Flx_ast
open Flx_btype
open Flx_types

let apl (sr:Flx_srcref.t) (fn : string) (arg:expr_t) =
  EXPR_apply ( sr, ( EXPR_name (sr,fn,[]), arg))

let call (sr:Flx_srcref.t) (fn : string) (arg:expr_t) =
  EXE_call ( EXPR_name (sr,fn,[]), arg)

let generic_rev bsym_table counter be rs sr env b =
  let argx,argt as arg = be rs b in
  match argt with
  | BTYP_tuple ls ->
    let ints = Flx_list.nlist (List.length ls) in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.rev xs in
    let e = EXPR_tuple (sr,xs) in
    be rs e

  | _ ->
    be rs (EXPR_apply (sr, (EXPR_name (sr,"rev",[]),b)))
 
let generic_map bsym_table counter be rs sr env fn b =
  let argx,argt as arg = be rs b in
  match argt with
  | BTYP_tuple ls ->
    let ints = Flx_list.nlist (List.length ls) in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.map (apl sr fn) xs in
    let e = EXPR_tuple (sr,xs) in
    be rs e

  | BTYP_array (t,BTYP_unitsum n) when n < 20 ->
    let ints = Flx_list.nlist n in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.map (apl sr fn) xs in
    let e = EXPR_tuple (sr,xs) in
    be rs e

  | BTYP_array (t,_) ->
    (* print_endline ("Cheating, array element type " ^ Flx_print.sbt bsym_table t); *)
    let ubt = Flx_typecode_of_btype.typecode_of_btype bsym_table counter sr t in
    let fname = EXPR_suffix(sr, (`AST_name (sr,fn,[]),ubt)) in
    be rs (EXPR_apply (sr, (EXPR_apply (sr,(EXPR_name (sr,"map",[]),fname)),b)))

  | BTYP_record ls ->
    let xs = List.map (fun (s,_) ->  s,apl sr fn (EXPR_get_named_variable (sr,(s,b)))) ls in
    let e = EXPR_record (sr,xs) in
    be rs e
  
  | _ -> 
    be rs (EXPR_apply (sr, (EXPR_apply (sr,(EXPR_name (sr,"map",[]),EXPR_name (sr,fn,[]))),b)))

let generic_map_proc bsym_table bind_exe be sr fn b =
  let argx,argt as arg = be b in
  match argt with
  | BTYP_tuple ls ->
    let ints = Flx_list.nlist (List.length ls) in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.map (call sr fn) xs in
    let xs = List.map (fun x -> sr,x) xs in 
    let xs = List.map (bind_exe bsym_table) xs in
    List.concat xs

  | _ -> Flx_exceptions.clierrx "[flx_bind/flx_gmap.ml:42: E79] " sr 
   ("_map procedure not implemented for this type " ^ Flx_print.sbt bsym_table argt)


let generic_rev_map bsym_table counter be rs sr env fn b =
  let argx,argt as arg = be rs b in
  match argt with
  | BTYP_tuple ls ->
    let ints = Flx_list.nlist (List.length ls) in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.map (apl sr fn) xs in
    let xs = List.rev xs in
    let e = EXPR_tuple (sr,xs) in
    be rs e

  | BTYP_array (t,BTYP_unitsum n) when n < 20 ->
    let ints = Flx_list.nlist n in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.map (apl sr fn) xs in
    let e = EXPR_tuple (sr,xs) in
    let xs = List.rev xs in
    be rs e


  | BTYP_array (t,_) ->
    (* print_endline ("Cheating, array element type " ^ Flx_print.sbt bsym_table t); *)
    let ubt = Flx_typecode_of_btype.typecode_of_btype bsym_table counter sr t in
    let fname = EXPR_suffix(sr, (`AST_name (sr,fn,[]),ubt)) in
    be rs (EXPR_apply (sr, (EXPR_apply (sr,(EXPR_name (sr,"rev_map",[]),fname)),b)))

  | _ -> 
    be rs (EXPR_apply (sr, (EXPR_apply (sr,(EXPR_name (sr,"rev_map",[]),EXPR_name (sr,fn,[]))),b)))

let generic_rev_map_proc bsym_table bind_exe be sr fn b =
  let argx,argt as arg = be b in
  match argt with
  | BTYP_tuple ls ->
    let ints = Flx_list.nlist (List.length ls) in
    let xs = List.map (fun i ->  EXPR_get_n (sr,(i,b))) ints in
    let xs = List.rev xs in
    let xs = List.map (call sr fn) xs in
    let xs = List.map (fun x -> sr,x) xs in 
    let xs = List.map (bind_exe bsym_table) xs in
    List.concat xs

  | _ -> Flx_exceptions.clierrx "[flx_bind/flx_gmap.ml:: E79] " sr 
   ("_map procedure not implemented for this type " ^ Flx_print.sbt bsym_table argt)




