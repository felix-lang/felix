open Flx_ast
open Flx_btype

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

