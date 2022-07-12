open Flx_mtypes2
open Flx_bid
open Flx_print
open Flx_kind
open Flx_btype

let field_merge counter br fs =
  let type_eq t1 t2 = Flx_typeeq.type_eq Flx_btype.st counter t1 t2 in
  let meet t1 t2 = 
    if type_eq t1 t2 then t1 (* same type, hooray! *) 
    else br (btyp_intersect [t1;t2])  (* calculate the greatest type which is a subtype of both  *)
  in
  let rec aux inp out = 
    match inp with 
    | [] -> List.rev out
    | [x] -> List.rev (x::out)
    | (s1,t1) :: (s2,t2) :: tail -> 
      if s1 = s2 then
        aux ((s1, meet t1 t2) :: tail) out
      else
        aux ((s2,t2) :: tail) ((s1,t1) :: out)
  in aux fs []


let intersect bsym_table counter br ls =
  let ls = List.map br ls in
  begin match ls with
  | [] -> btyp_any ()
  | [BTYP_record fs] -> btyp_record (field_merge counter br fs)
  | [x] -> x
  | h1 :: h2 :: tail ->
    begin match h1, h2 with
    (* Can this be done stepwise? NO. The width subtyping CAN be done stepwise IF it
       just "adds" more and more fields. However if there's a merge, we're intersecting
       the field arguments, and they could be, for example, primitives.

       The problem is the same: stepwise works fine if and only if the least supertype is unique
       and it just isn't.
    *)
    | BTYP_record fs1, BTYP_record fs2 ->
      let fs = fs1 @ fs2 in
      let cmp (s1,t1) (s2, t2) = compare s1 s2 in
      let fs = List.stable_sort cmp fs in
      let fs = field_merge counter br fs in (* the component types are already reduced *)
      let r = btyp_record fs in
      br (btyp_intersect (r :: tail))

    (* THIS IS WRONG. See the notes on least_supertype in Flx_bsym_table!
       We have to find the least supertype of the intersection of ALL the supertypes
       of ALL the primitives, it cannot be done stepwise
    *)
    
    (* primitive least supertype can be calculated here *)
    | BTYP_inst (`Nominal, i, [], KIND_type), BTYP_inst (`Nominal, j, [], KIND_type) ->
      begin match Flx_bsym_table.greatest_subtype bsym_table [i;j] with
      | Some k -> br (btyp_intersect (btyp_inst (`Nominal, k, [], KIND_type) :: tail))
      | None ->
        print_endline ("No greatest subtype of primitives " ^ Flx_btype.st h1 ^ " and " ^ Flx_btype.st h2);
        failwith ("No greatest subtype of primitives " ^ Flx_btype.st h1 ^ " and " ^ Flx_btype.st h2)
      end 
    | _ -> 
      print_endline ("No greatest subtype of " ^ Flx_btype.st h1 ^ " and " ^ Flx_btype.st h2);
      failwith ("No greatest_subtype of " ^ Flx_btype.st h1 ^ " and " ^ Flx_btype.st h2)
    end
  end

