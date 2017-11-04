open Flx_ast
open Flx_types
open Flx_bid

(* MOVED FROM PARSER so flx_sex2flx can do the nasty work
   -- too lazy to implement this in Scheme at the moment --
*)

(* handle curried type functions *)
let mktypefun sr name vs args return_type body =
  let argtyp t = match t with
    | [] -> failwith "Lambda abstraction requires nonunit parameter"
    | [x] -> x
    | x -> KND_tuple x
  in
  let body =
    let p = ref (List.rev args) in
    let r = ref return_type in
    let b = ref body in
    while !p <> [] do
      let arg = List.hd !p in
      p := List.tl !p;
      b := TYP_typefun (arg, !r, !b);
      r := KND_function(argtyp (List.map snd (arg)),!r)
    done;
    !b
  in
  STMT_type_alias
  (
    sr,
    name,
    vs,
    body
  )


let sye { Flx_name_map.base_sym=i } = i

exception UnificationError of Flx_btype.t * Flx_btype.t

(* unbound type *)
let type_of_argtypes ls = match ls with
 | [x] -> x
 | _ -> TYP_tuple ls

let funparamtype (_,_,t,_) = t

module FuntypeSet = Set.Make(
  struct type t = typecode_t let compare = compare end
)

module FunInstSet = Set.Make(
  struct
    type t = bid_t * Flx_btype.t list
    let compare = compare
  end
)

let flx_unit = TYP_tuple []
let flx_bool = TYP_unitsum 2


let flx_bbool = Flx_btype.btyp_unitsum 2

