open Flx_ast
open Flx_types

(* MOVED FROM PARSER so flx_sex2flx can do the nasty work
   -- too lazy to implement this in Scheme at the moment --
*)

(* handle curried type functions *)
let mktypefun sr (name:string) (vs:vs_list_t) (args: (string * typecode_t) list list) (return_type:typecode_t) (body:typecode_t) : statement_t =
  let argtyp t = match t with
    | [] -> failwith "Lambda abstraction requires nonunit parameter"
    | [x] -> x
    | x -> TYP_type_tuple x
  in
  let body =
    let p = ref (List.rev args) in
    let r = ref return_type in
    let b = ref body in
    while !p <> [] do
      let arg = List.hd !p in
      p := List.tl !p;
      b := TYP_typefun (arg, !r, !b);
      r := TYP_function(argtyp (List.map snd (arg)),!r)
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


let sye {base_sym=i} = i

let all_voids ls =
    List.fold_left
    (fun acc t -> acc && (t = btyp_void))
    true ls

let all_units ls =
    List.fold_left
    (fun acc t -> acc && (t = btyp_tuple []))
    true ls

let is_unitsum (t:btypecode_t) = match t with
  | BTYP_unitsum _ -> true
  | BTYP_sum ls ->  all_units ls
  | _ -> false


let int_of_unitsum t = match t with
  | BTYP_void -> 0
  | BTYP_tuple [] -> 1
  | BTYP_unitsum k -> k
  | BTYP_sum [] ->  0
  | BTYP_sum ls ->
    if all_units ls then List.length ls
    else raise Not_found

  | _ -> raise Not_found

exception UnificationError of btypecode_t * btypecode_t

(* unbound type *)
let type_of_argtypes ls = match ls with
 | [x] -> x
 | _ -> TYP_tuple ls

let funparamtype (_,_,t,_) = t

module FuntypeSet = Set.Make(
  struct type t=typecode_t let compare = compare end
)

module FunInstSet = Set.Make(
  struct
    type t= bid_t * btypecode_t list
    let compare = compare
  end
)

let flx_bool = TYP_unitsum 2
let flx_bbool = btyp_unitsum 2

(* Note floats are equal iff they're textually identical,
   we don't make any assumptions about the target machine FP model.
   OTOH, int comparisons are infinite precision, for the same
   int kind, even if the underlying machine model is not
*)

let cmp_literal (l:literal_t) (l':literal_t) = match l, l' with
  | AST_int (a,b), AST_int (a',b') -> a = a' && Big_int.eq_big_int b b'
  | AST_float (a,b), AST_float (a',b') -> a = a' && b = b'
  | AST_string s, AST_string s' -> s = s'
  | AST_cstring s, AST_cstring s' -> s = s'
  | AST_wstring s, AST_wstring s' -> s = s'
  | AST_ustring s, AST_ustring s' -> s = s'
  | _ -> false
