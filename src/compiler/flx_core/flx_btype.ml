open Flx_ast
open Flx_types
open Flx_bid
open Flx_kind

exception Invalid_int_of_unitsum

type btpattern_t = {
  pattern: t;

  (* pattern type variables, including 'any' vars *)
  pattern_vars: BidSet.t;

  (* assignments for 'as' vars *)
  assignments : (bid_t * t) list
}

and pvpiece_t = [`Ctor of (string * t) | `Base of t]

(** general typing *)
and t = 
  | BTYP_hole
  | BTYP_none
  | BTYP_sum of t list
  | BTYP_unitsum of int
  | BTYP_intersect of t list (** intersection type *)
  | BTYP_union of t list (** intersection type *)
  | BTYP_inst of bid_t * t list * Flx_kind.kind
  | BTYP_vinst of bid_t * t list * Flx_kind.kind
  | BTYP_tuple of t list
  | BTYP_array of t * t
  | BTYP_rptsum of t * t
  | BTYP_record of (string * t) list
  | BTYP_polyrecord of (string * t) list * t
  | BTYP_variant of (string * t) list
  | BTYP_polyvariant of pvpiece_t list

  | BTYP_pointer of t
  | BTYP_rref of t
  | BTYP_wref of t

  (* Compact linear type pointers required two types: the domain
    is the whole compact linear type, and the codomain is the 
    type pointer at
  *)
  | BTYP_cltpointer of t * t
  | BTYP_cltrref of t * t
  | BTYP_cltwref of t * t


  | BTYP_function of t * t
  | BTYP_effector of t * t * t
  | BTYP_cfunction of t * t
  | BTYP_void
  | BTYP_label (* type of a label *)
  | BTYP_fix of int * kind (* meta type *)
  | BTYP_rev of t
  | BTYP_uniq of t (* unique type *)

  | BTYP_type_tuple of t list
  | BTYP_type_function of (bid_t * kind) list * kind * t
  | BTYP_type_var of bid_t * kind
  | BTYP_type_apply of t * t 

  (* type_map is NOT a map over a kind, the argument should
     be a tuple type, and the result is a tuple type
     that is, the argument is NOT a type_tuple (which is 
     list of types) but an actual product type.
  *)
  | BTYP_type_map of t * t
  | BTYP_type_match of t * (btpattern_t * t) list
  | BTYP_subtype_match of t * (btpattern_t * t) list

  | BTYP_tuple_cons of t * t 
  | BTYP_tuple_snoc of t * t 

  (* type sets *)
  | BTYP_type_set of t list (** open union *)
  | BTYP_type_set_union of t list (** open union *)
  | BTYP_type_set_intersection of t list (** open union *)

  (* the int is the binding context *)
  (* used during typedef binding process transiently *)
  | BTYP_typeof of int * Flx_ast.expr_t

type overload_result =
 bid_t *  (* index of function *)
 t * (* type of function signature *)
 t * (* type of function return *)
 (bid_t * t) list * (* mgu *)
 t list (* ts *)

let rec trivorder t = match t with
  | BTYP_tuple [] -> Some 0
  | BTYP_pointer t -> 
    begin match trivorder t with
    | Some k -> Some (k + 1)
    | None -> None
    end
  | _ -> None

let rec istriv t =
  match trivorder t with
  | None -> false
  | Some _ -> true

let rec trivtype i = match i with
  | 0 -> BTYP_tuple []
  | _ -> BTYP_pointer (trivtype (i - 1))
  
let catmap sep f ls = String.concat sep (List.map f ls) 

let rec str_of_pvpiece k = match k with
  | `Ctor (s,t) -> "`" ^ s ^ " of " ^ str_of_btype t
  | `Base t -> str_of_btype t

and str_of_btype typ = 
  let s t = str_of_btype t in
  let ss ts = String.concat "," (List.map str_of_btype ts) in
  match typ with
  | BTYP_typeof (i,t) -> "BTYP_typeof(" ^string_of_int i ^ " unrepresentable)"

  | BTYP_hole -> "BTYP_hole"
  | BTYP_none -> "BTYP_none"
  | BTYP_sum ts -> "BTYP_sum(" ^ ss ts ^")"
  | BTYP_unitsum n -> string_of_int n
  | BTYP_intersect ts -> "BTYP_intersect(" ^ ss ts ^ ")"
  | BTYP_union ts -> "BTYP_union (" ^ ss ts ^ ")"
  | BTYP_inst (i,ts,mt) -> "BTYP_inst("^string_of_int i^"["^ss ts^"]:"^Flx_kind.sk mt^")"
  | BTYP_vinst (i,ts,mt) -> "BTYP_vinst("^string_of_int i^"["^ss ts^"]:"^Flx_kind.sk mt^")"
  | BTYP_tuple ts -> "BTYP_tuple(" ^ ss ts ^ ")"
  | BTYP_array (b,x) -> "BTYP_array(" ^ s b ^"," ^s x^")"
  | BTYP_rptsum (b,x) -> "BTYP_rptsum(" ^ s b ^"," ^s x^")"
  | BTYP_record (ls) -> "BTYP_record("^String.concat "," (List.map (fun (name,t)->name^":"^s t) ls)^")"
  | BTYP_polyrecord (ls,t) -> "BTYP_polyrecord("^String.concat "," (List.map (fun (name,t)->name^":"^s t) ls)^" | "^s t^")"
  | BTYP_variant (ls) -> "BTYP_variant(" ^String.concat " | " (List.map (fun (name,t)->name^" of "^s t) ls)^")"
  | BTYP_polyvariant ls -> "BTYP_polyvariant(" ^ String.concat " | " (List.map str_of_pvpiece ls) ^ ")"

  | BTYP_pointer t -> "BTYP_pointer("^s t^")"
  | BTYP_rref t -> "BTYP_rref("^s t^")"
  | BTYP_wref t -> "BTYP_wref("^s t^")"

  | BTYP_cltpointer (dt, ct) -> "BTYP_cltpointer("^s dt^", " ^ s ct^")"
  | BTYP_cltrref (dt, ct) -> "BTYP_cltrref("^s dt^", " ^ s ct^")"
  | BTYP_cltwref (dt, ct) -> "BTYP_cltwref("^s dt^", " ^ s ct^")"


  | BTYP_function (d,c) -> "BTYP_function(" ^ s d ^ " -> " ^ s c ^")"
  | BTYP_cfunction (d,c) -> "BTYP_cfunction(" ^ s d ^ " --> " ^ s c ^")"
  | BTYP_effector (d,e,c) -> "BTYP_effector(" ^ s d ^ " ->["^s e^"] " ^ s c ^")"
  | BTYP_rev t -> "BTYP_rev("^ s t ^")" 
  | BTYP_uniq t -> "BTYP_uniq(" ^ s t ^ ")"

  | BTYP_void -> "BTYP_void"
  | BTYP_label -> "BTYP_label" (* type of a label *)
  | BTYP_fix (i,t) -> "BTYP_fix("^string_of_int i ^ ":" ^ sk t ^")"

  | BTYP_type_tuple ts -> "BTYP_type_tuple(" ^ ss ts ^ ")"
  | BTYP_type_function (ps,r,b) -> "BTYP_type_function((" ^ 
      String.concat "," (List.map (fun (i,t)->string_of_int i^":"^sk t) ps)^"):"^
      sk r^"=("^ s b ^"))"

  | BTYP_type_var (i,t) -> "BTYP_type_var("^string_of_int i^":"^sk t^")"
  | BTYP_type_apply (f,x) -> "BTYP_type_apply("^s f^","^s x^ ")"
  | BTYP_type_match (v,pats) -> 
    let sa (i,t) = string_of_int i ^ " <- " ^ s t in
    let sas a = catmap ", " sa a in
    let sbs pvs = BidSet.fold (fun i acc -> (if acc="" then "" else acc ^ "," ) ^ string_of_int i) pvs "" in
    let sp {pattern=pat; pattern_vars=pvs; assignments=a; }  = "forall " ^ sbs pvs ^ ". " ^ s pat ^ " with " ^ sas a in
    let cases = String.concat "\n  | " (List.map (fun (p,t) -> sp p ^ " => " ^ s t) pats) in
    "BTYP_type_match("^s v^",(\n" ^cases ^ "\n))\n"

  | BTYP_subtype_match (v,pats) -> 
    let sa (i,t) = string_of_int i ^ " <- " ^ s t in
    let sas a = catmap ", " sa a in
    let sbs pvs = BidSet.fold (fun i acc -> (if acc="" then "" else acc ^ "," ) ^ string_of_int i) pvs "" in
    let sp {pattern=pat; pattern_vars=pvs; assignments=a; }  = "forall " ^ sbs pvs ^ ". " ^ s pat ^ " with " ^ sas a in
    let cases = String.concat "\n  | " (List.map (fun (p,t) -> sp p ^ " => " ^ s t) pats) in
    "BTYP_subtype_match("^s v^",(\n" ^cases ^ "\n))\n"


  | BTYP_type_map (f,t) -> "BTYP_type_map(" ^ s f ^"," ^s t^")"

  | BTYP_tuple_cons (h,t) -> "BTYP_tuple_cons (" ^ s h ^"**" ^ s t^")"
  | BTYP_tuple_snoc (h,t) -> "BTYP_tuple_snoc (" ^ s h ^"<**>" ^ s t^")"

  (* type sets *)
  | BTYP_type_set ts -> "BTYP_type_set(" ^ss ts^ ")"
  | BTYP_type_set_union ts -> "BTYP_type_set_union("^ ss ts^")"
  | BTYP_type_set_intersection ts -> "BTYP_type_set_intersection(" ^ ss ts ^ ")"


let st t = str_of_btype t
let sts ts = catmap "," st ts

exception Free_fixpoint of t
type breqs_t = (bid_t * t list) list

type biface_t =
  | BIFACE_export_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_cfun of Flx_srcref.t * bid_t * string
  | BIFACE_export_python_fun of Flx_srcref.t * bid_t * string
  | BIFACE_export_type of Flx_srcref.t * t * string
  | BIFACE_export_struct of Flx_srcref.t * bid_t
  | BIFACE_export_union of Flx_srcref.t * bid_t * string
  | BIFACE_export_requirement of Flx_srcref.t * breqs_t

(* -------------------------------------------------------------------------- *)
(* NOTE: this code only works on structural types because the symbol
   table is not available. However this should be good enough, IF we
   assume that the symbol table entry for a BTYP_inst only contains
   complete types. For example, for a struct, the list of field/type
   pairs, we have to be sure the type of each field is complete,
   and doesn't have a free fixpoint trying to refer back to the 
   struct (or worse something containing the struct).
*)

let complete_type t =
  let rec aux depth t' =
    let uf t = aux (depth + 1) t in
    match t' with
    | BTYP_sum ls -> List.iter uf ls
    | BTYP_tuple ls -> List.iter uf ls
    | BTYP_record (ls) -> List.iter (fun (s,t) -> uf t) ls
    | BTYP_polyrecord (ls,v) -> List.iter (fun (s,t) -> uf t) ls; uf v
    | BTYP_variant ls -> (List.iter (fun (s,t) -> uf t) ls)
    | BTYP_polyvariant ls -> (List.iter (fun k -> match k with | `Ctor (s,t) -> uf t | `Base t -> uf t) ls)
    | BTYP_array (a,b) -> uf a; uf b
    | BTYP_rptsum (a,b) -> uf a; uf b
    | BTYP_function (a,b) -> uf a;uf b
    | BTYP_effector (a,e,b) -> uf a; uf e; uf b
    | BTYP_cfunction (a,b) -> uf a;uf b
    | BTYP_pointer a -> uf a
    | BTYP_fix (i,_) when (-i) = depth -> ()
    | BTYP_fix (i,_) when (-i) > depth -> raise (Free_fixpoint t')
    | BTYP_type_apply (a,b) -> uf a;uf b
    | BTYP_type_map (a,b) -> uf a;uf b
    | BTYP_inst (i,ts,mt) -> List.iter uf ts
    | BTYP_vinst (i,ts,mt) -> List.iter uf ts
    | BTYP_type_function (p,r,b) ->
        uf b
 
    | BTYP_rev t -> uf t
    | BTYP_uniq t -> uf t
 
    | BTYP_subtype_match (a,tts)
    | BTYP_type_match (a,tts) ->
        uf a;
        List.iter (fun (p,x) -> uf x) tts
  
    | _ -> ()
  in try aux 0 t; true with | Free_fixpoint _ -> false

(* -------------------------------------------------------------------------- *)

let btyp_typeof (i,e) = BTYP_typeof (i,e)

let btyp_hole = BTYP_hole

let btyp_label () = BTYP_label

(** The none type. Used when we don't know the type yet. *)
let btyp_none () =
  BTYP_none


(** The void type. *)
let btyp_void () =
  BTYP_void

let btyp_unit () = 
  BTYP_tuple []

let btyp_bool () = 
  BTYP_unitsum 2

let btyp_any () =
  BTYP_fix (0, kind_type)

(** Construct a BTYP_unitsum type. *)
let btyp_unitsum n =
  match n with
  | 0 -> BTYP_void
  | 1 -> BTYP_tuple []
  | _ ->  BTYP_unitsum n

let btyp_rptsum (n,t) =
  match n with
  | BTYP_void -> BTYP_void
  | BTYP_tuple [] -> t
  | _ -> match t with
    | BTYP_void  -> BTYP_void
    | BTYP_tuple [] -> n
    | BTYP_unitsum k ->
      begin match n with
      | BTYP_unitsum j -> btyp_unitsum (j * k)
      | _ -> BTYP_rptsum (n,t)
      end
    | _ -> BTYP_rptsum (n,t)
     

(** Construct a BTYP_sum type. *)
let btyp_sum ts =
  match ts with 
  | [] -> BTYP_void
  | [t] -> t 
  | _ ->
  let first = List.hd ts in
  begin try List.iter (fun t -> if t <> first then raise Not_found) ts;
    let n = btyp_unitsum (List.length ts) in
    btyp_rptsum (n,first)
  with Not_found -> BTYP_sum ts
  end


(** Construct a BTYP_intersect type. *)
let btyp_intersect ls =
  let void_t = btyp_void () in
  let any_t = btyp_any () in
  if List.mem void_t ls then void_t
  else let ls = List.filter (fun i -> i <> any_t) ls in
  let ls = Flx_list.uniq_list ls in (* mandatory for type constraints to work *)
  match ls with
  | [] -> any_t
  | [t] -> t
  | ls -> BTYP_intersect ls

(** Construct a BTYP_intersect type. *)
let btyp_union ls =
  let void_t = btyp_void () in
  let any_t = btyp_any () in
  if List.mem any_t ls then any_t 
  else let ls = List.filter (fun i -> i <> void_t ) ls in
  let ls = Flx_list.uniq_list ls in 
  match ls with
  | [] -> void_t
  | [t] -> t
  | ls -> BTYP_union ls

let btyp_inst (bid, ts,mt) =
  BTYP_inst (bid, ts,mt)

let btyp_vinst (bid, ts,mt) =
  BTYP_vinst (bid, ts,mt)


let btyp_int () = btyp_inst (Flx_concordance.flx_int, [], Flx_kind.kind_type)

(** Construct a BTYP_tuple type. *)
let btyp_tuple ts = 
  match ts with
  | [] -> btyp_unit () 
  | [t] -> t
  | (head :: tail) as ts ->
      (* If all the types are the same, reduce the type to a BTYP_array. *)
      try
        List.iter (fun t -> if t <> head then raise Not_found) tail;
        BTYP_array (head, (BTYP_unitsum (List.length ts)))
      with Not_found ->
        BTYP_tuple ts

let btyp_rev t =
  match t with
  | BTYP_tuple ts -> btyp_tuple (List.rev ts)
  | BTYP_array _ -> t
  | _ -> BTYP_rev t

let btyp_uniq t = 
  BTYP_uniq t

let btyp_tuple_snoc body last = 
  match body with
  | BTYP_tuple ts -> btyp_tuple (ts@[last])
  | _ -> BTYP_tuple_snoc (body,last)


(** Construct a BTYP_array type. *)
let btyp_array (t, n) =
  match n with
  | BTYP_void -> BTYP_tuple []
(*
  | BTYP_tuple [] -> t
*)
  | _ -> BTYP_array (t, n)

(** Construct a BTYP_record type. *)
let btyp_record ts = 
   let all_blank = List.fold_left (fun acc (s,_) -> acc && s = "") true ts in
   if all_blank then btyp_tuple (List.map snd ts) else
   let cmp (s1,t1) (s2, t2) = compare s1 s2 in
   let ts = List.stable_sort cmp ts in
   BTYP_record (ts)

(** Construct a BTYP_polyrecord type. *)
let btyp_polyrecord ts v = 
(*
print_endline ("Constructing polyrecord, extensions=" ^ catmap "," (fun (s,t) -> s^":"^str_of_btype t) ts);
print_endline ("   ... core = " ^ st v);
*)
   match ts with [] -> v | _ ->
   match v with
   | BTYP_record flds -> 
     btyp_record (ts @ flds)

   | BTYP_void -> btyp_record ts

   | BTYP_polyrecord (flds,v2) ->
     let cmp (s1,t1) (s2, t2) = compare s1 s2 in
     let fields = List.stable_sort cmp (ts @ flds) in
     BTYP_polyrecord (fields,v2)
   | _ -> 
     let cmp (s1,t1) (s2, t2) = compare s1 s2 in
     let ts = List.stable_sort cmp ts in
     BTYP_polyrecord (ts,v)


(* FIXME: Idiot Ocaml strikes again. We need to minimise t before hashing
but the routine isn't defined yet, factored out and since dependent
on types, had to be defined in a subsequent file, and now cannot
be called. Ocaml compilation model sucks
*)
let vhash ((s,t) as x) = 
(*
  let h = Hashtbl.hash (s,t) in
*)
  let h = Hashtbl.hash s in
(*
  print_endline ("Hashing `"^s ^ " of " ^ st t ^ " to " ^ string_of_int h);
*)
  h

let hash_variants ls = List.map (fun x -> vhash x,x) ls
let find_vdata ls h = List.assoc h (hash_variants ls)
let maybe_find_vdata ls h = try Some (find_vdata ls h) with Not_found -> None
let vfind_argtype ls s = List.assoc s ls
let maybe_vfind_argtype ls s = try Some (vfind_argtype ls s) with Not_found -> None

(** Construct a BTYP_variant type. *)
let btyp_variant ls = 
  match ls with
  | [] -> BTYP_void
  | ts ->
      (* Make sure all the elements are sorted by name. *)
      let cmp (s1,t1) (s2, t2) = compare s1 s2 in
      let ts = List.stable_sort cmp ts in
      BTYP_variant ts


(** Construct a BTYP_pointer type. *)
let btyp_pointer ts =
  BTYP_pointer ts

(** Construct a BTYP_rref type. Pointer to temporary,
rvalue, or just const pointer or something similar *)
let btyp_rref ts =
  BTYP_rref ts

let btyp_wref ts =
  BTYP_wref ts


(** Construct a BTYP_pointer type. *)
let btyp_cltpointer d c =
  BTYP_cltpointer (d,c)

(** Construct a BTYP_rref type. Pointer to temporary,
rvalue, or just const pointer or something similar *)
let btyp_cltrref d c =
  BTYP_cltrref (d,c)

let btyp_cltwref d c =
  BTYP_cltwref (d,c)

(** Construct a BTYP_function type. *)
let btyp_function (args, ret) =
  BTYP_function (args, ret)

(** Construct a BTYP_function type. *)
let btyp_effector (args, effects, ret) =
  match effects with
  | BTYP_tuple [] -> BTYP_function (args,ret)
  | _ -> BTYP_effector (args, effects, ret)


(** Construct a BTYP_cfunction type. *)
let btyp_cfunction (args, ret) =
  BTYP_cfunction (args, ret)

(** Construct a BTYP_fix type. *)
let btyp_fix i mt =
  BTYP_fix (i, mt)

(** Construct a BTYP_type type. *)
let btyp_type i =
  Flx_kind.KIND_type
(*
  BTYP_type i
*)

(** Construct a BTYP_type_tuple type. *)
let btyp_type_tuple ts =
  BTYP_type_tuple ts

(** Construct a BTYP_function type. *)
let btyp_type_function (args, ret, body) =
  BTYP_type_function (args, ret, body)

(** Construct a BTYP_type_var type. *)
let btyp_type_var (bid, t) =
(*
if bid = 7141 then print_endline ("Flx_btype: Binding type variable " ^ string_of_int bid ^
  ", kind=" ^ Flx_kind.sk t);
*)
  BTYP_type_var (bid, t)

(** Construct a BTYP_type_apply type. *)
let btyp_type_apply (f, a) =
(*
print_endline ("Construct type application: " ^ st f ^ " to " ^ st a);
*)
  BTYP_type_apply (f, a)

let btyp_type_map (f,a) =
  BTYP_type_map (f,a)

(** Construct a BTYP_type_match type. *)
let btyp_type_match (t, ps) =
  BTYP_type_match (t, ps)

let btyp_subtype_match (t, ps) =
  BTYP_subtype_match (t, ps)


(** Construct a BTYP_type_set type. *)
let btyp_type_set ts =
  BTYP_type_set ts

(** Construct a BTYP_type_set_union type. *)
let btyp_type_set_union ts =
  BTYP_type_set_union ts

(** Construct a BTYP_type_set_intersection type. *)
let btyp_type_set_intersection ts =
  BTYP_type_set_intersection ts


(* -------------------------------------------------------------------------- *)
(* a HACK *) 

let rec bmt msg mt = match mt with
  | Flx_ast.KND_type -> kind_type 
  | Flx_ast.KND_function (t1,t2) -> kind_function (bmt msg t1, bmt msg t2)
  | Flx_ast.KND_tuple(ts) -> kind_tuple(List.map (bmt msg) ts)
  | _ -> kind_type 
(*
  | Flx_ast.KND_tpattern t -> print_endline ("BMT tpattern fail " ^ msg); assert false
  | Flx_ast.KND_typeset ts -> print_endline ("BMT typeset fail " ^ msg); assert false 
  | Flx_ast.KND_generic -> print_endline ("BMT generic fail " ^ msg); assert false
  | Flx_ast.KND_special s -> print_endline ("BMT special fail " ^ msg); assert false
*)

(* -------------------------------------------------------------------------- *)

(** Returns if the bound type is void. *)
let is_void = function
  | BTYP_void -> true
  | _ -> false

(** Returns if the bound type is unit. *)
let is_unit = function
  | BTYP_tuple [] -> true
  | _ -> false

(** Returns if the bound type list is all void types. *)
let all_voids = List.for_all is_void

(** Returns if the bound type list is all unit types. *)
let all_units = List.for_all is_unit

(** Returns if the bound type is or is equivalent to a BTYP_unitsum. *)
let is_unitsum t = match t with
  | BTYP_void -> true
  | BTYP_tuple [] -> true
  | BTYP_unitsum _ -> true
  | BTYP_sum ts -> all_units ts
  | _ -> false

let rec ipow i j = if j = 0 then 1 else if j = 1 then i else i * ipow i (j - 1)

(** Returns the integer value of the unit sum type. *)
let rec int_of_linear_type bsym_table t = match t with
  | BTYP_void -> 0
  | BTYP_tuple [] -> 1
  | BTYP_unitsum k -> k
  | BTYP_sum [] ->  0
  | BTYP_sum ts ->
    List.fold_left (fun i t -> i + int_of_linear_type bsym_table t) 0 ts
  | BTYP_tuple ts ->
    List.fold_left (fun i t -> i * int_of_linear_type bsym_table t) 1 ts
  | BTYP_array (a,ix) -> 
    let sa = int_of_linear_type bsym_table a in
    ipow sa (int_of_linear_type bsym_table ix)
  | _ -> raise (Invalid_int_of_unitsum)

let islinear_type bsym_table t =
  try ignore( int_of_linear_type bsym_table t ); true 
  with Invalid_int_of_unitsum -> false

let sizeof_linear_type bsym_table t = 
  try int_of_linear_type bsym_table t 
  with Invalid_int_of_unitsum -> assert false

let ncases_of_sum bsym_table t = match t with
  | BTYP_unitsum n -> n
  | BTYP_sum ls -> List.length ls 
  | BTYP_void -> 0
  | _ -> 1

let iscompact_linear_product t =
  match t with
  | BTYP_tuple _ 
  | BTYP_array _ when islinear_type () t -> true
  | _ -> false

(* -------------------------------------------------------------------------- *)

(** Iterate over each bound type and call the function on it. *)
let flat_iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  btype
=
  match btype with
  | BTYP_typeof (i, t) -> f_bid i

  | BTYP_hole -> ()
  | BTYP_label -> ()
  | BTYP_none -> ()
  | BTYP_sum ts -> List.iter f_btype ts
  | BTYP_unitsum k ->
      let unitrep = BTYP_tuple [] in
      for i = 1 to k do f_btype unitrep done
  | BTYP_intersect ts -> List.iter f_btype ts
  | BTYP_union ts -> List.iter f_btype ts
  | BTYP_inst (i,ts,mt) -> f_bid i; List.iter f_btype ts
  | BTYP_vinst (i,ts,mt) -> f_bid i; List.iter f_btype ts
  | BTYP_tuple ts -> List.iter f_btype ts
  | BTYP_rptsum (t1,t2)
  | BTYP_array (t1,t2)->  f_btype t1; f_btype t2
  | BTYP_record (ts) -> List.iter (fun (s,t) -> f_btype t) ts
  | BTYP_polyrecord (ts,v) -> List.iter (fun (s,t) -> f_btype t) ts; f_btype v
  | BTYP_variant ts -> List.iter (fun (s,t) -> f_btype t) ts
  | BTYP_polyvariant ts -> List.iter (fun k -> 
      match k with 
      | `Ctor (s,t) -> f_btype t
      | `Base t -> f_btype t
    ) ts

  | BTYP_pointer t -> f_btype t
  | BTYP_rref t -> f_btype t
  | BTYP_wref t -> f_btype t

  | BTYP_cltpointer (a,b)
  | BTYP_cltrref (a,b)
  | BTYP_cltwref (a,b)

  | BTYP_function (a,b) -> f_btype a; f_btype b
  | BTYP_effector (a,e,b) -> f_btype a; f_btype e; f_btype b
  | BTYP_cfunction (a,b) -> f_btype a; f_btype b
  | BTYP_rev t -> f_btype t
  | BTYP_uniq t -> f_btype t

  | BTYP_void -> ()
  | BTYP_fix _ -> ()
  | BTYP_tuple_cons (a,b) -> f_btype a; f_btype b
  | BTYP_tuple_snoc (a,b) -> f_btype a; f_btype b
  | BTYP_type_tuple ts -> List.iter f_btype ts
  | BTYP_type_function (its, a, b) ->
      (* The first argument of [its] is an index, not a bid. *)
      (* List.iter (fun (_,t) -> f_btype t) its; *)
      f_btype b
  | BTYP_type_var (_,t) -> ()
      (* The first argument of [BTYP_type_var] is just a unique integer, not a
       * bid. *)
  | BTYP_type_apply (a,b) -> f_btype a; f_btype b
  | BTYP_type_map (a,b) -> f_btype a; f_btype b

  | BTYP_subtype_match (t,ps)
  | BTYP_type_match (t,ps) ->
      f_btype t;
      List.iter begin fun (tp, t) ->
        f_btype tp.pattern;
        (* Assignment variables are type variable indices not bids 
        List.iter (fun (i, t) -> f_bid i; f_btype t) tp.assignments;
        *)
        f_btype t
      end ps
  | BTYP_type_set ts -> List.iter f_btype ts
  | BTYP_type_set_union ts -> List.iter f_btype ts
  | BTYP_type_set_intersection ts -> List.iter f_btype ts


(** Recursively iterate over each bound type and call the function on it. *)
let rec iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  btype
=
  f_btype btype;
  let f_btype btype = iter ~f_bid ~f_btype btype in
  flat_iter ~f_bid ~f_btype btype


(** Recursively iterate over each bound type and transform it with the
 * function. *)
let rec map ?(f_bid=fun i -> i) ?(f_btype=fun t -> t) = function
  | BTYP_typeof (i,t) -> btyp_typeof (f_bid i, t)

  | BTYP_hole as x -> x
  | BTYP_label as x -> x
  | BTYP_none as x -> x
  | BTYP_sum ts -> btyp_sum (List.map f_btype ts)
  | BTYP_unitsum k ->
    let mapped_unit = f_btype (BTYP_tuple []) in
    begin match mapped_unit with
    | BTYP_tuple [] -> BTYP_unitsum k
    | _ -> BTYP_sum (Flx_list.repeat mapped_unit k)
    end
  | BTYP_intersect ts -> btyp_intersect (List.map f_btype ts)
  | BTYP_union ts -> btyp_union (List.map f_btype ts)
  | BTYP_inst (i,ts,mt) -> btyp_inst (f_bid i, List.map f_btype ts,mt)
  | BTYP_vinst (i,ts,mt) -> btyp_vinst (f_bid i, List.map f_btype ts,mt)
  | BTYP_tuple ts -> btyp_tuple (List.map f_btype ts)
  | BTYP_array (t1,t2) -> btyp_array (f_btype t1, f_btype t2)
  | BTYP_rptsum (t1,t2) -> btyp_rptsum (f_btype t1, f_btype t2)
  | BTYP_record (ts) -> btyp_record (List.map (fun (s,t) -> s, f_btype t) ts)
  | BTYP_polyrecord (ts,v) -> btyp_polyrecord (List.map (fun (s,t) -> s, f_btype t) ts) (f_btype v)
  | BTYP_variant ts -> btyp_variant (List.map (fun (s,t) -> s, f_btype t) ts)
  | BTYP_polyvariant ts -> btyp_polyvariant (List.map (fun k -> 
      match k with 
      | `Ctor (s,t) -> `Ctor (s, f_btype t) 
      | `Base t -> `Base (f_btype t) 
    ) ts)

  | BTYP_pointer t -> btyp_pointer (f_btype t)
  | BTYP_rref t -> btyp_rref (f_btype t)
  | BTYP_wref t -> btyp_wref (f_btype t)

  | BTYP_cltpointer (d,c) -> btyp_cltpointer (f_btype d) (f_btype c)
  | BTYP_cltrref (d,c) -> btyp_cltrref (f_btype d) (f_btype c)
  | BTYP_cltwref (d,c) -> btyp_cltwref (f_btype d) (f_btype c) 


  | BTYP_function (a,b) -> btyp_function (f_btype a, f_btype b)
  | BTYP_effector (a,e,b) -> btyp_effector (f_btype a, f_btype e, f_btype b)
  | BTYP_cfunction (a,b) -> btyp_cfunction (f_btype a, f_btype b)
  | BTYP_rev t -> btyp_rev (f_btype t)
  | BTYP_uniq t -> btyp_uniq (f_btype t)

  | BTYP_void as x -> x
  | BTYP_fix _ as x -> x
  | BTYP_tuple_cons (a,b) -> btyp_tuple_cons (f_btype a) (f_btype b)
  | BTYP_tuple_snoc (a,b) -> btyp_tuple_snoc (f_btype a) (f_btype b)
  | BTYP_type_tuple ts -> btyp_type_tuple (List.map f_btype ts)
  | BTYP_type_function (its, a, b) ->
      btyp_type_function (List.map (fun (i,t) -> f_bid i,t) its, a, f_btype b)
  | BTYP_type_var (i,t) -> btyp_type_var (f_bid i,t)
  | BTYP_type_apply (a, b) -> btyp_type_apply (f_btype a, f_btype b)
  | BTYP_type_map (a, b) -> btyp_type_map (f_btype a, f_btype b)
  | BTYP_type_match (t,ps) ->
      let ps =
        List.map begin fun (tp, t) ->
          { tp with
            pattern = f_btype tp.pattern;
            assignments = List.map
              (fun (i, t) -> f_bid i, f_btype t)
              tp.assignments },
          f_btype t
        end ps
      in
      btyp_type_match (f_btype t, ps)

  | BTYP_subtype_match (t,ps) ->
      let ps =
        List.map begin fun (tp, t) ->
          { tp with
            pattern = f_btype tp.pattern;
            assignments = List.map
              (fun (i, t) -> f_bid i, f_btype t)
              tp.assignments },
          f_btype t
        end ps
      in
      btyp_subtype_match (f_btype t, ps)

  | BTYP_type_set ts ->
      let g acc elt =
        (* SHOULD USE UNIFICATIION! *)
        let elt = f_btype elt in
        if List.mem elt acc then acc else elt::acc
      in
      let ts = List.rev (List.fold_left g [] ts) in
      if List.length ts = 1 then List.hd ts else
      btyp_type_set ts
  | BTYP_type_set_union ls -> btyp_type_set_union (List.map f_btype ls)
  | BTYP_type_set_intersection ls ->
      btyp_type_set_intersection (List.map f_btype ls)


(* this routine adds 1 to the fixpoint counter of free fixpoints,
thereby allowing a subterm of a type to be lifted up one level
It must ONLY be used when reducing an existing correctly leveled type.
For example consider a tuple type

  BTYP_tuple_cons (H, T) and suppose T is replaced by BTYP_tuple [C;D]

then the reduction rule should be to:

  BTYP_tuple [H; adjust_fixpoint C; adjust_fixpoint D]

because the distance up from C in the term

  BTYP_tuple_cons (H, BTYP_tuple [C;D])

is one less in the reduced term. Of course this assumes the fixpoint
is set in the term BTYP_tuple [C;D] assuming it will be dropped
two levels down by the subtitution: the correction is because it
only got dropped one level.
*)

(* equivalent to widen_fixgap -1 *)
and adjust_fixpoint t =
  let rec adj depth t =
    let fx t = adj (depth + 1) t in
    match map ~f_btype:fx t with
    | BTYP_fix (i, mt) when i + depth < 0 -> btyp_fix (i+1) mt
    | x -> x
  in adj 0 t

and widen_fixgap level t =
  let rec adj depth t =
    let fx t = adj (depth + 1) t in
    match map ~f_btype:fx t with
    | BTYP_fix (i, mt) when i + depth < 0 -> 
      print_endline ("Widening fixgap by " ^ string_of_int level);
      btyp_fix (i-level) mt
    | x -> x
  in adj 0 t

(* tell if a complete type is recursive *)
and is_recursive_type t = 
  let rec ir j t = 
    match t with
    | BTYP_fix (i,_) when i = j -> raise Not_found (* means yes *)
    | _ ->
      let f_btype t = ir (j-1) t in
      flat_iter ~f_btype t
  in try ir 0 t; false with Not_found -> true



and btyp_tuple_cons head tail =
  if is_recursive_type tail then begin
    print_endline ("Flx_btype: btyp_tuple_cons WARNING: recursive type for tail of tuple_cons");
    print_endline ("Type with recursive tail " ^ st tail ^ " cannot ever be reduced");
    print_endline ("Useful only if the head is later eliminated, failing constructor for now");
    assert false
  end;
  match head, tail with 
  | t1, BTYP_tuple ls ->
    let ls = List.map adjust_fixpoint ls in
    let r = btyp_tuple (t1 :: ls) in
    r

  | t1, BTYP_array (t2, BTYP_unitsum n) when t1 = t2 ->
    let r = btyp_array (t1, btyp_unitsum (n+1)) in
    r

  | t1, BTYP_array (t2, BTYP_unitsum n) ->
    assert (n < 50);
    let t2 = adjust_fixpoint t2 in
    let rec arr n ts = match n with 0 -> ts | _ -> arr (n-1) (t2::ts) in
    let ts = arr n [] in
    let r = btyp_tuple (t1 :: ts) in
    r

  | BTYP_array (t2, BTYP_unitsum n),t1 ->
    assert (n < 50);
    let t1 = adjust_fixpoint t1 in
    let rec arr n ts = match n with 0 -> ts | _ -> arr (n-1) (t2::ts) in
    let ts = arr n [] in
    let r = btyp_tuple (ts@[t1]) in
    r

  (* Cons onto another irreducible cons is irreducibe until the inner one is reduced *)
  | _,(BTYP_tuple_cons _ ) -> BTYP_tuple_cons (head,tail)

  (* Irreducible until variable replaced *)
  | _,(BTYP_type_var _ ) -> BTYP_tuple_cons (head,tail)

  (* Irreducible until virtual type replaced *)
  | _,(BTYP_vinst _ ) -> BTYP_tuple_cons (head,tail)

  | _,_ ->
    print_endline ("Second argument of tuple_cons type must be tuple, array, or type variable");
    print_endline ("Got tail=" ^ st tail);
    failwith ("Type constructor error: btyp_tuple_cons(" ^st head ^"," ^ st tail ^ ")")

and btyp_polyvariant ls =
  let rec split ls =
    List.fold_left (fun (ctors, bases) k ->
      match k with 
      | `Ctor x -> x::ctors, bases 
      | `Base (BTYP_variant ts) -> 
        List.map (fun (s,t) -> s, adjust_fixpoint t) ts @ ctors, bases
      | `Base (BTYP_polyvariant ts) -> 
        let ctors', bases' = split ts in
        List.map (fun (s,t) -> s, adjust_fixpoint t) ctors' @ ctors, 
        List.map adjust_fixpoint bases' @ bases
      | `Base x -> ctors, x::bases
      ) 
    ([],[])
    ls
  in
  let ctors, bases = split ls in
  if List.length bases = 0 then
    btyp_variant ctors
  else
    let cmp (s1,t1) (s2, t2) = compare s1 s2 in
    let ctors = List.stable_sort cmp ctors in
    let pieces =
      List.map (fun k -> `Ctor k) ctors @
      List.map (fun k -> `Base k) bases
    in
    BTYP_polyvariant pieces
 

(* -------------------------------------------------------------------------- *)
and unfold msg t =
  let rec aux depth t' =
    let uf t = aux (depth + 1) t in
    match t' with
    | BTYP_sum ls -> btyp_sum (List.map uf ls)
    | BTYP_tuple ls -> btyp_tuple (List.map uf ls)
    | BTYP_record (ls) -> btyp_record (List.map (fun (s,t) -> s,uf t) ls)
    | BTYP_polyrecord (ls,v) -> btyp_polyrecord (List.map (fun (s,t) -> s,uf t) ls) (uf v)
    | BTYP_variant ls -> btyp_variant (List.map (fun (s,t) -> s,uf t) ls)

    | BTYP_polyvariant ls -> btyp_polyvariant (List.map (fun k ->
        match k with
        | `Ctor (s,t) -> `Ctor (s,uf t) 
        | `Base t -> `Base (uf t)
     ) ls)

    | BTYP_array (a,b) -> btyp_array (uf a,uf b)
    | BTYP_rptsum (a,b) -> btyp_rptsum (uf a,uf b)
    | BTYP_function (a,b) -> btyp_function (uf a,uf b)
    | BTYP_effector (a,e, b) -> btyp_effector (uf a,uf e,uf b)
    | BTYP_cfunction (a,b) -> btyp_cfunction (uf a,uf b)
    | BTYP_pointer a -> btyp_pointer (uf a)
    | BTYP_fix (i,_) when (-i) = depth -> t
    | BTYP_fix (i,_) when (-i) > depth -> 
(*
        print_endline (msg ^ ": Warning:unfold free fixpoint"); 
*)
        raise (Free_fixpoint t')
    | BTYP_type_apply (a,b) -> btyp_type_apply (uf a,uf b)
    | BTYP_type_map (a,b) -> btyp_type_map (uf a,uf b)
    | BTYP_inst (i,ts,mt) -> btyp_inst (i,List.map uf ts,mt)
    | BTYP_vinst (i,ts,mt) -> btyp_vinst (i,List.map uf ts,mt)
    | BTYP_type_function (p,r,b) ->
        btyp_type_function (p,r,uf b)
  
    | BTYP_type_match (a,tts) ->
        let a = uf a in
        (* don't unfold recursions in patterns yet because we don't know what
         * they mean *)
        let tts = List.map (fun (p,x) -> p,uf x) tts in
        btyp_type_match (a,tts)

     | BTYP_subtype_match (a,tts) ->
        let a = uf a in
        (* don't unfold recursions in patterns yet because we don't know what
         * they mean *)
        let tts = List.map (fun (p,x) -> p,uf x) tts in
        btyp_subtype_match (a,tts)
 
    | _ -> t'
  in aux 0 t


(* this is a hack, it fails to account for nominal types *)
let contains_uniq t =
  let rec aux t = 
    match t with
    | BTYP_uniq _ -> raise Not_found
    | BTYP_tuple _
    | BTYP_array _
    | BTYP_rptsum _
    | BTYP_record _
    | BTYP_sum _
    | BTYP_variant _
      -> flat_iter ~f_btype:aux t
    | _ -> () (* functions, pointers, etc, are ignored *)
  in
  try aux t; false with Not_found -> true 

