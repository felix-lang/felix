open Flx_ast
open Flx_types

exception Invalid_int_of_unitsum

type btpattern_t = {
  pattern: t;

  (* pattern type variables, including 'any' vars *)
  pattern_vars: BidSet.t;

  (* assignments for 'as' vars *)
  assignments : (bid_t * t) list
}

(** general typing *)
and t = 
  | BTYP_hole
  | BTYP_int (* type of a C++ int, so we don't have to look it up *)
  | BTYP_none
  | BTYP_sum of t list
  | BTYP_unitsum of int
  | BTYP_intersect of t list (** intersection type *)
  | BTYP_union of t list (** intersection type *)
  | BTYP_inst of bid_t * t list
  | BTYP_tuple of t list
  | BTYP_array of t * t
  | BTYP_record of (string * t) list
  | BTYP_polyrecord of (string * t) list * t
  | BTYP_variant of (string * t) list
  | BTYP_pointer of t
  | BTYP_function of t * t
  | BTYP_effector of t * t * t
  | BTYP_cfunction of t * t
  | BTYP_void
  | BTYP_label (* type of a label *)
  | BTYP_fix of int * t (* meta type *)
  | BTYP_rev of t

  | BTYP_type of int
  | BTYP_type_tuple of t list
  | BTYP_type_function of (bid_t * t) list * t * t
  | BTYP_type_var of bid_t * t
  | BTYP_type_apply of t * t

  (* type_map is NOT a map over a kind, the argument should
     be a tuple type, and the result is a tuple type
     that is, the argument is NOT a type_tuple (which is 
     list of types) but an actual product type.
  *)
  | BTYP_type_map of t * t
  | BTYP_type_match of t * (btpattern_t * t) list

  | BTYP_tuple_cons of t * t 
  | BTYP_tuple_snoc of t * t 

  (* type sets *)
  | BTYP_type_set of t list (** open union *)
  | BTYP_type_set_union of t list (** open union *)
  | BTYP_type_set_intersection of t list (** open union *)

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

let rec str_of_btype typ = 
  let s t = str_of_btype t in
  let ss ts = String.concat "," (List.map str_of_btype ts) in
  match typ with
  | BTYP_hole -> "BTYP_hole"
  | BTYP_int -> "BTYP_int"
  | BTYP_none -> "BTYP_none"
  | BTYP_sum ts -> "BTYP_sum(" ^ ss ts ^")"
  | BTYP_unitsum n -> string_of_int n
  | BTYP_intersect ts -> "BTYP_intersect(" ^ ss ts ^ ")"
  | BTYP_union ts -> "BTYP_union (" ^ ss ts ^ ")"
  | BTYP_inst (i,ts) -> "BTYP_inst("^string_of_int i^"["^ss ts^"])"
  | BTYP_tuple ts -> "BTYP_tuple(" ^ ss ts ^ ")"
  | BTYP_array (b,x) -> "BTYP_array(" ^ s b ^"," ^s x^")"
  | BTYP_record (ls) -> "BTYP_record("^String.concat "," (List.map (fun (name,t)->name^":"^s t) ls)^")"
  | BTYP_polyrecord (ls,t) -> "BTYP_polyrecord("^String.concat "," (List.map (fun (name,t)->name^":"^s t) ls)^" | "^s t^")"
  | BTYP_variant (ls) -> "BTYP_variant(" ^String.concat " | " (List.map (fun (name,t)->name^" of "^s t) ls)^")"
  | BTYP_pointer t -> "BTYP_pointer("^s t^")"
  | BTYP_function (d,c) -> "BTYP_function(" ^ s d ^ " -> " ^ s c ^")"
  | BTYP_cfunction (d,c) -> "BTYP_cfunction(" ^ s d ^ " --> " ^ s c ^")"
  | BTYP_effector (d,e,c) -> "BTYP_effector(" ^ s d ^ " ->["^s e^"] " ^ s c ^")"
  | BTYP_rev t -> "BTYP_rev("^ s t ^")" 

  | BTYP_void -> "BTYP_void"
  | BTYP_label -> "BTYP_label" (* type of a label *)
  | BTYP_fix (i,t) -> "BTYP_fix("^string_of_int i ^ ":" ^ s t ^")"

  | BTYP_type i -> "BTYP_type("^string_of_int i^")"
  | BTYP_type_tuple ts -> "BTYP_type_tuple(" ^ ss ts ^ ")"
  | BTYP_type_function (ps,r,b) -> "BTYP_type_function((" ^ 
      String.concat "," (List.map (fun (i,t)->string_of_int i^":"^s t) ps)^"):"^
      s r^"=("^ s b ^"))"

  | BTYP_type_var (i,t) -> "BTYP_type_var("^string_of_int i^":"^s t^")"
  | BTYP_type_apply (f,x) -> "BTYP_type_apply("^s f^","^s x^ ")"
  | BTYP_type_match (v,pats) -> "BTYP_type_match(too lazy)"
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

type entry_kind_t = {
  (* the function *)
  base_sym: bid_t;

  (* the type variables of the specialisation *)
  spec_vs: (string * bid_t) list;

  (* types to replace the old type variables expressed in terms of the new
   * ones *)
  sub_ts: t list
}

type entry_set_t =
  | FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t

type name_map_t = (string, entry_set_t) Hashtbl.t
type breqs_t = (Flx_types.bid_t * t list) list

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
    | BTYP_array (a,b) -> uf a; uf b
    | BTYP_function (a,b) -> uf a;uf b
    | BTYP_effector (a,e,b) -> uf a; uf e; uf b
    | BTYP_cfunction (a,b) -> uf a;uf b
    | BTYP_pointer a -> uf a
    | BTYP_fix (i,_) when (-i) = depth -> ()
    | BTYP_fix (i,_) when (-i) > depth -> raise (Free_fixpoint t')
    | BTYP_type_apply (a,b) -> uf a;uf b
    | BTYP_type_map (a,b) -> uf a;uf b
    | BTYP_inst (i,ts) -> List.iter uf ts
    | BTYP_type_function (p,r,b) ->
        uf b
 
    | BTYP_rev t -> uf t
 
    | BTYP_type_match (a,tts) ->
        uf a;
        List.iter (fun (p,x) -> uf x) tts
  
    | _ -> ()
  in try aux 0 t; true with | Free_fixpoint _ -> false

(* -------------------------------------------------------------------------- *)
let btyp_hole = BTYP_hole

let btyp_label () = BTYP_label

(** The none type. Used when we don't know the type yet. *)
let btyp_none () =
  BTYP_none

let btyp_int () = BTYP_int

(** The void type. *)
let btyp_void () =
  BTYP_void

let btyp_unit () = 
  BTYP_tuple []

let btyp_bool () = 
  BTYP_unitsum 2

let btyp_any () =
  BTYP_fix (0, BTYP_type 0)

(** Construct a BTYP_sum type. *)
let btyp_sum ts =
  match ts with 
  | [] -> BTYP_void
  (* | [t] -> t *)
  | _ -> 
   try 
     List.iter (fun t -> if t <> BTYP_tuple [] then raise Not_found) ts;
     BTYP_unitsum (List.length ts)
   with Not_found -> BTYP_sum ts

(** Construct a BTYP_unitsum type. *)
let btyp_unitsum n =
  match n with
  | 0 -> BTYP_void
  | 1 -> BTYP_tuple []
  | _ ->  BTYP_unitsum n

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

let btyp_inst (bid, ts) =
  BTYP_inst (bid, ts)

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

let btyp_tuple_cons head tail = 
  match tail with
  | BTYP_tuple ts -> btyp_tuple (head::ts)
  | _ -> BTYP_tuple_cons (head,tail)

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


(** Construct a BTYP_variant type. *)
let btyp_variant = function
  | [] -> BTYP_void
  | ts ->
      (* Make sure all the elements are sorted by name. *)
      let ts = List.sort compare ts in
      BTYP_variant ts

(** Construct a BTYP_pointer type. *)
let btyp_pointer ts =
  BTYP_pointer ts

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
  BTYP_type i

(** Construct a BTYP_type_tuple type. *)
let btyp_type_tuple ts =
  BTYP_type_tuple ts

(** Construct a BTYP_function type. *)
let btyp_type_function (args, ret, body) =
  BTYP_type_function (args, ret, body)

(** Construct a BTYP_type_var type. *)
let btyp_type_var (bid, t) =
  BTYP_type_var (bid, t)

(** Construct a BTYP_type_apply type. *)
let btyp_type_apply (f, a) =
  BTYP_type_apply (f, a)

let btyp_type_map (f,a) =
  BTYP_type_map (f,a)

(** Construct a BTYP_type_match type. *)
let btyp_type_match (t, ps) =
  BTYP_type_match (t, ps)

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
let unfold msg t =
  let rec aux depth t' =
    let uf t = aux (depth + 1) t in
    match t' with
    | BTYP_sum ls -> btyp_sum (List.map uf ls)
    | BTYP_tuple ls -> btyp_tuple (List.map uf ls)
    | BTYP_record (ls) -> btyp_record (List.map (fun (s,t) -> s,uf t) ls)
    | BTYP_polyrecord (ls,v) -> btyp_polyrecord (List.map (fun (s,t) -> s,uf t) ls) (uf v)
    | BTYP_variant ls -> btyp_variant (List.map (fun (s,t) -> s,uf t) ls)
    | BTYP_array (a,b) -> btyp_array (uf a,uf b)
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
    | BTYP_inst (i,ts) -> btyp_inst (i,List.map uf ts)
    | BTYP_type_function (p,r,b) ->
        btyp_type_function (p,r,uf b)
  
    | BTYP_type_match (a,tts) ->
        let a = uf a in
        (* don't unfold recursions in patterns yet because we don't know what
         * they mean *)
        let tts = List.map (fun (p,x) -> p,uf x) tts in
        btyp_type_match (a,tts)
  
    | _ -> t'
  in aux 0 t

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


(* -------------------------------------------------------------------------- *)

(** Iterate over each bound type and call the function on it. *)
let flat_iter
  ?(f_bid=fun _ -> ())
  ?(f_btype=fun _ -> ())
  btype
=
  match btype with
  | BTYP_hole -> ()
  | BTYP_int -> ()
  | BTYP_label -> ()
  | BTYP_none -> ()
  | BTYP_sum ts -> List.iter f_btype ts
  | BTYP_unitsum k ->
      let unitrep = BTYP_tuple [] in
      for i = 1 to k do f_btype unitrep done
  | BTYP_intersect ts -> List.iter f_btype ts
  | BTYP_union ts -> List.iter f_btype ts
  | BTYP_inst (i,ts) -> f_bid i; List.iter f_btype ts
  | BTYP_tuple ts -> List.iter f_btype ts
  | BTYP_array (t1,t2)->  f_btype t1; f_btype t2
  | BTYP_record (ts) -> List.iter (fun (s,t) -> f_btype t) ts
  | BTYP_polyrecord (ts,v) -> List.iter (fun (s,t) -> f_btype t) ts; f_btype v
  | BTYP_variant ts -> List.iter (fun (s,t) -> f_btype t) ts
  | BTYP_pointer t -> f_btype t
  | BTYP_function (a,b) -> f_btype a; f_btype b
  | BTYP_effector (a,e,b) -> f_btype a; f_btype e; f_btype b
  | BTYP_cfunction (a,b) -> f_btype a; f_btype b
  | BTYP_rev t -> f_btype t
  | BTYP_void -> ()
  | BTYP_fix _ -> ()
  | BTYP_type _ -> ()
  | BTYP_tuple_cons (a,b) -> f_btype a; f_btype b
  | BTYP_tuple_snoc (a,b) -> f_btype a; f_btype b
  | BTYP_type_tuple ts -> List.iter f_btype ts
  | BTYP_type_function (its, a, b) ->
      (* The first argument of [its] is an index, not a bid. *)
      List.iter (fun (_,t) -> f_btype t) its;
      f_btype a;
      f_btype b
  | BTYP_type_var (_,t) ->
      (* The first argument of [BTYP_type_var] is just a unique integer, not a
       * bid. *)
      f_btype t
  | BTYP_type_apply (a,b) -> f_btype a; f_btype b
  | BTYP_type_map (a,b) -> f_btype a; f_btype b
  | BTYP_type_match (t,ps) ->
      f_btype t;
      List.iter begin fun (tp, t) ->
        f_btype tp.pattern;
        List.iter (fun (i, t) -> f_bid i; f_btype t) tp.assignments;
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
let map ?(f_bid=fun i -> i) ?(f_btype=fun t -> t) = function
  | BTYP_hole as x -> x
  | BTYP_int as x -> x
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
  | BTYP_inst (i,ts) -> btyp_inst (f_bid i, List.map f_btype ts)
  | BTYP_tuple ts -> btyp_tuple (List.map f_btype ts)
  | BTYP_array (t1,t2) -> btyp_array (f_btype t1, f_btype t2)
  | BTYP_record (ts) -> btyp_record (List.map (fun (s,t) -> s, f_btype t) ts)
  | BTYP_polyrecord (ts,v) -> btyp_polyrecord (List.map (fun (s,t) -> s, f_btype t) ts) (f_btype v)
  | BTYP_variant ts -> btyp_variant (List.map (fun (s,t) -> s, f_btype t) ts)
  | BTYP_pointer t -> btyp_pointer (f_btype t)
  | BTYP_function (a,b) -> btyp_function (f_btype a, f_btype b)
  | BTYP_effector (a,e,b) -> btyp_effector (f_btype a, f_btype e, f_btype b)
  | BTYP_cfunction (a,b) -> btyp_cfunction (f_btype a, f_btype b)
  | BTYP_rev t -> btyp_rev (f_btype t)
  | BTYP_void as x -> x
  | BTYP_fix _ as x -> x
  | BTYP_tuple_cons (a,b) -> btyp_tuple_cons (f_btype a) (f_btype b)
  | BTYP_tuple_snoc (a,b) -> btyp_tuple_snoc (f_btype a) (f_btype b)
  | BTYP_type _ as x -> x
  | BTYP_type_tuple ts -> btyp_type_tuple (List.map f_btype ts)
  | BTYP_type_function (its, a, b) ->
      btyp_type_function (List.map (fun (i,t) -> f_bid i, f_btype t) its, f_btype a, f_btype b)
  | BTYP_type_var (i,t) -> btyp_type_var (f_bid i, f_btype t)
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

let map_entry fi ft {base_sym=base_sym; spec_vs=spec_vs; sub_ts=sub_ts } =
 {
   base_sym=fi base_sym; 
   spec_vs=List.map (fun (s,i) -> s, fi i) spec_vs; 
   sub_ts=List.map ft sub_ts
 }


let map_name_map fi ft nm =
  let me k = map_entry fi ft k in
  let numap = Hashtbl.create 97 in
  Hashtbl.iter (fun name es -> 
    let es = match es with
    | NonFunctionEntry ek -> NonFunctionEntry (me ek)
    | FunctionEntry eks -> FunctionEntry (List.map me eks)
    in
    Hashtbl.add numap name es
  ) 
  nm;
  numap


