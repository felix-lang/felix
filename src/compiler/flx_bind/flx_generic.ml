open Flx_types
open Flx_set
open Flx_mtypes2
open Flx_util
open Flx_list
open List
open Flx_exceptions
open Flx_print
open Flx_ast

let hfind msg h k =
  try Flx_sym_table.find h k
  with Not_found ->
    print_endline ("flx_generic Flx_sym_table.find failed " ^ msg);
    raise Not_found

(* Adjustment of type argument lists works much
like the activation record display, so well call
it the type display: it is just a list of all
the type variables bound by upscope quantifiers
(which should be all of them :-)

For a name without any subscripts, a sibling call,
or upscope call is possible, and just takes the head of the
type display corresponding to the call depth.

For a downscope call (eg referencing an element of
a contained module ..) additional type must be given.

However, sibling and upscope calls can also be made
with subscripts, replacing the trailing default
values of the current display.

So: the given subscripts can vary from 0 to the number
of variables at the call level, with the remaining head
variables defaulted from the calling environment, unless
the call depth is deeper in which case the trailing
values must be given

Actually the algorithm is simpler: just get
the default display for the target, and splice
its head with the given subscript list to get a
list the same length, if the target is longer
than the list, otherwise just take the head of the
subscript list -- this can happen when an instantiated
call calls upscope using an unindexed name.
*)

let merge_con
  {raw_type_constraint=con1; raw_typeclass_reqs=rtcr1}
  {raw_type_constraint=con2; raw_typeclass_reqs=rtcr2}
: vs_aux_t =
  let t =
    match con1,con2 with
    | TYP_tuple[],TYP_tuple[] -> TYP_tuple[]
    | TYP_tuple[],b -> b
    | a,TYP_tuple[] -> a
    | TYP_intersect a, TYP_intersect b -> TYP_intersect (a@b)
    | TYP_intersect a, b -> TYP_intersect (a @[b])
    | a,TYP_intersect b -> TYP_intersect (a::b)
    | a,b -> TYP_intersect [a;b]
  and
    rtcr = uniq_list (rtcr1 @ rtcr2)
  in
  { raw_type_constraint=t; raw_typeclass_reqs=rtcr}

let merge_ivs (vs1,con1) (vs2,con2) :ivs_list_t =
  vs1 @ vs2, merge_con con1 con2

(* finds the complete vs list *)
let rec find_vs sym_table bsym_table i : ivs_list_t =
  let { Flx_sym.parent=parent; vs=vs } = hfind "find_vs" sym_table i in
  match parent with
  | Some i -> merge_ivs (find_vs sym_table bsym_table i) vs
  | None -> vs

let rec find_func_vs sym_table bsym_table vs j =
  match hfind "find_func_vs" sym_table j with
  | { Flx_sym.parent=parent; vs=vs'; symdef=SYMDEF_module }
  | { Flx_sym.parent=parent; vs=vs'; symdef=SYMDEF_typeclass } ->
    begin match parent with
    | None ->
      let vs = merge_ivs vs' vs in
      [],fst vs, snd vs
    | Some j -> find_func_vs sym_table bsym_table (merge_ivs vs' vs) j
    end

  | _ ->
    let (vs',con) = find_vs sym_table bsym_table j in
    (* NOTE: the constraints of the parent are dropped
     * because they're automatically satisfied:
     * No merge is done on the constraints.
     * Hope this doesn't screw something up.
     * It will for sure if this routine is ALSO used
     * to get the calling context constraints.
     *
     * This line originally read:
     * vs',fst vs,merge_con con (snd vs)
     *
     *)
    vs',fst vs,snd vs

(* finds the triple pvs,vs,con where vs is the entity
   vs INCLUDING module vs. pvs is the vs of
   the ultimately containing function and its ancestors.
*)
let find_split_vs sym_table bsym_table i =
  match hfind "find_split_vs" sym_table i with
  | { Flx_sym.symdef=SYMDEF_typevar _ } -> [], [], Flx_ast.dfltvs_aux
  | { Flx_sym.parent=parent; vs=vs } ->
  match parent with
  | None -> [],fst vs, snd vs
  | Some j -> find_func_vs sym_table bsym_table vs j

let print_ivs vs =
  catmap ", " (fun (s,i,_) -> s ^ "<" ^ string_of_bid i ^ ">") vs

let adjust_ts sym_table bsym_table sr index ts =
  let pvs,vs,con = find_split_vs sym_table bsym_table index in
  let k = length pvs in
  let m = length vs in
  let n = length ts in
  if n>m then begin
    match hfind "adjust_ts" sym_table index with { Flx_sym.id=id } ->
    clierr sr
    (
      "For "^ id ^ "<" ^ string_of_bid index ^
      "> Too many type subscripts, expected " ^
      si m ^ " got " ^ si n ^
      "=[" ^ catmap "," (sbt bsym_table) ts ^ "]" ^
      "\nparent vs=" ^ print_ivs pvs ^
      "\nvs=" ^ print_ivs vs
    )
  end;
  if n<m then begin
    match hfind "adjust_ts" sym_table index with { Flx_sym.id=id } ->
    clierr sr
    (
      "For " ^ id ^ "<" ^ string_of_bid index ^
      "> [adjust_ts] Not enough type subscripts, expected " ^
      si m ^ " got " ^ si n ^
      "\nparent vs=" ^ print_ivs pvs ^
      "\nvs=" ^ print_ivs vs
    )
  end;

  map (fun (_,i,_) -> btyp_type_var (i,btyp_type 0)) pvs @ ts

let make_params sym_table bsym_table sr i ts =
  let vs,_ = find_vs sym_table bsym_table i in
  let ts = adjust_ts sym_table bsym_table sr i ts in
  assert (length vs = length ts);
  map2 (fun (s,i,_) t -> s,t) vs ts

(* full ts required *)
let make_varmap sym_table bsym_table sr i ts =
  let vs,_ = find_vs sym_table bsym_table i in
  if length ts != length vs then
    print_endline ("[flx_generic:make_varmap] vs/ts mismatch vs=" ^
    catmap "," (fun (s,_,_) -> s) vs ^
    "; ts = " ^ catmap "," (sbt bsym_table) ts)
  ;
  assert (length ts = length vs);
  let vars = map2 (fun (s,i,_) t -> i,t) vs ts in
  hashtable_of_list vars
