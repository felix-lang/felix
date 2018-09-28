open Flx_ast
open Flx_bid

(* NOTE: at this point in time, the spec_vs do not have an associated
kind, but they should have.

We have to note that terms of BTYP_* are values of KIND_*,
and so the BTYP_* terms are not all KIND_type. For example
type functions are KIND_function (KIND_type, KIND_type).

The means, that the spec_vs SHOULD have a KIND_* for 
each variable, since the substututions to be made
for them should be checked to see if they're the 
right kind.

For example, in class Monad, the class entry should
have KIND_function (KIND_type, KIND_type) as the
kind of its base variable, and so a 1-1 replacement
via the view here should have too.

In turn, this means the KIND_* to be used must be
calculable at the time name map is generated.
*)

type entry_kind_t = {
  (* the function *)
  base_sym: bid_t;

  (* the type variables of the specialisation *)
  spec_vs: Flx_kind.bvs_t;

  (* types to replace the old type variables expressed in terms of the new
   * ones *)
  sub_ts: Flx_btype.t list
}

type entry_set_t =
  | FunctionEntry of entry_kind_t list
  | NonFunctionEntry of entry_kind_t

type name_map_t = (string, entry_set_t) Hashtbl.t

let map_entry fi ft {base_sym=base_sym; spec_vs=spec_vs; sub_ts=sub_ts } =
 {
   base_sym=fi base_sym; 
   spec_vs=List.map (fun (s,i,k) -> s, fi i,k) spec_vs; 
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


(* use fresh variables, but preserve names *)
let mkentry counter_ref (ivs:Flx_types.ivs_list_t) i =
let flag = ref false in
  let is = List.map
    (fun (n,i,mt) -> 
      let j = fresh_bid counter_ref in
(*
if j > 7140 && j < 7150 then flag := true;
*)
     j
    )
    (fst ivs)
  in
  let ts = List.map2 (fun i (n,_,mt) ->
    let k = Flx_btype.bmt "Flx_name_map.mkentry1" mt in
    Flx_btype.btyp_type_var (i, k)) 
    is 
    (fst ivs)
  in
  let vs = List.map2 (fun i (n,_,mt) -> 
    let k = Flx_btype.bmt "Flx_name_map.mkentry2" mt in
    n,i,k) 
    is 
    (fst ivs) 
  in
  if !flag then begin
    print_endline ("Make entry for base = " ^ string_of_int i);
    print_endline ("Base vs=" ^ Flx_util.catmap ","  (fun (n,i,mt) -> n ^ "<" ^ string_of_int i ^">") (fst ivs));
    print_endline ("Sub vs=" ^ Flx_util.catmap ","  (fun (n,i,mt) -> n ^ "<" ^ string_of_int i ^">") vs);
    print_endline ("Sub ts=" ^ Flx_util.catmap ","  Flx_btype.st ts);
  end;
  {base_sym=i; spec_vs=vs; sub_ts=ts}

(* specialise an name map entry *)

(* NOTE: we should check the ts have kinds corresponding to the vs,
  but we can't do that here cause idiot Ocaml gets in the way:
  we would have to lookup the symbol table to check.
*)
let review_entry counter_ref name sr (vs:Flx_kind.bvs_t) ts 
  {base_sym=i; spec_vs=vs'; sub_ts=ts'} : entry_kind_t 
=
   let vs = ref (List.rev vs) in
   let vs',ts =
     let rec aux invs ints outvs outts =
       match invs,ints with
       | h::t,h'::t' -> aux t t' (h::outvs) (h'::outts)
       | h::t,[] ->
         let i = fresh_bid counter_ref in
         let (name,_,mt) = h in
         vs := (name,i,mt)::!vs;
(*
print_endline ("FUDGE: review entry: "^name^"=T<"^string_of_int i^">");
*)
         let h' = Flx_btype.btyp_type_var (i, mt) in
         aux t [] (h::outvs) (h'::outts)
       | [],h::t -> 
         (* NOT seem to happen in practice .. *)
         print_endline ("Flx_name_map.review_entry: Extra ts dropped, not enough vs");
         List.rev outvs, List.rev outts
       | [],[] -> List.rev outvs, List.rev outts
     in aux vs' ts [] []
   in
   let vs = List.rev !vs in
   let ts' = List.map (Flx_btype_subst.tsubst sr vs' ts) ts' in
   {base_sym=i; spec_vs=vs; sub_ts=ts'}

let review_entry_set counter_ref name v sr (vs: Flx_kind.bvs_t) ts : entry_set_t = 
  match v with
  | NonFunctionEntry i -> 
    NonFunctionEntry (review_entry counter_ref name sr vs ts i)
  | FunctionEntry fs -> 
    FunctionEntry (List.map (review_entry counter_ref name sr vs ts) fs)


let make_view_table counter table sr vs ts : name_map_t =
  let h = Hashtbl.create 97 in
  Hashtbl.iter
  (fun k v ->
    let v = review_entry_set counter k v sr vs ts in
    Hashtbl.add h k v
  )
  table
  ;
  h


