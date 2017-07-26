open Flx_ast
open Flx_bid


type entry_kind_t = {
  (* the function *)
  base_sym: bid_t;

  (* the type variables of the specialisation *)
  spec_vs: (string * bid_t) list;

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


(* use fresh variables, but preserve names *)
let mkentry counter_ref (vs:Flx_types.ivs_list_t) i =
  let is = List.map
    (fun _ -> fresh_bid counter_ref)
    (fst vs)
  in
  let ts = List.map2 (fun i (n,_,mt) ->
    let mt = Flx_btype.bmt mt in
    Flx_btype.btyp_type_var (i, mt)) is (fst vs)
  in
  let vs = List.map2 (fun i (n,_,_) -> 
  n,i) is (fst vs) in
  {base_sym=i; spec_vs=vs; sub_ts=ts}

(* specialise an name map entry *)
let review_entry counter_ref name sr vs ts 
  {base_sym=i; spec_vs=vs'; sub_ts=ts'} : entry_kind_t =
   let vs = ref (List.rev vs) in
   let vs',ts =
     let rec aux invs ints outvs outts =
       match invs,ints with
       | h::t,h'::t' -> aux t t' (h::outvs) (h'::outts)
       | h::t,[] ->
         let i = fresh_bid counter_ref in
         let (name,_) = h in
         vs := (name,i)::!vs;
(*
print_endline ("FUDGE: review entry: "^name^"=T<"^string_of_int i^">");
*)
         let h' = Flx_btype.btyp_type_var (i, Flx_btype.btyp_type 0) in
         (*
         let h' = let (_,i) = h in btyp_type_var (i, btyp_type 0) in
         *)
         aux t [] (h::outvs) (h'::outts)
       | [],h::t -> 
         (* NOT seem to happen in practice .. *)
         print_endline ("Extra ts dropped, not enough vs");
         List.rev outvs, List.rev outts
       | [],[] -> List.rev outvs, List.rev outts
     in aux vs' ts [] []
   in
   let vs = List.rev !vs in
   let ts' = List.map (Flx_type_aux.tsubst sr vs' ts) ts' in
   {base_sym=i; spec_vs=vs; sub_ts=ts'}

let review_entry_set counter_ref k v sr vs ts : entry_set_t = 
  match v with
  | NonFunctionEntry i -> 
    NonFunctionEntry (review_entry counter_ref k sr vs ts i)
  | FunctionEntry fs -> 
    FunctionEntry (List.map (review_entry counter_ref k sr vs ts) fs)


