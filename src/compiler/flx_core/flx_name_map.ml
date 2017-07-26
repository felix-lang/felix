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


