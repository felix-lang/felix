open Flx_name_map
open Flx_btype
open Flx_btype_subst
open Flx_srcref

let lookup_name_in_htab htab name : entry_set_t option =
  try Some (Hashtbl.find htab name)
  with Not_found -> None

let rec mk_bare_env (sym_table:Flx_sym_table.t) index =
  let parent, sym = Flx_sym_table.find_with_parent sym_table index in
  (index, sym.Flx_sym.id, sym.Flx_sym.privmap, [], Flx_ast.TYP_tuple []) ::
  match parent with
  | None -> []
  | Some index -> mk_bare_env sym_table index

let split_dirs open_excludes dirs :
    (Flx_types.ivs_list_t * Flx_ast.qualified_name_t) list *
    (Flx_types.ivs_list_t * Flx_ast.qualified_name_t) list *
    (string * Flx_ast.qualified_name_t) list
=
  let opens =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | Flx_types.DIR_open (vs,qn) -> if List.mem (vs,qn) open_excludes then [] else [vs,qn]
         | Flx_types.DIR_inject_module _  -> []
         | Flx_types.DIR_use _ -> []
       )
       dirs
     )
  and includes =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | Flx_types.DIR_open _-> []
         | Flx_types.DIR_inject_module (vs,qn) -> if List.mem (vs,qn) open_excludes then [] else [vs,qn]
         | Flx_types.DIR_use _ -> []
       )
       dirs
     )
  and uses =
     List.concat
     (
       List.map
       (fun (sr,x) -> match x with
         | Flx_types.DIR_open _-> []
         | Flx_types.DIR_inject_module _ -> []
         | Flx_types.DIR_use (n,qn) -> [n,qn]
       )
       dirs
     )
  in opens, includes, uses


let merge_functions
  (opens: entry_set_t list)
  name
: entry_kind_t list =
(*
if name = "ff" then print_endline "Merging 'ff'"; 
*)
  let result =
  List.fold_left
    (fun init x -> match x with
    | FunctionEntry ls ->
(*
if name = "ff" then print_endline "Function set ..";
*)
      List.fold_left
      (fun init ({base_sym=bid; spec_vs=vs; sub_ts=ts} as x) ->
(*
if name = "ff" then print_endline ("Merging view " ^ string_of_entry_kind x^ " len vs = " ^ string_of_int (List.length vs) ^ ", len ts = " ^ string_of_int (List.length ts));
*)
        if List.mem x init then 
          begin (* if name = "ff" then print_endline "Dup"; *) init end 
        else 
          begin (* if name = "ff" then print_endline "new";*) x :: init end
      )
      init ls
    | NonFunctionEntry x ->
      failwith
      ("[merge_functions] Expected " ^
        name ^ " to be function overload set in all open modules, got non-function:\n" ^
        Flx_print.string_of_entry_kind x
      )
    )
  []
  opens
  in
(*
  if name = "ff" then print_endline ("Merged list length=" ^ string_of_int (List.length opens));
*)
  result


(* compares entry kinds, with alpha conversion of type subscripts *)
let eq_entry_kinds y1 y2 =
  match y1, y2 with
    ({base_sym=bid1; spec_vs=vs1; sub_ts=ts1} as y1),
    ({base_sym=bid2; spec_vs=vs2; sub_ts=ts2} as y2)
   ->
   if bid2 <> bid2 then false else
   begin 
     assert (List.length ts1 = List.length ts2); (* got to be the same! *)
     if List.length vs1 <> List.length vs2 then false else
     let nvs = List.length vs1 in
     (* just hope these variables aren't used, since they're low indices this should be safe *)
     let nuvs = List.map (fun i -> btyp_type_var (i, btyp_type 0)) (Flx_list.nlist nvs) in
     let sr = dummy_sr in 
     let ts1 = List.map (fun t-> tsubst sr vs1 nuvs t) ts1 in
     let ts2 = List.map (fun t-> tsubst sr vs2 nuvs t) ts2 in
     (* OK, a bit hacky! should use type_eq but that requires counter and bsym_table *)
     (btyp_type_tuple ts1) = (btyp_type_tuple ts2)
   end

let lookup_name_in_table_dirs table dirs sr name : entry_set_t option =
(*
if name = "hhhhh" then print_endline ("Lookup name in table dirs " ^ name);
*)
  match lookup_name_in_htab table name with
  | Some x as y -> 
(*
    if name = "ff" then print_endline ("found core entry for ff");
*)
    y
  | None ->
(*
      if name = "ff" then print_endline ("Did not find core entry for ff, searching dirs .. ");
*)
      let opens = List.concat (
        List.map begin fun table ->
          match lookup_name_in_htab table name with
          | Some x -> [x]
          | None -> []
        end dirs)
      in
(*
      if name = "ff" then print_endline ("Found " ^ string_of_int (List.length opens) ^ " entries for ff in dirs");
*)
      match opens with
      | [x] -> 
        Some x
      | FunctionEntry ls :: rest ->
          Some (FunctionEntry (merge_functions opens name))

      | (NonFunctionEntry (i)) as some ::_ ->
          if
            List.fold_left begin fun t -> function
              | NonFunctionEntry (j) when eq_entry_kinds i j -> t
              | _ -> false
            end true opens
          then
            Some some
          else begin
            List.iter begin fun es ->
              print_endline ("[lookup_name_in_table_dirs] Symbol " ^ (Flx_print.string_of_entry_set es))
            end opens;
            print_endline ("[lookup_name_in_table_dirs] Conflicting nonfunction definitions for "^
              name ^" found in open modules");
            Flx_exceptions.clierrx "[flx_bind/flx_lookup.ml:183: E80] " sr ("[lookup_name_in_table_dirs] Conflicting nonfunction definitions for "^
              name ^" found in open modules"
            )
          end
      | [] -> None



