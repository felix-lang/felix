open Flx_ast
open Flx_types
open Flx_typing2


(* a hack *)
exception OverloadKindError of Flx_srcref.t * string


let get_pnames_and_unbound_dflts ps : (string * expr_t option) list =
  List.map 
    begin fun p -> match p with
    | Flx_ast.Satom (sr,_,name,_,d) -> name,d
    | Flx_ast.Slist _ -> raise Not_found (* can't allow nested param tuples *)
    end 
    ps


let sig_of_symdef symdef sr name i : typecode_t * typecode_t * ((string * expr_t option) list) option = 
  match symdef with
  (* primitives *)
  | SYMDEF_fun (_,ps,r,_,_,_)
    -> type_of_list ps,r,None

  | SYMDEF_callback (_,ts_orig,r,_)
    ->
      let ts_f =
        List.filter
        (function
          | `TYP_name (_,id,[]) when id = name -> false
          | t -> true
        )
        ts_orig
      in
      let tf_args = match ts_f with
        | [x] -> x
        | lst -> `TYP_tuple lst
      in
      let tf = `TYP_function (tf_args, r) in

      (* The type of the arguments Felix thinks the raw
         C function has on a call. A closure of this
         function is a Felix function .. NOT the raw
         C function.
      *)
      let ts_cf =
        List.map
        (function
          | `TYP_name (_,id,[]) when id = name -> tf
          | t -> t
        )
        ts_orig
      in
      type_of_list ts_cf,r,None

  | SYMDEF_function (ps,r,effects,_,_) ->
    let p = fst ps in
    let paramlist = match p with
    | Satom _ -> None
    | Slist ps -> try Some (get_pnames_and_unbound_dflts ps) with Not_found -> None
    in
    typeof_paramspec_t p,r,paramlist

  | SYMDEF_cstruct (ls, _) ->
    type_of_list (List.map snd ls), `TYP_index (sr,name,i),
     Some (List.map (fun (p,_) -> p,None) ls)

  | SYMDEF_struct ls ->
    type_of_list (List.map snd ls), `TYP_index (sr,name,i),
     Some (List.map (fun (p,_) -> p,None) ls)

  | SYMDEF_const_ctor (_,r,_,_) -> `TYP_void sr,r,None
  | SYMDEF_nonconst_ctor (_,r,_,_,t) -> t,r,None
  | SYMDEF_type_alias t ->
(*
    print_endline ("[sig_of_symdef] Found a typedef " ^ name);
*)
    begin match t with
    | `TYP_typefun (ps,r,b) ->
      print_endline "`TYP_typefun";
      assert false;
(*
      kind_of_list (List.map snd ps),r,None
*)
    | symdef ->
      (*
      print_endline "OverloadKindError";
      *)
      raise (OverloadKindError (sr,
        "[sig_of_symdef] Expected "^
        name
        ^" to be a type function, got " ^
        Flx_print.string_of_typecode t
      ))
    end

  | symdef ->
    raise (OverloadKindError (sr,
      "[sig_of_symdef] Expected "^
      name
      ^" to be function or procedure, got " ^
     Flx_print.string_of_symdef symdef name dfltivs
    ))



