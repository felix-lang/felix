open Flx_ast
open Flx_types
open Flx_util
open Flx_mtypes2

(* This routine calculates the display a routine with
  a given PARENT requires, which includes that parent
  if it exists.

  The result is list of pairs, each pair consisting
  of the ancestor and its vs subdisplay length,
  with the inner most ancestor towards at head of list,
  in particular the parent is always at the head of
  the list if it is empty, and the most global scope
  is at the end of the list.

  Note this is the reverse? of the order used to actually pass
  the display entries to constructors, which start with
  the thread frame (definitely) and work inwards (I think, maybe?? ..)
  Hmmm .. should check ..
*)

let cal_display syms bsym_table parent : (bid_t *int) list =
  let rec aux parent display =
    match parent with
    | None -> List.rev display
    | Some parent ->
    match
      try Some (Flx_bsym_table.find bsym_table parent)
      with Not_found ->  None
    with
    | Some (_,parent',sr,BBDCL_procedure (_,vs,_,_))
    | Some (_,parent',sr,BBDCL_function (_,vs,_,_,_))
      -> aux parent' ((parent, List.length vs)::display)

    (* typeclasses have to be treated 'as if' top level *)
    (* MAY NEED REVISION! *)
    | Some (_,parent',sr,BBDCL_typeclass _) -> List.rev display
    | Some (_,_,_,BBDCL_instance _) -> List.rev display
    | None ->
      begin
        try
          match Flx_sym_table.find syms.sym_table parent with
          (* instances have to be top level *)
          | { Flx_sym.symdef=SYMDEF_instance _ } -> List.rev display
          | { Flx_sym.symdef=SYMDEF_typeclass } -> List.rev display
          | { Flx_sym.id=id } ->
            failwith ("[cal_display] Can't find index(1) " ^ id ^ "<" ^
              Flx_print.string_of_bid parent ^ ">")

        with Not_found ->
          failwith ("[cal_display] Can't find index(2) " ^
            Flx_print.string_of_bid parent)
      end

    | _ -> assert false
  in aux parent []

(* inner most at head of list *)
let get_display_list syms bsym_table index : (bid_t * int) list =
  List.tl (cal_display syms bsym_table (Some index))
