open Flx_ast
open Flx_types
open Flx_util
open List
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

let cal_display syms bbdfns parent : (bid_t *int) list =
  let rec aux parent display =
    match parent with
    | None -> rev display
    | Some parent ->
    match
      try Some (Hashtbl.find bbdfns parent)
      with Not_found ->  None
    with
    | Some (_,parent',sr,BBDCL_procedure (_,vs,_,_))
    | Some (_,parent',sr,BBDCL_function (_,vs,_,_,_))
      -> aux parent' ((parent,length vs)::display)

    (* typeclasses have to be treated 'as if' top level *)
    (* MAY NEED REVISION! *)
    | Some (_,parent',sr,BBDCL_typeclass _ ) -> rev display
    | None ->
      begin
        try
          match Hashtbl.find syms.dfns parent with
          (* instances have to be top level *)
          | {id=id; symdef=`SYMDEF_instance _} -> rev display
          | {id=id; symdef=`SYMDEF_typeclass } -> rev display

          | {id=id} ->
            failwith ("[cal_display] Can't find index(1) " ^id^"<"^ si parent^">")

        with Not_found ->
          failwith ("[cal_display] Can't find index(2) " ^ si parent)
      end

    | _ -> assert false
  in aux parent []

(* inner most at head of list *)
let get_display_list syms bbdfns index : (bid_t * int) list =
  tl (cal_display syms bbdfns (Some index))

let strd the_display props =
  if length the_display = 0 then
  (if mem `Requires_ptf props then "(FLX_FPAR_PASS_ONLY)" else "()")
  else
  (if mem `Requires_ptf props then "(FLX_FPAR_PASS " else "(") ^ cat ", " the_display ^ ")"
