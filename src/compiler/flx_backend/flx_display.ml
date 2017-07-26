open Flx_types
open Flx_bbdcl
open Flx_util
open Flx_mtypes2
open Flx_bid

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

let cal_display bsym_table parent : (bid_t *int) list =
  let rec aux parent display =
    match parent with
    | None -> List.rev display
    | Some parent ->
        let bsym_parent, bsym =
          try Flx_bsym_table.find_with_parent bsym_table parent
          with Not_found ->
            failwith ("[cal_display] Can't find index(2) " ^
              Flx_print.string_of_bid parent)
        in
        match Flx_bsym.bbdcl bsym with
        | BBDCL_fun (_, vs, _, _, _,_) ->
            aux bsym_parent ((parent, List.length vs)::display)

        (* typeclasses have to be treated 'as if' top level *)
        (* MAY NEED REVISION! *)
        | BBDCL_typeclass _ -> List.rev display
        | BBDCL_instance _ -> List.rev display

        | _ -> assert false
  in aux parent []

(* inner most at head of list *)
let get_display_list bsym_table index : (bid_t * int) list =
  List.tl (cal_display bsym_table (Some index))

