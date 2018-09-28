open Flx_ast
open Flx_types
open Flx_bexe
open Flx_bbdcl
open Flx_mtypes2
open Flx_exceptions
open List
open Flx_util
open Flx_print
open Flx_bsym
open Flx_bid

(* parent -> label index *)
type labels_by_proc_t =
  (bid_t, bid_t list) Hashtbl.t

type label_kind_t = [`Far | `Near | `Unused ]

(* label -> usage *)
type label_usage_t = (bid_t, label_kind_t) Hashtbl.t

type label_info_t = { labels_by_proc: labels_by_proc_t; label_usage: label_usage_t }

let get_labels_for_proc label_info index =
  try Hashtbl.find label_info.labels_by_proc index 
  with Not_found -> []

let get_label_kind_from_index (h:label_usage_t) idx =
  try Hashtbl.find h idx
  with Not_found -> assert false 

type goto_kind_t = [ | `Local | `Nonlocal | `Unreachable ]

let get_label_parent bsym_table idx : bid_t =
  let parent,bsym = Flx_bsym_table.find_with_parent bsym_table idx in
  match parent, bsym.bbdcl with
  | Some p, BBDCL_label _ -> p
  | _ -> assert false

let get_init_label_info bsym_table : label_info_t =
  let pl = Hashtbl.create 97 in
  let lu = Hashtbl.create 97 in
  Flx_bsym_table.iter (fun idx parent bsym ->
    let add p =
      let kids = try Hashtbl.find pl p with Not_found -> [] in
      Hashtbl.replace pl p (idx::kids);
      Hashtbl.add lu idx `Unused
    in
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_label label  ->
      begin match parent with
      (* this should NOT happen! The top level procedure is 0 , all code should belong
         to some parent, even 0. None is only for definitions, including types,
         classes, and variables: the top level procedure refers to variables
         in the thread frame. So:

         Some 0 -> top level _init_ <<--- this is nested in the root
         None -> thread_frame <<--- this is the root 
       *)
      | None ->  print_endline ("Flx_label:get_init_label_info: label " ^ label ^ " Has no parent!");
        assert false 
      | Some p -> add p
      end
    | _ -> ()
  )
  bsym_table
  ;
  { labels_by_proc=pl; label_usage= lu }


(* Previously this test tried to stop jumps through functions.
  Unfortantely a jump which goes through a function *statically*
  is not the same thing as a jump through a function dynamically.
  For example if the function contains a procedure doing a jump to 
  the functions parent, it's fine if the procedure is returned by
  the function as a closure then executed. Although that procedure
  *is* executed in the scope of the function's dead stack frame, 
  the jump does not cross the function call control boundary: 
  the function has returned.

  The check was inadequate anyhow, because said closure can
  be passed out of the procedure containing the variable,
  and executed after the procedure has died .. which is an error,
  since eventually the procedure will try to return a second time.

  This is a bug in the type system really. It should prevent
  re-entering dead procedures by preventing closures containing
  jumps into the procedure from getting outside the scope of the
  procedure.

  Unfortunately the GC only tracks data pointers, not 
  control pointers (a goto is basically a control pointer).
*)

(* find the jump distance of a goto, unreachable should never happen now! *)
let rec find_label_distance bsym_table (pl:labels_by_proc_t) caller (label:bid_t) =
  let labels = try Hashtbl.find pl caller with Not_found -> [] in 
  if List.mem label labels then `Local else
  let bsym_parent, bsym = Flx_bsym_table.find_with_parent bsym_table caller in
  match bsym_parent with
  | None -> assert false (* `Unreachable (* shouldn't happen! *) *)
(*
    (* See above comment! *)
    let parent = 0 in (* HACK! *)
    let result = find_label_distance bsym_table pl parent label in
    begin match result with
    | `Local -> `Nonlocal
    | _ -> result
    end
*)
  | Some parent ->
    let result = find_label_distance bsym_table pl parent label in
    begin match result with
    | `Local -> `Nonlocal
    | _ -> result
    end


let cal_usage bsym_table label_info caller exes  =
(*
print_endline ("Scanning exes of procedure " ^ si caller);
*)
  let handle_label_use force_far sr label =
(*
    print_endline ("goto Label " ^ label ^ " loc= .. ");
*)
    let label_loc = find_label_distance bsym_table label_info.labels_by_proc caller label in
    begin match label_loc with
    | `Unreachable ->
print_endline "Unreachable or not found";
      syserr sr ("[flx_label] Caller " ^ string_of_bid caller ^
        " Jump to unreachable or non-existant label " ^ string_of_int label ^ "\n" ^
        (catmap "\n" (string_of_bexe bsym_table 2) exes))
    | `Local ->
      if force_far then Hashtbl.replace label_info.label_usage label `Far else
(*
print_endline ("Local label " ^ si lix);
*)
      begin match get_label_kind_from_index label_info.label_usage label with
      | `Unused -> Hashtbl.replace label_info.label_usage label `Near
      | `Near | `Far -> ()
      end

    | `Nonlocal  ->
(*
print_endline "Non-Local";
*)
      begin match get_label_kind_from_index label_info.label_usage label with
      | `Unused | `Near -> Hashtbl.replace label_info.label_usage label `Far
      | `Far -> ()
      end
    end
  in

  let check exe = 
    let sr = get_srcref exe in
    let f_label_use i = handle_label_use false sr i in (* direct goto in exe *)
    let f_label i = handle_label_use true sr i in  (* indirect use in expr, assume far *)

    let f_bexpr e = Flx_bexpr.iter ~f_label  e in
    Flx_bexe.iter ~f_label_use ~f_bexpr exe
  in
  iter check exes

let populate_label_usage bsym_table label_info =
  Flx_bsym_table.iter (fun index parent bsym ->
    match bsym.bbdcl with
    | BBDCL_fun (_,_,_,_,_,exes) -> cal_usage bsym_table label_info index exes 
    | _ -> ()
  )
  bsym_table

let create_label_info bsym_table =
  let label_info = get_init_label_info bsym_table in
  populate_label_usage bsym_table label_info;
  label_info

let get_label_name bsym_table index =
  let bsym = Flx_bsym_table.find bsym_table index in
  let id = Flx_bsym.id bsym in
  id ^ "_L"^ string_of_int index

(* the value of the program counter to use for the switch
  to the label: just use the label index
*)
let get_label_pc index = index

(* the value of the frame containing the pc to be used,
  is just the parent index
*)
let get_label_frame bsym_table index =
  let parent = Flx_bsym_table.find_parent bsym_table index in
  match parent with
  | Some p -> p
  | None -> assert false (* see above comment *)




