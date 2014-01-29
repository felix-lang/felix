open Flx_ast
open Flx_types
open Flx_bexe
open Flx_bbdcl
open Flx_mtypes2
open Flx_exceptions
open List
open Flx_util
open Flx_print

type label_map_t =
  (bid_t, (string, bid_t) Hashtbl.t) Hashtbl.t

type label_kind_t = [`Far | `Near | `Unused ]

type label_usage_t = (bid_t, label_kind_t) Hashtbl.t

type goto_kind_t =
[
  | `Local of bid_t
  | `Nonlocal of bid_t * bid_t
  | `Unreachable
]

let get_labels counter exes =
  let labels = Hashtbl.create 97 in
  List.iter
    (fun exe -> match exe with
      | BEXE_label (_,s) ->
        let bid = fresh_bid counter in 
(*
        print_endline ("    Add Label " ^ s ^ " = " ^ si bid);
*)
        Hashtbl.add labels s bid
      | _ -> ()
    )
    exes
  ;
  labels

let update_label_map counter label_map index bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,_,_,_,exes) ->
(*
print_endline ("Create label map for " ^ si index ^ " " ^ Flx_bsym.id bsym);
*)
      Hashtbl.add label_map index (get_labels counter exes)
  | _ -> ()

let create_label_map bsym_table counter =
  (*
  print_endline "Creating label map";
  *)
  let label_map = Hashtbl.create 97 in
  Flx_bsym_table.iter begin fun bid _ bsym ->
    update_label_map counter label_map bid bsym
  end bsym_table;
  label_map

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
let rec find_label bsym_table label_map caller label =
  let labels = try Hashtbl.find label_map caller with Not_found -> 
print_endline ("Cannot find label map for caller " ^ si caller ^ " to find label " ^ label);
    assert false 
  in
  try `Local (Hashtbl.find labels label)
  with Not_found ->
  let bsym_parent, bsym = Flx_bsym_table.find_with_parent bsym_table caller in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,_,_,_,_) -> 
      begin match bsym_parent with
      | None -> `Unreachable
      | Some parent ->
          begin match find_label bsym_table label_map parent label with
          | `Local i -> `Nonlocal (i,parent)
          | x -> x
          end
      end
  | _ -> assert false

let get_label_kind_from_index usage lix =
  try Hashtbl.find usage lix with Not_found -> `Unused

let get_label_kind label_map usage_map proc label =
(*
print_endline ("Get labal kind of " ^ label ^ " in proc " ^ si proc);
*)
  let labels = try Hashtbl.find label_map proc with Not_found -> assert false in
  try
    let lix = Hashtbl.find labels label in
    get_label_kind_from_index usage_map lix
  with 
    Not_found -> assert false

let cal_usage bsym_table label_map caller exes usage =
(*
print_endline ("Scanning exes of procedure " ^ si caller);
*)
  let handle_label_use force_far sr label =
(*
    print_endline ("goto Label " ^ label ^ " loc= .. ");
*)
    let label_loc = find_label bsym_table label_map caller label in
    begin match label_loc with
    | `Unreachable ->
print_endline "Unreachable or not found";
      syserr sr ("[flx_label] Caller " ^ string_of_bid caller ^
        " Jump to unreachable or non-existant label " ^ label ^ "\n" ^
        (catmap "\n" (string_of_bexe bsym_table 2) exes))
    | `Local lix ->
      if force_far then Hashtbl.replace usage lix `Far else
(*
print_endline ("Local label " ^ si lix);
*)
      begin match get_label_kind_from_index usage lix with
      | `Unused -> Hashtbl.replace usage lix `Near
      | `Near | `Far -> ()
      end
    | `Nonlocal (lix,_) ->
(*
print_endline "Non-Local";
*)
      begin match get_label_kind_from_index usage lix with
      | `Unused | `Near -> Hashtbl.replace usage lix `Far
      | `Far -> ()
      end
    end
  in

  let check exe = 
    let sr = get_srcref exe in
    let f_label_use s = handle_label_use false sr s in
    let f_label s = handle_label_use true sr s in
    let f_bexpr e = Flx_bexpr.iter ~f_label e in
    Flx_bexe.iter ~f_label_use ~f_bexpr exe
  in
  iter check exes

let update_label_usage bsym_table label_map usage index bsym =
  match Flx_bsym.bbdcl bsym with
  | BBDCL_fun (_,_,_,_,exes) ->
(*
print_endline ("Update label usage of " ^ si index ^ " " ^ Flx_bsym.id bsym);
*)
      cal_usage bsym_table label_map index exes usage
  | _ -> ()

let create_label_usage bsym_table label_map =
  let usage = Hashtbl.create 97 in

  Flx_bsym_table.iter begin fun bid _ bsym ->
    update_label_usage bsym_table label_map usage bid bsym
  end bsym_table;

  usage


