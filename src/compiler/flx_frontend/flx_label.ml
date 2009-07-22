open Flx_types
open Flx_ast
open Flx_mtypes2
open Flx_exceptions
open List
open Flx_util
open Flx_print

type label_map_t =
  (bid_t,(string, int) Hashtbl.t) Hashtbl.t

type label_kind_t = [`Far | `Near | `Unused]

type label_usage_t = (int,label_kind_t) Hashtbl.t

type goto_kind_t =
[
  | `Local of int
  | `Nonlocal of int * int
  | `Unreachable
]

let get_labels bbdfns counter exes =
  let labels = Hashtbl.create 97 in
  List.iter
    (fun exe -> match exe with
      | BEXE_label (_,s) ->
        (*
        print_endline ("Label " ^ s);
        *)
        Hashtbl.add labels s !counter; incr counter
      | _ -> ()
    )
    exes
  ;
  labels

let create_label_map bbdfns counter =
  (*
  print_endline "Creating label map";
  *)
  let label_map = Hashtbl.create 97 in
  Hashtbl.iter
  (fun index (id,parent,sr,entry) ->
    (*
    print_endline ("Routine " ^ id ^ "<"^ si index ^">");
    *)
    match entry with
    | BBDCL_function (_,_,_,_,exes) ->
      Hashtbl.add label_map index (get_labels bbdfns counter exes)
    | BBDCL_procedure (_,_,_,exes) ->
      Hashtbl.add label_map index (get_labels bbdfns counter exes)
    | _ -> ()
  )
  bbdfns
  ;
  label_map


let rec find_label bbdfns label_map caller label =
  let labels = Hashtbl.find label_map caller in
  try `Local (Hashtbl.find labels label)
  with Not_found ->
  let id,parent,sr,entry = Hashtbl.find bbdfns caller in
  match entry with
  | BBDCL_function _ -> `Unreachable
  | BBDCL_procedure _ ->
    begin match parent with None -> `Unreachable
    | Some parent ->
      begin match find_label bbdfns label_map parent label with
      | `Local i -> `Nonlocal (i,parent)
      | x -> x
      end
    end
  | _ -> assert false

let get_label_kind_from_index usage lix =
  try Hashtbl.find usage lix with Not_found -> `Unused

let get_label_kind label_map usage_map proc label =
  let labels = Hashtbl.find label_map proc in
  let lix = Hashtbl.find labels label in
  get_label_kind_from_index usage_map lix


let cal_usage syms bbdfns label_map caller exes usage =
  iter
  (function
    | BEXE_goto (sr,label)
    | BEXE_ifgoto (sr,_,label) ->
      begin match find_label bbdfns label_map caller label with
      | `Unreachable ->
        syserr sr ("[flx_label] Caller "^si caller^" Jump to unreachable label " ^ label ^ "\n" ^
        (catmap "\n" (string_of_bexe syms.dfns bbdfns 2) exes))
      | `Local lix ->
        begin match get_label_kind_from_index usage lix with
        | `Unused -> Hashtbl.replace usage lix `Near
        | `Near | `Far -> ()
        end
      | `Nonlocal (lix,_) ->
        begin match get_label_kind_from_index usage lix with
        | `Unused | `Near -> Hashtbl.replace usage lix `Far
        | `Far -> ()
        end
      end
    | _ -> ()
  )
  exes

let create_label_usage syms bbdfns label_map =
  let usage = Hashtbl.create 97 in
  Hashtbl.iter
  (fun index (id,parent,sr,entry) ->
    match entry with
    | BBDCL_function (_,_,_,_,exes)
    | BBDCL_procedure (_,_,_,exes) ->
      cal_usage syms bbdfns label_map index exes usage
    | _ -> ()
  )
  bbdfns
  ;
  usage
