open Flx_types
open Flx_ast
open Flx_mtypes2
open Flx_exceptions
open List
open Flx_util
open Flx_print

type label_map_t =
  (bid_t, (string, bid_t) Hashtbl.t) Hashtbl.t

type label_kind_t = [`Far | `Near | `Unused]

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
        (*
        print_endline ("Label " ^ s);
        *)
        Hashtbl.add labels s (fresh_bid counter)
      | _ -> ()
    )
    exes
  ;
  labels

let update_label_map counter label_map index (_,_,_,entry) =
  (*
  print_endline ("Routine " ^ id ^ "<"^ si index ^">");
  *)
  match entry with
  | BBDCL_function (_,_,_,_,exes) ->
    Hashtbl.add label_map index (get_labels counter exes)
  | BBDCL_procedure (_,_,_,exes) ->
    Hashtbl.add label_map index (get_labels counter exes)
  | _ -> ()

let create_label_map bsym_table counter =
  (*
  print_endline "Creating label map";
  *)
  let label_map = Hashtbl.create 97 in
  Flx_bsym_table.iter (update_label_map counter label_map) bsym_table;
  label_map

let rec find_label bsym_table label_map caller label =
  let labels = Hashtbl.find label_map caller in
  try `Local (Hashtbl.find labels label)
  with Not_found ->
  let id,parent,sr,entry = Flx_bsym_table.find bsym_table caller in
  match entry with
  | BBDCL_function _ -> `Unreachable
  | BBDCL_procedure _ ->
    begin match parent with None -> `Unreachable
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
  let labels = Hashtbl.find label_map proc in
  let lix = Hashtbl.find labels label in
  get_label_kind_from_index usage_map lix


let cal_usage syms sym_table bsym_table label_map caller exes usage =
  iter
  (function
    | BEXE_goto (sr,label)
    | BEXE_ifgoto (sr,_,label) ->
      begin match find_label bsym_table label_map caller label with
      | `Unreachable ->
        syserr sr ("[flx_label] Caller " ^ string_of_bid caller ^
          " Jump to unreachable label " ^ label ^ "\n" ^
          (catmap "\n" (string_of_bexe sym_table bsym_table 2) exes))
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

let update_label_usage syms sym_table bsym_table label_map usage index (_,_,_,entry) =
  match entry with
  | BBDCL_function (_,_,_,_,exes)
  | BBDCL_procedure (_,_,_,exes) ->
    cal_usage syms sym_table bsym_table label_map index exes usage
  | _ -> ()

let create_label_usage syms sym_table bsym_table label_map =
  let usage = Hashtbl.create 97 in
  Flx_bsym_table.iter
    (update_label_usage syms sym_table bsym_table label_map usage)
    bsym_table;
  usage
