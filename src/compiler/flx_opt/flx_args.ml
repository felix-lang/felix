open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_foldvars

let get_ps bsym_table f =
  match Flx_bsym_table.find_bbdcl bsym_table f with
  | BBDCL_function (_,_,(ps,_),_,_)
  | BBDCL_procedure (_,_,(ps,_),_) -> ps
  | _ -> assert false

let unpack syms bsym_table f ps a : tbexpr_t list =
  match ps with
  | [] -> []   (* arg should be unit *)
  | [_] -> [a] (* one param, one arg *)
  | _ ->       (* multiple params *)
  match a with
  | BEXPR_tuple ls,BTYP_tuple ts ->
    assert (length ts = length ps);
    assert (length ls = length ts);
    ls

  | BEXPR_tuple ls,BTYP_array (t,BTYP_unitsum k) ->
    assert (k = length ps);
    assert (length ls = k);
    ls

  | x,BTYP_tuple ts ->
    assert (length ts = length ps);
    let xs = map (fun i -> BEXPR_get_n (i,a)) (nlist (length ts)) in
    combine xs ts

  | x,BTYP_array (t,BTYP_unitsum k) ->
    assert (k = length ps);
    map (fun i -> BEXPR_get_n (i,a),t) (nlist k)

  | x,t ->
    print_endline ("Function " ^ string_of_bid f);
    print_endline ("Unexpected non tuple arg type " ^ sbt bsym_table t);
    print_endline ("Parameters = " ^
      catmap ", " (fun {pid=s;pindex=i} -> s ^ "<" ^ string_of_bid i ^ ">") ps);
    print_endline ("Argument " ^ sbe bsym_table a);
    assert false (* argument isn't a tuple type .. but there are multiple parameters!  *)

let merge_args syms bsym_table f c a b =
  let psf = get_ps bsym_table f in
  let psc = get_ps bsym_table c in
  let args = unpack syms bsym_table f psf a @ unpack syms bsym_table c psc b in
  match args with
  | [x] -> x
  | _ -> BEXPR_tuple args, btyp_tuple (map snd args)

let append_args syms bsym_table f a b =
  let psf = get_ps bsym_table f in
  let args = unpack syms bsym_table f psf a @ b in
  match args with
  | [x] -> x
  | _ -> BEXPR_tuple args, btyp_tuple (map snd args)
