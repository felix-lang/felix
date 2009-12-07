(** The type of the bound symbol table. *)
type t = (Flx_types.bid_t, Flx_bsym.t) Hashtbl.t

(** Construct a bound symbol table. *)
let create () = Hashtbl.create 97

(** Copies the bound symbol table. *)
let copy = Hashtbl.copy

(** Adds the bound symbol with the index to the symbol table. *)
let add = Hashtbl.replace

(** Returns if the bound index is in the bound symbol table. *)
let mem = Hashtbl.mem

(** Searches the bound symbol table for the given symbol. *)
let find = Hashtbl.find

(** Searches the bound symbol table for the given symbol's id. *)
let find_id bsym_table bid =
  let id,_,_,_ = find bsym_table bid in
  id

(** Searches the bound symbol table for the given symbol's source reference. *)
let find_sr bsym_table bid =
  let _,_,sr,_ = find bsym_table bid in
  sr

(** Searches the bound symbol table for the given symbol's parent. *)
let find_parent bsym_table bid =
  let _,parent,_,_ = find bsym_table bid in
  parent

(** Searches the bound symbol table for the given symbol's bbdcl. *)
let find_bbdcl bsym_table bid =
  let _,_,_,bbdcl = find bsym_table bid in
  bbdcl

let find_bvs bsym_table bid =
  Flx_bsym.get_bvs (find bsym_table bid)

(** Remove a binding from the bound symbol table. *)
let remove = Hashtbl.remove

(** Iterate over all the items in the bound symbol table. *)
let iter = Hashtbl.iter

(** Return if the bound symbol index is an identity function. *)
let is_identity bsym_table bid =
  Flx_bsym.is_identity (find bsym_table bid)

(** Return if the bound symbol index is a val, var, ref, or tmp. *)
let is_variable bsym_table bid =
  Flx_bsym.is_variable (find bsym_table bid)

(** Return if the bound symbol index is a global val or var. *)
let is_global_var bsym_table bid =
  Flx_bsym.is_global_var (find bsym_table bid)

(** Return if the bound symbol index is an identity function. *)
let is_function bsym_table bid =
  Flx_bsym.is_function (find bsym_table bid)

(** Update all the bound function and procedure's bound exes. *)
let update_bexes f bsym_table =
  iter begin fun i (id,parent,sr,bbdcl) ->
    match bbdcl with
    | Flx_types.BBDCL_function (ps, bvs, bpar, bty, bexes) ->
        let bbdcl = Flx_types.BBDCL_function (ps, bvs, bpar, bty, f bexes) in
        add bsym_table i (id,parent,sr,bbdcl)

    | Flx_types.BBDCL_procedure (ps, bvs, bpar, bexes) ->
        let bbdcl = Flx_types.BBDCL_procedure (ps, bvs, bpar, f bexes) in
        add bsym_table i (id,parent,sr,bbdcl)

    | _ -> ()
  end bsym_table
