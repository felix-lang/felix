(** The type of the bound symbol table. *)
type t = (
  Flx_types.bid_t,
  Flx_types.bid_t option *  (** The parent of the symbol. *)
  Flx_bsym.t                (** The symbol. *)
) Hashtbl.t

(** Construct a bound symbol table. *)
let create () = Hashtbl.create 97

(** Copies the bound symbol table. *)
let copy = Hashtbl.copy

(** Returns how many items are in the bound symbol table. *)
let length = Hashtbl.length

(** Adds the bound symbol with the index to the symbol table. *)
let add bsym_table parent bid bsym =
  (*
  assert (match parent with None -> true | Some p -> Hashtbl.mem bsym_table p);
  *)
  assert (not (Hashtbl.mem bsym_table bid));
  Hashtbl.replace bsym_table bid (parent, bsym)

(** Adds a root bound symbol with the index to the symbol table. *)
let add_root bsym_table bid bsym =
  assert (not (Hashtbl.mem bsym_table bid));
  Hashtbl.replace bsym_table bid (None, bsym)

(** Adds the bound symbol with the index to the symbol table. *)
let add_child bsym_table parent bid bsym =
  (*
  assert (Hashtbl.mem bsym_table parent);
  *)
  assert (not (Hashtbl.mem bsym_table bid));
  Hashtbl.replace bsym_table bid (Some parent, bsym)

(** Updates a bound symbol in place while preserving the child-parent
 * relationships. *)
let update bsym_table bid bsym =
  let parent, _ = Hashtbl.find bsym_table bid in
  Hashtbl.replace bsym_table bid (parent, bsym)

(** Update a bound symbol's bbdcl in place. *)
let update_bbdcl bsym_table bid bbdcl =
  let parent, bsym = Hashtbl.find bsym_table bid in
  Hashtbl.replace bsym_table bid (parent, Flx_bsym.replace_bbdcl bsym bbdcl)

(** Returns if the bound index is in the bound symbol table. *)
let mem = Hashtbl.mem

(** Searches the bound symbol table for the given symbol. *)
let find bsym_table bid =
  let _, bsym = Hashtbl.find bsym_table bid in
  bsym

(** Searches the bound symbol table for the given symbol's id. *)
let find_id bsym_table bid =
  (find bsym_table bid).Flx_bsym.id

(** Searches the bound symbol table for the given symbol's source reference. *)
let find_sr bsym_table bid =
  (find bsym_table bid).Flx_bsym.sr

(** Searches the bound symbol table for the given symbol's parent. *)
let find_parent bsym_table bid =
  let parent, _ = Hashtbl.find bsym_table bid in
  parent

(** Searches the bound symbol table for the given symbol's bbdcl. *)
let find_bbdcl bsym_table bid =
  (find bsym_table bid).Flx_bsym.bbdcl

(** Searches the bound symbol table for the given symbol's bparams. *)
let find_bparams bsym_table bid =
  Flx_bsym.get_bparams (find bsym_table bid)

(** Searches the bound symbol table for the given symbol's bvs. *)
let find_bvs bsym_table bid =
  Flx_bsym.get_bvs (find bsym_table bid)

(** Remove a binding from the bound symbol table. *)
let remove = Hashtbl.remove

(** Iterate over all the items in the bound symbol table. *)
let iter f bsym_table =
  Hashtbl.iter (fun bid (_, bsym) -> f bid bsym) bsym_table

(** Fold over all the items in the bound symbol table. *)
let fold f bsym_table init =
  Hashtbl.fold (fun bid (_, bsym) init -> f bid bsym init) bsym_table init

(** Return if the bound symbol index is an identity function. *)
let is_identity bsym_table bid =
  Flx_bsym.is_identity (find bsym_table bid)

(** Return if the bound symbol index is a val, var, ref, or tmp. *)
let is_variable bsym_table bid =
  Flx_bsym.is_variable (find bsym_table bid)

(** Return if the bound symbol index is a global val or var. *)
let is_global_var (bsym_table:t) bid =
  match Hashtbl.find bsym_table bid with
  | None, { Flx_bsym.bbdcl=Flx_bbdcl.BBDCL_var _ }
  | None, { Flx_bsym.bbdcl=Flx_bbdcl.BBDCL_val _ } -> true
  | _ -> false

(** Return if the bound symbol index is an identity function. *)
let is_function bsym_table bid =
  Flx_bsym.is_function (find bsym_table bid)

(** Update all the bound function and procedure's bound exes. *)
let update_bexes f bsym_table =
  iter begin fun bid bsym ->
    match bsym.Flx_bsym.bbdcl with
    | Flx_bbdcl.BBDCL_function (ps, bvs, bpar, bty, bexes) ->
        let bbdcl = Flx_bbdcl.bbdcl_function (ps, bvs, bpar, bty, f bexes) in
        update bsym_table bid (Flx_bsym.replace_bbdcl bsym bbdcl)

    | Flx_bbdcl.BBDCL_procedure (ps, bvs, bpar, bexes) ->
        let bbdcl = Flx_bbdcl.bbdcl_procedure (ps, bvs, bpar, f bexes) in
        update bsym_table bid (Flx_bsym.replace_bbdcl bsym bbdcl)

    | _ -> ()
  end bsym_table
