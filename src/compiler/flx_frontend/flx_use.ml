open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexe
open Flx_bparameter
open Flx_bexpr
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_unify
open Flx_maps
open Flx_exceptions


(* These routines find the absolute use closure of a symbol,
in particular they include variables which are initialised
but never used: these routine are intended to be used
to extract all the bound symbol table entries required
to process a set of roots.

Contrast with the 'Flx_call' usage routines, which
find some symbols which are useful, this excludes
types, and it excludes LHS vals and perhaps vars,
which are not used in some expression.

It seems a pity these routines are almost identical
(and the lot gets repeated yet again in the instantiator,
and weakly in the 'useless call eliminator', we hope
to find a better code reuse solution.. for now,
remember to update all three sets of routines when
changing the data structures.

*)

let rec uses_btype used bsym_table count_inits t =
  let f_btype t = uses_btype used bsym_table count_inits t in
 
  (* We only care about inits. *)
  match t with
  | BTYP_inst (i,ts) ->
      uses used bsym_table count_inits i;
      List.iter f_btype ts

  | _ -> Flx_btype.flat_iter ~f_btype t

and uses_bexe used bsym_table count_inits exe =
  let f_bexpr e = uses_bexpr used bsym_table count_inits e in

  match exe,count_inits with
  | BEXE_init (_,i,e),false -> f_bexpr e
  | BEXE_assign (_,lhs,rhs),_ ->
      (* check is a term is a tuple projection of a variable *)
      let rec is_proj e =
        match e with
        | BEXPR_name _,_ -> true
        | BEXPR_get_n (_,e),_ -> is_proj e
        | _ -> false
      in
      if count_inits or not (is_proj lhs)
      then f_bexpr lhs;
      f_bexpr rhs
  | _ ->
      Flx_bexe.iter
        ~f_bid:(uses used bsym_table count_inits)
        ~f_btype:(uses_btype used bsym_table count_inits)
        ~f_bexpr
        exe

and uses_bexpr used bsym_table count_inits ((e,t) as x) =
  Flx_bexpr.iter
    ~f_bid:(uses used bsym_table count_inits)
    ~f_btype:(uses_btype used bsym_table count_inits)
    x

and uses used bsym_table count_inits i =
  if not (BidSet.mem i !used) then begin
    let bbdcl =
      try Some (Flx_bsym_table.find_bbdcl bsym_table i)
      with Not_found -> None
    in
    match bbdcl with
    | Some bbdcl ->
        used := BidSet.add i !used;
        Flx_bbdcl.iter
          ~f_bid:(uses used bsym_table count_inits)
          ~f_btype:(uses_btype used bsym_table count_inits)
          ~f_bexpr:(uses_bexpr used bsym_table count_inits)
          ~f_bexe:(uses_bexe used bsym_table count_inits)
          bbdcl
      
    | None ->
        failwith ("[Flx_use.uses] Cannot find bound defn for <" ^
          string_of_bid i ^ ">")
  end

let find_roots syms bsym_table root bifaces =
  (* make a list of the root and all exported functions,
  add exported types and components thereof into the used
  set now too
  *)
  let roots = ref (BidSet.singleton root) in

  List.iter begin function
  | BIFACE_export_python_fun (_,x,_)
  | BIFACE_export_fun (_,x,_) -> roots := BidSet.add x !roots
  | BIFACE_export_type (_,t,_) -> uses_btype roots bsym_table true t
  end bifaces;

  syms.roots := !roots

let cal_use_closure_for_symbols syms bsym_table bids count_inits =
  let u = ref BidSet.empty in
  let v : BidSet.t = !(syms.roots) in
  let v = ref v in

  let add j =
    if not (BidSet.mem j !u) then begin
      u := BidSet.add j !u;
      uses v bsym_table count_inits j
    end
  in
  let ut t = uses_btype u bsym_table count_inits t in

  List.iter begin fun bid ->
    match Flx_hashtbl.find_opt syms.typeclass_to_instance bid with
    | Some instances ->
        List.iter begin fun (vs, con, st, j) ->
          add bid;
          add j;
          ut con
        end instances
    | None -> ()
  end bids;

  !u

let full_use_closure_for_symbols syms bsym_table bids =
  cal_use_closure_for_symbols syms bsym_table bids true

let cal_use_closure syms bsym_table (count_inits:bool) =
  let u = ref BidSet.empty in
  let v : BidSet.t = !(syms.roots) in
  let v = ref v in

  let add bid =
    if not (BidSet.mem bid !u) then begin
      u:= BidSet.add bid !u;
      uses v bsym_table count_inits bid
    end
  in
  let ut t = uses_btype u bsym_table count_inits t in

  (* Register use of the typeclass instances. *)
  Hashtbl.iter begin fun i entries ->
    add i;
    List.iter begin fun (j, (vs, con, ts)) ->
      add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.instances_of_typeclass;

  (* Register use for the typeclass instance functions. *)
  Hashtbl.iter begin fun i entries ->
    add i;
    List.iter begin fun (vs,con,ts,j) ->
      add j;
      ut con;
      List.iter ut ts
    end entries
  end syms.typeclass_to_instance;

  while not (BidSet.is_empty !v) do
    let bid = BidSet.choose !v in
    v := BidSet.remove bid !v;
    add bid
  done;

  !u

let full_use_closure syms bsym_table =
  cal_use_closure syms bsym_table true

let copy_used syms bsym_table =
  if syms.compiler_options.print_flag then
    print_endline "COPY USED";

  (* Calculate the used symbols. *)
  let bidset = full_use_closure syms bsym_table in

  (* Return a new bsym_table that has only the used symbols. *)
  let new_bsym_table = Flx_bsym_table.create () in

  (* Iterate through the used symbols and copy them to the new table. *)
  let rec aux bid =
    (* Exit early if we've already added the bid. *)
    if Flx_bsym_table.mem new_bsym_table bid then () else begin

      (* Try to add the parent if it's in the use list. *)
      let parent =
        match Flx_bsym_table.find_parent bsym_table bid with
        | None -> None
        | Some parent ->
            (* Only add the parent if we're in the use list. Otherwiser, just
             * turn the symbol into a root. *)
            if Flx_types.BidSet.mem parent bidset then begin
              aux parent;
              Some parent

            end else None
      in

      (* Finally, add the symbol to the root. *)
      Flx_bsym_table.add new_bsym_table parent bid
        (Flx_bsym_table.find bsym_table bid)
    end
  in

  (* Add all the symbols to the new symbol bsym_table. *)
  Flx_types.BidSet.iter aux bidset;

  (* Return the new symbol bsym_table. *)
  new_bsym_table
