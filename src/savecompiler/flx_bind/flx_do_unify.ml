open Flx_btype_subst
let debug = false

(* returns true if a and b have an mgu,
   and also adds each element of the mgu to
   the varmap if it isn't already present
   this routine is ONLY to be used for
   calculating the return types of functions,
   where we're unifying the type of the
   return statements... probably fails
   for generic functions .. since the two
   kinds of type variables aren't distinguished
   (Fun ret type var is an unknown type, not a
   variable one .. it must be eliminated, but
   type parameters must not be [since they're
   instantiated to multiple values .. ..])
*)
let do_unify counter varmap sym_table bsym_table a b =
  let a' = varmap_subst varmap a in
  let b' = varmap_subst varmap b in
  let eqns = [a',b'] in
  if debug then
  print_endline ("Calling unification " ^ Flx_btype.st a' ^ " ==? " ^ Flx_btype.st b');
  match Flx_unify.maybe_unification bsym_table counter eqns with
  | None ->  (*print_endline ("Unification failed");*) false
  | Some mgu ->
(*
    print_endline "mgu=";
    List.iter
    (fun (i, t) ->
      print_endline (string_of_int i ^ " -> " ^ Flx_print.sbt bsym_table t)
    )
    mgu;
*)
    (* This crud is used to find the return types of
    functions initially marked TYP_none, which really
    means the type is unknown and should be calculated.
    The system binds each TYP_none to a SPECIAL type variable,
    and this code is supposed to store type computed by
    some random unification in a hashtable for such variables.

    The variables are marked as SPECIAL by using the same
    index as the function whose return type is unknown.
    *)
    List.iter begin fun (i, t) ->
      if Hashtbl.mem varmap i
      then begin
        (*
        print_endline "Var already in varmap ..";
        *)
        let t' = Hashtbl.find varmap i in
        if t' <> t then
          failwith ("[do_unify] binding for type variable " ^
            Flx_print.string_of_bid i ^ " is inconsistent\n")
      end else begin
        let sym =
          try Flx_sym_table.find sym_table i with Not_found ->
            failwith ("BUG, flx_unify can't find symbol " ^
              Flx_print.string_of_bid i)
        in
        match sym.Flx_sym.symdef with
        | Flx_types.SYMDEF_function _ ->
if debug then
print_endline ("Adding binding for function " ^ string_of_int i ^ " ret type " ^ Flx_btype.st t);
            Hashtbl.add varmap i t

        (* if it's a declared type variable, leave it alone *)
        | Flx_types.SYMDEF_typevar _ -> ()

        | _ ->
            failwith ("[do_unify] attempt to add non-function return unknown " ^
              "type variable " ^ Flx_print.string_of_bid i ^ ", type " ^
              Flx_print.sbt bsym_table t ^ " to hashtble")
      end
    end mgu;
    true
  

