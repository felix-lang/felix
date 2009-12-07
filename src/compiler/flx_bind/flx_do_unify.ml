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

   The subtyping rule for lvalues also applies
   here. An lvalue type for a returned expression
   is compatible with a non-value function return.

   The unification algorithm can account for this,
   it requires the LHS = RHS equation to support
   an extra 'lvalue' in the RHS, but not the other
   way around. So the expression type has to be the RHS
   and the declared type the LHS.
*)
let do_unify syms sym_table bsym_table a b =
  let eqns =
    [
      Flx_unify.varmap_subst syms.Flx_mtypes2.varmap a,
      Flx_unify.varmap_subst syms.Flx_mtypes2.varmap b
    ]
  in
  (*
  print_endline "Calling unification";
  *)
  match Flx_unify.maybe_unification syms.Flx_mtypes2.counter eqns with
  | None -> false
  | Some mgu ->
    (*
    print_endline "mgu=";
    List.iter
    (fun (i, t) ->
      print_endline (string_of_int i ^ " -> " ^ string_of_btypecode sym_table t)
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
      if Hashtbl.mem syms.Flx_mtypes2.varmap i
      then begin
        (*
        print_endline "Var already in varmap ..";
        *)
        let t' = Hashtbl.find syms.Flx_mtypes2.varmap i in
        if t' <> t then
          failwith ("[do_unify] binding for type variable " ^
            Flx_print.string_of_bid i ^ " is inconsistent\n")
      end else begin
        match
          begin
            try Flx_sym_table.find sym_table i with Not_found ->
              failwith ("BUG, flx_unify can't find symbol " ^
                Flx_print.string_of_bid i)
          end
        with
        | { Flx_sym.symdef=Flx_types.SYMDEF_function _ } ->
          (*
          print_endline ("Adding variable " ^ string_of_int i ^ " type " ^ string_of_btypecode sym_table t);
          *)
          Hashtbl.add syms.Flx_mtypes2.varmap i t

        (* if it's a declared type variable, leave it alone *)
        | { Flx_sym.symdef=Flx_types.SYMDEF_typevar _ } -> ()

        | _ ->
          failwith ("[do_unify] attempt to add non-function return unknown " ^
            "type variable " ^ Flx_print.string_of_bid i ^ ", type " ^
            Flx_print.sbt bsym_table t ^ " to hashtble")
      end
    end mgu;
    true
