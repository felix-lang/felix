open Flx_bbdcl
open Flx_mtypes2
open Flx_exceptions
open Flx_print
open Flx_bid
open Flx_btype

let catmap x = Flx_util.catmap x
let si i = string_of_int i

let debug = false 

(* make recursive later *)
let rec remap_virtual_types' syms bsym_table (* tc *) t =
   let f_btype t = remap_virtual_types' syms bsym_table (* tc *) t in
(*
print_endline ("Remap virtual types " ^ Flx_btype.st t);
*)
  let t = Flx_btype.map ~f_btype t in
  match t with
  (* the i here is the index of the virtual type, the ts are the 
     types specialising the type class since currently we don't
     allow a virtual type to be indexed
  *)
  | BTYP_vinst (i,ts,mt) ->
    (* let ts = List.map f_btype ts in *)
    begin
      (* STEP 1: Find parent type class *)
      let parent,bsym = Flx_bsym_table.find_with_parent bsym_table i in
      let maybe_tc =
        begin match Flx_bsym.bbdcl bsym with
        (* The bvs here are the type variables inherited from
          the typeclass instance definition, as in 
          instance[vs] C[inst_ts]
          The inst_ts must match the type class vs, however they
          contain instance vs variables which we have to find.
        *)
        | BBDCL_virtual_type bvs -> 
          if debug then print_endline ("Remap: virtual type! " ^ Flx_bsym.id bsym ^ 
           "<" ^si i ^ ">["^
           catmap "," (fun (s,i,_) -> s ^ "<" ^ si i ^ ">") bvs^"]["^
           catmap "," (sbt bsym_table) ts^"]");
          begin match parent with
          | None -> if debug then print_endline ("Remap: no parent, can't be virtual type"); None
          | Some tc' ->
            let pbsym = Flx_bsym_table.find bsym_table tc' in
            begin match Flx_bsym.bbdcl pbsym with
            | BBDCL_typeclass _ -> 
              if debug then print_endline ("Remap: Ok, found type class " ^ 
                Flx_bsym.id pbsym ^"<" ^ si tc' ^">");
              Some tc'
            | _ -> 
              print_endline ("Remap: Wooops, virtual type doesn't have type class parent");
              assert false
            end
          end (* parent *)
        | _ -> None
        end
      in
(*
      let tckid = match parent with | Some tc' -> tc = tc' | None -> false in
      if tckid then begin
*)
      begin match maybe_tc with
      | None -> t
      | Some tc ->
        if debug then print_endline ("Remap Found type class kid type " ^ Flx_bsym.id bsym ^ "<" ^si i ^ ">" ^
           "["^catmap "," (sbt bsym_table) ts ^"]" );

      (* STEP2: grab all the instances of the virtual 
         this is a list:

         inst_vs,inst_constraint,inst_ts, inst_idx 

         The inst_vs should agree with the virtual type bvs.
         We're ignoring constraints for now.
         The inst_ts corresponds to the instance's ts corresponding
         to the type class vs. However the ts for the virtual type binding
         must be at least as specialised as the instance ts.

         TO find the correct instance, we try to unify the inst_ts
         of the instance as the parameter, with the ts of the BTYP_vinst
         as the argument, which should produce an MGU with bindings
         for the inst_vs.
      
         If more than one instance matches, we choose the most specialised
         based on the respective inst_vs, if no most specialised instance
         matches we barf. This is the standard algorithm. It's a pity
         we have to keep writing it out! 
 
      *)
        let entries = try Hashtbl.find syms.virtual_to_instances i with Not_found -> [] in
        if debug then print_endline ("instances of virtual type are:");
        if debug then List.iter (fun (inst_vs, inst_constraint, inst_ts, inst_idx)  ->
          print_endline ("     .. " ^ si inst_idx ^ 
          " inst_vs=" ^ catmap "," (fun (s,vidx,_) -> s ^ "<" ^ si vidx ^ ">") inst_vs ^
          " inst_ts=" ^ catmap "," (sbt bsym_table) inst_ts);
        )
        entries;
        let entries = 
          List.fold_left (fun acc ((inst_vs,inst_constraint,inst_ts, inst_idx) as entry) -> 
            if List.length inst_ts <> List.length ts  then 
            begin 
if debug then print_endline ("n_inst_ts <> n_ts");
              acc 
            end else 
            begin
              if debug then
                print_endline ("Unifying to find inst_vs");
              if debug then
                print_endline ("     .. " ^ si inst_idx ^ 
                " inst_vs=" ^ catmap "," (fun (s,vidx,_) -> s ^ "<" ^ si vidx ^ ">") inst_vs ^
                " inst_ts=" ^ catmap "," (sbt bsym_table) inst_ts ^ 
                " ts = " ^ catmap "," (sbt bsym_table) ts  
              );


              let eqns = List.combine inst_ts ts in
              let depvars = BidSet.of_list (List.map (fun (_,i,_)->i) inst_vs) in
              let maybe_mgu = try Some (Flx_unify.unification bsym_table syms.counter eqns depvars) with Not_found -> None in
              match maybe_mgu with
              | None -> 
                if debug then print_endline ("         ... no solution");
                acc
              | Some mgu -> 
                if debug then
                  print_endline ("Found inst_vs substitutions mgu=" ^ catmap ",  " 
                  (fun (i,t) -> si i ^ " <-- " ^ sbt bsym_table t) mgu);
                (mgu,entry)::acc
            end
          )
          []
          entries 
        in 
        let mgu,(inst_vs, inst_constraint, inst_ts, inst_idx) =
          match entries with
          | [entry] -> entry
          | [] -> 
            print_endline ("Woops, didn't find any instance of type class virtual type"); 
            assert false
          | _ -> 
            print_endline ("Woops, found too many instances of type class virtual type"); 
            List.iter (fun (mgu,(inst_vs, inst_constraint, inst_ts, inst_idx))  ->
              print_endline ("     .. " ^ si inst_idx ^ 
              " inst_vs=" ^ catmap "," (fun (s,vidx,_) -> s ^ "<" ^ si vidx ^ ">") inst_vs ^
             " inst_ts=" ^ catmap "," (sbt bsym_table) inst_ts);
            )
            entries;
            print_endline ("Select one with inst_ts = " ^ "["^catmap "," (sbt bsym_table) ts ^"]" );

            assert false
        in
        assert (List.length inst_ts = List.length ts);

        let inst_bsym = Flx_bsym_table.find bsym_table inst_idx in
        begin match Flx_bsym.bbdcl inst_bsym with
        | BBDCL_instance_type (bvs,repr) ->
          if debug then print_endline ("Found instance type, repr= " ^ sbt bsym_table repr);
          if debug then print_endline ("  instance type bvs = " ^catmap "," (fun (s,vidx,_) -> s ^ "<"^si vidx^">") bvs);
          if debug then print_endline ("Length bvs = " ^ si (List.length bvs) ^ 
             " length inst_ts = " ^ si (List.length inst_ts));
(*
          assert (List.length bvs = List.length inst_ts);
*)
(* SEE THE CALCULATION FOR THE SUBSTITUTION REQUIRED IN sigmatch *)
          let varmap = Flx_btype_subst.varmap_of_mgu mgu in
          let repr = Flx_btype_subst.varmap_subst varmap repr in
          (* let repr = Flx_tuplecons.normalise_tuple_cons bsym_table repr in *)
          if debug then print_endline ("Substituted type, repr= " ^ sbt bsym_table repr);
          repr
        | _ -> 
          print_endline ("Woops, expected to find instance type for type class virtual type");
          assert false
        end (* instance_type *)
      end (* maybe tc *)
    end (* nominal type *)

   | _ -> t (* Flx_btype.map ~f_btype t *)

let remap_virtual_types syms bsym_table (* tc *) t =
  if debug then print_endline ("---------------------");
  if debug then print_endline ("rvt input =" ^ sbt bsym_table t);
  let r =  remap_virtual_types' syms bsym_table (* tc *) t in
  if debug then print_endline ("rvt result=" ^ sbt bsym_table r);
  r

