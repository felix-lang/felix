open Flx_btype
open Flx_bexpr
exception Vfound (* for variants, Found already used elsewhere *)

let rec expand_coercion new_bsym_table bsym_table counter remap ((srcx,srct) as srce) dstt =
  if Flx_typeeq.type_eq (Flx_print.sbt bsym_table) counter srct dstt
  then srce
  else
  match srct,dstt with
    | BTYP_variant ls, BTYP_variant rs -> 
(*
print_endline ("Coercion is variant to variant, ignore");

THIS IS NO LONGER CORRECT. UNFORTUNATELY ITS A F'ING MESS TO DO RIGHT.
The problem is we HAVE to coerce the input argument's constructor 
argument value, which actually creates a new variant. To do this
we have to match on all possible cases of the argument's type
to find the one we need, get the value out of it, convert it,
and rebuild the variant with the same constructor name, but
a the new value of the new coerced type. The coercion is covariant
as for records. 

There is ALSO a problem here in that there is no encoding in the
variant of the sequence number of the variant, for variants of 
the same argument type. The type IS encoded at run time by virtue
of the hashcode.

There's one more thing, an optimisation: if we only have width subtyping,
there's no need to do anything at all due to the universal representation.
This also applies to covariant argument width subtyping on variants
recursively.
*)

      (* check for the special case where the argument constructors all
      have the same type as the corresponding parameters
      *)
      begin try 
        List.iter (fun (name, ltyp) ->
          (* find ALL the parameter constructors of the same name
          at least one of them has to have the same type *)
          begin try List.iter (fun (rname, rtyp) ->
            if name = rname 
            &&  Flx_typeeq.type_eq (Flx_print.sbt bsym_table) counter ltyp rtyp  
            then raise Vfound
            ) rs;
            raise Not_found
          with Vfound -> () 
          end
        ) ls; 
        remap srce (* safe, already checked, universal rep *)
      with 
      | Not_found -> 
      (* OK, now lets handle the special case where there's no choice
         because the target has only ONE constructor with a given name
         In fact, I am going to cheat, and just use the first name
         every time, which will crash is there isn't actually 
         a coercion for it .. we could then fix that by trying to
         generate the coercion, and if that fails, try the next
         case. What we really should do may be to pick the most
         general target type, but even that seems problematic at the moment.
         The first case is not bad, because there's currently no way
         to pattern match variant types with duplicated constructors,
         to make that work you would have to specify which one with
         the type. If there were duplicates with the same type,
         you'd have to go even further and allow a pattern match
         to repeat the same case. The first branch uses seq 0,
         the second seq 1, etc. Then the sequence has to also be
         encoded in the tag, and, the compiler has to analyse the
         pattern match and add in the sequence number, or, provide
         syntax for the user to do so.
      *)
        begin (* check the target for uniqueness of names *)
          let counts = Hashtbl.create 97 in
          let get_rel_seq name = 
            let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
            Hashtbl.replace counts name n;
            n
          in
          List.iter (fun (name,_) -> ignore (get_rel_seq name)) rs;
          Hashtbl.iter (fun name count -> 
            if count <> 0 then 
              print_endline ("Warning: Variant coercion target duplicates name " ^ 
                name ^ ", will use first one for coercion")
          ) counts;
          let coercions = List.map (fun (name, ltyp) ->
            let condition = bexpr_match_variant (name,srce) in
            let extracted = bexpr_variant_arg ltyp (name,srce) in
            (* just use first one .. later we could try next one if it fails *)
            let rtyp = List.assoc name rs in
            let coerced = bexpr_coerce (extracted,rtyp) in
            let new_variant = bexpr_variant dstt (name, coerced) in 
            condition, new_variant
            ) ls 
          in
          let rec chain cs = 
            match cs with
            | (cond,variant) :: second :: tail ->
               bexpr_cond cond variant (chain (second :: tail))
            | [_, variant] -> variant (* dubious, skipping check due to exhaustion *)
            | [] -> assert false
          in 
          let result = chain coercions in
          remap result
        end
      end

    | BTYP_record ls, BTYP_record rs ->
      (* count duplicate fields in target *)
      let counts = Hashtbl.create 97 in
      let get_rel_seq name = 
        let n = try Hashtbl.find counts name + 1 with Not_found -> 0 in
        Hashtbl.replace counts name n;
        n
      in
      begin try
        let prjs = List.map (fun ( name,rtyp) -> name,
          begin  
            let rel_seq = get_rel_seq name in
(*
print_endline ("Dst Field " ^ name ^ ", rel_seq=" ^ string_of_int rel_seq ^ ",type=" ^Flx_print.sbt bsym_table rtyp);
*)
            let maybe = find_seq name rel_seq ls in
            match maybe with
            | None -> print_endline ("Missing field " ^ name); assert false 
            | Some (idx,ltyp) ->
(*
print_endline ("src field idx=" ^ string_of_int idx ^ ", type=" ^ Flx_print.sbt bsym_table ltyp);          
*)
            let prj = bexpr_prj idx srct ltyp in
            let raw_dst = bexpr_apply ltyp (prj,srce) in
            let final_dst = bexpr_coerce (raw_dst,rtyp) in
            final_dst
          end
          ) 
          rs 
        in
        let r = bexpr_record prjs in
        remap r 
      with _ -> 
       failwith ("Bad record coercion in egen " ^ Flx_print.sbt bsym_table srct ^ " ===> " ^ Flx_print.sbt bsym_table dstt);
      end

  | _ -> Flx_bexpr.bexpr_coerce (srce, dstt)

and process_expr new_table bsym_table counter expr = 
  let f_bexpr expr = process_expr new_table bsym_table counter expr in
  let remap expr = Flx_bexpr.map ~f_bexpr expr in
  match remap expr with
  | BEXPR_coerce ((srcx,srct) as srce,dstt),_ -> 
    expand_coercion new_table bsym_table counter remap srce dstt

  | _ -> expr

let process_exe new_table bsym_table counter exe =
  Flx_bexe.map ~f_bexpr:(process_expr new_table bsym_table counter) exe


let process_exes new_table bsym_table counter exes =
  List.map (process_exe new_table bsym_table counter) exes 

let process_entry new_table bsym_table counter parent i (bsym : Flx_bsym.t) =
  match bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun (props,vs,ps,ret,effects,exes) ->
    let exes = process_exes new_table bsym_table counter exes in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props, vs, ps, ret,effects, exes) in 
    let sym = Flx_bsym.replace_bbdcl bsym bbdcl in
    Flx_bsym_table.add new_table i parent bsym 

  | bbdcl -> Flx_bsym_table.add new_table i parent bsym


let expand_coercions syms bsym_table = 
  let new_table = Flx_bsym_table.create () in
  Flx_bsym_table.iter 
   (fun i parent bsym -> process_entry new_table bsym_table syms.Flx_mtypes2.counter parent i bsym)
    bsym_table
  ;
  new_table

