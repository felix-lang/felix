open Flx_btype
open Flx_bexpr
open Flx_bid

exception Vfound (* for variants, Found already used elsewhere *)

let rec function_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ld lc rd rc =
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt in
  (* coerce function argument value to function parameter value 
     value subtype of parameter
     domain contravariant, codomain covariant
   *)

   let fidx = fresh_bid counter in
   let pidx = fresh_bid counter in

   (* parameter symbol table entry *)
   let bbdcl = Flx_bbdcl.bbdcl_val ([],rd, `Val)  in
   let pname = "_coerce_param" ^ string_of_int pidx in
   let bsym = Flx_bsym.create pname bbdcl in
   Flx_bsym_table.add new_table pidx (Some fidx) bsym;

   (* code for wrapper function *)
   let p = bexpr_varname rd (pidx, []) in 
   let cp = coerce (Some fidx) p ld in
   let e = remap (Some fidx) srce in
   let exe = 
     match lc with
     | BTYP_void -> assert (lc = rc);
       [
         Flx_bexe.bexe_call (Flx_srcref.dummy_sr, e, cp);
         Flx_bexe.bexe_proc_return (Flx_srcref.dummy_sr)
       ]
       
     | _ ->
       let calc = bexpr_apply lc (e, cp) in
       let r = coerce (Some fidx) calc rc in
       [Flx_bexe.bexe_fun_return (Flx_srcref.dummy_sr, r)]
   in 

   (* wrapper function *)
   let effects = Flx_btype.btyp_unit () in
   let param = {Flx_bparameter.pid=pname;pindex=pidx;pkind=`PVal;ptyp=rd} in
   let params = [param], None in
   let bbdcl = Flx_bbdcl.bbdcl_fun ([],[],params,rc,effects,exe) in
   let bsym = Flx_bsym.create ("_coerce" ^ string_of_int fidx) bbdcl in
   Flx_bsym_table.add new_table  fidx parent bsym;

   (* expanded coercion is closure of wrapper function *)
   let coerced_function = bexpr_closure  (btyp_function (rd,rc)) (fidx,[]) in
   coerced_function
   
and variant_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs =
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt in
 
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
      This is a CHEAT, we use a reinterpret case, doing this 
      depends on knowing the backend representation!
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
        let e = remap parent srce in
        bexpr_reinterpret_cast (e,dstt)
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
            let coerced = coerce parent extracted rtyp in
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
          remap parent result
        end
      end


and record_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs =
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt in
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
            let final_dst = coerce parent raw_dst rtyp in
            final_dst
          end
          ) 
          rs 
        in
        let r = bexpr_record prjs in
        remap parent r 
      with _ -> 
       failwith ("Bad record coercion in xcoerce " ^ Flx_print.sbt bsym_table srct ^ " ===> " ^ Flx_print.sbt bsym_table dstt);
      end

and tuple_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs =
  let n = List.length ls in
  let m = List.length rs in
  assert (n=m);
  let nlst = Flx_list.nlist n in
  let prjs = List.map2 (fun i t-> bexpr_get_n  t i srce) nlst ls in
  let prjs = List.map2 (fun p t-> bexpr_coerce (p,t)) prjs rs in
  remap parent (bexpr_tuple dstt prjs)

and array_coercion new_table bsym_table counter parent remap ((_,srct) as srce) dstt l r n =
  let coerce parent e t = expand_coercion new_table bsym_table counter parent remap e t in
  let fidx = Flx_bid.fresh_bid counter in
  let ix = Flx_bid.fresh_bid counter in
  let ixval = bexpr_varname l (ix,[]) in
  let mapping = coerce parent ixval r in
  let lam = bexpr_lambda ix l mapping in
  let acidx = Flx_lambda.add_array_map new_table counter parent fidx srce lam in
  let ft = btyp_function (srct, dstt) in
  bexpr_apply dstt (bexpr_closure ft (acidx,[]), srce)

(* this routine assumes coercions were introduces only when they're
justified by theory, it doesn't check if, for example, that
compact linear type transformations are actually isomorphisms, etc
*)
and expand_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt =
  if Flx_typeeq.type_eq (Flx_print.sbt bsym_table) counter srct dstt
  then srce
  else
  (* each of these reductions can introduce new coercions, so each must
  re-expand any parts of its return value that could contain these
  new coercions, to eliminate them if possible
  *)
  match srct,dstt with
  | BTYP_function (ld,lc) , BTYP_function (rd,rc)  ->
    function_coercion new_table bsym_table counter parent remap srce dstt ld lc rd rc

  | BTYP_variant ls, BTYP_variant rs ->
    variant_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | BTYP_record ls, BTYP_record rs ->
    record_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | BTYP_tuple ls, BTYP_tuple rs when List.length ls = List.length rs ->
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | BTYP_tuple ls, BTYP_array (r, BTYP_unitsum n) when List.length ls = n ->
    let rs = Flx_list.repeat r n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | BTYP_array (l, BTYP_unitsum n), BTYP_tuple rs when List.length rs = n ->
    let ls = Flx_list.repeat l n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | BTYP_array (l, BTYP_unitsum n), BTYP_array(r,BTYP_unitsum m) when n = m ->
    if n > 20 then begin
      print_endline ("oerce array longer than 20");
      array_coercion new_table bsym_table counter parent remap srce dstt l r n
    end
    else
    let ls = Flx_list.repeat l n in
    let rs = Flx_list.repeat r n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs

  | _ -> 
    (* not currently reducible, becomes a reinterpret cast in flx_egen,
    should really be a distinct term so that we're assured all coercions
    are eliminated
    *)
(*
print_endline ("Unable to expand coercion: " ^ Flx_print.sbe bsym_table srce ^ " {type=" ^
  Flx_print.sbt bsym_table srct ^
"} :>> " ^ Flx_print.sbt bsym_table dstt);
*)
    Flx_bexpr.bexpr_coerce (srce, dstt)

and process_expr new_table bsym_table counter parent expr = 
  let f_bexpr parent expr = process_expr new_table bsym_table counter parent expr in
  let remap parent expr = Flx_bexpr.map ~f_bexpr:(f_bexpr parent) expr in
  (* perform bottom up expansion, upto subparts of expression *)
  let e = remap parent expr in
  match e with
  (* coercion with argument free of reducible coercions *)
  | BEXPR_coerce ((srcx,srct) as srce,dstt),_ -> 
(*
    print_endline ("Examining coercion " ^ Flx_print.sbe bsym_table e );
*)
    let e' = expand_coercion new_table bsym_table counter parent remap srce dstt in
(*
    print_endline ("Expanded to " ^ Flx_print.sbe bsym_table e');
*)
    e'

  (* no reducible coercions left *)
  | _ -> e

let process_exe new_table bsym_table counter parent exe =
  let newexe = Flx_bexe.map ~f_bexpr:(process_expr new_table bsym_table counter parent) exe in
(*
  print_endline ("Old bexe=" ^ Flx_print.sbx bsym_table exe);
  print_endline ("New bexe=" ^ Flx_print.sbx bsym_table newexe);
*)
  newexe


let process_exes new_table bsym_table counter parent exes =
  List.map (process_exe new_table bsym_table counter parent) exes 

let process_entry new_table bsym_table counter parent i (bsym : Flx_bsym.t) =
  match bsym.Flx_bsym.bbdcl with
  | Flx_bbdcl.BBDCL_fun (props,vs,ps,ret,effects,exes) ->
(*
print_endline ("Processing function " ^ Flx_bsym.id bsym);
*)
    let exes = process_exes new_table bsym_table counter (Some i) exes in
    let bbdcl = Flx_bbdcl.bbdcl_fun (props, vs, ps, ret,effects, exes) in 
    let bsym = Flx_bsym.replace_bbdcl bsym bbdcl in
    Flx_bsym_table.add new_table i parent bsym 

  | bbdcl -> Flx_bsym_table.add new_table i parent bsym


let expand_coercions syms bsym_table = 
  let new_table = Flx_bsym_table.create () in
  Flx_bsym_table.iter 
   (fun i parent bsym -> process_entry new_table bsym_table syms.Flx_mtypes2.counter parent i bsym)
    bsym_table
  ;
  new_table

