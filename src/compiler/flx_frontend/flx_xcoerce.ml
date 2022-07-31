open Flx_btype
open Flx_bexpr
open Flx_bid

(* This module handles implicit subtyping coercions, see flx_coerce for the module
that handles explicit user coercions!
*)
let debug = false

exception Vfound (* for variants, Found already used elsewhere *)

let si x = string_of_int x

let rec function_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ld lc rd rc sr =
(*
let sbt b = Flx_print.sbt b in
print_endline ("xcoerce, function coercion: l=" ^ sbt bsym_table ld ^ " -> " ^ sbt bsym_table rd ^
 ", r=" ^ sbt bsym_table rd ^ " - > " ^ sbt bsym_table rc);
*)
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt sr in
  (* coerce function argument value to function parameter value 
     value subtype of parameter
     domain contravariant, codomain covariant
   *)

   let fidx = fresh_bid counter in
(*
print_endline ("Coercion function index = " ^ string_of_int fidx);
*)
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
   let params = Flx_ast.Satom param, None in 
   let bbdcl = Flx_bbdcl.bbdcl_fun ([],[],params,rc,effects,exe) in
   let bsym = Flx_bsym.create ("_coerce" ^ string_of_int fidx) bbdcl in
   Flx_bsym_table.add new_table  fidx parent bsym;

   (* expanded coercion is closure of wrapper function *)
   let coerced_function = bexpr_closure  (btyp_function (rd,rc)) (fidx,[]) in
   coerced_function
   
and variant_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs sr =
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt sr in
  if debug then
  print_endline ("Variant coercion " ^ Flx_btype.st srct ^ " => " ^ Flx_btype.st dstt); 
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
    if debug then
    print_endline ("Shortcut variant coercion!");
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
          let typ = List.assoc name rs in
          let eqtypes = List.fold_left 
            (fun acc (n,t) -> acc && n<>name || Flx_unify.type_eq bsym_table counter typ t) 
            true rs 
          in
          if not eqtypes then
            print_endline ("Flx_coerce.Warning: Variant coercion target duplicates name " ^ 
              name ^ ", will use first one for coercion, argtype = " ^ Flx_print.sbt bsym_table typ)
      ) counts;
      let coercions = List.map (fun (name, ltyp) ->
        let condition = bexpr_match_variant (name,srce) in
(*
print_endline ("ltyp = " ^ Flx_btype.st ltyp);
*)
        let extracted = bexpr_variant_arg ltyp (name,srce) in
        (* just use first one .. later we could try next one if it fails *)
(*
print_endline ("ltyp = " ^ Flx_btype.st ltyp);
*)
        let rtyp = List.assoc name rs in
        let coerced = coerce parent extracted rtyp in
(*
print_endline ("dstt = " ^ Flx_btype.st dstt);
*)
        (* this is required if dstt was recursive and unfolded,
           since the hash requires a minimised type.
           Should be enforced in the hash routine but Ocaml compilation
           model has got in the way.
        *)
        let dstt = Flx_fold.minimise bsym_table counter dstt in
(*
print_endline ("MINIMISED dstt = " ^ Flx_btype.st dstt);
*)
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


and record_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs sr =
  let coerce parent e dstt = expand_coercion new_table bsym_table counter parent remap e dstt sr in
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

and tuple_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt ls rs sr =
  let n = List.length ls in
  let m = List.length rs in
  assert (n=m);
  let nlst = Flx_list.nlist n in
  let prjs = List.map2 (fun i t-> bexpr_get_n  t i srce) nlst ls in
  let prjs = List.map2 (fun p t-> bexpr_coerce (p,t)) prjs rs in
  remap parent (bexpr_tuple dstt prjs)

and array_coercion new_table bsym_table counter parent remap ((_,srct) as srce) dstt l r n sr =
  let coerce parent e t = expand_coercion new_table bsym_table counter parent remap e t sr in
  let si i = string_of_int i in

  let lam = 
    let dt = l in (* domain array element type *)
    let ct = r in (* target array element type *)
    let fidx = Flx_bid.fresh_bid counter in
(*
print_endline ("array_coercion: element lambda function index  = " ^ si fidx);
*)
    let pidx = Flx_bid.fresh_bid counter in
(*
print_endline ("array_coercion: element lambda parameter index = " ^ si pidx);
*)
    let effects = Flx_btype.btyp_unit () in
    let param = bexpr_varname dt (pidx,[]) in 
    let retexpr = coerce parent param ct in
    let ret_stmt = Flx_bexe.bexe_fun_return (sr,retexpr) in
    let exes = [ret_stmt] in
    let lamname = Flx_lambda.add_wrapper_function new_table parent fidx pidx dt effects ct exes in
(*
print_endline ("array_coercion: added element lambda function index = " ^ string_of_int fidx);
*)
    let lamt = btyp_function (dt,ct) in
    let lam = bexpr_closure lamt (fidx, []) in
    lam
  in
  let acidx = Flx_lambda.add_array_map new_table counter parent srct lam in

(*
print_endline ("array_coercion: whole array function added, index= " ^ string_of_int acidx);
let bbdcl = Flx_bsym_table.find_bbdcl new_table acidx in
print_endline (Flx_print.string_of_bbdcl new_table bbdcl acidx);
*)
  (* apply the function to the array argument *)
  let ft = btyp_function (srct, dstt) in
  bexpr_apply dstt (bexpr_closure ft (acidx,[]), srce)

(* this routine assumes coercions were introduces only when they're
justified by theory, it doesn't check if, for example, that
compact linear type transformations are actually isomorphisms, etc
*)
and expand_coercion new_table bsym_table counter parent remap ((srcx,srct) as srce) dstt sr =
(*
print_endline ("Src type " ^ Flx_print.sbt bsym_table srct);
print_endline ("Dst type " ^ Flx_print.sbt bsym_table dstt);
*)
  if Flx_typeeq.type_eq (Flx_print.sbt bsym_table) counter srct dstt
  then srce
  else
  (* each of these reductions can introduce new coercions, so each must
  re-expand any parts of its return value that could contain these
  new coercions, to eliminate them if possible
  *)
  let srct = unfold "expand_coercion srct" srct in
  let dstt = unfold "expand_coercion dstt" dstt in
  match srct,dstt with
  | BTYP_inst (`Nominal _, src,lts,_), BTYP_inst (`Nominal _, dst,rts,_) when src = dst ->
    let bsym = Flx_bsym_table.find bsym_table src in
    let bbdcl = Flx_bsym.bbdcl bsym in
    begin match bbdcl with
    | BBDCL_cstruct (bvs,flds,_,variance)
    | BBDCL_struct (bvs,flds,variance) ->
      (* each member must be covariant: it's the same as a tuple *)

      (* get list of src values *)
      let lvarmap = Flx_btype_subst.mk_varmap sr bvs lts in
      let sflds = List.map (fun (_, t) -> Flx_btype_subst.varmap_subst lvarmap t) flds in
      let prjs = List.map2 (fun pos t -> bexpr_get_n t pos srce) (Flx_list.nlist (List.length flds)) sflds in

      (* get list of dst types *)
      let rvarmap = Flx_btype_subst.mk_varmap sr bvs rts in
      let dtyps = List.map (fun (_, t) -> Flx_btype_subst.varmap_subst rvarmap t) flds in
      let dtyp = btyp_tuple dtyps in 

      (* get list of coerced values *) 
      let vals = List.map2 (fun p t -> bexpr_coerce (p,t)) prjs dtyps in
      (* build argument tuple *)
      let arg = bexpr_tuple dtyp vals in

      (* we cannot use a apply_struct yet so we have to form a closure instead *)
      let ctor_typ = btyp_function (dtyp, dstt) in
      let ctor = bexpr_closure ctor_typ (dst, rts) in
      remap parent (bexpr_apply dstt (ctor, arg))

    | x ->
      Flx_exceptions.clierr sr ("Flx_xcoerce: NOT IMPLEMENTED: polymorphic nominal type coercions for " ^ Flx_print.sbt bsym_table srct) 
    end

    (* argument ......................  parameter *)
  | BTYP_inst (`Nominal _, src,sts,_), BTYP_inst (`Nominal _, dst,dts,_) ->
    if List.length sts > 0 || List.length dts > 0 then
       print_endline ("UNIMPLEMENTED POLYMORPHIC NOMINAL TYPE COERCION");
    if debug then
    print_endline ("Searching for nominal type conversion from " ^ 
    si src  ^ " -> " ^ si dst);
    let coercion_chains = Flx_bsym_table.find_coercion_chains bsym_table dst src in
    begin match coercion_chains with
    | [] -> 

      let srcid = Flx_bsym.id (Flx_bsym_table.find bsym_table src) in
      let dstid = Flx_bsym.id (Flx_bsym_table.find bsym_table dst) in
      print_endline ("Flx_xcoerce: Unable to find supertype coercion from " ^ 
      srcid ^ "<" ^ si src ^ "> to " ^ dstid ^ "<" ^ si dst ^ ">");
      Flx_bsym_table.iter_coercions bsym_table
        (fun ((a,b),c) -> print_endline ("       " ^ si c ^ ":" ^ si b ^ "->" ^ si a))
      ;
      Flx_exceptions.clierr sr ("Unable to find supertype coercion from " ^ 
      srcid ^ "<" ^ si src ^ "> to " ^ dstid ^ "<" ^ si dst ^ ">");
 
    | chains ->
      if debug then
      print_endline ("Found "^string_of_int (List.length chains) ^" coercion chains from " ^
      si src ^ " to " ^ si dst);
      let shortest_chain = 
        List.fold_left (fun acc chain -> 
          let n = List.length acc in if n = 0 || n > List.length chain then chain else acc
        ) [] chains
      in
      let result = List.fold_left (fun acc fn ->
        let ft = Flx_bsym_table.get_fun_type bsym_table fn in
        let cod = match ft with | BTYP_function (dom,cod) -> cod | _ -> assert false in
        if debug then
        print_endline ("Function " ^ (Flx_bsym_table.find bsym_table fn).id ^ "<" ^ string_of_int fn ^">:" ^ Flx_print.sbt bsym_table ft);
(* FIXME: we cannot do this now: instead we have to first find the MGU from specialising the domain so it matches the argument,
   the resulting type variable assignments then have to be ordered the same as those in the function, and then we use those type
   expressions in the closure. The resulting application yields an argument with the function's (dependent) type variables eliminated 
   We know this has to work because unification had to do this too to type check it 
*)
        let cls = bexpr_closure ft (fn,[]) in
        bexpr_apply cod (cls, acc)
        ) 
        srce 
       (List.rev shortest_chain)
      in
      assert (dstt = snd result);
      result
    end
     
  | BTYP_linearfunction (ld,lc) , BTYP_function (rd,rc) ->
    (* print_endline ("Coercing linear function type to ordinary function type .."); *)
    function_coercion new_table bsym_table counter parent remap srce dstt ld lc rd rc sr

  | BTYP_function (ld,lc) , BTYP_function (rd,rc)  ->
    function_coercion new_table bsym_table counter parent remap srce dstt ld lc rd rc sr

  | BTYP_variant ls, BTYP_variant rs ->
    let ls = List.map (fun (s,t) -> s,unfold "variant ls component" t) ls in
    let rs = List.map (fun (s,t) -> s,unfold "variant rs component" t) rs in
    variant_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

  | BTYP_record ls, BTYP_record rs ->
    record_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

  | BTYP_array _, BTYP_tuple rs 
  | BTYP_tuple _, BTYP_tuple rs when (match List.rev rs with | BTYP_ellipsis :: _ -> true | _ -> false) ->
    (* NOTE: the leading components of the argument (lhs) should be coerced to the leading
       components of the parameter (rhs) and the remainder passed "as is",
       however this code just passes everything "as is" at the moment
    *)
    srce

  | BTYP_tuple ls, BTYP_tuple rs when List.length ls = List.length rs ->
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

  | BTYP_tuple ls, BTYP_array (r, BTYP_unitsum n) when List.length ls = n ->
    let rs = Flx_list.repeat r n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

  | BTYP_array (l, BTYP_unitsum n), BTYP_tuple rs when List.length rs = n ->
    let ls = Flx_list.repeat l n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

  | BTYP_array (l, BTYP_unitsum n), BTYP_array(r,BTYP_unitsum m) when n = m ->
    if n > 20 then begin
      print_endline ("Flx_xcoerce. Coerce array longer than 20");
      array_coercion new_table bsym_table counter parent remap srce dstt l r n sr
    end
    else
    let ls = Flx_list.repeat l n in
    let rs = Flx_list.repeat r n in
    tuple_coercion new_table bsym_table counter parent remap srce dstt ls rs sr

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

and process_expr new_table bsym_table counter parent sr expr = 
  let f_bexpr parent expr = process_expr new_table bsym_table counter parent sr expr in
  let remap parent expr = Flx_bexpr.map ~f_bexpr:(f_bexpr parent) expr in
  (* perform bottom up expansion, upto subparts of expression *)
  let e = remap parent expr in
  match e with
  (* coercion with argument free of reducible coercions *)
  | BEXPR_coerce ((srcx,srct) as srce,dstt),_ -> 
(*
    print_endline ("Examining coercion " ^ Flx_print.sbe bsym_table e );
*)
    let e' = expand_coercion new_table bsym_table counter parent remap srce dstt sr in
(*
    print_endline ("Expanded to " ^ Flx_print.sbe bsym_table e');
*)
    e'

  (* no reducible coercions left *)
  | _ -> e

let process_exe new_table bsym_table counter parent exe =
  let sr = Flx_bexe.get_srcref exe in
(*
  print_endline ("Old bexe=" ^ Flx_print.sbx bsym_table exe);
*)
  let newexe = Flx_bexe.map ~f_bexpr:(process_expr new_table bsym_table counter parent sr) exe in
(*
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


(* This function takes a bsym_table with instructions with embedded coercions
  in them and removes all the coercions it can, producing a new table.
  The input table must include a proper subtype_map for nominal type
  subtyping coercions. The generated output table has no such map:
  the coercions should be gone, and so the subtype table shoudn't
  be needed. Its erasure is important because while it exists,
  it drags around a bunch of types and functions that may or may
  not be used, but cannot be eliminated in case they're used.
*)
let expand_coercions syms bsym_table = 
  let new_table = Flx_bsym_table.create_fresh () in
  Flx_bsym_table.iter 
   (fun i parent bsym -> process_entry new_table bsym_table syms.Flx_mtypes2.counter parent i bsym)
    bsym_table
  ;
  let reductions= Flx_bsym_table.get_reductions bsym_table in
(*
print_endline ("Flx_xcoerce: checking viability for " ^ string_of_int (List.length reductions) ^ " reductions");
*)
  let reductions = Flx_reduce.filter_viable_reductions new_table reductions in
  Flx_bsym_table.set_reductions new_table reductions;
    new_table

