(* New monomorphisation routine *)
open Flx_util
open Flx_btype
open Map
open Flx_mtypes2
open Flx_print
open Flx_types
open Flx_bbdcl
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_btype_subst
open Flx_bid


(* NOTES
   We make monomorphic specialisations of everything used.
   For Felix entities, references lose their type arguments (ts) and the
   entity loses its type paramaters (vs).

   For C things, we monomorphise the visible code, and so get
   a copy of the C thing, however the C thing doesn't lose
   its type parameters (vs) and references don't lose the
   type arguments (ts) .. even though the copy is monomorphic.

   This is because this routine does not substitute type arguments
   into the C code fragments, so the C code part of the entity
   remains polymorphic, even though the Felix part of the interface
   is monomorphised.

*)

let debug = false 

(* ----------------------------------------------------------- *)
(* ROUTINES FOR REPLACING TYPE VARIABLES IN TYPES              *)
(* ----------------------------------------------------------- *)

(* 
let check_mono_vars bsym_table vars sr t =
  try Flx_monocheck.check_mono bsym_table sr t
  with _ -> 
    print_endline (" **** using varmap " ^ showvars bsym_table vars);
    assert false
*)
(*
let monomap_compare (i,ts) (i',ts') = 
  let counter = ref 1 in (* HACK *)
  let dummy = Flx_bsym_table.create () in 
  if i = i' && List.length ts = List.length ts' &&
    List.fold_left2 (fun r t t' -> r && Flx_unify.type_eq dummy counter t t') true ts ts'
  then 0 else compare (i,ts) (i',ts') 
*)
let rec mono_element debug syms to_process processed bsym_table nutab nubids i ts j =
(*
  print_endline ("mono_element: " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^ "]" ^ " --> " ^ si j);
*)
  let virtualinst sr i ts =
    try Flx_typeclass.fixup_typeclass_instance syms bsym_table sr i ts 
    with Not_found -> 
      print_endline ("[mono-element:virtualinst] Can't find index " ^ si i); 
      if BidSet.mem i (!nubids) then begin
        print_endline "FOUND IN NEW TABLE .. OK";
        i,ts
      end else
        assert false
  in

  let polyinst sr i ts =  
    let sym = 
      try Some (Flx_bsym_table.find bsym_table i)
      with Not_found ->
         print_endline ("[mono-element:polyinst] Can't find index " ^ si i); 
         if BidSet.mem i (!nubids) then begin
           print_endline "FOUND IN NEW TABLE .. OK";
           None
         end else
         assert false
    in
    match sym with
    | None -> assert (ts = []); i, ts
    | Some sym ->
    let {Flx_bsym.id=id;sr=sr; bbdcl=bbdcl} = sym in
    match bbdcl with
    | BBDCL_virtual_type _ -> 
      print_endline ("Polyinst hit virtual type " ^ id ^ "<" ^ si i ^ ">["^
        catmap "," (sbt bsym_table) ts ^"]");
      let j = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids i ts in
      print_endline ("Remapping to mono type " ^ si j);
      j,[]

    | BBDCL_external_type _ 
    | BBDCL_external_const _ 
    | BBDCL_external_fun _ 
    | BBDCL_external_code _  -> 
      let j = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids i ts in
      j,ts
    | _ ->
      let j = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids i ts in
      j,[]
  in
  let sr = Flx_srcref.make_dummy "[mono_element]" in
  begin try List.iter (Flx_monocheck.check_mono bsym_table sr) ts with _ -> assert false end;
  try
    let parent,sym = 
      try Flx_bsym_table.find_with_parent bsym_table i 
      with Not_found -> assert false
    in
    let {Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} = sym in
    let parent = match parent with
      | None -> None
      | Some 0 -> Some 0 
      | Some p -> 
        let psym = 
          try Flx_bsym_table.find bsym_table p 
          with Not_found -> 
            print_endline ("[mono_element] Cannot find parent " ^ si p);
            assert false 
        in
        let {Flx_bsym.id=id;sr=sr;bbdcl=bbdcl} = psym in
        begin match bbdcl with
        | BBDCL_fun (_,vs,_,_,_,_) ->
          let n = List.length vs in
          let pts = Flx_list.list_prefix ts n in 
(*
print_endline ("Our ts = " ^ catmap "," (sbt bsym_table) ts);
print_endline ("Parent vs = " ^ catmap "," (fun (s,i) -> s) vs);
print_endline ("Parent ts = " ^ catmap "," (sbt bsym_table) pts);
*)
(*
          print_endline ("  mono_element: adding parent " ^ si p ^" = " ^ id ^ ", ts=" ^ catmap "," (sbt bsym_table) pts);
*)
          let nuparent = Flx_monomap.find_felix_inst syms bsym_table processed to_process nubids p pts in
(*
          print_endline ("Nu parent: " ^ si nuparent);
*)
          Some nuparent

        | BBDCL_instance _
        | BBDCL_module
        | BBDCL_typeclass _ -> None
        | _ -> assert false 
        end
    in
    let maybebbdcl = 
      try Flx_monobbdcl.mono_bbdcl syms bsym_table processed to_process nubids virtualinst polyinst ts sym i j 
      with Not_found -> assert false 
    in
    begin match maybebbdcl with
    | Some nubbdcl -> 
      (* NOTE: we don't use [] here bpair[unit, int]ecause it's confusing with polymorphism *)
      let nuname = Flx_bsym.id sym (* ^ ( 
        if List.length ts = 0 then "" 
         else "{" ^ catmap "," (sbt bsym_table) ts ^ "}")  *)
      in
      let nusym ={Flx_bsym.id=nuname; sr=sr; bbdcl=nubbdcl} in
      Flx_bsym_table.add nutab j parent nusym
    | None -> ()
    end
  with Not_found -> 
   print_endline "NOT FOUND in mono_element";
   raise Not_found

module MM = Flx_monomap.MonoMap

let monomorphise2 debug syms bsym_table =
(*
    print_endline "";
    print_endline "---------------------------";
    print_endline "PRE NUMONO";
    print_endline "---------------------------";
    print_endline "";

    Flx_print.print_bsym_table bsym_table;
*)
  let roots: BidSet.t = !(syms.roots) in
  assert (BidSet.cardinal roots > 0);


  (* add coercion types and functions to roots *)
  let roots = Flx_bsym_table.fold_coercions bsym_table
    (fun acc ((a,b),c) -> 
      let x = BidSet.add a acc in
      let y = BidSet.add b x in
      let z = BidSet.add c y in
      z
    )
    roots
  in

  (* to_process is the set of symbols yet to be scanned
     searching for symbols to monomorphise
  *)
  let to_process = ref MM.empty in
  BidSet.iter (fun i -> to_process := MM.add (i,[]) (i) (!to_process)) roots;
  
  let processed = ref MM.empty in

  (* new bsym_table *)
  let nutab = Flx_bsym_table.create_fresh () in

  (* Set of indices of NEW symbols to go or already gone into it *)
  let nubids = ref  BidSet.empty in 

  let sr = Flx_srcref.make_dummy "[monomorphise2]" in
  while not (MM.is_empty (!to_process)) do
    let (i,ts),j = MM.choose (!to_process) in
    assert (not (MM.mem (i,ts) (!processed) ));
    begin try List.iter (Flx_monocheck.check_mono bsym_table sr) ts with _ -> assert false end;

    to_process := MM.remove (i,ts) (!to_process);
    processed := MM.add (i,ts) j (!processed);

    (*
    (* if i <> j then *)
      print_endline ("numono: "^showts bsym_table i ts ^" ==> " ^ si j);
    *)
    assert (List.length ts > 0 || i == j);
    assert (not (Flx_bsym_table.mem nutab j));
    mono_element debug syms to_process processed bsym_table nutab nubids i ts j;
  done
  ;

  Hashtbl.clear syms.instances_of_typeclass;
  Hashtbl.clear syms.virtual_to_instances;
  syms.axioms := [];

(*
  syms.reductions := [];
*)
(*
print_endline ("Allowing " ^ string_of_int (List.length !(syms.reductions)) ^ " reductions");
*)
(* Add coercions to new table now *)

  Flx_bsym_table.iter_coercions bsym_table
    (fun (((a,b),c) as x) ->
     Flx_bsym_table.add_supertype nutab x
    )
  ;
  begin try
    Flx_bsym_table.validate "post-monomorphisation" nutab
  with Flx_bsym_table.IncompleteBsymTable (bid,bid2,_) ->
    print_endline ("Post monomorphisation, symbol " ^string_of_int bid2 
       ^ "used in " ^ string_of_int bid^ " missing from bound symbol table"
    );
    Flx_print.print_bsym_table  nutab;
    failwith "SYSTEM ERROR: monomorphisation failed"
  end;

  if syms.Flx_mtypes2.compiler_options.Flx_options.print_flag then 
  begin
    print_endline "";
    print_endline "---------------------------";
    print_endline "POST NUMONO";
    print_endline "---------------------------";
    print_endline "";
    print_endline (" +++ REMAP INDEX +++ ");
    MM.iter (fun ((i,ts), j) ->
      print_endline ("   " ^ si i ^ "[" ^ catmap "," (sbt bsym_table) ts ^"] --> " ^ si j ))
    !processed
    ;
    Flx_print.print_bsym_table  nutab
  end;
  nutab


