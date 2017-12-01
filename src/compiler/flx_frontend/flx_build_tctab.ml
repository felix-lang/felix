open Flx_bbdcl
open Flx_mtypes2
open Flx_exceptions
open Flx_print
open Flx_bid
open Flx_btype

let debug = false

(* Here we build two mappings:

  mutable instances_of_typeclass: 
    (bid_t, (bid_t * (bvs_t * Flx_btype.t * Flx_btype.t list)) list) Hashtbl.t;

This is a map from a typeclass index to a list of instances of the type class.
Each instance has an index, followed by an entry

   (bvs, constraint, instance type list)

which governs the map from the type class variables to the instance type list, 
where the latter may be polymorphic, bound by the bvs: these are the instance
quantifiers.

So this map is purely a type class to instance set, involving no virtuals. 

This map:

  mutable virtual_to_instances: 
    (bid_t, (bvs_t * Flx_btype.t * Flx_btype.t list * bid_t) list) Hashtbl.t;

is a derived map, which maps an virtual function to a list of possible overrides

   (bvs, constraint, instance types, function_instance)

one of which will replace the virtual function. The virtual is defined only
in the type class and the instances in multiple instance definitions.

*)

let catmap x = Flx_util.catmap x
let si i = string_of_int i

(* drop first n elements of list l *)
(*
let rec drop l n =
  if n = 0 then l else drop (List.tl l) (n-1)
*)

let drop l n = Flx_list.list_tail l n

let vs2ts vs = List.map (fun (s,i,k) -> btyp_type_var (i,k)) vs

let remap_virtual_types x = Flx_remap_vtypes.remap_virtual_types x

let check_binding syms bsym_table 
  (inst, inst_id, inst_vs, inst_ts, inst_sr, inst_map, inst_constraint) 
  tc tc_bvs tck sr id tck_bvs tctype 
=
  let sigmatch i inst_funbvs t =
    (* typeclass X[t1,t2] { virtual fun f[t3] .. }
       Instance[i1, i2, i3] X[..,..] { fun f[i4] 

       typeclass fun poly vars = all fun vars - typeclass vars = 3 - 1 = 1
       inst fun poly vars = all fun vars - inst vars = 4 - 3 = 1
    *)

    let tc_ptv = List.length tck_bvs - List.length tc_bvs in
    let inst_ptv = List.length inst_funbvs - List.length inst_vs in
if debug && id = "f" then
begin
print_endline ("Type class has " ^ si (List.length tc_bvs) ^ " type variables");
print_endline ("Virtual has " ^ si (List.length tck_bvs) ^ " type variables (total)");
print_endline ("Virtual has " ^ si tc_ptv ^ " type variables (local)");
print_endline ("Instance has " ^ si (List.length inst_vs) ^ " type variables (total)");
print_endline ("Virtual instance function has " ^ si (List.length inst_funbvs) ^ " type variables (total)");
print_endline ("Virtual instance function has " ^ si inst_ptv ^ " type variables (local)");
end;
    if inst_ptv <> tc_ptv then (
if debug && id = "f" then
      print_endline ("Wrong no args: inst_ptv="^ si inst_ptv^"<>"^si tc_ptv);
      false
    )
    else
    let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (List.length inst_vs)) in
    assert (List.length tck_bvs = List.length inst_funts);
    let tctype' = (Flx_btype_subst.tsubst inst_sr tck_bvs inst_funts tctype) in
    let tct = Flx_beta.beta_reduce "flx_typeclass: check_instance"
      syms.Flx_mtypes2.counter
      bsym_table
      sr
      tctype'
    in
let tct = remap_virtual_types syms bsym_table (* tc *) tct in
if debug && id = "f" then
print_endline ("Virtual type: " ^ sbt bsym_table tct ^ ", instance type: " ^ sbt bsym_table t);
    let matches =  tct = t in
if debug && id = "f" then
print_endline ("Matches= " ^ string_of_bool matches);
    matches
  in

(* strip out all the instance kids with the wrong name, or for which the signatures don't agree *)
  let entries = 
    List.filter 
    (fun (name,(i,(inst_funbvs,t))) -> 
       if name = id then begin
        let m = sigmatch i inst_funbvs t in
if debug && id = "f" then
        print_endline ("  ... filtering functions found name: " ^ name ^ " sigmatch = " ^ string_of_bool m);
        m
       end else false
    )
    inst_map 
  in
if debug && inst_id = "X" then
print_endline ("We have " ^ si (List.length entries) ^ " functions left");

(* see what we have left *)
  match entries with

(* if there's nothing left that's fine! The virtual might never be called. Alternatively a default
in the typeclass might be used instead. This routine only handles actual instances!
*)
  | [] -> 
if debug && inst_id = "X" then
begin
  print_endline ("     ** function: No binding found");
end;
    ()

  | [_,(i,(inst_funbvs,t))] ->
    let tc_ptv = List.length tck_bvs - List.length tc_bvs in
    let inst_ptv = List.length inst_funbvs - List.length inst_vs in
    if inst_ptv <> tc_ptv then
    clierrx "[flx_frontend/flx_typeclass.ml:208: E361] " sr ("Wrong number of type parameters in instance fun!\n" ^
      "Expected " ^ si tc_ptv ^ "\n" ^
      "Got " ^ si inst_ptv
    );

    let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (List.length inst_vs)) in

    assert (List.length tck_bvs = List.length inst_funts);

    let tct = Flx_beta.beta_reduce "flx_typeclass: check instance (2)"
      syms.Flx_mtypes2.counter
      bsym_table
      sr
      (Flx_btype_subst.tsubst inst_sr tck_bvs inst_funts tctype)
    in
    let old =
      try Hashtbl.find syms.virtual_to_instances tck
      with Not_found -> []
    in
    let entry = inst_vs, inst_constraint, inst_ts, i in
    if List.mem entry old then
      clierrx "[flx_frontend/flx_typeclass.ml:229: E362] " sr "Instance already registered??"
    else begin
if debug && inst_id = "X" then
begin
  print_endline ("      ** function: add virtual to instance binding")
end;
(* finally, add the instance to the virtual to instance mapping table for subsequent lookups *)
      Hashtbl.replace syms.virtual_to_instances tck (entry :: old);
    end

  | _ ->
    clierrx "[flx_frontend/flx_typeclass.ml:236: E363] " sr ("Felix can't handle overloads in typeclass instances yet, " ^ id ^ " is overloaded")

let check_type_binding syms bsym_table (inst, inst_id, inst_vs, inst_ts, inst_sr, inst_map, inst_constraint) tc tc_bvs tck sr id tck_bvs  =
(* strip out all the instance kids with the wrong name, or for which the signatures don't agree *)
  let entries = 
    List.filter 
    (fun (name,(i,(inst_funbvs,t))) -> name = id ) 
    inst_map 
  in
if debug && inst_id = "X" then
print_endline ("We have " ^ si (List.length entries) ^ " types left");

(* see what we have left *)
  match entries with

(* if there's nothing left that's fine! The virtual might never be called. Alternatively a default
in the typeclass might be used instead. This routine only handles actual instances!
*)
  | [] ->
if debug && inst_id = "X" then
begin
  print_endline ("     ** type: No binding found");
end;
  ()

  | [_,(i,(inst_funbvs,t))] ->
    let tc_ptv = List.length tck_bvs - List.length tc_bvs in
    let inst_ptv = List.length inst_funbvs - List.length inst_vs in
    if inst_ptv <> tc_ptv then
    clierrx "[flx_frontend/flx_typeclass.ml:208: E361] " sr ("Wrong number of type parameters in instance fun!\n" ^
      "Expected " ^ si tc_ptv ^ "\n" ^
      "Got " ^ si inst_ptv
    );

    let inst_funts = inst_ts @ vs2ts (drop inst_funbvs (List.length inst_vs)) in

    assert (List.length tck_bvs = List.length inst_funts);

(*
    let tct = Flx_beta.beta_reduce "flx_typeclass: check instance (2)"
      syms.Flx_mtypes2.counter
      bsym_table
      sr
      (Flx_btype_subst.tsubst inst_sr tck_bvs inst_funts tctype)
    in
*)
    let old =
      try Hashtbl.find syms.virtual_to_instances tck
      with Not_found -> []
    in
    let entry = inst_vs, inst_constraint, inst_ts, i in
    if List.mem entry old then
      clierrx "[flx_frontend/flx_typeclass.ml:229: E362] " sr "Instance already registered??"
    else begin
(* finally, add the instance to the virtual to instance mapping table for subsequent lookups *)
if debug && inst_id = "X" then
begin
  print_endline ("      ** type: add virtual to instance binding")
end;
      Hashtbl.replace syms.virtual_to_instances tck (entry :: old);
    end

  | _ ->
    clierrx "[flx_frontend/flx_typeclass.ml:236: E363] " sr ("Felix can't handle overloads in typeclass instances yet, " ^ id ^ " is overloaded")

let build_inst_map bsym_table inst_kids =
  BidSet.fold begin fun i acc ->
    let bsym = Flx_bsym_table.find bsym_table i in
    match Flx_bsym.bbdcl bsym with
    | BBDCL_external_fun (_,bvs,params,ret,_,_,_) ->
        let argt = btyp_tuple params in
        let qt = bvs, btyp_function (argt,ret) in
        (Flx_bsym.id bsym,(i,qt)) :: acc

    | BBDCL_fun (_,bvs,bps,ret,effects,_) ->
        let argt = btyp_tuple (Flx_bparams.get_btypes bps) in
        let qt = bvs, btyp_function (argt,ret) in
        (Flx_bsym.id bsym,(i,qt)) :: acc

    | BBDCL_external_const (_,bvs,ret,_,_) ->
        let qt = bvs,ret in
        (Flx_bsym.id bsym,(i,qt)) :: acc

    | BBDCL_val (bvs,ret,`Val) ->
        let qt = bvs,ret in
        (Flx_bsym.id bsym,(i,qt)) :: acc

    | BBDCL_virtual_type bvs ->
      clierr (Flx_bsym.sr bsym) ("Virtual type not allowed in type class instance")

    (* this doesn't really belong here, its a type, not a typed entity *)
    | BBDCL_instance_type (bvs,ret) ->
if debug then
print_endline ("Scanning instance, found instance type " ^ Flx_bsym.id bsym ^ " -> " ^ sbt bsym_table ret); 
      let qt = bvs,ret in
      (Flx_bsym.id bsym,(i,qt)) :: acc

    | _ -> acc
  end inst_kids []

let check_instance
  (syms:Flx_mtypes2.sym_state_t)
  bsym_table
  inst
  inst_id
  (inst_vs: Flx_kind.bvs_t)
  inst_constraint
  inst_sr
  inst_props
  tc
  inst_ts
=
(*
if debug && inst_id = "X" then
  print_endline ("Check instance, inst_constraint=" ^ sbt bsym_table inst_constraint);
*)
(* STEP 1: find the typeclass *)
  let tc_bsym = Flx_bsym_table.find bsym_table tc in
  let tc_id = Flx_bsym.id tc_bsym in
  match Flx_bsym.bbdcl tc_bsym with
  | BBDCL_typeclass (tc_props, tc_bvs) ->
(*
if debug && inst_id = "X" then
begin
    print_endline ("Found " ^ inst_id ^ "<"^si inst ^ ">" ^
    "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
    " to be instance of typeclass " ^ tc_id ^ "<"^si tc^">")
    ;
    print_endline ("Typeclass vs = " ^
      catmap "," (fun (s,j) -> s^"<"^si j^">") tc_bvs
    );
end;
*)
    if List.length tc_bvs <> List.length inst_ts then
      clierr2 inst_sr (Flx_bsym.sr tc_bsym)
      (
        "Instance [" ^
        catmap "," (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">") inst_vs
        ^ "] " ^
        inst_id ^"<"^ string_of_bid inst ^ ">" ^
        "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
        "\nsupplies wrong number of type arguments for typeclass parameters\n" ^
        inst_id ^ "[" ^
        catmap "," (fun (s,j,_) -> s ^ "<" ^ string_of_bid j ^ ">") tc_bvs ^ "]"
      )
    ;
(* list all the kids of the type class *)
    let tc_kids =
      try Flx_bsym_table.find_children bsym_table tc
      with Not_found -> BidSet.empty
    in
(*
if debug && inst_id = "X" then 
begin
  print_string ("Typeclass has children " );
  BidSet.iter (fun i-> print_string (si i ^ ",")) tc_kids;
  print_endline "";
end;
*)
(* list all the kids of the instance *)
    let inst_kids =
      try Flx_bsym_table.find_children bsym_table inst
      with Not_found -> BidSet.empty
    in
(*
if debug && inst_id = "X" then
begin
  print_string ("Instance has children ");
  BidSet.iter (fun i-> print_string (si i ^ ",")) inst_kids;
  print_endline "";
end;
*)
(* transform the instance kid list into an associatiion list
  mapping the function name to the index and function type
*)
    let inst_map = build_inst_map bsym_table inst_kids in
(*
if debug && inst_id = "X" then
begin
  print_endline ("Instance map for " ^ inst_id ^ "[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]");
  List.iter (fun (name,(index,(bvs,typ))) ->
    print_endline (name ^"<" ^ si index ^ ">: " ^ sbt bsym_table typ);
  )
  inst_map
end;
*)
(* check the polymorphic binding of the virtual function id[tck,tck_bvs] type tctype
   matches the given instance 
*)
(*
if debug && inst_id = "X" then
begin
  print_endline ("Scanning type class " ^ si tc ^ " children");
end;
*)
    let inst_data =  (inst, inst_id, inst_vs, inst_ts, inst_sr, inst_map, inst_constraint) in

(* We need two passes, because the function bindings depend on the type bindings *)
(* PASS 1, map virtual types to instance types *)
    BidSet.iter begin fun tck ->
      let tck_bsym = Flx_bsym_table.find bsym_table tck in
if debug && inst_id = "X" then
begin
  print_endline ("   type class child " ^ Flx_bsym.id tck_bsym);
end;

      match Flx_bsym.bbdcl tck_bsym with
      | BBDCL_virtual_type bvs -> 
(*
if debug && inst_id = "X" 
then print_endline ("       virtual type");
*)
        check_type_binding syms bsym_table inst_data tc tc_bvs tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs 
      | _ -> ()
    end 
    tc_kids
  ;

(* PASS 2, map virtual functins to instance functions *)
    BidSet.iter begin fun tck ->
      let tck_bsym = Flx_bsym_table.find bsym_table tck in
(*
if debug && inst_id = "X" then
begin
  print_endline ("   type class child " ^ Flx_bsym.id tck_bsym);
end;
*)
      match Flx_bsym.bbdcl tck_bsym with
      | BBDCL_external_fun (_,bvs,params,ret,_,_,`Code Flx_code_spec.Virtual) ->
(*
if debug && inst_id = "X" 
then print_endline ("       virtual extern function");
*)
        let ft = btyp_function (btyp_tuple params,ret) in
        check_binding syms bsym_table inst_data tc tc_bvs tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_fun (props,bvs,bps,ret,effects,_) when List.mem `Virtual props ->
(*
if debug && inst_id = "X" 
then print_endline ("       virtual felix function");
*)
        let argt = btyp_tuple (Flx_bparams.get_btypes bps) in
        (* ignore effects for now! *)
        let ft = btyp_function (argt,ret) in
        check_binding syms bsym_table inst_data tc tc_bvs tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ft

      | BBDCL_external_const (props,bvs,ret,_,_) when List.mem `Virtual props ->
(*
if debug && inst_id = "X" 
then print_endline ("       virtual extern const");
*)
        check_binding syms bsym_table inst_data tc tc_bvs tck (Flx_bsym.sr tck_bsym) (Flx_bsym.id tck_bsym) bvs ret

      | BBDCL_virtual_type bvs ->  ()

      | BBDCL_instance_type _ ->
        clierr (Flx_bsym.sr tck_bsym) ("instance type not allowed in type class?")

      | _ ->
if debug && inst_id = "X" 
then print_endline ("       non virtual entry");
        (*
        clierrx "[flx_frontend/flx_typeclass.ml:261: E364] " tcksr "Typeclass entry must be virtual function or procedure"
        *)
        (*
        print_endline ("Warning: typeclass " ^ Flx_bsym.id tc_bsym ^ " entry " ^
          Flx_bsym.id tck_bsym ^ " is not virtual");
        *)
        ()
    end 
    tc_kids

  | _ ->
    clierr2 inst_sr (Flx_bsym.sr tc_bsym) ("Expected " ^ inst_id ^ "<" ^
      string_of_bid inst ^ ">[" ^ catmap "," (sbt bsym_table) inst_ts ^ "]" ^
      " to be typeclass instance, but" ^ Flx_bsym.id tc_bsym ^ "<" ^
      string_of_bid tc ^ ">, is not a typeclass"
    )


(* NEW SEMANTICS 20/12/2015: every virtual function will be in this
table. If it is a virtual with a default, that shall be included
as an instance. If it has no default, and no separate instances,
it must still have an empty entry. Lookup on the table determines
if a function is virtual. If a lookup succeeds when instantiating
virtuals, the compiler must bug out if no instances matches
a call. After monomorphisation, only monomorphised clones 
of virtuals with defaults will be present, except for the special
case of monomorphising a monomorphic virtual which retains
its original identity.
*)

let build_typeclass_to_instance_table syms bsym_table : unit =
  let get_insts tc =
    try Hashtbl.find syms.instances_of_typeclass tc
    with Not_found -> 
    Hashtbl.add syms.instances_of_typeclass tc [];
    []
  in
  Flx_bsym_table.iter begin fun i parent bsym ->
  let id = Flx_bsym.id bsym in
  match Flx_bsym.bbdcl bsym with
  | BBDCL_instance (props, vs, cons, tc, ts) ->
    let iss = get_insts tc in
    let entry = i, (vs, cons, ts) in
    Hashtbl.replace syms.instances_of_typeclass tc (entry::iss);
  
    let inst_id = Flx_bsym.id bsym in
(*
    if debug && inst_id = "X" then
    print_endline ("Typeclass: " ^ Flx_bsym.id bsym ^"<"^ si tc ^ "> instance " ^ si i );
*)
      check_instance
        syms
        bsym_table
        i
        (Flx_bsym.id bsym)
        vs
        cons
        (Flx_bsym.sr bsym)
        props
        tc
        ts

  | BBDCL_virtual_type _ ->
(*
    print_endline ("build type class instance table, IGNORING virtual type " ^ id);
*)
    ()

  | BBDCL_instance_type _ ->
(*
    print_endline ("build type class instance table, IGNORING instance type " ^ id);
*)
    ()

  (* virtual with default *)
  | BBDCL_fun (props,bvs,params,rettype,effects, exes) ->
    if List.mem `Virtual  props then
    let tc = match parent with
    | None -> assert false
    | Some p -> p
    in
    let v2i = 
      try Hashtbl.find syms.virtual_to_instances i 
      with Not_found -> Hashtbl.add syms.virtual_to_instances i []; []  
    in
    let cons = Flx_btype.btyp_unit () in
    let ts = List.map (fun (s,j,k) -> Flx_btype.btyp_type_var (j,k)) bvs in
    let entry = bvs, cons, ts,i in (* self reference *)
    Hashtbl.replace syms.virtual_to_instances i (entry::v2i)

  (* virtual with default *)
  | BBDCL_external_fun (props,bvs,paramtypes,rettype,reqs,prec,kind) ->
    begin match kind with
    | `Code Flx_code_spec.Virtual -> 
(*
       print_endline ("Pure virtual primitive " ^ Flx_bsym.id bsym);
*)
       assert (List.mem `Virtual props)
    | _ -> ()
(*
      if List.mem `Virtual props then
        print_endline ("Instantiated Virtual primitive " ^ Flx_bsym.id bsym);
*)
    end;
    if List.mem `Virtual  props then
    begin match kind with
    | `Code (Flx_code_spec.Str_template s)
    | `Code (Flx_code_spec.Str s) ->
      let v2i = 
        try Hashtbl.find syms.virtual_to_instances i 
        with Not_found -> Hashtbl.add syms.virtual_to_instances i []; []  
      in
      if s <> "" then
        let cons = Flx_btype.btyp_unit () in
        let ts = List.map (fun (s,j,k) -> Flx_btype.btyp_type_var (j,k)) bvs in
        let entry = bvs, cons, ts,i in (* self reference *)
        Hashtbl.replace syms.virtual_to_instances i (entry::v2i)

    | _ -> ()
    end
  | _ -> ()
  end
  bsym_table


