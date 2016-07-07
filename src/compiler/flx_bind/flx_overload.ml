open Flx_util
open Flx_list
open Flx_ast
open Flx_types
open Flx_btype
open Flx_set
open Flx_mtypes2
open Flx_print
open Flx_exceptions
open Flx_typing
open Flx_typing2
open Flx_unify
open Flx_beta
open Flx_generic
open Flx_tconstraint
open Flx_tpat
open Flx_maps

exception OverloadKindError of Flx_srcref.t * string

let is_typeset tss1 =
  match List.rev tss1 with
  | [] -> false
  | (p1,v1) ::t ->
    p1.assignments = [] &&
    BidSet.cardinal p1.pattern_vars = 1 &&
    match p1.pattern,v1 with 
    | BTYP_type_var (i,BTYP_type 0), BTYP_void
      when i = BidSet.choose p1.pattern_vars ->
      begin try 
        List.iter (fun (p,v) -> match p,v with
        | { assignments=[]; 
            pattern_vars=pvs; 
            pattern=BTYP_inst (_,[])
          },
          BTYP_tuple [] when BidSet.is_empty pvs -> ()
        | _ -> raise Not_found
        )
        t;
        true
      with Not_found -> false
      end
    | _ -> false

let make_typeset tss : bid_t list =
  match List.rev tss with
  | h::t -> List.map (fun x ->
    match x with 
    | {pattern=BTYP_inst (i,[])},_ -> i
    | _ -> assert false
    ) 
    t
  | _ -> assert false

let is_subset tss1 tss2 : bool =
  let tss1: bid_t list = make_typeset tss1
  and tss2: bid_t list = make_typeset tss2 in
  try List.iter (fun x -> if not (List.mem x tss2) then raise Not_found) tss1; true
  with Not_found -> false

(* this routine checks that the second list of cases includes the first,
 * which means the first implies the second. This means, every case
 * in the first list must be in the second list. The order must agree
 * as well, since typematches are ordered.
 *)
let rec scancases bsym_table counter tss1 tss2 = match (tss1, tss2) with
  | [],_ -> true
  | _,[] -> false
  | (p1,v1)::t1 as c1, (p2,v2)::t2  ->
    if p1.assignments = [] 
    && p2.assignments = []
    then
      if BidSet.is_empty (p1.pattern_vars)
      && BidSet.is_empty (p2.pattern_vars)
      then
        if type_eq bsym_table counter p1.pattern p2.pattern
        && type_eq bsym_table counter v1 v2
        then scancases bsym_table counter t1 t2 (* advance both *)
        else scancases bsym_table counter c1 t2 (* skip rhs case *)
      (* special case of wildcard, somewhat hacked *)
      else match p1.pattern,p2.pattern with
      | BTYP_type_var _, BTYP_type_var _ ->
         if type_eq bsym_table counter v1 v2
         then scancases bsym_table counter t1 t2 (* advance both *)
         else scancases bsym_table counter c1 t2 (* skip rhs case *)
      | BTYP_type_var _,_ -> scancases bsym_table counter c1 t2 (* skip rhs case *)
      | _ -> false
   else false

let typematch_implies bsym_table counter a b = match a, b with
  | BTYP_type_match (v1,tss1), BTYP_type_match (v2,tss2) ->
     type_eq bsym_table counter v1 v2 &&
     if is_typeset tss1 && is_typeset tss2 
     then is_subset tss1 tss2
     else scancases bsym_table counter tss1 tss2
  | _ -> false

let factor_implies bsym_table counter ls b =
  try 
    List.iter (fun a ->
      if type_eq bsym_table counter a b then raise Not_found
      else if typematch_implies bsym_table counter a b then raise Not_found
    ) 
    ls;
    false
  with Not_found -> true

let terms_imply bsym_table counter ls1 ls2 =
  try
    List.iter (fun b ->
      if not (factor_implies bsym_table counter ls1 b) then raise Not_found
    )  
    ls2;
    true
   with Not_found -> false

let rec split_conjuncts' t =
  match t with
  | BTYP_intersect ls ->
    List.concat (List.map split_conjuncts' ls)
  | _ -> [t]

let filter_out_units ls = 
   List.filter (fun x -> x <> btyp_tuple []) ls

let split_conjuncts ls = filter_out_units (split_conjuncts' ls)

let constraint_implies bsym_table counter a b =
  let r = terms_imply bsym_table counter (split_conjuncts a) (split_conjuncts b) in
  r

type overload_result = Flx_btype.overload_result

let show_overload_result bsym_table (bid,sign,ret,mgu,ts) =
  string_of_int bid ^ ":" ^ sbt bsym_table sign^ " -> " ^ sbt bsym_table ret ^
  "\n   mgu=" ^ catmap "," (fun (v,t) -> string_of_int v ^ "<-" ^ sbt bsym_table t) mgu ^
  "\n  ts=" ^ catmap "," (sbt bsym_table) ts

type result =
  | Unique of overload_result
  | Fail

let show_result bsym_table r = match r with
  | Unique ovr -> "UNIQUE " ^ show_overload_result bsym_table ovr
  | Fail -> "FAIL"

let get_data table index =
  try Flx_sym_table.find table index
  with Not_found ->
    failwith ("[Flx_lookup.get_data] No definition of <" ^
      string_of_bid index ^ ">")

(* Overloading is called when an application apply (f,a) is found.
 * sig_of_symdef is called when f is a closure, to find the signature
 * of f, that is, the domain of f considered as a function.
 * Thus, f may be a primitive or felix function, but it can also
 * be a struct used as a constructor, a type function,
 * a type, since types can be used as constructors, or,
 * any value, since values can be used as function via
 * an apply function.
 *
 * Constructors and apply functions should fail here: it is the
 * job of the lookup routine to retry a type X considered as a
 * constructor with routine _ctor_X, similarly a value used as
 * a function via an apply method is retried by the lookup routine.
 * 
 * so .. this routine should only succeed for actual functions
 * or entities considered as functions "intrinsically", not as a
 * result of a syntax trick or user defined application or x
 * constructor.
 *)

let sig_of_symdef symdef sr name i = match symdef with
  (* primitives *)
  | SYMDEF_fun (_,ps,r,_,_,_)
    -> type_of_list ps,r,None

  | SYMDEF_callback (_,ts_orig,r,_)
    ->
      let ts_f =
        List.filter
        (function
          | TYP_name (_,id,[]) when id = name -> false
          | t -> true
        )
        ts_orig
      in
      let tf_args = match ts_f with
        | [x] -> x
        | lst -> TYP_tuple lst
      in
      let tf = TYP_function (tf_args, r) in

      (* The type of the arguments Felix thinks the raw
         C function has on a call. A closure of this
         function is a Felix function .. NOT the raw
         C function.
      *)
      let ts_cf =
        List.map
        (function
          | TYP_name (_,id,[]) when id = name -> tf
          | t -> t
        )
        ts_orig
      in
      type_of_list ts_cf,r,None

  | SYMDEF_function (ps,r,effects,_,_) ->
    let p = fst ps in
    begin match p,r with
    | _ ->
      paramtype p,r,Some (List.map (fun (_,name,_,d)->name,d) p)
    end

  | SYMDEF_cstruct (ls, _) ->
    type_of_list (List.map snd ls), TYP_index (sr,name,i),
     Some (List.map (fun (p,_) -> p,None) ls)

  | SYMDEF_struct ls ->
    type_of_list (List.map snd ls), TYP_index (sr,name,i),
     Some (List.map (fun (p,_) -> p,None) ls)

  | SYMDEF_const_ctor (_,r,_,_) -> TYP_void sr,r,None
  | SYMDEF_nonconst_ctor (_,r,_,_,t) -> t,r,None
  | SYMDEF_type_alias t ->
    (*
    print_endline ("[sig_of_symdef] Found a typedef " ^ name);
    *)
    begin match t with
    | TYP_typefun (ps,r,b) ->
      (*
      print_endline "TYP_typefun";
      *)
      type_of_list (List.map snd ps),r,None
    | symdef ->
      (*
      print_endline "OverloadKindError";
      *)
      raise (OverloadKindError (sr,
        "[sig_of_symdef] Expected "^
        name
        ^" to be a type function, got " ^
        string_of_typecode t
      ))
    end

  | symdef ->
    raise (OverloadKindError (sr,
      "[sig_of_symdef] Expected "^
      name
      ^" to be function or procedure, got " ^
     string_of_symdef symdef name dfltvs
    ))


let fixup_argtypes be bid pnames base_domain argt rs =
  match pnames with
  | None -> argt
  | Some ps ->
      match
        try
          List.iter (fun (name,_) -> ignore (List.assoc name ps)) rs;
          true
        with Not_found -> false
      with
      | false -> argt
      | true ->
          match base_domain with
          | TYP_record _ -> argt
          | TYP_tuple [] -> argt (* lazy *)
          | _ ->
              let ps =
                List.map begin fun (name,e) ->
                  name,
                  match e with
                  | None -> None
                  | Some e -> Some (be bid e)
                end ps
              in
              begin
                try
                  let ats =
                    List.map begin fun (name,d) ->
                      try List.assoc name rs
                      with Not_found ->
                        match d with (* ok to skip if there is a default *)
                        | Some (e,t) -> t
                        | None -> raise Not_found
                    end ps
                  in
                  let t =
                    match ats with
                    | [] -> assert false
                    | [x] -> x
                    | _ -> btyp_tuple ats
                  in
                  t
                with Not_found -> argt
              end


let resolve sym_table bsym_table base_sym bt be arg_types =
  let sym = Flx_sym_table.find sym_table base_sym in
  let name = sym.Flx_sym.id in
(*
if name = "accumulate" then print_endline "Attempting to resolve accumulate";
*)
  let opt_bsym = try Some (Flx_bsym_table.find bsym_table base_sym) with Not_found -> None in

  let pvs, vs, { raw_type_constraint=con } =
    find_split_vs sym_table bsym_table base_sym
  in
  let base_domain, base_result, pnames = sig_of_symdef
    sym.Flx_sym.symdef
    sym.Flx_sym.sr
    sym.Flx_sym.id
    base_sym
  in
(*
if name = "accumulate" then begin
  print_endline ("Base_sym=" ^si base_sym^ ", base domain="^string_of_typecode base_domain ^
   ", base result="^string_of_typecode base_result^", pnames from sig_of_symdef");
end;
*)

  let arg_types =
    match arg_types with
    | BTYP_record (rs) as argt :: tail ->
        fixup_argtypes be base_sym pnames base_domain argt rs :: tail

    | BTYP_tuple [] as argt :: tail ->
        fixup_argtypes be base_sym pnames base_domain argt [] :: tail

    | _ ->
        arg_types
  in
(*
if name = "accumulate" then 
  print_endline ("Arg types = " ^ catmap "," (sbt bsym_table) arg_types);
*)
  (* bind type in base context, then translate it to view context:
   * thus, base type variables are eliminated and specialisation
   * type variables introduced *)

  let con = match con with | TYP_tuple [] -> Flx_btype.btyp_tuple [] | _ -> bt sym.Flx_sym.sr con in
  let domain,base_result = 
  (* this is primarily an optimisation to save recursive overload resolution
   * to find the return type of a function, which may itself involve a chain
   * of overload resolutions. However it also helps if a function isn't declared
   * with a return type, to find the computed return type: however this will ONLY
   * WORK if the function is already bound, so it can't be relied on. This needs
   * to be fixed! Because the results of a call with multiple arguments depend
   * on the return type, and we can't have the success of overloading depend on
   * the order of binding the compiler happens to pick! FIX IT!
   *)
  match opt_bsym with
  | Some {Flx_bsym.id=id;sr=sr;bbdcl=Flx_bbdcl.BBDCL_fun (props,base_bvs,ps,rt,effects,_)} ->
(*
    print_endline ("Found function binding for " ^ id);
*)
    let domain = Flx_bparams.get_btype ps in
    let base_result = rt in
    domain, base_result

  | _ -> 
(*
print_endline ("Warning: didn't find function binding for " ^ sym.Flx_sym.id);
*)
(*
if name = "accumulate" then 
  print_endline ("Can't find bound symbol table entry, binding:");
*)
    let domain = 
      try bt sym.Flx_sym.sr base_domain 
      with _ -> 
       print_endline ("Can't bind base domain type " ^ string_of_typecode base_domain);
       assert false
    in
    let base_result = 
     try bt sym.Flx_sym.sr base_result 
     with _ -> print_endline ("Can't bind base result type " ^ string_of_typecode base_result); BTYP_none
    in
    domain,base_result
  in
  sym.Flx_sym.id, sym.Flx_sym.sr, vs, pvs, con, domain, base_result, arg_types


let rec unravel_ret tin dts =
  match tin with
  | BTYP_function (a,b) -> unravel_ret b (a::dts)
  | _ -> List.rev dts

let hack_name qn = match qn with
| `AST_name (sr,name,ts) -> `AST_name (sr,"_inst_"^name,ts)
| `AST_lookup (sr,(e,name,ts)) -> `AST_lookup (sr,(e,"_inst_"^name,ts))
| _ -> failwith "expected qn .."


let specialize_domain sr base_vs sub_ts t =
  (*
  print_endline ("specialise Base type " ^ sbt bsym_table t);
  *)
  let n = List.length base_vs in
  let ts = list_prefix sub_ts n in
  let vs = List.map (fun (i,n,_) -> i,n) base_vs in
  let t = tsubst sr vs ts t in
  (*
  print_endline ("to View type " ^ sbt bsym_table t);
  *)
  t


let make_equations
  counter
  bsym_table
  id
  sr
  entry_kind
  input_ts
  arg_types
  spec_domain
  spec_result
=
(*
  print_endline ("BASE Return type of function " ^ id ^ "<" ^
    si entry_kind.base_sym ^ ">=" ^ sbt bsym_table spec_result);
*)
  (* unravel function a->b->c->d->..->z into domains a,b,c,..y
     to match curry argument list *)
  let curry_domains =
    try unravel_ret spec_result []
    with _ -> print_endline "Failed to unravel candidate return type!"; []
  in
  let curry_domains = spec_domain :: curry_domains in
(*
  print_endline ("Argument  sigs= " ^  catmap "->" (sbt bsym_table) arg_types);
  print_endline ("Candidate sigs= " ^  catmap "->" (sbt bsym_table) curry_domains);
*)
  (* equations for user specified assignments *)
  let lhsi = List.map (fun (n,i) -> i) entry_kind.spec_vs in
  let lhs = List.map
    (fun (n,i) -> btyp_type_var ((i), btyp_type 0))
    entry_kind.spec_vs
  in
  let n = min (List.length entry_kind.spec_vs) (List.length input_ts) in
  let eqns = List.combine (list_prefix lhs n) (list_prefix input_ts n) in

  (* these are used for early substitution *)
  let eqnsi = List.combine (list_prefix lhsi n) (list_prefix input_ts n) in

  (*
  print_endline "TS EQUATIONS ARE:";
  List.iter (fun (t1,t2) -> print_endline (sbt bsym_table t1 ^ " = " ^ sbt bsym_table t2))
  eqns
  ;
  *)

  (*
  print_endline ("Curry domains (presub)   = " ^ catmap ", " (sbt bsym_table) curry_domains);
  *)
  let curry_domains = List.map
    (fun t -> list_subst counter eqnsi t)
    curry_domains
  in

  (*
  print_endline ("Curry domains (postsub)  = " ^ catmap ", " (sbt bsym_table) curry_domains);
  *)

  let curry_domains = List.map
    (fun t -> beta_reduce "flx_overload: curry domains" counter bsym_table sr t)
    curry_domains
  in

  (*
  print_endline ("Curry domains (postbeta) = " ^ catmap ", " (sbt bsym_table) curry_domains);
  *)

  let n = min (List.length curry_domains) (List.length arg_types) in
  let eqns = eqns @ List.combine
    (list_prefix curry_domains n)
    (list_prefix arg_types n)
  in

  let dvars = ref BidSet.empty in
  List.iter (fun (_,i)-> dvars := BidSet.add i !dvars) entry_kind.spec_vs;
(*
  print_endline "EQUATIONS ARE:";
  List.iter (fun (t1,t2) -> print_endline (sbt bsym_table t1 ^ " = " ^ sbt bsym_table t2))
  eqns
  ;

  (* WRONG!! dunno why, but it is! *)
  print_endline ("DEPENDENT VARIABLES ARE " ^ catmap "," si
    (BidSet.fold (fun i l-> i::l) !dvars []));
  print_endline "...";
*)
  let result = try Some (unification bsym_table counter eqns !dvars) with Not_found -> None in
(*
  begin match result with
  | None -> print_endline "Does not unify"
  | Some mgu -> print_endline ("UNIFIES with MGU = " ^ string_of_varlist bsym_table mgu)
  end;
*)
  result


let solve_mgu
  counter
  bsym_table
  id
  call_sr
  mgu
  entry_kind
  base_vs
  sr
  bt
  con
  arg_types
  parent_vs
  domain
  spec_result
  env_traint
=
(*
  print_endline "Specialisation detected";
  print_endline (" .. mgu = " ^ string_of_varlist bsym_table mgu);
*)
(*
if id = "accumulate" then print_endline "solve_mgu";
*)
  let mgu = ref mgu in
  (* each universally quantified variable must be fixed
    by the mgu .. if it doesn't its an error .. hmmm

    THIS CANNOT HAPPEN NOW!
    JS: 13/3/2006 .. oh yes it can!

  *)

  (*
  print_endline "Check for unresolved";
  *)
  let unresolved = ref (
    Flx_list.fold_lefti begin fun i acc (s,bid) ->
      if not (List.mem_assoc bid !mgu) then (s,bid,TYP_type,i)::acc else acc
    end [] entry_kind.spec_vs
  )
  in

  (* Below algorithm is changed! We now make list
     of unresolved dependent variables, and see
     if the constraint resolution can help.
     Actually, at this point, we can even try
     to see if the return type can help
   *)

  let th i =
    match i with
    | 0 -> "first"
    | 1 -> "second"
    | 2 -> "third"
    | _ -> si (i+1) ^ "'th"
  in

  let report_unresolved =
    List.fold_left begin fun acc (s,i,tp,k) ->
      acc ^ "  The " ^th k ^" subscript  " ^ s ^ "[" ^ string_of_bid i ^
        "]" ^ Flx_print.string_of_maybe_typecode tp ^ "\n"
    end "" !unresolved
  in
  (*
  if List.length !unresolved > 0 then
    print_endline (
    "WARNING: experimental feature coming up\n" ^
    "Below would be an error, but we try now to do more work\n" ^
    (* clierrx "[flx_bind/flx_overload.ml:578: E247] " call_sr ( *)
      "[resolve_overload] In application of " ^ id ^
      " cannot resolve:\n" ^
      report_unresolved ^
     "Try using explicit subscript" ^
     "\nMost General Unifier(mgu)=\n" ^ string_of_varlist sym_table !mgu
    )
  ;
  *)
  (*
  if List.length !unresolved > 0 then None else
  *)

  (* HACKERY to try to get more values from type patterns*)
  (*
  if List.length !unresolved > 0 then
  *)

  begin
    (* convert mgu from spec vars to base vars *)
    let basemap = List.map2
      (fun (_,i,_) t -> i,list_subst counter !mgu t)
      base_vs
      entry_kind.sub_ts
    in
    (*
    print_endline ("New basemap: " ^ catmap ","
      (fun (i,t) -> si i ^ "->" ^ sbt bsym_table t)
      basemap
    );
    *)

    let extra_eqns = ref [] in
    let dvars = ref BidSet.empty in

    List.iter begin fun (_,i)->
      if not (List.mem_assoc i !mgu) then (* mgu vars get eliminated *)
      dvars := BidSet.add i !dvars
    end entry_kind.spec_vs;

    List.iter begin fun (s,j',tp) ->
      let et,explicit_vars1,any_vars1, as_vars1, eqns1 =
        type_of_tpattern counter tp
      in
      let et = bt sr et in
      let et = specialize_domain sr base_vs entry_kind.sub_ts et in
      let et = list_subst counter !mgu et in
      let et = beta_reduce "flx_overload: make equations" counter bsym_table sr et in
      (*
      print_endline ("After substitution of mgu, Reduced type is:\n  " ^
        sbt bsym_table et)
      ;
      *)

      (* tp is a metatype .. it could be a pattern-like thing, which is
      a constraint. But it could also be an actual meta-type! In that
      case it is a constraint too, but not the right kind of constraint.
      EG in

         j' -> fun (x:TYPE):TYPE=>x : TYPE->TYP

      the TYPE->TYPE is a constraint only in the sense that it
      is the type of j'. Felix messes these things up.. you can
      give an explicit TYPE->TYPE which really amounts only
      to a typing constraint .. and no additional constraint.

      So we have to eliminate these from consideration
      *)

      (*
      print_endline ("Analysing "^s^"<"^si j'^">: " ^ string_of_typecode tp);
      print_endline (si j' ^ (if List.mem_assoc j' basemap then " IS IN BASEMAP" else " IS NOT IN BASEMAP"));
      *)

      (* this check is redundant .. we're SCANNING the base vs! *)
      match et with
      | BTYP_type _
      | BTYP_function _ -> () (* print_endline "ignoring whole metatype" *)
      | _ ->
      if List.mem_assoc j' basemap then begin
        let t1 = List.assoc j' basemap in
        let t2 = et in
        (*
        print_endline ("CONSTRAINT: Adding equation " ^ sbt bsym_table t1 ^ " = " ^ sbt bsym_table t2);
        *)
        extra_eqns := (t1,t2) :: !extra_eqns
      end;

      (* THIS CODE DOES NOT WORK RIGHT YET *)
      if List.length explicit_vars1 > 0 then

      print_endline ("Explicit ?variables: " ^
        catmap ","
          (fun (i,s) -> s ^ "<" ^ string_of_bid i ^ ">")
          explicit_vars1);

      List.iter begin fun (i,s) ->
        let coupled = List.filter (fun (s',_,_) -> s = s') base_vs in
        match coupled with
        | [] -> ()
        | [s',k,pat] ->
            print_endline (
              "Coupled " ^ s ^ ": " ^ string_of_bid k ^ "(vs var) <--> " ^
              string_of_bid i ^" (pat var)" ^
              " pat=" ^ string_of_typecode pat);
            let t1 = btyp_type_var (i, btyp_type 0) in
            let t2 = btyp_type_var (k, btyp_type 0) in

            print_endline ("Adding equation " ^ sbt bsym_table t1 ^ " = " ^
              sbt bsym_table t2);

            extra_eqns := (t1,t2) :: !extra_eqns;

            (*
            dvars := BidSet.add i !dvars;
            print_endline ("ADDING DEPENDENT VARIABLE " ^ si i);
            *)

         | _ -> assert false (* vs should have distinct names *)
      end explicit_vars1;

      if List.length as_vars1 > 0 then begin
        print_endline ("As variables: " ^
          catmap "," (fun (i,s) -> s ^ "<" ^ string_of_bid i ^ ">")
          as_vars1);
        print_endline "RECURSIVE?? AS VARS NOT HANDLED YET"
      end;

      (*
      if List.length any_vars1 > 0 then
      print_endline ("Wildcard variables: " ^
        catmap "," (fun i -> "<" ^ si i ^ ">") any_vars1)
      ;
      *)

      (* add wildcards to dependent variable set ?? *)
      List.iter (fun i-> dvars := BidSet.add i !dvars) any_vars1;

      (* add 'as' equations from patterns like
         t as v
      *)
      List.iter begin fun (i,t) ->
        let t2 = bt sr t in
        let t1 = btyp_type_var (i, btyp_type 0) in
        extra_eqns := (t1,t2) :: !extra_eqns
      end eqns1;

      (*
      if List.length eqns1 > 0 then
      print_endline ("Equations for as terms (unbound): " ^
        catmap "\n" (fun (i,t) -> si i ^ " -> " ^ string_of_typecode t) eqns1)
      ;
      *)
    end base_vs;

    (* NOW A SUPER HACK! *)
    let rec xcons con =
      match con with
      | BTYP_intersect cons -> List.iter xcons cons
      | BTYP_type_match (arg,[{pattern=pat},BTYP_tuple[]]) ->
          let arg = specialize_domain sr base_vs entry_kind.sub_ts arg in
          let arg = list_subst counter !mgu arg in
          let arg = beta_reduce "flx_overload: typematch arg" counter bsym_table sr arg in
          let pat = specialize_domain sr base_vs entry_kind.sub_ts pat in
          let pat = list_subst counter !mgu pat in
          let pat = beta_reduce "flx_overload: typematch pat" counter bsym_table sr pat in
          extra_eqns := (arg, pat)::!extra_eqns
      | _ -> ()
    in
    xcons con;

(*
    print_endline "UNIFICATION STAGE 2";
    print_endline "EQUATIONS ARE:";
    List.iter (fun (t1,t2) -> print_endline (sbt bsym_table t1 ^ " = " ^ sbt bsym_table t2))
    !extra_eqns
    ;
    print_endline "...";
    print_endline ("DEPENDENT VARIABLES ARE " ^ catmap "," si
      (BidSet.fold (fun i l-> i::l) !dvars [])
    );
*)
    let maybe_extra_mgu =
      try Some (unification bsym_table counter !extra_eqns !dvars)
      with Not_found -> None
    in
    match maybe_extra_mgu with
    | None ->  (* print_endline "COULDN'T RESOLVE EQUATIONS"; *) ()
    | Some extra_mgu ->
(*
        print_endline ("Resolved equations with mgu:\n  " ^
          string_of_varlist bsym_table extra_mgu)
        ;
*)
        let ur = !unresolved in
        unresolved := [];
        List.iter begin fun ((s,i,_,k) as u) ->
          (*
          let j = base + k in
          *)
          let j = i in
          if List.mem_assoc j extra_mgu
          then begin
            let t = List.assoc j extra_mgu in
            (*
            print_endline ("CAN NOW RESOLVE " ^
              th k ^ " vs term " ^ s ^ "<"^ si i^"> ---> " ^ sbt bsym_table t)
            ;
            *)
            mgu := (j,t) :: !mgu
          end
          else begin
            (*
            print_endline ("STILL CANNOT RESOLVE " ^ th k ^ " vs term " ^ s ^ "<"^si i^">");
            *)
            unresolved := u :: !unresolved
          end
        end ur
  end;
(*
if id = "accumulate" then
  print_endline ("Unresolved variables: " ^ si (List.length (!unresolved)));
*)
  if List.length !unresolved > 0 then None else begin
    let ok = ref true in
    List.iter begin fun sign ->
      if sign <> list_subst counter !mgu sign then begin
        ok := false;
        (*
        print_endline ("At " ^ Flx_srcref.short_string_of_src call_sr);
        (*
        clierrx "[flx_bind/flx_overload.ml:809: E248] " call_sr
        *)
        print_endline
        (
          "[resolve_overload] Unification of function " ^
          id ^ "<" ^ si entry_kind.base_sym ^ "> signature " ^
          sbt bsym_table domain ^
          "\nwith argument type " ^ sbt bsym_table sign ^
          "\nhas mgu " ^ string_of_varlist bsym_table !mgu ^
          "\nwhich specialises a variable of the argument type"
        )
        *)
      end
    end arg_types;
(*
if id = "accumulate" then print_endline (match !ok with | true -> "Argtypes ok" | _ -> "argtypes BAD");
*)
    if not (!ok) then None else
    (*
    print_endline ("Matched with mgu = " ^ string_of_varlist sym_table !mgu);
    *)
    (* RIGHT! *)
    (*
    let ts = List.map (fun i -> List.assoc (base+i) !mgu) (nlist (m+k)) in
    *)
    (* The above ts is for plugging into the view, but we
      have to return the elements to plug into the base
      vs list, this is the sub_ts with the above ts plugged in,
      substituting away the view vs
    *)

    let base_ts = List.map (list_subst counter !mgu) entry_kind.sub_ts in
    let base_ts = List.map (beta_reduce "flx_overload: base_ts" counter bsym_table sr) base_ts in
(*
if id = "accumulate" then print_endline ("base_ts = " ^ catmap "," (sbt bsym_table) base_ts);
*)
    (* we need to check the type constraint, it uses the
      raw vs type variable indicies. We need to substitute
      in the corresponding ts values. First we need to build
      a map of the correspondence
    *)
    let parent_ts = List.map
      (fun (n,i,_) -> btyp_type_var (i, btyp_type 0))
      parent_vs
    in
    let type_constraint = build_type_constraints counter bsym_table (bt sr) id sr base_vs in
    let type_constraint = btyp_intersect [type_constraint; con] in
(*
if id = "accumulate" then
    print_endline ("Raw type constraint " ^ sbt bsym_table type_constraint);
*)
    let vs = List.map (fun (s,i,_)-> s,i) base_vs in
    let type_constraint = tsubst sr vs base_ts type_constraint in
    (*
    print_endline ("Substituted type constraint " ^ sbt bsym_table type_constraint);
    *)
    let reduced_constraint = beta_reduce "flx_overload: constraint" counter bsym_table sr type_constraint in
(*
if id = "accumulate" then
    print_endline ("Reduced type constraint " ^ sbt bsym_table reduced_constraint);
*)
    begin match reduced_constraint with
    | BTYP_void ->
        (*
        print_endline "Constraint failure, rejecting candidate";
        *)
        None
    | BTYP_tuple [] ->
        let parent_ts = List.map
          (fun (n,i,_) -> btyp_type_var (i, btyp_type 0))
          parent_vs
        in
        Some (entry_kind.base_sym,domain,spec_result,!mgu,parent_ts @ base_ts)

    | x ->
        let implied = constraint_implies bsym_table counter env_traint reduced_constraint in
        if implied then 
          let parent_ts = List.map
            (fun (n,i,_) -> btyp_type_var (i, btyp_type 0))
            parent_vs in
          Some (entry_kind.base_sym,domain,spec_result,!mgu,parent_ts @ base_ts)
        else begin
          print_endline "Can't resolve type constraint!";
          print_endline ("Env traint = " ^ sbt bsym_table env_traint);
          print_endline ("Fun traint = " ^ sbt bsym_table reduced_constraint);
          print_endline ("Implication result = " ^ if implied then "true" else "false");

          clierrx "[flx_bind/flx_overload.ml:896: E249] " sr ("[overload] Cannot resolve type constraint! " ^
            sbt bsym_table type_constraint ^
            "\nReduced to " ^ sbt bsym_table x)
        end
    end
  end


(* Note this bt must bind types in the base context *)
let consider
  counter
  sym_table
  bsym_table
  call_sr
  env
  bt
  be
  name
  entry_kind
  input_ts
  arg_types
  env_traint 
: overload_result option =
  (* Helper function to simplify the bind type function. *)
  let bt sr t = bt sr entry_kind.base_sym t in

(*
if name = "accumulate" then print_endline ("Considering ..");
*)
  let id, sr, base_vs, parent_vs, con, domain, base_result, arg_types =
    resolve sym_table bsym_table entry_kind.base_sym bt be arg_types 
  in

  (*
  if List.length rtcr > 0 then begin
    (*
    print_endline (name ^" TYPECLASS INSTANCES REQUIRED (unbound): " ^
    catmap "," string_of_qualified_name rtcr);
    *)
    List.iter
    (fun qn -> let es,ts' = luqn2 i (hack_name qn) in
      print_endline ("With ts = " ^ catmap "," string_of_typecode ts');
      match es with
      | NonFunctionEntry _ -> print_endline "EXPECTED INSTANCES TO BE FUNCTION SET"
      | FunctionEntry es ->
          print_endline ("Candidates " ^ catmap "," string_of_entry_kind es)
    )
    rtcr
  end
  ;
  *)
  (*
  print_endline (id ^ "|-> " ^string_of_myentry bsym_table entry_kind);
  begin
    print_endline ("PARENT VS=" ^ catmap "," (fun (s,i,_)->s^"<"^si i^">") parent_vs);
    print_endline ("base VS=" ^ catmap "," (fun (s,i,_)->s^"<"^si i^">") base_vs);
    print_endline ("sub TS=" ^ catmap "," (sbt bsym_table) entry_kind.sub_ts);
    print_endline ("spec VS=" ^ catmap "," (fun (s,i)->s^"<"^si i^">") entry_kind.spec_vs);
    print_endline ("input TS=" ^ catmap "," (sbt bsym_table) input_ts);
  end
  ;
  *)

  (* these are wrong .. ? or is it just shitty table?
     or is the mismatch due to unresolved variables? *)
  if (List.length base_vs != List.length entry_kind.sub_ts) then begin
    print_endline "WARN: VS != SUB_TS";
    print_endline (id ^ "|-> " ^ string_of_myentry bsym_table entry_kind);
    print_endline ("PARENT VS=" ^
      catmap "," (fun (s,i,_)->s ^ "<"^ string_of_bid i ^ ">") parent_vs);
    print_endline ("base VS=" ^
      catmap "," (fun (s,i,_)->s ^ "<" ^ string_of_bid i ^ ">") base_vs);
    print_endline ("sub TS=" ^ catmap "," (sbt bsym_table) entry_kind.sub_ts);
    print_endline ("spec VS=" ^
      catmap "," (fun (s,i)-> s ^ "<" ^ string_of_bid i ^ ">") entry_kind.spec_vs);
    print_endline ("input TS=" ^ catmap "," (sbt bsym_table) input_ts);
  end;

  (*
  if (List.length spec_vs != List.length input_ts) then print_endline "WARN: SPEC_VS != INPUT_TS";
  *)


  (*
  if con <> btyp_tuple [] then
    print_endline ("type constraint (for "^name^") = " ^ sbt bsym_table con)
  ;
  *)

  (* We need to attempt to find assignments for spec_vs which
     unify the specialised function signature(s) with arguments.

     To do this we match up:

     (a) spec vs variables with input ts values
     (b) signatures with arguments

     which hopefully produces mgu with all the spec_vs variables

     If this succeeds, we plug these into the sub_ts to get
     assignments for base_vs, eliminating the spec vs and
     base vs variables. The parent variables must not be
     eliminated since they act like constants (constructors).

     The resulting ts is then returned, it may contain variables
     from the calling context.

     There is a twist: a polymorphic function calling itself.
     In that case the variables in the calling context
     can be the same as the base variables of the called
     function, but they have to be treated like constants.
     for example here:

     fun f[t] ... => .... f[g t]

     the 't' in f[g t] has to be treated like a constant.

     So the base_vs variables are renamed in the
     function signature where they're dependent.

     The spec vs variables don't need renaming
     because they're anonymous.

     Note that the base_vs variables are eliminated
     from the signature .. so the renaming is pointless! *)

(*
if name = "accumulate" then print_endline "Considering function .. ";
*)
  let spec_result =
    try specialize_domain sr base_vs entry_kind.sub_ts base_result
    with Not_found ->
      clierrx "[flx_bind/flx_overload.ml:1028: E250] " sr ("Failed to bind candidate return type! fn='" ^ name ^
        "', type=" ^ sbt bsym_table base_result)
  in
(*
if name = "accumulate" then print_endline "Making equations";
*)
  (* Step1: make equations for the ts *)
  let mgu = make_equations
    counter
    bsym_table
    id
    sr
    entry_kind
    input_ts
    arg_types
    (specialize_domain sr base_vs entry_kind.sub_ts domain)
    spec_result
  in
(*
if name = "accumulate" then print_endline "maybe got mgu ..";
*)
(*
  let mgu = maybe_specialisation counter bsym_table mgu in
*)
  (* doesn't work .. fails to solve for some vars
     which aren't local vs of the fun .. this case:

     fun g2[w with Eq[w,w]] (x:int,y:int)=> xeq(x,y);

     doesn't solve for w checking xeq(x,y) .. not
     sure why it should tho .. w should be fixed
     already by the instance match .. hmm .. *)
  match mgu with
  | Some mgu ->
(*
if name = "accumulate" then
print_endline "solving mgu";
*)
      solve_mgu
        counter
        bsym_table
        id
        call_sr
        mgu
        entry_kind
        base_vs
        sr
        bt
        con
        arg_types
        parent_vs
        domain
        spec_result
        env_traint

  | None ->
(*
if name = "accumulate" then
      print_endline "No match";
*)
      None


let overload
  counter
  sym_table
  bsym_table
  env
  rs
  bt
  be
  luqn2
  call_sr
  fs
  name
  sufs
  ts
:
  overload_result option
=
(*
if name = "accumulate" then
begin
  print_endline ("Overload " ^ name);
  print_endline ("Argument sigs are " ^ catmap ", " (sbt bsym_table) sufs);
  print_endline (string_of_int (List.length fs) ^ 
     " initial Candidates are:\n" ^ 
    catmap ",\n" (full_string_of_entry_kind sym_table bsym_table) fs ^ "\n");
  print_endline ("Input ts = " ^ catmap ", " (sbt bsym_table) ts);
end;
*)
  let env_traint = btyp_intersect (
    filter_out_units  
    (List.map
      (fun (ix,id,_,_,con) -> 
        if List.mem ix rs.constraint_overload_trail then btyp_tuple [] else
        let rs = { rs with constraint_overload_trail = ix::rs.constraint_overload_trail } in
        let r = match con with | TYP_tuple [] -> Flx_btype.btyp_tuple [] | _ -> bt rs call_sr ix con in
        r
      ) 
      env
    ))
  in

  (* HACK for the moment *)
  let aux i =
    match
      consider
        counter
        sym_table
        bsym_table
        call_sr
        env
        (bt rs)
        be
        name
        i
        ts
        sufs
        env_traint
    with
    | Some x -> 
(*
if name = "accumulate" then print_endline "Found unique result";
*)
      Unique x
    | None -> 
(*
if name = "accumulate" then print_endline "Failed to find result";
*)
      Fail
  in
  let fun_defs = List.map aux fs in

  let candidates =
    List.filter begin fun result ->
      match result with
      | Unique _ -> true
      | Fail -> false
    end fun_defs
  in
(*
  if name = "accumulate" then
    print_endline ("First stage: matching Candidates are:\n" ^ 
      catmap ",\n" (show_result bsym_table) candidates^"\n");
*)
    (*
    print_endline "Got matching candidates .. ";
    *)
  (* start with an empty list, and fold one result
  at a time into it, as follows: if one element
  of the list is greater (more general) than the candidate,
  then add the candidate to the list and remove all
  element greater than the candidate,

  otherwise, if one element of the list is less then
  the candidate, keep the list and discard the candidate.

  The list starts off empty, so that all elements in
  it are vacuously incomparable. It follows either
  the candidate is not less than all the list,
  or not less than all the list: that is, there cannot
  be two element a,b such that a < c < b, since by
  transitivity a < c would follow, contradicting
  the assumption the list contains no ordered pairs.

  If in case 1, all the greater element are removed and c added,
  all the elements must be less or not comparable to c,
  thus the list remains without comparable pairs,
  otherwise in case 2, the list is retained and c discarded
  and so trivially remains unordered.

  *)

  let candidates =
    List.fold_left begin fun oc r ->
      match r with
      | Unique (j,c,jtyp,_,jts) ->
          let rec aux lhs rhs =
            match rhs with
            | [] -> 
                (* return all non-greater elements plus candidate *)
                r::lhs

            | (Unique (i,typ,rtyp,mgu,ts) as x) :: t ->
                begin match compare_sigs bsym_table counter typ c with
                | `Less ->
                    (* Candidate is more general, discard it, retain whole
                     * list *)
                    lhs @ rhs (* keep whole list, discard c *)
                | `Equal ->
                    (* same function .. *)
                    if i = j then 
                      if ts = jts then aux lhs t 
                      else
                        let sym =
                          try Flx_sym_table.find sym_table i with Not_found ->
                            failwith "ovrload BUGGED"
                        in
                        clierrn [call_sr; sym.Flx_sym.sr]
                        (
                          "[resolve_overload] Ambiguous call: Not expecting " ^
                          "equal signatures due to same function" ^
                          "\n fun " ^ name ^ "<"^string_of_bid i ^ ">:" ^ sbt bsym_table typ ^
                          "\n but distinct type variable arguments " ^
                          "\n 1: " ^ catmap "," (sbt bsym_table) jts ^  " returns " ^ sbt bsym_table jtyp ^ 
                          "\n 2: " ^ catmap "," (sbt bsym_table) ts  ^ " returns " ^ sbt bsym_table rtyp ^
                          "\n try using explicit type arguments!" 
                        )
                    else
                    (* this bit is dubious! *)
                    let sym1 =
                      try Flx_sym_table.find sym_table i with Not_found ->
                        failwith "ovrload BUGGED"
                    in
                    let sym2 =
                      try Flx_sym_table.find sym_table j with Not_found ->
                        failwith "overload Bugged"
                    in
                    let isvirtual1 = 
                      let props = 
                        match sym1.Flx_sym.symdef with
                        | SYMDEF_function (_,_,_,props,_) -> props
                        | SYMDEF_fun (props,_,_,_,_,_) -> props
                        | _ -> failwith "OK, dunno what we got!"
                      in
                      List.mem `Virtual props  
                    in
                   let isvirtual2 = 
                      let props = 
                        match sym2.Flx_sym.symdef with
                        | SYMDEF_function (_,_,_,props,_) -> props
                        | SYMDEF_fun (props,_,_,_,_,_) -> props
                        | _ -> failwith "OK, dunno what we got!"
                      in
                      List.mem `Virtual props  
                    in
                    begin match isvirtual1, isvirtual2 with
                    | true, false -> aux lhs t
                    | false, true -> lhs @ rhs
                    | _ ->
                   (* end dubious bit *)
                      clierrn [call_sr; sym2.Flx_sym.sr; sym1.Flx_sym.sr]
                      (
                        "[resolve_overload] Ambiguous call: Not expecting " ^
                        "equal signatures" ^
                        "\n(1) fun " ^ string_of_bid i ^ ":" ^
                        sbt bsym_table typ ^
                        "\n(2) fun " ^ string_of_bid j ^ ":" ^
                        sbt bsym_table c
                      )
                    end

                | `Greater ->
                    (* Candidate is less general: discard this element *)
                    aux lhs t (* discard greater element *)
                | `Incomparable ->
                    (* Candidate is comparable, retail element *)
                    aux (x::lhs) t (* keep element *)
                end
            | Fail :: _ -> assert false
          in
          aux [] oc
      | Fail -> assert false
    end
    []
    candidates
  in
  (*
  if name = "ff" then
  print_endline ("Second stage: most specialised matching candidates are:\n" ^ 
    catmap ",\n" (show_result bsym_table ) candidates^"\n");
  *)
  match candidates with
  | [Unique (i,t,rtyp,mgu,ts)] -> Some (i,t,rtyp,mgu,ts)
  | [] -> None
  | _ ->
      clierrx "[flx_bind/flx_overload.ml:1305: E251] " call_sr
      (
        "Too many candidates match in overloading " ^ name ^
        " with argument types " ^ catmap "," (sbt bsym_table) sufs ^
        "\nOf the matching candidates, the following are most specialised " ^
        "ones are incomparable\n" ^
        catmap "\n" begin function
          | Unique (i,t,_,_,_) ->
              qualified_name_of_index sym_table i ^ "<" ^ string_of_bid i ^
              "> sig " ^ sbt bsym_table t
          | Fail -> assert false
        end candidates ^
        "\nPerhaps you need to define a function more specialised than all " ^
        "these?"
      )

(* FINAL NOTE: THIS STILL WON'T BE ENOUGH: THE SEARCH ALGORITHM
NEEDS TO BE MODIFIED TO FIND **ALL** FUNCTIONS .. alternatively,
keep the results from overload resolution for each scope, and resubmit
in a deeper scope: then if there is a conflict between signatures
(equal or unordered) the closest is taken if that resolves the
conflict
*)

