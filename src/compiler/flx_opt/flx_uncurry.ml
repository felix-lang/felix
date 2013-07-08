open List

open Flx_args
open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_exceptions
open Flx_foldvars
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_print
open Flx_reparent
open Flx_set
open Flx_spexes
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util

let rec vs_is_ts vs ts =  match vs,ts with
  | (_,i)::vt,(BTYP_type_var (j,BTYP_type 0))::tt when i = j -> vs_is_ts vt tt
  | [],[] -> true
  | _ -> false

let find_uncurry_expr syms bsym_table uncurry_map vs e =
  let aux e = match e with
  | BEXPR_apply
    (
      (
        BEXPR_apply
        (
          (BEXPR_closure (f,ts),_),
          ((a_e,a_t) as a)
        ),
        t
      ),
      ((b_e,b_t) as b)
    ),ret
    when Hashtbl.mem uncurry_map f && vs_is_ts vs ts ->
    let c,k,n = Hashtbl.find uncurry_map f in
    Hashtbl.replace uncurry_map f (c,k,n+1)

  | x -> ()
  in Flx_bexpr.iter ~f_bexpr:aux e

let find_uncurry_exe syms bsym_table uncurry_map vs exe =
  begin match exe with
  | BEXE_call
    (
      sr,
      (
        BEXPR_apply
        (
          (BEXPR_closure (f,ts),_),
          ((a_e,a_t) as a)
        ),
        t
      ),
      ((b_e,b_t) as b)
    )
    when Hashtbl.mem uncurry_map f && vs_is_ts vs ts ->
    let c,k,n = Hashtbl.find uncurry_map f in
    Hashtbl.replace uncurry_map f (c,k,n+1)
  | x -> ()
  end
  ;
  Flx_bexe.iter ~f_bexpr:(find_uncurry_expr syms bsym_table uncurry_map vs) exe

let find_uncurry_exes syms bsym_table uncurry_map vs exes =
  List.iter (find_uncurry_exe syms bsym_table uncurry_map vs) exes

let uncurry_expr syms bsym_table uncurry_map vs e =
  let rec aux e = match Flx_bexpr.map ~f_bexpr:aux e with
  | BEXPR_apply
    (
      (
        BEXPR_apply
        (
          (BEXPR_closure (f,ts),_),
          ((a_e,a_t) as a)
        ),
        t
      ),
      ((b_e,b_t) as b)
    ),ret
    when Hashtbl.mem uncurry_map f && vs_is_ts vs ts ->
    let e =
      let c,k,n = Hashtbl.find uncurry_map f in
      Hashtbl.replace uncurry_map f (c,k,n+1);
      let ab = merge_args syms bsym_table f c a b in
      bexpr_apply ret ((bexpr_closure t (k,ts)),ab)
    in aux e
  | x -> x
  in aux e

let uncurry_exe syms bsym_table uncurry_map vs exe =
  let exe = match exe with
  | BEXE_call
    (
      sr,
      (
        BEXPR_apply
        (
          (BEXPR_closure (f,ts),_),
          ((a_e,a_t) as a)
        ),
        t
      ),
      ((b_e,b_t) as b)
    )
    when Hashtbl.mem uncurry_map f && vs_is_ts vs ts ->
    let c,k,n = Hashtbl.find uncurry_map f in
    Hashtbl.replace uncurry_map f (c,k,n+1);
    let ab = merge_args syms bsym_table f c a b in
    bexe_call (sr,(bexpr_closure t (k,ts)),ab)
  | x -> x
  in
  Flx_bexe.map ~f_bexpr:(uncurry_expr syms bsym_table uncurry_map vs) exe

let uncurry_exes syms bsym_table uncurry_map vs exes =
  List.map (uncurry_exe syms bsym_table uncurry_map vs) exes

(** make the uncurry map *)
let make_uncurry_map syms bsym_table =
  let uncurry_map = Hashtbl.create 97 in

  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,vs,_,_,[BEXE_fun_return (_,(BEXPR_closure (f,ts),_))])
      when Flx_bsym_table.is_child bsym_table i f && vs_is_ts vs ts ->
        let k = fresh_bid syms.counter in
        Hashtbl.add uncurry_map i (f,k,0);
        if syms.compiler_options.print_flag then
        print_endline ("Detected curried function " ^ Flx_bsym.id bsym ^ "<" ^
          string_of_bid i ^ "> ret child= " ^ string_of_bid f ^ " synth= " ^
          string_of_bid k)

    | _ -> ()
  end bsym_table;

  (* count curried calls to these functions *)
  Flx_bsym_table.iter begin fun i _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (_,vs,_,_,exes) ->
        find_uncurry_exes syms bsym_table uncurry_map vs exes
    | _ -> ()
  end bsym_table;

  if syms.compiler_options.print_flag then
    Hashtbl.iter begin fun i (c,k,n) ->
      print_endline ("MAYBE UNCURRY: Orig " ^ string_of_bid i ^ " ret child " ^
        string_of_bid c ^ " synth " ^ string_of_bid k ^ " count=" ^
        string_of_int n);
    end uncurry_map;

  (* make a list of the ones actually called in curried form *)
  let to_uncurry = ref [] in
  Hashtbl.iter begin fun i (_,_,n) ->
    if n > 0 then to_uncurry := i :: !to_uncurry
  end uncurry_map;

  (* remove any function which is an ancestor of any other:
     keep the children (arbitrary choice)
  *)
  let isnot_asc adult =
    fold_left
    (fun acc child -> acc && not (Flx_bsym_table.is_ancestor bsym_table child adult))
    true !to_uncurry
  in

  let to_uncurry = filter isnot_asc (!to_uncurry) in

  let nu_uncurry_map = Hashtbl.create 97 in
  Hashtbl.iter begin fun i j ->
    if mem i to_uncurry
    then begin
      Hashtbl.add nu_uncurry_map i j
      (*
      ;
      print_endline ("Keeping " ^ si i)
      *)
    end else begin
      (*
      print_endline ("Discarding (ancestor) " ^ si i)
      *)
    end
  end uncurry_map;

  if syms.compiler_options.print_flag then
    Hashtbl.iter begin fun i (c,k,n) ->
      print_endline ("ACTUALLY UNCURRY: Orig " ^ string_of_bid i ^
        " ret child " ^ string_of_bid c ^ " synth " ^ string_of_bid k ^
        " count=" ^ si n);
    end uncurry_map;

  nu_uncurry_map


let fixup_function
  syms
  bsym_table
  ut vm rl
  i c k
  bsymi bsymi_parent
  vsc psc exesc
=
  let vs, ps =
    match Flx_bsym.bbdcl bsymi with
    | BBDCL_fun (_,vs,(ps,_),_,_) -> vs, ps
    | _ -> assert false
  in

  (* Make sure our variables are the same. *)
  assert (vs = vsc);

  (* Create a table that will help with us remapping the parameters. *)
  let revariable = Flx_reparent.reparent_children
    syms
    ut bsym_table
    vs
    (length vsc)
    c
    (Some k)
    rl
    vm
    true
    (Flx_bparameter.get_bids ps)
  in

  (* Helper function to look up a reparented index. *)
  let revar i =
    try Hashtbl.find revariable i with Not_found -> i
  in

  (* Create new bound symbols for the parameters. *)
  List.iter begin fun { pkind=pk; ptyp=t; pid=s; pindex=pi } ->
    let n = revar pi in
    let bbdcl = match pk with
    | `PVal -> bbdcl_val (vs,t,`Val)
    | `PVar -> bbdcl_val (vs,t,`Var)
    | `PRef -> bbdcl_val (vs,t,`Ref)
    | _ -> failwith "Unimplemented curried fun param not var or val"
    in

    if syms.compiler_options.print_flag then 
      print_endline ("New param " ^ s ^ "_uncurry<" ^ string_of_bid n ^
        ">[" ^ catmap
        "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^
        "] <-- " ^ string_of_bid pi ^ ", parent " ^ string_of_bid k ^
        " <-- " ^ string_of_bid i);

    Flx_bsym_table.add bsym_table n (Some k)
      (Flx_bsym.create ~sr:(Flx_bsym.sr bsymi) (s ^ "_uncurry") bbdcl)
  end ps;

  (* Make sure the parameter indices don't equal the remapped index. *)
  assert (List.for_all (fun ({ pindex=i }) -> (i <> revar i)) ps);
  assert (List.for_all (fun ({ pindex=i }) -> (i <> revar i)) psc);

  (* Update the parent's parameter indices. *)
  let ps =
    List.map begin fun ({ pid=s; pindex=i } as p) ->
      { p with pid=s ^ "_uncurry"; pindex=revar i }
    end ps
  in

  (* Update the child's parameter list. *)
  let psc = List.map
    (fun ({ pindex=i} as p) -> {p with pindex=revar i })
    psc
  in

  (* Finally, merge the parent and child parameters. *)
  let ps = ps @ psc in
  if syms.compiler_options.print_flag then
  List.iter (fun {pkind=pk; pid=s; ptyp=pt}->
    print_endline ("param " ^ string_of_param_kind pk ^" " ^ s ^ ":" ^ sbt bsym_table pt))
    ps
  ;


  (* Update the child's expressions and executables. *)
  let rec revare e = Flx_bexpr.map ~f_bid:revar ~f_bexpr:revare e in
  let exes = List.map
    (fun exe -> Flx_bexe.map ~f_bid:revar ~f_bexpr:revare exe)
    exesc
  in

  vs, ps, exes


let synthesize_function syms bsym_table ut vm rl i (c, k, n) =
  (* As a safety check, make sure that the child has the parent as the
   * parent. *)
  assert (Flx_bsym_table.find_parent bsym_table c = Some i);

  let bsymi_parent, bsymi = Flx_bsym_table.find_with_parent bsym_table i in

  if syms.compiler_options.print_flag then
    print_endline ("UNCURRY: Orig " ^ Flx_bsym.id bsymi ^"<" ^ string_of_bid i ^ "> ret child " ^
      string_of_bid c ^ " synth " ^ string_of_bid k ^ " count=" ^ si n);


  (* Add a placeholder symbol that will be updated later. *)
  Flx_bsym_table.add bsym_table k bsymi_parent
    (Flx_bsym.create
      ~sr:(Flx_bsym.sr bsymi)
      (Flx_bsym.id bsymi ^ "_uncurry")
      (Flx_bsym.bbdcl bsymi));

  let fixup_function = fixup_function
    syms
    bsym_table
    ut vm rl
    i c k
    bsymi bsymi_parent
  in

  (* Add the new function or procedure. *)
  let bbdcl =
    match Flx_bsym_table.find_bbdcl bsym_table c with
    | BBDCL_fun (propsc,vsc,(psc,traintc),retc,exesc) ->
      if syms.compiler_options.print_flag then
      print_endline ("Properties " ^ Flx_print.string_of_properties propsc);
      let vs,ps,exes = fixup_function vsc psc exesc in
      bbdcl_fun (propsc,vs,(ps,traintc),retc,exes)
    | _ -> assert false
  in

  Flx_bsym_table.update_bbdcl bsym_table k bbdcl


(** synthesise the new functions *)
let synthesize_functions syms bsym_table uncurry_map =
  let ut = Hashtbl.create 97 in (* dummy usage table *)
  let vm = Hashtbl.create 97 in (* dummy varmap *)
  let rl = Hashtbl.create 97 in (* dummy relabel *)

  Hashtbl.iter
    (synthesize_function syms bsym_table ut vm rl)
    uncurry_map


(** replace calls *)
let replace_calls syms bsym_table uncurry_map =
  Flx_bsym_table.iter begin fun bid _ bsym ->
    match Flx_bsym.bbdcl bsym with
    | BBDCL_fun (props,vs,ps,ret,exes) ->
        let exes = uncurry_exes syms bsym_table uncurry_map vs exes in
        let bbdcl = bbdcl_fun (props,vs,ps,ret,exes) in
        Flx_bsym_table.update_bbdcl bsym_table bid bbdcl

    | _ -> ()
  end bsym_table


let uncurry_gen syms bsym_table =
  let uncurry_map = make_uncurry_map syms bsym_table in

  synthesize_functions syms bsym_table uncurry_map;
  replace_calls syms bsym_table uncurry_map;

  (* Return how many new functions we've created. *)
  Hashtbl.length uncurry_map
