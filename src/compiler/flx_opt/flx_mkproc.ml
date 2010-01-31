open Flx_util
open Flx_ast
open Flx_types
open Flx_btype
open Flx_bexpr
open Flx_bexe
open Flx_bparameter
open Flx_bbdcl
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open List
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_args

let ident x = x

(* THIS CODE JUST COUNTS APPLICATIONS *)
let find_mkproc_expr mkproc_map e =
  let aux e = match e with
  | BEXPR_apply
    (
      (BEXPR_closure (f,ts),_),
      a
    )
    ,ret
    when Hashtbl.mem mkproc_map f ->
    let p,n = Hashtbl.find mkproc_map f in
    Hashtbl.replace mkproc_map f (p,n+1)

  | x -> ()
  in
  Flx_bexpr.iter ~fe:aux e

let find_mkproc_exe mkproc_map exe =
  Flx_bexe.iter ~fe:(find_mkproc_expr mkproc_map) exe

let find_mkproc_exes mkproc_map exes =
  iter (find_mkproc_exe mkproc_map) exes

(* THIS CODE REPLACES APPLICATIONS WITH CALLS *)
let mkproc_expr syms bsym_table sr this mkproc_map vs e =
  let exes = ref [] in
  let rec aux e = match Flx_bexpr.map ~fe:aux e with
  | BEXPR_apply
    (
      (BEXPR_closure (f,ts),_),
      a
    )
    ,ret
    when Hashtbl.mem mkproc_map f ->

    let e =
      (* count replacements *)
      let p,n = Hashtbl.find mkproc_map f in
      Hashtbl.replace mkproc_map f (p,n+1);

      (* create a new variable *)
      let k = fresh_bid syms.counter in
      let vid = "_mkp_" ^ string_of_bid k in
      Flx_bsym_table.add bsym_table k {
        Flx_bsym.id=vid;
        sr=sr;
        parent=Some this;
        vs=dfltvs;
        pubmap=Hashtbl.create 0;
        privmap=Hashtbl.create 0;
        dirs=[];
        bbdcl=BBDCL_var (vs,ret) };

      (* append a pointer to this variable to the argument *)
      let ts' = map (fun (s,i) -> btyp_type_var (i,btyp_type 0)) vs in
      let ptr = BEXPR_ref (k,ts'),btyp_pointer ret in
      let (_,at') as a' = append_args syms bsym_table f a [ptr] in

      (* create a call instruction to the mapped procedure *)
      let call =
        BEXE_call (sr,
          (BEXPR_closure (p,ts),btyp_function (at',btyp_void)),
          a'
        )
      in

      (* record the procedure call *)
      exes := call :: !exes;

      (* replace the original expression with the variable *)
      BEXPR_name (k,ts'),ret
    in e
  | x -> x
  in
  let e = aux e in
  e,rev !exes

let mkproc_exe syms bsym_table sr this mkproc_map vs exe =
  let exes = ref [] in
  let tocall e =
    let e,xs = mkproc_expr syms bsym_table sr this mkproc_map vs e in
    exes := xs @ !exes;
    e
  in
  let exe' = Flx_bexe.map ~fe:tocall exe in
  let exes = !exes @ [exe'] in
  if syms.compiler_options.print_flag then
  begin
    if length exes > 1 then begin
      print_endline ("Unravelling exe=\n" ^ string_of_bexe bsym_table 2 exe);
      print_endline ("Unravelled exes =");
      iter (fun exe -> print_endline (string_of_bexe bsym_table 2 exe)) exes;
    end;
  end;
  exes

let mkproc_exes syms bsym_table sr this mkproc_map vs exes =
  List.concat (map (mkproc_exe syms bsym_table sr this mkproc_map vs) exes)


let proc_exe k exe = match exe with
  | BEXE_fun_return (sr,e)
     -> [BEXE_assign (sr,k,e); BEXE_proc_return sr]

  | BEXE_yield (sr,e)
     ->
     (* yea, this is indeed quite funny .. since yield is just a return which
        doesn't wipe out the continuation address .. i.e. the pc variable..
     *)
     (* failwith "Can't handle yield in procedure made from generator yet! :))"; *)
     (* Argg, who know, it might work lol *)
     [BEXE_assign (sr,k,e); BEXE_proc_return sr]

  | x -> [x]

let proc_exes syms bsym_table k exes = concat (map (proc_exe k) exes)

let mkproc_gen syms bsym_table child_map =
  let ut = Hashtbl.create 97 in (* dummy usage table *)
  let vm = Hashtbl.create 97 in (* dummy varmap *)
  let rl = Hashtbl.create 97 in (* dummy relabel *)
  let mkproc_map = Hashtbl.create 97 in

  let unstackable i =
    let csf = Flx_stack_calls.can_stack_func
      syms
      bsym_table
      child_map
      (Hashtbl.create 97)
      (Hashtbl.create 97)
      i in
    (*
    print_endline ("Function " ^ si i ^ " is " ^ (if csf then "stackable" else "unstackable"));
    *)
    not csf
  in

  (* make the funproc map *)
  Flx_bsym_table.iter begin fun i bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
        let k = fresh_bid syms.counter in
        Hashtbl.add mkproc_map i (k,0);
        if syms.compiler_options.print_flag then
        print_endline ("Detected function to make into a proc? " ^
          bsym.Flx_bsym.id ^ "<" ^ string_of_bid i ^ "> synth= " ^
          string_of_bid k)

    | _ -> ()
  end bsym_table;

  (* count direct applications of these functions *)
  Flx_bsym_table.iter begin fun i bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_procedure (props,vs,(ps,traint),exes) ->
        find_mkproc_exes mkproc_map exes

    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
        find_mkproc_exes mkproc_map exes

    | _ -> ()
  end bsym_table;

  if syms.compiler_options.print_flag then
  Hashtbl.iter begin fun i (k,n) ->
    print_endline ("MAYBE MAKE PROC: Orig " ^ string_of_bid i ^ " synth " ^
      string_of_bid k ^ " count=" ^ si n);
  end mkproc_map;

  (* make a list of the ones actually applied directly *)
  let to_mkproc = ref [] in
  Hashtbl.iter begin fun i (_,n) ->
    if n > 0 then to_mkproc := i :: !to_mkproc
  end mkproc_map;

  (* remove any function which is an ancestor of any other:
     keep the children (arbitrary choice)
  *)
  let isnot_asc adult =
    fold_left
    (fun acc child -> acc && not (Flx_child.is_ancestor bsym_table child adult))
    true !to_mkproc
  in

  let to_mkproc = filter isnot_asc (!to_mkproc) in
  let to_mkproc = filter unstackable to_mkproc in

  let nu_mkproc_map = Hashtbl.create 97 in
  Hashtbl.iter begin fun i j ->
    if mem i to_mkproc
    then begin
      Hashtbl.add nu_mkproc_map i j
      (*
      ;
      print_endline ("Keeping " ^ si i)
      *)
    end else begin
      (*
      print_endline ("Discarding (ancestor) " ^ si i)
      *)
    end
  end mkproc_map;

  let mkproc_map = nu_mkproc_map in

  if syms.compiler_options.print_flag then
  Hashtbl.iter begin fun i (k,n) ->
    print_endline ("ACTUALLY MKPROC: Orig " ^ string_of_bid i ^ " synth " ^
      string_of_bid k ^ " count=" ^ si n);
  end mkproc_map;

  (* synthesise the new functions *)
  let nuprocs = ref 0 in
  Hashtbl.iter begin fun i (k,n) ->
      incr nuprocs;
      if syms.compiler_options.print_flag then
      print_endline ("MKPROC: Orig " ^ string_of_bid i ^ " synth " ^
        string_of_bid k ^ " count=" ^ si n);

      let bsym = Flx_bsym_table.find bsym_table i in
      let props, vs, ps, traint, ret, exes =
        match bsym.Flx_bsym.bbdcl with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> props, vs, ps, traint, ret, exes
        | _ -> assert false
      in

      if syms.compiler_options.print_flag then
      begin
        print_endline "OLD FUNCTION BODY ****************";
        iter (fun exe -> print_endline (string_of_bexe bsym_table 2 exe)) exes;
      end;

      let fixup vsc exesc =
        (* reparent all the children of i to k *)
        let bids = Flx_bparameter.get_bids ps in
        let revariable =
          Flx_reparent.reparent_children syms
          (ut,child_map,bsym_table)
          vs (length vs)
          i (Some k) rl vm true bids
        in
        let revar i = try Hashtbl.find revariable i with Not_found -> i in
        begin
          iter (fun ({pid=s; pindex=i} as p) -> assert (i <> revar i)) ps;
        end;

        (* make new parameter: note the name is remapped to _k_mkproc below *)
        let vix = fresh_bid syms.counter in
        let vdcl = BBDCL_var (vs,btyp_pointer ret) in
        let vid = "_" ^ string_of_bid vix in
        let ps = ps @ [{
          pindex=vix;
          pkind=`PVal;
          ptyp=btyp_pointer ret;
          pid=vid }]
        in

        (* clone old parameters, also happens to create our new one *)
        iter
          (fun {pkind=pk; ptyp=t; pid=s; pindex=pi} ->
            let n = revar pi in
            let bbdcl = match pk with
            | `PVal -> BBDCL_val (vs,t)
            | `PVar -> BBDCL_var (vs,t)
            | _ -> failwith "Unimplemented mkproc fun param not var or val (fixme!)"
            in
            if syms.compiler_options.print_flag then
            print_endline ("New param " ^ s ^ " " ^ string_of_bid n ^ " <-- " ^
              string_of_bid pi ^ ", parent " ^ string_of_bid k ^ " <-- " ^
              string_of_bid i);
            Flx_bsym_table.add bsym_table n {
              Flx_bsym.id=s ^ "_mkproc";
              sr=bsym.Flx_bsym.sr;
              parent=Some k;
              vs=dfltvs;
              pubmap=Hashtbl.create 0;
              privmap=Hashtbl.create 0;
              dirs=[];
              bbdcl=bbdcl };
            Flx_child.add_child child_map k n
          )
          ps
        ;

        (* rename parameter list *)
        let ps = map (fun ({pid=s; pindex=i} as p) -> {p with pid=s^"_mkproc"; pindex = revar i}) ps in
        let rec revare e = Flx_bexpr.map ~fi:revar ~fe:revare e in

        (* remap all the exes to use the new parameters and children *)
        let exes = List.map
          (fun exe -> Flx_bexe.map ~fi:revar ~fe:revare exe)
          exes
        in

        (* add new procedure as child of original function parent *)
        begin match bsym.Flx_bsym.parent with
        | Some p -> Flx_child.add_child child_map p k
        | None -> ()
        end
        ;
        vix,ps,exes
      in

      (* So now, grab fixed up function body, ready for conversion to procedure *)
      let vix,ps,exes = fixup vs exes in

      (* and actually convert it *)
      let ts = map (fun (_,i) -> btyp_type_var (i,btyp_type 0)) vs in
      (* let dv = BEXPR_deref (BEXPR_name (vix,ts),btyp_pointer * ret),btyp_lvalue ret in *)
      let dv = BEXPR_deref (BEXPR_name (vix,ts),btyp_pointer ret),ret in
      let exes = proc_exes syms bsym_table dv exes in

      (* save the new procedure *)
      Flx_bsym_table.add bsym_table k {
        Flx_bsym.id=bsym.Flx_bsym.id ^ "_mkproc";
        sr=bsym.Flx_bsym.sr;
        parent=bsym.Flx_bsym.parent;
        vs=dfltvs;
        pubmap=Hashtbl.create 0;
        privmap=Hashtbl.create 0;
        dirs=[];
        bbdcl=BBDCL_procedure (props,vs,(ps,traint),exes) };

      if syms.compiler_options.print_flag then
      begin
        print_endline "NEW PROCEDURE BODY ****************";
        iter (fun exe -> print_endline (string_of_bexe bsym_table 2 exe)) exes;
      end;
  end mkproc_map;


  (* replace applications *)
  (* DISABLE MODIFICATIONS DURING INITIAL DEPLOYMENT *)
  Flx_bsym_table.iter begin fun i bsym ->
    let mkproc_exes = mkproc_exes
      syms
      bsym_table
      bsym.Flx_bsym.sr
      i
      mkproc_map
    in
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_procedure (props,vs,(ps,traint),exes) ->
        let exes = mkproc_exes vs exes in
        Flx_bsym_table.add bsym_table i { bsym with
          Flx_bsym.bbdcl=BBDCL_procedure (props,vs,(ps,traint),exes) }

    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
        let exes = mkproc_exes vs exes in
        Flx_bsym_table.add bsym_table i { bsym with
          Flx_bsym.bbdcl=BBDCL_function (props,vs,(ps,traint),ret,exes) }

    | _ -> ()
  end bsym_table;
  !nuprocs
  (*
  0 (* TEMPORARY HACK, to prevent infinite recursion *)
  *)
