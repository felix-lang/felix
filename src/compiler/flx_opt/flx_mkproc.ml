open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open List
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_args

let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_inline Hashtbl.find failed " ^ msg);
    raise Not_found

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
  iter_tbexpr ignore aux ignore e

let find_mkproc_exe mkproc_map exe =
  iter_bexe ignore (find_mkproc_expr mkproc_map) ignore ignore ignore exe

let find_mkproc_exes mkproc_map exes =
  iter (find_mkproc_exe mkproc_map) exes

(* THIS CODE REPLACES APPLICATIONS WITH CALLS *)
let mkproc_expr syms bsym_table sr this mkproc_map vs e =
  let exes = ref [] in
  let rec aux e = match map_tbexpr ident aux ident e with
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
      let k = !(syms.counter) in incr (syms.counter);
      let vid = "_mkp_" ^ string_of_bid k in
      let vardecl = BBDCL_var (vs,ret) in
      Hashtbl.add bsym_table k (vid,Some this,sr,vardecl);

      (* append a pointer to this variable to the argument *)
      let ts' = map (fun (s,i) -> BTYP_var (i,BTYP_type 0)) vs in
      let ptr = BEXPR_ref (k,ts'),BTYP_pointer ret in
      let (_,at') as a' = append_args syms bsym_table f a [ptr] in

      (* create a call instruction to the mapped procedure *)
      let call : bexe_t =
        BEXE_call (sr,
          (BEXPR_closure (p,ts),BTYP_function (at',BTYP_void)),
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
  let exe' = map_bexe ident tocall ident ident ident exe in
  let exes = !exes @ [exe'] in
  if syms.compiler_options.print_flag then
  begin
    if length exes > 1 then begin
      print_endline ("Unravelling exe=\n" ^ string_of_bexe syms.sym_table bsym_table 2 exe);
      print_endline ("Unravelled exes =");
      iter (fun exe -> print_endline (string_of_bexe syms.sym_table bsym_table 2 exe)) exes;
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
  Hashtbl.iter
  (fun i (id,parent,sr,bbdcl) -> match bbdcl with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let k = !(syms.counter) in incr (syms.counter);
    Hashtbl.add mkproc_map i (k,0);
    if syms.compiler_options.print_flag then
    print_endline ("Detected function to make into a proc? " ^ id ^ "<" ^
      string_of_bid i ^ "> synth= " ^ string_of_bid k)

  | _ -> ()
  )
  bsym_table
  ;

  (* count direct applications of these functions *)
  Hashtbl.iter
  (fun i (id,parent,sr,bbdcl) -> match bbdcl with
  | BBDCL_procedure (props,vs,(ps,traint),exes) ->
    find_mkproc_exes mkproc_map exes

  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    find_mkproc_exes mkproc_map exes
  | _ -> ()
  )
  bsym_table
  ;

  if syms.compiler_options.print_flag then
  Hashtbl.iter
  (fun i (k,n) ->
    print_endline ("MAYBE MAKE PROC: Orig " ^ string_of_bid i ^ " synth " ^
      string_of_bid k ^ " count=" ^ si n);
  )
  mkproc_map
  ;

  (* make a list of the ones actually applied directly *)
  let to_mkproc = ref [] in
  Hashtbl.iter
  (fun i (_,n) ->
    if n > 0 then to_mkproc := i :: !to_mkproc
  )
  mkproc_map
  ;

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
  Hashtbl.iter
  (fun i j ->
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
  )
  mkproc_map;

  let mkproc_map = nu_mkproc_map in

  if syms.compiler_options.print_flag then
  Hashtbl.iter
  (fun i (k,n) ->
    print_endline ("ACTUALLY MKPROC: Orig " ^ string_of_bid i ^ " synth " ^
      string_of_bid k ^ " count=" ^ si n);
  )
  mkproc_map
  ;

  (* synthesise the new functions *)
  let nuprocs = ref 0 in
  Hashtbl.iter
  (fun i (k,n) ->
      incr nuprocs;
      if syms.compiler_options.print_flag then
      print_endline ("MKPROC: Orig " ^ string_of_bid i ^ " synth " ^
        string_of_bid k ^ " count=" ^ si n);

      let idm,parent,sr,bbdcl = Hashtbl.find bsym_table i in
      let props, vs, ps, traint, ret, exes =
        match bbdcl with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> props, vs, ps, traint, ret, exes
        | _ -> assert false
      in

      if syms.compiler_options.print_flag then
      begin
        print_endline "OLD FUNCTION BODY ****************";
        iter (fun exe -> print_endline (string_of_bexe syms.sym_table bsym_table 2 exe)) exes;
      end;

      let fixup vsc exesc =

        (* reparent all the children of i to k *)
        let extras = map (fun {pindex=i}->i) ps in
        let revariable =
          Flx_reparent.reparent_children syms
          (ut,child_map,bsym_table)
          vs (length vs)
          i (Some k) rl vm true extras
        in
        let revar i = try Hashtbl.find revariable i with Not_found -> i in
        begin
          iter (fun ({pid=s; pindex=i} as p) -> assert (i <> revar i)) ps;
        end;

        (* make new parameter: note the name is remapped to _k_mkproc below *)
        let vix = !(syms.counter) in incr (syms.counter);
        let vdcl = BBDCL_var (vs,BTYP_pointer ret) in
        let vid = "_" ^ string_of_bid vix in
        let ps = ps @ [{pindex=vix; pkind=`PVal; ptyp=BTYP_pointer ret; pid=vid}] in

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
            Hashtbl.add bsym_table n (s ^ "_mkproc",Some k,sr,bbdcl);
            Flx_child.add_child child_map k n
          )
          ps
        ;

        (* rename parameter list *)
        let ps = map (fun ({pid=s; pindex=i} as p) -> {p with pid=s^"_mkproc"; pindex = revar i}) ps in
        let rec revare e = map_tbexpr revar revare ident e in

        (* remap all the exes to use the new parameters and children *)
        let exes = map (fun exe -> map_bexe revar revare ident ident ident exe) exes in

        (* add new procedure as child of original function parent *)
        begin match parent with
        | Some p -> Flx_child.add_child child_map p k
        | None -> ()
        end
        ;
        vix,ps,exes
      in

      (* So now, grab fixed up function body, ready for conversion to procedure *)
      let vix,ps,exes = fixup vs exes in

      (* and actually convert it *)
      let ts = map (fun (_,i) -> BTYP_var (i,BTYP_type 0)) vs in
      (* let dv = BEXPR_deref (BEXPR_name (vix,ts),BTYP_pointer * ret),BTYP_lvalue ret in *)
      let dv = BEXPR_deref (BEXPR_name (vix,ts),BTYP_pointer ret),ret in
      let exes = proc_exes syms bsym_table dv exes in

      (* save the new procedure *)
      let bbdcl = BBDCL_procedure (props,vs,(ps,traint), exes) in
      Hashtbl.add bsym_table k (idm^"_mkproc",parent,sr,bbdcl);

      if syms.compiler_options.print_flag then
      begin
        print_endline "NEW PROCEDURE BODY ****************";
        iter (fun exe -> print_endline (string_of_bexe syms.sym_table bsym_table 2 exe)) exes;
      end;

  )
  mkproc_map
  ;


  (* replace applications *)
  (* DISABLE MODIFICATIONS DURING INITIAL DEPLOYMENT *)
  Hashtbl.iter
  (fun i (id, parent, sr, bbdcl) -> match bbdcl with
  | BBDCL_procedure (props,vs,(ps,traint),exes) ->
    let exes = mkproc_exes syms bsym_table sr i mkproc_map vs exes in
    (*
    ()
    *)
    Hashtbl.replace bsym_table i
      (id,parent,sr,BBDCL_procedure (props,vs,(ps,traint),exes))

  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let exes = mkproc_exes syms bsym_table sr i mkproc_map vs exes in
    (*
    ()
    *)
    Hashtbl.replace bsym_table i
      (id,parent,sr,BBDCL_function (props,vs,(ps,traint),ret,exes))

  | _ -> ()
  )
  bsym_table
  ;
  !nuprocs
  (*
  0 (* TEMPORARY HACK, to prevent infinite recursion *)
  *)
