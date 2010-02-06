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
open Flx_unify
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_foldvars
open Flx_args

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
  in Flx_bexpr.iter ~fe:aux e

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
  Flx_bexe.iter ~fe:(find_uncurry_expr syms bsym_table uncurry_map vs) exe

let find_uncurry_exes syms bsym_table uncurry_map vs exes =
  iter (find_uncurry_exe syms bsym_table uncurry_map vs) exes

let uncurry_expr syms bsym_table uncurry_map vs e =
  let rec aux e = match Flx_bexpr.map ~fe:aux e with
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
  Flx_bexe.map ~fe:(uncurry_expr syms bsym_table uncurry_map vs) exe

let uncurry_exes syms bsym_table uncurry_map vs exes = map (uncurry_exe syms bsym_table uncurry_map vs) exes

let uncurry_gen syms bsym_table child_map : int =
  let ut = Hashtbl.create 97 in (* dummy usage table *)
  let vm = Hashtbl.create 97 in (* dummy varmap *)
  let rl = Hashtbl.create 97 in (* dummy relabel *)
  let uncurry_map = Hashtbl.create 97 in

  (* make the uncurry map *)
  Flx_bsym_table.iter begin fun i bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
        begin match exes with
        | [BEXE_fun_return (sr2,(BEXPR_closure (f,ts),_))]
          when is_child child_map i f && vs_is_ts vs ts
        ->
          let k = fresh_bid syms.counter in
          Hashtbl.add uncurry_map i (f,k,0);
          if syms.compiler_options.print_flag then
          print_endline ("Detected curried function " ^ bsym.Flx_bsym.id ^ "<" ^
            string_of_bid i ^ "> ret child= " ^ string_of_bid f ^ " synth= " ^
            string_of_bid k)

        | _ -> ()
        end

    | _ -> ()
  end bsym_table;

  (* count curried calls to these functions *)
  Flx_bsym_table.iter begin fun i bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_procedure (props,vs,(ps,traint),exes) ->
        find_uncurry_exes syms bsym_table uncurry_map vs exes

    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
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
    (fun acc child -> acc && not (Flx_child.is_ancestor bsym_table child adult))
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

  let uncurry_map = nu_uncurry_map in

  if syms.compiler_options.print_flag then
    Hashtbl.iter begin fun i (c,k,n) ->
      print_endline ("ACTUALLY UNCURRY: Orig " ^ string_of_bid i ^
        " ret child " ^ string_of_bid c ^ " synth " ^ string_of_bid k ^
        " count=" ^ si n);
    end uncurry_map;

  (* synthesise the new functions *)
  let nufuns = ref 0 in
  Hashtbl.iter begin fun i (c,k,n) ->
    begin
      incr nufuns;
      if syms.compiler_options.print_flag then
      print_endline ("UNCURRY: Orig " ^ string_of_bid i ^ " ret child " ^
        string_of_bid c ^ " synth " ^ string_of_bid k ^ " count=" ^ si n);
      let bsymi = Flx_bsym_table.find bsym_table i in
      let bsymc = Flx_bsym_table.find bsym_table c in
      let bsymi_parent = Flx_bsym_table.find_parent bsym_table i in
      let bsymc_parent = Flx_bsym_table.find_parent bsym_table c in
      assert (bsymc_parent = Some i);
      let props, vs, ps, traint, ret, exes =
        match bsymi.Flx_bsym.bbdcl with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> props, vs, ps, traint, ret, exes
        | _ -> assert false
      in
      let fixup vsc psc exesc =
        assert (vs = vsc);
        let extras = Flx_bparameter.get_bids ps in
        let revariable =
          Flx_reparent.reparent_children syms
          (ut,child_map,bsym_table)
          vs (length vsc)
          c (Some k) rl vm true extras
        in
        let revar i = try Hashtbl.find revariable i with Not_found -> i in

        iter
          (fun {pkind=pk; ptyp=t; pid=s; pindex=pi} ->
            let n = revar pi in
            let bbdcl = match pk with
            | `PVal -> bbdcl_val (vs,t)
            | `PVar -> bbdcl_var (vs,t)
            | _ -> failwith "Unimplemented curried fun param not var or val"
            in
            if syms.compiler_options.print_flag then
              print_endline ("New param " ^ s ^ "_uncurry<" ^ string_of_bid n ^
                ">[" ^ catmap
                "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^
                "] <-- " ^ string_of_bid pi ^ ", parent " ^ string_of_bid k ^
                " <-- " ^ string_of_bid i);
            Flx_bsym_table.add bsym_table n {
              Flx_bsym.id=s ^ "_uncurry";
              sr=bsymi.Flx_bsym.sr;
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

        begin
          iter (fun ({pid=s; pindex=i} as p) -> assert (i <> revar i)) (ps @ psc);
        end;

        let ps = map (fun ({pid=s; pindex=i} as p) -> {p with pid=s^"_uncurry"; pindex = revar i}) ps in
        let psc = map (fun ({pindex=i} as p) -> {p with pindex = revar i}) psc in
        let ps = ps @ psc in
        let rec revare e = Flx_bexpr.map ~fi:revar ~fe:revare e in
        let exes = List.map
          (fun exe -> Flx_bexe.map ~fi:revar ~fe:revare exe)
          exesc
        in
        begin match bsymi_parent with
        | Some p -> Flx_child.add_child child_map p k
        | None -> ()
        end
        ;
        ps,exes
      in
      match bsymc.Flx_bsym.bbdcl with
      | BBDCL_function (propsc,vsc,(psc,traintc),retc,exesc) ->
        let ps,exes = fixup vsc psc exesc in
        let bbdcl = bbdcl_function (propsc,vs,(ps,traintc), retc, exes) in
        Flx_bsym_table.add bsym_table k {
          Flx_bsym.id=bsymi.Flx_bsym.id^"_uncurry";
          sr=bsymi.Flx_bsym.sr;
          parent=bsymi.Flx_bsym.parent;
          vs=dfltvs;
          pubmap=Hashtbl.create 0;
          privmap=Hashtbl.create 0;
          dirs=[];
          bbdcl=bbdcl }

      | BBDCL_procedure (propsc,vsc,(psc,traintc),exesc) ->
        let ps,exes = fixup vsc psc exesc in
        let bbdcl = bbdcl_procedure (propsc,vs,(ps,traintc), exes) in
        Flx_bsym_table.add bsym_table k {
          Flx_bsym.id=bsymi.Flx_bsym.id^"_uncurry";
          sr=bsymi.Flx_bsym.sr;
          parent=bsymi.Flx_bsym.parent;
          vs=dfltvs;
          pubmap=Hashtbl.create 0;
          privmap=Hashtbl.create 0;
          dirs=[];
          bbdcl=bbdcl }

      | _ -> assert false
    end
  end uncurry_map;

  (* replace calls *)
  Flx_bsym_table.iter begin fun i bsym ->
    match bsym.Flx_bsym.bbdcl with
    | BBDCL_procedure (props,vs,(ps,traint),exes) ->
        let exes = uncurry_exes syms bsym_table uncurry_map vs exes in
        let bbdcl = bbdcl_procedure (props,vs,(ps,traint),exes) in
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl

    | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
        let exes = uncurry_exes syms bsym_table uncurry_map vs exes in
        let bbdcl = bbdcl_function (props,vs,(ps,traint),ret,exes) in
        Flx_bsym_table.update_bbdcl bsym_table i bbdcl

    | _ -> ()
  end bsym_table;

  !nufuns
