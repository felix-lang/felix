open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_set
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open List
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_spexes
open Flx_foldvars
open Flx_args


let hfind msg h k =
  try Hashtbl.find h k
  with Not_found ->
    print_endline ("flx_inline Hashtbl.find failed " ^ msg);
    raise Not_found

let rec vs_is_ts vs ts =  match vs,ts with
  | (_,i)::vt,(BTYP_var (j,BTYP_type 0))::tt when i = j -> vs_is_ts vt tt
  | [],[] -> true
  | _ -> false

let id x = x

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
  in iter_tbexpr ignore aux ignore e

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
  iter_bexe ignore (find_uncurry_expr syms bsym_table uncurry_map vs) ignore ignore ignore exe

let find_uncurry_exes syms bsym_table uncurry_map vs exes =
  iter (find_uncurry_exe syms bsym_table uncurry_map vs) exes

let uncurry_expr syms bsym_table uncurry_map vs e =
  let rec aux e = match map_tbexpr id aux id e with
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
      BEXPR_apply ((BEXPR_closure (k,ts),t),ab),ret
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
    BEXE_call (sr,(BEXPR_closure (k,ts),t),ab)
  | x -> x
  in
  map_bexe id (uncurry_expr syms bsym_table uncurry_map vs) id id id exe

let uncurry_exes syms bsym_table uncurry_map vs exes = map (uncurry_exe syms bsym_table uncurry_map vs) exes

let uncurry_gen syms bsym_table child_map : int =
  let ut = Hashtbl.create 97 in (* dummy usage table *)
  let vm = Hashtbl.create 97 in (* dummy varmap *)
  let rl = Hashtbl.create 97 in (* dummy relabel *)
  let uncurry_map = Hashtbl.create 97 in

  (* make the uncurry map *)
  Hashtbl.iter
  (fun i (id,parent,sr,bbdcl) -> match bbdcl with
  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    begin match exes with
    | [BEXE_fun_return (sr2,(BEXPR_closure (f,ts),_))]
      when is_child child_map i f && vs_is_ts vs ts
      ->
      let k = !(syms.counter) in incr (syms.counter);
      Hashtbl.add uncurry_map i (f,k,0);
      if syms.compiler_options.print_flag then
      print_endline ("Detected curried function " ^ id ^ "<" ^ string_of_bid i
        ^ "> ret child= " ^ string_of_bid f ^ " synth= " ^ string_of_bid k)

    | _ -> ()
    end

  | _ -> ()
  )
  bsym_table
  ;

  (* count curried calls to these functions *)
  Hashtbl.iter
  (fun i (id,parent,sr,bbdcl) -> match bbdcl with
  | BBDCL_procedure (props,vs,(ps,traint),exes) ->
    find_uncurry_exes syms bsym_table uncurry_map vs exes

  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    find_uncurry_exes syms bsym_table uncurry_map vs exes
  | _ -> ()
  )
  bsym_table
  ;

  if syms.compiler_options.print_flag then
  Hashtbl.iter
  (fun i (c,k,n) ->
     print_endline ("MAYBE UNCURRY: Orig " ^ string_of_bid i ^ " ret child " ^
      string_of_bid c ^ " synth " ^ string_of_bid k ^ " count=" ^
      string_of_int n);
  )
  uncurry_map
  ;

  (* make a list of the ones actually called in curried form *)
  let to_uncurry = ref [] in
  Hashtbl.iter
  (fun i (_,_,n) ->
    if n > 0 then to_uncurry := i :: !to_uncurry
  )
  uncurry_map
  ;

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
  Hashtbl.iter
  (fun i j ->
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
  )
  uncurry_map;

  let uncurry_map = nu_uncurry_map in

  if syms.compiler_options.print_flag then
  Hashtbl.iter
  (fun i (c,k,n) ->
    print_endline ("ACTUALLY UNCURRY: Orig " ^ string_of_bid i ^
      " ret child " ^ string_of_bid c ^ " synth " ^ string_of_bid k ^
      " count=" ^ si n);
  )
  uncurry_map
  ;


  (* synthesise the new functions *)
  let nufuns = ref 0 in
  Hashtbl.iter
  (fun i (c,k,n) ->
    begin
      incr nufuns;
      if syms.compiler_options.print_flag then
      print_endline ("UNCURRY: Orig " ^ string_of_bid i ^ " ret child " ^
        string_of_bid c ^ " synth " ^ string_of_bid k ^ " count=" ^ si n);
      let idm,parent,sr,bbdcl = Hashtbl.find bsym_table i in
      let idc,parentc,src,bbdclc = Hashtbl.find bsym_table c in
      assert (parentc = Some i);
      let props, vs, ps, traint, ret, exes =
        match bbdcl with
        | BBDCL_function (props,vs,(ps,traint),ret,exes) -> props, vs, ps, traint, ret, exes
        | _ -> assert false
      in
      let fixup vsc psc exesc =
        assert (vs = vsc);
        let extras = map (fun {pindex=i}->i) ps in
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
            | `PVal -> BBDCL_val (vs,t)
            | `PVar -> BBDCL_var (vs,t)
            | _ -> failwith "Unimplemented curried fun param not var or val"
            in
            if syms.compiler_options.print_flag then
              print_endline ("New param " ^ s ^ "_uncurry<" ^ string_of_bid n ^
                ">[" ^ catmap
                "," (fun (s,i) -> s ^ "<" ^ string_of_bid i ^ ">") vs ^
                "] <-- " ^ string_of_bid pi ^ ", parent " ^ string_of_bid k ^
                " <-- " ^ string_of_bid i);
            Hashtbl.add bsym_table n (s ^ "_uncurry",Some k,sr,bbdcl);
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
        let rec revare e = map_tbexpr revar revare id e in
        let exes = map (fun exe -> map_bexe revar revare id id id exe) exesc in
        begin match parent with
        | Some p -> Flx_child.add_child child_map p k
        | None -> ()
        end
        ;
        ps,exes
      in
      match bbdclc with
      | BBDCL_function (propsc,vsc,(psc,traintc),retc,exesc) ->
        let ps,exes = fixup vsc psc exesc in
        let bbdcl = BBDCL_function (propsc,vs,(ps,traintc), retc, exes) in
        Hashtbl.add bsym_table k (idm^"_uncurry",parent,sr,bbdcl)

      | BBDCL_procedure (propsc,vsc,(psc,traintc),exesc) ->
        let ps,exes = fixup vsc psc exesc in
        let bbdcl = BBDCL_procedure (propsc,vs,(ps,traintc), exes) in
        Hashtbl.add bsym_table k (idm^"_uncurry",parent,sr,bbdcl)

      | _ -> assert false
    end
  )
  uncurry_map
  ;
  (* replace calls *)
  Hashtbl.iter
  (fun i (id,parent,sr,bbdcl) -> match bbdcl with
  | BBDCL_procedure (props,vs,(ps,traint),exes) ->
    let exes = uncurry_exes syms bsym_table uncurry_map vs exes in
    Hashtbl.replace bsym_table i
      (id,parent,sr,BBDCL_procedure (props,vs,(ps,traint),exes))

  | BBDCL_function (props,vs,(ps,traint),ret,exes) ->
    let exes = uncurry_exes syms bsym_table uncurry_map vs exes in
    Hashtbl.replace bsym_table i
      (id,parent,sr,BBDCL_function (props,vs,(ps,traint),ret,exes))
  | _ -> ()
  )
  bsym_table
  ;

  !nufuns
