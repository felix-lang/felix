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
open Flx_reparent
open Flx_call
open Flx_bid

type submode_t = [`Eager | `Lazy]

(* this only updates the uses table not the usedby table,
  because inlining changes usage (obviously).
  we need it in particular for the is_recursive test,
  so that tail recursions which have been eliminated
  won't cause the test to return a false positive
*)

(* FIXME: doesn't count usage of parameter constraint! *)
let recal_exes_usage uses sr i ps exes =
  (*
  print_endline ("Recal usage of "^ si i^", this code:\n" ^ catmap "\n" (sbx sym_table) exes);
  *)
  (* delete old entry *)
  (try Hashtbl.remove uses i with Not_found -> ());
  Flx_bparams.piter (Flx_call.cal_param_usage uses sr i) ps;
  iter (Flx_call.cal_exe_usage uses i) exes

let is_tailed ps exes =
  try iter
  (function
    | BEXE_init(_,i,_) when mem i ps -> raise Not_found
    | _ -> ()
  )
  exes;
  false
  with Not_found -> true

let ident x = x

(* Heavy inlining routine. This routine can inline
any procedure. The basic operation is emit the body
of the target procedure. We have to do the following to
make it all work.

(1) Each declared label is replaced by a fresh one,
and all jumps to these labels modified accordingly.

(2) Variables are replaced by fresh ones. This requires
making additions to the output bound tables. References
to the variables are modified. Note the parent is the
caller now.

(3) Paremeters are replaced like variables, initialised
by the arguments.

(4) Any type variables instantiated by the call must
also be instantiated in body expressions, as well as
the typing of any generated variables.

(5) If the procedure has any nested procedures, they
also must be replaced in toto by fresh ones, reparented
to the caller so that any calls to them will access
the fresh variables in the caller.

Note that the cache of children of the caller will
be wrong after the inlining (it may have acquired new
variables or procedure children).

Note that this inlining procedure is NOT recursive!
Its a flat one level inlining. This ensures recursive
calls don't cause an infinite unrolling, and hopefully
prevent gross bloat.
*)

let idt t = t

let rec rpl syms argmap x =
  match Flx_bexpr.map ~f_bexpr:(rpl syms argmap) x with
  (* No need to check ts or type here *)
  | (BEXPR_varname (i,_),_) as x ->
    (try
      let x' = Hashtbl.find argmap i in
      (*
      print_endline ("Replacing variable " ^ si i ^ " with " ^ sbe sym_table x');
      *)
      x'
      with Not_found -> x)
  | x -> x

let subarg syms bsym_table argmap exe =
  Flx_bexe.map ~f_bexpr:(rpl syms argmap) exe

(* NOTE: result is in reversed order *)
let gen_body syms uses bsym_table id
  ps revariable exes argument
  sr caller callee inline_method props
=
  if syms.compiler_options.Flx_options.print_flag then
  print_endline ("Gen body caller = " ^ Flx_bsym.id (Flx_bsym_table.find bsym_table caller) ^ 
    "<" ^string_of_bid caller ^
    ">, callee=" ^ id ^ "<" ^ string_of_bid callee ^ ">"
  );
  (*
  let argument = Flx_bexpr.reduce bsym_table argument in
  *)
  let psis = Flx_bparams.get_bids ps in

  (* NOTE: this is the inline method for val's ONLY.
    If a parameter is a var, it is inlined eagerly no
    matter what .. however we can't handle that yet,
    so we have to switch to eager evaluation if ANY
    of the parameters is a var.
  *)

  (*
  print_endline ("Inline " ^ id ^ ", input inline method = " ^ match inline_method with
  | `Eager -> "Eager"
  | `Lazy -> "Lazy"
  );

  print_endline ("Recursive? " ^ if Flx_call.is_recursive uses callee then "YES" else "NO");
  print_endline ("Tailed? " ^ if is_tailed psis exes then "YES" else "NO");
  *)

  let inline_method = match inline_method with
  | `Lazy ->
    if
      Flx_call.is_recursive uses callee ||
      is_tailed psis exes
    then `Eager
    else `Lazy
      (*
      fold_left (fun imeth {pkind=k} ->
        match imeth, k with
        | _, `PVar -> `Eager
        | x,_ -> x
        )
      `Lazy ps
      *)
  | `Eager -> `Eager
  in

  (* HACKERY *)

  (*
  print_endline ("Inlining " ^ si callee ^ " into " ^ si caller);
  *)
  if syms.compiler_options.Flx_options.print_flag then
  begin begin match inline_method with
  | `Eager ->
      print_endline ("Eager INLINING " ^ id ^ "<" ^
        string_of_bid callee ^ ">(" ^ sbe bsym_table argument ^
        ") into " ^ string_of_bid caller ^ " .. INPUT:");
  | `Lazy ->
      print_endline ("Lazy INLINING " ^ id ^ "<" ^ string_of_bid callee ^ ">(" ^
        sbe bsym_table argument ^") into " ^
        string_of_bid caller ^ " .. INPUT:");
  end
  ;
  iter (fun x -> print_endline (string_of_bexe bsym_table 0 x)) exes;
  end
  ;
  let paramtype  = Flx_bparams.get_btype ps in
  let ge e = 
(*
     print_endline ("Remap expr " ^ sbe bsym_table e);
*)
     let result = remap_expr syms bsym_table revariable e  in
(*
     print_endline ("Remap DONE result " ^ sbe bsym_table result);
*)
     result
  in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let end_label_uses = ref 0 in
  let end_index = fresh_bid syms.counter in
  let end_label =
    "_end_" ^ (string_of_bid end_index)
  in


  let remap exe =
(*
  print_endline ("Remap exe " ^ string_of_bexe bsym_table 0 exe);
*)
  let result = 
  match exe with
  | BEXE_axiom_check _ -> assert false
  | BEXE_call_prim (sr,i,ts,e2)  ->  
    let fixup i ts =
      try
        let j= Hashtbl.find revariable i in
        j, ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [bexe_call_prim (sr,i,ts, ge e2)]

  | BEXE_call_direct (sr,i,ts,e2)  -> 
    let fixup i ts =
      try
        let j= Hashtbl.find revariable i in
        j, ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [bexe_call_direct (sr,i,ts, ge e2)]

  | BEXE_jump_direct (sr,i,ts,e2)  ->
    let fixup i ts =
      try
        let j= Hashtbl.find revariable i in
        j, ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [bexe_jump_direct (sr,i,ts, ge e2)]

  | BEXE_call_stack (sr,i,ts,e2)  -> assert false
  | BEXE_call (sr,e1,e2)  -> [bexe_call (sr,ge e1, ge e2)]
  | BEXE_call_with_trap (sr,e1,e2)  -> [bexe_call_with_trap (sr,ge e1, ge e2)]
  | BEXE_jump (sr,e1,e2)  -> [bexe_jump (sr, ge e1, ge e2)]
  | BEXE_assert (sr,e) -> [bexe_assert (sr, ge e)]
  | BEXE_axiom_check2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e1 -> Some (ge e1) | None -> None in
    [bexe_axiom_check2 (sr, sr2, e1,ge e2)]
  | BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e1 -> Some (ge e1) | None -> None in
    [bexe_assert2 (sr, sr2, e1,ge e2)]

  | BEXE_ifgoto (sr,e,idx) -> [bexe_ifgoto (sr,ge e, revar idx)]
  | BEXE_cgoto (sr,e) -> [bexe_cgoto (sr,ge e)]
  | BEXE_ifcgoto (sr,e1,e2) -> [bexe_ifcgoto (sr,ge e1, ge e2)]

  | BEXE_fun_return (sr,e) -> [bexe_fun_return (sr, ge e)]
  | BEXE_yield (sr,e) -> [bexe_yield (sr, ge e)]
  | BEXE_assign (sr,e1,e2) -> [bexe_assign (sr, ge e1, ge e2)]
  | BEXE_storeat (sr,e1,e2) -> [bexe_storeat (sr, ge e1, ge e2)]
  | BEXE_init (sr,i,e) -> [bexe_init (sr,revar i, ge e)]
  | BEXE_svc (sr,i)  -> [bexe_svc (sr, revar i)]

  | BEXE_code (sr,s,e) -> [bexe_code (sr,s, ge e)]
  | BEXE_nonreturn_code (sr,s,e) -> [bexe_nonreturn_code (sr,s, ge e)]
  | BEXE_goto (sr,idx) -> [bexe_goto (sr, revar idx)]


  (* INLINING THING *)
  | BEXE_proc_return sr as x ->
    incr end_label_uses;
    [bexe_goto (sr,end_index)]

  | BEXE_comment (sr,s) as x -> [x]
  | BEXE_nop (sr,s) as x -> [x]
  | BEXE_halt (sr,s) as x -> [x]
  | BEXE_trace (sr,v,s) as x -> [x]
  | BEXE_label (sr,idx) -> [bexe_label (sr, revar idx)]
  | BEXE_begin as x -> [x]
  | BEXE_end as x -> [x]
  | BEXE_catch _ as x -> [x]
  | BEXE_try _ as x -> [x]
  | BEXE_endtry _ as x -> [x]
  in
(*
  print_endline ("Remap exe result = " ^ catmap "\n" (string_of_bexe bsym_table 0) result);
*)
  result
  in
    let kind = match inline_method with
      | `Lazy -> "Lazy "
      | `Eager -> "Eager "
    in
    let rec fgc props s =
      match props with
      | [] -> String.concat ", " s
      | `Generated x :: t -> fgc t (x :: s)
      | _ :: t -> fgc t s
    in
    let source =
      let x = fgc props [] in
      if x <> "" then " (Generated "^x^")" else ""
    in
    (* add a comment for non-generated functions .. *)
    let b = ref [] 
    (*
      ref
      (
        if source = "" && id <> "_init_" then
          [bexe_comment (sr,(kind ^ "inline call to " ^ id ^ "<" ^
            string_of_bid callee ^ ">" ^ source))]
        else []
      )
    *)
    in
    let handle_arg prolog argmap index argument typ kind =      
      let eagerly () =
         let x = bexe_init (sr,index,argument) in
         prolog := x :: !prolog
      in
      if Flx_btype.contains_uniq typ then eagerly () else
      match kind with
      | `POnce
      | `PVal ->
          if inline_method = `Lazy 
          then Hashtbl.add argmap index argument
          else eagerly ()

      | `PVar -> eagerly ()

    in
    (*
    if inline_method = `Eager then begin
      (* create a variable for the parameter *)
      let parameter = fresh_bid syms.counter in
      let param_id = "_p" ^ string_of_bid parameter in
      (*
      print_endline ("Parameter assigned index " ^ string_of_bid parameter);
      *)

      (* create variables for parameter components *)
      (* Whaaa??
      if length ps > 1 then
      for i = 1 to length ps do incr syms.counter done;
       (* Initialise parameter to argument, but only if
         the argument is not unit
      *)
      *)
      if length ps > 0 then
      begin
        let x =
          if length ps > 1
          then begin
            let entry = BBDCL_val (vs,paramtype,`Var) in
            Hashtbl.add bsym_table parameter (param_id,Some caller,sr,entry);
            BEXE_init (sr,parameter,argument)
          end
          else
            let {pid=vid; pindex=k} = hd ps in
            let index = revar k in
            BEXE_init (sr,index,argument)
        in
        b := x :: !b;

        (* unpack argument *)
        if length ps > 1 then
        let ts = map (fun (_,i) -> btyp_type_var (i, btyp_type 0)) vs in
        let p = BEXPR_varname (parameter,ts),paramtype in
        let n = ref 0 in
        iter
        (fun {pid=vid;pindex=ix; ptyp=prjt} ->
          let pj =
            match argument with
            (* THIS CASE MAY NOT WORK WITH TAIL REC OPT! *)
            | BEXPR_tuple ls,_ ->
              begin try nth ls (!n)
              with _ ->
                failwith (
                  "[gen_body1] Woops, prj "^si (!n) ^" tuple wrong length? " ^ si (length ts)
                )
              end
            | _ -> BEXPR_get_n (!n,p),prjt
          in
          (*
          let prj = Flx_bexpr.reduce bsym_table pj in
          *)
          let prj = pj in
          let index = revar ix in
          let x = BEXE_init (sr,index,prj) in
          b := x :: !b;
          incr n
        )
        ps
      end
      ;
      iter
      (fun exe ->
        iter
        (fun x -> b := x :: !b)
        (remap exe)
      )
      exes
    end else if inline_method = `Lazy then begin
    *)
    let argmap = Hashtbl.create 97 in
    let pprjs = Flx_bparams.get_prjs ps in
    begin match pprjs with
    | [] -> ()
    | [{pkind=kind; ptyp=typ; pindex=k},_] ->
      let index = revar k in
      handle_arg b argmap index argument typ kind
    | pss ->
      (* create a variable for the parameter *)
      let parameter = fresh_bid syms.counter in
      let param_id = "_p" ^ string_of_bid parameter in
      List.iter
      (fun ({pkind=kind; pid=vid; pindex=ix; ptyp=ctyp},pj) ->
        match pj with 
        | Some pj ->
          let component = bexpr_apply ctyp (pj,argument) in
          let index = revar ix in
          handle_arg b argmap index component ctyp kind;
        | None -> assert false
      )
      pss
    end
    ;
    (*
    print_endline "argmap = ";
    Hashtbl.iter
    (fun i e ->
      try
      let id,_,_,_ = Hashtbl.find bsym_table i in
      print_endline (id ^ "<"^ si i ^ "> --> " ^ sbe sym_table e)
      with Not_found -> print_endline ("Can't find index .." ^ si i)
    )
    argmap
    ;
    print_endline "----::----";
    *)
    let sba = if Hashtbl.length argmap = 0 then
      fun x -> b := x :: !b
    else
      fun x -> b := subarg syms bsym_table argmap x :: !b
    in
    iter
    (fun exe -> iter sba (remap exe))
    exes
    ;
    (*
    print_endline "Lazy evaluation, output=";
    iter (fun x -> print_endline (string_of_bexe sym_table 0 x)) (rev !b);
    *)
    (* substitute in kids too *)
    if Hashtbl.length argmap > 0 then begin
      let closure = Flx_bsym_table.find_descendants bsym_table callee in
      (*
         let cl = ref [] in BidSet.iter (fun i -> cl := i :: !cl) closure;
         print_endline ("Closure is " ^ catmap " " si !cl);
      *)
      let kids =
        BidSet.fold
        (fun i s -> BidSet.add (revar i) s)
        closure
        BidSet.empty
      in
      BidSet.iter begin fun i ->
        let bsym = Flx_bsym_table.find bsym_table i in
        match Flx_bsym.bbdcl bsym with
        | BBDCL_fun (props,vs,ps,ret,effects,exes) ->
            let exes = map (subarg syms bsym_table argmap) exes in
            recal_exes_usage uses (Flx_bsym.sr bsym) i ps exes;
            let bbdcl = bbdcl_fun (props,vs,ps,ret,effects,exes) in
            Flx_bsym_table.update_bbdcl bsym_table i bbdcl

        | _ -> ()
      end kids
    end;

    let trail_jump = match !b with
      | BEXE_goto (_,idx)::_ when idx = end_index -> true
      | _ -> false
    in
    if trail_jump then
      (b := tl !b; decr end_label_uses)
    ;
    if !end_label_uses > 0 then begin
(*
print_endline ("[flx_spexes: inserting end label " ^ end_label);
*)
      b := (bexe_label (sr,end_index)) :: !b;
      (* we made a new label so we have to add it to the bsym_table *) 
      let bbdcl = Flx_bbdcl.bbdcl_label end_label in
      let bsym = {Flx_bsym.id=end_label; sr=sr; bbdcl=bbdcl} in 
      let parent = Some caller in (* We're inlining into this procedure! *)
      (* NOTE: WARNING: THIS MAY SCREW UP THE CURRENT ITERATION THROUGH THE HASH TABLE !!! *)
      Flx_bsym_table.add bsym_table end_index parent bsym 
    end;
    if syms.compiler_options.Flx_options.print_flag then begin
    print_endline ("INLINING " ^ id ^ " into " ^ si caller ^ " .. OUTPUT:");
    iter (fun x -> print_endline (string_of_bexe bsym_table 0 x)) (rev !b);
    print_endline ("END OUTPUT for " ^ id)
    end;
    !b

