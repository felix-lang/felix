open Flx_util
open Flx_ast
open Flx_types
open Flx_print
open Flx_mtypes1
open Flx_mtypes2
open Flx_typing
open Flx_mbind
open Flx_srcref
open List
open Flx_unify
open Flx_treg
open Flx_generic
open Flx_maps
open Flx_exceptions
open Flx_use
open Flx_child
open Flx_reparent
open Flx_call


type submode_t = [`Eager | `Lazy]

module BidSet = IntSet

(* this only updates the uses table not the usedby table,
  because inlining changes usage (obviously).
  we need it in particular for the is_recursive test,
  so that tail recursions which have been eliminated
  won't cause the test to return a false positive
*)

let recal_exes_usage syms uses sr i ps exes =
  (*
  print_endline ("Recal usage of "^ si i^", this code:\n" ^ catmap "\n" (sbx syms.dfns) exes);
  *)
  (* delete old entry *)
  (try Hashtbl.remove uses i with Not_found -> ());
  iter (Flx_call.cal_param_usage syms uses sr i) ps;
  iter (Flx_call.cal_exe_usage syms uses i) exes

let is_tailed ps exes =
  try iter
  (function
    | `BEXE_init(_,i,_) when mem i ps -> raise Not_found
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

let rec rpl syms argmap x = match map_tbexpr ident (rpl syms argmap) idt x with
  (* No need to check ts or type here *)
  | (`BEXPR_name (i,_),_) as x ->
    (try
      let x' = Hashtbl.find argmap i in
      (*
      print_endline ("Replacing variable " ^ si i ^ " with " ^ sbe syms.dfns x');
      *)
      x'
      with Not_found -> x)
  | x -> x

let subarg syms bbdfns argmap exe =
  map_bexe idt (rpl syms argmap) idt idt idt exe

(* NOTE: result is in reversed order *)
let gen_body syms (uses,child_map,bbdfns) id
  varmap ps relabel revariable exes argument
  sr caller callee vs callee_vs_len inline_method props
=
  if syms.compiler_options.print_flag then
  print_endline ("Gen body caller = " ^ si caller ^
    ", callee=" ^ id ^ "<" ^ si callee ^ ">"
  );
  (*
  let argument = reduce_tbexpr bbdfns argument in
  *)
  let psis: int list = map (fun {pindex=i} -> i) ps in

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
      Flx_call.is_recursive uses callee or
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
  if syms.compiler_options.print_flag then
  begin match inline_method with
  | `Eager ->
    print_endline ("Eager INLINING " ^ id ^ "<"^si callee^">("^sbe syms.dfns bbdfns argument^") into " ^ si caller ^ " .. INPUT:");
  | `Lazy ->
    print_endline ("Lazy INLINING " ^ id ^ "<"^si callee^">("^sbe syms.dfns bbdfns argument^") into " ^ si caller ^ " .. INPUT:");
  end
  ;
  (*
  iter (fun x -> print_endline (string_of_bexe syms.dfns bbdfns 0 x)) exes;
  *)
  let paramtype  =
    let pt =
      let pts = map (fun {ptyp=t} -> t) ps in
      match pts with
      | [x] -> x
      | x -> `BTYP_tuple x
    in
      varmap_subst varmap pt
  in

  let caller_vars = map (fun (s,i) -> `BTYP_var (i,`BTYP_type 0)) vs in
  let ge e = remap_expr syms bbdfns varmap revariable caller_vars callee_vs_len e in
  let relab s = try Hashtbl.find relabel s with Not_found -> s in
  let revar i = try Hashtbl.find revariable i with Not_found -> i in
  let end_label_uses = ref 0 in
  let end_label =
    let end_index = !(syms.counter) in
    incr syms.counter;
    "_end_" ^ (si end_index)
  in


  let remap: bexe_t -> bexe_t list =  fun exe ->
  match exe with
  | `BEXE_axiom_check _ -> assert false
  | `BEXE_call_prim (sr,i,ts,e2)  ->  assert false
    (*
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [`BEXE_call_prim (sr,i,ts, ge e2)]
    *)

  | `BEXE_call_direct (sr,i,ts,e2)  ->  assert false
    (*
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [`BEXE_call_direct (sr,i,ts, ge e2)]
    *)

  | `BEXE_call_method_direct (sr,e1,i,ts,e2)  ->
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [`BEXE_call_method_direct (sr,ge e1,i,ts, ge e2)]

  | `BEXE_call_method_stack (sr,e1,i,ts,e2)  ->
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [`BEXE_call_method_stack (sr,ge e1,i,ts, ge e2)]

  | `BEXE_jump_direct (sr,i,ts,e2)  ->
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i,ts = fixup i ts in
    [`BEXE_jump_direct (sr,i,ts, ge e2)]

  | `BEXE_call_stack (sr,i,ts,e2)  -> assert false

  | `BEXE_apply_ctor (sr,i1,i2,ts,i3,e2) ->
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i2,ts = fixup i2 ts in
    let rv i = try Hashtbl.find revariable i with Not_found -> i in
    [`BEXE_apply_ctor (sr,rv i1, i2,ts,rv i3,ge e2)]

  | `BEXE_apply_ctor_stack (sr,i1,i2,ts,i3,e2) ->
    let fixup i ts =
      let auxt t = varmap_subst varmap t in
      let ts = map auxt ts in
      try
        let j= Hashtbl.find revariable i in
        j, vsplice caller_vars callee_vs_len ts
      with Not_found -> i,ts
    in
    let i2,ts = fixup i2 ts in
    let rv i = try Hashtbl.find revariable i with Not_found -> i in
    [`BEXE_apply_ctor_stack (sr,rv i1, i2,ts,rv i3,ge e2)]

  | `BEXE_call (sr,e1,e2)  -> [`BEXE_call (sr,ge e1, ge e2)]
  | `BEXE_jump (sr,e1,e2)  -> assert false

  | `BEXE_loop (sr,i,e) -> assert false

  | `BEXE_assert (sr,e) -> [`BEXE_assert (sr, ge e)]
  | `BEXE_assert2 (sr,sr2,e1,e2) ->
    let e1 = match e1 with Some e1 -> Some (ge e1) | None -> None in
    [`BEXE_assert2 (sr, sr2, e1,ge e2)]

  | `BEXE_ifgoto (sr,e,lab) -> [`BEXE_ifgoto (sr,ge e, relab lab)]
  | `BEXE_fun_return (sr,e) -> [`BEXE_fun_return (sr, ge e)]
  | `BEXE_yield (sr,e) -> [`BEXE_yield (sr, ge e)]
  | `BEXE_assign (sr,e1,e2) -> [`BEXE_assign (sr, ge e1, ge e2)]
  | `BEXE_init (sr,i,e) -> [`BEXE_init (sr,revar i, ge e)]
  | `BEXE_svc (sr,i)  -> [`BEXE_svc (sr, revar i)]

  | `BEXE_code (sr,s)  as x -> [x]
  | `BEXE_nonreturn_code (sr,s)  as x -> [x]
  | `BEXE_goto (sr,lab) -> [`BEXE_goto (sr, relab lab)]


  (* INLINING THING *)
  | `BEXE_proc_return sr as x ->
    incr end_label_uses;
    [`BEXE_goto (sr,end_label)]

  | `BEXE_comment (sr,s) as x -> [x]
  | `BEXE_nop (sr,s) as x -> [x]
  | `BEXE_halt (sr,s) as x -> [x]
  | `BEXE_trace (sr,v,s) as x -> [x]
  | `BEXE_label (sr,lab) -> [`BEXE_label (sr, relab lab)]
  | `BEXE_begin as x -> [x]
  | `BEXE_end as x -> [x]
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
    let b =
      ref
      (
        if source = "" && id <> "_init_" then
          [`BEXE_comment (sr,(kind ^ "inline call to " ^ id ^"<"^si callee^">"^source))]
        else []
      )
    in
    let handle_arg prolog argmap index argument kind =      
      let eagerly () =
         let x = `BEXE_init (sr,index,argument) in
         prolog := x :: !prolog
      in
      match kind with
      | `PFun ->
        let argt = match argument with
        | _,`BTYP_function (`BTYP_void,t)
        | _,`BTYP_function (`BTYP_tuple [],t) -> t
        | _,t -> failwith ("Expected argument to be function void->t, got " ^ sbt syms.dfns t)
        in
        let un = `BEXPR_tuple [], `BTYP_tuple [] in
        let apl = `BEXPR_apply (argument, un), argt in
        Hashtbl.add argmap index apl

      | `PVal when inline_method = `Lazy ->
        Hashtbl.add argmap index argument

      | `PRef ->
        begin match argument with
        | `BEXPR_ref (i,ts),`BTYP_pointer t ->
          Hashtbl.add argmap index (`BEXPR_name (i,ts),t)
        | _ -> eagerly ()
        end

      | `PVal when inline_method = `Eager -> eagerly ()

      | `PVar -> eagerly ()

    in
    (*
    if inline_method = `Eager then begin
      (* create a variable for the parameter *)
      let parameter = !(syms.counter) in
      incr syms.counter;
      let param_id = "_p" ^ si parameter in
      (*
      print_endline ("Parameter assigned index " ^ si parameter);
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
            let entry = `BBDCL_var (vs,paramtype) in
            let kids =
              try Hashtbl.find child_map caller
              with Not_found -> []
            in
            Hashtbl.replace child_map caller (parameter::kids);
            Hashtbl.add bbdfns parameter (param_id,Some caller,sr,entry);
            `BEXE_init (sr,parameter,argument)
          end
          else
            let {pid=vid; pindex=k} = hd ps in
            let index = revar k in
            `BEXE_init (sr,index,argument)
        in
        b := x :: !b;

        (* unpack argument *)
        if length ps > 1 then
        let ts = map (fun (_,i) -> `BTYP_var (i,`BTYP_type 0)) vs in
        let p = `BEXPR_name (parameter,ts),paramtype in
        let n = ref 0 in
        iter
        (fun {pid=vid;pindex=ix; ptyp=prjt} ->
          let prjt = varmap_subst varmap prjt in
          let pj =
            match argument with
            (* THIS CASE MAY NOT WORK WITH TAIL REC OPT! *)
            | `BEXPR_tuple ls,_ ->
              begin try nth ls (!n)
              with _ ->
                failwith (
                  "[gen_body1] Woops, prj "^si (!n) ^" tuple wrong length? " ^ si (length ts)
                )
              end
            | _ -> `BEXPR_get_n (!n,p),prjt
          in
          (*
          let prj = reduce_tbexpr bbdfns pj in
          *)
          let prj = pj in
          let index = revar ix in
          let x = `BEXE_init (sr,index,prj) in
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
    begin match length ps with
    | 0 -> ()
    | 1 ->
      let {pkind=kind; pid=vid; pindex=k; ptyp=ptyp} = hd ps in
      let index = revar k in
      handle_arg b argmap index argument kind
    | _ ->
      (* create a variable for the parameter *)
      let parameter = !(syms.counter) in
      incr syms.counter;
      let param_id = "_p" ^ si parameter in
      (*
      print_endline ("Parameter assigned index " ^ si parameter);
      *)

      let ts = map (fun (_,i) -> `BTYP_var (i,`BTYP_type 0)) vs in
      let n = ref 0 in
      iter
      (fun {pkind=kind; pid=vid; pindex=ix; ptyp=prjt} ->
        let prjt = varmap_subst varmap prjt in
        let pj =
          match argument with
          (* THIS CASE MAY NOT WORK WITH TAIL REC OPT! *)
          | `BEXPR_tuple ls,_ ->
            begin try nth ls (!n)
            with _ ->
                failwith (
                  "[gen_body2] Woops, prj "^si (!n) ^" tuple wrong length? " ^ si (length ts)
                )
            end
          | p -> `BEXPR_get_n (!n,p),prjt
        in
        (*
        let prj = reduce_tbexpr bbdfns pj in
        *)
        let prj = pj in
        let index = revar ix in
        handle_arg b argmap index prj kind;
        incr n
      )
      ps
    end
    ;
    (*
    print_endline "argmap = ";
    Hashtbl.iter
    (fun i e ->
      try
      let id,_,_,_ = Hashtbl.find bbdfns i in
      print_endline (id ^ "<"^ si i ^ "> --> " ^ sbe syms.dfns e)
      with Not_found -> print_endline ("Can't find index .." ^ si i)
    )
    argmap
    ;
    print_endline "----::----";
    *)
    let sba = if Hashtbl.length argmap = 0 then
      fun x -> b := x :: !b
    else
      fun x -> b := subarg syms bbdfns argmap x :: !b
    in
    iter
    (fun exe -> iter sba (remap exe))
    exes
    ;
    (*
    print_endline "Lazy evaluation, output=";
    iter (fun x -> print_endline (string_of_bexe syms.dfns 0 x)) (rev !b);
    *)
    (* substitute in kids too *)
    if Hashtbl.length argmap > 0 then begin
      let closure = descendants child_map callee in
      (*
         let cl = ref [] in IntSet.iter (fun i -> cl := i :: !cl) closure;
         print_endline ("Closure is " ^ catmap " " si !cl);
      *)
      let kids =
        IntSet.fold
        (fun i s -> IntSet.add (revar i) s)
        closure
        IntSet.empty
      in
      IntSet.iter (fun i ->
        let id,parent,sr,entry = Hashtbl.find bbdfns i in
        match entry with
        | `BBDCL_function (props,vs,(ps,traint),ret,exes) ->
          let exes = map (subarg syms bbdfns argmap) exes in
          recal_exes_usage syms uses sr i ps exes;
          Hashtbl.replace bbdfns i
          (id,parent,sr,`BBDCL_function (props,vs,(ps,traint),ret,exes))

        | `BBDCL_procedure (props,vs,(ps,traint),exes) ->
          (*
          print_endline ("MODIFY " ^ si i);
          *)
          let exes = map (subarg syms bbdfns argmap) exes in
          recal_exes_usage syms uses sr i ps exes;
          Hashtbl.replace bbdfns i
          (id,parent,sr,`BBDCL_procedure (props,vs,(ps,traint),exes))

        | `BBDCL_regmatch (props,vs,(ps,traint),ret,(alpha,states,h,mx)) ->
          (try Hashtbl.remove uses i with Not_found -> ());
          iter (cal_param_usage syms uses sr i) ps;
          let h2 = Hashtbl.create 97 in
          Hashtbl.iter (fun k x ->
            let x = rpl syms argmap x in
            Hashtbl.add h2 k x;
            cal_expr_usage syms uses i sr x
          )
          h
          ;
          Hashtbl.replace bbdfns i
          (id,parent,sr,`BBDCL_regmatch (props,vs,(ps,traint),ret,(alpha,states,h2,mx)))

        | `BBDCL_reglex (props,vs,(ps,traint),j,ret,(alpha,states,h,mx)) ->
          (try Hashtbl.remove uses i with Not_found -> ());
          iter (cal_param_usage syms uses sr i) ps;
          let h2 = Hashtbl.create 97 in
          Hashtbl.iter (fun k x ->
            let x = rpl syms argmap x in
            Hashtbl.add h2 k x;
            cal_expr_usage syms uses i sr x
          )
          h
          ;
          Hashtbl.replace bbdfns i
          (id,parent,sr,`BBDCL_reglex (props,vs,(ps,traint),j,ret,(alpha,states,h2,mx)))

        | _ -> ()
      )
      kids
    end
    ;
    let trail_jump = match !b with
      | `BEXE_goto (_,lab)::_ when lab = end_label -> true
      | _ -> false
    in
    if trail_jump then
      (b := tl !b; decr end_label_uses)
    ;
    if !end_label_uses > 0 then
      b := (`BEXE_label (sr,end_label)) :: !b
    ;
    (*
    print_endline ("INLINING " ^ id ^ " into " ^ si caller ^ " .. OUTPUT:");
    iter (fun x -> print_endline (string_of_bexe syms.dfns 0 x)) (rev !b);
    print_endline ("END OUTPUT for " ^ id);
    *)
    !b
