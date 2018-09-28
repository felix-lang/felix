open List

open Flx_ast
open Flx_bbdcl
open Flx_bexe
open Flx_bexpr
open Flx_bparameter
open Flx_btype
open Flx_call
open Flx_exceptions
open Flx_list
open Flx_maps
open Flx_mtypes2
open Flx_options
open Flx_print
open Flx_set
open Flx_types
open Flx_typing
open Flx_unify
open Flx_use
open Flx_util
open Flx_bid

let add_xclosure syms cls e =
  (*
  print_endline ("chk cls for " ^ sbe bsym_table e);
  *)
  match e with
  | BEXPR_closure (i,ts),t -> cls := BidSet.add i !cls
  | _ -> ()

let ident x = x

(* WARNING!! closure here has TWO meanings: a BEXPR_closure,
  and ALSO the setwise closure of all such explicit closure
  terms ..
*)

let expr_find_xclosures syms cls e =
  Flx_bexpr.iter ~f_bexpr:(add_xclosure syms cls) e

let exe_find_xclosure syms cls exe =
  Flx_bexe.iter ~f_bexpr:(expr_find_xclosures syms cls) exe

let exes_find_xclosure syms cls exes =
  List.iter (exe_find_xclosure syms cls) exes

let exes_get_xclosures syms exes =
  let cls = ref BidSet.empty in
  exes_find_xclosure syms cls exes;
  !cls

let function_find_xclosure syms cls bsym_table i =
  let exes =
    match Flx_bsym_table.find_bbdcl bsym_table i with
    | BBDCL_fun (_,_,_,_,_,exes) -> exes
    | _ -> []
  in
  (*
  print_endline ("ROUTINE " ^ si i);
  List.iter (fun exe -> print_endline (string_of_bexe 0 exe)) exes;
  *)
  exes_find_xclosure syms cls exes

let functions_find_xclosures syms cls bsym_table ii =
  BidSet.iter
  (function_find_xclosure syms cls bsym_table)
  ii

let rec check_ahead i n ls res =
  if n = 0 then rev res else match ls with
  | [] -> []
  | h :: t ->  match h with
  | BEXE_init (_,k, ( BEXPR_apply ((BEXPR_prj (p,_,_),_),(BEXPR_varname (j,[]),_)),typ))

  | BEXE_assign (_,(BEXPR_varname (k,[]),_), ( BEXPR_apply ((BEXPR_prj (p,_,_),_),(BEXPR_varname (j,[]),_)),typ))
    when i = j -> check_ahead i (n-1) t ((k,p,typ)::res)

  | _ -> []

let rec drop n ls =
  if n = 0 then ls else match ls with
  | h :: t -> drop (n-1) t
  | [] -> assert false


(* This routine checks the only use of i is to get its n'th projection *)
exception BadUse
let rec check_proj_wrap_expr n i e = match e with
  | BEXPR_varname (i',_),_ when i = i' -> raise BadUse
  | BEXPR_apply ((BEXPR_prj (n',_,_),_), (BEXPR_varname (i',_),_)),_
     when i = i' && n = n' ->  ()

  | x -> Flx_bexpr.flat_iter ~f_bexpr:(check_proj_wrap_expr n i) x

let check_proj_wrap_exe syms bsym_table n i x =
  try
    Flx_bexe.iter ~f_bexpr:(check_proj_wrap_expr n i) x
  with BadUse ->
    (*
    print_endline ("Bad use of " ^ si i ^ ".(" ^ si n ^") in " ^
      string_of_bexe bsym_table 0 x
    );
    *)
    raise BadUse

let check_proj_wrap_exes syms bsym_table n i xs =
  List.iter (check_proj_wrap_exe syms bsym_table n i) xs

let check_proj_wrap_entry syms bsym_table n i k =
  match Flx_bsym_table.find_bbdcl bsym_table k with
  | BBDCL_fun (_,_,_,_,_,exes) -> check_proj_wrap_exes syms bsym_table n i exes
  | _ -> ()

let check_proj_wrap_closure syms bsym_table descend usage n i e =
  (*
  print_endline ("Check use  of " ^ si i ^ ".(" ^ si n ^") in expr=" ^ sbe bsym_table e);
  *)
  check_proj_wrap_expr n i e;
  (*
  print_endline "Now, check in dependents";
  *)
  let u = expr_uses_unrestricted syms descend usage e in
  BidSet.iter (check_proj_wrap_entry syms bsym_table n i) u

let tailit syms bsym_table uses id this sr ps exes =
  if syms.compiler_options.print_flag then
  begin
    print_endline ("======= Tailing " ^ id ^ "<" ^ si this ^ "> exes=====");
    List.iter (fun x -> print_endline (string_of_bexe bsym_table 0 x)) exes;
    print_endline "======== END BODY ========";
  end
  ;


  let pset = List.fold_left
    (fun s i -> if i = 0 then s else BidSet.add i s)
    BidSet.empty
    (Flx_bparams.get_bids ps)
  in
  let parameters = ref [] in
  let descend = Flx_bsym_table.find_descendants bsym_table this in
  let children =
    try Flx_bsym_table.find_children bsym_table this
    with Not_found -> BidSet.empty
  in
  let can_loop () =
    let varlist = BidSet.filter
      (Flx_bsym_table.is_variable bsym_table)
      children
    in
    let funset = BidSet.filter
      (Flx_bsym_table.is_function bsym_table)
      descend
    in

    (*
    print_endline ("Procedure has " ^ si (length varlist) ^ " variables");
    print_endline ("Procedure has " ^ si (BidSet.cardinal funset) ^ " child funcs");
    *)

    let cls = ref BidSet.empty in
    functions_find_xclosures syms cls bsym_table funset;
    (* THIS FUNCTION IS BEING INLINED .. WE CANNOT LOOKUP ITS EXES!! *)
    exes_find_xclosure syms cls exes;
    (*
    print_endline ("Total xclosures " ^ si (BidSet.cardinal !cls));
    *)
    let kidcls = BidSet.inter !cls funset in
    (*
    print_endline ("Kid xclosures " ^ si (BidSet.cardinal kidcls));
    *)
    try
      BidSet.iter
      (fun i ->
        let usage = Hashtbl.find uses i in
        BidSet.iter begin fun j ->
          let usesj =   mem_assoc j usage in
          (*
          if usesj then
            print_endline (si i ^ " uses var " ^ si j)
          ;
          *)
          if usesj then raise Not_found;
        end varlist
      )
      kidcls
      ;
      true
    with
    | Not_found -> false
  in

  let jump_done = ref false in
  let lc = fresh_bid syms.counter in
  let start_label = "start_" ^ string_of_bid lc in

  (* note reverse order *)
  (* Weirdly, this works for BOTH tail calls
    and tail applies
  *)
  let pslist = Flx_bparams.get_prjs ps in
  let params = List.map fst pslist in
  let cal_tail_call e =
    let args  = 
      List.map 
        (fun prj -> match prj with 
        | Some ((_,BTYP_function (_,c)) as prj) ->
          (* we RELY on this application simplifying the arguments! *)
          Flx_bexpr.bexpr_apply c (prj,e)
        | None -> e
        | _ -> assert false
        ) 
        (List.map snd pslist)
    in
    match fst ps with
    | Slist [] ->
      [
        bexe_goto (sr,lc);
        bexe_comment (sr,"tail rec call (0)")
      ]
    | Satom {pindex=k} ->
      assert (k <> 0);
      [
        bexe_goto (sr,lc);
        bexe_init (sr,k,e);
        bexe_comment (sr,"tail rec call (1)")
      ]
    | _ ->
(*
      begin match e with
      | BEXPR_tuple ls,_ ->
        (*
        print_endline ("TUPLE ASSGN " ^ sbe bsym_table e);
        *)
        assert (length ls = length ps);
*)
        let pinits =
          map2
          (fun {pid=name; pindex=i; ptyp=t} e ->
            i,(name,t,e,expr_uses syms descend uses pset e)
          )
          params args
        in
        let tmps,exes = Flx_passign.passign syms bsym_table pinits sr in
        parameters := tmps @ !parameters;
        let result = ref exes in
        result := bexe_goto (sr,lc) :: !result;
        (*
          print_endline "Tail opt code is:";
          List.iter (fun x -> print_endline (string_of_bexe bsym_table 0 x) ) (rev !result);
        *)
        !result
(*
      | _ ->
        print_endline "NON TUPLE TAIL CALL";
        let t = snd e in
        let pix =
          try assoc t !parameters
          with Not_found ->
            let pix = fresh_bid syms.counter in
            parameters := (t,pix) :: !parameters;
            pix
        in
        let p = bexpr_varname t (pix,[]) in
        let n = ref 0 in
        let k = List.length ps in
        let param_decode =
          List.map
          (fun {pindex=ix; ptyp=prjt} ->
            let prj = Flx_bexpr.reduce (bexpr_get_n prjt (!n) p) in
            incr n;
            bexe_init (sr,ix,prj)
          )
          ps
        in
        [
          bexe_goto (sr,lc);
        ]
        @
        param_decode
        @
        [
          bexe_init (sr,pix,e);
          bexe_comment (sr,"tail rec call (2)")
        ]
      end
*)
  in
  let cal_asgn1 pas ls =
    let n = length pas in
    (* vset is the restriction set applied to the usage closure to
      restrict attention to the LHS variables
    *)
    let vset = List.fold_left
      (fun acc (k,_,_) ->BidSet.add k acc)
      BidSet.empty
      pas
    in
    let asgns = List.map
      (fun (k,p,t) ->
        let name = "DUNNO_" ^ string_of_bid k in
        let e = nth ls p in
        let d = expr_uses syms descend uses vset e in
        k, (name,t,e,d)
      )
      pas
    in
      let tmps,exes = Flx_passign.passign syms bsym_table asgns sr in
      parameters := tmps @ !parameters;
      if syms.compiler_options.print_flag then begin
        print_endline "CALCULATED ACTUAL PARALLEL ASSIGNMENTS!";
        List.iter
          (fun x -> print_endline (string_of_bexe bsym_table 0 x))
          (rev exes)
      end;
      (* NOTE: we have to KEEP the tuple variable in case
        it is used elsewhere .. it should get dropped by later
        garbage collection if it isn't used!
      *)
    exes
  in
  let asgn2 i t ls =
    let k = List.length ls in
    List.map2
    (fun (e,t' as x) j ->
      let t = btyp_pointer t in
      let t' = btyp_pointer t' in
      bexe_storeat
      (
(*
print_endline ("tailit:asgn2 assign to projection: fixed");
*)
        sr,
        (bexpr_get_n t' j (bexpr_ref t (i,[]))),
        x
      )
    )
    ls
    (nlist k)
(*
    List.map2
    (fun (e,t' as x) j ->
      bexe_assign
      (
print_endline ("tailit:asgn2 assign to projection");
        sr,
        (bexpr_get_n t' j (bexpr_varname t (i,[]))),
        x
      )
    )
    ls
    (nlist k)
*)
  in
  let rec aux inp res =
    let cal_par' i t ls h tail =
      if syms.compiler_options.print_flag then
      begin
        print_endline ("flx_tailit found possible parallel assignment opportunity in:");
        print_endline (string_of_bexe bsym_table 0 h);
      end;

      (* the descendants of the variables parents! *)
      let parent = Flx_bsym_table.find_parent bsym_table i in
      let descend = match parent with
        | Some parent -> Flx_bsym_table.find_descendants bsym_table parent
        | None ->
          let d = ref BidSet.empty in
          Flx_bsym_table.iter (fun i _ _ -> d := BidSet.add i !d) bsym_table;
          !d
      in

      let n = length ls in
      let pas = check_ahead i n tail [] in

      (* SPECIAL CASE

        tmp = r1, r2, r3;
        l1 = tmp.(0);
        l2 = tmp.(1);
        l3 = tmp.(2);

        generated by desugaring routine

      *)
      if length pas = n then begin
        if syms.compiler_options.print_flag then
        print_endline "FULLY OPTIMISE";
        if syms.compiler_options.print_flag then begin
          let rec p n ls = if n > 0 then match ls with
            | h :: t ->
               print_endline (string_of_bexe bsym_table 0 h);
               p (n-1) t
            | [] -> assert false
          in
          p n tail;
          print_endline "Assigns are:";
          let rec pr xs = match xs with
          | (k,p,t) :: tl ->
             print_endline (
               "var " ^ string_of_bid k ^ " : " ^ sbt bsym_table t ^
               " = var " ^ string_of_bid i ^ ".(" ^ string_of_int p ^ ") = " ^
               sbe bsym_table (nth ls p)
             );
             pr tl
          | [] -> ()
          in
          pr pas;
        end;

        let exes = cal_asgn1 pas ls in
        aux (drop n tail) (exes @ h :: res)
      end
      else  (* length pas <> n *)
      begin
        let can_opt =
          try
            let counter = ref 0 in
            List.iter (fun e ->
              let n = !counter in incr counter;
              check_proj_wrap_closure syms bsym_table descend uses n i e
            ) ls;
            true
          with
            BadUse -> false
        in
(*
        print_endline ("Maybe can optimise passign, type = " ^ sbt bsym_table t);
*)
        if islinear_type bsym_table t then
           print_endline "Woops, linear type shouldn't opt?"
        ;
        if can_opt then
        begin
          if syms.compiler_options.print_flag then
          begin
            print_endline ("SEMI OPTIMISE!");
          end;
          let exes = asgn2 i t ls in
          (* the 'rev' here is not necessary, but it preserves the
             order of writing by the user, i.e. from lowest index
             to highest index
          *)
          aux tail (rev exes @ res)
        end
        else begin
          (*
          assert (d = BidSet.singleton i);
          *)
          if syms.compiler_options.print_flag then
            print_endline ("UNOPTIMISED");
          aux tail (h::res)
        end
      end
    in
    let cal_par i t ls h tail =
      (* don't split up tuple components if the type is a linear
         type because the packed assignment is just an integer!
         Can't get faster than that!
      *)
      if islinear_type bsym_table t then
        aux tail (h::res)
      else
        cal_par' i t ls h tail 
    in
    match inp with
    | (BEXE_call (sr,(BEXPR_closure(i,ts),_),a)) as x :: tail -> assert false
    | (BEXE_call_direct (sr,i,ts,a)) as x :: tail  

      when (i)=(this) && Flx_cflow.tailable exes [] tail
      ->
      assert (ts=[]);
      if can_loop ()
      then begin
        (*
        print_endline ("--> Tail rec call optimised " ^ si this);
        *)
        jump_done := true;
        let res = cal_tail_call a @ res
        in aux tail res
      end else begin
        (*
        print_endline ("--> Tail rec call NOT optimised " ^ si this);
        *)
        aux tail (x::res)
      end

    | BEXE_fun_return (sr,(BEXPR_apply((BEXPR_closure (i,ts),_),a),_)) :: tail -> assert false

    | BEXE_fun_return (sr,(BEXPR_apply_direct(i,ts,a),_)) :: tail 

      when (i)=(this)
      ->
       assert (ts=[]);
       (*
       print_endline ("--> Tail rec apply " ^ si this);
       *)
       jump_done := true;
       let res = cal_tail_call a @ res
       in aux tail res

    | BEXE_fun_return (sr,(BEXPR_apply_direct(i,ts,a),_)) as x :: tail -> 
      (*
      print_endline ("--> NONSELF? Tail rec apply " ^ si i);
      print_endline ("This = " ^ si this);
      *)
      aux tail (x::res)


    | (BEXE_call_direct (sr,i,ts,a)) as x :: tail  -> 
      (*
      print_endline ("Untailed call " ^ si i ^ "["^catmap "," (sbt bsym_table) ts^"]");
      print_endline ("This = " ^ si this);
      print_endline ("ts'=" ^"["^catmap "," (sbt bsym_table) ts'^"]");
      print_endline "TAIL=";
      List.iter (fun x -> print_endline (string_of_bexe 0 x)) tail;
      print_endline "-- end of tail --";
      *)
      assert (ts=[]);
      aux tail (x::res)

    | [] -> rev res (* forward order *)
    | BEXE_init (sr,i,(BEXPR_tuple ls,t)) as h :: tail
      ->
        cal_par i t ls h tail

    | (BEXE_assign (sr,(BEXPR_varname (i,[]),_),(BEXPR_tuple ls,t)) as h) :: tail
      ->
       cal_par i t ls h tail

    | (BEXE_assign (sr,x,(BEXPR_tuple ls,t)) as h) :: tail
      ->
      let rec unproj e = match Flx_bexpr.map ~f_bexpr:unproj e with
      | BEXPR_apply ((BEXPR_prj (k,_,_),_),(BEXPR_tuple ls,_)),_ -> nth ls k
      | x -> x
      in
      let ls = List.map unproj ls in
      print_endline "DETECTED PARALLEL ASSIGN WITH LHS EXPR";
      print_endline (string_of_bexe bsym_table 0 h);
      let i = !(syms.counter) in incr (syms.counter);
      let n = length ls in
      let pbase = !(syms.counter) in syms.counter := !(syms.counter) + n;
      let me e = match Flx_unify_expr.expr_maybe_matches bsym_table syms.counter [] [] x e with
        | Some ([],[]) -> true
        | None -> false
        | _ -> assert false
      in
      let rec repl e = match Flx_bexpr.map ~f_bexpr:repl e with
        | x when me x -> bexpr_varname t (i,[])
        | BEXPR_apply  ((BEXPR_prj (k,_,_),_),(BEXPR_varname (j,[]),t)),t' when i = j->
          bexpr_varname t' (pbase+k,[])
        | x -> x
      in
      let check = me x in
      (*
      print_endline (if check then "MATCHES SELF" else "DOES'T MATCH SELF");
      *)
      if check then begin
        let ls' = List.map repl ls in
        (*
        let j = ref 0 in
        List.iter2 (fun x x' ->
          print_endline ("Recoded " ^
            sbe bsym_table x ^
            "\nas var " ^ string_of_bid (List.nth is !j) ^ "=" ^
            sbe bsym_table x' ^
            "\n"
          );
          incr j
        )
        ls ls'
        ;
        *)
        let vset = List.fold_left
          (fun acc k -> BidSet.add (k+pbase) acc)
          BidSet.empty
          (nlist n)
        in
        let asgns = List.map
          (fun k ->
          let name = "DUNNO_" ^ si (k+pbase) in
          let e = nth ls' k in
          let _,t = e in
          let d = expr_uses syms descend uses vset e in
          (*
          print_endline ("var " ^ si (k+pbase) ^ " = " ^
          print_endline ("var " ^ string_of_bid k ^ " = " ^
            sbe bsym_table e ^ " USES " ^ string_of_bidset d);
          *)
          k+pbase, (name,t,e,d)
        )
        (nlist n)
        in
        let tmps,exes = Flx_passign.passign syms bsym_table asgns sr in
        parameters := tmps @ !parameters;
        if syms.compiler_options.print_flag then
        begin
          print_endline "PARALLEL ASSIGNMENTS (before unsub) =";
          List.iter
            (fun x -> print_endline (string_of_bexe bsym_table 0 x))
            (rev exes)
        end;
        let rec undo_expr e = match Flx_bexpr.map ~f_bexpr:undo_expr e with
        | BEXPR_varname (j,[]),t when i = j  -> x
        | BEXPR_varname (j,[]),t when j >= pbase && j < pbase + n-> bexpr_get_n t (j-pbase) x
        | x -> x
        in
        let undo_st st = match st with
        | BEXE_init (sr,j,e) when j >= pbase && j < pbase + n ->
          let k = j - pbase in
          let _,t' = nth ls k in
print_endline ("tailit: assign to projection: FIXME");
          bexe_assign (sr,(bexpr_get_n t' k x),undo_expr e)

        | x -> Flx_bexe.map ~f_bexpr:undo_expr x
        in
        let exes = List.map undo_st exes in
        if syms.compiler_options.print_flag then
        begin
          print_endline "PARALLEL ASSIGNMENTS (after unsub) = ";
          List.iter
            (fun x -> print_endline (string_of_bexe bsym_table 0 x))
            (rev exes)
        end;
        aux tail (exes @ res)
      end
      else
      aux tail (h::res)


    | h :: t  -> aux t (h::res)
    in
      let exes = aux exes [] in

      (* instantiate any parameter temporaries *)
      List.iter begin fun (paramtype, parameter) ->
        let id = "_trp_" ^ string_of_bid parameter in
        Flx_bsym_table.add bsym_table parameter (Some this)
          (Flx_bsym.create ~sr id (bbdcl_val ([], paramtype, `Tmp)))
      end !parameters;

      (* return with posssible label at start *)
      let exes =
        if !jump_done
        then begin 
          let bbdcl = Flx_bbdcl.bbdcl_label start_label in
          let bsym = {Flx_bsym.id=start_label; sr=sr; bbdcl=bbdcl} in 
          let parent = Some this in 
(*
print_endline ("flx_tailit: adding label " ^ start_label ^ "<" ^ string_of_int lc ^">");
*)
          Flx_bsym_table.add bsym_table lc parent bsym;
 
          bexe_label (sr,lc) :: exes
        end
        else exes
      in
      if syms.compiler_options.print_flag then
      begin
        print_endline ("Tailed exes = ");
        List.iter (fun exe -> print_endline (string_of_bexe bsym_table 0 exe)) exes;
      end
      ;
      exes


